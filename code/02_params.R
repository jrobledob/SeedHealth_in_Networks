# =============================================================================
# 02_params.R  (pipeline v3)
# Seed-degeneration parameters.
#
# Design (per agreed spec):
#   * Three SEED-QUALITY scenarios differ only in the BASE mean pHS_0:
#         Informal           = 0.70
#         QDS (moderate)     = 0.80
#         QDS (optimistic)   = 0.90
#   * Each node's starting pHS_0 = scenario base + a region x node-type nudge:
#         Low -> -0.05 , Medium -> 0 , High -> +0.05   (clamped to [0,1])
#   * Wa (environment, -> wxtnormm) and Z (selection, -> zxtnormm) vary by the
#     SAME region x node-type category, mapped Low/Medium/High -> 0.2/0.5/0.8,
#     with a single SD of 0.10.
#   * The SAME tables are applied to BOTH the nonstress and stress networks;
#     the only structural difference between them is the network itself.
#   * All other onesim parameters (betax, Kx, r, g, c, E, theta, ...) are
#     identical across nodes and scenarios (see base_params).
# =============================================================================

# ── One number you may want to change ────────────────────────────────────────
# Interpretation of "5 units" in the pHS_0 nudge, on the 0-1 pHS scale.
PHS_REGION_ADJ <- 0.05

# ── Level -> numeric maps ─────────────────────────────────────────────────────
WZ_LEVELS  <- c(Low = 0.2, Medium = 0.5, High = 0.8)          # Wa and Z means
WZ_SD      <- 0.10                                            # single SD
PHS_ADJUST <- c(Low = -1, Medium = 0, High = 1) * PHS_REGION_ADJ

# ── Seed-quality scenarios (base mean pHS_0) ─────────────────────────────────
seed_scenarios <- data.frame(
  scenario = c("Informal (0.7)", "QDS moderate (0.8)", "QDS optimistic (0.9)"),
  pHS0     = c(0.70,             0.80,                 0.90),
  stringsAsFactors = FALSE
)

# ── Region x node-type category matrix ───────────────────────────────────────
# The pattern is identical for Wa, Z, and the pHS_0 nudge (as confirmed).
# Rows = region, columns = node type. Values in {Low, Medium, High}.
TYPES_ORDER <- c("Seed_specialist", "Farmer", "Custodian", "Other",
                 "Farm_assoc", "Government", "Trader", "NGO")

.pat_low <- rep("Low", 8)
.pat_A   <- c("Low", "Medium", "Low", "Medium", "Medium", "Low", "Medium", "Medium")
.pat_B   <- c("Low", "High",   "Low", "High",   "Medium", "Low", "High",   "Medium")

CATEGORY_MATRIX <- rbind(
  Huancavelica = .pat_low,
  Pasco        = .pat_low,
  Junin        = .pat_A,
  Huanuco      = .pat_B,
  Apurimac     = .pat_low,
  Ayacucho     = .pat_A,
  Lima         = .pat_B
)
colnames(CATEGORY_MATRIX) <- TYPES_ORDER

# Plex's networks collapse Government and NGO into one institutional
# type, "Public". Give it ONE Low/Medium/High rating per region; this drives the
# Wa / Z means and the pHS_0 nudge exactly as for every other type.
#
# PLACEHOLDER below mirrors the old "Government" column (Low in every region).
# Replace with the CIP-provided ratings for "Public" when they arrive.
PUBLIC_BY_REGION <- c(
  Huancavelica = "Low",
  Pasco        = "Low",
  Junin        = "Medium",
  Huanuco      = "Medium",
  Apurimac     = "Low",
  Ayacucho     = "Medium",
  Lima         = "Medium"
)
stopifnot(setequal(names(PUBLIC_BY_REGION), rownames(CATEGORY_MATRIX)))
CATEGORY_MATRIX <- cbind(CATEGORY_MATRIX,
                         Public = PUBLIC_BY_REGION[rownames(CATEGORY_MATRIX)])
TYPES_ORDER <- c(TYPES_ORDER, "Public")


# ── Category lookup with safe default ────────────────────────────────────────
# Unknown region/type -> "Medium" (neutral) with a one-time-ish warning.
lookup_category <- function(region, type) {
  if (is.na(region) || is.na(type) ||
      !(region %in% rownames(CATEGORY_MATRIX)) ||
      !(type   %in% colnames(CATEGORY_MATRIX))) {
    warning(sprintf("No category for region='%s' / type='%s' -> defaulting to 'Medium'.",
                    region, type), call. = FALSE)
    return("Medium")
  }
  CATEGORY_MATRIX[region, type]
}

# ── Shared (node-invariant) onesim parameters ────────────────────────────────
# wxtnormm / zxtnormm here are placeholders; build_node_params() overrides them.
base_params <- list(
  Kx        = 100,
  betax     = 0.02,
  wxtnormm  = 0.5,    wxtnormsd = WZ_SD,    # overridden per node (Wa table)
  hx        = 1,
  mxtnormm  = 1.0,    mxtnormsd = 0.10,     # no vector management
  axtnormm  = 1.0,    axtnormsd = 0.10,     # no roguing
  rx        = 0.05,                          # low reversion
  zxtnormm  = 0.5,    zxtnormsd = WZ_SD,    # overridden per node (Z table)
  gx        = 4,
  cx        = 0.9,
  phix      = 0,
  maY       = 100,
  miY       = 0,
  thetax    = 0.2,
  Ex        = 0.05
)

# ── Per-node parameter list (W and Z by region x type) ───────────────────────
# Returns a list of length vcount(g), named and ordered by V(g)$name.
build_node_params <- function(g, base_params = get("base_params")) {
  node_names <- igraph::V(g)$name
  regions    <- igraph::V(g)$region
  types      <- igraph::V(g)$type

  per_node <- lapply(seq_along(node_names), function(i) {
    cat_i <- lookup_category(regions[i], types[i])
    p <- base_params
    p$wxtnormm  <- unname(WZ_LEVELS[cat_i]);  p$wxtnormsd <- WZ_SD
    p$zxtnormm  <- unname(WZ_LEVELS[cat_i]);  p$zxtnormsd <- WZ_SD
    p
  })
  names(per_node) <- node_names
  per_node
}

# ── Per-node starting pHS_0 vector for a given scenario base ─────────────────
# pHS_0[node] = scenario_pHS0 + adjustment(category(region,type)), clamped.
build_pHSinit <- function(g, scenario_pHS0) {
  node_names <- igraph::V(g)$name
  regions    <- igraph::V(g)$region
  types      <- igraph::V(g)$type

  v <- vapply(seq_along(node_names), function(i) {
    cat_i <- lookup_category(regions[i], types[i])
    val   <- scenario_pHS0 + unname(PHS_ADJUST[cat_i])
    min(max(val, 0), 1)
  }, numeric(1))
  names(v) <- node_names
  v
}
