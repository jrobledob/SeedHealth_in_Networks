# =============================================================================
# 02_params.R
# Parameters for the seed degeneration network model
#
# Design principle:
#   Seed-quality categories (CS / QDS / IS) differ ONLY in pHSinit.
#   Biological / management / environmental parameters are otherwise
#   IDENTICAL across seed types, but vary spatially as follows:
#
#       pHSinit  varies by node TYPE   (via node_type_pHSinit)
#       W        varies by node REGION (wxtnormm / wxtnormsd via region_W)
#       Z        varies by node REGION (zxtnormm / zxtnormsd via region_Z)
#
#   Each node's effective parameter list is assembled by build_node_params()
#   below; this is what simulate_network() now consumes.
#


# ── Shared parameters (applied to every node by default) ─────────────────────
# Values listed for wxtnormm / wxtnormsd and zxtnormm / zxtnormsd are only
# fallbacks; build_node_params() overrides them from region_W and region_Z.
base_params <- list(
  Kx        = 100,
  betax     = 0.02,
  wxtnormm  = 0.5,    wxtnormsd = 0.10,   # default W (overridden by region_W)
  hx        = 1,
  mxtnormm  = 1.0,    mxtnormsd = 0.10,   # no vector management
  axtnormm  = 1.0,    axtnormsd = 0.10,   # no roguing (uniform across regions)
  rx        = 0.05,                       # low reversion
  zxtnormm  = 1.0,    zxtnormsd = 0.10,   # default Z (overridden by region_Z)
  gx        = 4,
  cx        = 0.9,
  phix      = 0,
  maY       = 100,
  miY       = 0,
  thetax    = 0.2,
  Ex        = 0.05
)

# ── Seed-type-specific pHSinit (intervention seed quality) ───────────────────
seed_pHSinit <- c(
  certified = 0.97,
  qds       = 0.80,
  informal  = 0.40
)

# ── Node-type-specific baseline pHSinit (Objective 2) ────────────────────────
node_type_pHSinit <- c(
  Seed_specialist = 0.80,
  Government      = 0.75,
  NGO             = 0.70,
  Farm_assoc      = 0.60,
  Custodian       = 0.55,
  Farmer          = 0.40,
  Trader          = 0.35,
  Other           = 0.30
)

# ── Region-specific W (weather conduciveness to disease) ─────────────────────
# Higher mean = environment more favorable to disease (faster spread).
# Default values reflect a hypothetical Andean potato gradient; edit to
# match your study area when running on real data.
region_W <- list(
  Junin        = list(mean = 0.50, sd = 0.10),
  Pasco        = list(mean = 0.60, sd = 0.10),
  Huancavelica = list(mean = 0.70, sd = 0.15),
  Lima         = list(mean = 0.40, sd = 0.10)
)

# ── Region-specific Z (positive selection / farmer-level management) ─────────
# In seedHealth, zxtnormm closer to 0 = stronger selection toward healthy
# seed (better management). Closer to 1 = essentially random selection.
region_Z <- list(
  Junin        = list(mean = 1.00, sd = 0.10),
  Pasco        = list(mean = 0.95, sd = 0.10),
  Huancavelica = list(mean = 0.90, sd = 0.15),
  Lima         = list(mean = 1.00, sd = 0.10)
)

# ── Defaults for any region not found in the tables above ────────────────────
default_region_W <- list(mean = 0.50, sd = 0.10)
default_region_Z <- list(mean = 1.00, sd = 0.10)

# ── Build node-indexed pHSinit vector ────────────────────────────────────────
init_pHS_by_node_type <- function(g, default = 0.40) {
  v <- node_type_pHSinit[V(g)$type]
  v[is.na(v)] <- default
  names(v)    <- V(g)$name
  v
}

# ── Build a per-node list of parameter lists ─────────────────────────────────
# Returns a list of length vcount(g), named by V(g)$name and in V(g)$name
# order. Each element is a copy of base_params with W and Z overridden
# according to the node's region. Pass this to simulate_network().
#
# To add another region-varying parameter (e.g. roguing axtnormm):
#   1. Define a region_A table with the same {mean, sd} structure.
#   2. Add a `default_region_A`.
#   3. Inside the lapply below, add:
#        Ap <- if (!is.null(A_table[[r]])) A_table[[r]] else default_A
#        p$axtnormm  <- Ap$mean
#        p$axtnormsd <- Ap$sd
build_node_params <- function(g, base_params,
                              W_table   = region_W,
                              Z_table   = region_Z,
                              default_W = default_region_W,
                              default_Z = default_region_Z) {
  
  node_names <- V(g)$name
  regions    <- V(g)$region
  
  if (length(regions) != length(node_names) || any(is.na(regions)))
    stop("V(g)$region is missing or has NAs — cannot assign per-region params.")
  
  per_node <- lapply(seq_along(node_names), function(i) {
    r  <- regions[i]
    Wp <- if (!is.null(W_table[[r]])) W_table[[r]] else default_W
    Zp <- if (!is.null(Z_table[[r]])) Z_table[[r]] else default_Z
    
    p <- base_params
    p$wxtnormm  <- Wp$mean
    p$wxtnormsd <- Wp$sd
    p$zxtnormm  <- Zp$mean
    p$zxtnormsd <- Zp$sd
    p
  })
  names(per_node) <- node_names
  per_node
}