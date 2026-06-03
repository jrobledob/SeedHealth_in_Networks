# =============================================================================
# 01b_conform_real_networks.R  (pipeline v3)
# Convert the collaborator's real igraph networks into the EXACT structure the
# downstream pipeline expects, then save a `networks`-shaped object
# (named list: Nonstress, Stress).
#
# Reference schema = small_net_*.RDS (the format the large nets will follow):
#   vertex attrs: name, type, region, gender, betweenness, in_strength,
#                 out_strength, in_degree, out_degree
#   edge attrs:   weight ; directed = TRUE
#
# Conform actions:
#   - type:     keep "Public" (= Government + NGO, merged by collaborator) as a
#               first-class type; fold only genuinely-unexpected labels -> Other
#   - region:   present -> keep (guard NA -> "Unknown")
#   - location: absent in this schema -> placeholder "Unknown"
#   - gender:   drop (unused by pipeline)
#   - metrics:  drop collaborator's and recompute via add_node_metrics() so they
#               are computed identically to the example (degree_total added)
#
# Run from the project root:
#     Rscript code/01b_conform_real_networks.R
# Then in 05_run_all.R:
#     networks <- readRDS(file.path(DATA_DIR, "networks_real.rds"))
# =============================================================================

library(igraph)

# Reuse add_node_metrics() + KNOWN_TYPES from the example builder.
source(file.path("code", "01_network.R"))


 
# ── CONFIG ───────────────────────────────────────────────────────────────────
DATA_DIR <- "Data"
# Point at the large nets for the real run; small nets for a dry test.
NET_FILES <- list(
  Nonstress = file.path(DATA_DIR, "large_net_nonstress.RDS"),
  Stress    = file.path(DATA_DIR, "large_net_stress.RDS")
)
OUT_FILE     <- file.path(DATA_DIR, "networks_real.rds")
LOCATION_FILL <- "Unknown"   # no location in this schema; placeholder value

# "Public" is a legitimate merged type (Government + NGO), NOT folded to Other.
ACCEPTED_TYPES <- union(KNOWN_TYPES, "Public")

# =============================================================================
# Conform one graph to the target structure
# =============================================================================
conform_network <- function(g, label = "") {
  stopifnot(igraph::is_igraph(g), igraph::is_directed(g))
  
  # 1. node type: keep Public; fold only truly-unexpected labels into Other ----
  ty <- igraph::V(g)$type
  ty[is.na(ty)] <- "Other"
  bad <- setdiff(unique(ty), ACCEPTED_TYPES)
  if (length(bad)) {
    warning(sprintf("[%s] folding unexpected type(s) into 'Other': %s",
                    label, paste(bad, collapse = ", ")))
    ty[ty %in% bad] <- "Other"
  }
  igraph::V(g)$type <- ty
  
  # 2. region (present) --------------------------------------------------------
  if (!"region" %in% igraph::vertex_attr_names(g))
    stop(sprintf("[%s] expected 'region' attribute is missing.", label))
  reg <- igraph::V(g)$region
  reg[is.na(reg)] <- "Unknown"
  igraph::V(g)$region <- as.character(reg)
  
  # 3. location (absent in this schema -> placeholder) -------------------------
  if ("location" %in% igraph::vertex_attr_names(g)) {
    loc <- igraph::V(g)$location
    loc[is.na(loc)] <- LOCATION_FILL
    igraph::V(g)$location <- as.character(loc)
  } else {
    igraph::V(g)$location <- LOCATION_FILL
  }
  
  # 4. drop unused attrs (gender) + pre-computed metrics -----------------------
  keep <- c("name", "type", "region", "location")
  for (a in setdiff(igraph::vertex_attr_names(g), keep))
    g <- igraph::delete_vertex_attr(g, a)
  
  # 5. recompute metrics identically to the example pipeline -------------------
  add_node_metrics(g)
}

# =============================================================================
# Run
# =============================================================================
for (p in NET_FILES) if (!file.exists(p))
  stop("Cannot find file: ", p, " (check path / capitalization).")

real <- lapply(NET_FILES, readRDS)
networks_real <- Map(conform_network, real, names(real))
names(networks_real) <- c("Nonstress", "Stress")   # enforce name + order

# ── Verification ─────────────────────────────────────────────────────────────
TARGET_VATTR <- c("name", "type", "region", "location", "betweenness",
                  "in_strength", "out_strength", "degree_total",
                  "in_degree", "out_degree")
cat("\n== Conformance check ==\n")
for (nm in names(networks_real)) {
  g  <- networks_real[[nm]]
  va <- igraph::vertex_attr_names(g)
  miss  <- setdiff(TARGET_VATTR, va)
  extra <- setdiff(va, TARGET_VATTR)
  ty_lv <- sort(unique(igraph::V(g)$type))
  cat(sprintf("  %-10s nodes=%d edges=%d  vattr_ok=%s  weight=%s\n",
              nm, igraph::vcount(g), igraph::ecount(g),
              length(miss) == 0 && length(extra) == 0,
              "weight" %in% igraph::edge_attr_names(g)))
  if (length(miss))  cat("    MISSING:", paste(miss,  collapse = ", "), "\n")
  if (length(extra)) cat("    EXTRA:  ", paste(extra, collapse = ", "), "\n")
  cat("    type levels:", paste(ty_lv, collapse = ", "), "\n")
  if ("Public" %in% ty_lv)
    cat("    NOTE: 'Public' present -> 02_params.R must define a pHSinit/pHS_0",
        "entry for type 'Public'.\n")
}

saveRDS(networks_real, OUT_FILE)
cat("\n== Saved -> ", OUT_FILE,
    "  (use as `networks` in 05_run_all.R) ==\n", sep = "")