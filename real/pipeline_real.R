# =============================================================================
# pipeline_real.R
# Pipeline for real data on a personal laptop.
#
# This file expects the 6 networks from Objective 1 as .rds files. While
# waiting for those, it builds a placeholder set from the original Peru
# Excel data so the rest of the pipeline can be tested end-to-end.
#
# Each section is labeled so you can run them independently in RStudio.
# =============================================================================

# ── Source all modules ────────────────────────────────────────────────────────
code_dir <- "../code"
for (f in list.files(code_dir, pattern = "^0[0-9]_.*\\.R$", full.names = TRUE))
  source(f)

set.seed(42)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ CONFIGURATION                                                              ║
# ╚════════════════════════════════════════════════════════════════════════════╝

DATA_DIR           <- "./Data"
OUTPUT_DIR         <- "./real_outputs"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

NSEASONS           <- 10
NSIM               <- 100
K_NODES_FRACTION   <- 0.10
RING_SIZE          <- 5
# [SET] Regions with logistics infrastructure for the convenience strategy
ACCESSIBLE_REGIONS <- c("Junin", "Lima")

# ── How to load networks: choose ONE of the two approaches ───────────────────
# OPTION A: Load Objective 1 networks (when available)
# OPTION B: Use original Peru Excel data as placeholder (current state)
USE_OBJECTIVE1_NETWORKS <- FALSE

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ LOAD NETWORKS                                                              ║
# ╚════════════════════════════════════════════════════════════════════════════╝

if (USE_OBJECTIVE1_NETWORKS) {
  # Expected layout: ./networks/real_nonstress.rds, ./networks/real_stress.rds,
  # ./networks/scale_free_nonstress.rds, etc.
  network_files <- c(
    real_nonstress        = "./networks/real_nonstress.rds",
    real_stress           = "./networks/real_stress.rds",
    scale_free_nonstress  = "./networks/scale_free_nonstress.rds",
    scale_free_stress     = "./networks/scale_free_stress.rds",
    small_world_nonstress = "./networks/small_world_nonstress.rds",
    small_world_stress    = "./networks/small_world_stress.rds"
  )
  networks <- lapply(network_files, load_network_rds)
} else {
  # Placeholder: build two networks (stress + nonstress) from the Excel file
  # and generate scale-free / small-world variants matching the node count.
  # This lets you test the pipeline now and swap in real networks later.
  g_nonstress <- build_network_from_excel(
    ties_path   = file.path(DATA_DIR, "TieData_social_network.xlsx"),
    nodes_path  = file.path(DATA_DIR, "NodeData_social_network.xlsx"),
    ties_sheet  = "nonstress_ties",
    nodes_sheet = "nonstress_nodes"
  )
  # [ASK] If "stress_ties" / "stress_nodes" sheets exist in your Excel file,
  # update the sheet names. Otherwise this falls back to the same data.
  g_stress <- tryCatch(
    build_network_from_excel(
      ties_path   = file.path(DATA_DIR, "TieData_social_network.xlsx"),
      nodes_path  = file.path(DATA_DIR, "NodeData_social_network.xlsx"),
      ties_sheet  = "stress_ties",
      nodes_sheet = "stress_nodes"
    ),
    error = function(e) g_nonstress   # fallback if sheets don't exist
  )

  # Quick stand-in generators for scale-free and small-world at matching size
  # Replace with real Objective 1 outputs ASAP.
  n  <- vcount(g_nonstress)
  sf_template <- function(seed) {
    set.seed(seed)
    g_sf <- sample_pa(n, m = 2, directed = TRUE, power = 1.2)
    V(g_sf)$name     <- V(g_nonstress)$name
    V(g_sf)$type     <- V(g_nonstress)$type
    V(g_sf)$region   <- V(g_nonstress)$region
    V(g_sf)$location <- V(g_nonstress)$location
    E(g_sf)$weight   <- sample(5:50, ecount(g_sf), replace = TRUE)
    add_node_metrics(g_sf)
  }
  sw_template <- function(seed) {
    set.seed(seed)
    g_sw <- sample_smallworld(1, n, nei = 3, p = 0.15)
    g_sw <- as_directed(g_sw, mode = "random")
    V(g_sw)$name     <- V(g_nonstress)$name
    V(g_sw)$type     <- V(g_nonstress)$type
    V(g_sw)$region   <- V(g_nonstress)$region
    V(g_sw)$location <- V(g_nonstress)$location
    E(g_sw)$weight   <- sample(5:50, ecount(g_sw), replace = TRUE)
    add_node_metrics(g_sw)
  }

  networks <- list(
    real_nonstress        = g_nonstress,
    real_stress           = g_stress,
    scale_free_nonstress  = sf_template(1001),
    scale_free_stress     = sf_template(1002),
    small_world_nonstress = sw_template(2001),
    small_world_stress    = sw_template(2002)
  )
}

cat("Loaded networks:\n")
for (nm in names(networks)) {
  g <- networks[[nm]]
  cat(sprintf("  %-25s nodes=%d edges=%d\n", nm, vcount(g), ecount(g)))
}

# Common k for all strategies
k_nodes <- max(1L, round(K_NODES_FRACTION * vcount(networks[[1]])))
cat(sprintf("\nk_nodes per strategy: %d (%.0f%% of network)\n",
            k_nodes, K_NODES_FRACTION * 100))


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ OBJECTIVE 2: Baseline epidemic across 18 scenarios                         ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n=== Running Objective 2 ===\n")

obj2 <- run_objective2(networks, base_params,
                       nseasons = NSEASONS, nsim = NSIM)
saveRDS(obj2, file.path(OUTPUT_DIR, "obj2_results.rds"))

p_obj2 <- obj2_heatmap(obj2)
ggsave(file.path(OUTPUT_DIR, "fig_obj2_heatmap.pdf"), p_obj2,
       width = 7, height = 5)

realistic <- select_realistic_scenarios(obj2, n_select = 3)
write.csv(realistic,
          file.path(OUTPUT_DIR, "realistic_scenarios.csv"), row.names = FALSE)
cat("\n3 worst-case scenarios for Objective 3:\n")
print(realistic)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ OBJECTIVE 3: 24 strategy scenarios                                         ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n=== Running Objective 3 ===\n")

obj3 <- run_objective3(networks, realistic, base_params,
                       k_nodes = k_nodes,
                       accessible_regions = ACCESSIBLE_REGIONS,
                       nseasons = NSEASONS, nsim = NSIM,
                       ring_size = RING_SIZE)
saveRDS(obj3, file.path(OUTPUT_DIR, "obj3_results.rds"))

p_obj3 <- obj3_heatmap(obj3)
ggsave(file.path(OUTPUT_DIR, "fig_obj3_heatmap.pdf"), p_obj3,
       width = 12, height = 5)

best <- select_best_distribution_scenarios(obj3)
saveRDS(best, file.path(OUTPUT_DIR, "best_strategies.rds"))
cat("\nBest distribution scenarios (one per strategy):\n")
print(best$best_per_strategy)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ OBJECTIVE 4: 100 risk-sensitivity scenarios                                ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n=== Running Objective 4 ===\n")

obj4 <- run_objective4(networks, best$best_per_strategy, base_params,
                       k_nodes = k_nodes,
                       accessible_regions = ACCESSIBLE_REGIONS,
                       nseasons = NSEASONS, nsim = NSIM,
                       ring_size = RING_SIZE)
saveRDS(obj4, file.path(OUTPUT_DIR, "obj4_results.rds"))

p_obj4 <- obj4_heatmap(obj4)
ggsave(file.path(OUTPUT_DIR, "fig_obj4_heatmap.pdf"), p_obj4,
       width = 12, height = 7)

cat("\n=== Pipeline complete. Outputs in:", OUTPUT_DIR, "===\n")
