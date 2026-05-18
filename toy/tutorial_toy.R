# =============================================================================
# tutorial_toy.R
# STEP-BY-STEP TUTORIAL — runs the entire pipeline on a 12-node toy network
# so every intermediate object can be inspected by hand.
#
# Each STEP prints what it just computed and (where relevant) produces a plot.
# Use this file to confirm the code works as you expect before scaling up.
#
# Run line by line in RStudio, or:  Rscript tutorial_toy.R
# =============================================================================

# ── Source all modules ────────────────────────────────────────────────────────
code_dir <- "./code"
for (f in list.files(code_dir, pattern = "^0[0-9]_.*\\.R$", full.names = TRUE))
  source(f)

set.seed(2024)
dir.create("tutorial_outputs", showWarnings = FALSE)

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 1: BUILD TOY NETWORKS                                                 ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 1: Generate 6 toy networks ════════════════════════════╗\n")

toy_networks <- generate_toy_networks()
cat(sprintf("Generated %d toy networks:\n", length(toy_networks)))
for (nm in names(toy_networks)) {
  g <- toy_networks[[nm]]
  cat(sprintf("  %-25s nodes=%d edges=%d\n", nm, vcount(g), ecount(g)))
}

# Pick one network to inspect in detail
g <- toy_networks$real_nonstress
cat("\nVertex attributes for 'real_nonstress' network:\n")
print(data.frame(
  name   = V(g)$name,
  type   = V(g)$type,
  region = V(g)$region
))

cat("\nEdge list (first 10 rows):\n")
print(head(as_data_frame(g, what = "edges"), 10))

# Plot the network
pdf("tutorial_outputs/step1_networks.pdf", width = 12, height = 8)
par(mfrow = c(2, 3), mar = c(1, 1, 2, 1))
for (nm in names(toy_networks)) {
  gi <- toy_networks[[nm]]
  plot(gi,
       vertex.size = 14, vertex.label = V(gi)$name,
       vertex.label.cex = 0.7,
       vertex.color = c(Seed_specialist = "lightblue", Government = "red",
                        NGO = "orange", Farm_assoc = "cyan",
                        Custodian = "magenta", Farmer = "green",
                        Trader = "yellow", Other = "gray")[V(gi)$type],
       edge.arrow.size = 0.3, main = nm)
}
dev.off()
cat("Saved: tutorial_outputs/step1_networks.pdf\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 2: COMPUTE NODE METRICS                                               ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 2: Compute node-level metrics ═════════════════════════╗\n")

g <- add_node_metrics(toy_networks$real_nonstress)

metrics_df <- data.frame(
  name         = V(g)$name,
  type         = V(g)$type,
  in_strength  = V(g)$in_strength,
  out_strength = V(g)$out_strength,
  betweenness  = round(V(g)$betweenness, 3)
)
print(metrics_df)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 3: ASSIGN INITIAL pHS BY NODE TYPE                                    ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 3: Initial pHS by node type ═══════════════════════════╗\n")

pHSinit_vec <- init_pHS_by_node_type(g)
print(data.frame(
  name = names(pHSinit_vec),
  type = V(g)$type,
  pHS0 = pHSinit_vec
))


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 4: BUILD MIXING MATRIX                                                ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 4: Mixing matrix (volume-weighted) ════════════════════╗\n")
cat("Each column j shows the proportion of node j's incoming seed from each row i.\n")
cat("Diagonal = 1 means the node has no incoming edges (uses own saved seed).\n\n")

mix_matrix <- compute_mix_matrix(g)
print(round(mix_matrix, 2))

cat("\nColumn sums (should all be 1):\n")
print(round(colSums(mix_matrix), 3))


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 5: ONE-SEASON SIMULATION (no intervention)                            ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 5: Simulate one season with no intervention ═══════════╗\n")

sim_baseline <- simulate_network(
  g                = g,
  mix_matrix       = mix_matrix,
  node_params      = node_params,
  strategy_fn      = make_strategy_none(),
  intervention_pHS = NA,
  nseasons         = 10,
  availability     = 0, acceptance = 0,
  pHSinit_vec      = pHSinit_vec
)

cat("First 5 rows (season 1):\n")
print(head(sim_baseline[sim_baseline$season == 1, ]))

cat("\nMean pHS and YL per season (network-wide):\n")
print(sim_baseline |>
  group_by(season) |>
  summarise(mean_pHS = mean(pHS), mean_YL = mean(YL)) |>
  as.data.frame())


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 6: ONE-SEASON SIMULATION (with random intervention)                   ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 6: Simulate with random intervention (CS, k=3) ════════╗\n")

k_nodes <- 3
sim_intervention <- simulate_network(
  g                = g,
  mix_matrix       = mix_matrix,
  node_params      = node_params,
  strategy_fn      = make_strategy_random(k_nodes),
  intervention_pHS = seed_pHSinit["certified"],
  nseasons         = 10,
  availability     = 1, acceptance = 1,
  pHSinit_vec      = pHSinit_vec
)

cat("Nodes that received intervention each season:\n")
print(sim_intervention |>
  filter(intervened) |>
  group_by(season) |>
  summarise(intervened_nodes = paste(node, collapse = ", "),
            .groups = "drop") |>
  as.data.frame())

cat("\nMean YL per season — baseline vs. intervention:\n")
comparison <- sim_baseline |>
  group_by(season) |>
  summarise(baseline_YL = mean(YL)) |>
  left_join(sim_intervention |>
    group_by(season) |>
    summarise(intervention_YL = mean(YL)), by = "season")
print(as.data.frame(comparison))


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 7: COMPARE ALL 4 STRATEGIES ON THE TOY NETWORK                        ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 7: Compare 4 strategies (nsim=20 for speed) ═══════════╗\n")

nsim <- 20

strategies_test <- list(
  baseline       = function(g, YL) character(0),
  random         = make_strategy_random(k_nodes),
  close_ring     = make_strategy_close_ring(k_nodes, ring_size = 2),
  network_metric = make_strategy_network_metric(k_nodes),
  convenience    = make_strategy_convenience(k_nodes, c("Junin", "Lima"))
)

strategy_results <- list()
for (s_name in names(strategies_test)) {
  cat(sprintf("  Running: %s\n", s_name))
  is_baseline <- s_name == "baseline"
  sims <- replicate(nsim,
    simulate_network(
      g                = g,
      mix_matrix       = mix_matrix,
      node_params      = node_params,
      strategy_fn      = strategies_test[[s_name]],
      intervention_pHS = if (is_baseline) NA else seed_pHSinit["certified"],
      nseasons         = 10,
      availability     = if (is_baseline) 0 else 1,
      acceptance       = if (is_baseline) 0 else 1,
      pHSinit_vec      = pHSinit_vec
    ),
    simplify = FALSE
  )
  strategy_results[[s_name]] <- summarize_sims(sims)
}

p <- plot_trajectories(strategy_results,
                       title = "Toy network — strategy comparison")
ggsave("tutorial_outputs/step7_strategies.pdf", p, width = 8, height = 5)
cat("Saved: tutorial_outputs/step7_strategies.pdf\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 8: MINI OBJECTIVE 2 (18 scenarios on toy networks)                    ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 8: Mini Objective 2 — 18 scenarios (nsim=20) ══════════╗\n")

obj2_toy <- run_objective2(toy_networks, base_params,
                           nseasons = 10, nsim = 20)

# Heatmap of cumulative YL
p2 <- obj2_heatmap(obj2_toy)
ggsave("tutorial_outputs/step8_obj2_heatmap.pdf", p2, width = 7, height = 5)
cat("Saved: tutorial_outputs/step8_obj2_heatmap.pdf\n")

# Identify 3 worst-case scenarios
realistic <- select_realistic_scenarios(obj2_toy, n_select = 3)
cat("\n3 worst-case (informal) scenarios for Objective 3:\n")
print(realistic)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 9: MINI OBJECTIVE 3 (24 scenarios)                                    ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 9: Mini Objective 3 — 24 scenarios (nsim=20) ══════════╗\n")

obj3_toy <- run_objective3(toy_networks, realistic, base_params,
                           k_nodes = k_nodes,
                           accessible_regions = c("Junin", "Lima"),
                           nseasons = 10, nsim = 20, ring_size = 2)

p3 <- obj3_heatmap(obj3_toy)
ggsave("tutorial_outputs/step9_obj3_heatmap.pdf", p3, width = 12, height = 5)
cat("Saved: tutorial_outputs/step9_obj3_heatmap.pdf\n")

# Select best strategies for Objective 4
best <- select_best_distribution_scenarios(obj3_toy)
cat("\nBest distribution scenarios (one per strategy):\n")
print(best$best_per_strategy)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║ STEP 10: MINI OBJECTIVE 4 (100 scenarios)                                  ║
# ╚════════════════════════════════════════════════════════════════════════════╝
cat("\n╔════ STEP 10: Mini Objective 4 — 100 scenarios (nsim=10) ════════╗\n")

obj4_toy <- run_objective4(toy_networks, best$best_per_strategy,
                           base_params,
                           k_nodes = k_nodes,
                           accessible_regions = c("Junin", "Lima"),
                           nseasons = 10, nsim = 10, ring_size = 2)

p4 <- obj4_heatmap(obj4_toy)
ggsave("tutorial_outputs/step10_obj4_heatmap.pdf", p4, width = 12, height = 7)
cat("Saved: tutorial_outputs/step10_obj4_heatmap.pdf\n")

cat("\n╔════════════════════════════════════════════════════════════════════╗\n")
cat("║ Tutorial complete. All outputs in: tutorial_outputs/               ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
