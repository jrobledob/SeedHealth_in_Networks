# =============================================================================
# 07_obj4_robustness.R
# Objective 4: Risk-factor sensitivity for the 4 best strategies.
# 4 best strategies × 25 risk combinations (5 availability × 5 acceptance)
# = 100 scenarios.
# =============================================================================

library(dplyr)

# ── Run Objective 4 ──────────────────────────────────────────────────────────
run_objective4 <- function(networks_list, best_strategies,
                           base_params, k_nodes, accessible_regions,
                           nseasons = 10, nsim = 100, ring_size = 3) {

  risk_levels <- c(0, 0.25, 0.50, 0.75, 1.00)
  risk_grid   <- expand.grid(availability = risk_levels,
                             acceptance   = risk_levels)

  scenarios <- merge(best_strategies, risk_grid, by = NULL,
                     stringsAsFactors = FALSE)

  results <- vector("list", nrow(scenarios))

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, ]
    g  <- networks_list[[sc$network_id]]

    cat(sprintf("[Obj4 %d/%d] %s × %s × %s | avail=%.2f acc=%.2f\n",
                i, nrow(scenarios),
                sc$network_id, sc$seed_type, sc$strategy,
                sc$availability, sc$acceptance))

    strategies <- list(
      random         = make_strategy_random(k_nodes),
      close_ring     = make_strategy_close_ring(k_nodes, ring_size),
      network_metric = make_strategy_network_metric(k_nodes),
      convenience    = make_strategy_convenience(k_nodes, accessible_regions)
    )

    mix_matrix  <- compute_mix_matrix(g)
    pHSinit_vec <- init_pHS_by_node_type(g)

    sims <- replicate(nsim,
      simulate_network(
        g                = g,
        mix_matrix       = mix_matrix,
        base_params      = base_params,
        strategy_fn      = strategies[[sc$strategy]],
        intervention_pHS = seed_pHSinit[sc$seed_type],
        nseasons         = nseasons,
        availability     = sc$availability,
        acceptance       = sc$acceptance,
        pHSinit_vec      = pHSinit_vec
      ),
      simplify = FALSE
    )

    results[[i]] <- list(scenario = sc, sims = sims)
  }

  list(scenarios = scenarios, results = results)
}
