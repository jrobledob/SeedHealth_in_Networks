# =============================================================================
# 06_obj3_strategies.R
# Objective 3: Distribution strategies on 3 realistic scenarios × 2 seed
# types × 4 strategies = 24 scenarios. No risk modifiers (perfect delivery).
# =============================================================================

library(dplyr)

# ── Run all 24 strategy scenarios ────────────────────────────────────────────
run_objective3 <- function(networks_list, realistic_scenarios,
                           base_params, k_nodes, accessible_regions,
                           nseasons = 10, nsim = 100, ring_size = 3) {

  intervention_seed_types <- c("certified", "qds")
  strategy_names          <- c("random", "close_ring",
                               "network_metric", "convenience")

  scenarios <- expand.grid(
    network_id = realistic_scenarios$network_id,
    seed_type  = intervention_seed_types,
    strategy   = strategy_names,
    stringsAsFactors = FALSE
  )

  results <- vector("list", nrow(scenarios))

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, ]
    g  <- networks_list[[sc$network_id]]

    cat(sprintf("[Obj3 %d/%d] %s × %s × %s\n",
                i, nrow(scenarios),
                sc$network_id, sc$seed_type, sc$strategy))

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
        availability     = 1, acceptance = 1,   # perfect delivery
        pHSinit_vec      = pHSinit_vec
      ),
      simplify = FALSE
    )

    results[[i]] <- list(scenario = sc, sims = sims)
  }

  list(scenarios = scenarios, results = results)
}

# ── Select the best strategy PER realistic scenario (per user spec) ──────────
# For each (realistic_scenario × strategy), pick the seed type with the
# lowest AUDPC. Then for each of the 4 strategies, identify the (network,
# seed_type) combination that delivers the best result.
select_best_distribution_scenarios <- function(obj3_output) {

  # Compute mean AUDPC per scenario
  summary_df <- do.call(rbind, lapply(obj3_output$results, function(r) {
    audpcs <- sapply(r$sims, function(df) {
      per_season <- df |>
        group_by(season) |>
        summarise(YL = mean(YL), .groups = "drop") |>
        arrange(season)
      yl <- per_season$YL
      if (length(yl) < 2) 0 else sum((yl[-1] + yl[-length(yl)]) / 2)
    })
    data.frame(r$scenario, AUDPC_mean = mean(audpcs), stringsAsFactors = FALSE)
  }))

  # Per (realistic scenario × strategy): pick the seed type with the best
  # (lowest) AUDPC. Yields 3 networks × 4 strategies = 12 rows.
  best_per_strategy_x_network <- summary_df |>
    group_by(network_id, strategy) |>
    slice_min(AUDPC_mean, n = 1, with_ties = FALSE) |>
    ungroup()

  # Then for each of the 4 strategies, pick the network where it performs best
  best_per_strategy <- best_per_strategy_x_network |>
    group_by(strategy) |>
    slice_min(AUDPC_mean, n = 1, with_ties = FALSE) |>
    ungroup()

  list(
    summary              = summary_df,
    best_per_combination = best_per_strategy_x_network,
    best_per_strategy    = best_per_strategy
  )
}
