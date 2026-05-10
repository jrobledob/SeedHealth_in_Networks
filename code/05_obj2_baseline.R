# =============================================================================
# 05_obj2_baseline.R
# Objective 2: Baseline epidemic trajectories across 6 networks × 3 seed types
#
# Output: list of 18 simulation runs, each containing nsim replicates.
# Used to identify the 3 worst-case scenarios for Objective 3.
# =============================================================================

library(dplyr)

# ── Run one (network, seed type) scenario with nsim replicates ───────────────
run_baseline_scenario <- function(g, base_params, seed_type,
                                  nseasons, nsim) {
  mix_matrix  <- compute_mix_matrix(g)
  pHSinit_vec <- init_pHS_by_node_type(g)

  # All nodes start at their type-specific baseline; seed_type only modifies
  # the system character (no intervention occurs in Objective 2 — the seed
  # type just defines which baseline regime the network is operating under)
  # Implementation: scale each node's pHSinit toward seed_pHSinit[seed_type]
  # to reflect the dominant seed system.
  #
  # Simpler interpretation: use node-type baselines AS-IS for "informal",
  # and apply seed-type multipliers for CS/QDS scenarios.
  scale_factor <- seed_pHSinit[seed_type] / seed_pHSinit["informal"]
  pHSinit_vec  <- pHSinit_vec * scale_factor
  pHSinit_vec[pHSinit_vec > 1] <- 1     # clamp without losing names

  replicate(nsim,
    simulate_network(
      g                = g,
      mix_matrix       = mix_matrix,
      base_params      = base_params,
      strategy_fn      = function(g, YL_state) character(0),
      intervention_pHS = NA,
      nseasons         = nseasons,
      availability     = 0, acceptance = 0,
      pHSinit_vec      = pHSinit_vec
    ),
    simplify = FALSE
  )
}

# ── Run full Objective 2 (all 6 networks × 3 seed types = 18 scenarios) ──────
run_objective2 <- function(networks_list, base_params,
                           nseasons = 10, nsim = 100) {

  scenarios <- expand.grid(
    network_id = names(networks_list),
    seed_type  = names(seed_pHSinit),
    stringsAsFactors = FALSE
  )

  results <- vector("list", nrow(scenarios))

  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, ]
    cat(sprintf("[Obj2 %d/%d] %s × %s\n", i, nrow(scenarios),
                sc$network_id, sc$seed_type))

    sims <- run_baseline_scenario(
      g           = networks_list[[sc$network_id]],
      base_params = base_params,
      seed_type   = sc$seed_type,
      nseasons    = nseasons, nsim = nsim
    )
    results[[i]] <- list(scenario = sc, sims = sims)
  }

  list(scenarios = scenarios, results = results)
}

# ── Identify the 3 worst-case informal scenarios ─────────────────────────────
# Ranks all (network, informal) combinations by mean AUDPC across replicates
# and returns the top 3 (highest disease burden).
select_realistic_scenarios <- function(obj2_output, n_select = 3) {
  scores <- sapply(obj2_output$results, function(r) {
    if (r$scenario$seed_type != "informal") return(NA_real_)
    audpcs <- sapply(r$sims, function(df) {
      per_season <- df |>
        group_by(season) |>
        summarise(YL = mean(YL), .groups = "drop") |>
        arrange(season)
      yl <- per_season$YL
      if (length(yl) < 2) 0 else sum((yl[-1] + yl[-length(yl)]) / 2)
    })
    mean(audpcs)
  })

  ord <- order(scores, decreasing = TRUE, na.last = TRUE)
  top <- ord[seq_len(n_select)]

  data.frame(
    rank       = seq_len(n_select),
    network_id = obj2_output$scenarios$network_id[top],
    AUDPC      = scores[top],
    stringsAsFactors = FALSE
  )
}
