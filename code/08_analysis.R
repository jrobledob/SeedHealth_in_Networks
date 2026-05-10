# =============================================================================
# 08_analysis.R
# Summary statistics and figures
#
# Primary metric: AUDPC (area under disease progress curve)
#   AUDPC = sum_{t=1}^{T-1} (YL[t] + YL[t+1]) / 2
# This is the standard plant-pathology summary of total disease burden over
# a multi-season trajectory.
#
# Secondary metric: final-season yield loss (YL at season T)
# Tertiary metric: final-season pHS (proportion of healthy seed at season T)
# =============================================================================

library(dplyr)
library(ggplot2)
library(igraph)

# ── AUDPC for a single trajectory ─────────────────────────────────────────────
audpc <- function(y) {
  if (length(y) < 2) return(0)
  sum((y[-1] + y[-length(y)]) / 2)
}

# ── Per-replicate summary scalars from one simulation data frame ─────────────
# Returns a one-row data frame with AUDPC, final_YL, final_pHS for the
# network-mean trajectory of one replicate.
sim_metrics <- function(df) {
  per_season <- df |>
    group_by(season) |>
    summarise(YL = mean(YL), pHS = mean(pHS), .groups = "drop") |>
    arrange(season)
  data.frame(
    AUDPC     = audpc(per_season$YL),
    final_YL  = tail(per_season$YL,  1),
    final_pHS = tail(per_season$pHS, 1)
  )
}

# ── Summarize one scenario across nsim replicates ────────────────────────────
# Returns BOTH (a) per-season trajectory summary and (b) scalar metric summary.
summarize_sims <- function(sims) {

  trajectory <- bind_rows(lapply(seq_along(sims), function(i) {
    mutate(sims[[i]], sim_id = i)
  })) |>
    group_by(sim_id, season) |>
    summarise(mean_YL  = mean(YL),
              mean_pHS = mean(pHS), .groups = "drop") |>
    group_by(season) |>
    summarise(
      YL_mean  = mean(mean_YL),
      YL_q05   = quantile(mean_YL, 0.05),
      YL_q95   = quantile(mean_YL, 0.95),
      pHS_mean = mean(mean_pHS),
      .groups  = "drop"
    )

  metrics_per_rep <- bind_rows(lapply(sims, sim_metrics))
  metrics_summary <- data.frame(
    AUDPC_mean     = mean(metrics_per_rep$AUDPC),
    AUDPC_sd       = sd(metrics_per_rep$AUDPC),
    final_YL_mean  = mean(metrics_per_rep$final_YL),
    final_YL_sd    = sd(metrics_per_rep$final_YL),
    final_pHS_mean = mean(metrics_per_rep$final_pHS)
  )

  list(trajectory = trajectory, metrics = metrics_summary)
}

# ── Trajectory plot (mean ± 90% CI) ──────────────────────────────────────────
# summary_list: a named list where each element is a `summarize_sims()` output
plot_trajectories <- function(summary_list, title = NULL) {
  df <- bind_rows(lapply(names(summary_list), function(nm) {
    mutate(summary_list[[nm]]$trajectory, scenario = nm)
  }))

  ggplot(df, aes(season, YL_mean, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = YL_q05, ymax = YL_q95), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    scale_x_continuous(breaks = seq_len(max(df$season))) +
    theme_classic(base_size = 12) +
    labs(x = "Season", y = "Mean Network Yield Loss (%)",
         title = title)
}

# ── Helper: compute scenario-level metric table from a run_objectiveX output ─
# Returns a data frame with one row per scenario, joined with scenario metadata
# and three metrics (AUDPC, final_YL, final_pHS). Any pre-existing metric
# columns carried over from upstream selectors are dropped to prevent
# duplicate column names.
scenario_metric_table <- function(obj_output) {
  drop_cols <- c("AUDPC_mean", "final_YL_mean", "final_pHS_mean", "cum_YL_mean")

  do.call(rbind, lapply(obj_output$results, function(r) {
    sc <- r$scenario
    sc <- sc[, !colnames(sc) %in% drop_cols, drop = FALSE]

    metrics_per_rep <- bind_rows(lapply(r$sims, sim_metrics))
    cbind(
      sc,
      AUDPC_mean     = mean(metrics_per_rep$AUDPC),
      final_YL_mean  = mean(metrics_per_rep$final_YL),
      final_pHS_mean = mean(metrics_per_rep$final_pHS),
      stringsAsFactors = FALSE
    )
  }))
}

# ── Objective 2: heatmap of AUDPC across 18 scenarios ────────────────────────
# `metric` selects which scalar to display: "AUDPC_mean" or "final_YL_mean"
obj2_heatmap <- function(obj2_output, metric = "AUDPC_mean") {
  df <- scenario_metric_table(obj2_output)
  df$value <- df[[metric]]

  metric_label <- switch(metric,
    AUDPC_mean    = "AUDPC",
    final_YL_mean = "Final-season YL (%)",
    metric)

  ggplot(df, aes(seed_type, network_id, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", value)),
              color = "white", size = 3.5) +
    scale_fill_viridis_c(option = "C", direction = -1, name = metric_label) +
    theme_minimal(base_size = 12) +
    labs(x = "Seed Type", y = "Network",
         title = sprintf("Objective 2: %s by network and seed type", metric_label))
}

# ── Objective 3: heatmap of AUDPC across 24 scenarios ────────────────────────
obj3_heatmap <- function(obj3_output, metric = "AUDPC_mean") {
  df <- scenario_metric_table(obj3_output)
  df$value <- df[[metric]]

  metric_label <- switch(metric,
    AUDPC_mean    = "AUDPC",
    final_YL_mean = "Final-season YL (%)",
    metric)

  ggplot(df, aes(strategy, seed_type, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", value)),
              color = "white", size = 3) +
    facet_wrap(~ network_id, nrow = 1) +
    scale_fill_viridis_c(option = "C", direction = -1, name = metric_label) +
    theme_minimal(base_size = 11) +
    labs(x = "Strategy", y = "Seed Type",
         title = sprintf("Objective 3: %s by network, seed type, and strategy",
                         metric_label)) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# ── Objective 4: risk-factor heatmap, one panel per best strategy ────────────
obj4_heatmap <- function(obj4_output, metric = "AUDPC_mean") {
  df <- scenario_metric_table(obj4_output)
  df$value <- df[[metric]]
  df$panel <- paste(df$strategy, df$seed_type, df$network_id, sep = " | ")

  metric_label <- switch(metric,
    AUDPC_mean    = "AUDPC",
    final_YL_mean = "Final-season YL (%)",
    metric)

  ggplot(df, aes(availability, acceptance, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", value)),
              color = "white", size = 3) +
    facet_wrap(~ panel, nrow = 2) +
    scale_fill_viridis_c(option = "B", direction = -1, name = metric_label) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme_minimal(base_size = 11) +
    labs(x = "Seed Availability", y = "Willingness to Accept",
         title = sprintf("Objective 4: %s by risk modifiers", metric_label))
}

# ── Network visualization: nodes colored by YL or pHS in a given season ──────
plot_network_state <- function(g, sim_df, season_t,
                               attribute = "YL", layout = NULL) {
  df <- sim_df[sim_df$season == season_t, ]
  V(g)$val <- df[[attribute]][match(V(g)$name, df$node)]
  if (is.null(layout)) layout <- layout_with_fr(g)

  val_norm <- V(g)$val / max(V(g)$val + 1e-9)
  colors   <- hcl.colors(100, "YlOrRd")[ceiling(val_norm * 99) + 1]

  plot(g,
       vertex.size        = 8 + val_norm * 12,
       vertex.color       = colors,
       vertex.label       = V(g)$name,
       vertex.label.cex   = 0.6,
       vertex.label.color = "black",
       edge.arrow.size    = 0.2,
       edge.color         = rgb(0, 0, 0, 0.25),
       layout             = layout,
       main               = sprintf("Season %d — %s", season_t, attribute))
}
