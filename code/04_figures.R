# =============================================================================
# 04_figures.R  (pipeline v3)
# Builds Figures 1-3 as ggplot/patchwork objects and saves them as PDF and as
# an editable PowerPoint (vector shapes/text via officer + rvg).
#
# Visual encoding shared across figures:
#   node SHAPE  = node type
#   node COLOR  = node region        (mapped to the point color, so all 8
#                 shapes work -- only 5 ggplot shapes accept a separate fill)
#   node SIZE   = total degree
#   edge WIDTH  = transaction volume (kg)
#   node ALPHA  = yield loss (%)      (Figure 2 only)
#
# NOTE: edges are drawn without arrowheads to keep the 24-panel Figure 2
# legible. Direction is preserved in the data and in the mixing matrix; set
# DRAW_ARROWS <- TRUE below to turn arrowheads on.
#
# Requires ggplot2 >= 3.4 (uses the `linewidth` aesthetic / scale).
# =============================================================================

library(ggplot2)
library(dplyr)
library(igraph)
library(patchwork)

DRAW_ARROWS <- FALSE

# ── Palettes (edit freely; also editable later in PowerPoint) ────────────────
REGION_COLORS <- c(
  Huancavelica = "#1b9e77", Pasco = "#d95f02", Junin = "#7570b3",
  Huanuco      = "#e7298a", Apurimac = "#66a61e", Ayacucho = "#e6ab02",
  Lima         = "#a6761d", Unknown = "#999999"
)

# 8 distinguishable plotting characters; color shows on all of them.
TYPE_SHAPES <- c(
  Seed_specialist = 15, Farmer = 16, Custodian = 17, Other = 18,
  Farm_assoc      = 3,  Government = 4, Trader = 8, NGO = 13
)

SCEN_COLORS <- c(
  "Informal (0.7)"       = "#b2182b",
  "QDS moderate (0.8)"   = "#ef8a62",
  "QDS optimistic (0.9)" = "#2166ac"
)

.edge_arrow <- if (DRAW_ARROWS) grid::arrow(length = grid::unit(0.06, "inches"),
                                            type = "closed") else NULL

# ── Layout + node/edge frames ────────────────────────────────────────────────
# Default layout algorithm. "graphopt" uses node-charge repulsion and spreads
# dense networks far better than Fruchterman-Reingold. Alternatives accepted by
# make_layout(): "fr", "kk" (Kamada-Kawai), "drl", "nicely".
DEFAULT_LAYOUT_ALGO <- "graphopt"

# Normalize a layout matrix to a fixed [0, span] square so node POINT sizes
# (which are in mm, independent of data units) sit on a predictable canvas and
# coord_equal() behaves consistently across networks.
.scale_coords <- function(lay, span = 100) {
  mins <- apply(lay, 2, min)
  rng  <- apply(lay, 2, function(z) diff(range(z)))
  rng[rng == 0] <- 1
  sweep(sweep(lay, 2, mins, "-"), 2, rng, "/") * span
}

# One fixed layout per network so node positions are stable across all panels.
make_layout <- function(g, algo = DEFAULT_LAYOUT_ALGO, seed = 42) {
  set.seed(seed)
  lay <- switch(
    algo,
    graphopt = igraph::layout_with_graphopt(g, niter = 1500, charge = 0.05,
                                            mass = 30, spring.length = 0,
                                            spring.constant = 1),
    fr       = igraph::layout_with_fr(g, niter = 5000),
    kk       = igraph::layout_with_kk(g),
    drl      = igraph::layout_with_drl(g),
    nicely   = igraph::layout_nicely(g),
    igraph::layout_with_graphopt(g)            # fallback
  )
  lay <- .scale_coords(lay)
  data.frame(
    name   = igraph::V(g)$name,
    x      = lay[, 1], y = lay[, 2],
    type   = igraph::V(g)$type,
    region = igraph::V(g)$region,
    degree = igraph::degree(g, mode = "all"),
    stringsAsFactors = FALSE
  )
}

make_edges <- function(g, nf) {
  el <- igraph::as_data_frame(g, what = "edges")   # from, to, weight
  data.frame(
    x      = nf$x[match(el$from, nf$name)],
    y      = nf$y[match(el$from, nf$name)],
    xend   = nf$x[match(el$to,   nf$name)],
    yend   = nf$y[match(el$to,   nf$name)],
    weight = el$weight,
    stringsAsFactors = FALSE
  )
}

# ── Single network panel (Figure 1) ──────────────────────────────────────────
draw_network <- function(nf, ef, title) {
  ggplot() +
    geom_segment(data = ef,
                 aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight),
                 color = "grey55", alpha = 0.35, lineend = "round",
                 arrow = .edge_arrow) +
    geom_point(data = nf,
               aes(x = x, y = y, shape = type, color = region, size = degree)) +
    scale_shape_manual(values = TYPE_SHAPES, name = "Node type") +
    scale_color_manual(values = REGION_COLORS, name = "Region") +
    scale_size_continuous(range = c(2, 7.5), name = "Degree (total)") +
    scale_linewidth_continuous(range = c(0.2, 1.8), name = "Edge volume (kg)") +
    coord_equal() +
    theme_void(base_size = 11) +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold")) +
    guides(color = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3)))
}

# ── Network summary metrics table (Figure 1, column 2) ───────────────────────
.safe <- function(expr) tryCatch(expr, error = function(e) NA_real_)

metrics_table <- function(g) {
  deg  <- igraph::degree(g, mode = "all")
  data.frame(
    metric = c("Nodes", "Edges", "Density", "Reciprocity",
               "Mean degree (total)", "Degree centralization",
               "Mean betweenness", "Components (weak)",
               "Largest component", "Mean path length",
               "Transitivity (global)", "Assortativity (region)",
               "Assortativity (type)", "Total volume (kg)",
               "Mean edge volume (kg)"),
    value = c(
      formatC(igraph::vcount(g), format = "d", big.mark = ","),
      formatC(igraph::ecount(g), format = "d", big.mark = ","),
      sprintf("%.3f", .safe(igraph::edge_density(g))),
      sprintf("%.3f", .safe(igraph::reciprocity(g))),
      sprintf("%.2f", mean(deg)),
      sprintf("%.3f", .safe(igraph::centr_degree(g, mode = "all")$centralization)),
      sprintf("%.3f", .safe(mean(igraph::betweenness(g, normalized = TRUE)))),
      formatC(.safe(igraph::components(g, mode = "weak")$no), format = "d"),
      formatC(.safe(max(igraph::components(g, mode = "weak")$csize)), format = "d"),
      sprintf("%.2f", .safe(igraph::mean_distance(g, directed = TRUE, unconnected = TRUE))),
      sprintf("%.3f", .safe(igraph::transitivity(g, type = "global"))),
      sprintf("%.3f", .safe(igraph::assortativity_nominal(g, as.integer(factor(igraph::V(g)$region)), directed = TRUE))),
      sprintf("%.3f", .safe(igraph::assortativity_nominal(g, as.integer(factor(igraph::V(g)$type)),   directed = TRUE))),
      formatC(.safe(sum(igraph::E(g)$weight)), format = "f", digits = 0, big.mark = ","),
      sprintf("%.1f", .safe(mean(igraph::E(g)$weight)))
    ),
    stringsAsFactors = FALSE
  )
}

metrics_panel <- function(mt, title) {
  mt$y <- rev(seq_len(nrow(mt)))
  ggplot(mt) +
    geom_text(aes(x = 0,    y = y, label = metric), hjust = 0, size = 3.3) +
    geom_text(aes(x = 1.05, y = y, label = value),  hjust = 1, size = 3.3,
              fontface = "bold") +
    scale_x_continuous(limits = c(-0.05, 1.10)) +
    scale_y_continuous(limits = c(0.4, nrow(mt) + 1.2)) +
    ggtitle(title) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0))
}

# ── FIGURE 1 ──────────────────────────────────────────────────────────────────
build_fig1 <- function(networks, layouts, edges_store) {
  net_ns <- draw_network(layouts[["Nonstress"]], edges_store[["Nonstress"]],
                         "Nonstress network")
  net_s  <- draw_network(layouts[["Stress"]],    edges_store[["Stress"]],
                         "Stress network")
  mp_ns  <- metrics_panel(metrics_table(networks[["Nonstress"]]),
                          "Nonstress — summary metrics")
  mp_s   <- metrics_panel(metrics_table(networks[["Stress"]]),
                          "Stress — summary metrics")

  (net_ns + mp_ns + net_s + mp_s) +
    plot_layout(ncol = 2, widths = c(2, 1), guides = "collect") +
    plot_annotation(
      title = "Figure 1. Potato seed-exchange networks and summary metrics",
      theme = theme(plot.title = element_text(face = "bold", size = 14)))
}

# ── FIGURE 2 ──────────────────────────────────────────────────────────────────
# 6 rows (3 seed scenarios x {Nonstress, Stress}) x 4 cycles. Node alpha = mean
# yield loss across replicates, on a single shared scale.
build_fig2 <- function(sims_store, layouts, edges_store, scen_df,
                       cycles = c(1, 4, 7, 10)) {
  net_names <- names(sims_store)          # c("Nonstress", "Stress")
  cyc_levels <- paste("Cycle", cycles)
  rl <- function(scen, net) paste(scen, net, sep = " | ")   # single source of truth

  node_rows <- list(); edge_rows <- list()
  for (net in net_names) {
    nf <- layouts[[net]]; ef <- edges_store[[net]]
    for (scen in scen_df$scenario) {
      sims <- sims_store[[net]][[scen]]
      mean_yl <- dplyr::bind_rows(sims) |>
        dplyr::filter(season %in% cycles) |>
        dplyr::group_by(node, season) |>
        dplyr::summarise(meanYL = mean(YL), .groups = "drop")

      m <- merge(mean_yl, nf, by.x = "node", by.y = "name")
      m$row_label <- rl(scen, net)
      m$cycle_lab <- factor(paste("Cycle", m$season), levels = cyc_levels)
      node_rows[[paste(net, scen)]] <- m

      for (cy in cycles) {
        e <- ef
        e$row_label <- rl(scen, net)
        e$cycle_lab <- factor(paste("Cycle", cy), levels = cyc_levels)
        edge_rows[[paste(net, scen, cy)]] <- e
      }
    }
  }
  nodes <- dplyr::bind_rows(node_rows)
  edges <- dplyr::bind_rows(edge_rows)

  # Row order: for each scenario, Nonstress then Stress.
  row_levels <- unlist(lapply(scen_df$scenario,
                              function(s) rl(s, net_names)))
  nodes$row_label <- factor(nodes$row_label, levels = row_levels)
  edges$row_label <- factor(edges$row_label, levels = row_levels)

  gmax <- max(nodes$meanYL, na.rm = TRUE)

  ggplot() +
    geom_segment(data = edges,
                 aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight),
                 color = "grey60", alpha = 0.18, lineend = "round") +
    geom_point(data = nodes,
               aes(x = x, y = y, shape = type, color = region,
                   size = degree, alpha = meanYL)) +
    facet_grid(rows = vars(row_label), cols = vars(cycle_lab), switch = "y") +
    scale_shape_manual(values = TYPE_SHAPES, name = "Node type") +
    scale_color_manual(values = REGION_COLORS, name = "Region") +
    scale_size_continuous(range = c(1, 4), guide = "none") +
    scale_linewidth_continuous(range = c(0.15, 1.1), guide = "none") +
    scale_alpha_continuous(range = c(0.12, 1), limits = c(0, gmax),
                           name = "Yield loss (%)") +
    coord_equal() +
    theme_void(base_size = 10) +
    theme(
      strip.text.y.left = element_text(angle = 0, size = 8, hjust = 1),
      strip.text.x      = element_text(size = 9, face = "bold"),
      legend.position   = "right",
      plot.title        = element_text(face = "bold", size = 14)) +
    labs(title = "Figure 2. Seed-health transmission across cycles (node opacity = yield loss)") +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)),
           shape = guide_legend(override.aes = list(size = 3, alpha = 1)))
}

# ── FIGURE 3 ──────────────────────────────────────────────────────────────────
# Two panels (Stress left, Nonstress right). Mean network yield loss per cycle
# with a 5-95% band for each seed scenario; mean AUDPC annotated per scenario.
build_fig3 <- function(sims_store, scen_df, nseasons) {
  net_names <- names(sims_store)
  traj_rows <- list(); aud_rows <- list()

  for (net in net_names) for (scen in scen_df$scenario) {
    sims <- sims_store[[net]][[scen]]

    per_rep <- dplyr::bind_rows(lapply(seq_along(sims), function(i) {
      sims[[i]] |>
        dplyr::group_by(season) |>
        dplyr::summarise(mYL = mean(YL), .groups = "drop") |>
        dplyr::mutate(sim_id = i)
    }))

    traj <- per_rep |>
      dplyr::group_by(season) |>
      dplyr::summarise(YL_mean = mean(mYL),
                       q05 = quantile(mYL, 0.05),
                       q95 = quantile(mYL, 0.95), .groups = "drop")
    traj$network <- net; traj$scenario <- scen
    traj_rows[[paste(net, scen)]] <- traj

    aud <- vapply(split(per_rep, per_rep$sim_id), function(d) {
      d <- d[order(d$season), ]; y <- d$mYL
      if (length(y) < 2) 0 else sum((y[-1] + y[-length(y)]) / 2)
    }, numeric(1))
    aud_rows[[paste(net, scen)]] <-
      data.frame(network = net, scenario = scen, AUDPC = mean(aud))
  }

  df <- dplyr::bind_rows(traj_rows)
  ad <- dplyr::bind_rows(aud_rows)

  df$network  <- factor(df$network,  levels = c("Stress", "Nonstress"))
  ad$network  <- factor(ad$network,  levels = c("Stress", "Nonstress"))
  df$scenario <- factor(df$scenario, levels = scen_df$scenario)
  ad$scenario <- factor(ad$scenario, levels = scen_df$scenario)

  ymax <- max(df$q95, na.rm = TRUE)
  ad$yy  <- ymax * (1 - 0.07 * (as.integer(ad$scenario) - 1))
  ad$lab <- sprintf("%s: AUDPC = %.0f", as.character(ad$scenario), ad$AUDPC)

  ggplot(df, aes(season, YL_mean, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.3) +
    geom_text(data = ad, aes(x = 1, y = yy, label = lab, color = scenario),
              hjust = 0, size = 3.3, inherit.aes = FALSE, show.legend = FALSE) +
    facet_wrap(~ network, ncol = 2) +
    scale_color_manual(values = SCEN_COLORS, name = "Seed scenario") +
    scale_fill_manual(values = SCEN_COLORS, name = "Seed scenario") +
    scale_x_continuous(breaks = seq_len(nseasons)) +
    labs(x = "Cycle (season)", y = "Mean network yield loss (%)",
         title = "Figure 3. Epidemic trajectories by seed scenario (mean, 5-95% band)") +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

# ── Grouped AUDPC trajectories (Figures 4-6) ─────────────────────────────────
# Same idea as Figure 3, but the per-replicate trajectory is averaged WITHIN a
# group (node type, region, or region x type) instead of across the whole
# network. Columns are still the two networks (Stress, Nonstress); rows are the
# groups; color is the seed scenario; the 5-95% band is across replicates; and
# the mean AUDPC per scenario is annotated in each panel.

.audpc_vec <- function(season, y) {
  o <- order(season); y <- y[o]
  if (length(y) < 2) return(0)
  sum((y[-1] + y[-length(y)]) / 2)
}

# Build trajectory + AUDPC tables grouped by `group_vars` (subset of
# c("type","region")). Returns list(traj=, aud=).
audpc_by_group <- function(sims_store, networks, scen_df, group_vars) {
  traj_rows <- list(); aud_rows <- list()

  for (net in names(sims_store)) {
    at <- data.frame(
      node   = igraph::V(networks[[net]])$name,
      type   = igraph::V(networks[[net]])$type,
      region = igraph::V(networks[[net]])$region,
      stringsAsFactors = FALSE
    )
    for (scen in scen_df$scenario) {
      sims <- sims_store[[net]][[scen]]

      per_rep <- dplyr::bind_rows(lapply(seq_along(sims), function(i) {
        d <- merge(sims[[i]], at, by = "node")
        d |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)), season) |>
          dplyr::summarise(mYL = mean(YL), .groups = "drop") |>
          dplyr::mutate(sim_id = i)
      }))

      traj <- per_rep |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)), season) |>
        dplyr::summarise(YL_mean = mean(mYL),
                         q05 = quantile(mYL, 0.05),
                         q95 = quantile(mYL, 0.95), .groups = "drop")
      traj$network <- net; traj$scenario <- scen
      traj_rows[[paste(net, scen)]] <- traj

      aud <- per_rep |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)), sim_id) |>
        dplyr::summarise(A = .audpc_vec(season, mYL), .groups = "drop") |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
        dplyr::summarise(AUDPC = mean(A), .groups = "drop")
      aud$network <- net; aud$scenario <- scen
      aud_rows[[paste(net, scen)]] <- aud
    }
  }
  list(traj = dplyr::bind_rows(traj_rows),
       aud  = dplyr::bind_rows(aud_rows))
}

# Generic plotter: rows = `row_var` (a single column in both tables), cols =
# network, color = scenario. AUDPC annotated per panel (toggle with annotate).
plot_audpc_grouped <- function(bundle, scen_df, nseasons, row_var, title,
                               annotate = TRUE, strip_size = 8,
                               text_size = 2.2) {
  traj <- bundle$traj; aud <- bundle$aud

  traj$network  <- factor(traj$network,  levels = c("Stress", "Nonstress"))
  aud$network   <- factor(aud$network,   levels = c("Stress", "Nonstress"))
  traj$scenario <- factor(traj$scenario, levels = scen_df$scenario)
  aud$scenario  <- factor(aud$scenario,  levels = scen_df$scenario)

  ymax    <- max(traj$q95, na.rm = TRUE)
  aud$yy  <- ymax * (1 - 0.13 * (as.integer(aud$scenario) - 1))
  aud$lab <- sprintf("%s: AUDPC %.0f", as.character(aud$scenario), aud$AUDPC)

  p <- ggplot(traj, aes(season, YL_mean, color = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.7) +
    facet_grid(rows = vars(.data[[row_var]]), cols = vars(network)) +
    scale_color_manual(values = SCEN_COLORS, name = "Seed scenario") +
    scale_fill_manual(values = SCEN_COLORS, name = "Seed scenario") +
    scale_x_continuous(breaks = seq_len(nseasons)) +
    labs(x = "Cycle (season)", y = "Mean yield loss (%)", title = title) +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(face = "bold"),
          strip.text.y = element_text(size = strip_size, angle = 0),
          panel.grid.minor = element_blank())

  if (annotate)
    p <- p + geom_text(data = aud,
                       aes(x = 1, y = yy, label = lab, color = scenario),
                       hjust = 0, size = text_size, inherit.aes = FALSE,
                       show.legend = FALSE)
  p
}

# Attach suggested PDF dimensions so the driver can size tall figures sensibly.
.with_dims <- function(p, width, height) { attr(p, "dims") <- c(width = width, height = height); p }
dims_of    <- function(p, default = c(width = 9, height = 7)) {
  d <- attr(p, "dims"); if (is.null(d)) default else d
}

# FIGURE 4 — AUDPC by node type
build_fig4 <- function(sims_store, networks, scen_df, nseasons) {
  b <- audpc_by_group(sims_store, networks, scen_df, "type")
  lv <- TYPES_ORDER[TYPES_ORDER %in% b$traj$type]
  b$traj$type <- factor(b$traj$type, levels = lv)
  b$aud$type  <- factor(b$aud$type,  levels = lv)
  p <- plot_audpc_grouped(
    b, scen_df, nseasons, "type",
    "Figure 4. AUDPC trajectories by node type (mean, 5-95% band)")
  .with_dims(p, width = 9, height = max(6, 1.9 * length(lv) + 1.5))
}

# FIGURE 5 — AUDPC by region
build_fig5 <- function(sims_store, networks, scen_df, nseasons) {
  b <- audpc_by_group(sims_store, networks, scen_df, "region")
  lv <- rownames(CATEGORY_MATRIX)[rownames(CATEGORY_MATRIX) %in% b$traj$region]
  lv <- c(lv, setdiff(unique(b$traj$region), lv))   # keep any unexpected regions
  b$traj$region <- factor(b$traj$region, levels = lv)
  b$aud$region  <- factor(b$aud$region,  levels = lv)
  p <- plot_audpc_grouped(
    b, scen_df, nseasons, "region",
    "Figure 5. AUDPC trajectories by region (mean, 5-95% band)")
  .with_dims(p, width = 9, height = max(6, 1.9 * length(lv) + 1.5))
}

# FIGURE 6 — AUDPC by region x node type (only combinations that occur)
build_fig6 <- function(sims_store, networks, scen_df, nseasons) {
  b <- audpc_by_group(sims_store, networks, scen_df, c("region", "type"))
  b$traj$grp <- paste(b$traj$region, b$traj$type, sep = " | ")
  b$aud$grp  <- paste(b$aud$region,  b$aud$type,  sep = " | ")

  key <- unique(b$traj[, c("region", "type", "grp")])
  ord <- key$grp[order(match(key$region, rownames(CATEGORY_MATRIX)),
                       match(key$type,   TYPES_ORDER))]
  b$traj$grp <- factor(b$traj$grp, levels = ord)
  b$aud$grp  <- factor(b$aud$grp,  levels = ord)

  p <- plot_audpc_grouped(
    b, scen_df, nseasons, "grp",
    "Figure 6. AUDPC trajectories by region x node type (mean, 5-95% band)",
    strip_size = 7, text_size = 2.0)
  .with_dims(p, width = 9.5, height = max(7, 1.5 * length(ord) + 1.5))
}

# ── Save helpers: PDF + editable PowerPoint ──────────────────────────────────
save_pdf <- function(p, path, width, height) {
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  device = grDevices::cairo_pdf, limitsize = FALSE)
  cat("Saved:", path, "\n")
}

# Scale (w,h) down to fit within a slide while preserving aspect ratio.
fit_to_slide <- function(w, h, max_w = 9.5, max_h = 7.0) {
  s <- min(max_w / w, max_h / h, 1)
  c(width = w * s, height = h * s)
}

# items: list of list(plot=, width=, height=). One editable slide per item.
save_pptx <- function(items, path) {
  if (!requireNamespace("officer", quietly = TRUE) ||
      !requireNamespace("rvg", quietly = TRUE))
    stop("Install the 'officer' and 'rvg' packages for editable PowerPoint output.")

  ppt <- officer::read_pptx()
  for (it in items) {
    fd <- fit_to_slide(it$width, it$height)
    ppt <- officer::add_slide(ppt, layout = "Blank", master = "Office Theme")
    vec <- rvg::dml(code = print(it$plot))
    ppt <- officer::ph_with(
      ppt, vec,
      location = officer::ph_location(left = 0.2, top = 0.2,
                                      width = fd["width"], height = fd["height"]))
  }
  print(ppt, target = path)
  cat("Saved:", path, "\n")
}
