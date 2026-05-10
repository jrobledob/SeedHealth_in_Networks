# =============================================================================
# 03_strategies.R
# Distribution strategy factories
#
# Each factory returns a function f(g, YL_state) -> character vector of node
# names selected for intervention this season. k is fixed across strategies
# so the operational footprint is identical (per Objective 3 requirement).
# =============================================================================

# ── Baseline: no intervention ─────────────────────────────────────────────────
make_strategy_none <- function() {
  function(g, YL_state) character(0)
}

# ── Random allocation (null model) ────────────────────────────────────────────
make_strategy_random <- function(k) {
  function(g, YL_state) sample(V(g)$name, min(k, vcount(g)))
}

# ── Surveillance-guided ring: target neighbors of highest-YL nodes ───────────
# Identifies the `ring_size` nodes with the highest current yield loss and
# selects their first-order network neighbors for intervention. At t = 1
# (no YL history) random jitter breaks ties.
make_strategy_close_ring <- function(k, ring_size = 3) {
  function(g, YL_state) {
    node_names <- V(g)$name
    yl         <- YL_state[node_names] + runif(length(node_names), 0, 1e-9)
    epicenters <- node_names[order(yl, decreasing = TRUE)][1:min(ring_size, length(node_names))]

    ring_nodes <- unique(unlist(lapply(epicenters, function(v) {
      V(g)[neighbors(g, v, mode = "all")]$name
    })))
    ring_nodes <- setdiff(ring_nodes, epicenters)

    if (length(ring_nodes) > k) ring_nodes <- sample(ring_nodes, k)
    if (length(ring_nodes) < k) {
      # If ring is smaller than k, fill with random non-epicenter nodes
      remaining <- setdiff(node_names, c(ring_nodes, epicenters))
      fill_n    <- min(k - length(ring_nodes), length(remaining))
      if (fill_n > 0) ring_nodes <- c(ring_nodes, sample(remaining, fill_n))
    }
    ring_nodes
  }
}

# ── Network-metric: top-k by betweenness centrality ──────────────────────────
make_strategy_network_metric <- function(k, metric = "betweenness") {
  function(g, YL_state) {
    node_names <- V(g)$name
    vals       <- vertex_attr(g, metric)
    if (is.null(vals) || length(vals) != length(node_names))
      stop(sprintf("Network missing vertex attribute '%s'. Run add_node_metrics(g) first.", metric))
    node_names[order(vals, decreasing = TRUE)][1:min(k, length(node_names))]
  }
}

# ── Convenience-based: nodes in logistically accessible regions ──────────────
make_strategy_convenience <- function(k, accessible_regions) {
  function(g, YL_state) {
    node_names <- V(g)$name
    eligible   <- node_names[V(g)$region %in% accessible_regions]
    if (length(eligible) > k) eligible <- sample(eligible, k)
    eligible
  }
}
