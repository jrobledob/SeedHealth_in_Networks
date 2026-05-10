# =============================================================================
# 09_toy_networks.R
# Generate small toy networks (12 nodes) that mirror the structure of the
# real Peru seed system. Used for testing and the step-by-step tutorial.
#
# Each toy network has:
#   - 12 nodes spanning all 8 Node_type categories
#   - 3 regions
#   - ~20 directed weighted edges
#
# Three topologies returned, mimicking what Objective 1 will deliver:
#   real_like, scale_free, small_world
# Each generated under two environmental conditions (stress, non_stress).
# =============================================================================

library(igraph)

# ── Helper: assign attributes, ensure connectivity, return igraph object ─────
finalize_toy <- function(adj_matrix, edge_weights = NULL) {

  node_types <- c("Seed_specialist", "Government", "NGO", "Farm_assoc",
                  "Custodian", "Farmer", "Farmer", "Farmer",
                  "Farmer", "Trader", "Trader", "Other")
  regions    <- c("Junin", "Pasco", "Junin", "Huancavelica",
                  "Junin", "Pasco", "Pasco", "Huancavelica",
                  "Huancavelica", "Junin", "Lima", "Pasco")
  locations  <- paste0("Loc_", LETTERS[seq_along(node_types)])

  g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed",
                                   weighted = TRUE)
  V(g)$name     <- paste0("T", sprintf("%02d", seq_along(node_types)))
  V(g)$type     <- node_types
  V(g)$region   <- regions
  V(g)$location <- locations

  if (!is.null(edge_weights) && length(edge_weights) == ecount(g))
    E(g)$weight <- edge_weights
  else if (is.null(E(g)$weight))
    E(g)$weight <- 1

  add_node_metrics(g)
}

# ── Real-like topology: random directed graph with realistic density ─────────
make_toy_real <- function(seed, density_mult = 1) {
  set.seed(seed)
  n  <- 12
  m  <- round(20 * density_mult)
  adj <- matrix(0, n, n)
  edges_added <- 0
  while (edges_added < m) {
    i <- sample(n, 1); j <- sample(n, 1)
    if (i != j && adj[i, j] == 0) { adj[i, j] <- 1; edges_added <- edges_added + 1 }
  }
  weights <- sample(5:50, m, replace = TRUE)
  finalize_toy(adj, weights)
}

# ── Scale-free topology: preferential attachment ─────────────────────────────
make_toy_scale_free <- function(seed, density_mult = 1) {
  set.seed(seed)
  g0 <- sample_pa(n = 12, m = round(2 * density_mult),
                  directed = TRUE, power = 1.2)
  adj <- as.matrix(as_adjacency_matrix(g0))
  weights <- sample(5:50, sum(adj), replace = TRUE)
  finalize_toy(adj, weights)
}

# ── Small-world topology: Watts-Strogatz ─────────────────────────────────────
make_toy_small_world <- function(seed, density_mult = 1) {
  set.seed(seed)
  g0 <- sample_smallworld(dim = 1, size = 12,
                          nei = max(1, round(2 * density_mult)),
                          p = 0.15)
  # convert to directed
  el  <- as_edgelist(g0)
  adj <- matrix(0, 12, 12)
  for (i in seq_len(nrow(el))) {
    a <- as.integer(el[i, 1]); b <- as.integer(el[i, 2])
    if (runif(1) < 0.5) adj[a, b] <- 1 else adj[b, a] <- 1
  }
  weights <- sample(5:50, sum(adj), replace = TRUE)
  finalize_toy(adj, weights)
}

# ── Generate all 6 toy networks for testing ──────────────────────────────────
# Stress condition: lower edge density (less seed exchange under stress)
generate_toy_networks <- function() {
  list(
    real_nonstress       = make_toy_real(seed = 101, density_mult = 1.0),
    real_stress          = make_toy_real(seed = 102, density_mult = 0.7),
    scale_free_nonstress = make_toy_scale_free(seed = 201, density_mult = 1.0),
    scale_free_stress    = make_toy_scale_free(seed = 202, density_mult = 0.7),
    small_world_nonstress = make_toy_small_world(seed = 301, density_mult = 1.0),
    small_world_stress    = make_toy_small_world(seed = 302, density_mult = 0.7)
  )
}
