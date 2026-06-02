# =============================================================================
# 01_network.R  (pipeline v3)
# Build the TWO real networks (nonstress, stress) directly from the Peru Excel
# files and attach node-level metrics.
#
# Excel column conventions (from the original survey):
#   TieData  sheet: `Source (from)`, `Sink (to)`, `Transaction_vol (kg)`
#   NodeData sheet: `Node_ID`, `Node_type`, `Node_region`, `Node_location`
#
# Each network is a directed igraph with vertex attributes:
#   name, type, region, location, and computed metrics
#   (degree_total, in_strength, out_strength, betweenness).
# Edge attribute `weight` = summed transaction volume (kg) between a pair.
# =============================================================================

library(readxl)
library(dplyr)
library(igraph)

# Canonical node types (anything outside this list is folded into "Other").
KNOWN_TYPES <- c("Seed_specialist", "Farmer", "Custodian", "Other",
                 "Farm_assoc", "Government", "Trader", "NGO")

# ── Build ONE network from a given pair of sheets ────────────────────────────
build_network_from_excel <- function(ties_path, nodes_path,
                                      ties_sheet, nodes_sheet) {

  ties  <- readxl::read_excel(ties_path,  sheet = ties_sheet)
  nodes <- readxl::read_excel(nodes_path, sheet = nodes_sheet)

  edge_list <- ties |>
    dplyr::group_by(`Source (from)`, `Sink (to)`) |>
    dplyr::summarise(weight = sum(as.numeric(`Transaction_vol (kg)`), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rename(from = `Source (from)`, to = `Sink (to)`) |>
    dplyr::filter(weight > 0)

  all_ids <- unique(c(edge_list$from, edge_list$to))
  idx     <- match(all_ids, nodes$Node_ID)

  node_table <- data.frame(
    name     = all_ids,
    type     = nodes$Node_type[idx],
    region   = nodes$Node_region[idx],
    location = nodes$Node_location[idx],
    stringsAsFactors = FALSE
  )

  # --- data hygiene + warnings (do not silently mangle the data) -------------
  n_bad_type   <- sum(is.na(node_table$type))
  n_bad_region <- sum(is.na(node_table$region))
  if (n_bad_type > 0)
    warning(sprintf("[%s] %d node(s) had no Node_type match -> set to 'Other'.",
                    nodes_sheet, n_bad_type))
  if (n_bad_region > 0)
    warning(sprintf("[%s] %d node(s) had no Node_region match -> set to 'Unknown'.",
                    nodes_sheet, n_bad_region))

  node_table$type[is.na(node_table$type)]     <- "Other"
  node_table$region[is.na(node_table$region)] <- "Unknown"

  unexpected <- setdiff(unique(node_table$type), KNOWN_TYPES)
  if (length(unexpected) > 0) {
    warning(sprintf("[%s] unexpected node type(s) folded into 'Other': %s",
                    nodes_sheet, paste(unexpected, collapse = ", ")))
    node_table$type[node_table$type %in% unexpected] <- "Other"
  }

  g <- igraph::graph_from_data_frame(edge_list, vertices = node_table,
                                     directed = TRUE)
  add_node_metrics(g)
}

# ── Build BOTH networks at once ──────────────────────────────────────────────
# Returns a named list with elements "Nonstress" and "Stress" (order matters:
# figures rely on Nonstress first).
build_both_networks <- function(ties_path, nodes_path) {
  list(
    Nonstress = build_network_from_excel(ties_path, nodes_path,
                                         "nonstress_ties", "nonstress_nodes"),
    Stress    = build_network_from_excel(ties_path, nodes_path,
                                         "stress_ties",    "stress_nodes")
  )
}

# ── Compute and attach node-level metrics ────────────────────────────────────
add_node_metrics <- function(g) {
  igraph::V(g)$betweenness   <- igraph::betweenness(g, normalized = TRUE)
  igraph::V(g)$in_strength   <- igraph::strength(g, mode = "in",  weights = igraph::E(g)$weight)
  igraph::V(g)$out_strength  <- igraph::strength(g, mode = "out", weights = igraph::E(g)$weight)
  igraph::V(g)$degree_total  <- igraph::degree(g, mode = "all")
  igraph::V(g)$in_degree     <- igraph::degree(g, mode = "in")
  igraph::V(g)$out_degree    <- igraph::degree(g, mode = "out")
  g
}
