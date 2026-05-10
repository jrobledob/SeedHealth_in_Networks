# =============================================================================
# 01_network.R
# Network loading and characterization
#
# Two entry points:
#   - build_network_from_excel(): for the original Peru data (testing while
#     waiting for Objective 1 outputs)
#   - load_network_rds(): for the 6 networks produced by Objective 1
#
# Both return an igraph object with vertex attributes:
#   name, type, region, location (when available), and computed metrics
#   (betweenness, in_strength, out_strength).
# =============================================================================

library(readxl)
library(dplyr)
library(igraph)

# ── Load from Excel files (original Peru data format) ────────────────────────
build_network_from_excel <- function(ties_path, nodes_path,
                                     ties_sheet  = "nonstress_ties",
                                     nodes_sheet = "nonstress_nodes") {

  ties  <- read_excel(ties_path,  sheet = ties_sheet)
  nodes <- read_excel(nodes_path, sheet = nodes_sheet)

  edge_list <- ties |>
    group_by(`Source (from)`, `Sink (to)`) |>
    summarise(weight = sum(`Transaction_vol (kg)`), .groups = "drop") |>
    rename(from = `Source (from)`, to = `Sink (to)`)

  all_ids <- unique(c(edge_list$from, edge_list$to))
  idx     <- match(all_ids, nodes$Node_ID)

  node_table <- data.frame(
    name     = all_ids,
    type     = nodes$Node_type[idx],
    region   = nodes$Node_region[idx],
    location = nodes$Node_location[idx],
    stringsAsFactors = FALSE
  )

  g <- graph_from_data_frame(edge_list, vertices = node_table, directed = TRUE)
  add_node_metrics(g)
}

# ── Load Objective 1 output (igraph saved as .rds) ───────────────────────────
load_network_rds <- function(rds_path) {
  g <- readRDS(rds_path)
  if (!inherits(g, "igraph")) stop("RDS file does not contain an igraph object")

  required <- c("name", "type", "region")
  missing  <- setdiff(required, vertex_attr_names(g))
  if (length(missing) > 0)
    stop(sprintf("Network missing vertex attributes: %s",
                 paste(missing, collapse = ", ")))
  if (!"weight" %in% edge_attr_names(g))
    stop("Network missing edge attribute: weight")

  add_node_metrics(g)
}

# ── Compute and attach node-level metrics ────────────────────────────────────
add_node_metrics <- function(g) {
  V(g)$betweenness  <- betweenness(g, normalized = TRUE)
  V(g)$in_strength  <- strength(g, mode = "in",  weights = E(g)$weight)
  V(g)$out_strength <- strength(g, mode = "out", weights = E(g)$weight)
  V(g)$in_degree    <- degree(g, mode = "in")
  V(g)$out_degree   <- degree(g, mode = "out")
  g
}
