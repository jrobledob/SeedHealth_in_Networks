# =============================================================================
# 03_simulate.R  (pipeline v3)
# Core network epidemic simulation.
#
# The transmission engine (compute_mix_matrix / run_node / simulate_network)
# is carried over UNCHANGED from the validated v2 pipeline. Indexing is
# position-based, aligned to V(g)$name order. The only thing v3 does
# differently is: no distribution strategy and no intervention are applied
# (these are pure baseline degeneration runs), and per-node pHS_0 / W / Z come
# from the region x node-type tables in 02_params.R.
# =============================================================================

library(seedHealth)
library(igraph)
library(dplyr)

# ── Mixing matrix: column-normalized weighted adjacency ──────────────────────
compute_mix_matrix <- function(g) {
  nm  <- igraph::V(g)$name
  adj <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  dimnames(adj) <- list(nm, nm)

  col_sums <- colSums(adj)
  no_in    <- which(col_sums == 0)
  col_sums[no_in] <- 1
  mix <- sweep(adj, 2, col_sums, "/")
  diag(mix)[no_in] <- 1

  dimnames(mix) <- list(nm, nm)
  mix
}

# ── Single-node, single-season onesim wrapper ────────────────────────────────
run_node <- function(pHS, params) {
  pHS <- as.numeric(pHS)[1]
  if (is.na(pHS)) stop("run_node received NA pHS")
  pHS <- min(max(pHS, 0), 1)

  res <- onesim(
    pHSinit   = pHS,
    Kx        = params$Kx,
    betax     = params$betax,
    wxtnormm  = params$wxtnormm,  wxtnormsd = params$wxtnormsd,
    hx        = params$hx,
    mxtnormm  = params$mxtnormm,  mxtnormsd = params$mxtnormsd,
    axtnormm  = params$axtnormm,  axtnormsd = params$axtnormsd,
    rx        = params$rx,
    zxtnormm  = params$zxtnormm,  zxtnormsd = params$zxtnormsd,
    gx        = params$gx,        cx        = params$cx,
    phix      = 0,
    nseasons  = 1,
    HPcut     = 0.5,              pHScut    = 0.5,
    maY       = params$maY,       miY       = params$miY,
    thetax    = params$thetax,    Ex        = params$Ex
  )
  list(pHS_new = as.numeric(res$outfin$pHS),
       YL      = as.numeric(res$outfin$YL))
}

# ── Main multi-season simulation over the network ────────────────────────────
simulate_network <- function(g, mix_matrix, node_params,
                             strategy_fn, intervention_pHS,
                             nseasons, availability, acceptance,
                             pHSinit_vec) {

  node_names <- igraph::V(g)$name
  n          <- length(node_names)

  if (length(node_params) != n)
    stop(sprintf("node_params length (%d) != number of nodes (%d).",
                 length(node_params), n))

  pHS_state <- as.numeric(pHSinit_vec[node_names]); names(pHS_state) <- node_names
  YL_state  <- rep(0, n);                           names(YL_state)  <- node_names

  if (any(is.na(pHS_state)))
    stop("pHSinit_vec has NA for some nodes after matching to V(g)$name")

  M_T <- t(mix_matrix); dimnames(M_T) <- NULL

  out <- vector("list", nseasons)

  for (t in seq_len(nseasons)) {

    targeted <- strategy_fn(g, YL_state)
    if (length(targeted) > 0 && !is.na(intervention_pHS)) {
      succeeded <- targeted[runif(length(targeted)) < availability * acceptance]
    } else {
      succeeded <- character(0)
    }

    pHS_mixed <- as.numeric(M_T %*% pHS_state); names(pHS_mixed) <- node_names
    if (length(succeeded) > 0) pHS_mixed[succeeded] <- intervention_pHS

    pHS_mixed[pHS_mixed < 0] <- 0
    pHS_mixed[pHS_mixed > 1] <- 1

    pHS_new <- numeric(n); names(pHS_new) <- node_names
    YL_new  <- numeric(n); names(YL_new)  <- node_names

    for (i in seq_len(n)) {
      r <- run_node(pHS_mixed[i], node_params[[i]])
      pHS_new[i] <- r$pHS_new
      YL_new[i]  <- r$YL
    }

    pHS_state <- pHS_new
    YL_state  <- YL_new

    out[[t]] <- data.frame(
      season = t, node = node_names,
      pHS = as.numeric(pHS_new), YL = as.numeric(YL_new),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(out)
}

# ── Baseline scenario runner (no intervention), nsim replicates ──────────────
# Returns a list of nsim data frames (season, node, pHS, YL).
run_scenario_sims <- function(g, scenario_pHS0, nseasons, nsim) {
  mix_matrix  <- compute_mix_matrix(g)
  node_params <- build_node_params(g, base_params)
  pHSinit_vec <- build_pHSinit(g, scenario_pHS0)

  replicate(nsim,
            simulate_network(
              g                = g,
              mix_matrix       = mix_matrix,
              node_params      = node_params,
              strategy_fn      = function(g, YL_state) character(0),
              intervention_pHS = NA,
              nseasons         = nseasons,
              availability     = 0, acceptance = 0,
              pHSinit_vec      = pHSinit_vec
            ),
            simplify = FALSE)
}

# ── Run every (network x scenario) and store the raw replicates ──────────────
# Returns sims_store[[network_name]][[scenario_label]] = list of nsim data frames.
run_all_scenarios <- function(networks, scen_df, nseasons, nsim) {
  sims_store <- list()
  for (net in names(networks)) {
    sims_store[[net]] <- list()
    for (i in seq_len(nrow(scen_df))) {
      scen <- scen_df$scenario[i]
      cat(sprintf("[sim] %-10s | %-22s | nsim=%d nseasons=%d\n",
                  net, scen, nsim, nseasons))
      sims_store[[net]][[scen]] <-
        run_scenario_sims(networks[[net]], scen_df$pHS0[i], nseasons, nsim)
    }
  }
  sims_store
}
