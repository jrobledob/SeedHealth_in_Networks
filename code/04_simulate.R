# =============================================================================
# 04_simulate.R
# Network epidemic simulation
#
# Implementation note:
# All internal indexing is POSITION-BASED, aligned to V(g)$name order.
# This avoids fragility when dimnames are dropped through matrix operations
# in some R versions.
# =============================================================================

library(seedHealth)
library(igraph)

# ── Mixing matrix: column-normalized adjacency ───────────────────────────────
# Dimnames are explicitly set so downstream code can rely on them.
compute_mix_matrix <- function(g) {
  nm  <- V(g)$name
  adj <- as.matrix(as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  dimnames(adj) <- list(nm, nm)              # enforce dimnames

  col_sums <- colSums(adj)
  no_in    <- which(col_sums == 0)
  col_sums[no_in] <- 1
  mix <- sweep(adj, 2, col_sums, "/")
  diag(mix)[no_in] <- 1

  dimnames(mix) <- list(nm, nm)              # re-enforce after sweep
  mix
}

# ── Single-node, single-season onesim wrapper ────────────────────────────────
# pHS is forced to an unnamed scalar to avoid named-vector confusion in onesim.
run_node <- function(pHS, params) {
  pHS <- as.numeric(pHS)[1]
  if (is.na(pHS)) stop("run_node received NA pHS")
  pHS <- min(max(pHS, 0), 1)                 # clamp to [0,1]

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

# ── Main simulation ──────────────────────────────────────────────────────────
simulate_network <- function(g, mix_matrix, base_params,
                             strategy_fn, intervention_pHS,
                             nseasons, availability, acceptance,
                             pHSinit_vec) {

  node_names <- V(g)$name
  n          <- length(node_names)

  # State vectors aligned to V(g)$name order (position-based throughout)
  pHS_state <- as.numeric(pHSinit_vec[node_names])
  YL_state  <- rep(0, n)
  names(pHS_state) <- node_names
  names(YL_state)  <- node_names

  if (any(is.na(pHS_state)))
    stop("Initial pHSinit_vec has NA for some nodes after matching to V(g)$name")

  # Strip mix_matrix of any dimnames for the multiplication step itself —
  # we rely solely on position correspondence with V(g)$name.
  M_T <- t(mix_matrix)
  dimnames(M_T) <- NULL

  out <- vector("list", nseasons)

  for (t in seq_len(nseasons)) {

    # Strategy selects targeted nodes; risk modifiers gate adoption
    targeted <- strategy_fn(g, YL_state)
    if (length(targeted) > 0 && !is.na(intervention_pHS)) {
      succeeded <- targeted[runif(length(targeted)) < availability * acceptance]
    } else {
      succeeded <- character(0)
    }

    # Volume-weighted seed mixing (position-based)
    pHS_mixed <- as.numeric(M_T %*% pHS_state)
    names(pHS_mixed) <- node_names

    # Intervention override
    if (length(succeeded) > 0)
      pHS_mixed[succeeded] <- intervention_pHS

    # Defensive clamp
    pHS_mixed[pHS_mixed < 0] <- 0
    pHS_mixed[pHS_mixed > 1] <- 1

    # One season of degeneration per node
    pHS_new <- numeric(n); names(pHS_new) <- node_names
    YL_new  <- numeric(n); names(YL_new)  <- node_names

    for (i in seq_len(n)) {
      r <- run_node(pHS_mixed[i], base_params)
      pHS_new[i] <- r$pHS_new
      YL_new[i]  <- r$YL
    }

    pHS_state <- pHS_new
    YL_state  <- YL_new

    out[[t]] <- data.frame(
      season = t, node = node_names,
      pHS = as.numeric(pHS_new), YL = as.numeric(YL_new),
      intervened = node_names %in% succeeded,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(out)
}
