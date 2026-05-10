# =============================================================================
# 02_params.R
# Parameters for the seed degeneration network model
#
# Design principle (per user request):
#   ONLY pHSinit differs across seed quality categories.
#   All biological / management / environmental parameters are IDENTICAL
#   across CS, QDS, and IS. This isolates the effect of the seed itself.
#
# The shared parameters listed in `base_params` are applied to every node in
# every seed scenario. Seed quality enters the model only through pHSinit,
# either as the system baseline (Objective 2) or as the value delivered by
# an intervention (Objectives 3 and 4).
# =============================================================================

# ── Shared parameters for all seed types ──────────────────────────────────────
# These reflect the INFORMAL Andean potato system: minimal disease management.
# - axtnormm = 1.0 : no systematic roguing (informal farmers don't remove
#                    symptomatic plants reliably)
# - zxtnormm = 1.0 : random seed selection (no positive selection against
#                    diseased plants)
# - rx       = 0.05: low reversion (most seed produced by infected plants
#                    remains infected)
# - Ex       = 0.05: moderate external inoculum pressure (vector / volunteer
#                    plant pressure typical for the region)
# - betax    = 0.02: maximum seasonal transmission rate (Thomas-Sharma default
#                    for moderate-to-high disease pressure)
#
# The intervention itself enters ONLY through pHSinit (see seed_pHSinit
# below) so seed quality remains the only experimental variable.
base_params <- list(
  Kx        = 100,
  betax     = 0.02,
  wxtnormm  = 0.5,    wxtnormsd = 0.1,
  hx        = 1,
  mxtnormm  = 1.0,    mxtnormsd = 0.1,    # no vector management
  axtnormm  = 1.0,    axtnormsd = 0.1,    # no roguing
  rx        = 0.05,                       # low reversion
  zxtnormm  = 1.0,    zxtnormsd = 0.1,    # random selection
  gx        = 4,
  cx        = 0.9,
  phix      = 0,      # certified entry handled by network distribution
  maY       = 100,
  miY       = 0,
  thetax    = 0.2,
  Ex        = 0.05    # moderate external inoculum
)

# ── Seed-type-specific pHSinit (the ONLY parameter that varies) ──────────────
# Used when a node receives intervention seed under Objectives 3 and 4.
seed_pHSinit <- c(
  certified = 0.97,
  qds       = 0.80,
  informal  = 0.40
)

# ── Node-type-specific baseline pHSinit (Objective 2) ────────────────────────
# At t = 0 every node receives a baseline pHS reflecting institutional access
# to improved seed. Seed_specialists and Government nodes are best-connected
# to certified-seed channels; Other and Trader nodes are worst-off.
node_type_pHSinit <- c(
  Seed_specialist = 0.80,
  Government      = 0.75,
  NGO             = 0.70,
  Farm_assoc      = 0.60,
  Custodian       = 0.55,
  Farmer          = 0.40,
  Trader          = 0.35,
  Other           = 0.30
)

# ── Build a node-indexed pHSinit vector for a given graph ────────────────────
# Used to initialize the simulation. Any node type not in node_type_pHSinit
# falls back to the value provided in `default`.
init_pHS_by_node_type <- function(g, default = 0.40) {
  v <- node_type_pHSinit[V(g)$type]
  v[is.na(v)] <- default
  names(v)    <- V(g)$name
  v
}
