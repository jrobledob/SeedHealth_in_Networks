# =============================================================================
# 05_run_all.R  (pipeline v3)
# End-to-end driver: build the two networks, run the seed-health transmission
# model, and produce Figures 1-3 as PDF + an editable PowerPoint.
#
# Run from the project root:
#     Rscript code/05_run_all.R
# or source it interactively in RStudio.
# =============================================================================

# ── CONFIG ────────────────────────────────────────────────────────────────────
DATA_DIR    <- "Data"
TIES_FILE   <- file.path(DATA_DIR, "TieData_social_network.xlsx")
NODES_FILE  <- file.path(DATA_DIR, "NodeData_social_network.xlsx")
OUTPUT_DIR  <- "outputs"

NSIM        <- 100              # stochastic replicates per scenario
NSEASONS    <- 10               # cycles
CYCLES      <- c(1, 4, 7, 10)   # cycles shown in Figure 2
LAYOUT_ALGO <- "graphopt"       # "graphopt" | "fr" | "kk" | "drl" | "nicely"
LAYOUT_SEED <- 42               # reproducible network layout
RNG_SEED    <- 2024             # reproducible simulation

SAVE_SIMS   <- TRUE             # cache raw replicates to outputs/sims_store.rds

# ── Source modules ────────────────────────────────────────────────────────────
code_dir <- "code"
for (f in c("01_network.R", "02_params.R", "03_simulate.R", "04_figures.R"))
  source(file.path(code_dir, f))

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(RNG_SEED)

# ── 1. Build networks ─────────────────────────────────────────────────────────
cat("\n== Building networks ==\n")
networks <- build_both_networks(TIES_FILE, NODES_FILE)
for (nm in names(networks)) {
  g <- networks[[nm]]
  cat(sprintf("  %-10s nodes=%d edges=%d components=%d\n",
              nm, vcount(g), ecount(g), components(g, mode = "weak")$no))
}

# Fixed layouts + edge frames (reused by Figures 1 and 2)
layouts     <- lapply(networks, make_layout, algo = LAYOUT_ALGO, seed = LAYOUT_SEED)
edges_store <- Map(make_edges, networks, layouts)
names(layouts) <- names(edges_store) <- names(networks)

# ── 2. Run the transmission model ─────────────────────────────────────────────
cat("\n== Running simulations ==\n")
sims_store <- run_all_scenarios(networks, seed_scenarios, NSEASONS, NSIM)
if (SAVE_SIMS) saveRDS(sims_store, file.path(OUTPUT_DIR, "sims_store.rds"))

# ── 3. Build figures ──────────────────────────────────────────────────────────
cat("\n== Building figures ==\n")
fig1 <- build_fig1(networks, layouts, edges_store)
fig2 <- build_fig2(sims_store, layouts, edges_store, seed_scenarios, CYCLES)
fig3 <- build_fig3(sims_store, seed_scenarios, NSEASONS)
fig4 <- build_fig4(sims_store, networks, seed_scenarios, NSEASONS)
fig5 <- build_fig5(sims_store, networks, seed_scenarios, NSEASONS)
fig6 <- build_fig6(sims_store, networks, seed_scenarios, NSEASONS)

# ── 4. Save PDF ───────────────────────────────────────────────────────────────
save_pdf(fig1, file.path(OUTPUT_DIR, "fig1_networks.pdf"),       width = 15, height = 9.5)
save_pdf(fig2, file.path(OUTPUT_DIR, "fig2_transmission.pdf"),   width = 14, height = 18)
save_pdf(fig3, file.path(OUTPUT_DIR, "fig3_trajectories.pdf"),   width = 11, height = 5)

d4 <- dims_of(fig4); save_pdf(fig4, file.path(OUTPUT_DIR, "fig4_audpc_by_type.pdf"),        d4["width"], d4["height"])
d5 <- dims_of(fig5); save_pdf(fig5, file.path(OUTPUT_DIR, "fig5_audpc_by_region.pdf"),      d5["width"], d5["height"])
d6 <- dims_of(fig6); save_pdf(fig6, file.path(OUTPUT_DIR, "fig6_audpc_by_region_type.pdf"), d6["width"], d6["height"])

# ── 5. Save editable PowerPoint (6 slides) ───────────────────────────────────
save_pptx(
  items = list(
    list(plot = fig1, width = 14.0, height = 9.0),
    list(plot = fig2, width = 11.0, height = 14.0),
    list(plot = fig3, width = 11.0, height = 5.0),
    list(plot = fig4, width = d4["width"], height = d4["height"]),
    list(plot = fig5, width = d5["width"], height = d5["height"]),
    list(plot = fig6, width = d6["width"], height = d6["height"])
  ),
  path = file.path(OUTPUT_DIR, "figures_editable.pptx")
)

cat("\n== Done. Outputs in:", normalizePath(OUTPUT_DIR), "==\n")
