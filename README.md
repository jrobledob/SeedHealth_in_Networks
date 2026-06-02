# Seed Degeneration Network — Pipeline v3 (two networks, three figures)

A focused pipeline that builds the **two real networks** (nonstress, stress)
from the Peru Excel survey, runs the seed-health transmission model, and
produces **three figures** as **PDF** and as an **editable PowerPoint**.

This replaces the six-network / four-objective v2 workflow. The validated
transmission engine (`compute_mix_matrix` / `run_node` / `simulate_network`)
is carried over unchanged; everything else is new.

---

## Layout

```
seed_network_v3/
├── code/
│   ├── 01_network.R     Build Nonstress + Stress networks from Excel
│   ├── 02_params.R      Region × node-type tables (Wa, Z, pHS0) + scenarios
│   ├── 03_simulate.R    Transmission engine (v2, unchanged) + scenario runner
│   ├── 04_figures.R     Figures 1–3 + PDF/PPTX savers
│   └── 05_run_all.R     Driver (edit the CONFIG block at the top)
├── Data/                Put the two Excel files here
└── outputs/             PDFs, the .pptx, and sims_store.rds land here
```

## Inputs (place in `Data/`)

```
TieData_social_network.xlsx    sheets: nonstress_ties, stress_ties
NodeData_social_network.xlsx   sheets: nonstress_nodes, stress_nodes
```

Expected columns — ties: `Source (from)`, `Sink (to)`, `Transaction_vol (kg)`;
nodes: `Node_ID`, `Node_type`, `Node_region`, `Node_location`.

## Run

```bash
cd seed_network_v3
Rscript code/05_run_all.R
```

Outputs: `fig1_networks.pdf`, `fig2_transmission.pdf`,
`fig3_trajectories.pdf`, `fig4_audpc_by_type.pdf`,
`fig5_audpc_by_region.pdf`, `fig6_audpc_by_region_type.pdf`, and
`figures_editable.pptx` (6 slides).

## Packages

`igraph`, `ggplot2` (**≥ 3.4** — the figures use the `linewidth` aesthetic),
`dplyr`, `readxl`, `patchwork`, `officer`, `rvg`, and **`seedHealth`**.

```r
install.packages(c("igraph","ggplot2","dplyr","readxl","patchwork","officer","rvg"))
# seedHealth: install from your existing source as in v2
```

---

## What the figures show

**Figure 1** — 2×2. Left column: the two networks (row 1 nonstress, row 2
stress). Right column: a summary-metrics table for each. Encoding: shape = node
type, color = region, size = total degree, edge width = transaction volume.

**Figure 2** — 6×4 composite. Columns = cycles 1, 4, 7, 10. Rows = the three
seed-quality scenarios × {nonstress, stress} (nonstress on top of each pair).
Same encoding as Figure 1, plus **node opacity = mean yield loss**, on a single
shared scale across all 24 panels. Layout is fixed per network so nodes stay in
place across cycles.

**Figure 3** — two panels (stress left, nonstress right). Mean network yield
loss per cycle for each seed scenario, with a 5–95% band, and **mean AUDPC**
printed per scenario.

**Figure 4** — AUDPC trajectories by **node type**. Same form as Figure 3
(columns = the two networks, color = seed scenario, 5–95% band, per-panel
AUDPC), with one row per node type.

**Figure 5** — AUDPC trajectories by **region**. One row per region.

**Figure 6** — AUDPC trajectories by **region × node type**. One row per
region/type combination that actually occurs in the data (empty combinations
are dropped). This figure is tall; its height scales with the number of
combinations, and the PowerPoint slide for it is auto-fit (resize as needed).

### Node legibility

Networks are laid out with a repulsion-based algorithm (`graphopt`) and the
coordinates are normalized to a fixed square, with capped node sizes, so the
structure is readable without overlap. Change the algorithm via `LAYOUT_ALGO`
in `05_run_all.R` (`"graphopt"`, `"fr"`, `"kk"`, `"drl"`, `"nicely"`) and the
seed via `LAYOUT_SEED`.

## Modeling choices (as agreed)

- Three scenarios set the **base** `pHS_0`: Informal 0.70, QDS-moderate 0.80,
  QDS-optimistic 0.90.
- Each node's start = base **± a region × node-type nudge**: Low −0.05,
  Medium 0, High +0.05 (clamped to [0,1]). The size of the nudge is the single
  constant `PHS_REGION_ADJ` in `02_params.R` — change it there if "5 units"
  meant something other than 0.05.
- **Wa → `wxtnormm`**, **Z → `zxtnormm`**, both Low/Med/High → 0.2/0.5/0.8,
  SD = 0.10. The same tables apply to both networks.
- No distribution strategy / intervention — these are pure baseline
  degeneration runs.
- All other `onesim` parameters are the v2 `base_params` values.

## Knobs (top of `05_run_all.R`)

| Variable | Meaning | Default |
|---|---|---|
| `NSIM` | replicates per scenario | 100 |
| `NSEASONS` | cycles | 10 |
| `CYCLES` | cycles drawn in Fig 2 | 1, 4, 7, 10 |
| `LAYOUT_ALGO` | network layout algorithm | "graphopt" |
| `LAYOUT_SEED` | network layout reproducibility | 42 |
| `RNG_SEED` | simulation reproducibility | 2024 |

Palettes (`REGION_COLORS`, `TYPE_SHAPES`, `SCEN_COLORS`) and `DRAW_ARROWS` are
near the top of `04_figures.R`. Everything is also editable in the `.pptx`.

---

## A few notes (this was not run before delivery)

I couldn't execute R in the environment where this was written, so treat the
first local run as the real test. Things most worth a glance:

1. **`seedHealth::onesim` output fields.** `run_node` reads `res$outfin$pHS`
   and `res$outfin$YL` exactly as v2 did — confirm those names still hold.
2. **PowerPoint template names.** `save_pptx` uses `layout = "Blank"`,
   `master = "Office Theme"` (officer's default template). If your default
   template differs, run `officer::layout_summary(officer::read_pptx())` and
   adjust.
3. **`cairo_pdf`.** If your R build lacks cairo, switch the `device` in
   `save_pdf` to `"pdf"`.
4. **Region/type spelling.** Regions must match the seven in
   `CATEGORY_MATRIX` (Huancavelica, Pasco, Junin, Huanuco, Apurimac, Ayacucho,
   Lima) and types the eight in `TYPES_ORDER`; anything else defaults to
   "Medium" / "Other" with a warning. Watch the console on the first run.
5. **Figure 2 size.** It's intentionally large (14×18 in). The PPTX slide for
   it may need resizing once opened — the content is fully editable vector.
