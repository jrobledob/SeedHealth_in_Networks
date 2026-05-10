# Seed Degeneration Network Epidemic — Pipeline v2

End-to-end pipeline for Objectives 2, 3, and 4 of the seed degeneration study.
Objective 1 (network generation from ERGM/TERGM models) is handled separately;
this pipeline consumes its outputs as `.rds` files.

---

## Directory Structure

```
seed_network_v2/
├── code/                       ← shared modules (do not edit during runs)
│   ├── 01_network.R            Network loading (Excel + rds)
│   ├── 02_params.R             Shared params + node-type pHS0 + seed-type pHS
│   ├── 03_strategies.R         4 distribution strategies + baseline
│   ├── 04_simulate.R           Core network epidemic simulation
│   ├── 05_obj2_baseline.R      Objective 2 driver
│   ├── 06_obj3_strategies.R    Objective 3 driver
│   ├── 07_obj4_robustness.R    Objective 4 driver
│   ├── 08_analysis.R           Heatmaps, trajectories, network plots
│   └── 09_toy_networks.R       12-node toy network generator
│
├── toy/
│   ├── tutorial_toy.R          10-step interactive tutorial
│   └── tutorial_outputs/       PDFs produced by tutorial steps
│
└── real/
    ├── pipeline_real.R         Full pipeline on real data
    ├── Data/                   Excel files (TieData, NodeData)
    ├── networks/               Objective 1 .rds files (when available)
    └── real_outputs/           Results and figures
```

---

## What changed from v1

1. **Only `pHSinit` varies between seed types.** All other parameters (β, m, α, z, r, E, …) are now identical across CS / QDS / informal. The seed itself is the only variable.

2. **Per-node-type initial pHS₀.** At t=0 each node receives a baseline pHS based on its institutional type, ranging from Seed_specialist (0.80) down to Other (0.30). See `02_params.R::node_type_pHSinit`.

3. **Six networks instead of one.** Objective 2 runs across all 6 networks from Objective 1 (real / scale-free / small-world × stress / nonstress).

4. **Realistic-scenario selection.** After Objective 2, the 3 worst-case (informal) scenarios are selected automatically and feed into Objective 3.

5. **Best-strategy selection per realistic scenario.** Objective 3 picks the best (network × seed_type) combination for each of the 4 strategies, giving 4 winning configurations for Objective 4.

6. **Risk modifiers as percentiles.** Availability and acceptance now take values {0, 0.25, 0.50, 0.75, 1.00} per Objective 4 specification.

---

## How to run

### Option A — Toy tutorial (recommended first)

Quickly validate everything on a 12-node toy network:

```bash
cd seed_network_v2/toy
Rscript tutorial_toy.R
```

The tutorial prints intermediate objects at every step (mixing matrix, node metrics, per-season YL, …) and writes PDFs to `tutorial_outputs/`. Read the console output alongside the PDFs to confirm the model behaves as expected before committing real-data runs.

### Option B — Real data on laptop

```bash
cd seed_network_v2/real
Rscript pipeline_real.R
```

This loads either the Objective 1 `.rds` networks (when available) or the Peru Excel data as a placeholder. Toggle `USE_OBJECTIVE1_NETWORKS` at the top of the script.

---

## Required inputs

### From Objective 1 (when available)

Six igraph objects saved as `.rds` files in `real/networks/`:

```
real_nonstress.rds
real_stress.rds
scale_free_nonstress.rds
scale_free_stress.rds
small_world_nonstress.rds
small_world_stress.rds
```

Each `.rds` must contain an `igraph` object with vertex attributes `name`, `type`, `region` (and optionally `location`) and edge attribute `weight`.

### From the original Peru survey (current state)

Two Excel files in `real/Data/`:

```
TieData_social_network.xlsx   (sheet: nonstress_ties [, stress_ties])
NodeData_social_network.xlsx  (sheet: nonstress_nodes [, stress_nodes])
```

---

## Configuration to set before a real run

Edit the top of `pipeline_real.R`:

| Variable | Meaning | Default |
|---|---|---|
| `NSIM` | Stochastic replicates per scenario | 100 |
| `NSEASONS` | Seasons to simulate | 10 |
| `K_NODES_FRACTION` | Proportion of nodes targeted per season | 0.10 |
| `RING_SIZE` | Number of epicenters for close-ring strategy | 5 |
| `ACCESSIBLE_REGIONS` | Regions reachable by convenience strategy | `c("Junin", "Lima")` |
| `USE_OBJECTIVE1_NETWORKS` | Toggle for input source | `FALSE` |

---

## Expected outputs (real run)

```
real_outputs/
├── obj2_results.rds            18-scenario simulation data
├── obj3_results.rds            24-scenario simulation data
├── obj4_results.rds            100-scenario simulation data
├── realistic_scenarios.csv     The 3 worst-case scenarios
├── best_strategies.rds         The 4 winning (network × seed × strategy) tuples
├── fig_obj2_heatmap.pdf        Baseline yield loss across 18 scenarios
├── fig_obj3_heatmap.pdf        Strategy comparison across 24 scenarios
└── fig_obj4_heatmap.pdf        Risk-factor sensitivity across 100 scenarios
```

---

## After approval — HiPerGator deployment

Once you confirm the output of `pipeline_real.R` looks right, the same modules can be wrapped in SLURM array scripts for HiPerGator (one array task per scenario, with `mclapply` parallelism within each task). I'll generate those scripts as a separate step.
