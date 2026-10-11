# Adaptation Registry and Desalination Analysis

This module forms part of the Barcelona School of Economics Master Project
*Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation*.

It constructs a mine-level desalination/adaptation registry and examines whether
commissioning desalinated-water infrastructure moderates the relationship between
climate stress and copper production.

## Research question

The module asks whether mines become less sensitive to climate stress after
desalination or seawater infrastructure becomes operational.

The main empirical challenge is that desalination is not randomly assigned:
adapted mines tend to be large, coastal, and located in especially water-stressed
regions, while several commissioning events coincide with broader mine expansions.
The analysis therefore treats the panel interactions as diagnostic evidence and
places greater weight on within-mine timing and robustness checks.

## Adaptation measure

The module constructs a mine-month adaptation indicator from a central desalination
registry and public information on commissioning dates.

Three measures are used:

- `adapt_main`: headline post-commissioning indicator;
- `adapt_strict`: switcher-focused coding for cleaner within-sample variation; and
- `adapt_share`: a continuous robustness measure for partial adaptation cases.

The underlying coding summary is available in `data/panels/50__coding_summary.csv`,
and the registry is retained in `data/registry/`.

## Empirical design

The analysis combines the adaptation indicators with the climate-stress panels from
the ENSO climate-exposure module.

The main exercises include:

1. mine-month interaction regressions for acute flood and heat stress;
2. mine-year interaction regressions for persistent drought exposure;
3. commissioning-event comparisons and event-study diagnostics;
4. treatment-cell and leave-one-out diagnostics;
5. channel-stratified placebo and mechanism checks; and
6. wild-cluster-bootstrap inference for the small number of regional clusters.

The headline interaction parameter measures whether the estimated climate-stress
effect differs after adaptation.

## Main findings

The full-panel interaction results do **not** provide a robust causal estimate of
adaptation.

Treatment cells are small, estimates are sensitive to the composition of adapted
mines, and desalination commissioning is correlated with mine type and investment
cycles. Once the drought specification is aligned with the cumulative exposure
measure used in the main climate analysis, the drought interaction is close to zero.

Wild-cluster-bootstrap inference further weakens the apparent flood interaction:
the small-cluster-adjusted results do not provide robust evidence of moderation.

The within-mine event evidence is more informative but remains preliminary. Spence,
the switcher with the longest usable post-commissioning window, shows a suggestive
reduction in heat sensitivity after commissioning. Given the limited number of
post-treatment stress events, this is treated as suggestive mechanism evidence rather
than a definitive causal effect.

## Repository structure

```text
.
├── README.md
├── code/
├── data/
│   ├── panels/
│   ├── registry/
│   └── inputs/
├── results/
│   ├── tables/
│   └── figures/
└── notes/
```

## Code

The `code/` folder contains the Python scripts used to construct the adaptation panel
and run the empirical analysis:

- `50_build_adapt_panel.py` — constructs mine-month adaptation measures;
- `51_acute_regressions.py` — acute climate-stress interactions;
- `52_drought_regressions.py` — annual drought interactions;
- `52b_drought_cum3_aligned.py` — cumulative three-year drought specification;
- `53_event_study.py` — commissioning-event analyses;
- `54_diagnostics.py` — treatment-cell and leave-one-out diagnostics;
- `55_channel_stratified.py` — mechanism/placebo comparisons;
- `56_wcb_bootstrap.py` — wild-cluster-bootstrap inference; and
- `99_run_all.py` — workflow driver.

For public organisation, only folder/path references have been adapted to the
repository structure. The substantive estimation logic has not been rewritten.

## Data

The repository contains the compact mine-month and mine-year analysis panels required
for the module, the desalination registry, and the monthly copper-production input used
to construct the adaptation panel.

The climate variables originate from the separate ENSO climate-exposure module and are
included here only in the analysis panels needed for this module.

No additional large raw climate datasets are redistributed.

## Running the code

From the module root, install the principal dependencies if needed:

```bash
pip install pandas pyarrow openpyxl pyfixest
```

Then run the full workflow:

```bash
python code/99_run_all.py
```

Individual scripts can also be run separately.

## Reproducibility

This repository is a cleaned research archive of the adaptation analysis rather than a
newly rebuilt replication package.

The analysis panels, registry, scripts, and final outputs used for the project are
included. The reorganised repository has not been rerun end-to-end after the
public-facing folder cleanup.

## Results

Final regression and diagnostic outputs are available in `results/tables/`.

The figure in `results/figures/` provides the public-facing visual summary of the
adaptation analysis.

## Software

The module was implemented in Python. The principal analytical packages are `pandas`,
`pyarrow`, `openpyxl`, and `pyfixest`.

## Authorship

This module is part of the group Master Project by Gleb Enson, Rohan Kale, Manuel Seick,
Julia Suárez, and Fatimah Yasin at the Barcelona School of Economics, 2026.
