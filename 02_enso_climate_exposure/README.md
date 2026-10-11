# ENSO-Related Climate Exposure in Chilean Copper Mining

This module forms part of the Barcelona School of Economics Master Project
*Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation*.

It studies when climate anomalies become operationally binding for Chilean copper mines,
with particular attention to flood, heat, and persistent drought channels and to the
subset of shocks that are consistent with ENSO-related teleconnections.

## Research question

The module asks whether acute and persistent climate stress is associated with changes in
mine-level copper production, and whether the effects differ between general climate
stress and stress episodes classified as ENSO-aligned.

The empirical analysis distinguishes:

- acute flood exposure;
- acute heat exposure; and
- persistent drought exposure.

The ENSO classification combines an active ONI phase, the expected regional
teleconnection direction, a realised local anomaly, and an operational threshold crossing.

## Main findings

The headline flood specification indicates that unusually wet regional months are
associated with approximately 4.7% lower monthly mine output. The ENSO-aligned subset
has the same sign but is estimated less precisely.

The heat specifications do not show a robust production response.

For persistent drought, the analysis finds a negative association between greater
three-year ENSO-aligned drought exposure and annual output. The magnitude is economically
meaningful, but the evidence is concentrated in the recent megadrought and small-sample
inference is correspondingly cautious. The drought estimates should therefore be read as
evidence within the observed episode rather than as a general causal law.

## Repository structure

```text
.
├── README.md
├── code/
├── data/
│   ├── registry/
│   ├── panels/
│   ├── ledger/
│   └── inputs/
├── results/
│   ├── figures/
│   ├── tables/
│   └── report/
└── notes/
```

## Code

The `code/` folder contains the R scripts used in the module.

The scripts cover climate-data preparation, construction of the stress ledger and
analysis panels, fixed-effects regressions, small-cluster inference, robustness checks,
validation exercises, and figure/table generation.

For public documentation, only file-path handling has been adapted to the repository
structure. The substantive empirical specifications and estimation logic have not been
rewritten.

The downstream analysis can be inspected from the bundled panels and ledger. Earlier
climate-ingest stages require large external climate files that are not redistributed
here.

## Data

The repository contains compact analysis inputs needed to document the downstream
workflow, including:

- mine and threshold registries;
- analysis panels used in the main regressions;
- the master stress ledger in Parquet format;
- monthly copper-production inputs;
- ONI and regional climate inputs; and
- selected market-price inputs.

Large raw climate files used in the earliest construction stages are not redistributed.

See `data/panels/README_PANELS.md` and `data/registry/channel_dictionary.csv` for the
analysis-panel and stress-channel definitions.

## Methods

The main mine-month and mine-year specifications use fixed effects to relate production
to climate-stress exposure.

Because the analysis has only seven regional clusters, the inference strategy goes
beyond conventional clustered standard errors and includes small-sample corrections,
wild-cluster bootstrap procedures, and leave-one-region / leave-one-event robustness
checks where appropriate.

The drought analysis uses a multi-year exposure measure rather than interpreting a
single dry month as the treatment.

## Results

Selected final figures are available in `results/figures/`, and regression/robustness
tables are available in `results/tables/`.

A standalone module report is available at:

`results/report/ENSO_Climate_Exposure_Report.pdf`

## Reproducibility

The repository is a cleaned research archive rather than a complete raw-data replication
package.

The downstream empirical analysis is documented with the panels, ledger, code, tables,
and figures used in the project. Reconstructing the earliest climate inputs requires
external raw CR2MET / ERA5-Land data and is outside the scope of this public module.

The reorganised repository has not been rerun end-to-end after the public-facing folder
cleanup.

## Software

The module was implemented in R. The principal packages used include `arrow`,
`data.table`, `dplyr`, `fixest`, `clubSandwich`, `ggplot2`, `patchwork`, `scales`, and
`sf`.

## Authorship

This module is part of the group Master Project by Gleb Enson, Rohan Kale, Manuel Seick,
Julia Suárez, and Fatimah Yasin at the Barcelona School of Economics, 2026.
