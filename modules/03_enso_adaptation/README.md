# ENSO-Focused Adaptation

This module forms part of the Barcelona School of Economics Master Project
*Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation*.

It examines whether Chilean water-scarcity decrees provide an administrative
signal of ENSO-related water stress in copper-mining regions and how that signal
compares with the physical drought channel used elsewhere in the project.

## Research question

The module asks whether water-scarcity decrees issued by Chile's Dirección
General de Aguas (DGA) co-move with ENSO conditions and with independently
constructed measures of physical drought stress.

The analysis is intended as a complementary institutional and adaptation layer:
decrees are decision-relevant policy responses to water scarcity, but they are
also lagged, persistent, and partly discretionary.

## Main findings

The direct relationship between July-centered ENSO conditions and decree
activity is weak once secular trends are removed.

The stronger pattern appears in the two links connecting ENSO, physical drought
stress, and the administrative response:

- JJA ONI is negatively associated with the physical drought-stress rate in the
  mining regions;
- physical drought stress is positively associated with water-scarcity decree
  activity; and
- decree activity is considerably more common in the central, surface-water-
  exposed mining regions than in the arid northern mining regions.

The results therefore support using physical hydrology as the more informative
forecasting target, while treating DGA decrees as a downstream institutional
confirmation of water stress rather than as a leading indicator.

These relationships are descriptive and should not be interpreted as a causal
effect of ENSO on administrative decisions.

## Repository structure

```text
.
├── README.md
├── code/
│   ├── 00_translate_decrees.py
│   ├── 01_build_decree_panel.py
│   ├── 02_make_figures.py
│   └── 03_reproduce_decrees.py
├── data/
│   ├── raw/
│   └── derived/
├── results/
│   ├── figures/
│   └── report/
└── notes/
```

## Data

The module uses the historical DGA water-scarcity decree registry and combines it
with ENSO and physical drought-stress inputs derived from the project's master
stress ledger.

The public repository includes:

- the DGA decree registry used for the analysis;
- an English-language translated registry;
- region-by-water-year decree counts;
- July-centered ONI values;
- regional winter teleconnection measures; and
- regional physical drought-stress rates.

The master stress ledger itself is maintained in the ENSO climate-exposure
module and is not duplicated here.

## Empirical design

DGA decrees are assigned to water years running from April of year Y through
March of year Y+1. The relevant ENSO predictor is the JJA ONI of year Y, which
corresponds to the austral winter contributing to that water year.

The analysis distinguishes:

- central mining regions with stronger ENSO-related precipitation variation; and
- the permanently arid northern mining regions.

It reports Pearson and Spearman correlations and also examines detrended
relationships to reduce the influence of the long Chilean megadrought and the
expansion of the decree regime over time.

A concordance exercise compares:

1. JJA ONI with the project's physical drought-stress rate; and
2. the physical drought-stress rate with decree counts.

Because the decree panel begins only in 2008, the sample is short and the
results should be interpreted as descriptive evidence.

## Running the code

The bundled derived files are sufficient to reproduce the reported decree
statistics and figures:

```bash
python code/02_make_figures.py
python code/03_reproduce_decrees.py
```

Rebuilding the derived files from the raw DGA registry requires the master stress
ledger from the ENSO climate-exposure module. Supply its location explicitly:

```bash
LEDGER_PATH=/path/to/20__master_stress_ledger.csv python code/01_build_decree_panel.py
```

## Reproducibility

The statistical reproduction script contains a deterministic verification block
for the principal reported values.

For public documentation, only file paths and folder locations have been adapted
to the repository structure. The analytical calculations have not been
rewritten.

## Results

Final figures are available in `results/figures/`.

The forecasting/adaptation chapter prepared for the project is available at:

`results/report/ENSO_Forecasting_Chapter.pdf`

## Software

The module was implemented in Python. The principal packages used are `pandas`,
`numpy`, `scipy`, `matplotlib`, `xlrd`, and `openpyxl`.

## Authorship

This module is part of the group Master Project by Gleb Enson, Rohan Kale,
Manuel Seick, Julia Suárez, and Fatimah Yasin at the Barcelona School of
Economics, 2026.
