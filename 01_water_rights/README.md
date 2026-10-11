# Water Rights and Water-Governance Response

This module forms part of the Barcelona School of Economics Master Project **Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation** (2026). It documents the institutional and water-governance environment facing Chilean copper mining and the sector's transition toward alternative water sources.

## Module overview

The module combines Chilean water-rights and water-governance information with mining-sector water-use data to describe how major copper operations are exposed to constraints on continental water resources. The analysis focuses on granted water rights, legally restricted or prohibited aquifers and exhausted river basins, regional water-source patterns, and mine-level desalination status.

The module is descriptive and institutional rather than a causal evaluation of the 2022 Water Code reform. Its role in the broader project is to document the governance and adaptation environment in which climate shocks affect mining operations.

## Data sources

The analysis draws primarily on:

- **Dirección General de Aguas (DGA)** — public water-rights registry and Mapoteca Digital layers, including restriction and prohibition zones, exhausted river basins, protected water bodies, and related governance constraints;
- **Comisión Chilena del Cobre (COCHILCO)** — annual mining statistics and water-consumption reports, including water use by source and region; and
- the project's **mine registry** — locations and identifiers for the major copper operations covered by the Master Project.

Raw source files are not redistributed in this repository. See [`data/README.md`](data/README.md) for the expected inputs and source documentation.

## Methods

The module uses Python-based data processing and geospatial analysis to:

1. harmonise and translate DGA water-rights records;
2. combine regional water-rights files into a common panel;
3. identify mining-related rights using curated company-name mappings;
4. construct national and regional water-source summaries from COCHILCO statistics;
5. map mines relative to legally restricted or prohibited aquifers and exhausted river basins; and
6. combine governance constraints with mine-level desalination information for descriptive adaptation analysis.

The geospatial routines use `geopandas`, while the data-processing and chart routines rely primarily on `pandas` and `matplotlib`.

## Repository structure

```text
01_water_rights/
├── README.md
├── code/
├── data/
│   └── README.md
├── results/
│   ├── Water_Governance_Section.pdf
│   ├── figures/
│   └── README.md
└── notes/
    └── methodology.md
```

## Code

The `code/` folder contains the main scripts used for the public-facing parts of this module:

- `01_translate_dga_registry.py` — translates and harmonises DGA registry fields;
- `02_build_water_rights_panel.py` — combines translated regional files and constructs panel outputs;
- `03_filter_mining_operations.py` — identifies water rights associated with the mining operations in the project;
- `04_build_water_source_charts.py` — constructs national and regional water-source summaries from COCHILCO data;
- `05_build_region_maps.py` — maps regional water-rights and non-use-fee information; and
- `06_build_water_governance_maps.py` — maps major mines relative to water-governance constraints and desalination status.

For public documentation, filesystem paths were normalised to the repository structure. The substantive analytical logic of the selected scripts was not rewritten. The reorganised module has not been rerun as a complete end-to-end package after these path changes.

## Results

The `results/` folder contains the final water-governance section used during the project and a selected set of public-facing figures. These include maps of major copper mines relative to closed or constrained water basins as well as regional summaries of water rights, water-source composition, and desalination status.

The figures are intended to document institutional exposure and adaptation patterns; they should not be interpreted as identifying a causal effect of regulation on firm behaviour.

## Data availability

Large raw DGA and COCHILCO source files, geospatial layers, and intermediate Excel workbooks are not redistributed here. This keeps the public repository compact and preserves clear attribution to the original data providers.

## Authorship

This module is part of a joint Master Project by **Gleb Enson, Rohan Kale, Manuel Seick, Julia Suárez, and Fatimah Yasin** at the Barcelona School of Economics in 2026.
