# Climate Shocks on Chilean Copper Mining

**ENSO Attribution, Impact, and Adaptation**  
Barcelona School of Economics - Master Project, International Trade, Finance and Development, 2026

**Gleb Enson · Rohan Kale · Manuel Seick · Julia Suárez · Fatimah Yasin**  
Supervisors: Fernando Broner and Manuel García-Santana

This repository contains the research materials for our Master Project on climate risk in Chilean copper mining. We build a mine-level panel for 19 major copper operations and combine production, climate, ENSO, water-governance, adaptation, and financial information to study where climate shocks become operationally binding and how mining firms respond.

## Research design

The project separates three questions:

1. **Attribution** - which mine-level climate shocks are consistent with ENSO-related regional teleconnections rather than background extremes?
2. **Impact** - how are flood, heat, and persistent drought exposure associated with copper production?
3. **Adaptation** - how do water governance, desalination, administrative responses, investment, and financial outcomes shape the industry's response to climate stress?

The core climate analysis distinguishes general operational-threshold crossings from an ENSO-aligned subset defined using the Oceanic Niño Index (ONI), regional teleconnections, realised local anomalies, and channel-specific operational thresholds.

## Main findings

- **Flood:** unusually wet months are associated with approximately **4.7% lower monthly mine output** in the headline specification. The ENSO-aligned flood subset has the same sign but is estimated less precisely.
- **Heat:** the project does not find a robust production response to heat exposure.
- **Drought:** the main evidence is concentrated in persistent, multi-year exposure. ENSO-aligned drought is negatively associated with annual production in the recent megadrought, but inference is necessarily cautious because the relevant episode is limited.
- **Adaptation:** the available panel does not support a clean causal estimate of desalination's effect on output. Institutional, event-based, and financial evidence instead documents how mines respond through water-source substitution, capital investment, and water-governance mechanisms.

These results should be read with the identification limits described in the paper and module documentation. The repository deliberately distinguishes causal claims from descriptive, institutional, and documentary evidence.

## Repository structure

```text
climate-shocks-chilean-copper/
├── README.md
├── CITATION.cff
├── .gitignore
├── paper/
│   ├── Enson_Kale_Seick_Suarez_Yasin_Climate_Shocks_on_Chilean_Copper_Mining.pdf
│   └── Enson_Kale_Seick_Suarez_Yasin_Climate_Shocks_on_Chilean_Copper_Mining_Presentation.pdf
├── modules/
│   ├── 01_water_rights/
│   ├── 02_enso_climate_exposure/
│   ├── 03_enso_adaptation/
│   ├── 04_adaptation_registry/
│   ├── 05_cost_side_adaptation/
│   └── 06_financial_evidence/
└── docs/
    ├── methodology.md
    ├── data_sources.md
    └── code_availability.md
```

## Research modules

| Module | Purpose | Public contents |
|---|---|---|
| [`01_water_rights`](modules/01_water_rights/) | Water rights, governance constraints, regional water sources, and desalination context | Python code, selected figures, documentation |
| [`02_enso_climate_exposure`](modules/02_enso_climate_exposure/) | Core mine-level climate exposure, ENSO attribution, production regressions, robustness and inference | R code, compact panels, stress ledger, tables, figures |
| [`03_enso_adaptation`](modules/03_enso_adaptation/) | DGA water-scarcity decrees and their relationship to ENSO and physical drought stress | Python code, compact derived data, figures |
| [`04_adaptation_registry`](modules/04_adaptation_registry/) | Mine-level desalination registry and adaptation interaction/event analyses | Python code, panels, registry, diagnostics, tables |
| [`05_cost_side_adaptation`](modules/05_cost_side_adaptation/) | Financial screening and documentary coding of water-related investment/adaptation events | Derived workbooks, event ledger, figure |
| [`06_financial_evidence`](modules/06_financial_evidence/) | Financial statements, closure liabilities, CAPEX/cost responses, and mine/operator evidence | Derived analytical workbooks, supporting data, report |

## Paper and presentation

The final paper and presentation are available in [`paper/`](paper/).

The paper is the authoritative source for the overall empirical design, results, interpretation, and limitations. Individual module folders provide the underlying research materials and more detailed documentation for specific components.

## Data and reproducibility

This is a **cleaned research archive**, not a complete raw-data replication package. Large raw climate files, geospatial layers, confidential data, and third-party annual reports are not redistributed. Where possible, the repository retains compact analysis panels, registries, derived datasets, code, and final outputs.

Some modules can be rerun from the included derived inputs; other early data-construction stages require external source files. The repository was reorganised for documentation and research-portfolio use and has not been rerun end-to-end after the final folder cleanup.

See [`docs/data_sources.md`](docs/data_sources.md) and [`docs/code_availability.md`](docs/code_availability.md) for details.

## Software

The project primarily uses **R** and **Python**. Module-specific package requirements and workflow notes are documented in the relevant module README files.

## Citation

If you use or refer to this repository, please cite the Master Project using the metadata in [`CITATION.cff`](CITATION.cff).
