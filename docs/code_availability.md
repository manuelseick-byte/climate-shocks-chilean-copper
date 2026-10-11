# Code and Reproducibility

This repository is a cleaned research archive prepared from the materials used in the 2026 BSE Master Project. It is intended to make the research workflow inspectable without redistributing large, confidential, or third-party source archives.

## Modules with analytical code

The following modules contain the principal scripts retained from the project:

- `01_water_rights/` - Python data-processing and geospatial scripts;
- `02_enso_climate_exposure/` - R climate construction, panel, regression, robustness, inference, and figure/table scripts;
- `03_enso_adaptation/` - Python decree-panel, figure, and reproduction scripts; and
- `04_adaptation_registry/` - Python adaptation-panel, regression, event-study, diagnostic, and bootstrap scripts.

For public organisation, selected filesystem references were changed to repository-relative paths. The substantive empirical specifications were not intentionally rewritten during the final repository cleanup.

## Modules without complete original code

The archived materials for `05_cost_side_adaptation/` and `06_financial_evidence/` did not contain the complete original scripts used to create every financial screen or derived workbook. No replacement code was reconstructed after the fact.

These modules are therefore published as documented research archives containing the derived analytical files, event coding, supporting data, figures/reports, and methodology notes that were available in the project materials.

## Raw-data reconstruction

Some early stages require external raw files that are not included here, particularly large climate grids, DGA geospatial layers, and third-party annual reports. Downstream analysis can often be inspected or rerun from the compact panels and derived inputs retained in the relevant module.

## End-to-end status

The repository was reorganised and cleaned after completion of the Master Project. It has **not** been rerun end-to-end in its final public folder structure. Module READMEs identify where this limitation matters.

## Licensing

No repository-wide software/data licence is asserted here because the project combines original analysis with third-party datasets, public administrative records, and derived materials whose reuse conditions differ by source. Users should consult the original data providers and source documentation before redistributing underlying data.
