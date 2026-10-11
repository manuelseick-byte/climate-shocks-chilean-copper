# Data Sources and Availability

The project combines mine production, climate, ENSO, water-governance, adaptation, and financial information. This public repository retains compact analysis inputs and derived datasets where appropriate but does not redistribute every raw source file.

## Core production and market data

- **Comisión Chilena del Cobre (COCHILCO):** monthly fine-copper production for the 19 operations in the project; annual mining and water-use statistics.
- **London Metal Exchange (LME) copper price:** monthly copper-price control used in the production analysis.

## Climate and ENSO data

- **CR2MET v2.5:** daily historical climate fields used for the mine-level climate panel.
- **ERA5-Land:** recent climate extension and inputs to the drought/SPEI construction.
- **NOAA Climate Prediction Center:** Oceanic Niño Index (ONI) used to classify ENSO state.
- **NOAA/IRI ENSO outlooks:** probabilistic forecast information used in the broader adaptation/forecasting work.

Large raw gridded climate files are not redistributed. The climate-exposure module retains compact derived panels, registries, and the master stress ledger used in downstream analysis.

## Water governance and water use

- **Dirección General de Aguas (DGA):** Catastro Público de Aguas, Mapoteca Digital layers, water-scarcity decrees, and related water-governance information.
- **COCHILCO:** *Consumo de Agua en la Minería del Cobre* and related public mining-water statistics.

Large DGA geospatial files and intermediate workbooks are not redistributed in the public archive.

## Adaptation and desalination

The mine-level adaptation registry is assembled from public operator disclosures and sector sources, including:

- operator annual reports / regulatory filings;
- COCHILCO water-use information; and
- public desalination/seawater infrastructure records used during the project.

The public repository retains the compact adaptation registry and analysis panels rather than the full documentary source archive.

## Financial information

Financial evidence is derived from audited operator annual reports and financial statements, including filings with Chile's **Comisión para el Mercado Financiero (CMF)** and relevant home-country reporting sources where applicable.

The public archive contains derived financial workbooks and compact supporting data. The underlying annual reports are not redistributed because of file size and third-party-document considerations.

## Confidential or unavailable data

Firm-level water-consumption microdata are not included. The project relies on publicly available aggregate water-use information and the disclosed sources described above.

## Module-level documentation

Each module contains its own `README.md` and, where relevant, `data/README.md` with more specific information about retained files, required external inputs, and reconstruction limits.
