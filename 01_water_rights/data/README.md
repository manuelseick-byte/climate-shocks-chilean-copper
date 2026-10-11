# Data

Raw data are not redistributed in this public module.

## Main source inputs

The analysis used public information from the following sources:

- **DGA water-rights registry (Catastro Público de Aguas)**: regional and national records of granted water rights.
- **DGA Mapoteca Digital**: geospatial layers for restriction/prohibition zones, exhausted river basins and related water-governance constraints.
- **COCHILCO statistical yearbooks and water-consumption reports**: mining water use by source and region.
- **Master Project mine registry**: mine names and geographic coordinates used across the broader project.

## Expected local structure

The selected public scripts use the following repository-relative locations:

```text
data/
├── raw/
│   ├── dga/
│   ├── dga_mapoteca/
│   ├── cochilco/
│   ├── geo/
│   └── mine_registry/
└── processed/
```

The raw files are intentionally omitted. Users wishing to reproduce the analysis should obtain the corresponding source materials from DGA and COCHILCO and place them under the locations above.

Processed files such as mine-level water-rights panels and desalination-status tables were generated during the project but are also omitted from the public portfolio repository.
