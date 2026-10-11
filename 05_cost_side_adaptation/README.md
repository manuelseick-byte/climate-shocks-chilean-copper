# Cost-Side Adaptation Evidence

This module forms part of the Barcelona School of Economics Master Project
*Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation*.

It investigates whether part of the economic burden of water stress appears on the
investment and cost side of mining operations rather than only in observed production.

## Research question

The main climate regressions show comparatively muted output responses for several
stress channels. This module asks whether mines maintained production partly by absorbing
water stress through capital expenditure, operating-cost pressure, and water-related
investment.

The analysis therefore combines financial-statement screening with manual review of
operator annual reports.

## Research design

The module uses a two-step approach.

First, mine-year financial observations are screened for unusual operating-margin and
capital-expenditure outcomes relative to mine-specific benchmarks:

- an operating-margin model relates operating margin to the copper price and mine fixed
  effects;
- a capital-expenditure model relates log capex to log revenue and mine fixed effects.

Low residual operating margins and high residual capex observations are used to identify
mine-years for closer documentary review.

Second, the corresponding annual reports and financial statements are read manually to
classify the flagged episodes.

The financial screen is a **search device rather than a causal estimator**. Its purpose is
to direct attention to mine-years where the underlying operator documents can be examined
for water-related investment, operational stress, or unrelated investment activity.

## Event classification

The manual event ledger classifies 32 flagged mine-years into four groups:

1. **Direct water-source adaptation** — desalination or seawater infrastructure is
   operational or commissioning in the flagged year;
2. **Precursor / build-phase capex** — spending associated with a water-supply project
   that commissions later, often bundled with broader capacity investment;
3. **Water-stress outcomes without contemporaneous adaptation** — operational
   consequences, closures, or forward commitments linked to water constraints; and
4. **Not adaptation** — investment or margin movements explained by capacity expansion,
   commodity-price conditions, or other non-water factors.

This classification is important because the financial screen also identifies many
non-adaptation events. The annual-report review is therefore used to distinguish
water-related evidence from ordinary investment and commodity-cycle variation.

## Main evidence

The documentary evidence is consistent with water stress affecting mining firms through
several channels beyond contemporaneous output.

Examples include:

- commissioning and construction of desalination / seawater infrastructure at operations
  such as Spence, Quebrada Blanca, and Collahuasi;
- build-phase investment occurring years before the commissioning dates used in the
  adaptation panel;
- operator-reported water restrictions and water-related operating constraints; and
- cases in which continued continental-water dependence is associated with operational
  limits or closure.

At the same time, desalination projects are frequently bundled with concentrator
expansions and broader mine-development programmes. The financial evidence therefore does
not identify a separate causal effect of water adaptation on costs or output.

The module is best interpreted as documentary and financial evidence supporting the
project's adaptation narrative, not as an independent causal design.

## Repository structure

```text
.
├── README.md
├── code/
│   └── README.md
├── data/
│   ├── financials/
│   └── event_coding/
├── results/
│   └── figures/
└── notes/
```

## Data

The public module contains:

- `Mining_Firms_Financials.xlsx` — the mine / operator financial dataset used for the
  cost-side screening;
- `62__annual_report_worklist.xlsx` — the annual-report review worklist and source
  references;
- `63__event_ledger.xlsx` — the manually coded event classification;
- `mines_desal.csv` — the desalination/adaptation registry used for cross-referencing
  mines and commissioning dates.

The underlying operator annual reports and financial statements are not redistributed.
The worklist records the relevant sources and page references used in the manual review.

## Code availability

The uploaded project archive contains the cost-side datasets, event ledger, documentary
worklist, and final figure, but it does **not** contain the original scripts used to
generate the financial-anomaly screen referred to as scripts 60–61 in the internal
working notes.

For that reason, this public module is presented as a documented research archive rather
than as a complete executable replication package.

The separate adaptation-registry module contains the Python code for the mine-level
desalination interaction analysis and is not duplicated here.

## Results

`results/figures/64__capex_intensity_switchers.png` provides the final visual summary of
capex intensity around the adaptation switchers retained from the source archive.

The detailed documentary evidence is contained in the event ledger and annual-report
worklist rather than in a standalone public report.

## Limitations

Several limitations are important for interpretation:

- desalination investment is often bundled with broader capacity expansion;
- financial reporting is sometimes available only at the firm or group level rather than
  the individual mine level;
- a residual-based flag can be triggered by collapsing revenue or commodity-price
  conditions rather than by unusually high physical investment;
- annual-report narratives are used to classify events, so the exercise is documentary
  rather than experimental; and
- the evidence does not establish that adaptation caused changes in operating margins,
  capex, or production.

## Authorship

This module is part of the group Master Project by Gleb Enson, Rohan Kale, Manuel Seick,
Julia Suárez, and Fatimah Yasin at the Barcelona School of Economics, 2026.
