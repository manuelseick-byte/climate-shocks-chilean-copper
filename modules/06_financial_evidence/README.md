# Financial Evidence

This module forms part of the Barcelona School of Economics Master Project
*Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation*.

It complements the mine-level climate and adaptation modules with financial-statement
evidence on how water stress and adaptation may appear in mining-company balance sheets,
capital expenditure, operating costs, closure liabilities, and cash-flow measures.

## Research question

The module asks whether climate and water-stress episodes are accompanied by observable
financial responses at Chilean copper operations and whether the patterns differ for
mines with desalination or seawater infrastructure.

The financial evidence is used as a complementary layer rather than as a standalone
causal design. Reporting frequency, mine-level disclosure, ownership structure, and
accounting rules vary substantially across operators, so the results are interpreted
primarily as descriptive and documentary evidence.

## Analytical components

The module contains four main analytical components:

1. **Financial signals** — mine- and operator-level CAPEX, operating-cost, severance,
   stripping-cost, and closure-provision indicators;
2. **Closure liabilities** — within-mine trajectories in rehabilitation and closure
   provisions, with attention to accounting and regulatory confounders;
3. **Master financial panel** — a mine-by-year panel combining financial, production,
   climate, adaptation, and ownership information; and
4. **CAPEX / cost lag analysis** — event-window evidence around identified water-stress
   episodes.

The consolidated report in `results/report/` provides the main financial-response
visualisation used in the project.

## Repository structure

```text
.
├── README.md
├── data/
│   ├── financial_signals/
│   ├── closure_liabilities/
│   ├── master_panel/
│   ├── lag_analysis/
│   └── supporting/
├── results/
│   └── report/
└── notes/
```

## Data

### Financial signals

`data/financial_signals/CHC_FinancialSignals_WaterStress.xlsx`

Contains the financial-signal extraction used to examine CAPEX, stripping costs,
severance, operating costs, and closure-provision patterns.

### Closure liabilities

`data/closure_liabilities/CHC_closure_liability_analysis.xlsx`

Contains the closure-liability analysis and within-mine provision trajectories used for
the rehabilitation / closure evidence.

### Master panel

`data/master_panel/CHC_Panel_Merged.xlsx`

Contains the mine-by-year analytical panel combining financial variables with production,
water-stress, ENSO, and adaptation indicators.

`data/master_panel/CHC_Canonical_Financial_Panel.xlsx`

Contains the canonical financial panel used for the final financial-response report.

### Lag analysis

`data/lag_analysis/CHC_CAPEX_OPEX_LagAnalysis.xlsx`

Contains the event-window analysis around documented water-stress episodes.

### Supporting data

The supporting folder contains compact CSV inputs for prices, mine operations, anomalies,
and documented breaches.

## Main interpretation

The financial evidence is consistent with several channels through which water stress can
affect mining firms without generating an immediate one-for-one fall in annual output.

In particular, the project documents:

- substantial desalination and seawater-related capital investment at several large mines;
- operating and financial adjustments around documented stress episodes;
- closure-provision changes at mines facing tighter water constraints or shortened mine
  lives; and
- important differences between continental-water-dependent and adapted mines.

However, these patterns do **not** by themselves identify a causal financial effect of
ENSO or desalination.

Several confounders are material:

- copper-price movements;
- major capacity-expansion programmes;
- changing discount rates used to value closure liabilities;
- accounting-standard changes;
- mine acquisitions and ownership changes;
- consolidated rather than mine-level financial reporting; and
- overlapping climate and regulatory events.

The strongest use of this module is therefore as corroborating financial and documentary
evidence alongside the project's physical climate and mine-level production analyses.

## Closure-liability evidence

Closure provisions are especially informative but require cautious interpretation.

They can move mechanically with discount rates, legal obligations, accounting rules, and
revised mine-life assumptions. For that reason, the public project avoids treating
cross-mine provision levels as directly comparable without adjustment.

The analysis places greater weight on within-mine trajectories and clearly documented
regulatory episodes.

## CAPEX and cost evidence

The lag-analysis workbook studies financial variables in windows around identified
water-stress episodes.

The corrected CAPEX measure is normalised using production and unit-consistency checks.
The exercise is descriptive: event windows can overlap, stress spells can last several
years, and major desalination projects are often bundled with broader mine expansions.

Accordingly, CAPEX increases around stress or adaptation episodes are interpreted as
supportive evidence of an investment response rather than as a causal treatment effect.

## Annual reports

The original working project used operator annual reports and financial statements to
construct and validate several of the financial variables.

Those source documents are **not redistributed in this repository** because of file size
and third-party document considerations. The public module instead retains the derived
analytical workbooks, compact supporting data, and ownership documentation.

## Code availability

The retained project materials for this module do not contain the original extraction and
analysis scripts used to construct all financial datasets.

No post-hoc replacement code has been created for the public repository. This module is
therefore best viewed as a cleaned research archive documenting the financial evidence
used in the Master Project rather than as a complete executable replication package.

## Results

The principal public-facing output is:

`results/report/Climate_Shock_Financial_Response.pdf`

It compares general and ENSO-attributed flood and drought exposure and documents the
corresponding financial trajectories for the mine/operator sample.

## Limitations

Financial disclosure is uneven across firms and years. Some variables are reported only
at consolidated company level, while other series are available at the individual-mine
level.

Codelco divisional observations require particular caution because financial values can
be inherited from consolidated reporting and therefore should not be treated as
independent mine-level financial observations.

The financial module should therefore be read together with the climate-exposure and
adaptation modules, which contain the main mine-level empirical designs.

## Authorship

This module is part of the group Master Project by Gleb Enson, Rohan Kale, Manuel Seick,
Julia Suárez, and Fatimah Yasin at the Barcelona School of Economics, 2026.
