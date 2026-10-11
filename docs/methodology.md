# Methodology Overview

This document explains how the six public modules fit into the empirical design of *Climate Shocks on Chilean Copper Mining: ENSO Attribution, Impact, and Adaptation*.

## 1. Core mine-level climate panel

The empirical core covers 19 Chilean copper operations across seven copper-producing regions. Monthly fine-copper output is matched to mine-level and regional climate information. The main priced estimation sample runs from 2006 to 2025.

Climate exposure is constructed from CR2MET v2.5 for the historical period and ERA5-Land for the recent extension. The project studies three operational channels:

- acute flood exposure;
- acute heat exposure; and
- persistent drought exposure.

The climate-exposure workflow, master stress ledger, mine-month/mine-year panels, regressions, validation exercises, robustness checks, and small-cluster inference are documented in `modules/02_enso_climate_exposure/`.

## 2. ENSO attribution

The project does not label every climate extreme as ENSO-driven. A stress episode is classified as ENSO-aligned only when the relevant conditions are jointly satisfied: an active Oceanic Niño Index phase, a regional teleconnection in the expected direction, a realised local anomaly, and a channel-specific operational threshold crossing.

This distinction separates general climate stress from the subset consistent with the project's ENSO attribution rule.

## 3. Production effects

Flood and heat are analysed mainly at the mine-month level using fixed-effects specifications. Persistent drought is treated as a slow-moving channel and summarised over multi-year exposure windows at the mine-year level.

Because the core panel has only seven regional clusters, the inference strategy supplements conventional clustered standard errors with small-sample corrections, wild-cluster bootstrap procedures, leave-one-region checks, and event-based robustness exercises where appropriate.

## 4. Water governance and administrative response

`modules/01_water_rights/` documents the institutional environment in which mines face water constraints. It combines DGA water-rights and governance information with COCHILCO water-use statistics and mine locations.

`modules/03_enso_adaptation/` studies DGA water-scarcity decrees as an administrative response to water stress and compares decree activity with ENSO conditions and physical drought exposure. These exercises are descriptive and institutional rather than causal evaluations of water policy.

## 5. Desalination and adaptation

`modules/04_adaptation_registry/` constructs mine-level desalination/seawater indicators and examines whether estimated climate sensitivity changes after commissioning.

Desalination is not randomly assigned. Adapted mines differ systematically from non-adapted mines, and commissioning dates can coincide with broader capacity-expansion programmes. The full-panel interactions are therefore treated as diagnostic evidence rather than a clean causal estimate. Event-based, switcher-focused, placebo, leave-one-out, and wild-cluster-bootstrap exercises provide additional context.

## 6. Cost-side and financial evidence

`modules/05_cost_side_adaptation/` uses financial screening and annual-report coding to identify water-related investment and adaptation episodes. It is a documentary classification exercise rather than an independent causal design.

`modules/06_financial_evidence/` examines CAPEX, operating-cost measures, closure liabilities, and other financial outcomes. Interpretation is deliberately cautious because disclosure is uneven and financial variables are affected by copper prices, accounting rules, discount rates, ownership changes, and major investment programmes.

## 7. Interpretation

The project therefore combines different evidentiary layers:

- the mine-level panel provides the main quantitative evidence on climate exposure and production;
- water-rights and decree modules document institutional constraints and responses;
- the adaptation registry studies the timing of desalination and seawater infrastructure; and
- cost-side and financial modules provide corroborating documentary evidence about how firms absorb or respond to water stress.

The final paper in `paper/` is the authoritative source for the complete specification choices, results, and limitations.
