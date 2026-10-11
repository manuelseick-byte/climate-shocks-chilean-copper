"""
55_channel_stratified.py

Option C structure: adaptation interaction is only theoretically motivated for
channels where desalination has a direct mechanism. Flood channels serve as
placebos. Heat is intermediate (weak indirect mechanism).

Structure
=========
  PRIMARY  — drought channels (meteo, hydro), Spec A and Spec B.
             Desalination directly addresses the underlying scarcity.
             Predicted: b2 > 0 (adaptation moderates the drought response).

  PLACEBO  — flood channels (mine-cell, regional-mean monthly).
             Desalination has no mechanism to moderate rainfall-driven
             disruption. Predicted: b2 ~ 0 if Adapt cleanly captures
             water-source adaptation. Any non-zero finding is interpreted
             as a separate "reshapes exposure" channel via coastal
             infrastructure (long water pipelines, port logistics).

  INTERMEDIATE — heat. Indirect link (water-cooled cooling systems,
             dust suppression). Predicted: weak / null.

Outputs
=======
  results/55__channel_stratified_primary.csv     — drought interactions
  results/55__channel_stratified_placebo.csv     — flood "placebo" interactions
  results/55__channel_stratified_intermediate.csv — heat interaction
  results/55__channel_stratified_summary.txt      — combined narrative

Run from the share-package root:
    python code/55_channel_stratified.py
"""

from pathlib import Path
import warnings
import io
import pandas as pd
import numpy as np
from pyfixest.estimation import feols

warnings.filterwarnings("ignore", category=UserWarning)

ROOT = Path(__file__).resolve().parent.parent
ACUTE = ROOT / "data" / "panels" / "30__acute_mine_month.parquet"
DROUGHT = ROOT / "data" / "panels" / "31__drought_mine_year.parquet"
ADAPT = ROOT / "data" / "panels" / "50__adapt_mine_month.parquet"

OUT_PRIMARY  = ROOT / "results" / "55__channel_stratified_primary.csv"
OUT_PLACEBO  = ROOT / "results" / "55__channel_stratified_placebo.csv"
OUT_INTERMED = ROOT / "results" / "55__channel_stratified_intermediate.csv"
OUT_SUMMARY  = ROOT / "results" / "55__channel_stratified_summary.txt"
OUT_PRIMARY.parent.mkdir(exist_ok=True)

out = io.StringIO()
def log(s=""):
    print(s)
    out.write(s + "\n")

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------
adapt_m = pd.read_parquet(ADAPT)
adapt_y = adapt_m.groupby(["mine_operation", "year"]).agg(
    adapt_year=("adapt_main", "mean"),
).reset_index()

# Acute panel for flood + heat
acute = pd.read_parquet(ACUTE)
acute = acute.merge(adapt_m[["mine_operation", "year", "month", "adapt_main"]],
                    on=["mine_operation", "year", "month"], how="left")
acute = acute[acute["in_sample"]].copy()
acute["calmonth"] = acute["calmonth"].astype(str)

# Drought panel
drought = pd.read_parquet(DROUGHT)
drought = drought.merge(adapt_y, on=["mine_operation", "year"], how="left")
drought = drought[drought["in_sample"]].copy().dropna(subset=["log_kt_year"])

# Annual price from acute panel
price_y = acute.groupby("year")["log_price"].mean().reset_index().rename(
    columns={"log_price": "log_price_year"})
drought = drought.merge(price_y, on="year", how="left")

# Cluster
CLUST = "codregion"

# ---------------------------------------------------------------------------
log("=" * 90)
log(" OPTION C — CHANNEL-STRATIFIED ADAPTATION INTERACTION")
log("=" * 90)
log("")
log(" Theoretical mapping channel <-> adaptation mechanism:")
log("   DROUGHT (meteo, hydro) - desalination directly substitutes for missing water  -> PRIMARY TEST")
log("   FLOOD (mine, region)   - desalination has no mechanism for rainfall events    -> PLACEBO")
log("   HEAT (mine-cell)       - weak indirect link via cooling/dust suppression      -> INTERMEDIATE")
log("")

# ===========================================================================
# PRIMARY: drought channels
# ===========================================================================
log("=" * 90)
log(" PRIMARY — DROUGHT CHANNELS (where adaptation should moderate)")
log("=" * 90)
log("")
log(" log_kt_year ~ Drought + (Drought x Adapt) + Adapt + log_price_year")
log("              | mine_operation + year, cluster = codregion (CR1)")
log("")

drought_channels = [
    ("thr_share_meteo", "Spec A - meteo (any cause, SPEI-12 <= -1)"),
    ("str_share_meteo", "Spec B - meteo (ENSO-attributable)"),
    ("thr_share_hydro", "Spec A - hydro (any cause, SSI-12 <= -1)"),
    ("str_share_hydro", "Spec B - hydro (ENSO-attributable)"),
]

primary_rows = []
for chan, label in drought_channels:
    ix = f"{chan}_x_a"
    drought[ix] = drought[chan] * drought["adapt_year"]
    f = (f"log_kt_year ~ {chan} + {ix} + adapt_year + log_price_year | "
         f"mine_operation + year")
    m = feols(f, data=drought, vcov={"CRV1": CLUST})
    b1 = m.coef().get(chan, np.nan);  p1 = m.pvalue().get(chan, np.nan)
    b2 = m.coef().get(ix, np.nan);    p2 = m.pvalue().get(ix, np.nan)
    b3 = m.coef().get("adapt_year", np.nan); p3 = m.pvalue().get("adapt_year", np.nan)
    marg = b1 + b2
    log(f"--- {label} ---")
    log(f"  b1 (stress on unadapted):     {b1:+.4f}  p = {p1:.3f}")
    log(f"  b2 (stress x adapt):          {b2:+.4f}  p = {p2:.3f}    <- moderation parameter")
    log(f"  b3 (adapt level):             {b3:+.4f}  p = {p3:.3f}")
    log(f"  marg.eff. on adapted (b1+b2): {marg:+.4f}")
    log(f"  N = {int(m._N)}")
    log("")
    primary_rows.append({
        "test_type": "PRIMARY (drought - direct mechanism)",
        "channel": chan, "label": label,
        "b1_stress": b1, "p1": p1,
        "b2_interaction": b2, "p2": p2,
        "b3_adapt": b3, "p3": p3,
        "marg_adapted": marg,
        "N": int(m._N),
    })

# ===========================================================================
# PLACEBO: flood channels
# ===========================================================================
log("=" * 90)
log(" PLACEBO — FLOOD CHANNELS (desalination has no mechanism)")
log("=" * 90)
log("")
log(" log_kt ~ Stress + (Stress x Adapt) + Adapt + log_price")
log("        | mine_operation + calmonth + year, cluster = codregion (CR1)")
log("")
log(" Predicted under H0 (Adapt = pure water-source variable): b2 ~ 0")
log(" Any significant b2 here is interpreted as evidence of an ADDITIONAL exposure")
log(" channel created by coastal infrastructure, not as moderation by desalination.")
log("")

flood_channels = [
    ("stress_flood_mine",     "Spec B - flood mine-cell (daily heavy rain)"),
    ("stress_flood_regmean_M", "Spec B - flood region-mean (monthly regional P)"),
    ("thr_flood_mine",        "Spec A - flood mine-cell (any cause)"),
    ("thr_flood_regmean_M",    "Spec A - flood region-mean (any cause)"),
]

placebo_rows = []
for chan, label in flood_channels:
    if chan not in acute.columns:
        log(f"--- {label}: column not in panel, skip")
        continue
    ix = f"{chan}_x_a"
    acute[ix] = acute[chan] * acute["adapt_main"]
    f = (f"log_kt ~ {chan} + {ix} + adapt_main + log_price | "
         f"mine_operation + calmonth + year")
    m = feols(f, data=acute, vcov={"CRV1": CLUST})
    b1 = m.coef().get(chan, np.nan);  p1 = m.pvalue().get(chan, np.nan)
    b2 = m.coef().get(ix, np.nan);    p2 = m.pvalue().get(ix, np.nan)
    b3 = m.coef().get("adapt_main", np.nan); p3 = m.pvalue().get("adapt_main", np.nan)
    marg = b1 + b2
    log(f"--- {label} ---")
    log(f"  b1 (stress on unadapted):     {b1:+.4f}  p = {p1:.3f}")
    log(f"  b2 (stress x adapt):          {b2:+.4f}  p = {p2:.3f}    <- placebo / exposure channel")
    log(f"  b3 (adapt level):             {b3:+.4f}  p = {p3:.3f}")
    log(f"  marg.eff. on adapted (b1+b2): {marg:+.4f}")
    log(f"  N = {int(m._N)}")
    log("")
    placebo_rows.append({
        "test_type": "PLACEBO (flood - no mechanism)",
        "channel": chan, "label": label,
        "b1_stress": b1, "p1": p1,
        "b2_interaction": b2, "p2": p2,
        "b3_adapt": b3, "p3": p3,
        "marg_adapted": marg,
        "N": int(m._N),
    })

# ===========================================================================
# INTERMEDIATE: heat
# ===========================================================================
log("=" * 90)
log(" INTERMEDIATE — HEAT CHANNEL (weak indirect mechanism)")
log("=" * 90)
log("")
log(" log_kt ~ Stress + (Stress x Adapt) + Adapt + log_price")
log("        | mine_operation + calmonth + year, cluster = codregion (CR1)")
log("")
log(" Predicted: weak / null. Desalination doesn't directly help workers")
log(" cope with heat, but adapted mines tend to be newer with modern HVAC.")
log("")

heat_channels = [
    ("stress_heat_mine", "Spec B - heat mine-cell"),
    ("thr_heat_mine",    "Spec A - heat mine-cell (any cause)"),
]

intermed_rows = []
for chan, label in heat_channels:
    if chan not in acute.columns:
        log(f"--- {label}: column not in panel, skip")
        continue
    ix = f"{chan}_x_a"
    acute[ix] = acute[chan] * acute["adapt_main"]
    f = (f"log_kt ~ {chan} + {ix} + adapt_main + log_price | "
         f"mine_operation + calmonth + year")
    m = feols(f, data=acute, vcov={"CRV1": CLUST})
    b1 = m.coef().get(chan, np.nan);  p1 = m.pvalue().get(chan, np.nan)
    b2 = m.coef().get(ix, np.nan);    p2 = m.pvalue().get(ix, np.nan)
    b3 = m.coef().get("adapt_main", np.nan); p3 = m.pvalue().get("adapt_main", np.nan)
    marg = b1 + b2
    log(f"--- {label} ---")
    log(f"  b1 (stress on unadapted):     {b1:+.4f}  p = {p1:.3f}")
    log(f"  b2 (stress x adapt):          {b2:+.4f}  p = {p2:.3f}")
    log(f"  b3 (adapt level):             {b3:+.4f}  p = {p3:.3f}")
    log(f"  marg.eff. on adapted (b1+b2): {marg:+.4f}")
    log(f"  N = {int(m._N)}")
    log("")
    intermed_rows.append({
        "test_type": "INTERMEDIATE (heat - indirect)",
        "channel": chan, "label": label,
        "b1_stress": b1, "p1": p1,
        "b2_interaction": b2, "p2": p2,
        "b3_adapt": b3, "p3": p3,
        "marg_adapted": marg,
        "N": int(m._N),
    })

# ===========================================================================
# Save
# ===========================================================================
pd.DataFrame(primary_rows).to_csv(OUT_PRIMARY, index=False)
pd.DataFrame(placebo_rows).to_csv(OUT_PLACEBO, index=False)
pd.DataFrame(intermed_rows).to_csv(OUT_INTERMED, index=False)

log("=" * 90)
log(" SUMMARY — interpretation matrix")
log("=" * 90)
log("")
log(" PRIMARY (drought, ENSO-attributable, Spec B):")
for r in primary_rows:
    if "str_" in r["channel"]:
        sig = "**" if r["p2"] < 0.05 else ("*" if r["p2"] < 0.10 else "")
        log(f"   {r['channel']:20s}  b2 = {r['b2_interaction']:+.3f}  p = {r['p2']:.3f}{sig}")
log("")
log(" PLACEBO (flood, ENSO-attributable, Spec B):")
for r in placebo_rows:
    if "stress_" in r["channel"]:
        sig = "**" if r["p2"] < 0.05 else ("*" if r["p2"] < 0.10 else "")
        log(f"   {r['channel']:20s}  b2 = {r['b2_interaction']:+.3f}  p = {r['p2']:.3f}{sig}")
log("")
log(" INTERMEDIATE (heat, ENSO-attributable, Spec B):")
for r in intermed_rows:
    if "stress_" in r["channel"]:
        sig = "**" if r["p2"] < 0.05 else ("*" if r["p2"] < 0.10 else "")
        log(f"   {r['channel']:20s}  b2 = {r['b2_interaction']:+.3f}  p = {r['p2']:.3f}{sig}")
log("")
log(" Interpretation framework:")
log("   - PRIMARY  significant + positive => adaptation moderates drought (hypothesis confirmed)")
log("   - PRIMARY  null or negative     => moderation hypothesis not supported in panel")
log("                                       (event study remains the cleaner test)")
log("   - PLACEBO  null                 => adapt is a clean water-source variable")
log("   - PLACEBO  non-zero             => additional channel: coastal-logistics exposure")
log("                                       created BY adaptation, not eliminated by it")
log("   - INTERMED null or weak         => expected; desalination is not a heat-stress intervention")

with open(OUT_SUMMARY, "w") as f:
    f.write(out.getvalue())

log("")
log(f"Wrote {OUT_PRIMARY.relative_to(ROOT)}")
log(f"Wrote {OUT_PLACEBO.relative_to(ROOT)}")
log(f"Wrote {OUT_INTERMED.relative_to(ROOT)}")
log(f"Wrote {OUT_SUMMARY.relative_to(ROOT)}")
