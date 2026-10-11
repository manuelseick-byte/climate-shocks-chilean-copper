"""
52_drought_regressions.py

Drought specification with adaptation interaction. Augments the deck baseline
(panels/31__drought_mine_year.parquet) with annual adaptation from
panels/50__adapt_mine_month.parquet (aggregated to mine-year as share of
months adapted).

Specification:
    log_kt_year ~ a_i + d_y
                  + b1*Drought + b2*(Drought x Adapt) + b3*Adapt
                  + theta*log_p_year + eps
Cluster-robust SEs at the region level (codregion, 7 clusters; CR1).

Note: log_price_year may be absorbed by the year fixed effect; pyfixest
drops it automatically when collinear.

Outputs:
    results/52__drought_baseline.csv
    results/52__drought_interaction.csv

Run from the share-package root:
    python code/52_drought_regressions.py
"""

from pathlib import Path
import warnings
import pandas as pd
import numpy as np
from pyfixest.estimation import feols

warnings.filterwarnings("ignore", category=UserWarning)

ROOT = Path(__file__).resolve().parent.parent
DROUGHT = ROOT / "data" / "panels" / "31__drought_mine_year.parquet"
ADAPT   = ROOT / "data" / "panels" / "50__adapt_mine_month.parquet"
ACUTE   = ROOT / "data" / "panels" / "30__acute_mine_month.parquet"   # for annual price
OUT_BASELINE = ROOT / "results" / "52__drought_baseline.csv"
OUT_INTERACT = ROOT / "results" / "52__drought_interaction.csv"
OUT_BASELINE.parent.mkdir(exist_ok=True)

# ---------------------------------------------------------------------------
# Load + merge
# ---------------------------------------------------------------------------
drought = pd.read_parquet(DROUGHT)
adapt_m = pd.read_parquet(ADAPT)
adapt_y = adapt_m.groupby(["mine_operation", "year"]).agg(
    adapt_year=("adapt_main", "mean"),          # share of months adapted in year
    adapt_year_strict=("adapt_strict", "mean"),
    adapt_share_year=("adapt_share", "mean"),
).reset_index()

df = drought.merge(adapt_y, on=["mine_operation", "year"], how="left")
df = df[df["in_sample"]].copy().dropna(subset=["log_kt_year"])

# Annual price from acute panel
acute = pd.read_parquet(ACUTE)
price_y = acute.groupby("year")["log_price"].mean().reset_index().rename(
    columns={"log_price": "log_price_year"})
df = df.merge(price_y, on="year", how="left")

print(f"[52] Drought sample: {len(df):,} mine-year obs across "
      f"{df['mine_operation'].nunique()} mines")
print(f"[52] Year range: {df['year'].min()}–{df['year'].max()}")
print(f"[52] adapt_year mean (share of months adapted): {df['adapt_year'].mean():.3f}")

# ---------------------------------------------------------------------------
# Regressions
# ---------------------------------------------------------------------------
fe = "mine_operation + year"
clust = "codregion"

channels = [
    ("thr_share_meteo", "Spec A — meteo (any cause, SPEI-12 <= -1)"),
    ("str_share_meteo", "Spec B — meteo (ENSO-attributable)"),
    ("thr_share_hydro", "Spec A — hydro (any cause, SSI-12 <= -1)"),
    ("str_share_hydro", "Spec B — hydro (ENSO-attributable)"),
]
adapt_vars = ["adapt_year", "adapt_year_strict", "adapt_share_year"]

baseline_rows = []
interaction_rows = []

print("\n[52] Specification: log_kt_year ~ Drought (x Adapt) + log_price_year | mine + year")
print("[52] Cluster: codregion (7 clusters, CR1)")
print()

for chan, label in channels:
    print(f"==== {label} ({chan}) ====")
    m0 = feols(f"log_kt_year ~ {chan} + log_price_year | {fe}", data=df,
               vcov={"CRV1": clust})
    b0  = m0.coef().get(chan, np.nan)
    se0 = m0.se().get(chan, np.nan)
    p0  = m0.pvalue().get(chan, np.nan)
    print(f"  baseline (no adapt): b = {b0:+.4f} ({se0:.4f}) p={p0:.3f}  N={int(m0._N)}")
    baseline_rows.append({"channel": chan, "b": b0, "se": se0, "p": p0,
                          "N": int(m0._N)})

    for av in adapt_vars:
        ix = f"{chan}_x_{av}"
        df[ix] = df[chan] * df[av]
        m1 = feols(f"log_kt_year ~ {chan} + {ix} + {av} + log_price_year | {fe}",
                   data=df, vcov={"CRV1": clust})
        b1  = m1.coef().get(chan, np.nan)
        b2  = m1.coef().get(ix, np.nan)
        b3  = m1.coef().get(av, np.nan)
        se1 = m1.se().get(chan, np.nan)
        se2 = m1.se().get(ix, np.nan)
        se3 = m1.se().get(av, np.nan)
        p1  = m1.pvalue().get(chan, np.nan)
        p2  = m1.pvalue().get(ix, np.nan)
        p3  = m1.pvalue().get(av, np.nan)
        marg = b1 + b2
        print(f"  + {av:18s}: b1={b1:+.4f}(p={p1:.2f})  "
              f"b2={b2:+.4f}(p={p2:.2f})  b3={b3:+.4f}(p={p3:.2f})  "
              f"marg(adapted)={marg:+.4f}")
        interaction_rows.append({
            "channel": chan, "adapt_var": av,
            "b1_stress": b1, "se1": se1, "p1": p1,
            "b2_interaction": b2, "se2": se2, "p2": p2,
            "b3_adapt_level": b3, "se3": se3, "p3": p3,
            "marg_eff_adapted": marg,
            "N": int(m1._N),
        })

pd.DataFrame(baseline_rows).to_csv(OUT_BASELINE, index=False)
pd.DataFrame(interaction_rows).to_csv(OUT_INTERACT, index=False)
print(f"\n[52] Wrote {OUT_BASELINE.relative_to(ROOT)}")
print(f"[52] Wrote {OUT_INTERACT.relative_to(ROOT)}")
