"""
51_acute_regressions.py

Run the headline adaptation interaction regressions for the acute channels
(flood-mine, flood-region-mean, heat-mine). Augments the deck baseline
(panels/30__acute_mine_month.parquet) with the adapt_main / adapt_strict /
adapt_share moderators from panels/50__adapt_mine_month.parquet.

Specification:
    log_kt ~ a_i + g_{r,m} + d_y
             + b1*Stress + b2*(Stress x Adapt) + b3*Adapt + theta*log_p + eps
Cluster-robust SEs at the region level (codregion, 7 clusters; CR1).

Outputs:
    results/51__acute_baseline.csv     — deck-style baseline, no adapt
    results/51__acute_interaction.csv  — all (channel x adapt) interactions

Run from the share-package root:
    python code/51_acute_regressions.py

Requires: pandas, pyarrow, pyfixest
"""

from pathlib import Path
import warnings
import pandas as pd
import numpy as np
from pyfixest.estimation import feols

warnings.filterwarnings("ignore", category=UserWarning)

ROOT = Path(__file__).resolve().parent.parent
ACUTE = ROOT / "data" / "panels" / "30__acute_mine_month.parquet"
ADAPT = ROOT / "data" / "panels" / "50__adapt_mine_month.parquet"
OUT_BASELINE = ROOT / "results" / "51__acute_baseline.csv"
OUT_INTERACT = ROOT / "results" / "51__acute_interaction.csv"
OUT_BASELINE.parent.mkdir(exist_ok=True)

# ---------------------------------------------------------------------------
# Load + merge
# ---------------------------------------------------------------------------
acute = pd.read_parquet(ACUTE)
adapt = pd.read_parquet(ADAPT)[["mine_operation", "year", "month",
                                 "adapt_main", "adapt_strict", "adapt_share"]]
df = acute.merge(adapt, on=["mine_operation", "year", "month"], how="left")
df = df[df["in_sample"]].copy()
df["calmonth"] = df["calmonth"].astype(str)

print(f"[51] Merged panel: {len(df):,} mine-month obs across "
      f"{df['mine_operation'].nunique()} mines")
print(f"[51] adapt_main share in sample:   {df['adapt_main'].mean():.3f}")
print(f"[51] adapt_strict share in sample: {df['adapt_strict'].mean():.3f}")

# ---------------------------------------------------------------------------
# Regressions
# ---------------------------------------------------------------------------
fe = "mine_operation + calmonth + year"
clust = "codregion"
channels = ["stress_flood_mine", "stress_flood_regmean_M", "stress_heat_mine"]
adapt_vars = ["adapt_main", "adapt_strict", "adapt_share"]

baseline_rows = []
interaction_rows = []

print("\n[51] Specification: log_kt ~ Stress (x Adapt) + log_price | mine + calmonth + year")
print("[51] Cluster: codregion (7 clusters, CR1)")
print()

for stress in channels:
    print(f"==== channel: {stress} ====")
    # Baseline (no adapt) — matches the deck
    m0 = feols(f"log_kt ~ {stress} + log_price | {fe}", data=df,
               vcov={"CRV1": clust})
    b0  = m0.coef().get(stress, np.nan)
    se0 = m0.se().get(stress, np.nan)
    p0  = m0.pvalue().get(stress, np.nan)
    print(f"  baseline (no adapt): b = {b0:+.4f} ({se0:.4f}) p={p0:.3f}  N={int(m0._N)}")
    baseline_rows.append({"channel": stress, "b": b0, "se": se0, "p": p0,
                          "N": int(m0._N)})

    for av in adapt_vars:
        ix = f"{stress}_x_{av}"
        df[ix] = df[stress] * df[av]
        m1 = feols(f"log_kt ~ {stress} + {ix} + {av} + log_price | {fe}",
                   data=df, vcov={"CRV1": clust})
        b1  = m1.coef().get(stress, np.nan)
        b2  = m1.coef().get(ix, np.nan)
        b3  = m1.coef().get(av, np.nan)
        se1 = m1.se().get(stress, np.nan)
        se2 = m1.se().get(ix, np.nan)
        se3 = m1.se().get(av, np.nan)
        p1  = m1.pvalue().get(stress, np.nan)
        p2  = m1.pvalue().get(ix, np.nan)
        p3  = m1.pvalue().get(av, np.nan)
        marg = b1 + b2
        print(f"  + {av:14s}: b1={b1:+.4f}(p={p1:.2f})  "
              f"b2={b2:+.4f}(p={p2:.2f})  b3={b3:+.4f}(p={p3:.2f})  "
              f"marg(adapted)={marg:+.4f}")
        interaction_rows.append({
            "channel": stress, "adapt_var": av,
            "b1_stress": b1, "se1": se1, "p1": p1,
            "b2_interaction": b2, "se2": se2, "p2": p2,
            "b3_adapt_level": b3, "se3": se3, "p3": p3,
            "marg_eff_adapted": marg,
            "N": int(m1._N),
        })

pd.DataFrame(baseline_rows).to_csv(OUT_BASELINE, index=False)
pd.DataFrame(interaction_rows).to_csv(OUT_INTERACT, index=False)
print(f"\n[51] Wrote {OUT_BASELINE.relative_to(ROOT)}")
print(f"[51] Wrote {OUT_INTERACT.relative_to(ROOT)}")
