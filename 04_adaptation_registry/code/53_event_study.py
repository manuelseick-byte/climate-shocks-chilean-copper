"""
53_event_study.py

Event study around desalination commissioning — the cleaner identification
strategy than the panel interaction. Two designs are run:

  A. Descriptive: pre vs post commissioning means by mine and channel.
  B. Pooled DiD across the 5 within-sample switchers, with a tight window
     (-36 to +60 months around each mine's commissioning date).
  C. Single-mine event studies for Spence (60mo post) and Quebrada Blanca
     (33mo post) — the only switchers with enough post-window today.

Commissioning dates:
    Spence              2021-01  (SGO concentrator H1 FY2021)
    Quebrada Blanca     2023-04  (QB2 first concentrate)
    Collahuasi          2024-01  (Patache phased)
    Los Pelambres       2024-03  (INCO Phase 1)
    Anglo American Norte 2024-01 (Mantoverde MVDP)

Outputs:
    results/53__event_study_descriptive.csv
    results/53__event_study_pooled_did.csv
    results/53__event_study_per_mine.csv

Run from the share-package root:
    python code/53_event_study.py
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

OUT_DESC   = ROOT / "results" / "53__event_study_descriptive.csv"
OUT_POOL   = ROOT / "results" / "53__event_study_pooled_did.csv"
OUT_PER    = ROOT / "results" / "53__event_study_per_mine.csv"
OUT_DESC.parent.mkdir(exist_ok=True)

COMM = {
    "Spence":                "2021-01-01",
    "Quebrada Blanca":       "2023-04-01",
    "Collahuasi":            "2024-01-01",
    "Los Pelambres":         "2024-03-01",
    "Anglo American Norte":  "2024-01-01",
}

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------
df = pd.read_parquet(ACUTE)
adapt = pd.read_parquet(ADAPT)[["mine_operation", "year", "month", "adapt_main"]]
df = df.merge(adapt, on=["mine_operation", "year", "month"], how="left")
df = df[df["in_sample"]].copy()
df["date"] = pd.to_datetime(df["date"])
df["calmonth"] = df["calmonth"].astype(str)

# ---------------------------------------------------------------------------
# A. Descriptive
# ---------------------------------------------------------------------------
print("[53] A) Descriptive — pre vs post commissioning")
desc_rows = []
for mine, comm_date in COMM.items():
    cd = pd.Timestamp(comm_date)
    sub = df[df["mine_operation"] == mine].copy()
    sub["post"] = (sub["date"] >= cd).astype(int)
    pre, post = sub[sub["post"] == 0], sub[sub["post"] == 1]
    print(f"\n  --- {mine} ({comm_date}) ---")
    print(f"      PRE:  N={len(pre):3d}  log_kt mean={pre['log_kt'].mean():+.3f}")
    print(f"      POST: N={len(post):3d}  log_kt mean={post['log_kt'].mean():+.3f}")
    for chan in ["stress_flood_mine", "stress_flood_regmean_M", "stress_heat_mine"]:
        n_pre, n_post = int(pre[chan].sum()), int(post[chan].sum())
        m_pre = pre.loc[pre[chan] == 1, "log_kt"].mean() if n_pre > 0 else np.nan
        m_post = post.loc[post[chan] == 1, "log_kt"].mean() if n_post > 0 else np.nan
        print(f"        {chan:25s} stressed: pre={n_pre:2d} (mean={m_pre:+.2f})  "
              f"post={n_post:2d} (mean={m_post:+.2f})")
        desc_rows.append({"mine": mine, "channel": chan,
                          "n_pre": n_pre, "n_post": n_post,
                          "mean_log_kt_stressed_pre": m_pre,
                          "mean_log_kt_stressed_post": m_post})

pd.DataFrame(desc_rows).to_csv(OUT_DESC, index=False)
print(f"\n[53] Wrote {OUT_DESC.relative_to(ROOT)}")

# ---------------------------------------------------------------------------
# B. Pooled DiD
# ---------------------------------------------------------------------------
print("\n[53] B) Pooled DiD — 5 switchers, [-36, +60] month window")
sw = df[df["mine_operation"].isin(COMM.keys())].copy()
sw["comm_date"] = sw["mine_operation"].map({k: pd.Timestamp(v) for k, v in COMM.items()})
sw["post"] = (sw["date"] >= sw["comm_date"]).astype(int)
sw["months_since_comm"] = ((sw["date"] - sw["comm_date"]) /
                           pd.Timedelta(days=30.44)).round().astype(int)
sw = sw[(sw["months_since_comm"] >= -36) & (sw["months_since_comm"] <= 60)].copy()
print(f"      Switcher panel: {len(sw):,} mine-months "
      f"(pre={(sw['post']==0).sum()}, post={(sw['post']==1).sum()})")

pool_rows = []
for chan in ["stress_flood_mine", "stress_flood_regmean_M", "stress_heat_mine"]:
    sw[f"{chan}_x_post"] = sw[chan] * sw["post"]
    try:
        m = feols(
            f"log_kt ~ {chan} + {chan}_x_post + post + log_price | "
            f"mine_operation + calmonth + year",
            data=sw, vcov={"CRV1": "codregion"},
        )
        b1 = m.coef().get(chan, np.nan)
        b2 = m.coef().get(f"{chan}_x_post", np.nan)
        b3 = m.coef().get("post", np.nan)
        p1 = m.pvalue().get(chan, np.nan)
        p2 = m.pvalue().get(f"{chan}_x_post", np.nan)
        p3 = m.pvalue().get("post", np.nan)
        n_pre = int(((sw[chan] == 1) & (sw["post"] == 0)).sum())
        n_post = int(((sw[chan] == 1) & (sw["post"] == 1)).sum())
        print(f"\n      --- {chan} (n_stress: pre={n_pre}, post={n_post}) ---")
        print(f"          b1 (pre stress):    {b1:+.4f} (p={p1:.3f})")
        print(f"          b2 (stress x post): {b2:+.4f} (p={p2:.3f})")
        print(f"          marg (post):        {b1+b2:+.4f}")
        pool_rows.append({"channel": chan,
                          "n_stress_pre": n_pre, "n_stress_post": n_post,
                          "b1_pre_stress": b1, "p1": p1,
                          "b2_stress_x_post": b2, "p2": p2,
                          "b3_post": b3, "p3": p3,
                          "marg_post": b1+b2, "N": int(m._N)})
    except Exception as e:
        print(f"      {chan} failed: {e}")

pd.DataFrame(pool_rows).to_csv(OUT_POOL, index=False)
print(f"\n[53] Wrote {OUT_POOL.relative_to(ROOT)}")

# ---------------------------------------------------------------------------
# C. Single-mine event studies (Spence + QB2)
# ---------------------------------------------------------------------------
print("\n[53] C) Single-mine event studies — Spence and Quebrada Blanca")
per_rows = []
for mine in ["Spence", "Quebrada Blanca"]:
    sub = df[df["mine_operation"] == mine].copy()
    sub["post"] = (sub["date"] >= pd.Timestamp(COMM[mine])).astype(int)
    print(f"\n      === {mine} (comm. {COMM[mine]}) ===")
    print(f"      Sample: {len(sub)} months  pre={int((sub['post']==0).sum())}  "
          f"post={int((sub['post']==1).sum())}")
    for chan in ["stress_flood_mine", "stress_flood_regmean_M", "stress_heat_mine"]:
        n_pre = int(((sub[chan] == 1) & (sub["post"] == 0)).sum())
        n_post = int(((sub[chan] == 1) & (sub["post"] == 1)).sum())
        if n_pre == 0 or n_post == 0:
            print(f"        {chan:25s} skip (pre={n_pre}, post={n_post})")
            continue
        sub[f"{chan}_x_post"] = sub[chan] * sub["post"]
        try:
            m = feols(
                f"log_kt ~ {chan} + {chan}_x_post + post + log_price | "
                f"year + calmonth",
                data=sub, vcov="HC1",
            )
            b1 = m.coef().get(chan, np.nan)
            b2 = m.coef().get(f"{chan}_x_post", np.nan)
            p1 = m.pvalue().get(chan, np.nan)
            p2 = m.pvalue().get(f"{chan}_x_post", np.nan)
            print(f"        {chan:25s} pre={n_pre} post={n_post}  "
                  f"b1={b1:+.3f}(p={p1:.2f})  b2={b2:+.3f}(p={p2:.2f})  "
                  f"marg_post={b1+b2:+.3f}")
            per_rows.append({"mine": mine, "channel": chan,
                             "n_stress_pre": n_pre, "n_stress_post": n_post,
                             "b1_pre_stress": b1, "p1": p1,
                             "b2_stress_x_post": b2, "p2": p2,
                             "marg_post": b1+b2})
        except Exception as e:
            print(f"        {chan} failed: {e}")

pd.DataFrame(per_rows).to_csv(OUT_PER, index=False)
print(f"\n[53] Wrote {OUT_PER.relative_to(ROOT)}")
