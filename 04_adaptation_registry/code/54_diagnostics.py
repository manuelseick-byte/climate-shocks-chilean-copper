"""
54_diagnostics.py

Diagnostic checks for the headline interaction. Tests whether b2 is being
identified off a small treatment cell or driven by individual mines.

  - Treatment cell size: how many adapted-AND-stressed mine-months exist?
  - Leave-one-out: how much does b2 move when each adapted mine is dropped?
  - Pre-sample vs within-sample split: do the always-adapted (pre-2018)
    and the switchers (within-sample) give the same answer?

Output:
    results/54__diagnostics.txt   — narrative summary of the three checks

Run from the share-package root:
    python code/54_diagnostics.py
"""

from pathlib import Path
import warnings
import io
import pandas as pd
from pyfixest.estimation import feols

warnings.filterwarnings("ignore", category=UserWarning)

ROOT = Path(__file__).resolve().parent.parent
ACUTE = ROOT / "data" / "panels" / "30__acute_mine_month.parquet"
ADAPT = ROOT / "data" / "panels" / "50__adapt_mine_month.parquet"
OUT_TXT = ROOT / "results" / "54__diagnostics.txt"
OUT_TXT.parent.mkdir(exist_ok=True)

# ---------------------------------------------------------------------------
out = io.StringIO()


def log(msg=""):
    print(msg)
    out.write(msg + "\n")


df = pd.read_parquet(ACUTE)
adapt = pd.read_parquet(ADAPT)[["mine_operation", "year", "month",
                                "adapt_main", "adapt_strict"]]
df = df.merge(adapt, on=["mine_operation", "year", "month"], how="left")
df = df[df["in_sample"]].copy()
df["calmonth"] = df["calmonth"].astype(str)

PRE_SAMPLE = ["Escondida", "Esperanza", "El Tesoro", "Candelaria"]
SWITCHERS = ["Spence", "Quebrada Blanca", "Collahuasi", "Los Pelambres",
             "Anglo American Norte"]

# ---------------------------------------------------------------------------
log("=" * 80)
log(" [54] Diagnostic 1 — treatment cell size")
log("=" * 80)
log("How many mine-months in the sample have both stress = 1 AND adapt_main = 1?")
log("If this number is small, b2 will be identified off a thin treatment cell.")
log("")
for chan in ["stress_flood_mine", "stress_flood_regmean_M", "stress_heat_mine"]:
    cell = df[(df[chan] == 1) & (df["adapt_main"] == 1)]
    log(f"  {chan:25s}: {len(cell):3d} stressed-AND-adapted mine-months "
        f"across {cell['mine_operation'].nunique()} mines")
    if len(cell) > 0:
        for m, n in cell.groupby("mine_operation").size().sort_values(
                ascending=False).items():
            log(f"      {m:30s}  {n}")
log("")

# ---------------------------------------------------------------------------
log("=" * 80)
log(" [54] Diagnostic 2 — leave-one-out for adapted mines")
log("=" * 80)
log("How much does b2 (the interaction) move when each adapted mine is dropped?")
log("If one mine drives the headline result, b2 will swing dramatically.")
log("")
adapted_mines = sorted(df[df["adapt_main"] == 1]["mine_operation"].unique())

for chan in ["stress_flood_regmean_M", "stress_heat_mine"]:
    log(f"--- channel: {chan} ---")
    df[f"{chan}_x_a"] = df[chan] * df["adapt_main"]
    f = (f"log_kt ~ {chan} + {chan}_x_a + adapt_main + log_price | "
         f"mine_operation + calmonth + year")
    m_full = feols(f, data=df, vcov={"CRV1": "codregion"})
    b2_full = m_full.coef()[f"{chan}_x_a"]
    p2_full = m_full.pvalue()[f"{chan}_x_a"]
    log(f"  FULL SAMPLE        : b2 = {b2_full:+.4f}  p = {p2_full:.3f}")
    for mine in adapted_mines:
        d2 = df[df["mine_operation"] != mine].copy()
        try:
            m = feols(f, data=d2, vcov={"CRV1": "codregion"})
            b2 = m.coef().get(f"{chan}_x_a", float("nan"))
            p2 = m.pvalue().get(f"{chan}_x_a", float("nan"))
            log(f"  drop {mine:25s}: b2 = {b2:+.4f}  p = {p2:.3f}")
        except Exception as e:
            log(f"  drop {mine:25s}: ERROR — {e}")
    log("")

# ---------------------------------------------------------------------------
log("=" * 80)
log(" [54] Diagnostic 3 — pre-sample adapted vs within-sample switchers")
log("=" * 80)
log("Split adapt into two interactions:")
log(f"  adapt_presample : {PRE_SAMPLE}")
log(f"  adapt_switcher  : {SWITCHERS}")
log("If both give negative b2, the result isn't only about ramp-ups.")
log("")
df["adapt_presample"] = ((df["adapt_main"] == 1) &
                          (df["mine_operation"].isin(PRE_SAMPLE))).astype(int)
df["adapt_switcher"]  = ((df["adapt_main"] == 1) &
                          (df["mine_operation"].isin(SWITCHERS))).astype(int)

for chan in ["stress_flood_regmean_M", "stress_heat_mine"]:
    df[f"{chan}_x_pre"] = df[chan] * df["adapt_presample"]
    df[f"{chan}_x_swt"] = df[chan] * df["adapt_switcher"]
    f = (f"log_kt ~ {chan} + {chan}_x_pre + {chan}_x_swt + "
         f"adapt_presample + adapt_switcher + log_price | "
         f"mine_operation + calmonth + year")
    m = feols(f, data=df, vcov={"CRV1": "codregion"})
    log(f"--- channel: {chan} ---")
    log(f"  Stress (b1)               : {m.coef()[chan]:+.4f}  p={m.pvalue()[chan]:.3f}")
    log(f"  Stress x adapt_presample  : {m.coef()[f'{chan}_x_pre']:+.4f}  "
        f"p={m.pvalue()[f'{chan}_x_pre']:.3f}")
    log(f"  Stress x adapt_switcher   : {m.coef()[f'{chan}_x_swt']:+.4f}  "
        f"p={m.pvalue()[f'{chan}_x_swt']:.3f}")
    log("")

with open(OUT_TXT, "w") as f:
    f.write(out.getvalue())
log(f"[54] Wrote {OUT_TXT.relative_to(ROOT)}")
