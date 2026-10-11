# -*- coding: utf-8 -*-
"""
Generate report figures from the embedded data/ (deterministic).
    python make_figures.py   ->  figures/*.png
"""
import os
import numpy as np
import pandas as pd
from scipy import stats
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))
DATA = os.path.join(ROOT, "data", "derived")
FIG  = os.path.join(ROOT, "results", "figures")
os.makedirs(FIG, exist_ok=True)

WY_MIN, WY_MAX  = 2008, 2025
FLOOD_REGIONS   = [4, 5, 6, 13]
DROUGHT_REGIONS = [1, 2, 3]
MINING_REGIONS  = FLOOD_REGIONS + DROUGHT_REGIONS
EL_NINO, LA_NINA = [2015, 2023], [2010, 2022]

dec  = pd.read_csv(os.path.join(DATA, "decrees_region_year.csv"))
oni  = pd.read_csv(os.path.join(DATA, "oni_jja.csv")).rename(columns={"year": "wy"})
phys = pd.read_csv(os.path.join(DATA, "region_drought_stress.csv"))
yrs  = pd.Index(range(WY_MIN, WY_MAX + 1), name="wy")
oni_wy = oni[["wy", "jja_oni"]]

C_DEC, C_ONI = "#c0504d", "#1f4e79"

# ── Fig 1: national decrees vs JJA ONI (ONI inverted so La Niña points up) ────
nat = dec.groupby("wy")["decrees"].sum().reindex(yrs, fill_value=0).reset_index().merge(oni_wy, on="wy")
fig, ax1 = plt.subplots(figsize=(9, 4.6))
ax1.bar(nat.wy, nat.decrees, color=C_DEC, alpha=.8, label="Decrees (national)")
ax1.set_ylabel("Water-scarcity decrees", color=C_DEC)
ax1.set_xlabel("DGA water year (Apr–Mar)")
ax1.tick_params(axis="y", labelcolor=C_DEC)
ax2 = ax1.twinx()
ax2.plot(nat.wy, nat.jja_oni, "o-", color=C_ONI, lw=2, label="JJA ONI")
ax2.axhline(0.5, ls=":", c="grey"); ax2.axhline(-0.5, ls=":", c="grey")
ax2.set_ylabel("JJA ONI (°C) — El Niño ↑ / La Niña ↓", color=C_ONI)
ax2.tick_params(axis="y", labelcolor=C_ONI)
ax2.invert_yaxis()
ax1.set_title("Fig 1 — Chile water-scarcity decrees vs July-centered ENSO (ONI axis inverted)")
fig.tight_layout(); fig.savefig(os.path.join(FIG, "fig1_decrees_vs_oni.png"), dpi=140); plt.close(fig)

# ── Fig 2: D4 concordance scatters ───────────────────────────────────────────
dm = (dec[dec.codregion.isin(MINING_REGIONS)]
      .merge(phys.rename(columns={"year": "wy"}), on=["codregion", "wy"])
      .merge(oni_wy, on="wy"))
dm = dm[(dm.wy >= WY_MIN) & (dm.wy <= WY_MAX)]
fig, axs = plt.subplots(1, 2, figsize=(10, 4.4))
for ax, (xc, yc, xl, yl, col) in zip(axs, [
        ("jja_oni", "drought_stress_rate", "JJA ONI (°C)",
         "Physical DROUGHT stress-flag rate", C_ONI),
        ("drought_stress_rate", "decrees", "Physical DROUGHT stress-flag rate",
         "Decrees (region-year)", C_DEC)]):
    ax.scatter(dm[xc], dm[yc], s=28, color=col, alpha=.7, edgecolor="white")
    b = np.polyfit(dm[xc], dm[yc], 1); xs = np.linspace(dm[xc].min(), dm[xc].max(), 50)
    ax.plot(xs, np.polyval(b, xs), "--", color="black", lw=1.2)
    r = stats.pearsonr(dm[xc], dm[yc])
    ax.set_xlabel(xl); ax.set_ylabel(yl)
    ax.set_title(f"r = {r.statistic:+.2f} (p = {r.pvalue:.3f}, n = {len(dm)})", fontsize=10)
fig.suptitle("Fig 2 — ENSO → physical drought → regulatory decrees (mining region-years)")
fig.tight_layout(); fig.savefig(os.path.join(FIG, "fig2_concordance.png"), dpi=140); plt.close(fig)

# ── Fig 3: event study by ENSO phase (FLOOD regions) ─────────────────────────
fl = dec[dec.codregion.isin(FLOOD_REGIONS)]
def pm(years): return fl[fl.wy.isin(years)].decrees.sum() / (len(FLOOD_REGIONS) * len(years))
neutral = [y for y in range(WY_MIN, WY_MAX + 1) if y not in EL_NINO + LA_NINA]
vals = [pm(EL_NINO), pm(neutral), pm(LA_NINA)]
labs = [f"El Niño\n{EL_NINO}", f"Neutral\n(n={len(neutral)})", f"La Niña\n{LA_NINA}"]
fig, ax = plt.subplots(figsize=(6, 4.4))
ax.bar(labs, vals, color=["#4472c4", "#a6a6a6", "#c0504d"])
for i, v in enumerate(vals): ax.text(i, v + .03, f"{v:.2f}", ha="center", fontweight="bold")
ax.set_ylabel("Mean decrees per FLOOD region-year")
ax.set_title("Fig 3 — FLOOD-region decree intensity by ENSO phase")
fig.tight_layout(); fig.savefig(os.path.join(FIG, "fig3_event_study.png"), dpi=140); plt.close(fig)

print("wrote:", ", ".join(sorted(os.listdir(FIG))))
