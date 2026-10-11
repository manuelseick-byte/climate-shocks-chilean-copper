# -*- coding: utf-8 -*-
"""
DGA Water-Scarcity Decrees × ENSO — Reproduction Script
Regulatory-stress extension to the ENSO adaptation thesis (BSE Mining Module).

Run from this directory (after build_decree_panel.py):
    pip install pandas numpy scipy
    python reproduce_decrees.py

Question: do water-scarcity decrees — an independent, administrative signal of
water stress — track July-centered (JJA) ENSO in Chile's copper-mining regions,
and do they corroborate the physical DROUGHT stress channel in the master
ledger? ENSO/region/physical-stress inputs all derive from the same master
stress ledger used elsewhere in the thesis (single source of truth).

Mechanism: El Niño (ONI>0) -> wetter central Chile -> fewer decrees, so the
expected sign of the winter-ONI -> decree relationship is NEGATIVE, strongest in
the high-teleconnection FLOOD regions; flat in the permanently arid DROUGHT north.
All outputs are deterministic and should match the verification block exactly.
"""
import os
import numpy as np
import pandas as pd
from scipy import stats

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))
DATA = os.path.join(ROOT, "data", "derived")

# ── conventions (aligned with master ledger groupings) ───────────────────────
WY_MIN, WY_MAX  = 2008, 2025                 # decree coverage; WY = Apr Y..Mar Y+1
FLOOD_REGIONS   = [4, 5, 6, 13]              # Coquimbo, Valparaiso, O'Higgins, RM
DROUGHT_REGIONS = [1, 2, 3]                  # Tarapaca, Antofagasta, Atacama
MINING_REGIONS  = FLOOD_REGIONS + DROUGHT_REGIONS
EL_NINO_YEARS   = [2015, 2023]              # JJA ONI >= 0.5, within coverage
LA_NINA_YEARS   = [2010, 2022]              # JJA ONI <= -0.5, within coverage
NAME = {1:"Tarapaca",2:"Antofagasta",3:"Atacama",4:"Coquimbo",5:"Valparaiso",
        6:"O'Higgins",13:"Santiago Metropolitan"}

def detrend(y, x):
    return y - np.polyval(np.polyfit(x, y, 1), x)

def corr_block(x, y, years):
    pr = stats.pearsonr(x, y)
    sp = stats.spearmanr(x, y)
    prd = stats.pearsonr(detrend(np.asarray(x, float), years),
                         detrend(np.asarray(y, float), years))
    return pr.statistic, pr.pvalue, sp.statistic, prd.statistic, prd.pvalue

# ── load embedded inputs ─────────────────────────────────────────────────────
dec  = pd.read_csv(os.path.join(DATA, "decrees_region_year.csv"))
oni  = pd.read_csv(os.path.join(DATA, "oni_jja.csv"))
tele = pd.read_csv(os.path.join(DATA, "region_teleconnection.csv"))
phys = pd.read_csv(os.path.join(DATA, "region_drought_stress.csv"))

yrs = pd.DataFrame({"wy": range(WY_MIN, WY_MAX + 1)})
oni_wy = oni.rename(columns={"year": "wy"})[["wy", "jja_oni", "phase"]]

print("=" * 66)
print("DGA WATER-SCARCITY DECREES × ENSO — REPRODUCTION SCRIPT")
print("=" * 66)
print(f"  Decree rows: {len(dec)} region-years | ONI source: master stress ledger")
print(f"  Window: WY {WY_MIN}–{WY_MAX} (DGA water year, Apr–Mar)")

# ── SECTION D1 — National decree activity vs July-centered ENSO ──────────────
print("\n" + "─" * 66)
print("SECTION D1 — National decrees vs JJA ('July') ONI")
print("─" * 66)
nat = (dec.groupby("wy")["decrees"].sum().reindex(yrs.wy, fill_value=0)
       .rename("decrees").reset_index().merge(oni_wy, on="wy"))
mreg = (dec[dec.codregion.isin(MINING_REGIONS)].groupby("wy")["decrees"].sum()
        .reindex(yrs.wy, fill_value=0).rename("decrees").reset_index()
        .merge(oni_wy, on="wy"))
for label, d in [("All regions", nat), ("Mining regions only", mreg)]:
    r, p, sp, rd, pd_ = corr_block(d.jja_oni, d.decrees, d.wy.values)
    print(f"  {label:20s}: Pearson r={r:+.3f} (p={p:.3f}) | "
          f"Spearman={sp:+.3f} | detrended r={rd:+.3f} (p={pd_:.3f})")

# ── SECTION D2 — FLOOD vs DROUGHT mining regions ─────────────────────────────
print("\n" + "─" * 66)
print("SECTION D2 — FLOOD vs DROUGHT mining-region decree response")
print("─" * 66)
for label, codes in [("FLOOD regions  (4,5,6,13)", FLOOD_REGIONS),
                     ("DROUGHT regions (1,2,3)  ", DROUGHT_REGIONS)]:
    d = (dec[dec.codregion.isin(codes)].groupby("wy")["decrees"].sum()
         .reindex(yrs.wy, fill_value=0).rename("decrees").reset_index()
         .merge(oni_wy, on="wy"))
    r, p, sp, rd, pd_ = corr_block(d.jja_oni, d.decrees, d.wy.values)
    print(f"  {label}: total={int(d.decrees.sum()):4d} | "
          f"Pearson r={r:+.3f} (p={p:.3f}) | detrended r={rd:+.3f}")
print("  (FLOOD = El Niño-wet central regions; DROUGHT = permanently arid north)")

# ── SECTION D3 — Per mining-region table ─────────────────────────────────────
print("\n" + "─" * 66)
print("SECTION D3 — Per-region: winter teleconnection vs decree response")
print("─" * 66)
print(f"  {'region':22s} {'rho_w':>6s} {'tot':>4s} {'r_raw':>6s} {'r_det':>6s} {'spear':>6s}")
tele_m = tele.set_index("codregion")["rho_winter"].to_dict()
rows = []
for code in MINING_REGIONS:
    d = (dec[dec.codregion == code].groupby("wy")["decrees"].sum()
         .reindex(yrs.wy, fill_value=0).rename("decrees").reset_index()
         .merge(oni_wy, on="wy"))
    if d.decrees.sum() < 3 or d.decrees.std() == 0:
        rows.append((NAME[code], tele_m.get(code, np.nan), int(d.decrees.sum()),
                     np.nan, np.nan, np.nan)); continue
    r, p, sp, rd, pd_ = corr_block(d.jja_oni, d.decrees, d.wy.values)
    rows.append((NAME[code], tele_m.get(code, np.nan), int(d.decrees.sum()), r, rd, sp))
rows.sort(key=lambda z: (np.nan_to_num(z[1], nan=-9)), reverse=True)
for nm, rho, tot, r, rd, sp in rows:
    rs = f"{r:+.2f}" if not np.isnan(r) else "  -- "
    rds = f"{rd:+.2f}" if not np.isnan(rd) else "  -- "
    sps = f"{sp:+.2f}" if not np.isnan(sp) else "  -- "
    print(f"  {nm:22s} {rho:+6.2f} {tot:4d} {rs:>6s} {rds:>6s} {sps:>6s}")

# ── SECTION D4 — Regulatory vs physical-stress concordance ───────────────────
print("\n" + "─" * 66)
print("SECTION D4 — Decrees vs physical DROUGHT channel (ledger) & vs ONI")
print("─" * 66)
# align decrees (WY) to physical stress (calendar year) on year==wy
dm = (dec[dec.codregion.isin(MINING_REGIONS)]
      .merge(phys.rename(columns={"year": "wy"}), on=["codregion", "wy"], how="inner")
      .merge(oni_wy, on="wy"))
dm = dm[(dm.wy >= WY_MIN) & (dm.wy <= WY_MAX)]
r1 = stats.pearsonr(dm.drought_stress_rate, dm.decrees)
r2 = stats.pearsonr(dm.jja_oni, dm.drought_stress_rate)
print(f"  decrees   vs physical DROUGHT stress-rate : r={r1.statistic:+.3f} "
      f"(p={r1.pvalue:.3f}, n={len(dm)})")
print(f"  JJA ONI   vs physical DROUGHT stress-rate : r={r2.statistic:+.3f} "
      f"(p={r2.pvalue:.3f})")
print("  (D4 tests whether the administrative decree signal corroborates the")
print("   ledger's physical drought channel, and how strongly ENSO drives it.)")

# ── SECTION D5 — ENSO event study (FLOOD regions) ────────────────────────────
print("\n" + "─" * 66)
print("SECTION D5 — Event study: mean decrees per FLOOD region by ENSO phase")
print("─" * 66)
fl = dec[dec.codregion.isin(FLOOD_REGIONS)]
def phase_mean(years):
    sub = fl[fl.wy.isin(years)]
    return sub.decrees.sum() / (len(FLOOD_REGIONS) * len(years))
en = phase_mean(EL_NINO_YEARS); ln = phase_mean(LA_NINA_YEARS)
neutral_years = [y for y in range(WY_MIN, WY_MAX + 1)
                 if y not in EL_NINO_YEARS + LA_NINA_YEARS]
nu = phase_mean(neutral_years)
print(f"  El Niño years {EL_NINO_YEARS}: {en:.2f} decrees/region-year")
print(f"  La Niña years {LA_NINA_YEARS}: {ln:.2f} decrees/region-year")
print(f"  Neutral years (n={len(neutral_years)})      : {nu:.2f} decrees/region-year")
print(f"  La Niña / El Niño ratio: {ln/en:.2f}x" if en else "  n/a")

# ── VERIFICATION ─────────────────────────────────────────────────────────────
print("\n" + "=" * 66)
print("VERIFICATION (values should match exactly)")
print("=" * 66)
def chk(label, got, exp, tol=0.005):
    ok = abs(got - exp) <= tol
    print(f"  [{'OK ' if ok else 'XX '}] {label}: {got:+.3f} (expected {exp:+.3f})")

# expected values are filled in after first run (see README "Verification")
EXP = {
    "D1_all_detrended_r":   -0.223,
    "D2_flood_detrended_r": -0.213,
    "D2_drought_total":     13,
    "D4_decrees_vs_phys_r": +0.454,
    "D4_oni_vs_phys_r":     -0.456,
    "D5_ln_en_ratio":       +1.750,
}
r_all = corr_block(nat.jja_oni, nat.decrees, nat.wy.values)[3]
fl_d  = (dec[dec.codregion.isin(FLOOD_REGIONS)].groupby("wy")["decrees"].sum()
         .reindex(yrs.wy, fill_value=0).rename("decrees").reset_index().merge(oni_wy, on="wy"))
r_flood = corr_block(fl_d.jja_oni, fl_d.decrees, fl_d.wy.values)[3]
drought_total = int(dec[dec.codregion.isin(DROUGHT_REGIONS)
                        & dec.wy.between(WY_MIN, WY_MAX)].decrees.sum())
got = {
    "D1_all_detrended_r":   r_all,
    "D2_flood_detrended_r": r_flood,
    "D2_drought_total":     drought_total,
    "D4_decrees_vs_phys_r": r1.statistic,
    "D4_oni_vs_phys_r":     r2.statistic,
    "D5_ln_en_ratio":       (ln / en) if en else float("nan"),
}
if all(v is None for v in EXP.values()):
    print("  (No expected values locked yet — current run values:)")
    for k, v in got.items():
        print(f"     {k} = {v:+.3f}" if isinstance(v, float) else f"     {k} = {v}")
else:
    for k, exp in EXP.items():
        if exp is not None:
            chk(k, got[k], exp)

print("\nDone. Outputs are deterministic; re-running yields identical values.")
