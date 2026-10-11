"""
50_build_adapt_panel.py

Construct the mine x month adaptation variable Adapt_{i,t} for the BSE
ENSO/Copper project. Outputs the adaptation panel to panels/.

Variables produced:
  adapt_main   : headline binary. = 1 from the first month the mine's
                 dominant desalinated process is operational.
  adapt_strict : robustness. = 1 only for the 5 within-sample switchers
                 (Spence, QB2, Collahuasi, Pelambres, Mantoverde).
  adapt_share  : continuous in {0, 0.5, 1.0}, downweights partial cases.

Run from the share-package root:
    python code/50_build_adapt_panel.py

Sources for all coding decisions are documented in panels/coding_summary.csv.
"""

from pathlib import Path
import pandas as pd

# ---------------------------------------------------------------------------
# Locate the share-package root (the parent of `scripts/`).
# This script must be run from the share-package root for relative paths
# to resolve.
# ---------------------------------------------------------------------------
ROOT = Path(__file__).resolve().parent.parent
COCHILCO = ROOT / "data" / "inputs" / "cochilco" / "01a__cochilco_copper_mine_monthly.csv"
OUT_PANEL = ROOT / "data" / "panels" / "50__adapt_mine_month.parquet"
OUT_SUMMARY = ROOT / "data" / "panels" / "50__coding_summary.csv"
OUT_PANEL.parent.mkdir(exist_ok=True)

# ---------------------------------------------------------------------------
# 1. Mine roster from the Cochilco monthly production file (the climate
#    module already uses this; we use the same mine_operation keys for
#    a clean merge with panels/30__acute_mine_month.parquet).
# ---------------------------------------------------------------------------
master = pd.read_csv(COCHILCO)
master = master[master["mine_operation"].notna()].copy()
master = master[master["mine_operation"] != "Others"].copy()
master["date"] = pd.to_datetime(
    master["year"].astype(str) + "-" +
    master["month"].astype(str).str.zfill(2) + "-01"
)

mines = sorted(master["mine_operation"].unique())
print(f"[50] Mines in panel: {len(mines)}")

# ---------------------------------------------------------------------------
# 2. Adaptation coding by Masterfile mine_operation
#    Sources: Mining_Firms_Comprehensive.docx (constructed from CMF Memorias
#    and operator ARs) + Cochilco public studies + ACADES desalination registry.
# ---------------------------------------------------------------------------

# Headline coding: month from which adapt_main = 1.
# Dates reconciled to the central mines_desal.csv registry (2026-06-02 audit).
adapt_main_start = {
    # ALWAYS-ADAPTED / no clean in-sample switch (registry dates):
    "Escondida":                "2018-01-01",   # EWS operational transition 2018 (100% by 2020); 2006/2020 as robustness
    "Esperanza":                "2010-01-01",   # direct seawater via Centinela district since 2010
    "El Tesoro":                "2010-01-01",   # direct seawater via Centinela district since 2010
    "Candelaria":               "2013-01-01",   # own desal plant commissioned 2013 (Lundin)
    # WITHIN-SAMPLE SWITCHERS (registry):
    "Spence":                   "2021-01-01",   # SGO 2021
    "Quebrada Blanca":          "2023-04-01",   # QB2 ramp 2023
    "Collahuasi":               "2023-01-01",   # Patache phased from 2023
    "Los Pelambres":            "2024-03-01",   # INCO 2024
    # EXCLUDED: registered cell (Mantos Blancos) has no desal; Mantoverde is a
    # separate COCHILCO unit. Coded non-adapted per registry.
    "Anglo American Norte":     None,
    # NEVER ADAPTED in sample (no plant operational):
    "Codelco - Gaby":           None,
    "Codelco - Salvador":       None,
    "Codelco - Andina":         None,
    "Codelco - Teniente":       None,
    "Codelco - Chuqui, RT y MH":None,
    "El Abra":                  None,
    "Zaldivar":                 None,
    "Lomas Bayas":              None,
    "Anglo American Sur":       None,
    "Cerro Colorado":           None,
}

adapt_strict_start = {
    # true switcher-only set (registry); Anglo American Norte removed
    "Spence":                   "2021-01-01",
    "Quebrada Blanca":          "2023-04-01",
    "Collahuasi":               "2023-01-01",
    "Los Pelambres":            "2024-03-01",
}

# adapt_share — continuous bracket. EMPAE microdata is confidential, so the
# share is bracketed using AR disclosures of dominant process water source.
adapt_share_value = {
    "Escondida":                ("2018-01-01", 1.0),
    "Esperanza":                ("2010-01-01", 1.0),
    "El Tesoro":                ("2010-01-01", 1.0),
    "Candelaria":               ("2013-01-01", 1.0),
    "Spence":                   ("2021-01-01", 1.0),
    "Quebrada Blanca":          ("2023-04-01", 1.0),
    "Collahuasi":               ("2023-01-01", 0.5),
    "Los Pelambres":            ("2024-03-01", 0.5),
}


# ---------------------------------------------------------------------------
# 3. Build the panel
# ---------------------------------------------------------------------------
panel = master[["date", "year", "month", "mine_operation"]].copy()


def _post(row, table):
    start = table.get(row["mine_operation"])
    if start is None:
        return 0
    return 1 if row["date"] >= pd.Timestamp(start) else 0


def _share(row):
    info = adapt_share_value.get(row["mine_operation"])
    if info is None:
        return 0.0
    start, val = info
    return val if row["date"] >= pd.Timestamp(start) else 0.0


panel["adapt_main"]   = panel.apply(lambda r: _post(r, adapt_main_start), axis=1)
panel["adapt_strict"] = panel.apply(lambda r: _post(r, adapt_strict_start), axis=1)
panel["adapt_share"]  = panel.apply(_share, axis=1)
panel = panel.sort_values(["mine_operation", "date"]).reset_index(drop=True)

# ---------------------------------------------------------------------------
# 4. Coding summary (one row per mine, with source)
# ---------------------------------------------------------------------------
summary_rows = [
    ("Escondida",                "2018-01", "Always-adapted (registry: EWS 2018, 100% by 2020)",
     "EWS operational transition 2018 (2,500 l/s); 100% desal by 2020. Registry headline 2018; 2006/2020 robustness"),
    ("Esperanza",                "2010-01", "Always-adapted (direct seawater since 2010)",
     "Direct seawater via Centinela district since 2010 (registry)"),
    ("El Tesoro",                "2010-01", "Always-adapted (direct seawater since 2010)",
     "Direct seawater via Centinela district since 2010 (registry)"),
    ("Candelaria",               "2013-01", "Always-adapted (own desal 2013, Lundin)",
     "Own desal plant commissioned 2013 (Lundin Mining); MVDP expansion 2024 (registry)"),
    ("Spence",                   "2021-01", "Within-sample switcher (SGO start)",
     "SGO concentrator H1 FY2021 (Jul-Dec 2020); 1,000 l/s desal at Bahia de Mejillones"),
    ("Quebrada Blanca",          "2023-04", "Within-sample switcher (QB2 ramp)",
     "QB2 first concentrate Q1 2023; dedicated desal plant - Teck QB CMF Memoria 2023"),
    ("Collahuasi",               "2023-01", "Within-sample switcher (Patache phased from 2023)",
     "Patache seawater desal + pipeline, phased commissioning from 2023 (registry)"),
    ("Los Pelambres",            "2024-03", "Within-sample switcher (INCO Phase 1)",
     "INCO desal Phase 1 operational March 2024 (400 l/s)"),
    ("Anglo American Norte",     None,      "Excluded (no desal in registered cell)",
     "Registered cell = Mantos Blancos (no desal); Mantoverde is a separate COCHILCO unit (registry)"),
    ("Codelco - Gaby",           None,      "Never adapted in sample (planned 2027)",
     "Tocopilla Northern District desal tendered 2024; targeted 2027"),
    ("Codelco - Chuqui, RT y MH",None,      "Never adapted in sample (planned 2027)",
     "Tocopilla Northern District desal tendered 2024; targeted 2027"),
    ("Codelco - Salvador",       None,      "Never adapted (no transition)",
     "Continental aquifer + Andean run-off; no desal disclosed"),
    ("Codelco - Andina",         None,      "Never adapted (no transition)",
     "Maipo basin; no desal disclosed"),
    ("Codelco - Teniente",       None,      "Never adapted (no transition)",
     "Maipo/Cachapoal basin; no desal disclosed"),
    ("El Abra",                  None,      "Never adapted (under study)",
     "Salar de Ascotán aquifer; alternative water under study"),
    ("Zaldivar",                 None,      "Never adapted (no transition)",
     "Salar de Punta Negra / Negrillar continental wells"),
    ("Lomas Bayas",              None,      "Never adapted in sample (2023 RCA only)",
     "RCA approval for non-continental waters 2023; no plant commissioned"),
    ("Anglo American Sur",       None,      "Never adapted in sample (Aguas Pacífico 2025-26)",
     "Aguas Pacífico desal supply from 2025-26; right-edge of sample"),
    ("Cerro Colorado",           None,      "Closure (Dec 2023)",
     "Water-rights restriction; closed Dec 2023"),
]
summary_df = pd.DataFrame(summary_rows,
                          columns=["mine_operation", "adapt_start",
                                   "status", "source_note"])

# ---------------------------------------------------------------------------
# 5. Write
# ---------------------------------------------------------------------------
panel.to_parquet(OUT_PANEL, index=False)
summary_df.to_csv(OUT_SUMMARY, index=False)

print(f"[50] Wrote {OUT_PANEL.relative_to(ROOT)} ({len(panel):,} rows)")
print(f"[50] Wrote {OUT_SUMMARY.relative_to(ROOT)} ({len(summary_df)} mines)")

# Diagnostic
print("\n[50] Diagnostic — mine-months with adapt_main = 1 by mine (sample 2003-2025):")
diag = panel[panel["year"].between(2003, 2025)].groupby("mine_operation").agg(
    n_obs=("adapt_main", "size"),
    n_adapt=("adapt_main", "sum"),
)
diag = diag[diag["n_adapt"] > 0].sort_values("n_adapt", ascending=False)
for mine, row in diag.iterrows():
    print(f"       {mine:30s} {int(row['n_adapt']):4d} / {int(row['n_obs']):4d}")
