# -*- coding: utf-8 -*-
"""
BUILD step (analogous to script 20 in the main thesis codebase).

Reads the raw DGA water-scarcity decree registry and the master stress ledger,
and writes the embedded, self-contained inputs that reproduce_decrees.py reads.
Run once; deterministic.

    python build_decree_panel.py

Outputs (data/):
    decrees_region_year.csv     region x water-year decree counts (derived)
    oni_jja.csv                 JJA ONI by year  (snapshot from ledger oni_value)
    region_teleconnection.csv   winter ONI-precip rho by region (from ledger)
    region_drought_stress.csv   region x year physical DROUGHT stress-flag rate
"""
import os, re, unicodedata
import numpy as np
import pandas as pd
import xlrd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))
RAW  = os.path.join(ROOT, "data", "raw", "Decretos_zonas_escasez_historico_10-04-2026.xls")
DATA = os.path.join(ROOT, "data", "derived")

# Rebuilding the derived files requires the master stress ledger from the
# ENSO climate-exposure module. Provide its path explicitly:
#   LEDGER_PATH=/path/to/20__master_stress_ledger.csv python code/01_build_decree_panel.py
LEDGER = os.environ.get("LEDGER_PATH")
if not LEDGER:
    raise RuntimeError(
        "LEDGER_PATH is required to rebuild the derived data. "
        "The bundled data/derived files can be used directly for reproduction."
    )

# ── decree parsing ────────────────────────────────────────────────────────────
def strip_accents(s):
    return "".join(c for c in unicodedata.normalize("NFD", str(s))
                   if unicodedata.category(c) != "Mn")

MONTHS = {"enero":1,"febrero":2,"marzo":3,"abril":4,"mayo":5,"junio":6,"julio":7,
          "agosto":8,"septiembre":9,"setiembre":9,"octubre":10,"noviembre":11,
          "diciembre":12,"ene":1,"feb":2,"mar":3,"abr":4,"may":5,"jun":6,"jul":7,
          "ago":8,"sept":9,"set":9,"oct":10,"nov":11,"dic":12}
MONTH_RE = re.compile(r"\b(" + "|".join(sorted(MONTHS, key=len, reverse=True)) + r")\b")

# region name token -> codregion (substring match on accent-stripped lower-case)
REGION_KEYS = [("arica",15),("parinacota",15),("tarapaca",1),("antofagasta",2),
    ("atacama",3),("coquimbo",4),("valparaiso",5),("higgins",6),("maule",7),
    ("nuble",16),("biobio",8),("araucania",9),("lagos",10),("rios",14),
    ("aysen",11),("magallanes",12),("metropolitan",13)]
CODE_NAME = {1:"Tarapaca",2:"Antofagasta",3:"Atacama",4:"Coquimbo",5:"Valparaiso",
    6:"O'Higgins",7:"Maule",8:"Biobio",9:"La Araucania",10:"Los Lagos",11:"Aysen",
    12:"Magallanes",13:"Santiago Metropolitan",14:"Los Rios",
    15:"Arica y Parinacota",16:"Nuble"}

def regions_from_text(cell):
    if not cell or not str(cell).strip():
        return set()
    norm = strip_accents(cell).lower()
    found = set()
    for tok in re.split(r",|\sy\s|\sand\s|/", norm):
        for key, code in REGION_KEYS:
            if key in tok:
                found.add(code)
    return found

def parse_month_year(decree_cell):
    norm = strip_accents(decree_cell).lower()
    yrs = re.findall(r"\b(20\d{2})\b", norm)
    year = int(yrs[-1]) if yrs else None
    m = MONTH_RE.search(norm)
    return (MONTHS[m.group(1)] if m else None, year)

def parse_decrees():
    b = xlrd.open_workbook(RAW)
    s = b.sheet_by_index(0)
    recs, unknown = [], 0
    for r in range(s.nrows):
        row = [s.cell_value(r, c) for c in range(5)]
        region, decree, desc = row[1], row[2], row[4]
        if str(region).strip() == "Región" and str(decree).strip() == "Decreto":
            continue
        if not str(decree).strip():
            continue
        month, dyear = parse_month_year(str(decree))
        if dyear is None:
            continue
        regs = regions_from_text(region) or regions_from_text(desc)
        if not regs:
            unknown += 1
            continue
        # DGA water year: Apr Y .. Mar Y+1 -> recharge from winter (JJA) of year Y
        wy = dyear if (month or 6) >= 4 else dyear - 1
        for code in regs:
            recs.append({"codregion": code, "wy": wy})
    df = pd.DataFrame(recs)
    print(f"[build] parsed {len(df)} region-decree rows, {unknown} unresolved-region decrees dropped")
    return df

# ── build ─────────────────────────────────────────────────────────────────────
def main():
    os.makedirs(DATA, exist_ok=True)
    long = parse_decrees()
    panel = (long.groupby(["codregion", "wy"]).size().rename("decrees").reset_index())
    panel["region_en"] = panel["codregion"].map(CODE_NAME)
    panel.to_csv(os.path.join(DATA, "decrees_region_year.csv"), index=False)
    print(f"[build] decrees_region_year.csv: {len(panel)} rows")

    led = pd.read_csv(LEDGER)
    # JJA ONI snapshot (single source = ledger oni_value)
    jja = (led[led.month.isin([6, 7, 8])].groupby("year")["oni_value"].mean()
           .rename("jja_oni").reset_index())
    jja["phase"] = np.where(jja.jja_oni >= 0.5, "nino",
                     np.where(jja.jja_oni <= -0.5, "nina", "neutral"))
    jja.to_csv(os.path.join(DATA, "oni_jja.csv"), index=False)
    print(f"[build] oni_jja.csv: {jja.year.min()}–{jja.year.max()}")

    # winter ONI-precip teleconnection per region (ledger rho_region_month)
    tele = (led[led.month.isin([5, 6, 7, 8])].groupby(["codregion", "region_en"])
            ["rho_region_month"].mean().rename("rho_winter").reset_index())
    tele.to_csv(os.path.join(DATA, "region_teleconnection.csv"), index=False)
    print(f"[build] region_teleconnection.csv: {len(tele)} regions")

    # physical DROUGHT stress-flag rate per region x year (cross-check channel)
    dr = led[led.channel.isin(["DROUGHT_hydro", "DROUGHT_meteo"])]
    drr = (dr.groupby(["codregion", "year"])["stress_flag"].mean()
           .rename("drought_stress_rate").reset_index())
    drr.to_csv(os.path.join(DATA, "region_drought_stress.csv"), index=False)
    print(f"[build] region_drought_stress.csv: {len(drr)} region-years")
    print("[build] done.")

if __name__ == "__main__":
    main()
