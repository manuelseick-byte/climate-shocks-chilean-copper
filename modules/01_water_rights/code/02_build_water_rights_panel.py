# Repository note: filesystem paths were normalised for public documentation; analytical logic is unchanged.
from pathlib import Path
"""Build panel datasets from the translated DGA water-rights files.

Outputs (all under English/):
  Water_Rights_All_Regions.xlsx     - combined wide table, every right, all regions
  Water_Rights_Monthly_Panel.xlsx   - long format: one row per (right x month)
  Region_Use_Summary.xlsx           - aggregated by region x decade x water use
"""

import glob
import os
import re

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = PROJECT_ROOT / "data" / "processed" / "dga_translated"
MONTHS = [
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December",
]

# Conversion of any flow unit into L/s where possible. Units without a clean
# volumetric conversion (Shares, Parts, Blocks, %, Regadores) are left null.
TO_LPS = {
    "L/s": 1.0,
    "L/min": 1.0 / 60.0,
    "L/h": 1.0 / 3600.0,
    "L/day": 1.0 / 86400.0,
    "m3/s": 1000.0,
    "m3/min": 1000.0 / 60.0,
    "m3/h": 1000.0 / 3600.0,
    "m3/day": 1000.0 / 86400.0,
    "m3/month": 1000.0 / (86400.0 * 30.4375),
    "m3/year": 1000.0 / (86400.0 * 365.25),
    "Mm3/year": 1_000_000_000.0 / (86400.0 * 365.25),
}


def parse_num(x):
    """Parse Chilean-format numbers ('1.234,56' or '5,3000') into float."""
    if x is None or (isinstance(x, float) and pd.isna(x)):
        return None
    if isinstance(x, (int, float)):
        return float(x)
    s = str(x).strip()
    if not s:
        return None
    # Chilean: '.' is thousand sep, ',' is decimal
    if "," in s and "." in s:
        s = s.replace(".", "").replace(",", ".")
    elif "," in s:
        s = s.replace(",", ".")
    try:
        return float(s)
    except ValueError:
        return None


def parse_year(s):
    if pd.isna(s):
        return None
    m = re.search(r"(\d{4})", str(s))
    return int(m.group(1)) if m else None


def region_from_filename(fname: str) -> str:
    m = re.search(r"Region-([^_.]+?)(?:_EN)?\.xlsx$", fname)
    return m.group(1) if m else fname


def build():
    files = sorted(glob.glob(os.path.join(OUT_DIR, "Derechos_Concedidos_*_EN.xlsx")))
    frames = []
    for f in files:
        d = pd.read_excel(f)
        d["source_file"] = os.path.basename(f)
        d["region_label"] = region_from_filename(os.path.basename(f))
        frames.append(d)
    big = pd.concat(frames, ignore_index=True)

    # Numeric parsing
    for m in MONTHS:
        big[m + "_num"] = big[m].map(parse_num)
    big["annual_flow_num"] = big["Average Annual Flow"].map(parse_num)
    big["ecological_flow_lps"] = big["Ecological Flow (l/s)"].map(parse_num)
    big["volume_num"] = big["Volume"].map(parse_num)

    # Convert annual flow to L/s where unit is convertible
    big["annual_flow_Lps"] = big.apply(
        lambda r: r["annual_flow_num"] * TO_LPS[r["Flow Unit"]]
        if r["Flow Unit"] in TO_LPS and pd.notna(r["annual_flow_num"]) else None,
        axis=1,
    )

    # Grant year from resolution date
    big["grant_year"] = big[
        "Date of Resolution / Sent to Judge / CBR Registration"
    ].map(parse_year)
    big["grant_decade"] = (big["grant_year"] // 10) * 10

    # Save wide
    wide_out = os.path.join(OUT_DIR, "Water_Rights_All_Regions.xlsx")
    big.to_excel(wide_out, index=False, sheet_name="All Rights")
    print(f"Wrote {wide_out}: {len(big):,} rows")

    # Long monthly panel
    id_vars = [
        "source_file", "region_label", "Region", "Province", "Commune",
        "File Code", "Application No.", "Applicant Name",
        "Right Type", "Nature of Water", "Source Classification", "Water Use",
        "Basin", "Sub-basin", "Sub-sub-basin", "Source",
        "Exercise of the Right", "Flow Unit", "grant_year", "grant_decade",
        "annual_flow_num", "annual_flow_Lps",
    ]
    long_df = big.melt(
        id_vars=id_vars,
        value_vars=[m + "_num" for m in MONTHS],
        var_name="month_name",
        value_name="month_flow",
    )
    long_df["month_name"] = long_df["month_name"].str.replace("_num", "", regex=False)
    month_to_int = {m: i + 1 for i, m in enumerate(MONTHS)}
    long_df["month_num"] = long_df["month_name"].map(month_to_int)
    long_df["month_flow_Lps"] = long_df.apply(
        lambda r: r["month_flow"] * TO_LPS[r["Flow Unit"]]
        if r["Flow Unit"] in TO_LPS and pd.notna(r["month_flow"]) else None,
        axis=1,
    )
    long_out = os.path.join(OUT_DIR, "Water_Rights_Monthly_Panel.xlsx")
    long_df.to_excel(long_out, index=False, sheet_name="Monthly Panel")
    print(f"Wrote {long_out}: {len(long_df):,} rows")

    # Aggregated summary: region x decade x use, sum of L/s and count of rights
    summary = (
        big.groupby(["region_label", "grant_decade", "Water Use", "Right Type"],
                    dropna=False)
        .agg(
            n_rights=("File Code", "count"),
            total_annual_flow_Lps=("annual_flow_Lps", "sum"),
            mean_annual_flow_Lps=("annual_flow_Lps", "mean"),
        )
        .reset_index()
    )
    sum_out = os.path.join(OUT_DIR, "Region_Use_Summary.xlsx")
    with pd.ExcelWriter(sum_out) as w:
        summary.to_excel(w, sheet_name="region_decade_use", index=False)
        # Region totals
        (big.groupby(["region_label", "Water Use"])
         .agg(n_rights=("File Code", "count"),
              total_annual_flow_Lps=("annual_flow_Lps", "sum"))
         .reset_index()
         .to_excel(w, sheet_name="region_use_totals", index=False))
        # Top applicants per region for mining use
        mining = big[big["Water Use"] == "Mining Use"]
        (mining.groupby(["region_label", "Applicant Name"])
         .agg(n_rights=("File Code", "count"),
              total_annual_flow_Lps=("annual_flow_Lps", "sum"))
         .reset_index()
         .sort_values(["region_label", "total_annual_flow_Lps"],
                      ascending=[True, False])
         .to_excel(w, sheet_name="mining_top_applicants", index=False))
    print(f"Wrote {sum_out}")


if __name__ == "__main__":
    build()
