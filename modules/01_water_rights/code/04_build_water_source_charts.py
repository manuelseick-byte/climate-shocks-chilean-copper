# Repository note: filesystem paths were normalised for public documentation; analytical logic is unchanged.
from pathlib import Path
"""Two COCHILCO water charts:
   1. Share of water extraction by SOURCE over time, 2013-2024 (national).
   2. Regional water extraction by SOURCE, 2013 vs 2024 reference years.

Source data: Anuario 2003-2022 (Tabla 32 sec 2, Tabla 69) for 2013/2014;
             Anuario 2005-2024 (Tabla 34 sec 2, Tabla 72) for 2015-2024.

Note on years requested: water data does not exist in COCHILCO before 2013
(the encuesta only started then), and 2025 has not yet been published — so
the realistic window is 2013-2024.
"""

import os
import re
import warnings

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

warnings.simplefilter("ignore")
PROJECT_ROOT = Path(__file__).resolve().parents[1]
BASE = PROJECT_ROOT / "data" / "raw" / "cochilco"
OUT = PROJECT_ROOT / "results" / "figures"
os.makedirs(OUT, exist_ok=True)
OLD = f"{BASE}/Anuario-de-Estadisticas-del-Cobre-y-Otros-Minerales-2003-2022.xlsx"
NEW = f"{BASE}/Anuario-de-Estadisticas-del-Cobre-y-otros-Minerales-2005-2024.xlsx"

SOURCE_ORDER = ["Surface", "Groundwater", "Third-party", "Seawater"]
SOURCE_COLORS = {"Surface": "#2a7fb8", "Groundwater": "#8c6d3f",
                 "Third-party": "#9aa0a6", "Seawater": "#16a085"}


def _num(x):
    if pd.isna(x):
        return None
    try:
        return float(x)
    except (TypeError, ValueError):
        s = re.sub(r"[^0-9.\-]", "", str(x))
        return float(s) if s else None


def _row_label(s):
    s = str(s).lower()
    if "superficial" in s: return "Surface"
    if "subterrán" in s or "subterran" in s: return "Groundwater"
    if "terceros" in s or "third-party" in s or "purchased" in s: return "Third-party"
    if "aguas de mar" in s or "seawater" in s: return "Seawater"
    return None


# ----------------------------- Chart 1 data -----------------------------------
old = pd.read_excel(OLD, sheet_name="Tabla 32", header=None)
new = pd.read_excel(NEW, sheet_name="Tabla 34", header=None)

def extract_section2(df):
    """Return DataFrame year -> {Surface, GW, Third-party, Seawater} in L/s."""
    # locate the section 2 header row, then the header (years) and 4 source rows
    sec2 = next(i for i, r in df.iterrows()
                if str(r[0]).strip().startswith("2.") and
                ("EXTRACCIÓN" in str(r[0]) or "EXTRACCION" in str(r[0])))
    # find the year header in the next few rows
    hdr_row = None
    for j in range(sec2 + 1, sec2 + 6):
        cells = [c for c in df.iloc[j] if pd.notna(c)]
        if any(isinstance(c, (int, float)) and 2000 < c < 2030 for c in cells):
            hdr_row = j; break
    years = [int(v) for v in df.iloc[hdr_row].tolist()
             if isinstance(v, (int, float)) and 2000 < v < 2030]
    year_cols = [df.iloc[hdr_row].tolist().index(y) for y in years]
    rows = {}
    for j in range(hdr_row + 1, hdr_row + 6):
        lab = _row_label(df.iloc[j, 0])
        if lab and lab in SOURCE_ORDER:
            rows[lab] = [_num(df.iloc[j, c]) for c in year_cols]
    return pd.DataFrame(rows, index=years)


tab_old = extract_section2(old)
tab_new = extract_section2(new)

print("Tabla 32 sec 2 (2003-2022 Anuario):")
print(tab_old)
print("\nTabla 34 sec 2 (2005-2024 Anuario):")
print(tab_new)

# Stitch: prefer new where available; old fills 2013, 2014
all_years = sorted(set(tab_old.index) | set(tab_new.index))
series = pd.DataFrame(index=all_years, columns=SOURCE_ORDER, dtype=float)
for y in all_years:
    src = tab_new.loc[y] if y in tab_new.index else tab_old.loc[y]
    for c in SOURCE_ORDER:
        series.loc[y, c] = src.get(c, np.nan)
series = series.dropna(how="all")
print("\nCombined national-source series (L/s):")
print(series.round(0).to_string())
series.to_excel(f"{OUT}/COCHILCO_water_source_national_2013_2024.xlsx")

# ----------------------------- Chart 1 plot -----------------------------------
shares = series.div(series.sum(axis=1), axis=0) * 100

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(13.5, 5.4))

# Absolute L/s stacked area
years = series.index.values
bottoms = np.zeros(len(series))
for src in SOURCE_ORDER:
    ax1.fill_between(years, bottoms, bottoms + series[src].values,
                     color=SOURCE_COLORS[src], label=src, alpha=0.92)
    bottoms = bottoms + series[src].values
ax1.set_xlim(years.min(), years.max())
ax1.set_ylim(0, bottoms.max() * 1.05)
ax1.set_xlabel("Year")
ax1.set_ylabel("Water extraction (L/s)")
ax1.set_title("Chilean copper mining — water extraction by source, 2013-2024",
              fontweight="bold", fontsize=11)
ax1.grid(alpha=0.3)
ax1.legend(loc="upper left", fontsize=9, frameon=True)
ax1.text(0.99, 0.02,
         "Source: COCHILCO Anuario, Tabla 32 (2013-14) + Tabla 34 (2015-24)",
         transform=ax1.transAxes, ha="right", va="bottom", fontsize=7, color="#777")

# Share (%) stacked area — the requested "share" view
bottoms = np.zeros(len(shares))
for src in SOURCE_ORDER:
    ax2.fill_between(years, bottoms, bottoms + shares[src].values,
                     color=SOURCE_COLORS[src], label=src, alpha=0.92)
    bottoms = bottoms + shares[src].values
ax2.set_xlim(years.min(), years.max())
ax2.set_ylim(0, 100)
ax2.set_xlabel("Year")
ax2.set_ylabel("Share of total water extraction (%)")
ax2.set_title("Share of water extraction by source, 2013-2024",
              fontweight="bold", fontsize=11)
ax2.grid(alpha=0.3)
ax2.legend(loc="upper left", fontsize=9, frameon=True)
# annotate seawater share at 2013 and 2024 to make the shift visible
sw_first = shares["Seawater"].iloc[0]
sw_last = shares["Seawater"].iloc[-1]
ax2.text(years[0], 100 - sw_first/2, f"Seawater\n{sw_first:.0f}%",
         ha="left", va="center", fontsize=8, color="white", fontweight="bold")
ax2.text(years[-1], 100 - sw_last/2, f"{sw_last:.0f}%",
         ha="right", va="center", fontsize=9, color="white", fontweight="bold")

fig.tight_layout()
fig.savefig(f"{OUT}/COCHILCO_chart1_source_share_2013_2024.png", dpi=140,
            bbox_inches="tight")
plt.close(fig)
print(f"\nWrote Chart 1: COCHILCO_chart1_source_share_2013_2024.png")

# ============================ Chart 1B — line chart ===========================
# Total water extraction by source, line chart, with crosses on the seawater
# line at user-specified years.
MARK_YEARS = [2011, 2016, 2018, 2020, 2021, 2023, 2024]

fig, ax = plt.subplots(figsize=(11, 5.4))
years = series.index.values
for src in SOURCE_ORDER:
    ax.plot(years, series[src].values, color=SOURCE_COLORS[src],
            lw=2.2, marker="o", markersize=5, label=src)

# Crosses on the seawater line at marked years
sw = series["Seawater"]
crosses_x, crosses_y, missing = [], [], []
for y in MARK_YEARS:
    if y in sw.index and pd.notna(sw.loc[y]):
        crosses_x.append(y); crosses_y.append(sw.loc[y])
    else:
        missing.append(y)
ax.scatter(crosses_x, crosses_y, marker="x", s=80, c="#000000", lw=1.8,
           zorder=5, label="Desal plants live")
for x, y in zip(crosses_x, crosses_y):
    ax.annotate(f"{x}\n{y:,.0f} L/s", xy=(x, y), xytext=(0, 14),
                textcoords="offset points", ha="center", fontsize=8,
                fontweight="bold", color="#000")

ax.set_xlim(years.min() - 0.5, years.max() + 0.5)
ax.set_ylim(0, series.values.max() * 1.15)
ax.set_xlabel("Year")
ax.set_ylabel("Water extraction (L/s)")
ax.set_title("Chilean copper mining — total water extraction by source",
             fontweight="bold", fontsize=11.5)
ax.grid(alpha=0.3)
ax.legend(loc="upper left", fontsize=9, frameon=True)

fig.tight_layout()
fig.savefig(f"{OUT}/COCHILCO_chart1b_source_lines_marked.png", dpi=140,
            bbox_inches="tight")
plt.close(fig)
print(f"Wrote Chart 1B: COCHILCO_chart1b_source_lines_marked.png")

# ----------------------------- Chart 2 data -----------------------------------
old69 = pd.read_excel(OLD, sheet_name="Tabla 69", header=None)
new72 = pd.read_excel(NEW, sheet_name="Tabla 72", header=None)


def extract_regional(df, target_year):
    """Parse the regional × source table; return region × source L/s for target_year.
    Handles both Anuario formats (old: 'ARICA AND PARINACOTA REGION'; new:
    'Región de Tarapacá / Tarapaca Region')."""
    hdr_row, ycol = None, None
    for i, r in df.iterrows():
        for j, c in enumerate(r.tolist()):
            if isinstance(c, (int, float)) and not pd.isna(c) and \
                    2000 < c < 2030 and int(c) == int(target_year):
                hdr_row, ycol = i, j; break
        if hdr_row is not None: break
    if hdr_row is None:
        return None

    out, cur_region = {}, None
    for i in range(hdr_row + 1, len(df)):
        lab = str(df.iloc[i, 0]).strip()
        ll = lab.lower()
        if not lab or lab == "nan":
            continue
        # Skip national-total marker rows
        if ("nacional" in ll or "domestic total" in ll or "chile total" in ll
                or "country total" in ll or "total país" in ll or "total pais" in ll):
            cur_region = None
            continue
        # Source row?
        src = _row_label(lab)
        if src:
            if cur_region:
                v = _num(df.iloc[i, ycol])
                if v is not None:
                    out[cur_region][src] = v
            continue
        # Total-continental / total row (skip)
        if (ll.startswith("total ") or "total aguas continentales" in ll
                or "total continental water" in ll):
            continue
        # Region header? Must contain the word "Region" (case-insensitive).
        # New format: "Región de Tarapacá / Tarapaca Region"
        # Old format: "ARICA AND PARINACOTA REGION"
        if not re.search(r"\bregion\b", lab, flags=re.I):
            continue
        region = None
        if "/" in lab:
            en = lab.split("/", 1)[1].strip()
            if not re.search(r"\bRegion\b", en, flags=re.I):
                continue  # not a real region header
            en = re.sub(r"\s+Region\s*$", "", en, flags=re.I).strip()
            if en:
                region = en
        else:
            m2 = re.match(r"^(.+?)\s+REGION\s*$", lab.strip(), flags=re.I)
            if m2:
                region = m2.group(1).strip().title()
        if region:
            cur_region = region
            out.setdefault(cur_region, {})
    return pd.DataFrame(out).T.reindex(columns=SOURCE_ORDER, fill_value=0.0)


reg_2013 = extract_regional(old69, 2013)
reg_2024 = extract_regional(new72, 2024)
print("\n=== Regional by source, 2013 ===")
print(reg_2013.round(1) if reg_2013 is not None else "MISSING")
print("\n=== Regional by source, 2024 ===")
print(reg_2024.round(1) if reg_2024 is not None else "MISSING")

# Align regions (keep ones with non-zero in either year), sensible order
COPPER_REGIONS = ["Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
                  "Valparaíso", "Metropolitana de Santiago",
                  "del Libertador General Bernardo O'Higgins",
                  "del Maule"]
# Normalise to short labels
SHORT = {
    "Arica And Parinacota": "Arica y Parinacota",
    "Arica and Parinacota": "Arica y Parinacota",
    "Arica y Parinacota": "Arica y Parinacota",
    "Tarapaca": "Tarapacá",
    "Tarapacá": "Tarapacá",
    "Antofagasta": "Antofagasta",
    "Atacama": "Atacama",
    "Coquimbo": "Coquimbo",
    "Valparaiso": "Valparaíso",
    "Valparaíso": "Valparaíso",
    "Santiago Metropolitan": "Metropolitana",
    "Metropolitana": "Metropolitana",
    "O'Higgins": "O'Higgins",
    "Maule": "Maule",
    "Aysen": "Aysén",
    "Aysén": "Aysén",
}


def normalise(df):
    if df is None:
        return None
    df = df.copy()
    # normalise apostrophe variants (´ ' ’ → ')
    norm_idx = []
    for r in df.index:
        s = str(r).strip().replace("´", "'").replace("’", "'").replace("`", "'")
        norm_idx.append(SHORT.get(s, s))
    df.index = norm_idx
    df = df.groupby(level=0).sum()
    return df


reg_2013 = normalise(reg_2013)
reg_2024 = normalise(reg_2024)
regions_keep = ["Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
                "Valparaíso", "Metropolitana", "O'Higgins"]
reg_2013 = reg_2013.reindex(regions_keep, fill_value=0)
reg_2024 = reg_2024.reindex(regions_keep, fill_value=0)
reg_2013.to_excel(f"{OUT}/COCHILCO_regional_by_source_2013.xlsx")
reg_2024.to_excel(f"{OUT}/COCHILCO_regional_by_source_2024.xlsx")

# ----------------------------- Chart 2 plot -----------------------------------
fig, ax = plt.subplots(figsize=(12.5, 6.2))
n = len(regions_keep)
bw = 0.4
x = np.arange(n)

def stack_bars(ax, x, df, width, hatch=None, alpha=1.0, label_suffix=""):
    bottoms = np.zeros(len(df))
    for src in SOURCE_ORDER:
        ax.bar(x, df[src].values, width, bottom=bottoms,
               color=SOURCE_COLORS[src], edgecolor="#222", linewidth=0.5,
               alpha=alpha, hatch=hatch, label=src + label_suffix)
        bottoms = bottoms + df[src].values
    return bottoms


tot13 = stack_bars(ax, x - bw/2, reg_2013, bw, label_suffix=" (2013)")
tot24 = stack_bars(ax, x + bw/2, reg_2024, bw, label_suffix=" (2024)")

# Year labels above each bar pair
ymax = max(tot13.max(), tot24.max())
for xi, r in enumerate(regions_keep):
    ax.text(xi - bw/2, tot13[xi] + ymax * 0.01, "2013",
            ha="center", va="bottom", fontsize=7, color="#444")
    ax.text(xi + bw/2, tot24[xi] + ymax * 0.01, "2024",
            ha="center", va="bottom", fontsize=7, color="#444")
ax.set_xticks(x)
ax.set_xticklabels(regions_keep, rotation=10, ha="right")
ax.set_ylabel("Water extraction (L/s)")
ax.set_title("Chilean copper mining — regional water extraction by source, "
             "2013 vs 2024",
             fontweight="bold", fontsize=11)
# Dedup legend
handles, labels = ax.get_legend_handles_labels()
seen = {}
clean_h, clean_l = [], []
for h, l in zip(handles, labels):
    key = l.split(" (")[0]
    if key not in seen:
        seen[key] = True
        # Use a clean handle without hatch for the legend
        clean_h.append(plt.Rectangle((0, 0), 1, 1, color=SOURCE_COLORS[key]))
        clean_l.append(key)
ax.legend(clean_h, clean_l, fontsize=9, loc="upper right",
          title="Source", frameon=True)
ax.grid(axis="y", alpha=0.3)
fig.tight_layout()
fig.savefig(f"{OUT}/COCHILCO_chart2_regional_source_2013_vs_2024.png",
            dpi=140, bbox_inches="tight")
plt.close(fig)
print(f"Wrote Chart 2: COCHILCO_chart2_regional_source_2013_vs_2024.png")
