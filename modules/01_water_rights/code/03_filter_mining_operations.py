# Repository note: filesystem paths were normalised for public documentation; analytical logic is unchanged.
from pathlib import Path
"""Filter the DGA water rights to only the companies listed in
Mine_Dataset.xlsx -> Panel_Mine tab, and save to a new workbook.

The 14 parent companies in Panel_Mine map to many DGA holder-name variants
(divisions, accents, legal-form suffixes). Below is a curated whitelist of the
EXACT DGA 'Applicant Name' strings to keep for each parent, deliberately
excluding look-alikes (people, farms, water co-ops, unions) that share a
keyword but are not the mining company.
"""

import os
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC = PROJECT_ROOT / "data" / "processed" / "Water_Rights_All_Regions.xlsx"
OUT = PROJECT_ROOT / "data" / "processed" / "Panel_Mine_Water_Rights.xlsx"

# parent label -> exact DGA Applicant Name strings to include
WHITELIST = {
    "Anglo American Sur": [
        "ANGLO AMERICAN SUR S.A.",
        "ANGLO AMERICAN SUR S.A",
    ],
    "Minera Zaldivar": [
        "COMPAÑIA MINERA ZALDIVAR SPA",
        "COMPAÑIA MINERA ZALDIVAR LIMITADA",
        "COMPAÑÍA MINERA ZALDÍVAR LIMITADA",
    ],
    "Cia Contractual Minera Candelaria": [
        "COMPAÑIA CONTRACTUAL MINERA CANDELARIA",
    ],
    "Cia Minera Cerro Colorado": [
        "COMPAÑIA MINERA CERRO COLORADO LTDA.",
        "COMPAÑIA MINERA CERRO COLORADO LIMITADA.",
    ],
    "Cia Minera Lomas Bayas (Xstrata)": [
        "COMPAÑIA MINERA XSTRATA LOMAS BAYAS",
    ],
    "Teck Quebrada Blanca": [
        "COMPAÑIA MINERA TECK QUEBRADA BLANCA S.A.",
        "COMPAÑÍA MINERA TECK QUEBRADA BLANCA S.A.",
    ],
    "Codelco (all divisions)": [
        "CORP. NACIONAL DEL COBRE, CODELCO CHILE, DIV. ANDINA",
        "CORPORACION NACIONAL DEL COBRE DE CHILE, DIVISION ANDINA",
        "CODELCO CHILE, DIVISION EL TENIENTE",
        "CORPORACION NACIONAL DEL COBRE DE CHILE",
        "CODELCO CHILE DIVISION RADOMIRO TOMIC",
        "CORPORACION DEL COBRE DE CHILE- CODELCO CHILE",
        "CORPORACION NACIONAL DEL COBRE DE CHILE, DIVISION SALVADOR",
        "CORPORACION  NACIONAL DEL COBRE DE CHILE, DIVISION  EL TENIENTE",
        "CORPORACION NACIONAL DEL COBRE, CODELCO CHILE, DIVISION ANDINA",
        "CORPORACION NACIONAL DEL COBRE DE CHILE (CODELCO-CHILE)",
        "CODELCO CHILE, DIVISION CHUQUICAMATA",
        "CODELCO-CHILE, DIVISIÓN SALVADOR",
        "CODELCO CHILE - DIVISION CHUQUICAMATA",
        "CODELCO CHILE DIVISION ANDINA",
        "CORPORACIÓN NACIONAL DEL COBRE DE CHILE, DIVISIÓN CHUQUICAMATA",
        "CODELCO CHILE DIVISION EL TENIENTE",
        "CODELCO CHILE DIVISION GABRIELA MISTRAL",
        "CORPORACION NACIONAL DEL COBRE DE CHILE, DIVISION SALVADOR Y OTROS",
        "CODELCO CHILE DIVISION VENTANAS",
        "CODELCO CHILE",
        "CODELCO CHILE DIV.SALVADOR",
        "CORPORACION NACIONAL DEL COBRE DE CHILE DIVISION MINISTRO HALES",
        "CORPORACION NACIONAL DEL COBRE DE CHILE, DIVISION CHUQUICAMATA",
        "CORP.NACIONAL DEL COBRE-DIVISION EL TENIENTE",
        "CORPORACIÓN NACIONAL DEL COBRE DE CHILE, DIVISIÓN ANDINA",
        "CORPORACION NACIONAL DEL COBRE DE CHILE CODELCO CH",
        "PROSPECTO MINERO CODELCO",
        # excluded: "SINDICATO TURNADOS CODELCO CHILE DIVISION VENTANAS." (labor union)
    ],
    "Cia Minera Dona Ines de Collahuasi": [
        "COMPAÑIA MINERA DOÑA INES DE COLLAHUASI SCM.",
        "COMPAÑIA MINERA DOÑA INES DE COLLAHUASI SCM",
        "COMPAÑIA MINERA DOÑA INES DE COLLAHUASI S.A",
        "COMPAÑIA MINERA DOÑA INES DE COLLAHUASI S.A.",
        "COMPAÑÍA MINERA DOÑA INÉS DE COLLAHUASI SCM",
        "COMPAÑIA MINERA DE DOÑA INES DE COLLAHUASI S.A. Y OTROS",
        "COMPAÑIA MINERA DOÑA INES DE COLLAHUASI S. A.",
        # excluded: "CIA.MINERA PODEROSA COLLAHUASI" (different company)
    ],
    "Mantos Copper / Anglo American Norte": [
        "ANGLO AMERICAN NORTE S.A.",
        "INVERSIONES ANGLO AMERICAN NORTE SPA.",
        "INVERSIONES ANGLO AMERICAN NORTE SPA",
    ],
    "Minera Centinela (Tesoro/Esperanza)": [
        "MINERA CENTINELA",
        "MINERA EL TESORO",
    ],
    "Minera Los Pelambres": [
        "MINERA LOS PELAMBRES",
    ],
    "Minera Spence": [
        "MINERA SPENCE S.A.",
    ],
    "Minera Escondida": [
        "MINERA ESCONDIDA LIMITADA",
    ],
    "Soc Contractual Minera El Abra": [
        "SOCIEDAD CONTRACTUAL MINERA EL  ABRA",
    ],
}


def main():
    big = pd.read_excel(SRC)
    big["Applicant Name"] = big["Applicant Name"].astype(str).str.strip()

    # Sanity flag for implausible source values (>50 m3/s in one right)
    IMPLAUSIBLE = 50_000
    big["flow_flag"] = big["annual_flow_Lps"].apply(
        lambda v: "CHECK: implausibly large (>50 m3/s)"
        if pd.notna(v) and v > IMPLAUSIBLE else "")
    big["annual_flow_Lps_clean"] = big["annual_flow_Lps"].apply(
        lambda v: v if (pd.isna(v) or v <= IMPLAUSIBLE) else None)

    # Map each whitelisted DGA name -> parent label
    name_to_parent = {}
    for parent, names in WHITELIST.items():
        for n in names:
            name_to_parent[n] = parent

    # Verify every whitelisted name actually exists in the data
    present = set(big["Applicant Name"].unique())
    missing = [n for n in name_to_parent if n not in present]
    if missing:
        print("WARNING: whitelist names not found in data:")
        for m in missing:
            print("   ", repr(m))

    filtered = big[big["Applicant Name"].isin(name_to_parent)].copy()
    filtered["Parent Company"] = filtered["Applicant Name"].map(name_to_parent)
    # put Parent Company first
    cols = ["Parent Company"] + [c for c in filtered.columns if c != "Parent Company"]
    filtered = filtered[cols]

    print(f"Parent companies: {len(WHITELIST)}")
    print(f"Filtered rights:  {len(filtered):,}")

    # Index: one row per parent company
    index = (
        filtered.groupby("Parent Company")
        .agg(
            n_rights=("File Code", "count"),
            n_mining_use=("Water Use", lambda s: (s == "Mining Use").sum()),
            n_other_use=("Water Use", lambda s: (s != "Mining Use").sum()),
            n_flagged=("flow_flag", lambda s: (s != "").sum()),
            total_Lps_clean=("annual_flow_Lps_clean", "sum"),
            regions=("region_label", lambda s: ", ".join(sorted(set(s.dropna())))),
            first_grant_year=("grant_year", "min"),
            last_grant_year=("grant_year", "max"),
            dga_name_variants=("Applicant Name",
                               lambda s: " | ".join(sorted(set(s)))),
        )
        .reset_index()
        .sort_values("total_Lps_clean", ascending=False)
    )

    import re as _re

    def sheet_name(parent, used):
        clean = _re.sub(r"[\\/\?\*\[\]:]", "-", parent)
        s = clean[:31].rstrip()
        i = 2
        while s.lower() in used:
            s = f"{clean[:28].rstrip()}({i})"
            i += 1
        used.add(s.lower())
        return s

    used = {"index", "all rows"}
    print(f"Writing {OUT}...")
    with pd.ExcelWriter(OUT, engine="openpyxl") as w:
        index.to_excel(w, sheet_name="Index", index=False)
        filtered.to_excel(w, sheet_name="All Rows", index=False)
        for parent in index["Parent Company"]:
            sub = filtered[filtered["Parent Company"] == parent]
            sub.to_excel(w, sheet_name=sheet_name(parent, used), index=False)

    print(f"Wrote {OUT}: {os.path.getsize(OUT):,} bytes")
    print()
    print(index[["Parent Company", "n_rights", "n_mining_use",
                 "n_other_use", "total_Lps_clean", "regions"]].to_string(index=False))


if __name__ == "__main__":
    main()
