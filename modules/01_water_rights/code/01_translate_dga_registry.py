# Repository note: filesystem paths were normalised for public documentation; analytical logic is unchanged.
from pathlib import Path
"""Translate DGA Chile 'Derechos Concedidos' .xls files into English .xlsx files.

Translates: Spanish column headers, Spanish month columns, and Spanish categorical
values (water type, source classification, water use, flow units, etc.).
Leaves proper nouns alone (region/province/comuna/basin/applicant names).
"""

import os
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SRC_DIR = PROJECT_ROOT / "data" / "raw" / "dga"
OUT_DIR = PROJECT_ROOT / "data" / "processed" / "dga_translated"

HEADER_MAP = {
    "N°": "No.",
    "Código de \nExpediente": "File Code",
    "N° \nSolicitud": "Application No.",
    "Región": "Region",
    "Provincia": "Province",
    "Comuna": "Commune",
    "Nombre Solicitante": "Applicant Name",
    "Unidad de Resolución/ Oficio/ C.B.R.": "Resolution / Official Letter / CBR Unit",
    "Fecha de Resolución/ Envío al Juez/ Inscripción C.B.R.": "Date of Resolution / Sent to Judge / CBR Registration",
    "N° Resolución/ \nOficio/ Fojas | \nN° CBR": "Resolution No. / Official Letter / Pages | CBR No.",
    "Fecha Toma \nRazón": "Date of Approval (Toma de Razón)",
    "Artículo \nTransitorio": "Transitory Article",
    "Tipo Derecho": "Right Type",
    "Naturaleza del Agua": "Nature of Water",
    "Clasificación Fuente": "Source Classification",
    "Uso del Agua": "Water Use",
    "Cuenca": "Basin",
    "SubCuenca": "Sub-basin",
    "SubSubCuenca": "Sub-sub-basin",
    "Fuente": "Source",
    "Enero": "January",
    "Febrero": "February",
    "Marzo": "March",
    "Abril": "April",
    "Mayo": "May",
    "Junio": "June",
    "Julio": "July",
    "Agosto": "August",
    "Septiembre": "September",
    "Octubre": "October",
    "Noviembre": "November",
    "Diciembre": "December",
    "Ejercicio del Derecho": "Exercise of the Right",
    "Caudal \nAnual\nProm": "Average Annual Flow",
    "Unidad de \nCaudal": "Flow Unit",
    "¿Caudal Promedio Anual?": "Is Annual Average Flow?",
    "Caudal Ecológico\n(l/s)": "Ecological Flow (l/s)",
    "¿Caudal Ecológico Promedio?": "Is Ecological Flow Average?",
    "Acciones en el Cauce": "Shares in the Watercourse",
    "Acciones en la Fuente": "Shares in the Source",
    "Volumen": "Volume",
    "UTM \nNorte \nCaptación\n(m)": "UTM North Intake (m)",
    "UTM \nEste \nCaptación\n(m)": "UTM East Intake (m)",
    "Huso": "UTM Zone",
    "Datum": "Datum",
    "Latitud \nCaptación": "Intake Latitude",
    "Longitud \nCaptación": "Intake Longitude",
    "UTM \nNorte \nRestitución\n(m)": "UTM North Restitution (m)",
    "UTM \nEste \nRestitución\n(m)": "UTM East Restitution (m)",
    "Latitud \nRestitución": "Restitution Latitude",
    "Longitud \nRestitución": "Restitution Longitude",
    "Referencia a puntos conocidos de captación": "Reference to Known Intake Points",
    "Referencia a puntos conocidos de restitución": "Reference to Known Restitution Points",
    "Distancia": "Distance",
    "Unidad\nDistancia": "Distance Unit",
    "Desnivel\n(m)": "Elevation Difference (m)",
    "C.B.R.": "CBR (Real Estate Registry)",
    "Fojas": "Pages",
    "N° CBR": "CBR No.",
    "Año": "Year",
    "Código Expediente\nAntiguo": "Old File Code",
    "¿Posee \nmás de un \npunto de captación?": "Has More Than One Intake Point?",
    "N° Certificado/ Año": "Certificate No. / Year",
    "Tiene Renuncia": "Has Waiver",
    "Tiene Transacción": "Has Transaction",
    "Radio\n(m)": "Radius (m)",
    "Profundidad\n(m)": "Depth (m)",
}

# Disambiguate the two "Datum" columns (one for intake, one for restitution)
DATUM_DUP_LABELS = ("Intake Datum", "Restitution Datum")

VALUE_MAPS = {
    "Right Type": {
        "Consuntivo": "Consumptive",
        "No Consuntivo": "Non-Consumptive",
    },
    "Nature of Water": {
        "Subterranea": "Groundwater",
        "Superficial": "Surface",
        "Superf. y Detenida": "Surface and Standing",
        "Superficial y Detenida": "Surface and Standing",
        "Superficial y Corriente": "Surface and Flowing",
        "Superficial Corrientes/Detenidas": "Surface Flowing/Standing",
    },
    "Source Classification": {
        "Acuifero": "Aquifer",
        "Arroyo": "Stream",
        "Charca-Aguada": "Pond-Waterhole",
        "Chorrillo": "Brook",
        "Derrame": "Spillway/Overflow",
        "Embalse": "Reservoir",
        "Lago/Laguna": "Lake/Lagoon",
        "Quebrada": "Ravine/Creek",
        "Rio/Estero": "River/Estuary",
        "Vertiente": "Spring",
        "Zanjón": "Ditch",
    },
    "Water Use": {
        "Bebida/Uso Domestico/Saneamiento": "Drinking/Domestic Use/Sanitation",
        "Energia Hidroeléctrica": "Hydroelectric Energy",
        "Otros Usos": "Other Uses",
        "Para Observacion y Analisis": "For Observation and Analysis",
        "Piscicultura": "Fish Farming",
        "Riego": "Irrigation",
        "SilvoAgropecuario": "Silvo-Agricultural-Livestock",
        "Uso Industrial": "Industrial Use",
        "Uso Medicinal": "Medicinal Use",
        "Uso Minero": "Mining Use",
    },
    "Exercise of the Right": {
        "** Sin Ejercicio **": "** No Exercise **",
        "Eventual y Alternado": "Eventual and Alternating",
        "Eventual y Continuo": "Eventual and Continuous",
        "Eventual y Discontinuo": "Eventual and Discontinuous",
        "Perm. y Alter. y Provisionales": "Permanent and Alternating and Provisional",
        "Perm. y Cont. y Provisionales": "Permanent and Continuous and Provisional",
        "Perm. y Disc. y Provisionales": "Permanent and Discontinuous and Provisional",
        "Permanente y Alternado": "Permanent and Alternating",
        "Permanente y Continuo": "Permanent and Continuous",
        "Permanente y Discontinuo": "Permanent and Discontinuous",
    },
    "Flow Unit": {
        "Acciones": "Shares",
        "Cuadras": "Blocks",
        "Horas Turno": "Shift Hours",
        "Partes": "Parts",
        "Regadores": "Regadores (irrigation unit)",
        "lt/día": "L/day",
        "Lt/h": "L/h",
        "Lt/min": "L/min",
        "Lt/s": "L/s",
        "Mm3/año": "Mm3/year",
        "m3/año": "m3/year",
        "m3/dia": "m3/day",
        "m3/h": "m3/h",
        "m3/mes": "m3/month",
        "m3/min": "m3/min",
        "m3/s": "m3/s",
    },
    "Distance Unit": {
        "Km": "Km",
        "mts": "m",
    },
    "Is Annual Average Flow?": {"SI": "YES", "NO": "NO"},
    "Is Ecological Flow Average?": {"SI": "YES", "NO": "NO"},
    "Has More Than One Intake Point?": {"SI": "YES", "NO": "NO"},
    "Has Waiver": {"SI": "YES", "NO": "NO"},
    "Has Transaction": {"SI": "YES", "NO": "NO"},
}


def translate_file(src_path: str, out_path: str) -> None:
    raw = pd.read_excel(src_path, header=None, dtype=object)
    header_row = raw.iloc[6].tolist()
    data = raw.iloc[7:].reset_index(drop=True)
    data = data.dropna(how="all").reset_index(drop=True)

    new_cols = []
    datum_seen = 0
    for c in header_row:
        key = "" if pd.isna(c) else str(c).strip()
        label = HEADER_MAP.get(key, key if key else f"col_{len(new_cols)}")
        if label == "Datum":
            label = DATUM_DUP_LABELS[min(datum_seen, 1)]
            datum_seen += 1
        new_cols.append(label)
    data.columns = new_cols
    df = data

    for col, mapping in VALUE_MAPS.items():
        if col not in df.columns:
            continue
        df[col] = df[col].map(
            lambda v: mapping.get(str(v).strip(), v) if pd.notna(v) else v
        )

    # Strip whitespace on object columns to clean trailing spaces from the source
    for col in df.columns:
        if df[col].dtype == object:
            df[col] = df[col].map(
                lambda v: v.strip() if isinstance(v, str) else v
            )

    df.to_excel(out_path, index=False, sheet_name="Water Rights")


def main() -> None:
    files = sorted(f for f in os.listdir(SRC_DIR) if f.endswith(".xls"))
    for f in files:
        src = os.path.join(SRC_DIR, f)
        out_name = os.path.splitext(f)[0] + "_EN.xlsx"
        out = os.path.join(OUT_DIR, out_name)
        print(f"Translating {f} -> {out_name}")
        translate_file(src, out)
        print(f"  done: {os.path.getsize(out):,} bytes")


if __name__ == "__main__":
    main()
