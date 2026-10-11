# -*- coding: utf-8 -*-
"""Translate the DGA water-scarcity decree registry (Spanish -> English).

Translate the controlled descriptive vocabulary, dates, headers and region
names, while leaving Chilean proper nouns (river/commune/province names)
untouched. Rules are mostly case-sensitive so that:
  * lowercase Spanish descriptors (rio, comuna, region...) are translated, but
    capitalized proper nouns (Rio Claro, Los Vilos, La Serena...) are preserved;
  * already-translated English words are never re-clobbered.
The connector "de/del" is only converted inside explicit descriptor phrases, so
proper names like "San Felipe de Aconcagua" or "Alto del Carmen" stay intact.
"""
import re, datetime
import xlrd
import openpyxl
from openpyxl.styles import Font, Alignment, PatternFill, Border, Side

SRC = "data/raw/Decretos_zonas_escasez_historico_10-04-2026.xls"
OUT = "data/derived/Water_Scarcity_Zone_Decrees_historical_10-04-2026_EN.xlsx"

I = re.IGNORECASE

MONTHS = {
    "enero": "January", "febrero": "February", "marzo": "March", "abril": "April",
    "mayo": "May", "junio": "June", "julio": "July", "agosto": "August",
    "septiembre": "September", "setiembre": "September", "octubre": "October",
    "noviembre": "November", "diciembre": "December",
}
MONTH_ABBR = {
    "ene": "January", "feb": "February", "abr": "April", "jun": "June",
    "jul": "July", "ago": "August", "sept": "September", "set": "September",
    "oct": "October", "nov": "November", "dic": "December",
}

# (pattern, replacement, flags). Applied in order. flags=0 means case-sensitive.
PHRASES = [
    # --- specific multi-word phrases ---
    (r"Sector Hidrogeol[oó]gico de Aprovechamiento Com[uú]n", "Common-Use Hydrogeological Sector", I),
    (r"Zonas baja y media de la cuenca del", "Lower and middle zones of the basin of the", I),
    (r"Quebrada de Paipote", "Paipote Ravine", I),
    (r"[Ll]ocalidad de", "Locality of", I),
    (r"del sector de secano ubicad[oa] en la comuna de", "of the dryland sector located in the commune of", I),
    (r"del sector de secano de la provincia de", "of the dryland sector of the province of", I),
    (r"sector de secano ubicad[oa] en la comuna de", "dryland sector located in the commune of", I),
    (r"sector de secano de la provincia de", "dryland sector of the province of", I),
    (r"sector de secano", "dryland sector", I),
    (r"del sector", "of the sector", I),
    # --- basins / rivers / streams (case-sensitive pairs to keep sentence casing) ---
    (r"Cuencas de los r[ií]os y esteros", "Basins of the rivers and streams", 0),
    (r"cuencas de los r[ií]os y esteros", "basins of the rivers and streams", 0),
    (r"Cuenca de los r[ií]os y esteros", "Basin of the rivers and streams", 0),
    (r"cuenca de los r[ií]os y esteros", "basin of the rivers and streams", 0),
    (r"Cuencas de los r[ií]os", "Basins of the rivers", 0),
    (r"cuencas de los r[ií]os", "basins of the rivers", 0),
    (r"Cuenca de los r[ií]os", "Basin of the rivers", 0),
    (r"cuenca de los r[ií]os", "basin of the rivers", 0),
    (r"Cuenca de los esteros", "Basin of the streams", 0),
    (r"cuenca de los esteros", "basin of the streams", 0),
    (r"Cuencas costeras", "Coastal basins", 0),
    (r"cuencas costeras", "coastal basins", 0),
    (r"Cuencas del [Rr][ií]o", "Basins of the River", 0),
    (r"Cuenca del [Rr][ií]o", "Basin of the River", 0),
    (r"cuenca del r[ií]o", "basin of the river", 0),
    (r"Cuencas de", "Basins of", 0),
    (r"cuencas de", "basins of", 0),
    (r"Cuenca del", "Basin of the", 0),
    (r"cuenca del", "basin of the", 0),
    (r"Cuenca de", "Basin of", 0),
    (r"cuenca de", "basin of", 0),
    (r"\bcuancas\b", "basins", I),   # source typo for "cuencas"
    (r"\bcuanca\b", "basin", I),
    (r"del Estero", "of the stream", I),
    (r"\bEstero\b", "Stream", 0),
    (r"de los r[ií]os y esteros", "of the rivers and streams", 0),
    (r"de los esteros", "of the streams", 0),
    (r"de los r[ií]os", "of the rivers", 0),
    # --- located in ... ---
    (r"ubicad[oa]s? en la provincia de", "located in the province of", I),
    (r"ubicad[oa]s? en la comuna de", "located in the commune of", I),
    (r"ubicad[oa]s? en la regi[oó]n de\b", "located in the region of", I),
    (r"ubicad[oa]s? en el", "located in the", I),
    (r"ubicad[oa]s?", "located", I),
    # --- "a la / a las / a los" (to the ...) ---
    (r"a las provincias de", "to the provinces of", I),
    (r"a la provincia de", "to the province of", I),
    (r"a las comunas de", "to the communes of", I),
    (r"a la comuna de", "to the commune of", I),
    (r"a la regi[oó]n de\b", "to the region of", I),
    (r"a los r[ií]os", "to the rivers", I),
    (r"\ba las\b", "to the", I),
    (r"\ba los\b", "to the", I),
    (r"\ba la\b", "to the", I),
    # --- "en la / en el" (in the ...) ---
    (r"en la provincia de", "in the province of", I),
    (r"en la comuna de", "in the commune of", I),
    (r"en la regi[oó]n del", "in the region of the", I),
    (r"en la regi[oó]n de\b", "in the region of", I),
    (r"\ben las\b", "in the", I),
    (r"\ben la\b", "in the", I),
    (r"\ben el\b", "in the", I),
    # --- province / commune / region descriptors (case pairs) + typos ---
    (r"Provincias de", "Provinces of", 0),
    (r"provincias de", "provinces of", 0),
    (r"Provinc[oa]as de", "Provinces of", I),   # typo "Provincoas"
    (r"Provin[vc]ia de", "Province of", 0),     # incl. typo "Provinvia"
    (r"provin[vc]ia de", "province of", 0),
    (r"Comunas de", "Communes of", 0),
    (r"comunas de", "communes of", 0),
    (r"Comuna de", "Commune of", 0),
    (r"comuna de", "commune of", 0),
    (r"Regi[oó]n Metropolitana", "Metropolitan Region", I),
    (r"Regi[oó]n del", "Region of the", 0),
    (r"regi[oó]n del", "region of the", 0),
    (r"Regi[oó]n de\b", "Region of", 0),
    (r"regi[oó]n de\b", "region of", 0),
    # --- bare standalone descriptor nouns (e.g. "Comunas: Colina") ---
    (r"\bProvincias\b", "Provinces", 0),
    (r"\bprovincias\b", "provinces", 0),
    (r"\bProvincia\b", "Province", 0),
    (r"\bprovincia\b", "province", 0),
    (r"\bComunas\b", "Communes", 0),
    (r"\bcomunas\b", "communes", 0),
    (r"\bComuna\b", "Commune", 0),
    (r"\bcomuna\b", "commune", 0),
    (r"\bRegiones\b", "Regions", 0),
    (r"\bregiones\b", "regions", 0),
    # --- leading real articles (only the article rows start with capital "Las") ---
    (r"^Las provincias", "The provinces", 0),
    (r"^Las cu[ae]ncas", "The basins", 0),
    (r"^Las\b", "The", 0),
    # --- lowercase connectors / articles (case-sensitive: never touch proper nouns) ---
    (r"\bde la\b", "of the", 0),
    (r"\bde los\b", "of the", 0),
    (r"\bde las\b", "of the", 0),
    (r"\by\b", "and", 0),
    (r"\besteros\b", "streams", 0),
    (r"\bestero\b", "stream", 0),
    (r"\br[ií]os\b", "rivers", 0),
    (r"\br[ií]o\b", "river", 0),
    (r"\blas\b", "the", 0),
    (r"\blos\b", "the", 0),
    (r"\bla\b", "the", 0),
    (r"\bel\b", "the", 0),
]


def translate_months(text):
    for es, en in MONTHS.items():
        text = re.sub(r"\b" + es + r"\b", en, text, flags=I)
    return text


def fix_glued(text):
    text = re.sub(r"\bde(\d{4})\b", r"de \1", text)            # de2020 -> de 2020
    text = re.sub(r"octubrede", "octubre de", text, flags=I)   # octubrede -> octubre de
    return text


def translate_date_cell(text):
    if not text:
        return text
    t = fix_glued(text)
    t = t.replace("N°", "No.").replace("Nº", "No.").replace("N °", "No.")
    t = translate_months(t)
    for ab, en in MONTH_ABBR.items():
        t = re.sub(r"\b" + ab + r"\.?\b", en, t, flags=I)
    t = re.sub(r"(\d{1,2})\s+de\s+([A-Z][a-z]+)\s+de\s+(\d{4})", r"\1 \2 \3", t)
    t = re.sub(r"(\d{1,2})\s+de\s+([A-Z][a-z]+)\s+(\d{4})", r"\1 \2 \3", t)  # day de Month year
    t = re.sub(r"([A-Z][a-z]+)\s+de\s+(\d{4})", r"\1 \2", t)
    t = re.sub(r"\bdel\b\s+(\d)", r"of \1", t)
    # remaining connector "de"/"del" in a pure date cell -> "of"
    t = re.sub(r"\b[Dd]el\b", "of the", t)
    t = re.sub(r"\b[Dd]e\b", "of", t)
    t = re.sub(r"\bof of\b", "of", t)   # source double "de de" typo
    t = re.sub(r"\s+,", ",", t)
    t = re.sub(r"\s+", " ", t).strip()
    return t


def translate_desc(text):
    if not text:
        return text
    t = text.strip()
    for pat, rep, flags in PHRASES:
        t = re.sub(pat, rep, t, flags=flags)
    t = re.sub(r"\s+", " ", t).strip()
    t = t.replace(" of the of ", " of ").replace(" of of ", " of ")
    return t


REGION_MAP = {
    "Metropolitana": "Metropolitan Region",
    "Magallanes y de la Antártica Chilena": "Magallanes and Chilean Antarctica",
    "Bíobío": "Biobío",
    "Biobío": "Biobío",
    "O´higgins": "O'Higgins",
    "O' Higgins": "O'Higgins",
    "O'Higgins": "O'Higgins",
}


def translate_region(text):
    if not text:
        return text
    t = text.strip()
    if t in REGION_MAP:
        return REGION_MAP[t]
    t = re.sub(r"\s+y\s+", " and ", t)
    t = t.replace("Metropolitana", "Metropolitan")
    return t


HEADER_MAP = {
    "Año": "Year",
    "Región": "Region",
    "Decreto": "Decree",
    "Fecha de Caducidad": "Expiry Date",
    "Cuenca/Comuna/Provincia": "Basin / Commune / Province",
}


def excel_serial_to_date(n):
    base = datetime.date(1899, 12, 30)
    return (base + datetime.timedelta(days=int(n))).strftime("%d %B %Y")


# ---- read source ----
b = xlrd.open_workbook(SRC)
s = b.sheet_by_index(0)
src = []
for r in range(s.nrows):
    row = []
    for c in range(5):
        v = s.cell_value(r, c)
        if isinstance(v, float):
            v = str(int(v)) if v == int(v) else str(v)
        row.append(v)
    src.append(row)

# ---- translate ----
out_rows = []
for row in src:
    year, region, decree, expiry, desc = row
    if region == "Región" and decree == "Decreto":
        out_rows.append([HEADER_MAP.get(x, x) for x in row])
        continue
    if re.fullmatch(r"\d{5}", (expiry or "").strip()):
        expiry_en = excel_serial_to_date(expiry.strip())
    else:
        expiry_en = translate_date_cell(expiry)
    out_rows.append([
        year,
        translate_region(region),
        translate_date_cell(decree),
        expiry_en,
        translate_desc(desc),
    ])

# ---- write xlsx ----
wb = openpyxl.Workbook()
ws = wb.active
ws.title = "Sheet1"
hdr_fill = PatternFill("solid", fgColor="D9E1F2")
hdr_font = Font(bold=True)
thin = Side(style="thin", color="BFBFBF")
border = Border(left=thin, right=thin, top=thin, bottom=thin)
for r, row in enumerate(out_rows, start=1):
    is_hdr = row[1] == "Region" and row[2] == "Decree"
    for c, val in enumerate(row, start=1):
        cell = ws.cell(row=r, column=c, value=val)
        cell.border = border
        cell.alignment = Alignment(vertical="top", wrap_text=(c == 5))
        if is_hdr:
            cell.fill = hdr_fill
            cell.font = hdr_font
        elif c == 1 and val and re.fullmatch(r"\d{4}", str(val)):
            cell.font = Font(bold=True)
widths = [8, 26, 34, 22, 95]
for i, w in enumerate(widths, start=1):
    ws.column_dimensions[openpyxl.utils.get_column_letter(i)].width = w
ws.freeze_panes = "A2"
wb.save(OUT)
print("wrote", OUT, "rows", len(out_rows))

for i in range(len(out_rows)):
    print(i, " | ".join(str(x) for x in out_rows[i]))
