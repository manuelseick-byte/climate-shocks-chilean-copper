# =============================================================================
# mining_module_alternative/scripts/00_setup.R
#
# Common setup: paths, packages, region keys, palette.  Sourced by every
# downstream script.  This module is a fresh rebuild and writes ONLY into
# mining_module_alternative/.  Inputs are read from mining_module/panels/
# and Data_processed/ but never written there.
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
})

MODULE_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
PROJECT_ROOT <- MODULE_ROOT
ALT_ROOT <- MODULE_ROOT
ORIG_PANELS <- file.path(MODULE_ROOT, "data", "panels")
DATA_PROC <- file.path(MODULE_ROOT, "data", "inputs")

SCRIPTS  <- file.path(ALT_ROOT, "code")
REGISTRY <- file.path(ALT_ROOT, "data", "registry")
PANELS   <- file.path(ALT_ROOT, "data", "panels")
LEDGER   <- file.path(ALT_ROOT, "data", "ledger")
TABLES   <- file.path(ALT_ROOT, "results", "tables")
FIGURES  <- file.path(ALT_ROOT, "results", "figures")
WRITEUP  <- file.path(ALT_ROOT, "results", "report")

for (d in c(PANELS, LEDGER, TABLES, FIGURES)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

COPPER_REGIONS <- c(1L, 2L, 3L, 4L, 5L, 6L, 13L)
REGION_NAME <- c(
  "1" = "Tarapaca", "2" = "Antofagasta", "3" = "Atacama", "4" = "Coquimbo",
  "5" = "Valparaiso", "6" = "O'Higgins", "13" = "Santiago Metropolitan"
)

# Three documented multi-year ENSO event windows used for leave-one-event-out
ENSO_EVENT_WINDOWS <- list(
  list(label = "2015-16 El Nino", start = as.Date("2014-09-01"), end = as.Date("2016-05-31")),
  list(label = "2020-22 La Nina", start = as.Date("2020-08-01"), end = as.Date("2023-02-28")),
  list(label = "2023-24 El Nino", start = as.Date("2023-06-01"), end = as.Date("2024-05-31"))
)

# Colour palette for figures — colour-blind safe, journal-friendly
PAL <- list(
  ink         = "#1A1A1A",
  paper       = "#FFFFFF",
  rule        = "#B8B8B8",
  flood_low   = "#3B5BA5",   # mine-cell (conservative)
  flood_up    = "#9FBAE6",   # FLOOD upper shade (precip Z panels)
  heat        = "#D2691E",
  drought     = "#8B5A3C",
  enso_nino   = "#C0392B",
  enso_nina   = "#1F77B4",
  enso_neutral= "#A0A0A0",
  pos_event   = "#2C7FB8",
  miss        = "#E76F51"
)

cat("[00_setup] PROJECT_ROOT =", PROJECT_ROOT, "\n")
cat("[00_setup] ALT_ROOT    =", ALT_ROOT, "\n")
