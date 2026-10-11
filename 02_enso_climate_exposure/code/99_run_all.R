# =============================================================================
# 99_run_all.R --- orchestrate the full pipeline end-to-end.
# =============================================================================

.find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "CLAUDE.md"))) {
    p <- dirname(d); if (p == d) stop("CLAUDE.md not found"); d <- p
  }
  d
}
ROOT <- .find_root()
SCRIPTS <- file.path(ROOT, "mining_module_alternative", "scripts")

steps <- c(
  "10_climate_minecell_daily.R",
  "11_enso_context_monthly.R",
  "12_minecell_monthly_indicators.R",
  "20_master_ledger.R",
  "30_acute_panel.R",
  "31_drought_annual_panel.R",
  "40_regressions_acute.R",
  "41_regressions_drought.R",
  "42_validation_documented_events.R",
  "60_figures.R",
  "61_figures_regression.R",
  "70_build_tex_tables.R"
)

for (s in steps) {
  cat("=========================================\n")
  cat(" >>>", s, "\n")
  cat("=========================================\n")
  source(file.path(SCRIPTS, s), echo = FALSE)
}
cat("[99] done.  Now compile the PDF with xelatex.\n")
