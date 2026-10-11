# =============================================================================
# 11_enso_context_monthly.R
#
# Build the ENSO context table at region-month resolution.
# Three named components are kept explicit (no "leg" wording):
#   - oni_rolling       : NOAA 3-month rolling Oceanic Nino Index
#   - rho_pr / rho_t    : regional teleconnection - Pearson correlation between
#                         the regional monthly variable and ONI by calendar month,
#                         computed on the 1980-2010 baseline
#   - z_pr_m / z_t_m    : realised standardised anomaly for the region-month
#
# Three "ENSO-aligned for channel c" flags are produced (rule: all three named
# components must point in the channel-relevant direction):
#   - aligned_drought : drier (s_c = -1)  -> rho_pr*ONI <0, z_pr_m<0, |ONI|>=0.5
#   - aligned_flood_m : wetter (s_c = +1) -> rho_pr*ONI >0, z_pr_m>0, |ONI|>=0.5   (monthly Z reference)
#   - aligned_heat    : hotter (s_c = +1) -> rho_t*ONI  >0, z_t_m >0, |ONI|>=0.5
#
# Inputs (READ-ONLY):
#   Data_processed/climate/02b__regional_climate_monthly_panel.parquet
#
# Outputs:
#   mining_module_alternative/panels/11__enso_context_monthly.parquet
# =============================================================================

source(file.path("code", "00_setup.R"))

rcm <- as.data.table(read_parquet(
  file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")
))

cat("[11] Regional climate monthly: rows =", nrow(rcm), "\n")

# Keep only the 7 copper-mining regions and the columns we need
keep_cols <- c("codregion", "year", "month", "date",
               "oni_rolling", "phase_rolling",
               "rho_pr_m", "rho_t_m",
               "Z_pr_m", "Z_t_m",
               "pr_total_mm", "tmax_c", "tmean_c")
ec <- rcm[codregion %in% COPPER_REGIONS, ..keep_cols]
setnames(ec, c("Z_pr_m", "Z_t_m"), c("z_pr_m", "z_t_m"))
ec[, date := as.Date(date)]

# Named, explicit alignment flags
ec[, oni_active := as.integer(abs(oni_rolling) >= 0.5)]

# DROUGHT (drier): predict drier when rho_pr * ONI < 0 AND observe z_pr_m < 0
ec[, aligned_drought := as.integer(
  oni_active == 1 &
  rho_pr_m * oni_rolling < 0 &
  z_pr_m < 0
)]

# FLOOD via monthly Z (used by the regional-mean monthly descriptive view)
# predict wetter when rho_pr * ONI > 0 AND observe z_pr_m > 0
ec[, aligned_flood_monthlyZ := as.integer(
  oni_active == 1 &
  rho_pr_m * oni_rolling > 0 &
  z_pr_m > 0
)]

# HEAT: predict hotter when rho_t * ONI > 0 AND observe z_t_m > 0
ec[, aligned_heat := as.integer(
  oni_active == 1 &
  rho_t_m * oni_rolling > 0 &
  z_t_m > 0
)]

# Diagnostic
cat("[11] firing rates over 7 mining regions, 1960-2025:\n")
diag <- ec[, .(
  share_oni_active        = mean(oni_active, na.rm = TRUE),
  share_aligned_drought   = mean(aligned_drought, na.rm = TRUE),
  share_aligned_flood_mZ  = mean(aligned_flood_monthlyZ, na.rm = TRUE),
  share_aligned_heat      = mean(aligned_heat, na.rm = TRUE)
), by = codregion]
print(diag)

write_parquet(ec, file.path(PANELS, "11__enso_context_monthly.parquet"))
cat("[11] Wrote", file.path(PANELS, "11__enso_context_monthly.parquet"),
    " (rows =", nrow(ec), ")\n")
