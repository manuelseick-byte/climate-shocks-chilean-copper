# =============================================================================
# 12_minecell_monthly_indicators.R
#
# Aggregate the daily mine-cell panel to monthly indicators for the acute
# channels FLOOD and HEAT.
#
# Mine-month indicators:
#   - thr_flood_mine        any day in the month with pr_mm >= 20 at the mine cell
#   - thr_heat_mine         any day in the month with tmax_c >= 27 at the mine cell
#   - n_wet_days            count of days with pr_mm >= 20
#   - n_hot_days            count of days with tmax_c >= 27
#   - max_pr_mm             monthly max daily precipitation at the mine cell
#   - max_tmax_c            monthly max daily Tmax at the mine cell
#   - max_z_pr_on_wet_day   max z_pr_d on days with pr_mm >= 20 (NA if none)
#                           -> used in the FLOOD daily-Z alignment for FLOOD_mine
#
# Inputs:  mining_module_alternative/panels/10__minecell_daily.parquet
# Outputs: mining_module_alternative/panels/12__minecell_monthly.parquet
# =============================================================================

source(file.path("code", "00_setup.R"))

d <- as.data.table(read_parquet(file.path(PANELS, "10__minecell_daily.parquet")))

P_FLOOD       <- 20    # mm/day (daily threshold; FLOOD daily, Z daily)
HEAT_MONTHLY  <- 23.5  # deg C monthly-mean Tmax (HEAT monthly, Z monthly)

m <- d[, .(
  n_days_obs    = .N,
  n_wet_days    = sum(pr_mm   >= P_FLOOD, na.rm = TRUE),
  n_hot_days_27 = sum(tmax_c  >= 27,      na.rm = TRUE),   # informational only
  max_pr_mm     = suppressWarnings(max(pr_mm,  na.rm = TRUE)),
  max_tmax_c    = suppressWarnings(max(tmax_c, na.rm = TRUE)),
  mean_tmax_c   = mean(tmax_c, na.rm = TRUE),
  max_z_pr_on_wet_day = ifelse(any(pr_mm >= P_FLOOD, na.rm = TRUE),
                               suppressWarnings(max(z_pr_d[pr_mm >= P_FLOOD], na.rm = TRUE)),
                               NA_real_),
  mean_z_pr_m   = mean(z_pr_d,   na.rm = TRUE),
  mean_z_tmax_m = mean(z_tmax_d, na.rm = TRUE),
  source_majority = as.character(ifelse(sum(source == "cr2met") >= sum(source == "era5_bc"),
                                        "cr2met", "era5_bc"))
), by = .(mine_operation, codregion, year, month)]

m[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
m[, thr_flood_mine := as.integer(n_wet_days >= 1)]
m[, thr_heat_mine  := as.integer(!is.na(mean_tmax_c) & mean_tmax_c >= HEAT_MONTHLY)]
m[is.infinite(max_pr_mm),  max_pr_mm  := NA_real_]
m[is.infinite(max_tmax_c), max_tmax_c := NA_real_]
m[is.nan(mean_tmax_c),     mean_tmax_c := NA_real_]
setorder(m, mine_operation, year, month)

write_parquet(m, file.path(PANELS, "12__minecell_monthly.parquet"))
cat("[12] rows =", nrow(m), "\n")
cat("[12] mine-cell threshold-firing rates (1960-2025):\n")
print(m[, .(
  n_months         = .N,
  firing_flood_pct = round(100 * mean(thr_flood_mine, na.rm = TRUE), 2),
  firing_heat_pct  = round(100 * mean(thr_heat_mine, na.rm = TRUE), 2)
), by = mine_operation][order(-firing_flood_pct)])
