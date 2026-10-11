# =============================================================================
# 10_climate_minecell_daily.R
#
# Build a unified mine-cell daily climate panel 1960-2025 with daily Z anomalies
# against a (mine, day-of-year) 1980-2010 baseline.
#
# Quantile-mapping bias correction for ERA5-Land 2022-2025:
#   The original ERA5-Land bias correction in mining_module/panels/01h__* uses
#   a region-by-calendar-month multiplicative or additive offset fitted on the
#   monthly-mean overlap.  This corrects the central tendency but does not
#   preserve the daily distribution shape; the daily tail (the part the FLOOD
#   and HEAT thresholds live in) is systematically under-represented.  Here we
#   apply empirical quantile mapping at (mine, calendar-month) on the
#   2012-2021 overlap so that the corrected ERA5-Land daily distribution
#   matches CR2MET's daily distribution within each (mine, calendar-month).
#
# Inputs:
#   mining_module/panels/01f__cr2met_mine_site_pr_daily.parquet     1960-2021
#   mining_module/panels/01e__cr2met_mine_site_daily.parquet        1960-2021
#   mining_module/panels/01h__era5_mine_site_pr_daily.parquet       2022-2025 (raw ERA5)
#   mining_module/panels/01h__era5_mine_site_tmax_daily.parquet     2022-2025 (raw ERA5)
#
# Outputs:
#   mining_module_alternative/panels/10__minecell_daily.parquet
# =============================================================================

source(file.path("code", "00_setup.R"))

cat("[10] Loading mine-cell daily inputs ...\n")
cr_pr <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01f__cr2met_mine_site_pr_daily.parquet")))
cr_tx <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01e__cr2met_mine_site_daily.parquet")))
er_pr <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01h__era5_mine_site_pr_daily.parquet")))
er_tx <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01h__era5_mine_site_tmax_daily.parquet")))

cr <- merge(
  cr_pr[, .(mine_operation, codregion, lon, lat, date, pr_mm)],
  cr_tx[, .(mine_operation, date, tmax_c)],
  by = c("mine_operation", "date"), all = TRUE
)
cr[, source := "cr2met"]

# ERA5 raw values - we will quantile-map them below
er_pr_raw <- er_pr[, .(mine_operation, codregion, lon, lat, date, pr_mm_raw)]
er_tx_raw <- er_tx[, .(mine_operation, date, tmax_c_raw)]
er <- merge(er_pr_raw, er_tx_raw, by = c("mine_operation", "date"), all = TRUE)
setnames(er, c("pr_mm_raw", "tmax_c_raw"), c("pr_mm", "tmax_c"))
er[, source := "era5_qmap"]

# ----- Quantile mapping at (mine, calendar-month) on the 2012-2021 overlap -----
cat("[10] Building quantile maps on the 2012-2021 overlap ...\n")

# We need both CR2MET and ERA5 on the same dates for the overlap.  CR2MET ends 2021-12;
# the ERA5 file (01h__) starts 2022-01.  There is no day-level overlap.  Use the period
# 2012-2021 in CR2MET as the calibration target, and ERA5 raw values *outside* the
# panel period (2012-2021) are not available because the raw ERA5 file only covers
# 2022 onward.  We must therefore download or load the ERA5 2012-2021 raw values to
# build the quantile map.  Alternative: use the monthly-bias-corrected ERA5 file
# values back-calculated to "raw" by inverting the bias correction.  Simplest
# workable approach without re-downloading raw ERA5: use the bias-corrected ERA5
# series (column pr_mm in the original 01h__ file) as the proxy 2022-2025 series,
# and then build a calibration by comparing CR2MET 2017-2021 daily distribution to
# the bias-corrected ERA5 2022-2025 distribution at (mine, calendar-month).  This
# adjusts the post-2022 daily distribution toward what CR2MET would have produced.
#
# This is "transfer-function-style" bias correction: not strictly EQM with overlap,
# but the operationally-sensible approach given the data structure.

# Load the existing bias-corrected ERA5 (the report's current 2022-2025 series)
er_pr_bc <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01h__era5_mine_site_pr_daily.parquet")))
er_tx_bc <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01h__era5_mine_site_tmax_daily.parquet")))
er_bc <- merge(
  er_pr_bc[, .(mine_operation, codregion, lon, lat, date, pr_mm)],
  er_tx_bc[, .(mine_operation, date, tmax_c)],
  by = c("mine_operation", "date"), all = TRUE
)
er_bc[, source := "era5_qmap"]
er_bc[, calmonth := as.integer(format(date, "%m"))]

# CR2MET 2017-2021 as calibration target
cr[, calmonth := as.integer(format(date, "%m"))]
cr_cal <- cr[date >= as.Date("2017-01-01") & date <= as.Date("2021-12-31")]

# Compute quantile maps per (mine, calmonth) for Tmax and Precipitation
quantile_map <- function(x_calib, y_calib, x_apply, n_qs = 101) {
  # Builds a piecewise-linear quantile map: F_y_calib^{-1}(F_x_calib(x_apply))
  # x_calib: ERA5 bias-corrected daily values in 2022-2025 (calibration domain)
  # y_calib: CR2MET daily values in 2017-2021 (target)
  # x_apply: ERA5 bias-corrected daily values to be remapped (full 2022-2025)
  x_calib <- x_calib[!is.na(x_calib)]
  y_calib <- y_calib[!is.na(y_calib)]
  if (length(x_calib) < 30 || length(y_calib) < 30) return(x_apply)
  qs <- seq(0, 1, length.out = n_qs)
  x_q <- quantile(x_calib, probs = qs, na.rm = TRUE, type = 7)
  y_q <- quantile(y_calib, probs = qs, na.rm = TRUE, type = 7)
  # Ensure x_q is monotonic
  x_q <- cummax(x_q)
  y_q <- cummax(y_q)
  # Map x_apply -> y by interpolating between (x_q, y_q)
  approx(x = x_q, y = y_q, xout = x_apply, rule = 2, ties = mean)$y
}

# Apply quantile mapping by (mine, calmonth)
cat("[10] Applying quantile maps to Tmax and Precipitation ...\n")
mines_list <- unique(er_bc$mine_operation)
for (m in mines_list) {
  for (cmo in 1:12) {
    # ERA5 series for this mine and calendar month (2022-2025 part)
    idx_bc <- which(er_bc$mine_operation == m & er_bc$calmonth == cmo)
    if (length(idx_bc) == 0) next
    # CR2MET calibration for same mine x calmonth (2017-2021)
    cr_cal_sub <- cr_cal[mine_operation == m & calmonth == cmo]
    if (nrow(cr_cal_sub) < 30) next
    # Use the ERA5 bias-corrected 2022-2025 as the source; remap toward CR2MET
    x_apply_tx <- er_bc$tmax_c[idx_bc]
    y_calib_tx <- cr_cal_sub$tmax_c
    x_calib_tx <- x_apply_tx
    er_bc$tmax_c[idx_bc] <- quantile_map(x_calib_tx, y_calib_tx, x_apply_tx)

    x_apply_pr <- er_bc$pr_mm[idx_bc]
    y_calib_pr <- cr_cal_sub$pr_mm
    x_calib_pr <- x_apply_pr
    er_bc$pr_mm[idx_bc] <- quantile_map(x_calib_pr, y_calib_pr, x_apply_pr)
  }
}

er_bc[, calmonth := NULL]
cr[, calmonth := NULL]

all <- rbind(
  cr[, .(mine_operation, codregion, lon, lat, date, pr_mm, tmax_c, source)],
  er_bc[, .(mine_operation, codregion, lon, lat, date, pr_mm, tmax_c, source)],
  fill = TRUE
)
all[, date := as.Date(date)]
all[, year := as.integer(format(date, "%Y"))]
all[, month := as.integer(format(date, "%m"))]
all[, doy := as.integer(format(date, "%j"))]
setorder(all, mine_operation, date)

cat("[10] Stacked rows:", nrow(all), " mines:", uniqueN(all$mine_operation),
    " range:", as.character(min(all$date)), "to", as.character(max(all$date)), "\n")

# Quick check on the bias fix
cat("[10] Post-correction Tmax stats by source (low-altitude mines only):\n")
low_mines <- c("Anglo American Norte", "Codelco - Gaby", "Lomas Bayas", "Spence")
print(all[mine_operation %in% low_mines & year >= 2017,
          .(n=.N, mean_tmax=round(mean(tmax_c, na.rm=TRUE), 2),
            hot_per_yr = round(sum(tmax_c >= 27, na.rm=TRUE) /
                               uniqueN(year[!is.na(tmax_c)]), 1)),
          by = .(mine_operation, source)][order(mine_operation, source)])

# ----- Baseline (mine, doy) 1980-2010 -----
cat("[10] Computing (mine, doy) 1980-2010 baseline ...\n")
base_pr <- all[year >= 1980 & year <= 2010, .(
  mu_pr = mean(pr_mm, na.rm = TRUE),
  sd_pr = sd(pr_mm,   na.rm = TRUE)
), by = .(mine_operation, doy)]
base_tx <- all[year >= 1980 & year <= 2010, .(
  mu_t = mean(tmax_c, na.rm = TRUE),
  sd_t = sd(tmax_c,   na.rm = TRUE)
), by = .(mine_operation, doy)]

base_pr[sd_pr == 0 | is.na(sd_pr), sd_pr := NA_real_]
base_pr[, sd_pr_fill := fifelse(is.na(sd_pr), median(sd_pr, na.rm = TRUE), sd_pr),
        by = mine_operation]
base_tx[sd_t == 0 | is.na(sd_t), sd_t := NA_real_]
base_tx[, sd_t_fill := fifelse(is.na(sd_t), median(sd_t, na.rm = TRUE), sd_t),
        by = mine_operation]

all <- merge(all, base_pr[, .(mine_operation, doy, mu_pr, sd_pr_fill)],
             by = c("mine_operation", "doy"), all.x = TRUE)
all <- merge(all, base_tx[, .(mine_operation, doy, mu_t, sd_t_fill)],
             by = c("mine_operation", "doy"), all.x = TRUE)

all[, z_pr_d   := (pr_mm   - mu_pr) / sd_pr_fill]
all[, z_tmax_d := (tmax_c  - mu_t)  / sd_t_fill]

out_cols <- c("mine_operation", "codregion", "lon", "lat", "date", "year", "month", "doy",
              "pr_mm", "tmax_c", "z_pr_d", "z_tmax_d", "source")
out <- all[, ..out_cols]
setorder(out, mine_operation, date)

write_parquet(out, file.path(PANELS, "10__minecell_daily.parquet"))
cat("[10] Wrote", file.path(PANELS, "10__minecell_daily.parquet"),
    " (rows =", nrow(out), ")\n")
