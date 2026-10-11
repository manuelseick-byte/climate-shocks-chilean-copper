# =============================================================================
# 31_drought_annual_panel.R
#
# Build the mine x year panel for the slow water-stress channel.
# Drought operates on multi-year timescales (aquifer drawdown, reservoir
# depletion, water-rights tightening); the natural identifying frequency is
# annual.  We compute the annual share of months in which DROUGHT_meteo or
# DROUGHT_hydro stress fires, plus 1, 2 and 3-year lagged shares, and average
# the y-1, y-2, y-3 shares into a single cumulative regressor as the headline.
#
# Inputs:  ledger/20__master_stress_ledger.parquet
# Outputs: panels/31__drought_mine_year.parquet
#          panels/31__drought_mine_year_active.csv
# =============================================================================

source(file.path("code", "00_setup.R"))

ledger <- as.data.table(read_parquet(file.path(LEDGER, "20__master_stress_ledger.parquet")))

dr <- ledger[channel %in% c("DROUGHT_meteo", "DROUGHT_hydro")]

# Annual mine-year shares of months with stress fired and threshold crossed
annual_meteo <- dr[channel == "DROUGHT_meteo",
                   .(n_valid_spei      = sum(!is.na(threshold_crossed)),
                     thr_share_meteo   = mean(threshold_crossed == 1, na.rm = TRUE),
                     str_share_meteo   = mean(stress_flag == 1, na.rm = TRUE),
                     n_months          = .N,
                     n_kt_obs          = sum(!is.na(copper_kt))),
                   by = .(mine_operation, codregion, year)]
# FIX (2026-05-30, per supervisor step 2): require a FULL year of valid mine-cell
# SPEI before computing an annual drought share; partial years -> NA (do not
# average a share over an incomplete year).  With the 2022-2025 ERA5 extension
# (script 13) every year now has 12 valid months, so no real year is dropped.
annual_meteo[n_valid_spei < 12, c("thr_share_meteo", "str_share_meteo") := NA_real_]

annual_hydro <- dr[channel == "DROUGHT_hydro",
                   .(thr_share_hydro   = mean(threshold_crossed == 1, na.rm = TRUE),
                     str_share_hydro   = mean(stress_flag == 1, na.rm = TRUE),
                     n_months_hydro    = .N,
                     n_kt_obs_h        = sum(!is.na(copper_kt))),
                   by = .(mine_operation, codregion, year)]

# --- SSI-12 basin validity (final alignment) -------------------------------
# SSI-12 is the standardised streamflow drought index of each mine's source
# basin.  It is defined only for mines whose source basin has a representative
# gauged catchment (mine_basin_crosswalk.csv column `ssi_valid`).  Mines drawing
# from closed endorheic salar systems with no representative streamflow gauge
# (Salar de Atacama: Escondida, Zaldivar; Salar Elvira: Codelco-Gaby;
# Salar de Ascotan: El Abra) and the coastal/cross-region Anglo American Norte
# aggregate carry no SSI-12; they enter the drought analysis through the
# mine-cell SPEI-12 only.
xw <- fread(file.path(PROJECT_ROOT, "mining_module", "registry",
                      "mine_basin_crosswalk.csv"))
xw[, ssi_valid := as.logical(ssi_valid)]
ssi_excluded <- xw[ssi_valid == FALSE, mine_operation]
n_match <- annual_hydro[mine_operation %in% ssi_excluded, uniqueN(mine_operation)]
cat("[31] SSI-12 excluded (no representative streamflow basin):",
    paste(ssi_excluded, collapse = "; "), "->", n_match, "of",
    length(ssi_excluded), "matched in panel\n")
annual_hydro[mine_operation %in% ssi_excluded,
             c("thr_share_hydro", "str_share_hydro") := NA_real_]

ann <- merge(annual_meteo, annual_hydro,
             by = c("mine_operation", "codregion", "year"), all.x = TRUE)

# Mine x year copper output: annual total kt (from monthly)
co <- fread(file.path(ORIG_PANELS, "01a__cochilco_copper_mine_monthly.csv"))
copper_year <- co[, .(copper_kt_year = sum(copper_kt, na.rm = TRUE),
                      n_months_active = sum(copper_kt > 0, na.rm = TRUE)),
                  by = .(mine_operation, codregion, year)]

ann <- merge(ann, copper_year, by = c("mine_operation", "codregion", "year"), all.x = TRUE)
ann[, log_kt_year := suppressWarnings(log(copper_kt_year))]
ann[is.infinite(log_kt_year), log_kt_year := NA_real_]

# Annual mean ONI from the regional climate panel (same for all mines in a region)
rcm <- as.data.table(read_parquet(file.path(DATA_PROC, "climate",
                                            "02b__regional_climate_monthly_panel.parquet")))
oni_year <- rcm[codregion %in% COPPER_REGIONS,
                .(oni_year = mean(oni_rolling, na.rm = TRUE),
                  oni_year_abs = mean(abs(oni_rolling), na.rm = TRUE)),
                by = .(codregion, year)]
ann <- merge(ann, oni_year, by = c("codregion", "year"), all.x = TRUE)

# Non-ENSO drought share = threshold-crossed but NOT ENSO-aligned (= thr - str,
# since stress months are a subset of threshold months).  Enables the
# decomposition test (supervisor Priority 1): ENSO-attributable vs non-ENSO.
ann[, nonenso_share_meteo := thr_share_meteo - str_share_meteo]

# Lags and cumulative
setorder(ann, mine_operation, year)
for (col in c("thr_share_meteo", "str_share_meteo", "nonenso_share_meteo",
              "thr_share_hydro", "str_share_hydro")) {
  for (lg in 1:3) {
    ann[, paste0(col, "_lag", lg) := shift(get(col), n = lg, type = "lag"), by = mine_operation]
  }
}
# FIX (2026-05-30, per supervisor step 3): the HEADLINE meteo cum3 requires a
# COMPLETE 3-lag window (na.rm = FALSE) -> NA if any of y-1,y-2,y-3 is missing.
# Even after the NA fix, available-lag averaging lets a single surviving lag
# (e.g. the 2021 megadrought) contaminate post-gap years.  An available-lag
# variant is retained, clearly labelled, for the robustness contrast only.
ann[, cum3_str_meteo := rowMeans(.SD),
    .SDcols = c("str_share_meteo_lag1", "str_share_meteo_lag2", "str_share_meteo_lag3")]
ann[, cum3_thr_meteo := rowMeans(.SD),
    .SDcols = c("thr_share_meteo_lag1", "thr_share_meteo_lag2", "thr_share_meteo_lag3")]
ann[, cum3_nonenso_meteo := rowMeans(.SD),
    .SDcols = c("nonenso_share_meteo_lag1", "nonenso_share_meteo_lag2", "nonenso_share_meteo_lag3")]
ann[, cum3_str_meteo_avail := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("str_share_meteo_lag1", "str_share_meteo_lag2", "str_share_meteo_lag3")]
ann[is.nan(cum3_str_meteo_avail), cum3_str_meteo_avail := NA_real_]
# SSI (hydro) is structurally gappy and demoted to robustness: available-lag.
ann[, cum3_str_hydro := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("str_share_hydro_lag1", "str_share_hydro_lag2", "str_share_hydro_lag3")]
ann[is.nan(cum3_str_hydro), cum3_str_hydro := NA_real_]
ann[, cum3_thr_hydro := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("thr_share_hydro_lag1", "thr_share_hydro_lag2", "thr_share_hydro_lag3")]
ann[is.nan(cum3_thr_hydro), cum3_thr_hydro := NA_real_]

# Active sample (year-level): at least 6 active mine-months and positive output
ann[, in_sample := !is.na(log_kt_year) & n_months_active >= 6]

setorder(ann, mine_operation, year)
write_parquet(ann, file.path(PANELS, "31__drought_mine_year.parquet"))
fwrite(ann[in_sample == TRUE], file.path(PANELS, "31__drought_mine_year_active.csv"))

cat("[31] drought annual panel: rows =", nrow(ann),
    " active =", sum(ann$in_sample, na.rm = TRUE),
    " year range =", min(ann$year[ann$in_sample]), "-", max(ann$year[ann$in_sample]), "\n")
print(ann[in_sample == TRUE,
          .(year_range = paste(min(year), "-", max(year)),
            mean_log_kt = round(mean(log_kt_year, na.rm = TRUE), 2),
            mean_str_meteo = round(mean(str_share_meteo, na.rm = TRUE), 3),
            mean_str_hydro = round(mean(str_share_hydro, na.rm = TRUE), 3)),
          by = mine_operation])
