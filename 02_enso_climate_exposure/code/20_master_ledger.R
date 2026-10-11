# =============================================================================
# 20_master_ledger.R
#
# THE master stress ledger.  One row per mine x date(month-start) x channel.
# Every row carries:
#   - mine / region keys
#   - the physical observation (value + unit)
#   - the operational threshold (value + unit) and whether it was crossed
#   - the three named ENSO components (ONI value, regional teleconnection rho,
#     z-anomaly value and which kind: monthly or daily-on-wet-day)
#   - the ENSO-aligned flag (1 iff the three components agree on the channel's
#     predicted direction with |ONI| >= 0.5)
#   - the stress flag (AND of threshold-crossed and ENSO-aligned)
#   - copper output (log_kt) for that mine-month when available
#
# Channels in the ledger:
#   FLOOD_regmean_M   regional-mean monthly accumulation (headline)
#   FLOOD_mine        mine-cell daily extreme  (conservative point-level lower bound)
#   HEAT_mine         mine-cell daily exceedance        (headline acute)
#   DROUGHT_meteo     SPEI-12 at mine cell              (slow, monthly aggregator)
#   DROUGHT_hydro     SSI-12  at primary basin gauge    (slow, monthly aggregator)
#
# Inputs:
#   panels/12__minecell_monthly.parquet
#   panels/11__enso_context_monthly.parquet
#   mining_module/panels/03e__regionmax_flood_monthly_extended.parquet   region-MAX FLOOD daily, 1960-2025
#   mining_module/panels/02d__daily_pr_z_unified.parquet                 region-day Z anomaly (for max-Z-on-wet-day)
#   mining_module/panels/02__enso_anomalies_monthly.parquet              has any_wetday_with_posZ at region-MAX scale
#   mining_module/panels/01k__minesite_spei12_monthly.parquet            SPEI-12 mine cell
#   mining_module/panels/01j__ssi_mine_monthly.parquet                   SSI-12 basin gauge
#   mining_module/panels/01a__cochilco_copper_mine_monthly.csv           copper output
#   registry/mines.csv                                                   mine inventory
#
# Outputs:
#   ledger/20__master_stress_ledger.parquet   (full)
#   ledger/20__master_stress_ledger.csv       (auditable)
#   tables/20__ledger_channel_counts.csv      summary by channel
# =============================================================================

source(file.path("code", "00_setup.R"))

mines <- fread(file.path(REGISTRY, "mines.csv"))

# ----- INPUTS -----
mc  <- as.data.table(read_parquet(file.path(PANELS, "12__minecell_monthly.parquet")))
ec  <- as.data.table(read_parquet(file.path(PANELS, "11__enso_context_monthly.parquet")))

# (FLOOD_corridor region-MAX channel dropped 2026-05-31: removed from the
#  distributed ledger as a deprecated and confusable channel -- it was being
#  mistaken for the regional headline FLOOD_regmean_M. See registry/channel_dictionary.csv.)

# Regional mean monthly accumulation, descriptive — from the regional climate panel
rcm <- as.data.table(read_parquet(file.path(DATA_PROC, "climate",
                                            "02b__regional_climate_monthly_panel.parquet")))
rcm <- rcm[codregion %in% COPPER_REGIONS,
           .(codregion, year, month, date,
             regmean_pr_total_mm = pr_total_mm)]
rcm[, date := as.Date(date)]

# Slow indices
# DROUGHT fix (2026-05-30): use the 1960-2025 mine-cell SPEI extended with
# bias-corrected ERA5-Land 2022-2025 (script 13), replacing the 1960-2021-only
# 01k panel whose post-2021 gap was being coded as zero drought.
spei <- as.data.table(read_parquet(file.path(PANELS, "13__minecell_spei12_1960_2025.parquet")))
spei <- spei[, .(mine_operation, codregion, year, month, spei12_minecell = spei12)]

ssi <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01j__ssi_mine_monthly.parquet")))
ssi <- ssi[, .(mine_operation, year, month, ssi12 = ssi_12, ssi12_robust = ssi_12_robust)]

# Copper output
co <- fread(file.path(ORIG_PANELS, "01a__cochilco_copper_mine_monthly.csv"))
co <- co[, .(mine_operation, codregion, year, month, copper_kt, log_kt)]

# ----- BUILD THE MINE-MONTH BACKBONE -----
backbone <- expand.grid(
  mine_operation = mines$mine_operation,
  year  = 1960:2025,
  month = 1:12,
  stringsAsFactors = FALSE
) |> as.data.table()
backbone <- merge(backbone, mines[, .(mine_operation, codregion, region_en,
                                      lon, lat, approx_elev_m, altitude_tier,
                                      primary_basin_gauge_code, primary_basin_gauge_name)],
                  by = "mine_operation", all.x = TRUE)
backbone[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
backbone <- merge(backbone, ec,  by = c("codregion", "year", "month", "date"), all.x = TRUE)
backbone <- merge(backbone, mc,  by = c("mine_operation", "codregion", "year", "month", "date"),
                  all.x = TRUE)
backbone <- merge(backbone, rcm, by = c("codregion", "year", "month", "date"), all.x = TRUE)
backbone <- merge(backbone, spei, by = c("mine_operation", "codregion", "year", "month"), all.x = TRUE)
backbone <- merge(backbone, ssi, by = c("mine_operation", "year", "month"), all.x = TRUE)
backbone <- merge(backbone, co, by = c("mine_operation", "codregion", "year", "month"), all.x = TRUE)
setorder(backbone, mine_operation, year, month)

# ----- ENSO ALIGNMENT AT THE RESOLUTION THAT MATCHES EACH CHANNEL -----
# Symmetry principle: each channel's Z resolution matches its threshold resolution.
#   FLOOD daily  -> daily Z (max_z_pr_on_wet_day at mine cell; any_wetday_posZ at region-MAX)
#   HEAT monthly -> monthly-mean Z_T at region scale (aligned_heat from script 11)
#   DROUGHT monthly -> monthly-mean Z_pr at region scale (aligned_drought from script 11)
# All require |ONI| >= 0.5 AND rho * ONI > 0 (regional teleconnection predicts channel direction)
backbone[, aligned_flood_mine := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  !is.na(rho_pr_m)    & rho_pr_m * oni_rolling > 0 &
  !is.na(max_z_pr_on_wet_day) & max_z_pr_on_wet_day > 0
)]
# HEAT (monthly-monthly): aligned_heat is computed in script 11 using monthly-mean Z_T at the region scale

# ----- LONG-FORMAT LEDGER: one row per channel -----
chan_list <- list()

# --- FLOOD_mine (mine-cell daily; channel string is "FLOOD_mine") ---
chan_list[["FLOOD_mine"]] <- backbone[, .(
  mine_operation, codregion, region_en, date, year, month,
  copper_kt, log_kt,
  channel             = "FLOOD_mine",
  physical_value      = max_pr_mm,
  physical_unit       = "mm/day (mine-cell max daily P)",
  threshold_value     = 20,
  threshold_unit      = "mm/day",
  threshold_crossed   = thr_flood_mine,
  oni_value           = oni_rolling,
  oni_phase           = phase_rolling,
  rho_region_month    = rho_pr_m,
  z_anomaly_value     = max_z_pr_on_wet_day,
  z_anomaly_type      = "daily Z on wet day (mine cell)",
  enso_aligned        = aligned_flood_mine,
  stress_flag         = pmin(thr_flood_mine, aligned_flood_mine, na.rm = TRUE)
)]

# (FLOOD_corridor channel removed 2026-05-31 -- see note above.)

# --- FLOOD_regmean_M (headline regional FLOOD channel - Spec A -4.7%) ---
chan_list[["FLOOD_regmean_M"]] <- backbone[, .(
  mine_operation, codregion, region_en, date, year, month,
  copper_kt, log_kt,
  channel             = "FLOOD_regmean_M",
  physical_value      = regmean_pr_total_mm,
  physical_unit       = "mm/month (region-mean accumulation)",
  threshold_value     = 20,
  threshold_unit      = "mm/month",
  threshold_crossed   = as.integer(!is.na(regmean_pr_total_mm) & regmean_pr_total_mm >= 20),
  oni_value           = oni_rolling,
  oni_phase           = phase_rolling,
  rho_region_month    = rho_pr_m,
  z_anomaly_value     = z_pr_m,
  z_anomaly_type      = "monthly-mean Z (region)",
  enso_aligned        = aligned_flood_monthlyZ,
  stress_flag         = pmin(as.integer(!is.na(regmean_pr_total_mm) & regmean_pr_total_mm >= 20),
                             aligned_flood_monthlyZ, na.rm = TRUE)
)]

# --- HEAT_mine (consistent-monthly: monthly threshold + monthly Z; string "HEAT_mine") ---
chan_list[["HEAT_mine"]] <- backbone[, .(
  mine_operation, codregion, region_en, date, year, month,
  copper_kt, log_kt,
  channel             = "HEAT_mine",
  physical_value      = mean_tmax_c,
  physical_unit       = "deg C (mine-cell monthly-mean Tmax)",
  threshold_value     = 23.5,
  threshold_unit      = "deg C (monthly mean)",
  threshold_crossed   = thr_heat_mine,
  oni_value           = oni_rolling,
  oni_phase           = phase_rolling,
  rho_region_month    = rho_t_m,
  z_anomaly_value     = z_t_m,
  z_anomaly_type      = "monthly-mean Z_T (region)",
  enso_aligned        = aligned_heat,
  stress_flag         = pmin(thr_heat_mine, aligned_heat, na.rm = TRUE)
)]

# --- DROUGHT_meteo (SPEI-12 at mine cell) ---
chan_list[["DROUGHT_meteo"]] <- backbone[, .(
  mine_operation, codregion, region_en, date, year, month,
  copper_kt, log_kt,
  channel             = "DROUGHT_meteo",
  physical_value      = spei12_minecell,
  physical_unit       = "unitless (SPEI-12)",
  threshold_value     = -1,
  threshold_unit      = "unitless",
  # FIX (2026-05-30): missing SPEI -> NA, NOT 0.  A missing mine-cell SPEI is
  # "drought unknown", not "no drought"; coding it 0 fabricated non-drought.
  threshold_crossed   = fifelse(is.na(spei12_minecell), NA_integer_,
                                as.integer(spei12_minecell <= -1)),
  oni_value           = oni_rolling,
  oni_phase           = phase_rolling,
  rho_region_month    = rho_pr_m,
  z_anomaly_value     = z_pr_m,
  z_anomaly_type      = "monthly-mean Z_pr (region)",
  enso_aligned        = aligned_drought,
  stress_flag         = fifelse(is.na(spei12_minecell), NA_integer_,
                                pmin(as.integer(spei12_minecell <= -1),
                                     aligned_drought, na.rm = TRUE))
)]

# --- DROUGHT_hydro (SSI-12) ---
chan_list[["DROUGHT_hydro"]] <- backbone[, .(
  mine_operation, codregion, region_en, date, year, month,
  copper_kt, log_kt,
  channel             = "DROUGHT_hydro",
  physical_value      = ssi12,
  physical_unit       = "unitless (SSI-12)",
  threshold_value     = -1,
  threshold_unit      = "unitless",
  # FIX (2026-05-30): missing SSI -> NA, NOT 0 (same rationale as DROUGHT_meteo).
  threshold_crossed   = fifelse(is.na(ssi12), NA_integer_, as.integer(ssi12 <= -1)),
  oni_value           = oni_rolling,
  oni_phase           = phase_rolling,
  rho_region_month    = rho_pr_m,
  z_anomaly_value     = z_pr_m,
  z_anomaly_type      = "monthly-mean Z_pr (region)",
  enso_aligned        = aligned_drought,
  stress_flag         = fifelse(is.na(ssi12), NA_integer_,
                                pmin(as.integer(ssi12 <= -1),
                                     aligned_drought, na.rm = TRUE))
)]

ledger <- rbindlist(chan_list, fill = TRUE, use.names = TRUE)
setorder(ledger, mine_operation, date, channel)

# Coerce NAs to 0 in flag columns where the inputs are missing pre-1960 / mine-not-active etc.
# Stress_flag should be NA if threshold is NA (e.g., SSI pre-2006 or SPEI pre-1961 with NA)
ledger[is.na(stress_flag), stress_flag := NA_integer_]

# Persist
write_parquet(ledger, file.path(LEDGER, "20__master_stress_ledger.parquet"))
fwrite(ledger,        file.path(LEDGER, "20__master_stress_ledger.csv"))

# Summaries
cat("[20] master ledger:", nrow(ledger), "rows;", length(unique(ledger$channel)), "channels;",
    length(unique(ledger$mine_operation)), "mines;", length(unique(ledger$date)), "dates\n")

s <- ledger[, .(
  n_rows           = .N,
  n_thr_crossed    = sum(threshold_crossed == 1, na.rm = TRUE),
  pct_thr_crossed  = round(100 * mean(threshold_crossed == 1, na.rm = TRUE), 2),
  n_enso_aligned   = sum(enso_aligned == 1, na.rm = TRUE),
  pct_enso_aligned = round(100 * mean(enso_aligned == 1, na.rm = TRUE), 2),
  n_stress         = sum(stress_flag == 1, na.rm = TRUE),
  pct_stress       = round(100 * mean(stress_flag == 1, na.rm = TRUE), 2)
), by = channel]
print(s)
fwrite(s, file.path(TABLES, "20__ledger_channel_counts.csv"))
cat("[20] Wrote ledger to ledger/ and channel summary to tables/.\n")
