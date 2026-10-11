# =============================================================================
# 43_robustness_enso.R
#
# ENSO-attribution robustness battery.  Re-fits the headline regressions under
# three alternative ENSO-attribution rules and compares to the baseline:
#
#   (A) BASELINE: 3-month rolling ONI, Pearson rho, symmetric sign test
#   (B) Seasonal-window ONI: JJA ONI for DROUGHT and FLOOD; DJF ONI for HEAT
#   (C) Spearman rho instead of Pearson rho
#   (D) Phase-conditional mean Z-bar instead of symmetric sign(rho * ONI)
#
# Headline specifications re-fitted under each rule:
#   - FLOOD_regmean stress (Spec B ENSO-attributable)
#   - HEAT_mine stress (Spec B ENSO-attributable)
#   - DROUGHT 3-yr cumulative stress, SPEI-12 (mine + year FE)
#
# Output: tables/43__enso_robustness.csv  -- compact comparison table
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

# ----- Load primitives -----
cat("[43] Loading inputs ...\n")
rcm <- as.data.table(read_parquet(file.path(DATA_PROC, "climate",
                                            "02b__regional_climate_monthly_panel.parquet")))
rcm <- rcm[codregion %in% COPPER_REGIONS]

oni_seas <- as.data.table(read_parquet(file.path(DATA_PROC, "climate", "01d__oni_seasonal.parquet")))
# oni_seas has year, season ('JJA', 'DJF', ...), oni, phase

# Mine-cell monthly indicators (for the daily-Z anchors)
mc <- as.data.table(read_parquet(file.path(PANELS, "12__minecell_monthly.parquet")))

# Regression panels and inputs
acu <- as.data.table(read_parquet(file.path(PANELS, "30__acute_mine_month.parquet")))
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))

# Copper price for drought annual
DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")
cp <- fread(file.path(DATA_RAW, "BCCh", "copper_lme_monthly_usd_per_mt.csv"))
cp[, date := as.Date(paste0(date, "-01"))]
cp[, year := as.integer(format(date, "%Y"))]
cp[, log_price := log(price_usd_mt)]
yp <- cp[, .(log_price_year = mean(log_price, na.rm = TRUE)), by = year]

# ----- Compute alternative rho's -----

# Pearson rho (baseline reference) - already in rcm as rho_pr_m, rho_t_m

# Spearman rho on 1980-2010
cat("[43] Computing Spearman rho 1980-2010 ...\n")
rho_spearman <- rcm[year >= 1980 & year <= 2010, .(
  rho_pr_spearman = suppressWarnings(cor(pr_total_mm, oni_rolling, method = "spearman", use = "complete.obs")),
  rho_t_spearman  = suppressWarnings(cor(tmean_c,     oni_rolling, method = "spearman", use = "complete.obs"))
), by = .(codregion, month)]

# Phase-conditional mean Z-bar on 1980-2010
cat("[43] Computing phase-conditional Z-bar 1980-2010 ...\n")
rcm[, oni_phase_lab := fcase(
  oni_rolling >=  0.5, "nino",
  oni_rolling <= -0.5, "nina",
  default = "neutral")]

zbar <- rcm[year >= 1980 & year <= 2010 & oni_phase_lab %in% c("nino","nina"), .(
  zbar_pr = mean(Z_pr_m, na.rm = TRUE),
  zbar_t  = mean(Z_t_m,  na.rm = TRUE)
), by = .(codregion, month, oni_phase_lab)]

# ----- Seasonal-window ONI -----
# JJA ONI: NOAA's JJA value for the year
# DJF ONI: NOAA's DJF value attached to the following calendar year (DJF 2014/15 -> 2015)
oni_seas_w <- dcast(oni_seas[season %in% c("JJA","DJF"), .(year, season, oni)],
                   year ~ season, value.var = "oni")
setnames(oni_seas_w, c("year","DJF","JJA"), c("year","oni_DJF","oni_JJA"))

# ----- Assemble region-month grids for the three alternative rules -----
ec <- rcm[, .(codregion, year, month, date, oni_rolling, phase_rolling,
              rho_pr_m, rho_t_m, Z_pr_m, Z_t_m, oni_phase_lab)]

# Merge Spearman rho
ec <- merge(ec, rho_spearman, by = c("codregion","month"), all.x = TRUE)

# Merge phase-conditional Z-bar separately for nino and nina
zbar_w <- dcast(zbar, codregion + month ~ oni_phase_lab, value.var = c("zbar_pr","zbar_t"))
ec <- merge(ec, zbar_w, by = c("codregion","month"), all.x = TRUE)

# Merge seasonal-window ONI
ec <- merge(ec, oni_seas_w, by = "year", all.x = TRUE)

# Channel direction indicators
ec[, s_drought := -1L]   # drier
ec[, s_flood   :=  1L]   # wetter
ec[, s_heat    :=  1L]   # hotter

# ----- ALTERNATIVE (B) Seasonal-window ONI -----
# Replace oni_rolling with channel-appropriate seasonal ONI in the rule.
# DROUGHT and FLOOD: JJA ONI (Chilean rainy season); HEAT: DJF ONI (austral summer)

ec[, aligned_drought_seasonal := as.integer(
  !is.na(oni_JJA) & abs(oni_JJA) >= 0.5 &
  rho_pr_m * oni_JJA * s_drought > 0 &
  Z_pr_m * s_drought > 0
)]
ec[, aligned_flood_seasonal := as.integer(
  !is.na(oni_JJA) & abs(oni_JJA) >= 0.5 &
  rho_pr_m * oni_JJA * s_flood > 0 &
  Z_pr_m * s_flood > 0
)]
ec[, aligned_heat_seasonal := as.integer(
  !is.na(oni_DJF) & abs(oni_DJF) >= 0.5 &
  rho_t_m  * oni_DJF * s_heat > 0 &
  Z_t_m * s_heat > 0
)]

# ----- ALTERNATIVE (C) Spearman rho -----
ec[, aligned_drought_spearman := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  rho_pr_spearman * oni_rolling * s_drought > 0 &
  Z_pr_m * s_drought > 0
)]
ec[, aligned_flood_spearman := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  rho_pr_spearman * oni_rolling * s_flood > 0 &
  Z_pr_m * s_flood > 0
)]
ec[, aligned_heat_spearman := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  rho_t_spearman * oni_rolling * s_heat > 0 &
  Z_t_m * s_heat > 0
)]

# ----- ALTERNATIVE (D) Phase-conditional Z-bar -----
# Predicted direction = sign of historical mean Z in the current phase
ec[, predicted_pr := fcase(
  oni_phase_lab == "nino",  sign(zbar_pr_nino),
  oni_phase_lab == "nina",  sign(zbar_pr_nina),
  default = 0)]
ec[, predicted_t := fcase(
  oni_phase_lab == "nino",  sign(zbar_t_nino),
  oni_phase_lab == "nina",  sign(zbar_t_nina),
  default = 0)]

ec[, aligned_drought_phase := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  predicted_pr == s_drought &
  Z_pr_m * s_drought > 0
)]
ec[, aligned_flood_phase := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  predicted_pr == s_flood &
  Z_pr_m * s_flood > 0
)]
ec[, aligned_heat_phase := as.integer(
  !is.na(oni_rolling) & abs(oni_rolling) >= 0.5 &
  predicted_t == s_heat &
  Z_t_m * s_heat > 0
)]

cat("[43] Region-month alignment summary across rules (firing share):\n")
print(ec[, .(
  drought_baseline = round(mean(rho_pr_m * oni_rolling > 0 & Z_pr_m < 0 & abs(oni_rolling) >= 0.5, na.rm=TRUE), 3),
  drought_seasonal = round(mean(aligned_drought_seasonal, na.rm=TRUE), 3),
  drought_spearman = round(mean(aligned_drought_spearman, na.rm=TRUE), 3),
  drought_phase    = round(mean(aligned_drought_phase, na.rm=TRUE), 3),
  flood_baseline   = round(mean(rho_pr_m * oni_rolling > 0 & Z_pr_m > 0 & abs(oni_rolling) >= 0.5, na.rm=TRUE), 3),
  flood_seasonal   = round(mean(aligned_flood_seasonal, na.rm=TRUE), 3),
  flood_spearman   = round(mean(aligned_flood_spearman, na.rm=TRUE), 3),
  flood_phase      = round(mean(aligned_flood_phase, na.rm=TRUE), 3),
  heat_baseline    = round(mean(rho_t_m * oni_rolling > 0 & Z_t_m > 0 & abs(oni_rolling) >= 0.5, na.rm=TRUE), 3),
  heat_seasonal    = round(mean(aligned_heat_seasonal, na.rm=TRUE), 3),
  heat_spearman    = round(mean(aligned_heat_spearman, na.rm=TRUE), 3),
  heat_phase       = round(mean(aligned_heat_phase, na.rm=TRUE), 3)
)])

# ----- Build mine-month panels under each alternative rule -----
# For the acute regressions we need stress flags at mine-month level.
# For each alternative, replace the region-month aligned_* flag in the acute panel.

build_alt_panel <- function(rule_label, suffix) {
  # rule_label: 'seasonal', 'spearman', 'phase'
  cols_to_pull <- c("codregion","year","month",
                    paste0("aligned_drought_", suffix),
                    paste0("aligned_flood_",   suffix),
                    paste0("aligned_heat_",    suffix))
  ec_sub <- ec[, ..cols_to_pull]
  setnames(ec_sub, paste0("aligned_drought_", suffix), "aligned_drought_alt")
  setnames(ec_sub, paste0("aligned_flood_",   suffix), "aligned_flood_alt")
  setnames(ec_sub, paste0("aligned_heat_",    suffix), "aligned_heat_alt")
  out <- merge(copy(acu), ec_sub, by = c("codregion","year","month"), all.x = TRUE)
  # regional-mean FLOOD stress under alt rule
  out[, stress_flood_regmean_alt := as.integer(thr_flood_regmean_M == 1 & aligned_flood_alt == 1)]
  out[is.na(stress_flood_regmean_alt), stress_flood_regmean_alt := 0L]
  out[, nonenso_flood_regmean_alt := as.integer(thr_flood_regmean_M == 1 & (is.na(aligned_flood_alt) | aligned_flood_alt == 0))]
  out[, stress_heat_mine_alt := as.integer(thr_heat_mine == 1 & aligned_heat_alt == 1)]
  out[is.na(stress_heat_mine_alt), stress_heat_mine_alt := 0L]
  out[, nonenso_heat_mine_alt := as.integer(thr_heat_mine == 1 & (is.na(aligned_heat_alt) | aligned_heat_alt == 0))]
  out
}

panels_alt <- list(
  seasonal = build_alt_panel("seasonal", "seasonal"),
  spearman = build_alt_panel("spearman", "spearman"),
  phase    = build_alt_panel("phase",    "phase")
)

# ----- Re-fit acute headline regressions under each rule -----
fit_acute_alt <- function(panel, rule_label) {
  d <- panel[in_sample == TRUE]
  for (col in c("thr_flood_regmean_M","stress_flood_regmean_alt","nonenso_flood_regmean_alt",
                "thr_heat_mine","stress_heat_mine_alt","nonenso_heat_mine_alt")) {
    d[is.na(get(col)), (col) := 0L]
  }
  d[, mine := factor(mine_operation)]
  d[, calmonth_region := factor(sprintf("R%02d_M%02d", codregion, month))]
  d[, yr := factor(year)]
  d[, region_clust := factor(codregion)]
  rows <- list()
  # FLOOD regional-mean Spec B ENSO-attributable
  m <- feols(log_kt ~ nonenso_flood_regmean_alt + stress_flood_regmean_alt + log_price |
                mine + calmonth_region + yr, data = d, cluster = ~ region_clust, notes = FALSE)
  ct <- coeftable(m)
  rows[[1]] <- data.table(rule = rule_label, spec = "FLOOD_regmean_stress",
                          beta = unname(ct["stress_flood_regmean_alt","Estimate"]),
                          se   = unname(ct["stress_flood_regmean_alt","Std. Error"]),
                          p    = unname(ct["stress_flood_regmean_alt","Pr(>|t|)"]),
                          n    = nobs(m))
  # HEAT mine Spec B ENSO-attributable
  m2 <- feols(log_kt ~ nonenso_heat_mine_alt + stress_heat_mine_alt + log_price |
                mine + calmonth_region + yr, data = d, cluster = ~ region_clust, notes = FALSE)
  ct2 <- coeftable(m2)
  rows[[2]] <- data.table(rule = rule_label, spec = "HEAT_mine_stress",
                          beta = unname(ct2["stress_heat_mine_alt","Estimate"]),
                          se   = unname(ct2["stress_heat_mine_alt","Std. Error"]),
                          p    = unname(ct2["stress_heat_mine_alt","Pr(>|t|)"]),
                          n    = nobs(m2))
  rbindlist(rows)
}

cat("\n[43] Fitting acute headlines under each rule ...\n")
acute_rows <- list(
  baseline = NULL,  # to fill from existing tables
  seasonal = fit_acute_alt(panels_alt$seasonal, "seasonal-ONI"),
  spearman = fit_acute_alt(panels_alt$spearman, "spearman-rho"),
  phase    = fit_acute_alt(panels_alt$phase,    "phase-cond-Zbar")
)

# Pull baseline from existing 40 results
acu_base <- fread(file.path(TABLES, "40__regressions_acute.csv"))
acute_rows$baseline <- rbindlist(list(
  acu_base[label == "FLOOD_regmean_B",
           .(rule = "baseline", spec = "FLOOD_regmean_stress", beta, se, p = p_cr0, n = n_obs)],
  acu_base[label == "HEAT_mine_B_stress",
           .(rule = "baseline", spec = "HEAT_mine_stress",      beta, se, p = p_cr0, n = n_obs)]
))

acute_compare <- rbindlist(acute_rows, fill = TRUE)
acute_compare[, rule := factor(rule, levels = c("baseline","seasonal-ONI","spearman-rho","phase-cond-Zbar"))]
setorder(acute_compare, spec, rule)

cat("\n[43] ACUTE HEADLINE COMPARISON:\n")
print(acute_compare)

# ----- DROUGHT annual headlines under each rule -----
# For DROUGHT we need to rebuild the 3-yr cumulative ENSO-attributable share using
# the alternative alignment.  Build a mine-month drought share under each rule, then
# annual share, then 3-yr cumulative, then re-fit.

build_drought_alt_panel <- function(suffix) {
  cols_to_pull <- c("codregion","year","month", paste0("aligned_drought_", suffix))
  ec_sub <- ec[, ..cols_to_pull]
  setnames(ec_sub, paste0("aligned_drought_", suffix), "aligned_drought_alt")
  # spei12 threshold flag at mine level (already in master ledger)
  led <- as.data.table(read_parquet(file.path(LEDGER, "20__master_stress_ledger.parquet")))
  led_dr <- led[channel == "DROUGHT_meteo",
                .(mine_operation, codregion, date, year, month,
                  thr_meteo = threshold_crossed)]
  led_dr <- merge(led_dr, ec_sub, by = c("codregion","year","month"), all.x = TRUE)
  led_dr[is.na(aligned_drought_alt), aligned_drought_alt := 0L]
  led_dr[, str_meteo_alt := as.integer(thr_meteo == 1 & aligned_drought_alt == 1)]
  # Annual share
  # FIX (2026-05-30): match the corrected headline construction -- require a
  # full year of valid SPEI months, and use a COMPLETE 3-lag window (na.rm=FALSE).
  yr_share <- led_dr[, .(n_valid = sum(!is.na(thr_meteo)),
                         str_share_meteo_alt = mean(str_meteo_alt, na.rm = TRUE),
                         n_months = .N), by = .(mine_operation, codregion, year)]
  yr_share[n_valid < 12, str_share_meteo_alt := NA_real_]
  setorder(yr_share, mine_operation, year)
  yr_share[, str_lag1 := shift(str_share_meteo_alt, 1), by = mine_operation]
  yr_share[, str_lag2 := shift(str_share_meteo_alt, 2), by = mine_operation]
  yr_share[, str_lag3 := shift(str_share_meteo_alt, 3), by = mine_operation]
  yr_share[, cum3_str_meteo_alt := rowMeans(.SD),
           .SDcols = c("str_lag1","str_lag2","str_lag3")]
  out <- merge(copy(ann), yr_share[, .(mine_operation, year, cum3_str_meteo_alt)],
               by = c("mine_operation","year"), all.x = TRUE)
  out <- merge(out, yp, by = "year", all.x = TRUE)
  out[, mine := factor(mine_operation)]
  out[, yr := factor(year)]
  out[, region_clust := factor(codregion)]
  out[in_sample == TRUE & !is.na(log_kt_year) & !is.na(log_price_year)]
}

fit_drought_alt <- function(panel, rule_label) {
  m <- try(feols(log_kt_year ~ cum3_str_meteo_alt + log_price_year | mine + yr,
                 data = panel, cluster = ~ region_clust, notes = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) return(data.table(rule = rule_label, spec = "DROUGHT_SPEI_cum3",
                                                  beta = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_))
  ct <- coeftable(m)
  data.table(rule = rule_label, spec = "DROUGHT_SPEI_cum3",
             beta = unname(ct["cum3_str_meteo_alt","Estimate"]),
             se   = unname(ct["cum3_str_meteo_alt","Std. Error"]),
             p    = unname(ct["cum3_str_meteo_alt","Pr(>|t|)"]),
             n    = nobs(m))
}

cat("\n[43] Fitting DROUGHT headline under each rule ...\n")
drought_rows <- list(
  baseline = NULL,
  seasonal = fit_drought_alt(build_drought_alt_panel("seasonal"), "seasonal-ONI"),
  spearman = fit_drought_alt(build_drought_alt_panel("spearman"), "spearman-rho"),
  phase    = fit_drought_alt(build_drought_alt_panel("phase"),    "phase-cond-Zbar")
)
# Baseline from script 41 output
dro_base <- fread(file.path(TABLES, "41__regressions_drought.csv"))
drought_rows$baseline <- dro_base[label == "cum3_stress_SPEI_meteo" & fe == "mineYR",
                                  .(rule = "baseline", spec = "DROUGHT_SPEI_cum3",
                                    beta, se, p = p_cr0, n = n_obs)]
drought_compare <- rbindlist(drought_rows, fill = TRUE)
drought_compare[, rule := factor(rule, levels = c("baseline","seasonal-ONI","spearman-rho","phase-cond-Zbar"))]
setorder(drought_compare, rule)

cat("\n[43] DROUGHT HEADLINE COMPARISON:\n")
print(drought_compare)

# ----- Write the compact comparison table -----
all_rows <- rbindlist(list(acute_compare, drought_compare), fill = TRUE)
fwrite(all_rows, file.path(TABLES, "43__enso_robustness.csv"))
cat("\n[43] Wrote tables/43__enso_robustness.csv\n")
