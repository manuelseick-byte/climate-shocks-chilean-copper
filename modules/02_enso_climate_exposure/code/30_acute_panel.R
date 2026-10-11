# =============================================================================
# 30_acute_panel.R
#
# Build the mine-month acute regression panel for FLOOD and HEAT.
# Sample: mine-months with copper_kt > 0 (active operating months) intersected
# with the LME copper price series (2006-03 onwards).
#
# Columns include:
#   - log_kt, log_price
#   - thr_flood_mine, thr_flood_regmean_M (descriptive)
#   - thr_heat_mine
#   - stress_flood_mine, stress_flood_regmean_M, stress_heat_mine
#   - nonenso_flood_mine, nonenso_flood_regmean_M, nonenso_heat_mine   (decomposition)
#   - ENSO event-window labels for leave-one-event-out
# =============================================================================

source(file.path("code", "00_setup.R"))

ledger <- as.data.table(read_parquet(file.path(LEDGER, "20__master_stress_ledger.parquet")))

# Pivot the acute channels back to wide for the regression
# (FLOOD_corridor dropped 2026-05-31 -- see registry/channel_dictionary.csv)
acute_chans <- c("FLOOD_mine", "FLOOD_regmean_M", "HEAT_mine")
acu <- ledger[channel %in% acute_chans,
              .(mine_operation, codregion, region_en, date, year, month,
                copper_kt, log_kt,
                channel, threshold_crossed, enso_aligned, stress_flag,
                physical_value, oni_value, oni_phase, rho_region_month, z_anomaly_value)]

# Wide reshape
w <- dcast(acu, mine_operation + codregion + region_en + date + year + month + copper_kt + log_kt +
                 oni_value + oni_phase ~ channel,
           value.var = c("threshold_crossed", "enso_aligned", "stress_flag"))

setnames(w,
  c("threshold_crossed_FLOOD_mine",     "threshold_crossed_FLOOD_regmean_M","threshold_crossed_HEAT_mine",
    "enso_aligned_FLOOD_mine",          "enso_aligned_FLOOD_regmean_M",     "enso_aligned_HEAT_mine",
    "stress_flag_FLOOD_mine",           "stress_flag_FLOOD_regmean_M",      "stress_flag_HEAT_mine"),
  c("thr_flood_mine",      "thr_flood_regmean_M",  "thr_heat_mine",
    "aligned_flood_mine",  "aligned_flood_regmean_M","aligned_heat_mine",
    "stress_flood_mine",   "stress_flood_regmean_M","stress_heat_mine")
)

# Spec-B decomposition: non-ENSO threshold = threshold AND not ENSO-aligned
w[, nonenso_flood_mine     := as.integer(thr_flood_mine == 1     & aligned_flood_mine == 0)]
w[, nonenso_flood_regmean_M:= as.integer(thr_flood_regmean_M == 1 & aligned_flood_regmean_M == 0)]
w[, nonenso_heat_mine      := as.integer(thr_heat_mine == 1      & aligned_heat_mine == 0)]

# Copper price (LME, monthly USD/mt) — long series back to 2006-03
DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")
cp <- fread(file.path(DATA_RAW, "BCCh", "copper_lme_monthly_usd_per_mt.csv"))
cp[, date := as.Date(paste0(date, "-01"))]
cp[, log_price := log(price_usd_mt)]
cp <- cp[, .(date, log_price)]

w <- merge(w, cp, by = "date", all.x = TRUE)

# Calendar-month index for region x calendar-month FE (seasonal)
w[, calmonth := factor(sprintf("R%02d_M%02d", codregion, month))]

# Active sample = copper_kt > 0 AND log_price available
w[, in_sample := !is.na(copper_kt) & copper_kt > 0 & !is.na(log_price)]

# ENSO event-window label for leave-one-event-out
w[, event_label := "OTHER"]
for (e in ENSO_EVENT_WINDOWS) {
  w[date >= e$start & date <= e$end, event_label := e$label]
}

setorder(w, mine_operation, date)
write_parquet(w, file.path(PANELS, "30__acute_mine_month.parquet"))
fwrite(w[in_sample == TRUE,
         .(mine_operation, codregion, region_en, date, year, month,
           copper_kt, log_kt, log_price, oni_value, oni_phase, event_label,
           thr_flood_mine, thr_flood_regmean_M, thr_heat_mine,
           aligned_flood_mine, aligned_flood_regmean_M, aligned_heat_mine,
           stress_flood_mine, stress_flood_regmean_M, stress_heat_mine,
           nonenso_flood_mine, nonenso_flood_regmean_M, nonenso_heat_mine)],
       file.path(PANELS, "30__acute_mine_month_active.csv"))

cat("[30] acute panel: rows =", nrow(w),
    "active =", sum(w$in_sample, na.rm = TRUE),
    "range =", as.character(min(w$date[w$in_sample])),
    "to",     as.character(max(w$date[w$in_sample])), "\n")

# Summaries
cat("[30] active-sample event window counts:\n")
print(w[in_sample == TRUE, .N, by = event_label])
cat("[30] active-sample stress firing rates by channel:\n")
fr <- w[in_sample == TRUE, .(
  flood_mine_thr   = round(100 * mean(thr_flood_mine), 2),
  flood_mine_str   = round(100 * mean(stress_flood_mine), 2),
  flood_rmM_thr    = round(100 * mean(thr_flood_regmean_M), 2),
  heat_mine_thr    = round(100 * mean(thr_heat_mine), 2),
  heat_mine_str    = round(100 * mean(stress_heat_mine), 2)
)]
print(fr)
fwrite(fr, file.path(TABLES, "30__active_sample_firing.csv"))
