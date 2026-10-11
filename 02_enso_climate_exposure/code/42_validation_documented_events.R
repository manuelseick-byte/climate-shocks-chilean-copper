# =============================================================================
# 42_validation_documented_events.R
#
# Test the FLOOD stress signal against documented Chilean flood-disruption
# events from the engineering / climate literature.  For each event cell
# (region, year, month) we report whether the threshold fires, whether the
# ENSO attribution fires, and whether the AND-stress flag fires, under
# the two FLOOD definitions used in the report (mine-cell and the
# regional-mean variant).
#
# Output: tables/42__documented_events_recovery.csv  (per cell)
#         tables/42__recovery_rate_summary.csv       (per definition)
# =============================================================================

source(file.path("code", "00_setup.R"))

ledger <- as.data.table(read_parquet(file.path(LEDGER, "20__master_stress_ledger.parquet")))
mines  <- fread(file.path(REGISTRY, "mines.csv"))

# Build the documented-event ledger.  These are canonical Chilean flood-disruption
# episodes 2006-2025 drawn from Wilcox et al. 2016 GRL, news archives, and the
# CR2 Explorer station network for 2023 events.  Cells use region codes.
docs <- data.table(
  event_label = c(
    "Mar 2015 Atacama aluvion", "Apr 2015 Atacama spillover", "Mar 2015 Coquimbo spillover",
    "Apr 2017 Cajon del Maipo I", "May 2017 Cajon del Maipo II",
    "Apr 2017 Coquimbo cluster", "Apr 2017 OHiggins cluster",
    "Feb 2017 Atacama micro-storm",
    "Feb 2023 Atacama Coquimbo", "Feb 2023 Atacama Coquimbo (Atacama)",
    "Jun 2023 atm river Valparaiso", "Jun 2023 atm river OHiggins",
    "Jun 2023 atm river Maule (extra)", "Jun 2023 atm river RM",
    "Aug 2023 floods Valparaiso", "Aug 2023 floods OHiggins",
    "Aug 2023 floods Maule (extra)", "Aug 2023 floods RM",
    "Aug 2015 central Chile rainfall", "Apr 2024 Maipo cluster"
  ),
  codregion = c(
    3, 3, 4,
    13, 13,
    4, 6,
    3,
    4, 3,
    5, 6, 6, 13,
    5, 6, 6, 13,
    13, 13
  ),
  year = c(
    2015, 2015, 2015,
    2017, 2017,
    2017, 2017,
    2017,
    2023, 2023,
    2023, 2023, 2023, 2023,
    2023, 2023, 2023, 2023,
    2015, 2024
  ),
  month = c(
    3, 4, 3,
    4, 5,
    4, 4,
    2,
    2, 2,
    6, 6, 6, 6,
    8, 8, 8, 8,
    8, 4
  )
)

# Pull one row per (region, year, month, channel) — use the first mine in that region
# (alignment-related fields don't vary across mines within region; physical_value/threshold
# at mine cell does, so for the per-cell summary we take a representative high-elevation mine)
lg <- ledger[channel %in% c("FLOOD_mine", "FLOOD_regmean_M")]

# For FLOOD_mine, the threshold value depends on the mine.  Report ALL mines per region.
# For the regional-mean variant the value is region-wide so we keep one row per (region, date).
recovery_mine <- merge(docs, lg[channel == "FLOOD_mine"],
                       by = c("codregion", "year", "month"), allow.cartesian = TRUE)

# Compute per-event, per-channel summary
recovery_mine_summary <- recovery_mine[, .(
  event_label = unique(event_label),
  channel     = "FLOOD_mine",
  any_threshold = as.integer(any(threshold_crossed == 1, na.rm = TRUE)),
  any_aligned   = as.integer(any(enso_aligned == 1, na.rm = TRUE)),
  any_stress    = as.integer(any(stress_flag == 1, na.rm = TRUE)),
  n_mines       = .N,
  max_pr_mm     = max(physical_value, na.rm = TRUE)
), by = .(codregion, year, month)]

recovery_rmM <- lg[channel == "FLOOD_regmean_M"][docs, on = c("codregion", "year", "month")]
recovery_rmM <- unique(recovery_rmM[, .(codregion, year, month, event_label,
                                        channel = "FLOOD_regmean_M",
                                        any_threshold = threshold_crossed,
                                        any_aligned = enso_aligned,
                                        any_stress = stress_flag,
                                        n_mines = NA_integer_,
                                        max_pr_mm = physical_value)])

rec_all <- rbindlist(list(recovery_mine_summary, recovery_rmM), fill = TRUE)
setorder(rec_all, channel, codregion, year, month)
fwrite(rec_all, file.path(TABLES, "42__documented_events_recovery.csv"))

# Per-definition recovery rate
summary_tbl <- rec_all[, .(
  n_events         = .N,
  n_thr_fires      = sum(any_threshold == 1, na.rm = TRUE),
  n_aligned_fires  = sum(any_aligned   == 1, na.rm = TRUE),
  n_stress_fires   = sum(any_stress    == 1, na.rm = TRUE),
  pct_thr          = round(100 * mean(any_threshold == 1, na.rm = TRUE), 1),
  pct_aligned      = round(100 * mean(any_aligned   == 1, na.rm = TRUE), 1),
  pct_stress       = round(100 * mean(any_stress    == 1, na.rm = TRUE), 1)
), by = channel]
fwrite(summary_tbl, file.path(TABLES, "42__recovery_rate_summary.csv"))

cat("[42] Documented-events recovery summary\n")
print(summary_tbl)
cat("\n[42] Per-event detail:\n")
print(rec_all)
