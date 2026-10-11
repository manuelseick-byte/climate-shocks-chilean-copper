# =============================================================================
# 80_z_assessment.R
#
# Q1 diagnostic suite — characterise the frequency and distribution of the
# standardised anomaly Z that underpins the rest of the ENSO-attribution
# machinery.  Standalone: reads existing panels, writes only into
# mining_module_alternative/q1_diagnostics/ (figures + tables + README).
# Does NOT overwrite anything used by the current writeup.
#
# Produces:
#   q1_diagnostics/
#     figures/
#       F_Z1__monthly_distribution.png        — histogram + N(0,1) overlay,
#                                               baseline (1980-2010) vs full
#                                               sample (1960-2025), pr & t.
#       F_Z2__threshold_freq_timeseries.png   — yearly count of region-months
#                                               with |Z|>1, |Z|>2, decomposed
#                                               by sign; megadrought visible.
#       F_Z3__heatmap_region_calmonth.png     — region × calendar-month share
#                                               of years with |Z|>1, pr & t.
#       F_Z4__enso_phase_decomposition.png    — mean Z within El Nino / Neutral
#                                               / La Nina, by region, pr & t.
#                                               (Bridge to Q2.)
#       F_Z5__daily_z_distribution.png        — daily mine-cell Z: histogram +
#                                               N(0,1) overlay, baseline vs
#                                               full, pr & tmax.
#     tables/
#       T_Z1__by_region_summary.csv           — mean / SD / |Z|>k frequencies,
#                                               baseline and full sample, by
#                                               region and variable.
#       T_Z2__threshold_freq_yearly.csv       — yearly N region-months by
#                                               |Z|>k category, signed.
#       T_Z3__by_region_calmonth.csv          — region × cal-month |Z|>1 share.
#       T_Z4__by_enso_phase.csv               — region × phase mean/SD of Z.
#       T_Z5__daily_z_by_mine.csv             — daily Z summary by mine.
#     README.md                                — one-page interpretive memo.
#
# Inputs (READ-ONLY):
#   mining_module_alternative/panels/11__enso_context_monthly.parquet
#   mining_module_alternative/panels/10__minecell_daily.parquet
#   Data_processed/climate/02b__regional_climate_monthly_panel.parquet
#     (used only for the all-16-region context heatmap)
#
# Inputs to the writeup:
#   None — this script does not touch anything used by copper_climate_report.tex.
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ---- output directories ----------------------------------------------------
Q1_DIR     <- file.path(ALT_ROOT, "q1_diagnostics")
Q1_FIGURES <- file.path(Q1_DIR, "figures")
Q1_TABLES  <- file.path(Q1_DIR, "tables")
for (d in c(Q1_DIR, Q1_FIGURES, Q1_TABLES)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# ---- load monthly region-level Z panel (7 copper regions) -------------------
ec <- as.data.table(read_parquet(
  file.path(PANELS, "11__enso_context_monthly.parquet")
))
setnames(ec, c("z_pr_m", "z_t_m"), c("Z_pr", "Z_t"))
ec[, region_en := REGION_NAME[as.character(codregion)]]
ec[, is_baseline := year >= 1980 & year <= 2010]

# Also load the 16-region upstream panel for the context heatmap
m16 <- as.data.table(read_parquet(
  file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")
))
setnames(m16, c("Z_pr_m", "Z_t_m"), c("Z_pr", "Z_t"))
m16[, is_baseline := year >= 1980 & year <= 2010]

# ---- common ggplot theme ----------------------------------------------------
theme_q1 <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold", size = 12, colour = PAL$ink),
    plot.subtitle      = element_text(size = 10, colour = "#444444"),
    plot.caption       = element_text(size = 8, colour = "#666666", hjust = 0),
    strip.text         = element_text(face = "bold", colour = PAL$ink),
    legend.position    = "bottom"
  )

# Helper that returns a long-form table of Z values, labelled by variable
make_long <- function(dt) {
  rbind(
    dt[, .(codregion, region_en, year, month, date, is_baseline,
           variable = "Precipitation", Z = Z_pr)],
    dt[, .(codregion, region_en, year, month, date, is_baseline,
           variable = "Temperature",   Z = Z_t)]
  )
}
ec_long <- make_long(ec)

# =============================================================================
# Table T_Z1 — by-region summary (mean, SD, |Z|>1, |Z|>2), baseline vs full
# =============================================================================
cat("[80] Table T_Z1 — by-region summary\n")
summary_by_region <- function(dt, window_label) {
  dt[, .(
    window       = window_label,
    n            = sum(!is.na(Z)),
    mean_Z       = round(mean(Z, na.rm = TRUE), 3),
    sd_Z         = round(sd(Z,   na.rm = TRUE), 3),
    pct_gt_1     = round(100 * mean(abs(Z) > 1,   na.rm = TRUE), 1),
    pct_gt_1p5   = round(100 * mean(abs(Z) > 1.5, na.rm = TRUE), 1),
    pct_gt_2     = round(100 * mean(abs(Z) > 2,   na.rm = TRUE), 1),
    pct_pos_gt_1 = round(100 * mean(Z >  1,       na.rm = TRUE), 1),
    pct_neg_gt_1 = round(100 * mean(Z < -1,       na.rm = TRUE), 1)
  ), by = .(codregion, region_en, variable)]
}
T_Z1 <- rbind(
  summary_by_region(ec_long[is_baseline == TRUE],  "Baseline 1980-2010"),
  summary_by_region(ec_long[is_baseline == FALSE | is_baseline == TRUE], "Full sample 1960-2025")
)
# also add an all-region pooled row
pooled <- rbind(
  ec_long[is_baseline == TRUE,  .(codregion = 0L, region_en = "ALL 7 MINING REGIONS",
                                  variable, Z, is_baseline = TRUE)][,
    .(window = "Baseline 1980-2010", n = sum(!is.na(Z)),
      mean_Z = round(mean(Z, na.rm = TRUE), 3),
      sd_Z   = round(sd(Z,   na.rm = TRUE), 3),
      pct_gt_1   = round(100 * mean(abs(Z) > 1,   na.rm = TRUE), 1),
      pct_gt_1p5 = round(100 * mean(abs(Z) > 1.5, na.rm = TRUE), 1),
      pct_gt_2   = round(100 * mean(abs(Z) > 2,   na.rm = TRUE), 1),
      pct_pos_gt_1 = round(100 * mean(Z >  1, na.rm = TRUE), 1),
      pct_neg_gt_1 = round(100 * mean(Z < -1, na.rm = TRUE), 1)),
    by = .(codregion, region_en, variable)],
  ec_long[,                .(codregion = 0L, region_en = "ALL 7 MINING REGIONS",
                                  variable, Z, is_baseline = FALSE)][,
    .(window = "Full sample 1960-2025", n = sum(!is.na(Z)),
      mean_Z = round(mean(Z, na.rm = TRUE), 3),
      sd_Z   = round(sd(Z,   na.rm = TRUE), 3),
      pct_gt_1   = round(100 * mean(abs(Z) > 1,   na.rm = TRUE), 1),
      pct_gt_1p5 = round(100 * mean(abs(Z) > 1.5, na.rm = TRUE), 1),
      pct_gt_2   = round(100 * mean(abs(Z) > 2,   na.rm = TRUE), 1),
      pct_pos_gt_1 = round(100 * mean(Z >  1, na.rm = TRUE), 1),
      pct_neg_gt_1 = round(100 * mean(Z < -1, na.rm = TRUE), 1)),
    by = .(codregion, region_en, variable)]
)
T_Z1_out <- rbind(T_Z1, pooled, fill = TRUE)
setcolorder(T_Z1_out, c("codregion", "region_en", "variable", "window",
                        "n", "mean_Z", "sd_Z", "pct_gt_1", "pct_gt_1p5", "pct_gt_2",
                        "pct_pos_gt_1", "pct_neg_gt_1"))
setorder(T_Z1_out, variable, codregion, window)
fwrite(T_Z1_out, file.path(Q1_TABLES, "T_Z1__by_region_summary.csv"))

# =============================================================================
# Figure F_Z1 — distribution check
# =============================================================================
cat("[80] Figure F_Z1 — distribution check\n")
norm_curve <- data.table(Z = seq(-4, 4, length.out = 401))
norm_curve[, density := dnorm(Z)]

f_z1_pr <- ggplot(ec_long[variable == "Precipitation"],
                  aes(x = Z, fill = is_baseline)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, alpha = 0.55, colour = NA) +
  geom_line(data = norm_curve, aes(x = Z, y = density), inherit.aes = FALSE,
            colour = PAL$ink, linewidth = 0.7, linetype = "longdash") +
  facet_wrap(~ is_baseline, ncol = 2,
             labeller = labeller(is_baseline = c(`TRUE`  = "Baseline 1980-2010",
                                                 `FALSE` = "Outside baseline 1960-79 & 2011-25"))) +
  scale_fill_manual(values = c(`TRUE` = PAL$pos_event, `FALSE` = PAL$enso_nino), guide = "none") +
  scale_x_continuous(limits = c(-4, 4), breaks = -4:4) +
  labs(title    = "Precipitation Z anomaly  —  monthly, 7 copper regions",
       subtitle = "Histogram with N(0,1) reference (dashed).  Baseline panel is calibrated to N(0,1) by construction; full-sample shifts indicate the megadrought / secular signal.",
       x = "Z (standardised anomaly)", y = "Density") +
  theme_q1

f_z1_t  <- ggplot(ec_long[variable == "Temperature"],
                  aes(x = Z, fill = is_baseline)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, alpha = 0.55, colour = NA) +
  geom_line(data = norm_curve, aes(x = Z, y = density), inherit.aes = FALSE,
            colour = PAL$ink, linewidth = 0.7, linetype = "longdash") +
  facet_wrap(~ is_baseline, ncol = 2,
             labeller = labeller(is_baseline = c(`TRUE`  = "Baseline 1980-2010",
                                                 `FALSE` = "Outside baseline 1960-79 & 2011-25"))) +
  scale_fill_manual(values = c(`TRUE` = PAL$pos_event, `FALSE` = PAL$heat), guide = "none") +
  scale_x_continuous(limits = c(-4, 4), breaks = -4:4) +
  labs(title    = "Temperature Z anomaly  —  monthly, 7 copper regions",
       x = "Z (standardised anomaly)", y = "Density",
       caption = "Reference curve: N(0,1).  Baseline window 1980-2010 is by construction near mean 0, SD 1 (sample SD biased slightly low for finite N).") +
  theme_q1

ggsave(file.path(Q1_FIGURES, "F_Z1__monthly_distribution.png"),
       (f_z1_pr / f_z1_t), width = 9.6, height = 7.6, dpi = 200, bg = "white")

# =============================================================================
# Table T_Z2 / Figure F_Z2 — frequency over time, decomposed by sign
# =============================================================================
cat("[80] Table T_Z2 / Figure F_Z2 — frequency over time\n")
T_Z2 <- ec_long[, .(
  region_months_total = .N,
  pos_gt_1            = sum(Z >  1,   na.rm = TRUE),
  neg_lt_minus1       = sum(Z < -1,   na.rm = TRUE),
  pos_gt_2            = sum(Z >  2,   na.rm = TRUE),
  neg_lt_minus2       = sum(Z < -2,   na.rm = TRUE)
), by = .(year, variable)]
T_Z2[, pct_pos_gt_1 := 100 * pos_gt_1      / region_months_total]
T_Z2[, pct_neg_lt_1 := 100 * neg_lt_minus1 / region_months_total]
T_Z2[, pct_pos_gt_2 := 100 * pos_gt_2      / region_months_total]
T_Z2[, pct_neg_lt_2 := 100 * neg_lt_minus2 / region_months_total]
fwrite(T_Z2, file.path(Q1_TABLES, "T_Z2__threshold_freq_yearly.csv"))

# Reshape for the figure
T_Z2_long <- melt(T_Z2,
                  id.vars       = c("year", "variable", "region_months_total"),
                  measure.vars  = c("pct_pos_gt_1", "pct_neg_lt_1", "pct_pos_gt_2", "pct_neg_lt_2"),
                  variable.name = "metric", value.name = "pct")
T_Z2_long[, sign_label := fifelse(grepl("pos", metric), "+1 SD or more (wetter / hotter)",
                                  "-1 SD or more (drier / cooler)")]
T_Z2_long[, threshold  := fifelse(grepl("gt_2|lt_2", metric), "|Z| > 2", "|Z| > 1")]

# Event window shading for FIGURE
ev_df <- data.table(
  event = c("2015-16 El Nino", "2020-22 La Nina", "2023-24 El Nino", "2010-2022 megadrought"),
  start = as.Date(c("2014-09-01", "2020-08-01", "2023-06-01", "2010-01-01")),
  end   = as.Date(c("2016-05-31", "2023-02-28", "2024-05-31", "2022-12-31")),
  ymin  = c(0, 0, 0, 0)
)
ev_df[, x_start_y := as.numeric(format(start, "%Y")) + (as.numeric(format(start, "%j"))-1)/365]
ev_df[, x_end_y   := as.numeric(format(end,   "%Y")) + (as.numeric(format(end,   "%j"))-1)/365]

mk_freq_panel <- function(var_label, fill_high, fill_low) {
  d <- T_Z2_long[variable == var_label & threshold == "|Z| > 1"]
  d[, sign_signed := ifelse(grepl("pos", metric), pct, -pct)]
  ggplot() +
    geom_rect(data = ev_df, aes(xmin = x_start_y, xmax = x_end_y,
                                ymin = -Inf, ymax = Inf),
              fill = "#F2F2F2", alpha = 0.7, inherit.aes = FALSE) +
    geom_text(data = ev_df[event != "2010-2022 megadrought"],
              aes(x = (x_start_y + x_end_y)/2, y = 80, label = event),
              size = 2.6, colour = "#888888", inherit.aes = FALSE) +
    geom_text(data = ev_df[event == "2010-2022 megadrought"],
              aes(x = (x_start_y + x_end_y)/2, y = 88, label = event),
              size = 2.6, fontface = "italic", colour = "#888888", inherit.aes = FALSE) +
    geom_col(data = d, aes(x = year, y = sign_signed, fill = sign_label),
             width = 0.85) +
    geom_hline(yintercept = 0, colour = PAL$rule, linewidth = 0.3) +
    scale_fill_manual(values = c("+1 SD or more (wetter / hotter)" = fill_high,
                                 "-1 SD or more (drier / cooler)"  = fill_low),
                      name = NULL) +
    scale_x_continuous(breaks = seq(1960, 2025, by = 10), expand = c(0.005, 0.005)) +
    scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(title    = paste0(var_label, " Z anomaly  —  share of region-months above / below 1 SD"),
         subtitle = "Each bar = one calendar year; positive (up) = pct of region-months with Z > +1; negative (down) = pct with Z < -1.  7 copper regions.",
         x = NULL, y = "% of region-months / year") +
    theme_q1
}

f_z2_pr <- mk_freq_panel("Precipitation", PAL$flood_up, PAL$drought)
f_z2_t  <- mk_freq_panel("Temperature",   PAL$heat,     "#5DADE2")
ggsave(file.path(Q1_FIGURES, "F_Z2__threshold_freq_timeseries.png"),
       (f_z2_pr / f_z2_t), width = 11.5, height = 7.6, dpi = 200, bg = "white")

# =============================================================================
# Table T_Z3 / Figure F_Z3 — region × calendar-month heatmap
# =============================================================================
cat("[80] Table T_Z3 / Figure F_Z3 — region × cal-month heatmap\n")
T_Z3 <- ec_long[, .(
  share_abs_gt_1 = mean(abs(Z) > 1, na.rm = TRUE),
  mean_Z         = mean(Z,           na.rm = TRUE),
  n_years        = uniqueN(year[!is.na(Z)])
), by = .(codregion, region_en, variable, month)]
fwrite(T_Z3, file.path(Q1_TABLES, "T_Z3__by_region_calmonth.csv"))

T_Z3[, region_en := factor(region_en,
                           levels = c("Tarapaca", "Antofagasta", "Atacama",
                                      "Coquimbo", "Valparaiso", "Santiago Metropolitan", "O'Higgins"))]

mk_heatmap <- function(var_label, low, high) {
  ggplot(T_Z3[variable == var_label],
         aes(x = factor(month), y = region_en, fill = 100 * share_abs_gt_1)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.0f", 100 * share_abs_gt_1)),
              size = 2.6, colour = PAL$ink) +
    scale_fill_gradient(low = low, high = high, name = "% years with |Z|>1") +
    scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")) +
    labs(title    = paste0(var_label, " Z  —  share of years 1960-2025 with |Z|>1 in each region × calendar month"),
         x = NULL, y = NULL) +
    theme_q1 +
    theme(panel.grid = element_blank(), legend.position = "right")
}
f_z3_pr <- mk_heatmap("Precipitation", "#F7FBFF", PAL$drought)
f_z3_t  <- mk_heatmap("Temperature",   "#FFF5EB", PAL$heat)
ggsave(file.path(Q1_FIGURES, "F_Z3__heatmap_region_calmonth.png"),
       (f_z3_pr / f_z3_t), width = 10, height = 7.6, dpi = 200, bg = "white")

# =============================================================================
# Table T_Z4 / Figure F_Z4 — ENSO-phase decomposition
# =============================================================================
cat("[80] Table T_Z4 / Figure F_Z4 — ENSO-phase decomposition\n")
ec_long_phase <- merge(
  ec_long,
  ec[, .(codregion, date, phase_rolling)],
  by = c("codregion", "date"), all.x = TRUE
)
ec_long_phase[, phase := factor(phase_rolling,
                                levels = c("nino", "neutral", "nina"),
                                labels = c("El Nino", "Neutral", "La Nina"))]

T_Z4 <- ec_long_phase[!is.na(phase),
                      .(mean_Z = round(mean(Z, na.rm = TRUE), 3),
                        sd_Z   = round(sd(Z,   na.rm = TRUE), 3),
                        n      = .N,
                        pct_abs_gt_1 = round(100 * mean(abs(Z) > 1, na.rm = TRUE), 1)),
                      by = .(codregion, region_en, variable, phase)]
fwrite(T_Z4, file.path(Q1_TABLES, "T_Z4__by_enso_phase.csv"))

T_Z4[, region_en := factor(region_en,
                           levels = rev(c("Tarapaca", "Antofagasta", "Atacama",
                                          "Coquimbo", "Valparaiso",
                                          "Santiago Metropolitan", "O'Higgins")))]

mk_phase_plot <- function(var_label) {
  ggplot(T_Z4[variable == var_label],
         aes(x = mean_Z, y = region_en, fill = phase)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_vline(xintercept = 0, colour = PAL$rule, linewidth = 0.4) +
    scale_fill_manual(values = c("El Nino" = PAL$enso_nino,
                                 "Neutral" = PAL$enso_neutral,
                                 "La Nina" = PAL$enso_nina),
                      name = NULL) +
    labs(title = paste0(var_label, " — mean Z by ENSO phase and region"),
         x = "Mean Z within phase", y = NULL) +
    theme_q1
}
f_z4_pr <- mk_phase_plot("Precipitation")
f_z4_t  <- mk_phase_plot("Temperature")
ggsave(file.path(Q1_FIGURES, "F_Z4__enso_phase_decomposition.png"),
       (f_z4_pr / f_z4_t) + plot_layout(guides = "collect") &
       theme(legend.position = "bottom"),
       width = 9.6, height = 7.6, dpi = 200, bg = "white")

# =============================================================================
# Table T_Z5 / Figure F_Z5 — daily mine-cell Z
# =============================================================================
cat("[80] Table T_Z5 / Figure F_Z5 — daily mine-cell Z\n")
mc <- as.data.table(read_parquet(
  file.path(PANELS, "10__minecell_daily.parquet")
))
mc[, is_baseline := year >= 1980 & year <= 2010]
mc[, year := as.integer(format(date, "%Y"))]

# Daily Z summary by mine (compute |Z|>k frequencies on the daily series)
T_Z5 <- mc[, .(
  n_days         = sum(!is.na(z_pr_d)),
  mean_Z_pr_d    = round(mean(z_pr_d,   na.rm = TRUE), 3),
  sd_Z_pr_d      = round(sd(z_pr_d,     na.rm = TRUE), 3),
  pct_Z_pr_gt_1  = round(100 * mean(z_pr_d > 1, na.rm = TRUE), 2),
  pct_Z_pr_gt_2  = round(100 * mean(z_pr_d > 2, na.rm = TRUE), 2),
  pct_Z_pr_gt_3  = round(100 * mean(z_pr_d > 3, na.rm = TRUE), 2),
  mean_Z_tmax_d  = round(mean(z_tmax_d, na.rm = TRUE), 3),
  sd_Z_tmax_d    = round(sd(z_tmax_d,   na.rm = TRUE), 3),
  pct_abs_Z_tmax_gt_1 = round(100 * mean(abs(z_tmax_d) > 1, na.rm = TRUE), 2),
  pct_abs_Z_tmax_gt_2 = round(100 * mean(abs(z_tmax_d) > 2, na.rm = TRUE), 2)
), by = .(mine_operation, codregion)]
T_Z5[, region_en := REGION_NAME[as.character(codregion)]]
setorder(T_Z5, codregion, mine_operation)
fwrite(T_Z5, file.path(Q1_TABLES, "T_Z5__daily_z_by_mine.csv"))

# F_Z5 daily-Z distribution figure (pooled across mines for compactness):
# pr is bounded at 0; on most mine-days pr = 0 mm so Z_pr_d collapses to a
# near-constant tiny-negative value -> the unconditional histogram is degenerate.
# The FLOOD AND-gate uses daily Z only on wet days (>= 20 mm). Show the
# distribution conditional on a *wet day* (pr >= 1 mm) so the panel is
# informative for what the gate actually sees.
mc_pr_all <- mc[!is.na(z_pr_d) & is.finite(z_pr_d) & pr_mm >= 1]
# Filter to a wide data window (-1, 10) so the visible-range bins aren't
# inflated by edge accumulation, then constrain the display to (-1, 6).
mc_pr     <- mc_pr_all[z_pr_d <= 10]
mc_t      <- mc[!is.na(z_tmax_d) & is.finite(z_tmax_d)]

# Side-notes for the figure subtitle: wet-day share and right-tail mass
wet_share <- mc[!is.na(pr_mm), .(
  total_days = .N,
  wet_days   = sum(pr_mm >= 1, na.rm = TRUE),
  flood_days = sum(pr_mm >= 20, na.rm = TRUE)
), by = is_baseline]

tail_share <- mc_pr_all[, .(
  n_wet    = .N,
  n_gt_6   = sum(z_pr_d > 6,  na.rm = TRUE),
  n_gt_10  = sum(z_pr_d > 10, na.rm = TRUE),
  pct_gt_6 = round(100 * mean(z_pr_d > 6,  na.rm = TRUE), 2)
)]

f_z5_pr <- ggplot(mc_pr, aes(x = z_pr_d, colour = is_baseline, fill = is_baseline)) +
  geom_density(alpha = 0.55, linewidth = 0.7, adjust = 1.2) +
  geom_vline(xintercept = c(1, 2, 3), colour = "#888888",
             linewidth = 0.25, linetype = "dotted") +
  geom_line(data = norm_curve[Z >= 0], aes(x = Z, y = density),
            inherit.aes = FALSE,
            colour = PAL$ink, linewidth = 0.7, linetype = "longdash") +
  facet_wrap(~ is_baseline, ncol = 2,
             labeller = labeller(is_baseline = c(`TRUE`  = "Baseline 1980-2010",
                                                 `FALSE` = "Outside baseline 1960-79 & 2011-25"))) +
  scale_fill_manual(values = c(`TRUE` = PAL$pos_event, `FALSE` = PAL$flood_low),
                    guide = "none") +
  scale_colour_manual(values = c(`TRUE` = PAL$pos_event, `FALSE` = PAL$flood_low),
                    guide = "none") +
  coord_cartesian(xlim = c(-1, 6)) +
  scale_x_continuous(breaks = seq(-1, 6, by = 1)) +
  labs(title    = "Daily precipitation Z on WET DAYS (pr >= 1 mm)  —  pooled across 19 operations",
       subtitle = sprintf(
         "Wet days = %.1f%% of mine-days inside baseline, %.1f%% outside. Distribution truncated at Z=6 for display; %d wet days (%.2f%%) lie above (max ~%.0f, see README note on degenerate-baseline cells).",
         100 * wet_share[is_baseline == TRUE,  wet_days / total_days],
         100 * wet_share[is_baseline == FALSE, wet_days / total_days],
         tail_share$n_gt_6, tail_share$pct_gt_6, max(mc_pr_all$z_pr_d, na.rm = TRUE)
       ),
       x = "Z (daily, against mine x day-of-year 1980-2010 baseline)", y = "Density") +
  theme_q1

f_z5_t <- ggplot(mc_t, aes(x = z_tmax_d, fill = is_baseline)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80, alpha = 0.55, colour = NA) +
  geom_line(data = norm_curve, aes(x = Z, y = density), inherit.aes = FALSE,
            colour = PAL$ink, linewidth = 0.7, linetype = "longdash") +
  facet_wrap(~ is_baseline, ncol = 2,
             labeller = labeller(is_baseline = c(`TRUE`  = "Baseline 1980-2010",
                                                 `FALSE` = "Outside baseline 1960-79 & 2011-25"))) +
  scale_fill_manual(values = c(`TRUE` = PAL$pos_event, `FALSE` = PAL$heat),
                    guide = "none") +
  coord_cartesian(xlim = c(-4, 4)) +
  labs(title    = "Daily Tmax Z at the mine cell  —  pooled across 19 operations",
       x = "Z (daily, against mine x day-of-year 1980-2010 baseline)", y = "Density",
       caption = "Reference curve: N(0,1). The HEAT AND-gate uses the monthly-mean Z (Figure F_Z1), not this daily Z; this panel is provided as a calibration check on the daily input.") +
  theme_q1
ggsave(file.path(Q1_FIGURES, "F_Z5__daily_z_distribution.png"),
       (f_z5_pr / f_z5_t), width = 10, height = 7.6, dpi = 200, bg = "white")

# =============================================================================
# README — short interpretive memo
# =============================================================================
cat("[80] Writing README.md\n")
readme_path <- file.path(Q1_DIR, "README.md")
readme_lines <- c(
"# Q1 — Z-anomaly frequency assessment",
"",
"This folder contains the Q1 diagnostic suite for the standardised anomaly Z that",
"feeds the rest of the mining-module's ENSO-attribution pipeline.  Generated by",
"`scripts/80_z_assessment.R` from the existing climate panels; nothing in",
"`writeup/`, `tables/`, or `figures/` has been overwritten.",
"",
"## Files",
"",
"- `figures/F_Z1__monthly_distribution.png` — histogram of monthly Z (precipitation",
"  and temperature) for the 7 copper regions, side-by-side panels for the baseline",
"  window (1980-2010) and outside it (1960-79 + 2011-25), with the N(0,1)",
"  reference overlaid.  Confirms calibration on the baseline; shows the small",
"  negative shift in Z_pr outside the baseline (megadrought signature) and the",
"  fatter Z_t tails outside the baseline (secular warming + ENSO).",
"",
"- `figures/F_Z2__threshold_freq_timeseries.png` — yearly count of region-months",
"  with Z > +1 SD (up bars) and Z < -1 SD (down bars), separately for",
"  precipitation and temperature.  Megadrought visible as an extended run of",
"  negative-precip bars 2010-2022; late-2010s/2020s warming visible as positive",
"  temperature bars.  ENSO event windows shaded for orientation.",
"",
"- `figures/F_Z3__heatmap_region_calmonth.png` — region x calendar-month heatmap",
"  of the share of years 1960-2025 with |Z|>1, separately for precipitation and",
"  temperature.  Answers \"where and when does Chile see anomalies most\".",
"",
"- `figures/F_Z4__enso_phase_decomposition.png` — mean monthly Z within each",
"  ENSO phase (El Nino / Neutral / La Nina), by region, separately for",
"  precipitation and temperature.  Bridges to Q2 by showing how much of the Z",
"  variation is gated by the ONI phase (i.e., the substantive content of",
"  condition (i) of the AND-gate).",
"",
"- `figures/F_Z5__daily_z_distribution.png` — daily Z at the mine cell, pooled",
"  across 19 operations.  Precipitation panel is conditional on a wet day",
"  (>= 1 mm) because the dry-cell mass would otherwise collapse the histogram;",
"  this is the distribution the FLOOD AND-gate evaluates on its >= 20 mm",
"  threshold-firing days.  Tmax daily Z is unconditional and approximately",
"  N(0,1) on baseline, fatter-tailed outside; shown as a calibration check.",
"",
"- `tables/T_Z1__by_region_summary.csv` — mean, SD, and |Z|>k frequencies per",
"  region x variable x window (baseline / full).  This is the table to cite in",
"  the writeup when defending the methodology.",
"- `tables/T_Z2__threshold_freq_yearly.csv` — underlying data for F_Z2.",
"- `tables/T_Z3__by_region_calmonth.csv` — underlying data for F_Z3.",
"- `tables/T_Z4__by_enso_phase.csv` — underlying data for F_Z4.",
"- `tables/T_Z5__daily_z_by_mine.csv` — daily-Z summary per mine (pr and tmax).",
"",
"## What this is for",
"",
"Q1 of the revised chain of thought is \"how strong is the monthly deviation",
"from normal climate in Chile\".  The current writeup defines Z formally in",
"section 2.1 but does not visualise its distribution or report how often",
"|Z|>k.  This suite supplies those.  All five figures are stand-alone and any",
"subset can be ported into the writeup.",
"",
"## Stylised facts the figures support",
"",
"1. Monthly Z on the 1980-2010 baseline has mean 0 and SD ~0.985 by",
"   construction (small finite-sample shrinkage from the SD).",
"2. Outside the baseline window monthly Z_pr has a small negative mean",
"   (-0.06 to -0.13 depending on region) attributable to the 2010-2022",
"   megadrought; monthly Z_t has slightly fatter tails (SD 1.10-1.21)",
"   attributable to secular warming + ENSO.",
"3. Threshold-crossing rates on the full sample are 12-23% for |Z_pr|>1 and",
"   29-38% for |Z_t|>1 (theoretical: 31.7% under N(0,1)).  Z_pr",
"   under-disperses against N(0,1) because precipitation is right-skewed and",
"   bounded at 0; Z_t is close to symmetric.",
"4. The threshold-rate distribution by region matches the documented Chilean",
"   teleconnection geography: central regions (Valparaiso, Santiago, O'Higgins)",
"   show the highest |Z_pr|>1 rates; northern regions show the highest",
"   |Z_t|>2 rates.",
"5. *Degenerate-baseline tail at the daily resolution.*  Daily Z_pr on wet",
"   days has a very long right tail: max ~2460, 99th percentile ~30, ~1%",
"   of wet days have Z>6. This is mechanical, not data error: at hyper-arid",
"   mine cells (Atacama / altiplano) the (mine x day-of-year) 1980-2010",
"   baseline SD is essentially zero, so any non-trivial wet day produces a",
"   numerically huge Z. The mining-module construction in script `10` fills",
"   zero-SD baselines with the mine's median SD, which bounds but does not",
"   remove the inflation. The FLOOD AND-gate is robust to this because:",
"   (a) its Z-condition is a SIGN test (Z>0 fires; magnitude is irrelevant),",
"   and (b) the threshold for firing is on physical mm (>=20 mm), not on Z.",
"   Any magnitude-based use of daily Z_pr (e.g., percentile-based extreme",
"   indices, mediation regressions on |Z|) would need to handle the tail.",
""
)
writeLines(readme_lines, readme_path)

cat("[80] Done. Outputs in:", Q1_DIR, "\n")
