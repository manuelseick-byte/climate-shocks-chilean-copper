# =============================================================================
# 61_figures_regression.R
#
# Figures 4-6 - depend on completed regression tables.
#   fig4  Coefficient forest plot - acute channels (FLOOD lower/upper, HEAT) +
#         drought annual headline.  Each point has a CR0 / CR2 / WCB p-stack
#         and LOO range whiskers.
#   fig5  Case-study chart - mine-level deviations vs calendar-month baseline
#         during named ENSO flood events.
#   fig6  Drought cumulative profile - SPEI vs SSI on the 3-year cum regressor,
#         showing 1- and 2- and 3-year cumulative shares as separate runs.
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(scales)
  library(fixest); library(clubSandwich)
})

ink   <- "#1A1A1A"; rule <- "#B8B8B8"
flood_lo <- "#2C7FB8"; flood_hi <- "#9FBAE6"
heat_c   <- "#D2691E"; drought_c <- "#8B5A3C"

theme_journal <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = ink),
          plot.title    = element_text(face = "bold", size = rel(1.1)),
          plot.subtitle = element_text(colour = ink, size = rel(0.95)),
          plot.caption  = element_text(colour = "#555555", size = rel(0.8), hjust = 0),
          axis.text     = element_text(colour = ink),
          axis.title    = element_text(colour = ink),
          panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA),
          strip.background = element_rect(fill = "#F2F2F2", colour = NA),
          strip.text       = element_text(face = "bold", colour = ink),
          legend.position  = "top",
          legend.title     = element_text(face = "bold", size = rel(0.9)),
          legend.background = element_rect(fill = "white", colour = NA),
          legend.key.height = unit(0.4, "cm"))
}

dpi <- 300

# =========================================================================
# fig4 -- coefficient forest plot
# Layout: single panel, descriptive row labels reverted to "Spec A/B"
# wording, navy/light-blue palette of the original draft, right-side
# annotation = exact beta + WCB (acute) or CR0 (drought) p-value.
# =========================================================================
acu  <- fread(file.path(TABLES, "40__regressions_acute.csv"))
acu_loor <- fread(file.path(TABLES, "40__loo_region_summary.csv"))
dro  <- fread(file.path(TABLES, "41__regressions_drought.csv"))
dro_loor <- fread(file.path(TABLES, "41__loo_region_drought.csv"))

acu_keep <- c("FLOOD_regmean_A","FLOOD_regmean_B",
              "FLOOD_mine_A","FLOOD_mine_B",
              "HEAT_mine_A","HEAT_mine_B")
acu_labels <- c(
  "FLOOD regional-mean monthly  Spec A",
  "FLOOD regional-mean monthly  Spec B",
  "FLOOD mine-cell  Spec A",
  "FLOOD mine-cell  Spec B",
  "HEAT mine-cell   Spec A",
  "HEAT mine-cell   Spec B")
acu_focal <- acu[label %in% acu_keep]
acu_focal[, display := factor(label, levels = acu_keep, labels = acu_labels)]
acu_focal[, p_show := p_wcb]
acu_focal[, p_kind := "WCB"]
acu_focal[, scale_pp := 1.0]  # binary regressor: 100*beta = approximate % output change

loor_summary <- acu_loor[, .(label, loo_min = beta_min, loo_max = beta_max)]
acu_focal <- merge(acu_focal, loor_summary, by = "label", all.x = TRUE)

# DROUGHT rows (2026-05-31): show the all-drought null vs ENSO-aligned negative
# contrast (SSI-12 demoted to appendix, not shown in the forest plot).
dro_keep <- c("cum3_threshold_SPEI","cum3_stress_SPEI_meteo")
dro_labels <- c("DROUGHT all-drought (SPEI-12, annual)",
                "DROUGHT ENSO-aligned (SPEI-12, annual)")
dro_focal <- dro[fe == "mineYR" & label %in% dro_keep,
                 .(label, term, beta, se, p_cr0,
                   p_wcb = NA_real_,
                   n_obs, n_clust = 7)]
dro_focal[, display := factor(label, levels = dro_keep, labels = dro_labels)]
dro_focal[, p_show := p_cr0]
dro_focal[, p_kind := "CR0"]
dro_focal[, scale_pp := 0.10]  # per +10 pp share, so 100*beta*0.10 = % output change
dro_loor_sum <- dro_loor[, .(loo_min = min(beta), loo_max = max(beta)),
                         by = .(label, term)]
dro_focal <- merge(dro_focal, dro_loor_sum, by = c("label","term"), all.x = TRUE)

plot_tbl <- rbind(acu_focal, dro_focal, fill = TRUE)
plot_tbl[, beta_pct    := 100 * beta * scale_pp]
plot_tbl[, ci_low_pct  := 100 * (beta - 1.96 * se) * scale_pp]
plot_tbl[, ci_high_pct := 100 * (beta + 1.96 * se) * scale_pp]
plot_tbl[, loo_lo_pct  := 100 * loo_min * scale_pp]
plot_tbl[, loo_hi_pct  := 100 * loo_max * scale_pp]
plot_tbl[, p_str := ifelse(is.na(p_show), "n/a",
                           ifelse(p_show < 0.005, "<.01",
                                  sub("^0", "", sprintf("%.2f", p_show))))]
plot_tbl[, right_lab := sprintf("%+0.1f%%  |  %s p=%s", beta_pct, p_kind, p_str)]
plot_tbl[, display := factor(display, levels = rev(c(acu_labels, dro_labels)))]

navy <- "#1F4E79"; loo_band <- "#5876A3"

x_min <- floor(min(c(plot_tbl$ci_low_pct,  plot_tbl$loo_lo_pct), na.rm = TRUE) - 1)
x_max <- max(5, ceiling(max(c(plot_tbl$ci_high_pct, plot_tbl$loo_hi_pct), na.rm = TRUE) + 1))
x_pad <- 0.50 * (x_max - x_min)

fig4 <- ggplot(plot_tbl, aes(y = display, x = beta_pct)) +
  geom_vline(xintercept = 0, colour = "#9A9A9A", linetype = 2, linewidth = 0.4) +
  geom_segment(aes(x = loo_lo_pct, xend = loo_hi_pct, yend = display),
               colour = loo_band, linewidth = 5.0, alpha = 0.45,
               lineend = "round") +
  geom_segment(aes(x = ci_low_pct, xend = ci_high_pct, yend = display),
               colour = "#1A1A1A", linewidth = 0.45) +
  geom_point(size = 3.2, shape = 21, fill = navy, colour = "#1A1A1A", stroke = 0.4) +
  geom_text(aes(x = x_max + 0.5, label = right_lab),
            hjust = 0, colour = "#1A1A1A", size = 3.0) +
  scale_x_continuous(breaks = scales::pretty_breaks(7),
                     labels = function(x) paste0(x, "%"),
                     limits = c(x_min, x_max + x_pad),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title    = "Effect of ENSO-related climate stress on copper mine output",
       subtitle = paste0("Forest plot of point estimate (dot) with 95% asymptotic cluster CI (thin line) ",
                         "and leave-one-region range (thick blue band)."),
       x = "Approximate change in copper output", y = NULL,
       caption = paste0("Cluster SE at region (7). Acute coefficients shown as approximate % output change; ",
                        "drought rows scaled to a +10 pp change in cumulative stress share. ",
                        "p-values: WCB wild-cluster bootstrap-t for acute rows (Rademacher, null imposed, B=999); ",
                        "CR0 asymptotic for drought rows."))  +
  theme_journal(base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#ECECEC", linewidth = 0.3),
        axis.text.y        = element_text(size = rel(1.0)),
        plot.title         = element_text(face = "bold", size = rel(1.15), hjust = 0),
        plot.subtitle      = element_text(hjust = 0),
        plot.margin        = margin(t = 10, r = 10, b = 10, l = 10))

ggsave(file.path(FIGURES, "fig4_coefficients.png"), fig4, width = 10.4, height = 6.4, dpi = dpi)
ggsave(file.path(FIGURES, "fig4_coefficients.svg"), fig4, width = 10.4, height = 6.4)
cat("[61] wrote fig4_coefficients\n")

# =========================================================================
# fig5 -- case-study chart: mine-month deviations during named ENSO floods
# =========================================================================
co <- fread(file.path(ORIG_PANELS, "01a__cochilco_copper_mine_monthly.csv"))
co <- co[!is.na(copper_kt) & copper_kt > 0]
# Calendar-month baseline per mine (mean 2006-2025)
base <- co[year >= 2006, .(baseline_kt = mean(copper_kt, na.rm = TRUE)),
           by = .(mine_operation, month)]
co <- merge(co, base, by = c("mine_operation", "month"), all.x = TRUE)
co[, dev_pct := 100 * (copper_kt - baseline_kt) / baseline_kt]

cases <- data.table(
  event_label = c("Mar 2015 Atacama aluvion",
                  "Mar 2015 Atacama aluvion",
                  "Apr 2017 Cajon del Maipo",
                  "Apr 2017 Cajon del Maipo",
                  "Feb 2023 Atacama / Coquimbo",
                  "Jun 2023 atmospheric river",
                  "Jun 2023 atmospheric river",
                  "Jun 2023 atmospheric river",
                  "Aug 2023 central Chile floods",
                  "Aug 2023 central Chile floods",
                  "Aug 2023 central Chile floods",
                  "Aug 2023 central Chile floods"),
  mine_operation = c("Codelco - Salvador", "Candelaria",
                     "Anglo American Sur", "Codelco - Teniente",
                     "Codelco - Salvador",
                     "Codelco - Andina", "Codelco - Teniente", "Anglo American Sur",
                     "Anglo American Sur", "Codelco - Teniente", "Codelco - Andina", "Los Pelambres"),
  year  = c(2015, 2015, 2017, 2017, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023),
  month = c(3, 3, 4, 4, 2, 6, 6, 6, 8, 8, 8, 8)
)
fig5_tbl <- merge(cases, co[, .(mine_operation, year, month, copper_kt, baseline_kt, dev_pct)],
                  by = c("mine_operation", "year", "month"), all.x = TRUE)
fig5_tbl[, event_label := factor(event_label, levels = unique(event_label))]
fig5_tbl[, mine_short := factor(mine_operation, levels = unique(mine_operation))]

fig5 <- ggplot(fig5_tbl, aes(y = mine_short, x = dev_pct)) +
  geom_vline(xintercept = 0, colour = rule, linewidth = 0.3) +
  geom_vline(xintercept = -15, colour = "#E76F51", linewidth = 0.3, linetype = 2) +
  geom_col(aes(fill = dev_pct < 0), width = 0.6) +
  geom_text(aes(label = sprintf("%+0.0f%%", dev_pct),
                hjust = ifelse(dev_pct < 0, 1.1, -0.1)),
            size = 2.7, colour = ink) +
  facet_wrap(~ event_label, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = "#2C7FB8", `FALSE` = "#BEBEBE"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  labs(title = "Mine-level output deviations during named ENSO-related flood events",
       subtitle = "Percent deviation from each mine's 2006-2025 calendar-month baseline. Reference: -15%.",
       x = "Deviation from baseline (%)", y = NULL,
       caption = "Source: Cochilco monthly mine-level archive; baseline = mean copper_kt for the same calendar month, 2006-2025.")  +
  theme_journal()

ggsave(file.path(FIGURES, "fig5_case_study.png"), fig5, width = 9.0, height = 7.5, dpi = dpi)
ggsave(file.path(FIGURES, "fig5_case_study.svg"), fig5, width = 9.0, height = 7.5)
cat("[61] wrote fig5_case_study\n")

# =========================================================================
# fig6 -- DROUGHT cumulative profile (SPEI vs SSI, 1-, 2-, 3-year cums)
# =========================================================================
# Re-fit the annual panel with cumulative shares over 1, 2, 3 years
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
ann <- ann[in_sample == TRUE]
DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")
cp <- fread(file.path(DATA_RAW, "BCCh", "copper_lme_monthly_usd_per_mt.csv"))
cp[, date := as.Date(paste0(date, "-01"))]; cp[, year := as.integer(format(date, "%Y"))]
cp[, log_price := log(price_usd_mt)]
yp <- cp[, .(log_price_year = mean(log_price, na.rm = TRUE)), by = year]
ann <- merge(ann, yp, by = "year", all.x = TRUE)
ann <- ann[!is.na(log_price_year)]
ann[, mine := factor(mine_operation)]; ann[, yr := factor(year)]
ann[, region_clust := factor(codregion)]

ann[, cum1_str_meteo := str_share_meteo_lag1]
ann[, cum2_str_meteo := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("str_share_meteo_lag1","str_share_meteo_lag2")]
ann[, cum1_str_hydro := str_share_hydro_lag1]
ann[, cum2_str_hydro := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("str_share_hydro_lag1","str_share_hydro_lag2")]

profile_rows <- list()
for (regname in c("str_share_meteo","cum1_str_meteo","cum2_str_meteo","cum3_str_meteo",
                  "str_share_hydro","cum1_str_hydro","cum2_str_hydro","cum3_str_hydro")) {
  if (!regname %in% names(ann)) next
  f <- sprintf("log_kt_year ~ %s | mine + yr", regname)
  m <- try(feols(as.formula(f), data = ann, cluster = ~ region_clust, notes = FALSE),
           silent = TRUE)
  if (inherits(m, "try-error")) next
  ct <- coeftable(m)
  if (!regname %in% rownames(ct)) next
  beta <- unname(ct[regname, "Estimate"])
  se   <- unname(ct[regname, "Std. Error"])
  idx <- switch(regname,
                "str_share_meteo" = 0, "cum1_str_meteo" = 1,
                "cum2_str_meteo" = 2, "cum3_str_meteo" = 3,
                "str_share_hydro" = 0, "cum1_str_hydro" = 1,
                "cum2_str_hydro" = 2, "cum3_str_hydro" = 3)
  idx_type <- ifelse(grepl("meteo", regname), "SPEI-12 (meteorological)", "SSI-12 (hydrological)")
  profile_rows[[length(profile_rows)+1]] <- data.table(
    horizon = idx, index = idx_type,
    beta = beta, se = se,
    ci_lo = beta - 1.96 * se, ci_hi = beta + 1.96 * se
  )
}
profile <- rbindlist(profile_rows, fill = TRUE)

fig6 <- ggplot(profile, aes(horizon, beta, colour = index, fill = index)) +
  geom_hline(yintercept = 0, colour = rule, linewidth = 0.3) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = 0:3,
                     labels = c("0 (current year)",
                                "1-yr lag",
                                "2-yr cum (y-1, y-2)",
                                "3-yr cum (y-1..y-3)")) +
  scale_colour_manual(values = c("SPEI-12 (meteorological)" = drought_c,
                                 "SSI-12 (hydrological)"   = "#3E6E4E"),
                      name = "Drought index") +
  scale_fill_manual(values   = c("SPEI-12 (meteorological)" = drought_c,
                                 "SSI-12 (hydrological)"   = "#3E6E4E"),
                    name = "Drought index") +
  labs(title = "DROUGHT cumulative effect builds over multi-year window",
       subtitle = "Effect on log annual mine output, mine + year FE, two physically-different drought indices.",
       x = "Horizon over which the ENSO-attributable drought share is averaged",
       y = "Coefficient on log mine output",
       caption = "Shaded ribbon = 95% asymptotic cluster CI at region (7).") +
  theme_journal()

# fig6 is now rebuilt by scripts/18_fig_drought_matched.R as the matched-scale
# all-drought-vs-ENSO-aligned contrast; do not overwrite it here (2026-05-31).
# ggsave(file.path(FIGURES, "fig6_drought_profile.png"), fig6, width = 9.0, height = 5.0, dpi = dpi)
# ggsave(file.path(FIGURES, "fig6_drought_profile.svg"), fig6, width = 9.0, height = 5.0)
cat("[61] fig6 left to script 18 (matched-scale)\n")
