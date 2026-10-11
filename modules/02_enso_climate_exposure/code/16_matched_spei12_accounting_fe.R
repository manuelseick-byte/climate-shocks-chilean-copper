# =============================================================================
# 16_matched_spei12_accounting_fe.R
#
# Diagnostic follow-up to the matched-scale DROUGHT battery.
#
# Purpose:
#   1. Visualize annual accounting for matched SPEI-12:
#        all threshold drought = ENSO-attributed + non-ENSO drought.
#   2. Make the FE contrast explicit by estimating all/ENSO/decomposition under
#        baseline FE, mine trends, and region-year FE.
#
# Outputs:
#   panels/16__matched_spei12_accounting_panel.csv
#   tables/16__matched_spei12_annual_accounting.csv
#   tables/16__matched_spei12_fe_ladder.csv
#   tables/16__matched_spei12_period_sensitivity.csv
#   figures/16__matched_spei12_accounting.png/.svg
#   figures/16__matched_spei12_fe_ladder.png/.svg
# =============================================================================

.find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "CLAUDE.md"))) {
    p <- dirname(d)
    if (p == d) stop("CLAUDE.md not found from ", start)
    d <- p
  }
  d
}

ROOT <- .find_root()
source(file.path(ROOT, "mining_module_alternative", "scripts", "00_setup.R"))

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(fixest)
  library(ggplot2)
})

# ---- 1. Reconstruct matched SPEI-12 x 12-month gate exposure ----------------
ec <- as.data.table(read_parquet(file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")))
ec <- ec[codregion %in% COPPER_REGIONS,
         .(codregion, year, month, date, oni_rolling, Z_pr_m, rho_pr_m)]
setorder(ec, codregion, date)
ec[, oni_roll12 := frollmean(oni_rolling, 12), by = codregion]
ec[, z12 := frollmean(Z_pr_m, 12), by = codregion]
ec[, gate12 := as.integer(!is.na(oni_roll12) & abs(oni_roll12) >= 0.5 &
                            !is.na(z12) & rho_pr_m * oni_roll12 < 0 & z12 < 0)]

mc <- as.data.table(read_parquet(file.path(PANELS, "14__minecell_spei_multiscale_1960_2025.parquet")))
mc <- mc[, .(mine_operation, codregion, year, month,
             f12 = drought_flag_spei12,
             v12 = as.integer(!is.na(spei12)))]
mc <- merge(mc, ec[, .(codregion, year, month, gate12)],
            by = c("codregion", "year", "month"), all.x = TRUE)

yr_exp <- mc[, .(
  valid_months = sum(v12),
  thr = mean(f12),
  str = mean(as.integer(f12 == 1 & !is.na(gate12) & gate12 == 1)),
  non = mean(as.integer(f12 == 1 & (is.na(gate12) | gate12 == 0)))
), by = .(mine_operation, codregion, year)]
yr_exp[valid_months < 12, c("thr", "str", "non") := NA_real_]

setorder(yr_exp, mine_operation, year)
for (col in c("thr", "str", "non")) {
  for (lg in 1:3) {
    yr_exp[, paste0(col, "_l", lg) := shift(get(col), lg), by = mine_operation]
  }
  yr_exp[, paste0("cum3_", col) := rowMeans(.SD), .SDcols = paste0(col, "_l", 1:3)]
}

# ---- 2. Outcome sample matching script 15 -----------------------------------
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
ann <- ann[in_sample == TRUE, .(mine_operation, codregion, year, log_kt_year)]
cp <- fread(file.path(PROJECT_ROOT, "Data_raw", "BCCh", "copper_lme_monthly_usd_per_mt.csv"))
cp[, year := as.integer(format(as.Date(paste0(date, "-01")), "%Y"))]
ann <- merge(ann, unique(cp[, .(year)])[, has_price := 1], by = "year", all.x = TRUE)[has_price == 1]

D <- merge(
  ann,
  yr_exp[, .(mine_operation, year, valid_months, thr, str, non, cum3_thr, cum3_str, cum3_non)],
  by = c("mine_operation", "year"),
  all.x = TRUE
)
D[, `:=`(
  mine = factor(mine_operation),
  yr = factor(year),
  rc = factor(codregion),
  region_year = interaction(codregion, year, drop = TRUE)
)]
D[, `:=`(
  check_identity_annual = thr - str - non,
  check_identity_cum3 = cum3_thr - cum3_str - cum3_non
)]

fwrite(D, file.path(PANELS, "16__matched_spei12_accounting_panel.csv"))

annual <- D[, .(
  mines = .N,
  annual_all = mean(thr, na.rm = TRUE),
  annual_enso = mean(str, na.rm = TRUE),
  annual_nonenso = mean(non, na.rm = TRUE),
  cum3_all = mean(cum3_thr, na.rm = TRUE),
  cum3_enso = mean(cum3_str, na.rm = TRUE),
  cum3_nonenso = mean(cum3_non, na.rm = TRUE)
), by = year][order(year)]
fwrite(annual, file.path(TABLES, "16__matched_spei12_annual_accounting.csv"))

cat("[16] identity checks: annual max abs =",
    max(abs(D$check_identity_annual), na.rm = TRUE),
    "| cum3 max abs =", max(abs(D$check_identity_cum3), na.rm = TRUE), "\n")

# ---- 3. FE ladder ------------------------------------------------------------
fit_one <- function(label, fe_label, reg, fe) {
  d <- D[!is.na(get(reg))]
  m <- feols(as.formula(sprintf("log_kt_year ~ %s | %s", reg, fe)),
             d, cluster = ~ rc, notes = FALSE)
  ct <- coeftable(m)
  data.table(
    model = label,
    fe = fe_label,
    term = reg,
    beta = unname(ct[reg, "Estimate"]),
    se = unname(ct[reg, "Std. Error"]),
    p = unname(ct[reg, "Pr(>|t|)"]),
    n = nobs(m)
  )
}

fit_decomp <- function(fe_label, fe) {
  d <- D[!is.na(cum3_str) & !is.na(cum3_non)]
  m <- feols(as.formula(sprintf("log_kt_year ~ cum3_str + cum3_non | %s", fe)),
             d, cluster = ~ rc, notes = FALSE)
  ct <- coeftable(m)
  rbindlist(lapply(c("cum3_str", "cum3_non"), function(tm) {
    data.table(
      model = "Decomposition",
      fe = fe_label,
      term = tm,
      beta = unname(ct[tm, "Estimate"]),
      se = unname(ct[tm, "Std. Error"]),
      p = unname(ct[tm, "Pr(>|t|)"]),
      n = nobs(m)
    )
  }))
}

fe_specs <- list(
  list(label = "Mine + year FE", fe = "mine + yr"),
  list(label = "Mine + year FE + mine trends", fe = "mine + yr + mine[year]"),
  list(label = "Mine + region-year FE", fe = "mine + region_year")
)

fe_rows <- list()
for (sp in fe_specs) {
  fe_rows[[length(fe_rows) + 1]] <- fit_one("Threshold-only all drought", sp$label, "cum3_thr", sp$fe)
  fe_rows[[length(fe_rows) + 1]] <- fit_one("ENSO-attributed only", sp$label, "cum3_str", sp$fe)
  fe_rows[[length(fe_rows) + 1]] <- fit_decomp(sp$label, sp$fe)
}
fe_tbl <- rbindlist(fe_rows, fill = TRUE)
fe_tbl[, term_label := fifelse(term == "cum3_thr", "All drought",
                        fifelse(term == "cum3_str", "ENSO-attributed", "Non-ENSO"))]
fe_tbl[, `:=`(
  beta = round(beta, 6),
  se = round(se, 6),
  p = round(p, 6),
  ci_low = round(beta - 1.96 * se, 6),
  ci_high = round(beta + 1.96 * se, 6)
)]
fwrite(fe_tbl, file.path(TABLES, "16__matched_spei12_fe_ladder.csv"))

# ---- 4. Late-megadrought block sensitivity ---------------------------------
fit_subset <- function(label, expr) {
  d <- D[eval(parse(text = expr)) & !is.na(cum3_str)]
  if (nrow(d) < 50 || uniqueN(d$year) < 4) {
    return(data.table(test = label, n = nrow(d), beta = NA_real_, se = NA_real_, p = NA_real_))
  }
  m <- feols(log_kt_year ~ cum3_str | mine + yr, d, cluster = ~ rc, notes = FALSE)
  ct <- coeftable(m)
  data.table(
    test = label,
    n = nobs(m),
    beta = unname(ct["cum3_str", "Estimate"]),
    se = unname(ct["cum3_str", "Std. Error"]),
    p = unname(ct["cum3_str", "Pr(>|t|)"])
  )
}
period_tbl <- rbindlist(list(
  fit_subset("Full sample", "TRUE"),
  fit_subset("Drop output years 2022-2025", "!(year %in% 2022:2025)"),
  fit_subset("Drop output years 2021-2025", "!(year %in% 2021:2025)"),
  fit_subset("Drop output years 2020-2025", "!(year %in% 2020:2025)"),
  fit_subset("Pre-2020 output years only", "year < 2020"),
  fit_subset("Post-2020 output years only", "year >= 2020")
), fill = TRUE)
period_tbl[, `:=`(
  beta = round(beta, 6),
  se = round(se, 6),
  p = round(p, 6)
)]
fwrite(period_tbl, file.path(TABLES, "16__matched_spei12_period_sensitivity.csv"))

# ---- 5. Figures -------------------------------------------------------------
yr_min <- min(annual$year)
yr_max <- max(annual$year)
shade_start <- 2020.5
shade_end <- 2024.5

fmt_pct <- function(x) paste0(round(100 * x), "%")

annual_stack <- melt(
  annual,
  id.vars = "year",
  measure.vars = c("annual_nonenso", "annual_enso"),
  variable.name = "component",
  value.name = "share"
)
annual_stack[, component := factor(component,
  levels = c("annual_nonenso", "annual_enso"),
  labels = c("Non-ENSO SPEI-12 drought", "ENSO-attributed SPEI-12 drought")
)]

cum_lines <- melt(
  annual,
  id.vars = "year",
  measure.vars = c("cum3_all", "cum3_nonenso", "cum3_enso"),
  variable.name = "series",
  value.name = "share"
)
cum_lines[, series := factor(series,
  levels = c("cum3_all", "cum3_nonenso", "cum3_enso"),
  labels = c("All drought (threshold-only)", "Non-ENSO component", "ENSO-attributed component")
)]

p1 <- ggplot() +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf,
           fill = "#F6E5DE", alpha = 0.55) +
  geom_col(data = annual_stack, aes(x = year, y = share, fill = component),
           width = 0.78, color = NA) +
  geom_line(data = annual, aes(x = year, y = annual_all),
            color = "#111111", linewidth = 0.75) +
  geom_point(data = annual, aes(x = year, y = annual_all),
             color = "#111111", size = 1.4) +
  scale_fill_manual(values = c("Non-ENSO SPEI-12 drought" = "#B8BEC8",
                               "ENSO-attributed SPEI-12 drought" = "#B64A3A")) +
  scale_y_continuous(labels = fmt_pct, limits = c(0, max(annual$annual_all, na.rm = TRUE) * 1.12)) +
  scale_x_continuous(breaks = seq(yr_min, yr_max, by = 2)) +
  labs(
    title = "A. Annual matched SPEI-12 drought accounting",
    subtitle = "Stacked bars sum to all threshold drought; black line is the all-drought share.",
    x = NULL,
    y = "Monthly share in year",
    fill = NULL
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "#555555", size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p2 <- ggplot(cum_lines, aes(x = year, y = share, color = series)) +
  annotate("rect", xmin = shade_start, xmax = shade_end, ymin = -Inf, ymax = Inf,
           fill = "#F6E5DE", alpha = 0.55) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.35) +
  scale_color_manual(values = c("All drought (threshold-only)" = "#111111",
                                "Non-ENSO component" = "#777F8D",
                                "ENSO-attributed component" = "#B64A3A")) +
  scale_y_continuous(labels = fmt_pct, limits = c(0, max(annual$cum3_all, na.rm = TRUE) * 1.12)) +
  scale_x_continuous(breaks = seq(yr_min, yr_max, by = 2)) +
  labs(
    title = "B. Regressor: 3-year lagged cumulative exposure",
    subtitle = "The threshold-only regressor mostly follows the non-ENSO component; the ENSO component rises late.",
    x = NULL,
    y = "Mean cum3 share",
    color = NULL,
    caption = "Matched SPEI-12 x 12-mo gate; 19-mine sample means. Cum3 averages y-1:y-3; shading marks the late-megadrought block."
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "#555555", size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "#666666", size = 8, hjust = 0)
  )

if (requireNamespace("patchwork", quietly = TRUE)) {
  fig_accounting <- p1 / p2 + patchwork::plot_layout(heights = c(1, 1))
} else {
  fig_accounting <- p1
}

ggsave(file.path(FIGURES, "16__matched_spei12_accounting.png"),
       fig_accounting, width = 9.0, height = 7.2, dpi = 220)
ggsave(file.path(FIGURES, "16__matched_spei12_accounting.svg"),
       fig_accounting, width = 9.0, height = 7.2)

plot_fe <- copy(fe_tbl)
plot_fe <- plot_fe[model %in% c("Threshold-only all drought", "ENSO-attributed only", "Decomposition")]
plot_fe[, plot_label := fifelse(model == "Decomposition",
                                paste0("Decomp: ", term_label),
                                fifelse(model == "Threshold-only all drought",
                                        "All drought alone", "ENSO-attributed alone"))]
plot_fe[, plot_label := factor(plot_label,
  levels = c("All drought alone", "ENSO-attributed alone",
             "Decomp: ENSO-attributed", "Decomp: Non-ENSO")
)]
plot_fe[, fe := factor(fe, levels = vapply(fe_specs, `[[`, character(1), "label"))]

p_fe <- ggplot(plot_fe, aes(x = beta, y = plot_label, color = plot_label)) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.45) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.18, linewidth = 0.75) +
  geom_point(size = 2.2) +
  facet_wrap(~ fe, ncol = 1) +
  scale_color_manual(values = c("All drought alone" = "#111111",
                                "ENSO-attributed alone" = "#B64A3A",
                                "Decomp: ENSO-attributed" = "#B64A3A",
                                "Decomp: Non-ENSO" = "#777F8D")) +
  labs(
    title = "Matched SPEI-12 coefficient ladder",
    subtitle = "All-drought is null because the non-ENSO component is positive/null; the ENSO-attributed component remains negative.",
    x = "Coefficient on cum3 drought share",
    y = NULL,
    color = NULL,
    caption = "Clustered SE by region. Region-year FE is stricter and identifies from within-region, cross-mine differences."
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "#555555", size = 9),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(color = "#666666", size = 8, hjust = 0)
  )

ggsave(file.path(FIGURES, "16__matched_spei12_fe_ladder.png"),
       p_fe, width = 8.2, height = 7.2, dpi = 220)
ggsave(file.path(FIGURES, "16__matched_spei12_fe_ladder.svg"),
       p_fe, width = 8.2, height = 7.2)

cat("[16] wrote figures and tables.\n")
cat("[16] annual accounting:\n")
print(annual)
cat("[16] FE ladder:\n")
print(fe_tbl)
cat("[16] period sensitivity:\n")
print(period_tbl)
