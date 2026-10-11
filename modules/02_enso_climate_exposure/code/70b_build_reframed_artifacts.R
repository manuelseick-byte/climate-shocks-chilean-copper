# =============================================================================
# 70b_build_reframed_artifacts.R   (mining_module_alternative/scripts/)
#
# Build all the report-ready artefacts for the reframed FLOOD narrative:
#   - reg-mean A and B as headline FLOOD result
#   - mine-cell A (conservative lower bound) and mine-cell B with singletons
#     filtered (where coefficient collapses to ~0)
#   - corridor entirely removed
#   - HEAT mine-cell A and B unchanged
#   - DROUGHT unchanged (decision deferred)
#   - Spec A relabelled "General"; Spec B relabelled "ENSO-attributable"
#
# Outputs (overwriting):
#   tables/tex__acute_regressions.tex     — new acute regressions table
#   tables/40__regressions_acute.csv      — reframed regression CSV
#   tables/40__loo_region_summary.csv     — reframed LOO summary
#   figures/fig4_coefficients.png         — reframed forest plot
#
# Inputs (READ-ONLY):
#   panels/30__acute_mine_month.parquet
#   panels/31__drought_mine_year.parquet
#   tables/41__regressions_drought.csv  (kept as-is for drought)
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest); library(ggplot2); library(scales); library(patchwork)
})

# ---- Load panel & build cleaned regressors -----------------------------------
a <- as.data.table(read_parquet(file.path(PANELS, "30__acute_mine_month.parquet")))
a <- a[in_sample == TRUE]
flag_cols <- c("thr_flood_mine","thr_flood_regmean_M","thr_heat_mine",
               "stress_flood_mine","stress_flood_regmean_M","stress_heat_mine")
for (col in flag_cols) a[is.na(get(col)), (col) := 0L]
a[, mine := factor(mine_operation)]
a[, region_calmonth := factor(sprintf("R%02d_M%02d", codregion, month))]
a[, yr := factor(year)]
a[, region_clust := factor(codregion)]

# Singleton-filtered mine-cell stress
firings_per_mine <- a[, .(n_stress = sum(stress_flood_mine == 1)),
                       by = mine_operation]
multi_firing_mines <- firings_per_mine[n_stress >= 2, mine_operation]
a[, stress_flood_mine_clean := stress_flood_mine]
a[!(mine_operation %in% multi_firing_mines) & stress_flood_mine == 1,
  stress_flood_mine_clean := 0L]
cat("[70b] mine-cell Spec B singleton filter: raw firings =",
    sum(a$stress_flood_mine), " | cleaned =",
    sum(a$stress_flood_mine_clean), "\n")

# ---- WCB helper --------------------------------------------------------------
wild_cluster_p <- function(formula_str, dat, term, B = 999, seed = 12345) {
  set.seed(seed)
  m_full <- feols(as.formula(formula_str), data = dat,
                  cluster = ~ region_clust, notes = FALSE)
  cf <- coef(m_full)
  if (!term %in% names(cf)) return(NA_real_)
  se_full <- sqrt(diag(vcov(m_full))[term])
  t_obs <- abs(cf[term] / se_full)
  f_full <- as.formula(formula_str)
  f_r    <- update(f_full, as.formula(paste(". ~ . -", term)))
  m_r    <- feols(f_r, data = dat, cluster = ~ region_clust, notes = FALSE)
  u <- resid(m_r); yhat0 <- predict(m_r, newdata = dat)
  yname <- as.character(f_full[[2L]])
  cl <- dat$region_clust; cl_levels <- levels(cl)
  d_boot <- copy(dat); t_b <- numeric(B)
  for (b in seq_len(B)) {
    rad <- sample(c(-1, 1), length(cl_levels), replace = TRUE)
    rad_per_obs <- rad[match(cl, cl_levels)]
    d_boot[[yname]] <- yhat0 + rad_per_obs * u
    mb <- try(feols(as.formula(formula_str), data = d_boot,
                    cluster = ~ region_clust, notes = FALSE), silent = TRUE)
    if (inherits(mb, "try-error")) { t_b[b] <- NA_real_; next }
    cf_b <- coef(mb); se_b <- sqrt(diag(vcov(mb))[term])
    t_b[b] <- abs(cf_b[term] / se_b)
  }
  t_b <- t_b[is.finite(t_b)]
  if (length(t_b) == 0) return(NA_real_)
  mean(t_b >= t_obs)
}

# ---- Define the reframed runs (in the order they will appear in the table) --
runs <- list(
  list(label = "FLOOD_regmean_A", group = "FLOOD — regional-mean monthly (headline)",
       spec_lbl = "General flood shocks",
       focal = "thr_flood_regmean_M",
       formula = "log_kt ~ thr_flood_regmean_M + log_price | mine + region_calmonth + yr"),
  list(label = "FLOOD_regmean_B", group = "FLOOD — regional-mean monthly (headline)",
       spec_lbl = "ENSO-attributable flood shocks",
       focal = "stress_flood_regmean_M",
       formula = "log_kt ~ stress_flood_regmean_M + log_price | mine + region_calmonth + yr"),
  list(label = "FLOOD_mine_A", group = "FLOOD — mine-cell (conservative)",
       spec_lbl = "General flood shocks",
       focal = "thr_flood_mine",
       formula = "log_kt ~ thr_flood_mine + log_price | mine + region_calmonth + yr"),
  list(label = "FLOOD_mine_B", group = "FLOOD — mine-cell (conservative)",
       spec_lbl = "ENSO-attributable flood shocks (singletons filtered)",
       focal = "stress_flood_mine_clean",
       formula = "log_kt ~ stress_flood_mine_clean + log_price | mine + region_calmonth + yr"),
  list(label = "HEAT_mine_A", group = "HEAT — mine-cell",
       spec_lbl = "General heat shocks",
       focal = "thr_heat_mine",
       formula = "log_kt ~ thr_heat_mine + log_price | mine + region_calmonth + yr"),
  list(label = "HEAT_mine_B", group = "HEAT — mine-cell",
       spec_lbl = "ENSO-attributable heat shocks",
       focal = "stress_heat_mine",
       formula = "log_kt ~ stress_heat_mine + log_price | mine + region_calmonth + yr")
)

# ---- Run + LOO ---------------------------------------------------------------
main_rows <- list(); loo_rows <- list()
for (r in runs) {
  cat("\n[70b]", r$label, " - ", r$spec_lbl, "\n")
  m <- feols(as.formula(r$formula), data = a,
             cluster = ~ region_clust, notes = FALSE)
  ct <- coeftable(m)
  if (!r$focal %in% rownames(ct)) next
  beta <- unname(ct[r$focal, "Estimate"])
  se   <- unname(ct[r$focal, "Std. Error"])
  pcr0 <- unname(ct[r$focal, "Pr(>|t|)"])
  pwcb <- wild_cluster_p(r$formula, a, r$focal, B = 999)
  cat(sprintf("    beta=%+0.4f se=%0.4f pCR0=%0.4f pWCB=%0.4f\n",
              beta, se, pcr0, pwcb))

  main_rows[[length(main_rows)+1]] <- data.table(
    label = r$label, group = r$group, spec_lbl = r$spec_lbl,
    term = r$focal, beta = beta, se = se,
    p_cr0 = pcr0, p_wcb = pwcb,
    n_firings = sum(a[[r$focal]] == 1),
    n_obs = nobs(m), n_clust = 7
  )

  # LOO-region
  for (rg in unique(a$codregion)) {
    sub <- a[codregion != rg]
    mlr <- try(feols(as.formula(r$formula), data = sub,
                     cluster = ~ region_clust, notes = FALSE), silent = TRUE)
    if (inherits(mlr, "try-error")) next
    ct2 <- coeftable(mlr)
    if (!r$focal %in% rownames(ct2)) next
    loo_rows[[length(loo_rows)+1]] <- data.table(
      label = r$label, drop_region = rg,
      beta = unname(ct2[r$focal, "Estimate"]),
      se = unname(ct2[r$focal, "Std. Error"]),
      p  = unname(ct2[r$focal, "Pr(>|t|)"])
    )
  }
}

main_tbl <- rbindlist(main_rows, fill = TRUE)
loo_tbl  <- rbindlist(loo_rows, fill = TRUE)
fwrite(main_tbl, file.path(TABLES, "40__regressions_acute.csv"))
fwrite(loo_tbl,  file.path(TABLES, "40__loo_region.csv"))

loo_summary <- loo_tbl[, .(beta_min = min(beta), beta_max = max(beta),
                           p_min = min(p), p_max = max(p)),
                       by = label]
fwrite(loo_summary, file.path(TABLES, "40__loo_region_summary.csv"))

# ---- Write the reframed acute regressions tex ----------------------------
main_tbl <- merge(main_tbl, loo_summary, by = "label", all.x = TRUE)

# Significance helper
sig_str <- function(p) {
  fcase(is.na(p), "",
        p < 0.01, "$^{***}$",
        p < 0.05, "$^{**}$",
        p < 0.10, "$^{*}$",
        default = "")
}

make_row <- function(rr) {
  # Spec label with indent
  sl <- rr$spec_lbl
  sprintf("\\quad %s & %0.3f & %0.3f & %0.3f%s & %0.3f%s & %s & [%0.3f, %0.3f] \\\\",
          sl, rr$beta, rr$se,
          rr$p_cr0, sig_str(rr$p_cr0),
          rr$p_wcb, sig_str(rr$p_wcb),
          format(rr$n_obs, big.mark = ","),
          rr$beta_min, rr$beta_max)
}

# Build the table body grouped by `group`
group_order <- c("FLOOD — regional-mean monthly (headline)",
                 "FLOOD — mine-cell (conservative)",
                 "HEAT — mine-cell")

body_lines <- character(0)
for (g in group_order) {
  body_lines <- c(body_lines, sprintf("\\multicolumn{7}{l}{\\textit{%s}}\\\\", g))
  sub <- main_tbl[group == g][order(-grepl("General", spec_lbl))]  # General first
  for (i in seq_len(nrow(sub))) {
    body_lines <- c(body_lines, make_row(sub[i]))
  }
  if (g != tail(group_order, 1)) {
    body_lines <- c(body_lines, "\\midrule")
  }
}

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Acute mine-month regressions, reframed. Spec\\,\\textbf{A — General} fires on any physical threshold crossing; Spec\\,\\textbf{B — ENSO-attributable} fires only on the AND-gate (active ONI, teleconnection-aligned, realised-Z direction, threshold crossed).  Two $p$-values are reported: CR0 (asymptotic cluster-robust at region, 7 clusters) and WCB (wild-cluster bootstrap-$t$, Rademacher, null imposed, $B=999$).  LOO range is the minimum and maximum coefficient across the seven leave-one-region refits.  Mine-cell Spec B uses the singletons-filtered regressor (the 4 mines that fire only once at the mine cell are dropped because under mine FE a singleton firing has no within-mine identifying variation; the raw coefficient is $+0.014$, $p_{\\text{WCB}}=0.61$).}",
  "\\label{tab:acute-regressions}",
  "\\small",
  "\\begin{tabular}{p{6.0cm}rrrrrr}",
  "\\toprule",
  "\\textbf{Specification} & $\\hat\\beta$ & SE & $p_{\\text{CR0}}$ & $p_{\\text{WCB}}$ & $N$ & \\textbf{LOO range} \\\\",
  "\\midrule",
  body_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}\\small\\item Significance: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.\\end{tablenotes}",
  "\\end{table}"
)
writeLines(tex, file.path(TABLES, "tex__acute_regressions.tex"))
cat("[70b] wrote tables/tex__acute_regressions.tex\n")

# ---- Build the reframed forest plot fig4_coefficients.png -------------------
# Read the drought regressions (unchanged) for the bottom panel
dr <- fread(file.path(TABLES, "41__regressions_drought.csv"))

# Build acute plot data
acu_plot <- copy(main_tbl)
acu_plot[, channel := fcase(
  grepl("regmean",   label), "FLOOD reg-mean monthly (headline)",
  grepl("^FLOOD_mine", label), "FLOOD mine-cell (conservative)",
  grepl("^HEAT",     label), "HEAT mine-cell",
  default = "?"
)]
acu_plot[, spec_short := fcase(
  grepl(" A$", label), "A — General",
  grepl(" B$", label), "B — ENSO-attributable",
  default = ""
)]
acu_plot[grepl("FLOOD_regmean_A", label), spec_short := "A — General"]
acu_plot[grepl("FLOOD_regmean_B", label), spec_short := "B — ENSO-attributable"]
acu_plot[grepl("FLOOD_mine_A", label),    spec_short := "A — General"]
acu_plot[grepl("FLOOD_mine_B", label),    spec_short := "B — ENSO-attributable\n(singletons filtered)"]
acu_plot[grepl("HEAT_mine_A", label),     spec_short := "A — General"]
acu_plot[grepl("HEAT_mine_B", label),     spec_short := "B — ENSO-attributable"]

acu_plot[, beta_pct := 100 * beta]
acu_plot[, ci_lo_pct := 100 * (beta - 1.96 * se)]
acu_plot[, ci_hi_pct := 100 * (beta + 1.96 * se)]
acu_plot[, sig := fcase(
  !is.na(p_wcb) & p_wcb < 0.01, "***",
  !is.na(p_wcb) & p_wcb < 0.05, "**",
  !is.na(p_wcb) & p_wcb < 0.10, "*",
  default = "")]

acu_plot[, channel := factor(channel,
  levels = c("FLOOD reg-mean monthly (headline)",
             "FLOOD mine-cell (conservative)",
             "HEAT mine-cell"))]

# Drought side (2026-06-01 revision): matched-rho headline from script 19 so the
# forest plot reconciles EXACTLY with Table 7 (all-drought +0.075 null vs
# ENSO-aligned -0.277).  Scaled per +10 pp of the 3-yr cumulative exposure share.
# SSI-12 is a non-corroborating cross-check parked in Annex B and is NOT shown.
dr_plot <- fread(file.path(TABLES, "19__drought_headline_for_fig.csv"))
dr_plot[, is_enso := grepl("ENSO", spec)]
dr_plot[, spec_short := fifelse(is_enso,
                                "ENSO-aligned\n(3-yr cumulative share)",
                                "All-drought\n(3-yr cumulative share)")]
dr_plot[, spec_short := factor(spec_short,
  levels = c("All-drought\n(3-yr cumulative share)", "ENSO-aligned\n(3-yr cumulative share)"))]
dr_plot[, beta_pct  := 100 * beta * 0.10]
dr_plot[, ci_lo_pct := 100 * (beta - 1.96 * se) * 0.10]
dr_plot[, ci_hi_pct := 100 * (beta + 1.96 * se) * 0.10]
dr_plot[, sig := fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")]

theme_q1 <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", colour = "#1A1A1A", size = 12),
        plot.subtitle = element_text(size = 10, colour = "#444444"),
        plot.caption = element_text(size = 8, colour = "#666666", hjust = 0),
        strip.text.y = element_text(face = "bold", angle = 0, hjust = 0),
        legend.position = "bottom")

COL_GENERAL  <- "#A0A0A0"
COL_ENSO     <- "#1F4E79"
COL_ENSO_LT  <- "#5C7FB3"

acu_plot[, color := fcase(grepl("^A", spec_short), COL_GENERAL,
                          grepl("singletons", spec_short), COL_ENSO_LT,
                          default = COL_ENSO)]

p_acute <- ggplot(acu_plot, aes(x = beta_pct, y = spec_short, colour = color)) +
  geom_vline(xintercept = 0, colour = "#CCCCCC", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo_pct, xmax = ci_hi_pct), height = 0, linewidth = 0.5) +
  geom_point(size = 2.6) +
  geom_text(aes(label = sprintf("%+0.1f%% %s", beta_pct, sig)),
            hjust = -0.15, vjust = 1.6, size = 3, colour = "#222222") +
  facet_grid(channel ~ ., switch = "y", space = "free_y", scales = "free_y") +
  scale_colour_identity() +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Acute channels — reframed", x = "% change in log(kt)", y = NULL,
       subtitle = "FLOOD reg-mean Spec A (general flood shocks) = headline. Mine-cell as conservative lower bound. Corridor excluded.") +
  theme_q1

p_drought <- ggplot(dr_plot, aes(x = beta_pct, y = spec_short)) +
  geom_vline(xintercept = 0, colour = "#CCCCCC", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo_pct, xmax = ci_hi_pct), height = 0, linewidth = 0.5,
                 colour = COL_ENSO) +
  geom_point(aes(colour = is_enso), size = 2.8) +
  scale_colour_manual(values = c(`TRUE` = COL_ENSO, `FALSE` = COL_GENERAL), guide = "none") +
  geom_text(aes(label = sprintf("%+0.1f%% %s", beta_pct, sig)),
            hjust = -0.15, vjust = 1.6, size = 3, colour = "#222222") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "DROUGHT (annual, mine-year): all-drought null vs ENSO-aligned negative",
       x = "% change in annual log output per +10 pp of 3-yr cumulative exposure share",
       y = NULL,
       subtitle = "Headline rests on SPEI-12 (ENSO-aligned, matched-rho). SSI-12 is a non-corroborating cross-check (Annex B), not shown.",
       caption = paste0(
         "Stars on the DROUGHT bars are CR0 asymptotic cluster-robust significance. The wild-cluster bootstrap is degenerate on this\n",
         "panel (7 clusters; exposure zero in ~79% of mine-years, concentrated in the post-2020 block), so the preferred small-sample\n",
         "p-value is from randomisation inference: p = 0.13 one-sided. The effect is sign-stable across all 7 leave-one-region refits.")) +
  theme_q1

fig4 <- (p_acute / p_drought) +
  plot_layout(heights = c(3, 2)) +
  plot_annotation(
    title = "Headline coefficients across channels",
    caption = "Mine-cell Spec B uses the singletons-filtered regressor (4 mines that fire only once dropped; under mine FE a singleton firing has no within-mine identifying variation). The raw mine-cell Spec B (with singletons) gives +1.4% (p_WCB = 0.61). DROUGHT regressors are cum3 of the (y-1, y-2, y-3) share of ENSO-aligned drought months. FE: mine + region×calmonth + year (acute) or mine + year (drought); drought rows scaled per +10 pp of the 3-yr cumulative exposure share. SE cluster: region (7 clusters).",
    theme = theme(plot.title = element_text(face = "bold", size = 13, colour = "#1A1A1A"),
                  plot.caption = element_text(size = 8, colour = "#666666", hjust = 0))
  )
ggsave(file.path(FIGURES, "fig4_coefficients.png"),
       fig4, width = 11, height = 9, dpi = 150, bg = "white")
cat("[70b] wrote figures/fig4_coefficients.png\n")

cat("[70b] Done.\n")
