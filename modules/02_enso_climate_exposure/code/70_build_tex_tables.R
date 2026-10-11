# =============================================================================
# 70_build_tex_tables.R
#
# Compile LaTeX fragments from the regression and validation CSV tables.
# Output: tables/tex__*.tex
# =============================================================================

source(file.path("code", "00_setup.R"))

f1 <- function(x, dig = 3) ifelse(is.na(x), "--", formatC(x, format = "f", digits = dig))
fp <- function(x) {
  if (is.na(x)) return("--")
  stars <- if (x < 0.01) "$^{***}$" else if (x < 0.05) "$^{**}$" else if (x < 0.10) "$^{*}$" else ""
  paste0(formatC(x, format = "f", digits = 3), stars)
}

# ----------------------- 1.  Thresholds registry --------------------------
thr <- fread(file.path(REGISTRY, "thresholds.csv"))
thr[, anchor_num := seq_len(.N)]
thr_tex <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{Operational thresholds and their roles. The regional-mean monthly accumulation is the headline FLOOD definition; the mine-cell daily variant is a conservative point-level lower bound. Source anchors are listed in the footnote.}\n",
  "\\label{tab:thresholds}\n",
  "\\small\n",
  "\\begin{tabular}{p{3.0cm}p{3.8cm}p{2.8cm}p{4.4cm}}\n",
  "\\toprule\n",
  "\\textbf{Channel} & \\textbf{Threshold} & \\textbf{Resolution} & \\textbf{Role} \\\\\n",
  "\\midrule\n",
  paste(apply(thr, 1, function(r) {
    sprintf("\\texttt{%s}$^{%s}$ & %s & %s & %s \\\\",
            gsub("_","\\\\_", r["channel"]),
            r["anchor_num"],
            r["physical_threshold"],
            r["temporal_resolution"],
            r["role"])
  }), collapse = "\n"),
  "\n\\bottomrule\n\\end{tabular}\n",
  "\\\\[2pt]\n",
  "{\\footnotesize\\raggedright\n",
  paste(apply(thr, 1, function(r) {
    sprintf("$^{%s}$\\,%s.",
            r["anchor_num"],
            r["source_anchor"])
  }), collapse = "\\quad\n"),
  "\n}\n",
  "\\end{table}\n"
)
writeLines(thr_tex, file.path(TABLES, "tex__thresholds.tex"))

# ----------------------- 2.  Acute headline regression table --------------
if (file.exists(file.path(TABLES, "40__regressions_acute.csv"))) {
  acu <- fread(file.path(TABLES, "40__regressions_acute.csv"))
  loor <- fread(file.path(TABLES, "40__loo_region_summary.csv"))
  loor2 <- loor[, .(label, loo_min = beta_min, loo_max = beta_max)]
  acu <- merge(acu, loor2, by = "label", all.x = TRUE)

  desired <- c("FLOOD_mine_A", "FLOOD_mine_B",
               "FLOOD_regmean_A", "FLOOD_regmean_B",
               "HEAT_mine_A", "HEAT_mine_B")
  acu[, ord := match(label, desired)]; acu <- acu[!is.na(ord)][order(ord)]
  acu[, display := paste0("\\texttt{", gsub("_","\\\\_", label), "}")]

  body <- paste(apply(acu, 1, function(r) {
    sprintf("%s & %s & %s & %s & %s & %s & [%s, %s] \\\\",
            r["display"],
            f1(as.numeric(r["beta"]), 3),
            f1(as.numeric(r["se"]),   3),
            fp(as.numeric(r["p_cr0"])),
            fp(as.numeric(r["p_wcb"])),
            r["n_obs"],
            f1(as.numeric(r["loo_min"]), 3),
            f1(as.numeric(r["loo_max"]), 3))
  }), collapse = "\n")

  acu_tex <- paste0(
    "\\begin{table}[H]\n\\centering\n",
    "\\caption{Acute mine-month regressions. Two p-values are reported: CR0 (asymptotic cluster-robust at region, 7 clusters) and WCB (wild-cluster bootstrap-t, Rademacher weights, null imposed, B = 999). LOO-region range reports the minimum and maximum coefficient across the seven leave-one-region-out refits.}\n",
    "\\label{tab:acute-regressions}\n",
    "\\small\n",
    "\\begin{tabular}{lrrrrrr}\n",
    "\\toprule\n",
    "\\textbf{Specification} & $\\hat\\beta$ & SE & $p_{\\text{CR0}}$ & $p_{\\text{WCB}}$ & $N$ & \\textbf{LOO range} \\\\\n",
    "\\midrule\n",
    body,
    "\n\\bottomrule\n\\end{tabular}\n",
    "\\begin{tablenotes}\\small\\item Significance: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.\\end{tablenotes}\n",
    "\\end{table}\n"
  )
  writeLines(acu_tex, file.path(TABLES, "tex__acute_regressions.tex"))
  cat("[70] wrote tex__acute_regressions.tex\n")
}

# ----------------------- 3.  Drought annual table -------------------------
if (file.exists(file.path(TABLES, "41__regressions_drought.csv"))) {
  dro <- fread(file.path(TABLES, "41__regressions_drought.csv"))
  loor <- fread(file.path(TABLES, "41__loo_region_drought.csv"))
  loor_sum <- loor[, .(loo_min = min(beta), loo_max = max(beta),
                       loo_pmax = max(p, na.rm = TRUE)), by = .(label, term)]
  dro <- merge(dro, loor_sum, by = c("label","term"), all.x = TRUE)
  desired <- c("cum3_stress_SPEI_meteo", "cum3_stress_SSI_hydro",
               "cum3_threshold_SPEI", "cum3_threshold_SSI")
  fes <- c("mineYR", "mineYR_trend")
  dro[, ord := paste(match(label, desired), match(fe, fes))]
  dro <- dro[label %in% desired & fe %in% fes][order(ord)]
  dro[, label_tex := dplyr::recode(label,
                                   cum3_stress_SPEI_meteo = "ENSO-attrib.\\ stress, SPEI-12",
                                   cum3_stress_SSI_hydro  = "ENSO-attrib.\\ stress, SSI-12",
                                   cum3_threshold_SPEI    = "Threshold-only, SPEI-12",
                                   cum3_threshold_SSI     = "Threshold-only, SSI-12")]
  dro[, fe_tex := dplyr::recode(fe,
                                mineYR = "mine + year FE",
                                mineYR_trend = "mine + year + mine$\\times$year FE")]
  # Dynamic SSI sample size + count of mines with a valid source-basin gauge
  ssi_n <- as.integer(dro[label == "cum3_stress_SSI_hydro" & fe == "mineYR", n_obs][1])
  xw_d <- fread(file.path(PROJECT_ROOT, "mining_module", "registry",
                          "mine_basin_crosswalk.csv"))
  xw_d[, ssi_valid := as.logical(ssi_valid)]
  n_ssi_mines <- xw_d[ssi_valid == TRUE, .N]
  body <- paste(apply(dro, 1, function(r) {
    loo_cell <- if (r["fe"] == "mineYR") {
      sprintf("[%s, %s]",
              f1(as.numeric(r["loo_min"]), 3),
              f1(as.numeric(r["loo_max"]), 3))
    } else {
      "--"
    }
    sprintf("%s & %s & %s & %s & %s & %s \\\\",
            r["label_tex"],
            r["fe_tex"],
            f1(as.numeric(r["beta"]), 3),
            f1(as.numeric(r["se"]), 3),
            fp(as.numeric(r["p_cr0"])),
            loo_cell)
  }), collapse = "\n")

  dro_tex <- paste0(
    "\\begin{table}[H]\n\\centering\n",
    "\\caption{DROUGHT mine-year regressions on 3-year cumulative shares. The mine-year FE specification is the headline; the additional mine$\\times$year linear-trend specification shows how the coefficient evolves with explicit mine-specific trajectories. LOO-region range is computed for the headline FE only; trend-row cells are blank.}\n",
    "\\label{tab:drought-regressions}\n",
    "\\small\n",
    "\\begin{tabular}{llrrrr}\n",
    "\\toprule\n",
    "\\textbf{Regressor} & \\textbf{FE} & $\\hat\\beta$ & SE & $p_{\\text{CR0}}$ & \\textbf{LOO range} \\\\\n",
    "\\midrule\n",
    body,
    "\n\\bottomrule\n\\end{tabular}\n",
    sprintf("\\begin{tablenotes}\\small\\item Spec: $\\log(\\text{kt}_y)\\sim\\text{cum}_{3}$ averaged over $y\\!-\\!1,y\\!-\\!2,y\\!-\\!3$. SPEI-12 rows: $N=369$ mine-years (all 19 operations). SSI-12 rows: $N=%d$ (the %d operations with a representative continental source-basin gauge; the endorheic-salar, fully-desalinated, and coastal operations enter through SPEI-12 only). Annual copper price is collinear with year FE and is omitted. Cluster SE at region (7).\\end{tablenotes}\n", ssi_n, n_ssi_mines),
    "\\end{table}\n"
  )
  writeLines(dro_tex, file.path(TABLES, "tex__drought_regressions.tex"))
  cat("[70] wrote tex__drought_regressions.tex\n")
}

# ----------------------- 3b. ENSO-attribution robustness table ------------
if (file.exists(file.path(TABLES, "43__enso_robustness.csv"))) {
  rb <- fread(file.path(TABLES, "43__enso_robustness.csv"))
  rb[, rule_tex := dplyr::recode(rule,
                                 baseline          = "Baseline (3-mo ONI, Pearson $\\rho$)",
                                 `seasonal-ONI`    = "Seasonal-window ONI (JJA / DJF)",
                                 `spearman-rho`    = "Spearman $\\rho$",
                                 `phase-cond-Zbar` = "Phase-conditional $\\overline{Z}$")]
  rb[, spec_tex := dplyr::recode(spec,
                                 FLOOD_regmean_stress  = "FLOOD regional-mean stress (Spec B)",
                                 HEAT_mine_stress      = "HEAT mine-cell stress (Spec B)",
                                 DROUGHT_SPEI_cum3     = "DROUGHT 3-yr cum SPEI-12")]
  rb[, spec_tex := factor(spec_tex, levels = c("FLOOD regional-mean stress (Spec B)",
                                               "HEAT mine-cell stress (Spec B)",
                                               "DROUGHT 3-yr cum SPEI-12"))]
  rb[, rule_tex := factor(rule_tex, levels = c(
    "Baseline (3-mo ONI, Pearson $\\rho$)",
    "Seasonal-window ONI (JJA / DJF)",
    "Spearman $\\rho$",
    "Phase-conditional $\\overline{Z}$"))]
  setorder(rb, spec_tex, rule_tex)

  body <- paste(apply(rb, 1, function(r) {
    sprintf("%s & %s & %s & %s & %s & %s \\\\",
            r["spec_tex"], r["rule_tex"],
            f1(as.numeric(r["beta"]), 3),
            f1(as.numeric(r["se"]),   3),
            fp(as.numeric(r["p"])),
            r["n"])
  }), collapse = "\n")

  rb_tex <- paste0(
    "\\begin{table}[H]\n\\centering\n",
    "\\caption{ENSO-attribution robustness. Each headline coefficient is re-fitted under three alternatives to the baseline ENSO-attribution rule: a seasonal-window ONI (JJA for DROUGHT and FLOOD; DJF for HEAT) in place of the 3-month rolling ONI; Spearman rank correlation in place of the Pearson teleconnection $\\rho$; and the phase-conditional mean $\\overline{Z}^v_{r,m,\\text{phase}}$ in place of the symmetric sign test $\\text{sign}(\\rho \\cdot \\text{ONI})$. All fits cluster SE at region (7).}\n",
    "\\label{tab:enso-robust}\n",
    "\\small\n",
    "\\begin{tabular}{llrrrr}\n",
    "\\toprule\n",
    "\\textbf{Specification} & \\textbf{ENSO-attribution rule} & $\\hat\\beta$ & SE & $p_{\\text{CR0}}$ & $N$ \\\\\n",
    "\\midrule\n",
    body,
    "\n\\bottomrule\n\\end{tabular}\n",
    "\\end{table}\n"
  )
  writeLines(rb_tex, file.path(TABLES, "tex__enso_robustness.tex"))
  cat("[70] wrote tex__enso_robustness.tex\n")
}

# ----------------------- 4.  Validation recovery table --------------------
if (file.exists(file.path(TABLES, "42__recovery_rate_summary.csv"))) {
  rec <- fread(file.path(TABLES, "42__recovery_rate_summary.csv"))
  rec[, channel_tex := dplyr::recode(channel,
    FLOOD_mine      = "FLOOD mine-cell daily (conservative lower bound)",
    FLOOD_regmean_M = "FLOOD regional-mean monthly (headline)")]
  rec_tex <- paste0(
    "\\begin{table}[H]\n\\centering\n",
    "\\caption{Validation against 20 documented Chilean flood-disruption episodes 2015--2024. ",
    "Each row reports the share of events at which the threshold-flag, ENSO-aligned flag, and stress flag fire under the listed FLOOD definition.}\n",
    "\\label{tab:validation}\n",
    "\\small\n",
    "\\begin{tabular}{lrrr}\n",
    "\\toprule\n",
    "\\textbf{Definition} & \\textbf{Threshold} & \\textbf{ENSO-aligned} & \\textbf{Stress flag} \\\\\n",
    "\\midrule\n",
    paste(apply(rec, 1, function(r) {
      sprintf("%s & %s/20 (%s\\%%) & %s/20 (%s\\%%) & %s/20 (%s\\%%) \\\\",
              r["channel_tex"],
              r["n_thr_fires"], r["pct_thr"],
              r["n_aligned_fires"], r["pct_aligned"],
              r["n_stress_fires"], r["pct_stress"])
    }), collapse = "\n"),
    "\n\\bottomrule\n\\end{tabular}\n\\end{table}\n"
  )
  writeLines(rec_tex, file.path(TABLES, "tex__validation.tex"))
  cat("[70] wrote tex__validation.tex\n")
}

# ----------------------- 5.  Mine inventory table -------------------------
mines <- fread(file.path(REGISTRY, "mines.csv"))
co    <- fread(file.path(ORIG_PANELS, "01a__cochilco_copper_mine_monthly.csv"))
cum_kt <- co[, .(cum_kt = round(sum(copper_kt, na.rm = TRUE))), by = mine_operation]
m2 <- merge(mines, cum_kt, by = "mine_operation", all.x = TRUE)

# Source-basin SSI-12 gauge from the authoritative crosswalk (final alignment).
# Mines without a representative streamflow gauge (endorheic salar systems and
# the coastal cross-region aggregate) carry no SSI-12 and are shown as SPEI-only.
xw <- fread(file.path(PROJECT_ROOT, "mining_module", "registry",
                      "mine_basin_crosswalk.csv"))
xw[, ssi_valid := as.logical(ssi_valid)]
# Clean short display labels for the gauge column (river + station qualifier).
gauge_short <- c(
  "RIO LOA ANTES REPRESA LEQUENA"                    = "R\\'io Loa (Lequena)",
  "RIO PIGA EN COLLACAGUA"                           = "R\\'io Piga (Collacagua)",
  "RIO CACHAPOAL 5 KM. AGUAS ABAJO JUNTA CORTADERAL" = "R\\'io Cachapoal (Cortaderal)",
  "RIO CHOAPA EN CUNCUMEN"                           = "R\\'io Choapa (Cuncum\\'en)",
  "RIO MAIPO EN EL MANZANO"                          = "R\\'io Maipo (El Manzano)",
  "RIO ACONCAGUA EN CHACABUQUITO"                    = "R\\'io Aconcagua (Chacabuquito)",
  "RIO COPIAPO EN PASTILLO"                          = "R\\'io Copiap\\'o (Pastillo)",
  "RIO HUATACONDO EN COPAQUIRE"                      = "R\\'io Huatacondo (Copaquire)",
  "RIO LA OLA EN VERTEDERO"                          = "R\\'io La Ola (Pedernales)")
reason_label <- c(endorheic         = "Endorheic basin --- SPEI-12 only",
                  desalinated       = "Desalinated (seawater) --- SPEI-12 only",
                  `coastal-aggregate` = "Coastal / cross-region --- SPEI-12 only")
xw[, gauge_disp := ifelse(ssi_valid,
                          ifelse(primary_station_name %in% names(gauge_short),
                                 gauge_short[primary_station_name], primary_station_name),
                          ifelse(ssi_exclude_reason %in% names(reason_label),
                                 reason_label[ssi_exclude_reason],
                                 "Not assigned --- SPEI-12 only"))]
m2 <- merge(m2, xw[, .(mine_operation, gauge_disp)], by = "mine_operation", all.x = TRUE)
m2[is.na(gauge_disp), gauge_disp := "Not assigned --- SPEI-12 only"]
m2[, region_short := dplyr::recode(codregion,
                                   `1`="I","2"="II","3"="III","4"="IV","5"="V",
                                   `6`="VI","13"="RM")]
m2 <- m2[order(-cum_kt)]
m_tex <- paste0(
  "\\begin{table}[H]\n\\centering\n",
  "\\caption{The 19 copper-mining operations and their key attributes. The final column is the standardised-streamflow (SSI-12) source-basin gauge assigned to each mine; operations drawing from closed endorheic salar systems with no representative streamflow gauge enter the slow water-stress analysis through the mine-cell SPEI-12 only.}\n",
  "\\label{tab:mines}\n",
  "\\footnotesize\n",
  "\\begin{tabular}{lllrrl}\n",
  "\\toprule\n",
  "\\textbf{Operation} & \\textbf{Region} & \\textbf{Altitude tier} & \\textbf{Elev.\\ (m)} & ",
  "\\textbf{Cum.\\ kt} & \\textbf{SSI-12 source-basin gauge} \\\\\n",
  "\\midrule\n",
  paste(apply(m2, 1, function(r) {
    sprintf("%s & %s & %s & %s & %s & %s \\\\",
            r["mine_operation"], r["region_en"], r["altitude_tier"],
            r["approx_elev_m"], r["cum_kt"], r["gauge_disp"])
  }), collapse = "\n"),
  "\n\\bottomrule\n\\end{tabular}\n\\end{table}\n"
)
writeLines(m_tex, file.path(TABLES, "tex__mines.tex"))
cat("[70] wrote tex__mines.tex\n")
cat("[70] done.\n")
