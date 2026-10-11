# =============================================================================
# 41_regressions_drought.R
#
# Mine x year annual regressions for the slow water-stress channel.
# Headline specification:
#   log(copper_kt_year) ~ cum3_str_meteo | mine + year
# where cum3_str_meteo = mean(str_share_meteo over y-1, y-2, y-3).
# Annual copper price is collinear with year FE and is dropped.
#
# Robustness:
#   - SSI replacement (cum3_str_hydro) - cross-index check on the same physical
#     channel (multi-year water deficit)
#   - threshold-only variant (cum3_thr_meteo / cum3_thr_hydro)
#   - leave-one-region-out and leave-one-year-out
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
ann <- ann[in_sample == TRUE]
# add annual log-price (mean monthly log price in the year)
DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")
cp <- fread(file.path(DATA_RAW, "BCCh", "copper_lme_monthly_usd_per_mt.csv"))
cp[, date := as.Date(paste0(date, "-01"))]
cp[, year := as.integer(format(date, "%Y"))]
cp[, log_price := log(price_usd_mt)]
yp <- cp[, .(log_price_year = mean(log_price, na.rm = TRUE)), by = year]
ann <- merge(ann, yp, by = "year", all.x = TRUE)
ann <- ann[!is.na(log_price_year)]

ann[, mine := factor(mine_operation)]
ann[, yr := factor(year)]
ann[, region_clust := factor(codregion)]


cat("[41] drought annual panel: rows =", nrow(ann),
    " year range =", min(ann$year), "-", max(ann$year),
    " mines =", uniqueN(ann$mine_operation), "\n")

specs <- list(
  list(label = "cum3_stress_SPEI_meteo",  reg = "cum3_str_meteo"),
  list(label = "cum3_stress_SSI_hydro",   reg = "cum3_str_hydro"),
  list(label = "cum3_threshold_SPEI",     reg = "cum3_thr_meteo"),
  list(label = "cum3_threshold_SSI",      reg = "cum3_thr_hydro")
)

# FE variants
fe_variants <- list(
  list(fe_label = "mineYR",       fe = "mine + yr"),
  list(fe_label = "mineYR_trend", fe = "mine + yr + mine[year]")   # linear mine trend
)

main_rows <- list(); loo_rows <- list(); year_loo_rows <- list()

for (s in specs) {
  for (fev in fe_variants) {
    f <- sprintf("log_kt_year ~ %s | %s", s$reg, fev$fe)
    m <- try(feols(as.formula(f), data = ann, cluster = ~ region_clust, notes = FALSE),
             silent = TRUE)
    if (inherits(m, "try-error")) { cat("  failed:", s$label, fev$fe_label, "\n"); next }
    ct <- coeftable(m)
    if (!s$reg %in% rownames(ct)) next
    beta <- unname(ct[s$reg, "Estimate"])
    se   <- unname(ct[s$reg, "Std. Error"])
    p_cr0 <- unname(ct[s$reg, "Pr(>|t|)"])
    main_rows[[length(main_rows)+1]] <- data.table(
      label = s$label, fe = fev$fe_label, term = s$reg,
      beta = beta, se = se, p_cr0 = p_cr0,
      n_obs = nobs(m), n_clust = 7
    )
    cat(sprintf("[41] %-25s %s  b=%+0.3f  SE=%0.3f  pCR0=%0.3f\n",
                s$label, fev$fe_label, beta, se, p_cr0))

    # LOO-region (only on the baseline FE)
    if (fev$fe_label == "mineYR") {
      for (rg in unique(ann$codregion)) {
        sub <- ann[codregion != rg]
        mlr <- try(feols(as.formula(f), data = sub, cluster = ~ region_clust, notes = FALSE),
                   silent = TRUE)
        if (inherits(mlr, "try-error")) next
        ct2 <- coeftable(mlr)
        if (!s$reg %in% rownames(ct2)) next
        loo_rows[[length(loo_rows)+1]] <- data.table(
          label = s$label, term = s$reg, drop = paste0("region_", rg),
          beta = unname(ct2[s$reg, "Estimate"]), p = unname(ct2[s$reg, "Pr(>|t|)"])
        )
      }
      # LOO-year
      for (yy in unique(ann$year)) {
        sub <- ann[year != yy]
        mle <- try(feols(as.formula(f), data = sub, cluster = ~ region_clust, notes = FALSE),
                   silent = TRUE)
        if (inherits(mle, "try-error")) next
        ct2 <- coeftable(mle)
        if (!s$reg %in% rownames(ct2)) next
        year_loo_rows[[length(year_loo_rows)+1]] <- data.table(
          label = s$label, term = s$reg, drop = paste0("year_", yy),
          beta = unname(ct2[s$reg, "Estimate"]), p = unname(ct2[s$reg, "Pr(>|t|)"])
        )
      }
    }
  }
}

# --- Decomposition (supervisor Priority 1): ENSO-attributable vs non-ENSO ----
# Both drought regressors in one model.  If the ENSO-attributable coefficient
# stays negative while non-ENSO is null, the ENSO interpretation is credible;
# if it collapses, the headline was just selecting the megadrought episode.
for (fev in fe_variants) {
  f <- sprintf("log_kt_year ~ cum3_str_meteo + cum3_nonenso_meteo | %s", fev$fe)
  m <- try(feols(as.formula(f), data = ann, cluster = ~ region_clust, notes = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) next
  ct <- coeftable(m)
  for (tm in c("cum3_str_meteo", "cum3_nonenso_meteo")) {
    if (!tm %in% rownames(ct)) next
    main_rows[[length(main_rows)+1]] <- data.table(
      label = "decomp_ENSO_vs_nonENSO", fe = fev$fe_label, term = tm,
      beta = unname(ct[tm,"Estimate"]), se = unname(ct[tm,"Std. Error"]),
      p_cr0 = unname(ct[tm,"Pr(>|t|)"]), n_obs = nobs(m), n_clust = 7)
  }
  cat(sprintf("[41] decomposition %s: ENSO b=%+.3f (p=%.3f) | nonENSO b=%+.3f (p=%.3f)\n",
              fev$fe_label, ct["cum3_str_meteo","Estimate"], ct["cum3_str_meteo","Pr(>|t|)"],
              ct["cum3_nonenso_meteo","Estimate"], ct["cum3_nonenso_meteo","Pr(>|t|)"]))
}

main_tbl <- rbindlist(main_rows, fill = TRUE)
loo_tbl <- rbindlist(loo_rows, fill = TRUE)
loo_yr  <- rbindlist(year_loo_rows, fill = TRUE)
fwrite(main_tbl, file.path(TABLES, "41__regressions_drought.csv"))
fwrite(loo_tbl,  file.path(TABLES, "41__loo_region_drought.csv"))
fwrite(loo_yr,   file.path(TABLES, "41__loo_year_drought.csv"))

cat("\n[41] HEADLINE DROUGHT TABLE\n"); print(main_tbl)
cat("\n[41] LOO-region drought range:\n")
print(loo_tbl[, .(n = .N, beta_min = round(min(beta), 3), beta_max = round(max(beta), 3),
                   p_min = round(min(p), 3), p_max = round(max(p), 3)),
              by = .(label, term)])
cat("\n[41] LOO-year drought range (showing first few):\n")
print(loo_yr[, .(n = .N, beta_min = round(min(beta), 3), beta_max = round(max(beta), 3),
                  p_min = round(min(p), 3), p_max = round(max(p), 3)),
             by = .(label, term)])
