# =============================================================================
# 44_fe_ladder.R
#
# Build a "ladder" of nested fixed-effects specifications for the supervisor's
# diagnostic exercise: start from the simplest pooled OLS, then add FE step-by-step,
# and watch the coefficient evolve.  Reports the headline acute regressors
# (FLOOD_regmean Spec B, FLOOD_regmean Spec A, FLOOD_mine Spec A, HEAT_mine
# Spec A and B) plus the headline drought regressor (cum3_str_meteo, cum3_str_hydro).
#
# For each focal regressor we report SEVEN steps:
#   M0:  pooled OLS (no FE, no price)
#   M1:  + log_price
#   M2:  + mine FE                                 (level absorbed)
#   M3:  + mine + year FE                          (mine level + national year shocks)
#   M4:  + mine + year + calmonth FE               (+ national seasonality)
#   M5:  + mine + year + region x calmonth FE      (+ regional seasonality -- HEADLINE)
#   M6:  + mine + year x calmonth + region x calmonth (saturated time control;
#                                                       collinearity check vs price)
#
# Standard errors are CR0 clustered at region.  We also extract the singularity
# diagnostics from fixest::feols (whether log_price was dropped, whether any FE
# levels were collinear).
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

# ---------- 1. Acute panel ------------------------------------------------
w <- as.data.table(read_parquet(file.path(PANELS, "30__acute_mine_month.parquet")))
a <- w[in_sample == TRUE]
flag_cols <- c("thr_flood_mine","thr_flood_regmean_M","thr_heat_mine",
               "stress_flood_mine","stress_flood_regmean_M","stress_heat_mine")
for (col in flag_cols) a[is.na(get(col)), (col) := 0L]
a[, mine            := factor(mine_operation)]
a[, calmonth        := factor(month)]
a[, calmonth_region := factor(sprintf("R%02d_M%02d", codregion, month))]
a[, yr              := factor(year)]
a[, yr_calmonth     := factor(sprintf("Y%04d_M%02d", year, month))]
a[, region_clust    := factor(codregion)]

# ---------- 2. Drought panel ----------------------------------------------
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
ann <- ann[in_sample == TRUE]
# Annual log-price (mean monthly log price)
DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")
cp <- fread(file.path(DATA_RAW, "BCCh", "copper_lme_monthly_usd_per_mt.csv"))
cp[, date := as.Date(paste0(date, "-01"))]
cp[, year := as.integer(format(date, "%Y"))]
cp[, log_price := log(price_usd_mt)]
yp <- cp[, .(log_price_year = mean(log_price, na.rm = TRUE)), by = year]
ann <- merge(ann, yp, by = "year", all.x = TRUE)
ann <- ann[!is.na(log_price_year)]
ann[, mine         := factor(mine_operation)]
ann[, yr           := factor(year)]
ann[, region_clust := factor(codregion)]

# ---------- 3. Acute ladder definition ------------------------------------
acute_steps <- list(
  list(step = "M0", label = "Pooled OLS",                   rhs = "",                          ctrl = ""),
  list(step = "M1", label = "+ log_price",                  rhs = "",                          ctrl = "+ log_price"),
  list(step = "M2", label = "+ mine FE",                    rhs = "| mine",                    ctrl = "+ log_price"),
  list(step = "M3", label = "+ mine + year FE",             rhs = "| mine + yr",               ctrl = "+ log_price"),
  list(step = "M4", label = "+ mine + year + calmonth FE",  rhs = "| mine + yr + calmonth",    ctrl = "+ log_price"),
  list(step = "M5", label = "+ mine + year + reg x calmonth FE  (HEADLINE)",
                                                            rhs = "| mine + yr + calmonth_region", ctrl = "+ log_price"),
  list(step = "M6", label = "+ mine + (year x calmonth) FE  (saturated time)",
                                                            rhs = "| mine + yr_calmonth + calmonth_region", ctrl = "+ log_price")
)

acute_focal <- list(
  list(label = "FLOOD_regmean_B", focal = "stress_flood_regmean_M"),
  list(label = "FLOOD_regmean_A", focal = "thr_flood_regmean_M"),
  list(label = "FLOOD_mine_A",     focal = "thr_flood_mine"),
  list(label = "FLOOD_mine_B",     focal = "stress_flood_mine"),
  list(label = "HEAT_mine_A",      focal = "thr_heat_mine"),
  list(label = "HEAT_mine_B",      focal = "stress_heat_mine")
)

run_one <- function(focal_var, step, dat) {
  rhs  <- step$rhs
  ctrl <- step$ctrl
  if (rhs == "") {
    f <- sprintf("log_kt ~ %s %s", focal_var, ctrl)
  } else {
    f <- sprintf("log_kt ~ %s %s %s", focal_var, ctrl, rhs)
  }
  m <- try(feols(as.formula(f), data = dat, cluster = ~ region_clust, notes = FALSE),
           silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  ct <- coeftable(m)
  out <- list(formula = f, m = m, ct = ct)
  out
}

acute_rows <- list()
for (fc in acute_focal) {
  for (s in acute_steps) {
    res <- run_one(fc$focal, s, a)
    if (is.null(res)) next
    ct <- res$ct
    if (!fc$focal %in% rownames(ct)) {
      acute_rows[[length(acute_rows) + 1]] <- data.table(
        spec = fc$label, focal = fc$focal, step = s$step, label = s$label,
        beta = NA_real_, se = NA_real_, p_cr0 = NA_real_, n_obs = nobs(res$m),
        price_in = grepl("log_price", res$formula, fixed = TRUE),
        price_kept = FALSE, formula = res$formula
      )
      next
    }
    price_kept <- "log_price" %in% rownames(ct)
    acute_rows[[length(acute_rows) + 1]] <- data.table(
      spec   = fc$label,
      focal  = fc$focal,
      step   = s$step,
      label  = s$label,
      beta   = unname(ct[fc$focal, "Estimate"]),
      se     = unname(ct[fc$focal, "Std. Error"]),
      p_cr0  = unname(ct[fc$focal, "Pr(>|t|)"]),
      n_obs  = nobs(res$m),
      price_in   = grepl("log_price", res$formula, fixed = TRUE),
      price_kept = price_kept,
      formula = res$formula
    )
  }
}
acute_ladder <- rbindlist(acute_rows, fill = TRUE)
fwrite(acute_ladder, file.path(TABLES, "44__fe_ladder_acute.csv"))

cat("\n[44] ACUTE LADDER\n")
print(acute_ladder[, .(spec, step, label, beta = round(beta, 4), se = round(se, 4),
                       p_cr0 = round(p_cr0, 3), n_obs, price_kept)])

# ---------- 4. Drought ladder --------------------------------------------
drought_steps <- list(
  list(step = "D0", label = "Pooled OLS",                     rhs = "",                       ctrl = ""),
  list(step = "D1", label = "+ log_price (annual mean)",       rhs = "",                       ctrl = "+ log_price_year"),
  list(step = "D2", label = "+ mine FE",                       rhs = "| mine",                 ctrl = "+ log_price_year"),
  list(step = "D3", label = "+ mine + year FE (HEADLINE)",     rhs = "| mine + yr",            ctrl = ""),
  list(step = "D4", label = "+ mine + year + mine-linear trend", rhs = "| mine + yr + mine[year]", ctrl = "")
)
drought_focal <- list(
  list(label = "DROUGHT_SPEI", focal = "cum3_str_meteo"),
  list(label = "DROUGHT_SSI",  focal = "cum3_str_hydro"),
  list(label = "DROUGHT_SPEI_threshold", focal = "cum3_thr_meteo"),
  list(label = "DROUGHT_SSI_threshold",  focal = "cum3_thr_hydro")
)

drought_rows <- list()
for (fc in drought_focal) {
  for (s in drought_steps) {
    rhs  <- s$rhs
    ctrl <- s$ctrl
    if (rhs == "") {
      f <- sprintf("log_kt_year ~ %s %s", fc$focal, ctrl)
    } else {
      f <- sprintf("log_kt_year ~ %s %s %s", fc$focal, ctrl, rhs)
    }
    m <- try(feols(as.formula(f), data = ann, cluster = ~ region_clust, notes = FALSE),
             silent = TRUE)
    if (inherits(m, "try-error")) next
    ct <- coeftable(m)
    if (!fc$focal %in% rownames(ct)) next
    drought_rows[[length(drought_rows) + 1]] <- data.table(
      spec   = fc$label,
      focal  = fc$focal,
      step   = s$step,
      label  = s$label,
      beta   = unname(ct[fc$focal, "Estimate"]),
      se     = unname(ct[fc$focal, "Std. Error"]),
      p_cr0  = unname(ct[fc$focal, "Pr(>|t|)"]),
      n_obs  = nobs(m),
      price_in   = grepl("log_price_year", f, fixed = TRUE),
      price_kept = "log_price_year" %in% rownames(ct),
      formula = f
    )
  }
}
drought_ladder <- rbindlist(drought_rows, fill = TRUE)
fwrite(drought_ladder, file.path(TABLES, "44__fe_ladder_drought.csv"))

cat("\n[44] DROUGHT LADDER\n")
print(drought_ladder[, .(spec, step, label, beta = round(beta, 3), se = round(se, 3),
                         p_cr0 = round(p_cr0, 3), n_obs, price_kept)])

# ---------- 5. Collinearity diagnostics for HEADLINE M5 spec -------------
cat("\n[44] HEADLINE COLLINEARITY DIAGNOSTICS  (FLOOD_regmean Spec B, M5)\n")
m_h <- feols(log_kt ~ stress_flood_regmean_M + log_price | mine + yr + calmonth_region,
             data = a, cluster = ~ region_clust)
cat("\n-- coeftable\n"); print(coeftable(m_h))
cat("\n-- fixef summary (lengths of each FE block)\n")
fe <- fixef(m_h)
for (nm in names(fe)) cat("  ", nm, ": n_levels =", length(fe[[nm]]), "\n")

# Is mine FE perfectly collinear with any region structure?
# Check: each mine maps to exactly one region.
cat("\n-- mine -> region mapping (each mine should belong to exactly one region)\n")
print(unique(a[, .(mine_operation, codregion)])[order(codregion, mine_operation)])

# Was log_price kept in the headline?
cat("\n-- log_price retained in M5?  ", "log_price" %in% rownames(coeftable(m_h)), "\n")

# What happens to log_price under M6 (saturated time)?
m6 <- feols(log_kt ~ stress_flood_regmean_M + log_price | mine + yr_calmonth + calmonth_region,
            data = a, cluster = ~ region_clust)
cat("-- log_price retained in M6 (year x calmonth saturated)? ",
    "log_price" %in% rownames(coeftable(m6)), "\n")
cat("\nM6 coefficient on stress_flood_regmean_M and (if retained) log_price:\n")
print(coeftable(m6))

cat("\n[44] DONE.\n")
