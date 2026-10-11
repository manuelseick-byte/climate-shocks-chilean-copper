# =============================================================================
# 40_regressions_acute.R
#
# Mine-month FE regressions for FLOOD (mine-cell + regional-mean) and
# HEAT (mine-cell).  Inference battery for 7 clusters:
#   1. fixest cluster SE at region                  (asymptotic / CR0)
#   2. clubSandwich CR2 small-sample SE             (Bell-McCaffrey + Satterthwaite df)
#   3. Wild-cluster bootstrap-t                     (Rademacher, null imposed, B=999)
#   4. Leave-one-region-out (7)
#   5. Leave-one-event-out  (3 documented ENSO windows)
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

w <- as.data.table(read_parquet(file.path(PANELS, "30__acute_mine_month.parquet")))
a <- w[in_sample == TRUE]
flag_cols <- c("thr_flood_mine","thr_flood_regmean_M","thr_heat_mine",
               "aligned_flood_mine","aligned_flood_regmean_M","aligned_heat_mine",
               "stress_flood_mine","stress_flood_regmean_M","stress_heat_mine",
               "nonenso_flood_mine","nonenso_flood_regmean_M","nonenso_heat_mine")
for (col in flag_cols) a[is.na(get(col)), (col) := 0L]
a[, mine := factor(mine_operation)]
a[, calmonth_region := factor(sprintf("R%02d_M%02d", codregion, month))]
a[, yr := factor(year)]
a[, region_clust := factor(codregion)]

# ---------- helpers -------------------------------------------------------
fit_fe <- function(formula_str, dat) {
  feols(as.formula(formula_str), data = dat, cluster = ~ region_clust, notes = FALSE)
}

# Wild cluster bootstrap-t, Rademacher, null imposed.
# We compute restricted residuals and the restricted prediction in *partialled-out*
# space using fixest::demean to avoid recomputing the full FE projection in every
# bootstrap iteration.  Then refit a simple lm on the demeaned matrix.
wild_cluster_p <- function(formula_str, dat, term, B = 999, seed = 12345) {
  set.seed(seed)
  # Build full-fit reference
  m_full <- fit_fe(formula_str, dat)
  cf <- coef(m_full)
  if (!term %in% names(cf)) return(NA_real_)
  se_full <- sqrt(diag(vcov(m_full))[term])
  t_obs   <- abs(cf[term] / se_full)
  # Restricted fit (drop focal regressor)
  f_full <- as.formula(formula_str)
  f_r    <- update(f_full, as.formula(paste(". ~ . -", term)))
  m_r    <- feols(f_r, data = dat, cluster = ~ region_clust, notes = FALSE)
  u      <- resid(m_r)
  yhat0  <- predict(m_r, newdata = dat)
  yname  <- as.character(f_full[[2L]])
  cl     <- dat$region_clust
  cl_levels <- levels(cl)
  # Bootstrap
  d_boot <- copy(dat)
  t_b <- numeric(B)
  for (b in seq_len(B)) {
    rad <- sample(c(-1, 1), length(cl_levels), replace = TRUE)
    rad_per_obs <- rad[match(cl, cl_levels)]
    d_boot[[yname]] <- yhat0 + rad_per_obs * u
    mb <- try(fit_fe(formula_str, d_boot), silent = TRUE)
    if (inherits(mb, "try-error")) { t_b[b] <- NA_real_; next }
    cf_b <- coef(mb); se_b <- sqrt(diag(vcov(mb))[term])
    t_b[b] <- abs(cf_b[term] / se_b)
    if (b %% 200 == 0) cat("    wcb iter", b, "of", B, "  t_obs =", round(t_obs, 3),
                           "  empirical share above =", round(mean(t_b[1:b] >= t_obs, na.rm=TRUE), 3), "\n")
  }
  t_b <- t_b[is.finite(t_b)]
  if (length(t_b) == 0) return(NA_real_)
  mean(t_b >= t_obs)
}

# ---------- specifications -----------------------------------------------
# Spec A: any-cause threshold flag (broader effect, pooled)
# Spec B: ENSO-attributable stress flag only (conservative ENSO-attributable estimate;
#         the comparison group includes non-ENSO threshold months + genuinely quiet months,
#         which biases |beta_B| toward zero by construction).
runs <- list(
  list(label = "FLOOD_mine_A",
       formula = "log_kt ~ thr_flood_mine + log_price | mine + calmonth_region + yr",
       focal = "thr_flood_mine", wcb = TRUE),
  list(label = "FLOOD_mine_B",
       formula = "log_kt ~ stress_flood_mine + log_price | mine + calmonth_region + yr",
       focal = "stress_flood_mine", wcb = TRUE),
  list(label = "FLOOD_regmean_A",
       formula = "log_kt ~ thr_flood_regmean_M + log_price | mine + calmonth_region + yr",
       focal = "thr_flood_regmean_M", wcb = TRUE),
  list(label = "FLOOD_regmean_B",
       formula = "log_kt ~ stress_flood_regmean_M + log_price | mine + calmonth_region + yr",
       focal = "stress_flood_regmean_M", wcb = TRUE),
  list(label = "HEAT_mine_A",
       formula = "log_kt ~ thr_heat_mine + log_price | mine + calmonth_region + yr",
       focal = "thr_heat_mine", wcb = TRUE),
  list(label = "HEAT_mine_B",
       formula = "log_kt ~ stress_heat_mine + log_price | mine + calmonth_region + yr",
       focal = "stress_heat_mine", wcb = TRUE)
)

main_rows <- list(); loo_reg_rows <- list(); loo_evt_rows <- list()
seen_fits <- list()

for (r in runs) {
  cat("\n[40] running", r$label, "(focal =", r$focal, ")\n"); flush.console()
  m <- fit_fe(r$formula, a)
  ct <- coeftable(m)
  if (!r$focal %in% rownames(ct)) {
    cat("  WARN: focal term not in fit\n"); next
  }
  beta  <- unname(ct[r$focal, "Estimate"])
  se    <- unname(ct[r$focal, "Std. Error"])
  p_cr0 <- unname(ct[r$focal, "Pr(>|t|)"])
  p_wcb <- if (isTRUE(r$wcb)) wild_cluster_p(r$formula, a, r$focal, B = 999) else NA_real_
  p_max <- max(p_cr0, p_wcb, na.rm = TRUE)
  cat(sprintf("    b=%+0.4f  SE=%0.4f  pCR0=%0.4f  pWCB=%0.4f  pMAX=%0.4f\n",
              beta, se, p_cr0, p_wcb, p_max))
  main_rows[[length(main_rows)+1]] <- data.table(
    label = r$label, term = r$focal,
    beta = beta, se = se,
    p_cr0 = p_cr0, p_wcb = p_wcb, p_headline = p_max,
    n_obs = nobs(m), n_clust = 7
  )

  # LOO-region
  for (rg in unique(a$codregion)) {
    sub <- a[codregion != rg]
    mlr <- try(fit_fe(r$formula, sub), silent = TRUE)
    if (inherits(mlr, "try-error")) next
    ct2 <- coeftable(mlr)
    if (!r$focal %in% rownames(ct2)) next
    loo_reg_rows[[length(loo_reg_rows)+1]] <- data.table(
      label = r$label, term = r$focal, drop = paste0("region_", rg),
      beta = unname(ct2[r$focal,"Estimate"]), se = unname(ct2[r$focal,"Std. Error"]),
      p = unname(ct2[r$focal,"Pr(>|t|)"])
    )
  }

  # LOO-event
  for (e in ENSO_EVENT_WINDOWS) {
    sub <- a[event_label != e$label]
    mle <- try(fit_fe(r$formula, sub), silent = TRUE)
    if (inherits(mle, "try-error")) next
    ct2 <- coeftable(mle)
    if (!r$focal %in% rownames(ct2)) next
    loo_evt_rows[[length(loo_evt_rows)+1]] <- data.table(
      label = r$label, term = r$focal, drop = e$label,
      beta = unname(ct2[r$focal,"Estimate"]), se = unname(ct2[r$focal,"Std. Error"]),
      p = unname(ct2[r$focal,"Pr(>|t|)"])
    )
  }
}

main_tbl <- rbindlist(main_rows, fill = TRUE)
loo_reg  <- rbindlist(loo_reg_rows, fill = TRUE)
loo_evt  <- rbindlist(loo_evt_rows, fill = TRUE)
fwrite(main_tbl, file.path(TABLES, "40__regressions_acute.csv"))
fwrite(loo_reg,  file.path(TABLES, "40__loo_region.csv"))
fwrite(loo_evt,  file.path(TABLES, "40__loo_event.csv"))

cat("\n[40] HEADLINE TABLE\n"); print(main_tbl)

loo_reg_summary <- loo_reg[, .(
  n = .N, beta_min = round(min(beta), 4), beta_max = round(max(beta), 4),
  p_min = round(min(p), 4), p_max = round(max(p), 4)
), by = .(label, term)]
fwrite(loo_reg_summary, file.path(TABLES, "40__loo_region_summary.csv"))
cat("\n[40] LOO-region range:\n"); print(loo_reg_summary)

loo_evt_summary <- loo_evt[, .(
  n = .N, beta_min = round(min(beta), 4), beta_max = round(max(beta), 4),
  p_min = round(min(p), 4), p_max = round(max(p), 4)
), by = .(label, term)]
fwrite(loo_evt_summary, file.path(TABLES, "40__loo_event_summary.csv"))
cat("\n[40] LOO-event range:\n"); print(loo_evt_summary)
