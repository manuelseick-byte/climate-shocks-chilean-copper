# =============================================================================
# 19_drought_ri_summary.R
#
# Supervisor revision (2026-06-01): close the DROUGHT inference asymmetry and
# make the effect size interpretable.  ALL outputs are built on the SAME
# matched-rho ENSO-aligned SPEI-12 regressor that produces the Table 7 headline
# (beta = -0.277, p_CR0 = 0.025, N = 369) -- reconstructed here exactly as in
# 17_matched_robustness_battery.R so the new outputs reconcile with Table 7.
#
# IMPORTANT: panel 31's pre-built cum3_str_meteo uses the ORIGINAL current-month
# rho gate (beta = -0.228, N = 417) and is NOT the Table 7 headline.  We rebuild
# the matched-rho gate from scratch here.
#
#   (1) tables/19__drought_loo_region.csv   leave-one-region-out (matched-rho)
#   (2) tables/19__drought_ri.csv           randomisation-inference p-value
#   (3) tables/19__exposure_summary.csv     regressor mean/SD/IQR + pctile effect
#   (4) tables/19__drought_headline_for_fig.csv  beta/se/p for fig4 (all-drought
#                                                & ENSO-aligned), so the forest
#                                                plot CIs match Table 7 exactly.
# =============================================================================

.find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "CLAUDE.md"))) { p <- dirname(d); if (p==d) stop("no root"); d <- p }
  d
}
ROOT <- .find_root()
source(file.path(ROOT, "mining_module_alternative", "scripts", "00_setup.R"))
suppressPackageStartupMessages({library(arrow); library(data.table); library(fixest)})

# ---- Rebuild matched-rho SPEI-12 gate (mirrors script 17, k = 12) -----------
ec <- as.data.table(read_parquet(file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")))
ec <- ec[codregion %in% COPPER_REGIONS, .(codregion, year, month, oni_rolling, Z_pr_m, rho_pr_m)]
setorder(ec, codregion, year, month)
ec[, oni12 := frollmean(oni_rolling, 12), by = codregion]
ec[, z12   := frollmean(Z_pr_m,     12), by = codregion]
# matched rho_12 = cor(12-month Z, 12-month ONI) on 1980-2010 by (region, calendar-month)
rho12 <- ec[year>=1980 & year<=2010, .(rhok12 = {
    a <- z12; b <- oni12; ok <- is.finite(a) & is.finite(b)
    if (sum(ok) >= 10) suppressWarnings(cor(a[ok], b[ok])) else NA_real_ }),
    by = .(codregion, month)]
ec <- merge(ec, rho12, by = c("codregion","month"), all.x = TRUE)
ec[, gateM12 := as.integer(!is.na(oni12) & abs(oni12) >= 0.5 &
                           !is.na(rhok12) & rhok12*oni12 < 0 &
                           !is.na(z12) & z12 < 0)]

# ---- mine-cell SPEI-12 flags + annual shares (12 valid months) --------------
mc <- as.data.table(read_parquet(file.path(PANELS, "14__minecell_spei_multiscale_1960_2025.parquet")))
mc <- mc[, .(mine_operation, codregion, year, month,
             f12 = drought_flag_spei12, v12 = as.integer(!is.na(spei12)))]
mc <- merge(mc, ec[, .(codregion, year, month, gateM12)], by = c("codregion","year","month"), all.x = TRUE)
a <- mc[, .(nv  = sum(v12),
            thr = mean(f12),
            str = mean(as.integer(f12==1 & !is.na(gateM12) & gateM12==1))),
        by = .(mine_operation, codregion, year)]
a[nv < 12, c("thr","str") := NA_real_]
setorder(a, mine_operation, year)
cum_lag <- function(d, col) {
  for (l in 1:3) d[, paste0(".l",l) := shift(get(col), l), by = mine_operation]
  v <- d[, rowMeans(.SD), .SDcols = paste0(".l",1:3)]      # complete 3-lag window
  d[, paste0(".l",1:3) := NULL]; v
}
a[, cum3_str := cum_lag(a, "str")]
a[, cum3_thr := cum_lag(a, "thr")]

# ---- outcome + headline sample (in_sample, price years -> N = 369) ----------
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))[in_sample==TRUE,
        .(mine_operation, codregion, year, log_kt_year)]
cp <- fread(file.path(PROJECT_ROOT, "Data_raw/BCCh/copper_lme_monthly_usd_per_mt.csv"))
cp[, year := as.integer(format(as.Date(paste0(date,"-01")),"%Y"))]
ann <- merge(ann, unique(cp[,.(year)])[, p1 := 1], by = "year", all.x = TRUE)[p1 == 1]
D <- merge(ann, a[, .(mine_operation, year, cum3_str, cum3_thr)], by = c("mine_operation","year"), all.x = TRUE)
D[, `:=`(mine = factor(mine_operation), yr = factor(year), region_clust = factor(codregion))]
De <- D[!is.na(cum3_str)]

# ---- headline sanity gate ---------------------------------------------------
m_en  <- feols(log_kt_year ~ cum3_str | mine + yr, data = De, cluster = ~region_clust, notes = FALSE)
m_all <- feols(log_kt_year ~ cum3_thr | mine + yr, data = D[!is.na(cum3_thr)], cluster = ~region_clust, notes = FALSE)
b0 <- coeftable(m_en)["cum3_str","Estimate"]; p0 <- coeftable(m_en)["cum3_str","Pr(>|t|)"]
se0 <- coeftable(m_en)["cum3_str","Std. Error"]
ba <- coeftable(m_all)["cum3_thr","Estimate"]; pa <- coeftable(m_all)["cum3_thr","Pr(>|t|)"]
sea <- coeftable(m_all)["cum3_thr","Std. Error"]
cat(sprintf("KEY headline_enso_beta: %+.4f\n", b0))
cat(sprintf("KEY headline_enso_p: %.4f\n", p0))
cat(sprintf("KEY headline_enso_N: %d\n", nobs(m_en)))
cat(sprintf("KEY headline_all_beta: %+.4f\n", ba))
cat(sprintf("KEY headline_all_p: %.4f\n", pa))
if (abs(b0 - (-0.277)) > 0.01)
  cat(sprintf("!! WARNING: reconstructed beta %.4f != Table 7 headline -0.277\n", b0))

# fig4 source (guarantees the forest plot reconciles with Table 7)
fwrite(data.table(
  spec = c("All-drought (threshold-only)", "ENSO-aligned (matched-rho, SPEI-12)"),
  beta = c(ba, b0), se = c(sea, se0), p = c(pa, p0), n = c(nobs(m_all), nobs(m_en))),
  file.path(TABLES, "19__drought_headline_for_fig.csv"))

# ---- (1) leave-one-region-out (matched-rho ENSO-aligned) --------------------
regs <- sort(unique(De$codregion))
loo <- rbindlist(lapply(regs, function(rr) {
  m <- feols(log_kt_year ~ cum3_str | mine + yr, data = De[codregion != rr],
             cluster = ~region_clust, notes = FALSE)
  ct <- coeftable(m)
  data.table(region_dropped = rr, beta = ct["cum3_str","Estimate"],
             se = ct["cum3_str","Std. Error"], p = ct["cum3_str","Pr(>|t|)"], n_obs = nobs(m))
}))
fwrite(loo, file.path(TABLES, "19__drought_loo_region.csv"))
cat(sprintf("KEY loo_beta_min: %+.4f\n", min(loo$beta)))
cat(sprintf("KEY loo_beta_max: %+.4f\n", max(loo$beta)))
cat(sprintf("KEY loo_p_min: %.4f\n", min(loo$p)))
cat(sprintf("KEY loo_p_max: %.4f\n", max(loo$p)))
cat(sprintf("KEY loo_all_negative: %s\n", all(loo$beta < 0)))
cat(sprintf("KEY loo_n_sig05: %d of %d\n", sum(loo$p < 0.05), nrow(loo)))

# ---- (2) randomisation inference --------------------------------------------
# Sharp null: exposure has no effect for any mine, so the ENSO-aligned exposure
# assignment is exchangeable across mines WITHIN each year (this holds the year
# FE fixed exactly; mine FE is re-estimated each draw).  RI builds an exact
# reference distribution and does not rely on a 7-cluster SE estimate, which is
# why it is used here instead of the wild-cluster bootstrap (unstable at N=369).
set.seed(20260601L)
B <- 2000L
yr_idx <- split(seq_len(nrow(De)), De$year)
x_orig <- De$cum3_str
perm_beta <- numeric(B)
for (b in seq_len(B)) {
  xp <- x_orig
  for (ix in yr_idx) if (length(ix) > 1) xp[ix] <- x_orig[sample(ix)]
  De[, .xperm := xp]
  mb <- feols(log_kt_year ~ .xperm | mine + yr, data = De, cluster = ~region_clust, notes = FALSE)
  ct <- coeftable(mb)
  perm_beta[b] <- if (".xperm" %in% rownames(ct)) ct[".xperm","Estimate"] else NA_real_
}
De[, .xperm := NULL]
perm_beta <- perm_beta[is.finite(perm_beta)]
p_perm   <- (1 + sum(perm_beta <= b0)) / (1 + length(perm_beta))      # one-sided, +1 correction
p_perm2  <- (1 + sum(abs(perm_beta) >= abs(b0))) / (1 + length(perm_beta))
fwrite(data.table(stat = c("p_perm_one_sided","p_perm_two_sided","B_effective","placebo_mean","placebo_sd"),
                  value = c(p_perm, p_perm2, length(perm_beta), mean(perm_beta), sd(perm_beta))),
       file.path(TABLES, "19__drought_ri.csv"))
cat(sprintf("KEY perm_p_one_sided: %.4f\n", p_perm))
cat(sprintf("KEY perm_p_two_sided: %.4f\n", p_perm2))
cat(sprintf("KEY perm_B: %d\n", length(perm_beta)))
cat(sprintf("KEY perm_placebo_sd: %.4f\n", sd(perm_beta)))

# ---- (3) exposure-share summary + percentile effect size --------------------
summ <- function(x, nm) {
  x <- x[is.finite(x)]; q <- quantile(x, c(.25,.5,.75))
  data.table(regressor = nm, mean = mean(x), sd = sd(x),
             p25 = q[[1]], p50 = q[[2]], p75 = q[[3]], iqr = q[[3]]-q[[1]], max = max(x))
}
es <- rbind(summ(De$cum3_str, "ENSO-aligned cum3 (matched-rho, SPEI-12)"),
            summ(D[!is.na(cum3_thr)]$cum3_thr, "All-drought cum3 (threshold-only)"))
fwrite(es, file.path(TABLES, "19__exposure_summary.csv"))
iqr_enso <- es[1, iqr]
cat(sprintf("KEY enso_mean: %.4f\n", es[1, mean]))
cat(sprintf("KEY enso_sd: %.4f\n", es[1, sd]))
cat(sprintf("KEY enso_p25: %.4f\n", es[1, p25]))
cat(sprintf("KEY enso_p75: %.4f\n", es[1, p75]))
cat(sprintf("KEY enso_iqr: %.4f\n", iqr_enso))
cat(sprintf("KEY enso_max: %.4f\n", es[1, max]))
cat(sprintf("KEY eff_per10pp_pct: %+.3f\n", 100 * b0 * 0.10))
cat(sprintf("KEY eff_iqr_pct: %+.3f\n", 100 * b0 * iqr_enso))
cat(sprintf("KEY eff_p25p75_move_pp: %.1f\n", 100 * iqr_enso))
cat(sprintf("KEY all_mean: %.4f\n", es[2, mean]))
cat(sprintf("KEY all_iqr: %.4f\n", es[2, iqr]))
cat("KEY done: 1\n")
