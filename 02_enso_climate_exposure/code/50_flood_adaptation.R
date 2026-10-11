# =============================================================================
# 50_flood_adaptation.R
#
# Adaptation test for the FLOOD channel:  did mines' output-sensitivity to a
# wet regional month attenuate after the 2015-16 El Nino?
#
# The 2015-16 El Nino is the operational inflection event for the Chilean
# mining industry:  the March 2015 Atacama aluvion + the broader 2015-16 El
# Nino period are the documented break (PSCC 2024; PNM2050 2020; ARCLIM 2020).
# The 2015-16 El Nino window in 00_setup.R is 2014-09 -> 2016-05.
#
# Design:
#   PRE   = 2006-01 ... 2014-08   (before the El Nino event)
#   EVENT = 2014-09 ... 2016-05   (the 2015-16 El Nino itself; EXCLUDED)
#   POST  = 2016-06 ... 2025-12   (after the El Nino event ends)
#
# Two specifications, both with mine + region x calmonth + year FE and the
# LME monthly copper-price control, cluster SE at region:
#
#   Spec A (GENERAL flood):
#     log_kt ~ thr_flood_regmean_M * Post + log_price | FE
#     beta_A1 = pre-event general-flood elasticity
#     beta_A2 = post-event attenuation (>0 means flood hurts less in POST)
#
#   Spec B (ENSO-attributable flood):
#     log_kt ~ stress_flood_regmean_M * Post + log_price | FE
#     beta_B1 = pre-event ENSO-attributable elasticity
#     beta_B2 = post-event attenuation
#
# The NOVELTY test is the COMPARISON of beta_B2 vs beta_A2:  if mines exploit
# the recurring / quasi-predictable nature of ENSO (NOAA forecasts give 6-12
# months lead), they can anticipate ENSO-aligned floods more than random
# storms, so beta_B2 > beta_A2 would be evidence of anticipation behaviour.
#
# Cuts:
#   1. National pooled  (all 19 mines, 7 regions)
#   2. Atacama-only     (the documented 2015-03 aluvion region;  3-4 mines)
#   3. Placebo north    (Tarapaca + Antofagasta;  not directly affected by
#                        the Atacama aluvion;  diagnoses generic post-2016
#                        calendar effects)
#
# Outputs (TABLES/):
#   50__flood_adaptation_main.csv         coefficients for all cuts/specs
#   50__flood_adaptation_byregion.csv     LOO-region for the headline
#   50__flood_adaptation_log.txt          run log
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

log_file <- file.path(TABLES, "50__flood_adaptation_log.txt")
sink(log_file, split = TRUE)
on.exit(sink(NULL), add = TRUE)

cat("=============================================================\n")
cat(" 50_flood_adaptation.R  --  run at ", format(Sys.time()), "\n", sep = "")
cat("=============================================================\n\n")

# ------------------------------------------------------------------
# 1. Load panel and define PRE / EVENT / POST
# ------------------------------------------------------------------
w <- as.data.table(read_parquet(file.path(PANELS, "30__acute_mine_month.parquet")))
a <- w[in_sample == TRUE]

# Force any NA flag to 0 (consistent with 40_regressions_acute.R)
flag_cols <- c("thr_flood_mine","thr_flood_regmean_M","thr_heat_mine",
               "stress_flood_mine","stress_flood_regmean_M","stress_heat_mine")
for (col in flag_cols) a[is.na(get(col)), (col) := 0L]

# The 2015-16 El Nino event window comes from 00_setup.R::ENSO_EVENT_WINDOWS
elnino_2015_16 <- ENSO_EVENT_WINDOWS[[1]]
stopifnot(elnino_2015_16$label == "2015-16 El Nino")
event_start <- elnino_2015_16$start    # 2014-09-01
event_end   <- elnino_2015_16$end      # 2016-05-31

a[, period := fifelse(date <  event_start, "PRE",
                fifelse(date >  event_end,  "POST", "EVENT"))]
a[, Post := as.integer(period == "POST")]

cat("[50] period assignment\n")
print(a[, .(n_mine_months = .N), by = period])
cat("\n")

# Active sub-samples used in regressions: PRE + POST only
ap <- a[period %in% c("PRE", "POST")]
cat("[50] DiD sample (PRE + POST) =", nrow(ap), "mine-months\n")
cat("       of which PRE =", sum(ap$Post == 0L),
    "   POST =", sum(ap$Post == 1L), "\n\n")

ap[, mine := factor(mine_operation)]
ap[, calmonth_region := factor(sprintf("R%02d_M%02d", codregion, month))]
ap[, yr := factor(year)]
ap[, region_clust := factor(codregion)]

# ------------------------------------------------------------------
# 2. Helper: estimate, return coefficient + clustered SE/p
# ------------------------------------------------------------------
fit_did <- function(dat, regressor) {
  f <- sprintf("log_kt ~ %s + %s:Post + log_price | mine + calmonth_region + yr",
               regressor, regressor)
  n_reg <- uniqueN(dat$region_clust)
  if (n_reg >= 2L) {
    feols(as.formula(f), data = dat, cluster = ~ region_clust, notes = FALSE)
  } else {
    # single-region cut (e.g. Atacama only) -- fall back to HC3 robust SE
    feols(as.formula(f), data = dat, vcov = "hetero", notes = FALSE)
  }
}

extract_did <- function(m, regressor, label, sample_label) {
  ct <- coeftable(m)
  rn <- rownames(ct)
  b1_term <- intersect(rn, c(regressor, paste0("Post:", regressor)))[1]
  b2_term <- intersect(rn, c(paste0(regressor, ":Post"), paste0("Post:", regressor)))[1]
  has_b1 <- !is.na(b1_term)
  has_b2 <- !is.na(b2_term) && b2_term != b1_term
  out <- data.table(
    spec_label   = label,
    sample       = sample_label,
    regressor    = regressor,
    beta_pre     = if (has_b1) unname(ct[b1_term, "Estimate"])   else NA_real_,
    se_pre       = if (has_b1) unname(ct[b1_term, "Std. Error"]) else NA_real_,
    p_pre        = if (has_b1) unname(ct[b1_term, "Pr(>|t|)"])   else NA_real_,
    beta_attn    = if (has_b2) unname(ct[b2_term, "Estimate"])   else NA_real_,
    se_attn      = if (has_b2) unname(ct[b2_term, "Std. Error"]) else NA_real_,
    p_attn       = if (has_b2) unname(ct[b2_term, "Pr(>|t|)"])   else NA_real_,
    beta_post    = if (has_b1 && has_b2) unname(ct[b1_term, "Estimate"] + ct[b2_term, "Estimate"]) else NA_real_,
    n_obs        = nobs(m), n_clust = 7L
  )
  out
}

# ------------------------------------------------------------------
# 3. Run the matrix:  3 samples x 2 specs (A general, B ENSO-attributable)
# ------------------------------------------------------------------
sample_defs <- list(
  list(label = "national",       filter = expression(rep(TRUE, .N))),
  list(label = "atacama_only",   filter = expression(codregion == 3L)),
  list(label = "north_placebo",  filter = expression(codregion %in% c(1L, 2L)))
)

spec_defs <- list(
  list(label = "A_general_flood",       regressor = "thr_flood_regmean_M"),
  list(label = "B_enso_attributable",   regressor = "stress_flood_regmean_M")
)

rows <- list()
for (sd in sample_defs) {
  sub <- ap[eval(sd$filter)]
  cat(sprintf("\n[50] === sample = %s   N = %d   PRE = %d   POST = %d ===\n",
              sd$label, nrow(sub), sum(sub$Post == 0L), sum(sub$Post == 1L)))
  for (sp in spec_defs) {
    cat(sprintf("    -> %s\n", sp$label))
    m <- try(fit_did(sub, sp$regressor), silent = TRUE)
    if (inherits(m, "try-error")) {
      cat("       FAILED:", as.character(m), "\n"); next
    }
    rows[[length(rows) + 1]] <- extract_did(m, sp$regressor, sp$label, sd$label)
    r <- rows[[length(rows)]]
    fmt <- function(x) if (is.na(x)) "    NA" else sprintf("%+0.4f", x)
    fmt_se <- function(x) if (is.na(x)) "  NA  " else sprintf("%0.4f", x)
    fmt_p  <- function(x) if (is.na(x)) " NA  " else sprintf("%0.3f", x)
    cat(sprintf("       beta_pre  = %s (SE %s, p %s)\n",
                fmt(r$beta_pre), fmt_se(r$se_pre), fmt_p(r$p_pre)))
    cat(sprintf("       beta_attn = %s (SE %s, p %s)  <-- ADAPTATION\n",
                fmt(r$beta_attn), fmt_se(r$se_attn), fmt_p(r$p_attn)))
    if (!is.na(r$beta_post))
      cat(sprintf("       beta_post = %+0.4f  (implied pre + attn)\n", r$beta_post))
  }
}

main_tbl <- rbindlist(rows, fill = TRUE)
fwrite(main_tbl, file.path(TABLES, "50__flood_adaptation_main.csv"))

cat("\n[50] MAIN TABLE (written to 50__flood_adaptation_main.csv)\n")
print(main_tbl)

# ------------------------------------------------------------------
# 4. LOO-region around the national headline (Spec A) and Spec B
# ------------------------------------------------------------------
loo_rows <- list()
for (sp in spec_defs) {
  for (rg in sort(unique(ap$codregion))) {
    sub <- ap[codregion != rg]
    m <- try(fit_did(sub, sp$regressor), silent = TRUE)
    if (inherits(m, "try-error")) next
    ct <- coeftable(m)
    rn <- rownames(ct)
    b2_term <- intersect(rn, c(paste0(sp$regressor, ":Post"),
                                paste0("Post:", sp$regressor)))[1]
    if (is.na(b2_term)) next
    loo_rows[[length(loo_rows) + 1]] <- data.table(
      spec_label = sp$label, drop = sprintf("region_%02d", rg),
      beta_attn  = unname(ct[b2_term, "Estimate"]),
      p_attn     = unname(ct[b2_term, "Pr(>|t|)"])
    )
  }
}
loo_tbl <- rbindlist(loo_rows, fill = TRUE)
fwrite(loo_tbl, file.path(TABLES, "50__flood_adaptation_byregion.csv"))

cat("\n[50] LOO-region for the adaptation coefficient (beta_attn)\n")
print(loo_tbl[, .(n_drops    = .N,
                  beta_min   = round(min(beta_attn), 4),
                  beta_max   = round(max(beta_attn), 4),
                  p_min      = round(min(p_attn), 4),
                  p_max      = round(max(p_attn), 4)),
              by = spec_label])

# ------------------------------------------------------------------
# 5. Anticipation novelty test:  Spec B attenuation > Spec A attenuation?
# ------------------------------------------------------------------
nat <- main_tbl[sample == "national"]
if (nrow(nat) == 2) {
  bA <- nat[spec_label == "A_general_flood",     beta_attn]
  bB <- nat[spec_label == "B_enso_attributable", beta_attn]
  cat("\n[50] ANTICIPATION TEST (national, headline)\n")
  cat(sprintf("       Spec A general-flood attenuation     = %+0.4f\n", bA))
  cat(sprintf("       Spec B ENSO-attributable attenuation = %+0.4f\n", bB))
  cat(sprintf("       Differential (B - A)                 = %+0.4f\n", bB - bA))
  cat("       Positive differential is consistent with the anticipation hypothesis\n")
  cat("       (mines exploit ENSO forecast skill more than they can prepare for random storms).\n")
  cat("       NB: standard error of (B - A) is not computed here -- both specs share\n")
  cat("       the same data so their estimates are correlated;  the differential is\n")
  cat("       diagnostic only and should be read as the SIGN of an anticipation effect,\n")
  cat("       not as a formally identified causal estimate.\n")
}

cat("\n[50] done.\n")
