# =============================================================================
# 51_drought_desal_adaptation.R
#
# Adaptation test for the DROUGHT channel:  do mines that commissioned
# desalination (or direct-seawater) capacity attenuate the drought-output
# elasticity AFTER the commissioning year?
#
# The PER-MINE treatment dates come from registry/mines_desal.csv, which
# distills the desalination commissioning timeline in
# mining_module_alternative/Adaptation story/Adaptation_Narrative.docx
# Table 10.  Shared / multi-client desal capacity counts as PostDesal in this
# script (consistent with the user's instruction).  Codelco divisions are
# treated as separate mines with PostDesal = 0 throughout the sample.
#
# Headline regressor:  cum3_thr_meteo (SPEI-12 cumulative 3-year THRESHOLD
# share, NO ENSO filter).  This is the "total water stress" version --
# megadrought + ENSO together, per the user's reframing.
#
# Cross-index robustness:  cum3_thr_hydro (SSI-12 cumulative 3-year share).
#
# Secondary leg:  cum3_str_meteo and cum3_str_hydro (ENSO-attributable
# variants) -- reported alongside the headline to show whether the
# adaptation result depends on the ENSO-attribution filter.
#
# Headline regression (annual mine-year panel):
#   log_kt_year ~ D + D:PostDesal + PostDesal | mine + yr
#   where D in { cum3_thr_meteo, cum3_thr_hydro, cum3_str_meteo, cum3_str_hydro }
#
# Adaptation prediction:  beta(D:PostDesal) > 0  (attenuation)
#
# Robustness:
#   1. Escondida 2018 EWS vs 2006 Coloso as the treatment year
#   2. Drop Codelco divisions (where PostDesal == 0 throughout)
#   3. LOO-region
#
# Outputs:
#   tables/51__drought_desal_main.csv          headline + robustness
#   tables/51__drought_desal_byregion.csv      LOO-region
#   tables/51__drought_desal_log.txt           run log
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

log_file <- file.path(TABLES, "51__drought_desal_log.txt")
sink(log_file, split = TRUE)
on.exit(sink(NULL), add = TRUE)

cat("=============================================================\n")
cat(" 51_drought_desal_adaptation.R  --  run at ", format(Sys.time()), "\n", sep = "")
cat("=============================================================\n\n")

# ------------------------------------------------------------------
# 1. Load annual drought panel and desal crosswalk
# ------------------------------------------------------------------
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
ann <- ann[in_sample == TRUE]

xw <- fread(file.path(REGISTRY, "mines_desal.csv"))
xw[, desal_year := suppressWarnings(as.integer(desal_year))]    # NA stays NA
cat("[51] desalination crosswalk (registry/mines_desal.csv):\n")
print(xw[, .(mine_operation, desal_year, desal_type)])

# Merge into panel.  PostDesal_{i,y} = 1 iff y >= desal_year (NA stays 0).
ann <- merge(ann, xw[, .(mine_operation, desal_year, desal_type)],
             by = "mine_operation", all.x = TRUE)
ann[, PostDesal := as.integer(!is.na(desal_year) & year >= desal_year)]

cat("\n[51] PostDesal coverage by mine (headline cutoff)\n")
print(ann[, .(treated_year = unique(desal_year),
              n_pre  = sum(PostDesal == 0L),
              n_post = sum(PostDesal == 1L)),
          by = mine_operation][order(-treated_year)])

ann[, mine := factor(mine_operation)]
ann[, yr   := factor(year)]
ann[, region_clust := factor(codregion)]

# ------------------------------------------------------------------
# 2. Headline regression matrix
# ------------------------------------------------------------------
regressors <- list(
  list(label = "SPEI12_threshold_noENSO",     reg = "cum3_thr_meteo"),
  list(label = "SSI12_threshold_noENSO",      reg = "cum3_thr_hydro"),
  list(label = "SPEI12_ENSOattributable",     reg = "cum3_str_meteo"),
  list(label = "SSI12_ENSOattributable",      reg = "cum3_str_hydro")
)

samples <- list(
  list(label = "all_mines",        filter = expression(rep(TRUE, .N))),
  list(label = "excl_codelco",     filter = expression(!grepl("^Codelco", mine_operation)))
)

fit_did_annual <- function(dat, regressor) {
  f <- sprintf("log_kt_year ~ %s + %s:PostDesal + PostDesal | mine + yr",
               regressor, regressor)
  feols(as.formula(f), data = dat, cluster = ~ region_clust, notes = FALSE)
}

extract_did_annual <- function(m, regressor, label, sample_label, treat_label) {
  ct <- coeftable(m)
  rn <- rownames(ct)
  b1_term <- regressor
  b2_term <- paste0(regressor, ":PostDesal")
  if (!b1_term %in% rn) b1_term <- intersect(rn, c(regressor, paste0("PostDesal:", regressor)))[1]
  if (!b2_term %in% rn) b2_term <- intersect(rn, c(paste0(regressor, ":PostDesal"),
                                                    paste0("PostDesal:", regressor)))[1]
  pd_term <- "PostDesal"
  out <- data.table(
    spec_label    = label,
    sample        = sample_label,
    treatment     = treat_label,
    regressor     = regressor,
    beta_pre      = ifelse(!is.na(b1_term), unname(ct[b1_term, "Estimate"]),   NA_real_),
    se_pre        = ifelse(!is.na(b1_term), unname(ct[b1_term, "Std. Error"]), NA_real_),
    p_pre         = ifelse(!is.na(b1_term), unname(ct[b1_term, "Pr(>|t|)"]),   NA_real_),
    beta_attn     = ifelse(!is.na(b2_term), unname(ct[b2_term, "Estimate"]),   NA_real_),
    se_attn       = ifelse(!is.na(b2_term), unname(ct[b2_term, "Std. Error"]), NA_real_),
    p_attn        = ifelse(!is.na(b2_term), unname(ct[b2_term, "Pr(>|t|)"]),   NA_real_),
    beta_post_eff = NA_real_,
    n_obs         = nobs(m), n_clust = 7
  )
  if (!is.na(b1_term) && !is.na(b2_term))
    out[, beta_post_eff := beta_pre + beta_attn]
  out
}

# ------------------------------------------------------------------
# 3. Headline: Escondida 2018 EWS cutoff
# ------------------------------------------------------------------
ann_v1 <- copy(ann)
# (already at EWS-2018 per registry default)

# Alternative: Escondida 2006 Coloso cutoff
xw_v2 <- copy(xw); xw_v2[mine_operation == "Escondida", desal_year := 2006L]
ann_v2 <- merge(ann[, !c("PostDesal","desal_year","desal_type"), with = FALSE],
                xw_v2[, .(mine_operation, desal_year, desal_type)],
                by = "mine_operation", all.x = TRUE)
ann_v2[, PostDesal := as.integer(!is.na(desal_year) & year >= desal_year)]
ann_v2[, mine := factor(mine_operation)]
ann_v2[, yr   := factor(year)]
ann_v2[, region_clust := factor(codregion)]

treatments <- list(
  list(label = "Escondida_2018_EWS_headline", dat = ann_v1),
  list(label = "Escondida_2006_Coloso_robust", dat = ann_v2)
)

rows <- list()
for (tr in treatments) {
  cat(sprintf("\n[51] === treatment timing = %s ===\n", tr$label))
  for (sd in samples) {
    sub <- tr$dat[eval(sd$filter)]
    cat(sprintf("  -- sample = %s   N = %d\n", sd$label, nrow(sub)))
    for (rg in regressors) {
      if (!rg$reg %in% names(sub)) next
      m <- try(fit_did_annual(sub, rg$reg), silent = TRUE)
      if (inherits(m, "try-error")) {
        cat("       FAILED:", rg$label, "\n"); next
      }
      r <- extract_did_annual(m, rg$reg, rg$label, sd$label, tr$label)
      rows[[length(rows) + 1]] <- r
      cat(sprintf("       %-26s  pre = %+0.3f (p %0.3f)   attn = %+0.3f (p %0.3f)\n",
                  rg$label, r$beta_pre, r$p_pre, r$beta_attn, r$p_attn))
    }
  }
}

main_tbl <- rbindlist(rows, fill = TRUE)
fwrite(main_tbl, file.path(TABLES, "51__drought_desal_main.csv"))

cat("\n[51] MAIN TABLE (written to 51__drought_desal_main.csv)\n")
print(main_tbl)

# ------------------------------------------------------------------
# 4. LOO-region around the headline (Escondida-2018 cutoff, all mines)
# ------------------------------------------------------------------
loo_rows <- list()
for (rg in regressors) {
  if (!rg$reg %in% names(ann_v1)) next
  for (cr in sort(unique(ann_v1$codregion))) {
    sub <- ann_v1[codregion != cr]
    m <- try(fit_did_annual(sub, rg$reg), silent = TRUE)
    if (inherits(m, "try-error")) next
    ct <- coeftable(m); rn <- rownames(ct)
    b2_term <- intersect(rn, c(paste0(rg$reg, ":PostDesal"),
                                paste0("PostDesal:", rg$reg)))[1]
    if (is.na(b2_term)) next
    loo_rows[[length(loo_rows) + 1]] <- data.table(
      spec_label = rg$label, drop = sprintf("region_%02d", cr),
      beta_attn  = unname(ct[b2_term, "Estimate"]),
      p_attn     = unname(ct[b2_term, "Pr(>|t|)"])
    )
  }
}
loo_tbl <- rbindlist(loo_rows, fill = TRUE)
fwrite(loo_tbl, file.path(TABLES, "51__drought_desal_byregion.csv"))

if (nrow(loo_tbl) > 0) {
  cat("\n[51] LOO-region adaptation coefficients (Escondida-2018 cutoff)\n")
  print(loo_tbl[, .(n_drops = .N,
                    beta_min = round(min(beta_attn), 3),
                    beta_max = round(max(beta_attn), 3),
                    p_min    = round(min(p_attn), 3),
                    p_max    = round(max(p_attn), 3)),
                by = spec_label])
}

cat("\n[51] done.\n")
