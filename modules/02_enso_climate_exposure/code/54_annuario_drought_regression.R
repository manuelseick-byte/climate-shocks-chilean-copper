# =============================================================================
# 54_annuario_drought_regression.R   (DROUGHT-channel Annuario extension, Step 3)
#
# Build the extended annual mine-year DROUGHT panel (37 operations, 1988-2021)
# and re-identify the SPEI-12 drought effect on log annual copper output, with
# the decisive OUT-OF-MEGADROUGHT test that the 19-mine / 2006-2025 panel
# could not run.
#
# ISOLATION: reads 52 (Annuario output), 53 (Annuario SPEI), and the master
# ledger (READ-ONLY, only to borrow the region-month ENSO-drought alignment).
# Writes ONLY to panels/54__ and tables/54__.  Touches no original panel.
#
# Regressors (3-year cumulative share over y-1,y-2,y-3):
#   cum3_thr  = share of months with SPEI-12 <= -1            (total water stress)
#   cum3_str  = share of months with SPEI-12 <= -1 AND ENSO-aligned drought
#               (ENSO-attributable; alignment borrowed from the master ledger
#                at region-month level; comparable to the report headline)
#
# Key specifications (mine + year FE, cluster region):
#   S1  log_kt ~ cum3_thr                       full panel  (extended headline)
#   S2  log_kt ~ cum3_str                       full panel  (ENSO-attributable)
#   S3  log_kt ~ cum3_thr + cum3_thr:megadrought            (does it survive
#       outside the 2010-2022 megadrought?)
#   S4  log_kt ~ cum3_thr   on PRE-2010 subsample only      (1998-99 era alone)
#   S5  size-filtered (drop mean output < 20 kt) robustness
# =============================================================================

source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages({ library(data.table); library(arrow); library(fixest) })

# ---- inputs ----------------------------------------------------------------
out <- as.data.table(read_parquet(file.path(PANELS, "52__annuario_mine_year.parquet")))   # operation, year, copper_kt
# FIX (2026-05-30, supervisor Priority 3): use the EXTENDED Annuario SPEI through
# 2024 (script 55, ERA5-Land bridge) instead of the 1960-2021-only panel 53.
spei <- as.data.table(read_parquet(file.path(PANELS, "55__annuario_minecell_spei_1960_2024.parquet")))

# region-month ENSO-drought alignment, borrowed read-only from the master ledger
led <- as.data.table(read_parquet(file.path(LEDGER, "20__master_stress_ledger.parquet")))
align <- unique(led[channel == "DROUGHT_meteo", .(codregion, year, month, enso_aligned)])

# ---- monthly drought indicators -------------------------------------------
spei[, spei_dry := as.integer(!is.na(spei12) & spei12 <= -1)]
spei <- merge(spei, align, by = c("codregion","year","month"), all.x = TRUE)
spei[, enso_dry := as.integer(spei_dry == 1 & !is.na(enso_aligned) & enso_aligned == 1)]

# ---- annual shares ---------------------------------------------------------
ann <- spei[, .(thr_share = mean(spei_dry, na.rm = TRUE),
                str_share = mean(enso_dry, na.rm = TRUE),
                n_spei    = sum(!is.na(spei12))),
            by = .(operation, codregion, year)]
ann <- ann[n_spei >= 12]                                  # FIX: full-year coverage (corrected rule)
ann[, nonenso_share := thr_share - str_share]             # non-ENSO drought (for decomposition)

setorder(ann, operation, year)
for (col in c("thr_share","str_share","nonenso_share")) for (lg in 1:3)
  ann[, paste0(col,"_lag",lg) := shift(get(col), lg), by = operation]
# FIX: COMPLETE 3-lag windows (na.rm removed) to match the corrected headline.
ann[, cum3_thr     := rowMeans(.SD), .SDcols=c("thr_share_lag1","thr_share_lag2","thr_share_lag3")]
ann[, cum3_str     := rowMeans(.SD), .SDcols=c("str_share_lag1","str_share_lag2","str_share_lag3")]
ann[, cum3_nonenso := rowMeans(.SD), .SDcols=c("nonenso_share_lag1","nonenso_share_lag2","nonenso_share_lag3")]

# ---- join output, build outcome --------------------------------------------
ann <- merge(ann, out[, .(operation, year, copper_kt)], by = c("operation","year"), all.x = TRUE)
ann[, log_kt := suppressWarnings(log(copper_kt))]
ann[is.infinite(log_kt), log_kt := NA_real_]
ann[, megadrought := as.integer(year >= 2010 & year <= 2024)]   # extended: drought continued through 2024
ann[, mean_kt := mean(copper_kt, na.rm=TRUE), by = operation]
ann[, mine := factor(operation)]; ann[, yr := factor(year)]; ann[, reg := factor(codregion)]

panel <- ann[!is.na(log_kt) & !is.na(cum3_thr)]
write_parquet(panel, file.path(PANELS, "54__annuario_drought_panel.parquet"))
cat(sprintf("[54] extended drought panel: %d operations, %d mine-years, %d-%d, %d regions\n",
            uniqueN(panel$operation), nrow(panel), min(panel$year), max(panel$year), uniqueN(panel$codregion)))

# ---- regressions -----------------------------------------------------------
row <- function(m, term, label, n=nobs(m)) {
  ct <- coeftable(m)
  if (!term %in% rownames(ct)) return(NULL)
  data.table(spec=label, term=term, beta=ct[term,"Estimate"], se=ct[term,"Std. Error"],
             p=ct[term,"Pr(>|t|)"], n=n)
}
res <- list()
m1 <- feols(log_kt ~ cum3_thr | mine + yr, panel, cluster=~reg)
res[[1]] <- row(m1,"cum3_thr","S1 total-SPEI, full 1988-2021")
m2 <- feols(log_kt ~ cum3_str | mine + yr, panel, cluster=~reg)
res[[2]] <- row(m2,"cum3_str","S2 ENSO-attrib SPEI, full")
m3 <- feols(log_kt ~ cum3_thr + cum3_thr:megadrought | mine + yr, panel, cluster=~reg)
res[[3]] <- row(m3,"cum3_thr","S3 total-SPEI (out-of-megadrought main)")
res[[4]] <- row(m3,"cum3_thr:megadrought","S3 x megadrought add-on")
pre <- panel[year <= 2009]
m4 <- feols(log_kt ~ cum3_thr | mine + yr, pre, cluster=~reg)
res[[5]] <- row(m4,"cum3_thr","S4 total-SPEI, PRE-2010 only (incl 1998-99)")
big <- panel[mean_kt >= 20]
m5 <- feols(log_kt ~ cum3_thr | mine + yr, big, cluster=~reg)
res[[6]] <- row(m5,"cum3_thr","S5 total-SPEI, mines >=20kt only")
m6 <- feols(log_kt ~ cum3_thr | mine + yr, panel[mean_kt>=20 & year<=2009], cluster=~reg)
res[[7]] <- row(m6,"cum3_thr","S6 >=20kt AND pre-2010")
# S7 ENSO-attributable headline on the broader 37-op panel (the key survival test)
m7 <- feols(log_kt ~ cum3_str | mine + yr, panel, cluster=~reg)
res[[length(res)+1]] <- row(m7,"cum3_str","S7 ENSO-attrib SPEI, full 1988-2024")
# S8 decomposition: ENSO-attributable vs non-ENSO drought in one model
m8 <- feols(log_kt ~ cum3_str + cum3_nonenso | mine + yr, panel, cluster=~reg)
res[[length(res)+1]] <- row(m8,"cum3_str","S8 decomp ENSO-attributable")
res[[length(res)+1]] <- row(m8,"cum3_nonenso","S8 decomp non-ENSO")
# S9 ENSO-attributable, PRE-2010 only (out-of-megadrought, incl 1998-99 La Nina)
m9 <- feols(log_kt ~ cum3_str | mine + yr, panel[year<=2009], cluster=~reg)
res[[length(res)+1]] <- row(m9,"cum3_str","S9 ENSO-attrib, PRE-2010 out-of-megadrought")

restab <- rbindlist(res, fill=TRUE)
restab[, `:=`(beta=round(beta,3), se=round(se,3), p=round(p,3))]
fwrite(restab, file.path(TABLES, "54__annuario_drought_regressions.csv"))
cat("\n[54] EXTENDED DROUGHT REGRESSIONS (mine + year FE, cluster region)\n")
print(restab)

# LOO-region on S1
loo <- list()
for (rg in sort(unique(panel$codregion))) {
  mm <- try(feols(log_kt ~ cum3_thr | mine + yr, panel[codregion!=rg], cluster=~reg), silent=TRUE)
  if (!inherits(mm,"try-error")) { ct<-coeftable(mm); loo[[length(loo)+1]] <-
    data.table(drop=rg, beta=round(ct["cum3_thr","Estimate"],3), p=round(ct["cum3_thr","Pr(>|t|)"],3)) }
}
loo <- rbindlist(loo)
cat("\n[54] LOO-region (S1 cum3_thr): range [", min(loo$beta), ",", max(loo$beta), "]\n")
print(loo)
cat("\n[54] done.\n")
