# =============================================================================
# 15_matched_scale_battery.R   (supervisor's time-scale-matched ENSO test)
#
# Construct-validity test for the DROUGHT ENSO attribution.  The headline gates a
# 12-month accumulated index (SPEI-12) with a ~current-month ENSO condition.  Here
# we MATCH the ENSO-gate window to the SPEI accumulation window:
#   for k in {1,3,6,12}: gate SPEI-k<=-1 with a k-month ENSO condition
#     (i)  |k-month-mean ONI| >= 0.5
#     (ii) rho_pr * (k-month ONI) < 0          (teleconnection predicts drier)
#     (iii) k-month-mean precip Z < 0          (realised drier, precip-based;
#                                               distinct from the water-balance SPEI)
# Then annual ENSO-attributable share (12 valid months), complete-lag cum3,
# headline + decomposition per scale.  Also SPEI-12 across gate widths.
#
# Inputs: panels/14__minecell_spei_multiscale_1960_2025.parquet  (SPEI-1/3/6/12)
#         Data_processed/climate/02b__regional_climate_monthly_panel.parquet
#         panels/31__drought_mine_year.parquet  (outcome + in_sample, headline sample)
#         Data_raw/BCCh/copper_lme_monthly_usd_per_mt.csv  (defines 2006+ sample)
# Output: tables/15__matched_scale_battery.csv
# =============================================================================
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table); library(fixest) })

# ---- ENSO context with rolling windows -------------------------------------
ec <- as.data.table(read_parquet(file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")))
ec <- ec[codregion %in% COPPER_REGIONS, .(codregion, year, month, date, oni_rolling, oni_roll3, oni_roll6, Z_pr_m, rho_pr_m)]
setorder(ec, codregion, date)
ec[, oni_roll12 := frollmean(oni_rolling, 12), by = codregion]
ec[, z1 := Z_pr_m]
ec[, z3  := frollmean(Z_pr_m, 3),  by = codregion]
ec[, z6  := frollmean(Z_pr_m, 6),  by = codregion]
ec[, z12 := frollmean(Z_pr_m, 12), by = codregion]
mk_gate <- function(oni, z) as.integer(!is.na(oni) & abs(oni) >= 0.5 & !is.na(z) & ec$rho_pr_m * oni < 0 & z < 0)
ec[, gate1  := mk_gate(oni_rolling, z1)]
ec[, gate3  := mk_gate(oni_roll3,   z3)]
ec[, gate6  := mk_gate(oni_roll6,   z6)]
ec[, gate12 := mk_gate(oni_roll12,  z12)]

# ---- mine-cell multiscale drought flags ------------------------------------
mc <- as.data.table(read_parquet(file.path(PANELS, "14__minecell_spei_multiscale_1960_2025.parquet")))
mc <- mc[, .(mine_operation, codregion, year, month,
             f1 = drought_flag_spei1, f3 = drought_flag_spei3, f6 = drought_flag_spei6, f12 = drought_flag_spei12,
             v1 = as.integer(!is.na(spei1)), v3 = as.integer(!is.na(spei3)),
             v6 = as.integer(!is.na(spei6)), v12 = as.integer(!is.na(spei12)))]
mc <- merge(mc, ec[, .(codregion, year, month, gate1, gate3, gate6, gate12)],
            by = c("codregion","year","month"), all.x = TRUE)

# ---- annual ENSO-attributable / non-ENSO / threshold shares per scale ------
# requires 12 valid SPEI-k months in the year
ann_share <- function(fk, vk, gk) {
  d <- mc[, .(nv = sum(get(vk)),
              thr = mean(get(fk)),
              str = mean(as.integer(get(fk)==1 & !is.na(get(gk)) & get(gk)==1)),
              non = mean(as.integer(get(fk)==1 & (is.na(get(gk)) | get(gk)==0)))),
          by = .(mine_operation, codregion, year)]
  d[nv < 12, c("thr","str","non") := NA_real_]
  d
}
build_cum <- function(d, col) {
  setorder(d, mine_operation, year)
  for (lg in 1:3) d[, paste0(".l",lg) := shift(get(col), lg), by = mine_operation]
  v <- d[, rowMeans(.SD), .SDcols = c(".l1",".l2",".l3")]   # complete 3-lag window
  d[, c(".l1",".l2",".l3") := NULL]; v
}
scales <- list(c("f1","v1","gate1","1"), c("f3","v3","gate3","3"),
               c("f6","v6","gate6","6"), c("f12","v12","gate12","12"))
reg <- mc[, .(mine_operation, codregion, year)][!duplicated(paste(mine_operation, year))]
for (s in scales) {
  a <- ann_share(s[1], s[2], s[3]); k <- s[4]
  a[, (paste0("cum3_str_",k)) := build_cum(a, "str")]
  a[, (paste0("cum3_non_",k)) := build_cum(a, "non")]
  a[, (paste0("cum3_thr_",k)) := build_cum(a, "thr")]
  reg <- merge(reg, a[, c("mine_operation","year", paste0("cum3_str_",k),
                          paste0("cum3_non_",k), paste0("cum3_thr_",k)), with=FALSE],
               by = c("mine_operation","year"), all.x = TRUE)
}
# SPEI-12 flag across gate widths (the direct mismatch test)
for (g in c("1","3","6")) {
  a <- ann_share("f12","v12", paste0("gate",g))
  a[, (paste0("cum3_s12_g",g)) := build_cum(a, "str")]
  reg <- merge(reg, a[, c("mine_operation","year", paste0("cum3_s12_g",g)), with=FALSE],
               by = c("mine_operation","year"), all.x = TRUE)
}

# ---- outcome + headline sample (match script 41: in_sample, 2006+) ----------
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))[in_sample==TRUE,
        .(mine_operation, codregion, year, log_kt_year)]
cp <- fread(file.path(PROJECT_ROOT, "Data_raw/BCCh/copper_lme_monthly_usd_per_mt.csv"))
cp[, year := as.integer(format(as.Date(paste0(date,"-01")),"%Y"))]
ann <- merge(ann, unique(cp[, .(year)])[, has_price := 1], by="year", all.x=TRUE)[has_price==1]
D <- merge(ann, reg, by = c("mine_operation","codregion","year"), all.x = TRUE)
D[, `:=`(mine=factor(mine_operation), yr=factor(year), rc=factor(codregion))]

fit <- function(reg, fe="mine + yr") {
  d <- D[!is.na(get(reg))]
  m <- feols(as.formula(sprintf("log_kt_year ~ %s | %s", reg, fe)), d, cluster=~rc, notes=FALSE)
  ct <- coeftable(m); data.table(beta=ct[reg,"Estimate"], se=ct[reg,"Std. Error"], p=ct[reg,"Pr(>|t|)"], n=nobs(m)) }
fit2 <- function(r1, r2) {
  d <- D[!is.na(get(r1)) & !is.na(get(r2))]
  m <- feols(as.formula(sprintf("log_kt_year ~ %s + %s | mine + yr", r1, r2)), d, cluster=~rc, notes=FALSE)
  ct <- coeftable(m); data.table(beta=ct[r1,"Estimate"], se=ct[r1,"Std. Error"], p=ct[r1,"Pr(>|t|)"], n=nobs(m),
                                 non_beta=ct[r2,"Estimate"], non_p=ct[r2,"Pr(>|t|)"]) }

out <- list()
for (k in c("1","3","6","12")) {
  out[[length(out)+1]] <- cbind(spec=paste0("MATCHED SPEI-",k," x ",k,"mo-gate (headline)"), fit(paste0("cum3_str_",k)))
  dec <- fit2(paste0("cum3_str_",k), paste0("cum3_non_",k))
  out[[length(out)+1]] <- cbind(spec=paste0("  decomp ENSO  (SPEI-",k,")"), dec[, .(beta,se,p,n)])
  out[[length(out)+1]] <- cbind(spec=paste0("  decomp nonENSO (SPEI-",k,")"),
                                data.table(beta=dec$non_beta, se=NA_real_, p=dec$non_p, n=dec$n))
  out[[length(out)+1]] <- cbind(spec=paste0("  threshold-only SPEI-",k), fit(paste0("cum3_thr_",k)))
}
for (g in c("1","3","6","12")) {
  rn <- if (g=="12") "cum3_str_12" else paste0("cum3_s12_g",g)
  out[[length(out)+1]] <- cbind(spec=paste0("SPEI-12 x ",g,"mo-gate"), fit(rn))
}
res <- rbindlist(out, fill=TRUE)
res[, `:=`(beta=round(beta,3), se=round(se,3), p=round(p,3))]
fwrite(res, file.path(TABLES, "15__matched_scale_battery.csv"))
cat("\n================ MATCHED-SCALE ENSO BATTERY (19 mines) ================\n")
print(res, nrows=40)
cat("\nReference: current headline (SPEI-12, ~current-month gate) = -0.271, p=0.011.\n")
cat("Read the diagonal: does the ENSO-attributable negative survive as gate matches accumulation?\n")

# ---- LOO-region on the fully-matched SPEI-12 x 12mo-gate --------------------
cat("\n-- LOO-region: matched SPEI-12 x 12mo-gate (headline = -0.439) --\n")
loo <- list()
for (rg in sort(unique(D$codregion))) {
  d <- D[codregion != rg & !is.na(cum3_str_12)]
  m <- try(feols(log_kt_year ~ cum3_str_12 | mine + yr, d, cluster = ~rc, notes = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) next
  b <- unname(coef(m)["cum3_str_12"]); pp <- unname(coeftable(m)["cum3_str_12","Pr(>|t|)"])
  loo[[length(loo)+1]] <- data.table(drop_region = rg, beta = round(b,3), p = round(pp,3))
  cat(sprintf("   drop region %2d -> beta=%+.3f  p=%.3f\n", rg, b, pp))
}
loo <- rbindlist(loo)
cat(sprintf("   LOO range: [%.3f, %.3f]  (all negative: %s)\n",
            min(loo$beta), max(loo$beta), all(loo$beta < 0)))
fwrite(loo, file.path(TABLES, "15__matched_loo_region.csv"))
