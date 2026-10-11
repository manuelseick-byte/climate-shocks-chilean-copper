# =============================================================================
# 17_matched_robustness_battery.R   (supervisor matched-diagnostics, Phase 1)
#
# A1 matched-window teleconnections rho_k: rebuild the matched gate using
#    rho computed over the SAME k-month window (not month-specific rho_pr_m).
# A2 gate-definition sensitivity (SPEI-12): AND-gate / no-realised-Z /
#    ONI+teleconnection-only / realised-Z-only.
# A3 period (block) sensitivity with the fully matched gate.
# A4 placebo/lead: future matched exposure vs lagged exposure.
# A5 FE strategy (mine+yr headline; +trends; region-year appendix).
# C  SSI-12 matched-style check (threshold-only + decomposition).
#
# Outputs: tables/17__{matched_rho_diagonal, gate_sensitivity, period_sensitivity,
#                      placebo_lead, ssi_matched}.csv
# =============================================================================
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table); library(fixest) })

# ---- ENSO context: matched-window ONI, Z, and MATCHED rho_k -----------------
ec <- as.data.table(read_parquet(file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")))
ec <- ec[codregion %in% COPPER_REGIONS, .(codregion, year, month, date, oni_rolling, Z_pr_m, rho_pr_m)]
setorder(ec, codregion, year, month)
for (k in c(3,6,12)) {
  ec[, paste0("oni",k) := frollmean(oni_rolling, k), by = codregion]
  ec[, paste0("z",k)   := frollmean(Z_pr_m,     k), by = codregion]
}
ec[, oni1 := oni_rolling][, z1 := Z_pr_m]
# A1: matched rho_k = cor(k-month Z, k-month ONI) on 1980-2010 by (region, calendar-month)
for (k in c(1,3,6,12)) {
  zc <- paste0("z",k); oc <- paste0("oni",k)
  rho_k <- ec[year>=1980 & year<=2010, .(rho = {
      a <- get(zc); b <- get(oc); ok <- is.finite(a) & is.finite(b)
      if (sum(ok) >= 10) suppressWarnings(cor(a[ok], b[ok])) else NA_real_ }),
      by = .(codregion, month)]
  setnames(rho_k, "rho", paste0("rhok",k))
  ec <- merge(ec, rho_k, by = c("codregion","month"), all.x = TRUE)
}
# gate builder: matched rho_k version
gate_match <- function(k) {
  oni <- ec[[paste0("oni",k)]]; z <- ec[[paste0("z",k)]]; rho <- ec[[paste0("rhok",k)]]
  as.integer(!is.na(oni) & abs(oni) >= 0.5 & !is.na(rho) & rho*oni < 0 & !is.na(z) & z < 0)
}
for (k in c(1,3,6,12)) ec[[paste0("gateM",k)]] <- gate_match(k)
# gate-definition variants for SPEI-12 (A2), all on the 12-month window + matched rho12
ec[, g12_and   := gateM12]
ec[, g12_noZ   := as.integer(!is.na(oni12) & abs(oni12)>=0.5 & !is.na(rhok12) & rhok12*oni12<0)]
ec[, g12_oniT  := g12_noZ]                                   # ONI + teleconnection only (== noZ)
ec[, g12_Zonly := as.integer(!is.na(z12) & z12<0)]           # realised dry only, no ENSO
ec[, g12_oniZ  := as.integer(!is.na(oni12) & abs(oni12)>=0.5 & !is.na(z12) & z12<0)]  # ONI active + dry, no teleconnection

# ---- mine-cell multiscale flags --------------------------------------------
mc <- as.data.table(read_parquet(file.path(PANELS, "14__minecell_spei_multiscale_1960_2025.parquet")))
mc <- mc[, .(mine_operation, codregion, year, month,
             f3=drought_flag_spei3, f6=drought_flag_spei6, f12=drought_flag_spei12,
             v3=as.integer(!is.na(spei3)), v6=as.integer(!is.na(spei6)), v12=as.integer(!is.na(spei12)))]
gcols <- c("gateM3","gateM6","gateM12","g12_and","g12_noZ","g12_Zonly","g12_oniZ")
mc <- merge(mc, ec[, c("codregion","year","month",gcols), with=FALSE], by=c("codregion","year","month"), all.x=TRUE)

# ---- annual share + cum builders -------------------------------------------
ann_share <- function(fk, vk, gk) {
  d <- mc[, .(nv=sum(get(vk)),
              thr=mean(get(fk)),
              str=mean(as.integer(get(fk)==1 & !is.na(get(gk)) & get(gk)==1)),
              non=mean(as.integer(get(fk)==1 & (is.na(get(gk)) | get(gk)==0)))),
          by=.(mine_operation, codregion, year)]
  d[nv<12, c("thr","str","non"):=NA_real_]; d }
cum_lag <- function(d, col){ setorder(d, mine_operation, year)
  for(l in 1:3) d[, paste0(".l",l):=shift(get(col), l), by=mine_operation]
  v<-d[, rowMeans(.SD), .SDcols=paste0(".l",1:3)]; d[, paste0(".l",1:3):=NULL]; v }
cum_lead <- function(d, col){ setorder(d, mine_operation, year)        # A4 placebo: FUTURE y+1..y+3
  for(l in 1:3) d[, paste0(".f",l):=shift(get(col), l, type="lead"), by=mine_operation]
  v<-d[, rowMeans(.SD), .SDcols=paste0(".f",1:3)]; d[, paste0(".f",1:3):=NULL]; v }

# outcome (headline 19-mine sample, 2006+)
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))[in_sample==TRUE,
        .(mine_operation, codregion, year, log_kt_year)]
cp <- fread(file.path(PROJECT_ROOT, "Data_raw/BCCh/copper_lme_monthly_usd_per_mt.csv"))
cp[, year:=as.integer(format(as.Date(paste0(date,"-01")),"%Y"))]
ann <- merge(ann, unique(cp[,.(year)])[, p1:=1], by="year", all.x=TRUE)[p1==1]

mkD <- function(...) { regs <- list(...); D <- copy(ann)
  for (r in regs) D <- merge(D, r, by=c("mine_operation","year"), all.x=TRUE)
  D[, `:=`(mine=factor(mine_operation), yr=factor(year), rc=factor(codregion))]; D }
fit <- function(D, rg, fe="mine + yr"){ d<-D[!is.na(get(rg))]
  m<-feols(as.formula(sprintf("log_kt_year ~ %s | %s",rg,fe)), d, cluster=~rc, notes=FALSE); ct<-coeftable(m)
  data.table(beta=round(ct[rg,"Estimate"],3), se=round(ct[rg,"Std. Error"],3), p=round(ct[rg,"Pr(>|t|)"],3), n=nobs(m)) }
fit2 <- function(D, r1, r2){ d<-D[!is.na(get(r1))&!is.na(get(r2))]
  m<-feols(as.formula(sprintf("log_kt_year ~ %s + %s | mine + yr",r1,r2)), d, cluster=~rc, notes=FALSE); ct<-coeftable(m)
  data.table(enso=round(ct[r1,"Estimate"],3), enso_p=round(ct[r1,"Pr(>|t|)"],3),
             non=round(ct[r2,"Estimate"],3), non_p=round(ct[r2,"Pr(>|t|)"],3), n=nobs(m)) }

# ===== A1: fully-matched diagonal (matched rho_k) ===========================
diag <- list()
for (k in c(3,6,12)) {
  a <- ann_share(paste0("f",k), paste0("v",k), paste0("gateM",k))
  a[, str_c := cum_lag(a,"str")]; a[, non_c := cum_lag(a,"non")]; a[, thr_c := cum_lag(a,"thr")]
  D <- mkD(a[,.(mine_operation,year,str_c,non_c,thr_c)])
  h <- fit(D,"str_c"); de <- fit2(D,"str_c","non_c"); th <- fit(D,"thr_c")
  diag[[length(diag)+1]] <- data.table(scale=paste0("SPEI-",k," x ",k,"mo matched-rho"),
    enso_beta=h$beta, enso_p=h$p, dec_enso_beta=de$enso, dec_enso_p=de$enso_p,
    nonenso_beta=de$non, nonenso_p=de$non_p, allthr_beta=th$beta, allthr_p=th$p, n=h$n)
}
diag <- rbindlist(diag); fwrite(diag, file.path(TABLES,"17__matched_rho_diagonal.csv"))

# ===== A5 + A2 + A3 + A4 all on SPEI-12 (headline scale) ====================
a12 <- ann_share("f12","v12","g12_and")
a12[, str_c := cum_lag(a12,"str")]; a12[, non_c := cum_lag(a12,"non")]; a12[, thr_c := cum_lag(a12,"thr")]
a12[, str_lead := cum_lead(a12,"str")]
D12 <- mkD(a12[,.(mine_operation,year,str_c,non_c,thr_c,str_lead)])

# A5 FE ladder (matched-rho SPEI-12)
fel <- rbindlist(list(
  cbind(fe="mine+yr",            cbind(spec="ENSO",      fit(D12,"str_c"))),
  cbind(fe="mine+yr",            cbind(spec="all-drought",fit(D12,"thr_c"))),
  cbind(fe="mine+yr+trend",      cbind(spec="ENSO",      fit(D12,"str_c","mine+yr+mine[year]"))),
  cbind(fe="mine+yr+trend",      cbind(spec="all-drought",fit(D12,"thr_c","mine+yr+mine[year]"))),
  cbind(fe="mine+region-year",   cbind(spec="ENSO",      fit(D12,"str_c","mine+codregion^year"))),
  cbind(fe="mine+region-year",   cbind(spec="all-drought",fit(D12,"thr_c","mine+codregion^year")))), fill=TRUE)
fwrite(fel, file.path(TABLES,"17__fe_ladder_matchedrho.csv"))

# A2 gate-definition sensitivity (SPEI-12, matched window)
gate_specs <- c(AND_gate="g12_and", ONI_plus_teleconn_noZ="g12_noZ",
                ONI_active_and_dry_noTeleconn="g12_oniZ", realisedZ_only="g12_Zonly")
gs <- rbindlist(lapply(names(gate_specs), function(nm){
  a <- ann_share("f12","v12", gate_specs[[nm]]); a[, sc := cum_lag(a,"str")]
  D <- mkD(a[,.(mine_operation,year,sc)]); cbind(gate=nm, fit(D,"sc")) }))
fwrite(gs, file.path(TABLES,"17__gate_sensitivity.csv"))

# A3 period/block sensitivity (matched-rho SPEI-12)
blocks <- list(Full=function(d) d, `Drop 2022-2025`=function(d) d[year<=2021],
  `Drop 2021-2025`=function(d) d[year<=2020], `Pre-2020 only`=function(d) d[year<=2019],
  `Post-2020 only`=function(d) d[year>=2020])
ps <- rbindlist(lapply(names(blocks), function(nm){ cbind(block=nm, fit(blocks[[nm]](D12),"str_c")) }))
fwrite(ps, file.path(TABLES,"17__period_sensitivity.csv"))

# A4 placebo/lead: lagged vs future exposure
pl <- rbindlist(list(cbind(exposure="lagged (y-1..y-3, headline)", fit(D12,"str_c")),
                     cbind(exposure="lead   (y+1..y+3, placebo)",  fit(D12,"str_lead"))))
fwrite(pl, file.path(TABLES,"17__placebo_lead.csv"))

# ===== C: SSI-12 matched-style check ========================================
led <- as.data.table(read_parquet(file.path(LEDGER,"20__master_stress_ledger.parquet")))
ssi <- led[channel=="DROUGHT_hydro", .(mine_operation, codregion, year, month, fS=threshold_crossed, vS=as.integer(!is.na(physical_value)))]
ssi <- merge(ssi, ec[,.(codregion,year,month,gateM12)], by=c("codregion","year","month"), all.x=TRUE)
aS <- ssi[, .(nv=sum(vS), thr=mean(fS,na.rm=TRUE),
              str=mean(as.integer(fS==1 & !is.na(gateM12) & gateM12==1), na.rm=TRUE),
              non=mean(as.integer(fS==1 & (is.na(gateM12)|gateM12==0)), na.rm=TRUE)), by=.(mine_operation,codregion,year)]
aS[nv<12, c("thr","str","non"):=NA_real_]
aS[, str_c:=cum_lag(aS,"str")]; aS[, non_c:=cum_lag(aS,"non")]; aS[, thr_c:=cum_lag(aS,"thr")]
DS <- mkD(aS[,.(mine_operation,year,str_c,non_c,thr_c)])
ssi_res <- rbindlist(list(cbind(spec="SSI-12 ENSO matched", fit(DS,"str_c")),
                          cbind(spec="SSI-12 all-drought",  fit(DS,"thr_c")),
                          cbind(spec="SSI-12 decomp ENSO",  data.table(beta=fit2(DS,"str_c","non_c")$enso, se=NA, p=fit2(DS,"str_c","non_c")$enso_p, n=fit2(DS,"str_c","non_c")$n))))
fwrite(ssi_res, file.path(TABLES,"17__ssi_matched.csv"))

cat("\n===== A1  FULLY-MATCHED DIAGONAL (matched rho_k) =====\n"); print(diag)
cat("\n===== A5  FE LADDER (matched-rho SPEI-12) =====\n"); print(fel)
cat("\n===== A2  GATE-DEFINITION SENSITIVITY (SPEI-12) =====\n"); print(gs)
cat("\n===== A3  PERIOD SENSITIVITY (matched-rho SPEI-12) =====\n"); print(ps)
cat("\n===== A4  PLACEBO / LEAD =====\n"); print(pl)
cat("\n===== C   SSI-12 MATCHED CHECK =====\n"); print(ssi_res)
