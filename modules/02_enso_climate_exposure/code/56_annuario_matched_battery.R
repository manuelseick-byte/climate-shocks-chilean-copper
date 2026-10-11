# =============================================================================
# 56_annuario_matched_battery.R
#
# Time-scale-matched ENSO DROUGHT battery on the BROADER 37-operation Annuario
# panel (1988-2024).  Mirrors script 15 but on the Annuario operations, using the
# water-balance now preserved in script 55, and Annuario annual output (script 52).
#
# Inputs: panels/55__annuario_minecell_spei_1960_2024.parquet  (needs water_balance)
#         Data_processed/climate/02b__regional_climate_monthly_panel.parquet
#         panels/52__annuario_mine_year.parquet  (operation, year, copper_kt)
# Output: tables/56__annuario_matched_battery.csv
# =============================================================================
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table); library(SPEI); library(fixest) })
SCALES <- c(1L,3L,6L,12L)

# ---- 1. multiscale SPEI from preserved water-balance ------------------------
sp <- as.data.table(read_parquet(file.path(PANELS, "55__annuario_minecell_spei_1960_2024.parquet")))
if (!"water_balance" %in% names(sp)) stop("55 panel lacks water_balance; re-run script 55 first.")
spei_fit <- function(wb, y0, m0, scale) {
  f <- tryCatch({ o<-NULL; invisible(capture.output(o<-suppressWarnings(SPEI::spei(
        ts(wb, start=c(y0,m0), frequency=12), scale=scale, ref.start=c(1960,1), ref.end=c(2021,12), na.rm=TRUE)))); o$fitted },
       error=function(e) rep(NA_real_, length(wb)))
  f <- as.numeric(f); f[is.nan(f)] <- NA_real_; pmax(-3, pmin(3, f))
}
add_scales <- function(df){ df<-df[order(year,month)]; y0<-df$year[1]; m0<-df$month[1]
  for (s in SCALES) df[[paste0("spei",s)]] <- spei_fit(df$water_balance, y0, m0, s); df }
setorder(sp, operation, year, month)
mc <- sp[, add_scales(.SD), by = operation]
for (s in SCALES) { nm<-paste0("spei",s)
  mc[[paste0("f",s)]] <- as.integer(!is.na(mc[[nm]]) & mc[[nm]] <= -1)
  mc[[paste0("v",s)]] <- as.integer(!is.na(mc[[nm]])) }

# ---- 2. matched ENSO gates (identical to script 15) -------------------------
ec <- as.data.table(read_parquet(file.path(DATA_PROC, "climate", "02b__regional_climate_monthly_panel.parquet")))
ec <- ec[codregion %in% COPPER_REGIONS, .(codregion, year, month, oni_rolling, oni_roll3, oni_roll6, Z_pr_m, rho_pr_m)]
setorder(ec, codregion, year, month)
ec[, oni_roll12 := frollmean(oni_rolling, 12), by=codregion]
ec[, z1:=Z_pr_m][, z3:=frollmean(Z_pr_m,3),by=codregion][, z6:=frollmean(Z_pr_m,6),by=codregion][, z12:=frollmean(Z_pr_m,12),by=codregion]
g <- function(oni,z) as.integer(!is.na(oni)&abs(oni)>=0.5&!is.na(z)&ec$rho_pr_m*oni<0&z<0)
ec[, gate1:=g(oni_rolling,z1)][, gate3:=g(oni_roll3,z3)][, gate6:=g(oni_roll6,z6)][, gate12:=g(oni_roll12,z12)]
mc <- merge(mc, ec[, .(codregion,year,month,gate1,gate3,gate6,gate12)], by=c("codregion","year","month"), all.x=TRUE)

# ---- 3. annual matched shares + complete-lag cum3 ---------------------------
ann_share <- function(fk,vk,gk){ d<-mc[, .(nv=sum(get(vk)),
    thr=mean(get(fk)), str=mean(as.integer(get(fk)==1 & !is.na(get(gk)) & get(gk)==1)),
    non=mean(as.integer(get(fk)==1 & (is.na(get(gk)) | get(gk)==0)))), by=.(operation,codregion,year)]
  d[nv<12, c("thr","str","non"):=NA_real_]; d }
build_cum <- function(d,col){ setorder(d,operation,year)
  for(lg in 1:3) d[, paste0(".l",lg):=shift(get(col),lg), by=operation]
  v<-d[, rowMeans(.SD), .SDcols=c(".l1",".l2",".l3")]; d[,c(".l1",".l2",".l3"):=NULL]; v }
reg <- unique(mc[, .(operation, codregion, year)])
for (s in SCALES){ k<-as.character(s); a<-ann_share(paste0("f",s),paste0("v",s),paste0("gate",s))
  a[, (paste0("cum3_str_",k)):=build_cum(a,"str")]; a[, (paste0("cum3_non_",k)):=build_cum(a,"non")]
  a[, (paste0("cum3_thr_",k)):=build_cum(a,"thr")]
  reg<-merge(reg,a[,c("operation","year",paste0("cum3_str_",k),paste0("cum3_non_",k),paste0("cum3_thr_",k)),with=FALSE],by=c("operation","year"),all.x=TRUE) }
for (gw in c("1","3","6")){ a<-ann_share("f12","v12",paste0("gate",gw))
  a[, (paste0("cum3_s12_g",gw)):=build_cum(a,"str")]
  reg<-merge(reg,a[,c("operation","year",paste0("cum3_s12_g",gw)),with=FALSE],by=c("operation","year"),all.x=TRUE) }

# ---- 4. outcome (Annuario annual output) ------------------------------------
out <- as.data.table(read_parquet(file.path(PANELS, "52__annuario_mine_year.parquet")))
out[, log_kt := suppressWarnings(log(copper_kt))]; out[is.infinite(log_kt), log_kt := NA_real_]
D <- merge(reg, out[, .(operation, year, log_kt)], by=c("operation","year"), all.x=TRUE)
D <- D[!is.na(log_kt)]
D[, `:=`(mine=factor(operation), yr=factor(year), rc=factor(codregion))]

fit <- function(rg){ d<-D[!is.na(get(rg))]; m<-feols(as.formula(sprintf("log_kt ~ %s | mine + yr",rg)),d,cluster=~rc,notes=FALSE)
  ct<-coeftable(m); data.table(beta=round(ct[rg,"Estimate"],3),se=round(ct[rg,"Std. Error"],3),p=round(ct[rg,"Pr(>|t|)"],3),n=nobs(m)) }
fit2 <- function(r1,r2){ d<-D[!is.na(get(r1))&!is.na(get(r2))]; m<-feols(as.formula(sprintf("log_kt ~ %s + %s | mine + yr",r1,r2)),d,cluster=~rc,notes=FALSE)
  ct<-coeftable(m); data.table(beta=round(ct[r1,"Estimate"],3),se=round(ct[r1,"Std. Error"],3),p=round(ct[r1,"Pr(>|t|)"],3),n=nobs(m),
    non_beta=round(ct[r2,"Estimate"],3),non_p=round(ct[r2,"Pr(>|t|)"],3)) }
res<-list()
for (k in c("1","3","6","12")){
  res[[length(res)+1]]<-cbind(spec=paste0("MATCHED SPEI-",k," x ",k,"mo-gate"), fit(paste0("cum3_str_",k)))
  dec<-fit2(paste0("cum3_str_",k),paste0("cum3_non_",k))
  res[[length(res)+1]]<-cbind(spec=paste0("  decomp ENSO (SPEI-",k,")"), dec[,.(beta,se,p,n)])
  res[[length(res)+1]]<-cbind(spec=paste0("  decomp nonENSO (SPEI-",k,")"), data.table(beta=dec$non_beta,se=NA_real_,p=dec$non_p,n=dec$n)) }
for (gw in c("1","3","6","12")){ rn<-if(gw=="12")"cum3_str_12" else paste0("cum3_s12_g",gw)
  res[[length(res)+1]]<-cbind(spec=paste0("SPEI-12 x ",gw,"mo-gate"), fit(rn)) }
restab<-rbindlist(res,fill=TRUE)
fwrite(restab, file.path(TABLES, "56__annuario_matched_battery.csv"))
cat(sprintf("\n======== ANNUARIO MATCHED-SCALE BATTERY (37 ops, %d-%d, N up to %d) ========\n",
            min(D$year), max(D$year), max(restab$n,na.rm=TRUE)))
print(restab, nrows=40)
