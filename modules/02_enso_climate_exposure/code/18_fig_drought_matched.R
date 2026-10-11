# =============================================================================
# 18_fig_drought_matched.R
# Rebuild Figure 6 as the matched-scale contrast: all-drought (threshold-only)
# null vs ENSO-aligned (matched-rho) negative, by SPEI accumulation scale.
# Overwrites figures/fig6_drought_profile.{png,svg}.
# =============================================================================
.find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash="/", mustWork=TRUE)
  while (!file.exists(file.path(d,"CLAUDE.md"))) { p<-dirname(d); if(p==d) stop("no root"); d<-p }; d }
source(file.path(.find_root(),"mining_module_alternative","scripts","00_setup.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table); library(fixest); library(ggplot2) })

ec <- as.data.table(read_parquet(file.path(DATA_PROC,"climate","02b__regional_climate_monthly_panel.parquet")))
ec <- ec[codregion %in% COPPER_REGIONS, .(codregion,year,month,oni_rolling,Z_pr_m,rho_pr_m)]
setorder(ec,codregion,year,month)
for (k in c(3,6,12)) { ec[, paste0("oni",k):=frollmean(oni_rolling,k),by=codregion]; ec[, paste0("z",k):=frollmean(Z_pr_m,k),by=codregion] }
ec[, oni1:=oni_rolling][, z1:=Z_pr_m]
for (k in c(1,3,6,12)) { zc<-paste0("z",k); oc<-paste0("oni",k)
  rk <- ec[year>=1980&year<=2010, .(rho={a<-get(zc);b<-get(oc);ok<-is.finite(a)&is.finite(b); if(sum(ok)>=10) suppressWarnings(cor(a[ok],b[ok])) else NA_real_}), by=.(codregion,month)]
  setnames(rk,"rho",paste0("rhok",k)); ec<-merge(ec,rk,by=c("codregion","month"),all.x=TRUE) }
for (k in c(1,3,6,12)) ec[[paste0("gateM",k)]] <- as.integer(!is.na(ec[[paste0("oni",k)]]) & abs(ec[[paste0("oni",k)]])>=0.5 &
  !is.na(ec[[paste0("rhok",k)]]) & ec[[paste0("rhok",k)]]*ec[[paste0("oni",k)]]<0 & !is.na(ec[[paste0("z",k)]]) & ec[[paste0("z",k)]]<0)

mc <- as.data.table(read_parquet(file.path(PANELS,"14__minecell_spei_multiscale_1960_2025.parquet")))
mc <- merge(mc, ec[,c("codregion","year","month",paste0("gateM",c(1,3,6,12))),with=FALSE], by=c("codregion","year","month"), all.x=TRUE)
ann <- as.data.table(read_parquet(file.path(PANELS,"31__drought_mine_year.parquet")))[in_sample==TRUE,.(mine_operation,codregion,year,log_kt_year)]
cp <- fread(file.path(PROJECT_ROOT,"Data_raw/BCCh/copper_lme_monthly_usd_per_mt.csv")); cp[,year:=as.integer(format(as.Date(paste0(date,"-01")),"%Y"))]
ann <- merge(ann, unique(cp[,.(year)])[,p1:=1], by="year")[p1==1]

rows <- list()
for (k in c(1,3,6,12)) {
  fk<-paste0("drought_flag_spei",k); nm<-paste0("spei",k); gk<-paste0("gateM",k)
  a <- mc[, .(nv=sum(!is.na(get(nm))), thr=mean(get(fk)),
              str=mean(as.integer(get(fk)==1 & !is.na(get(gk)) & get(gk)==1))), by=.(mine_operation,codregion,year)]
  a[nv<12, c("thr","str"):=NA_real_]; setorder(a,mine_operation,year)
  for (cc in c("thr","str")) { for(l in 1:3) a[, paste0(cc,l):=shift(get(cc),l), by=mine_operation]
    a[, paste0("cum_",cc):=rowMeans(.SD), .SDcols=paste0(cc,1:3)] }
  D <- merge(ann, a[,.(mine_operation,year,cum_thr,cum_str)], by=c("mine_operation","year"))
  D[, `:=`(mine=factor(mine_operation),yr=factor(year),rc=factor(codregion))]
  for (sp in c("cum_thr","cum_str")) { d<-D[!is.na(get(sp))]
    m<-feols(as.formula(sprintf("log_kt_year ~ %s | mine + yr",sp)),d,cluster=~rc,notes=FALSE); ct<-coeftable(m)
    rows[[length(rows)+1]] <- data.table(scale=k, series=ifelse(sp=="cum_thr","All drought (threshold-only)","ENSO-aligned (matched gate)"),
      beta=ct[sp,"Estimate"], se=ct[sp,"Std. Error"]) }
}
df <- rbindlist(rows); df[, `:=`(lo=beta-1.96*se, hi=beta+1.96*se)]
df[, series:=factor(series, levels=c("All drought (threshold-only)","ENSO-aligned (matched gate)"))]

p <- ggplot(df, aes(scale, beta, colour=series, fill=series)) +
  geom_hline(yintercept=0, colour="#999999", linewidth=0.3) +
  geom_ribbon(aes(ymin=lo,ymax=hi), alpha=0.13, colour=NA) +
  geom_line(linewidth=0.9) + geom_point(size=2.6) +
  scale_x_continuous(breaks=c(1,3,6,12), labels=c("SPEI-1","SPEI-3","SPEI-6","SPEI-12")) +
  scale_colour_manual(values=c("All drought (threshold-only)"="#9A9A9A","ENSO-aligned (matched gate)"="#8B5A3C"), name=NULL) +
  scale_fill_manual(values=c("All drought (threshold-only)"="#9A9A9A","ENSO-aligned (matched gate)"="#8B5A3C"), name=NULL) +
  labs(title="DROUGHT: not all drought, but ENSO-aligned persistent drought",
       subtitle="Coefficient on the 3-year cumulative drought-exposure share, by SPEI accumulation scale (mine + year FE, region-clustered SE, N=369). ENSO-aligned uses the matched k-month gate.",
       x="SPEI accumulation window (months)", y="Coefficient on log annual mine output",
       caption="Threshold-only (all) drought is null at every scale; the ENSO-aligned effect is negative across the multi-month scales, peaking near SPEI-6 and statistically resolved at SPEI-3 and SPEI-12. Shaded = 95% cluster CI.") +
  theme_minimal(base_size=10) +
  theme(legend.position="top", panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"), plot.caption=element_text(colour="#555555", hjust=0))
ggsave(file.path(FIGURES,"fig6_drought_profile.png"), p, width=9, height=5, dpi=300)
ggsave(file.path(FIGURES,"fig6_drought_profile.svg"), p, width=9, height=5)
cat("[18] rebuilt fig6_drought_profile (matched-scale all-drought vs ENSO-aligned)\n"); print(df[,.(scale,series,beta=round(beta,3))])
