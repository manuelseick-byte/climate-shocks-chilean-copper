# =============================================================================
# 55_annuario_spei_extend.R   (DROUGHT Annuario extension, through 2024)
#
# Extends the 37-operation Annuario mine-cell SPEI-12 from 1960-2021 to 1960-2024,
# using the SAME ERA5-Land bridge as script 13 (supervisor Priority 3).
#   - REUSE ops (17 mapped to the original 19): pull from the EXTENDED mine-cell
#     SPEI panel (script 13, 1960-2025).
#   - EXTRACT ops (20 new mines): CR2MET daily 1960-2021 (as script 53) PLUS the
#     ERA5-Land monthly bridge 2022-2024, bias-corrected per (operation, cal-month)
#     on the 2012-2021 overlap.  SPEI standardisation reference window = 1960-2021.
#
# ISOLATION: reads 13 panel, CR2MET, ERA5 (READ-ONLY); writes ONLY panels/55__.
# Output: panels/55__annuario_minecell_spei_1960_2024.parquet
# =============================================================================
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages({ library(terra); library(data.table); library(arrow); library(stringr); library(fs); library(SPEI) })
DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")
ERA5DIR  <- file.path(DATA_RAW, "ERA5_Land", "monthly_means")

# ---- (a) REUSE from the EXTENDED 13 panel ----------------------------------
ANN2ORIG <- c(
  "Codelco - Andina"="Codelco - Andina","Codelco - Teniente"="Codelco - Teniente",
  "Codelco - Salvador"="Codelco - Salvador","Codelco - Gabriela Mistral"="Codelco - Gaby",
  "Codelco - Chuqui/RT/MH"="Codelco - Chuqui, RT y MH","Escondida"="Escondida",
  "Collahuasi"="Collahuasi","Los Pelambres"="Los Pelambres","Spence"="Spence",
  "Candelaria"="Candelaria","El Abra"="El Abra","Quebrada Blanca"="Quebrada Blanca",
  "Zaldivar"="Zaldivar","Cerro Colorado"="Cerro Colorado","Lomas Bayas"="Lomas Bayas",
  "Anglo American Sur"="Anglo American Sur","Anglo American Norte"="Anglo American Norte")
spei_ext <- as.data.table(read_parquet(file.path(PANELS, "13__minecell_spei12_1960_2025.parquet")))
spei_ext <- spei_ext[, .(orig=mine_operation, codregion, year, month, date, pr_mm, tmean_c, pet_mm, water_balance, spei12)]
reuse <- rbindlist(lapply(names(ANN2ORIG), function(op){
  d <- spei_ext[orig == ANN2ORIG[op]]; if(!nrow(d)) return(NULL)
  d[, .(operation=op, codregion, year, month, date, pr_mm, tmean_c, pet_mm, water_balance, spei12, source="reuse")] }))
cat(sprintf("[55] reuse: %d ops from extended 13 panel, %d-%d\n",
            uniqueN(reuse$operation), min(reuse$year), max(reuse$year)))

# ---- (b) EXTRACT new mines: CR2MET history (1960-2021) ----------------------
newm <- fread(file.path(REGISTRY, "annuario_new_mines_coords.csv"))
pts <- terra::vect(as.data.frame(newm[, .(lon, lat)]), geom=c("lon","lat"), crs="EPSG:4326")
extract_daily <- function(nc_dir, regex, subds_list) {
  files <- fs::dir_ls(nc_dir, regexp = regex) |> sort(); out <- vector("list", length(files))
  for (k in seq_along(files)) {
    ym <- str_extract(basename(files[k]), "\\d{4}_\\d{2}")
    yr <- as.integer(substr(ym,1,4)); mo <- as.integer(substr(ym,6,7))
    start <- as.Date(sprintf("%04d-%02d-01", yr, mo))
    mats <- lapply(subds_list, function(sd){ r<-terra::rast(files[k],subds=sd); list(n=terra::nlyr(r), m=terra::extract(r,pts,ID=FALSE)) })
    dates <- seq(start, by="day", length.out=mats[[1]]$n)
    rows <- lapply(seq_len(nrow(newm)), function(i){ dt<-data.table(operation=newm$operation[i],codregion=newm$codregion[i],date=dates)
      for(s in seq_along(subds_list)) dt[[subds_list[s]]] <- as.numeric(mats[[s]]$m[i,]); dt })
    out[[k]] <- rbindlist(rows)
  }
  rbindlist(out)
}
cat("[55] extracting CR2MET precip (744 files) ...\n")
pr <- extract_daily(file.path(DATA_RAW,"CR2MET","pr"), "CR2MET_pr_v2\\.5_day_\\d{4}_\\d{2}_005deg\\.nc$", c("pr"))
setnames(pr,"pr","pr_mm")
cat("[55] extracting CR2MET tmin/tmax (744 files) ...\n")
tx <- extract_daily(file.path(DATA_RAW,"CR2MET","txn"), "CR2MET_tmin_tmax_v2\\.5_day_\\d{4}_\\d{2}_005deg\\.nc$", c("tmax","tmin"))
pr[, c("year","month") := .(as.integer(format(date,"%Y")), as.integer(format(date,"%m")))]
tx[, c("year","month") := .(as.integer(format(date,"%Y")), as.integer(format(date,"%m")))]
mon_cr <- merge(pr[, .(pr_mm=sum(pr_mm,na.rm=TRUE)), by=.(operation,codregion,year,month)],
                tx[, .(tmean_c=mean((tmax+tmin)/2,na.rm=TRUE)), by=.(operation,year,month)],
                by=c("operation","year","month"), all=TRUE)
cat(sprintf("[55] CR2MET monthly for %d new mines, %d-%d\n", uniqueN(mon_cr$operation), min(mon_cr$year), max(mon_cr$year)))

# ---- (b2) ERA5-Land monthly bridge 2022-2024 for new mines ------------------
ndays_of <- function(y,m){dm<-c(31,28,31,30,31,30,31,31,30,31,30,31)[m]; dm+as.integer(m==2 & (y%%4==0 & (y%%100!=0|y%%400==0)))}
extract_era5 <- function(ncfile, coords){
  sds<-terra::sds(ncfile); t2m<-sds["t2m"]; tp<-sds["tp"]
  vt<-as.integer(sub(".*valid_time=","",names(t2m))); dts<-as.POSIXct(vt,origin="1970-01-01",tz="UTC")
  yy<-as.integer(format(dts,"%Y")); mm<-as.integer(format(dts,"%m"))
  mt<-as.matrix(terra::extract(t2m,coords)); mp<-as.matrix(terra::extract(tp,coords))
  rbindlist(lapply(seq_len(nrow(coords)), function(i) data.table(operation=newm$operation[i], year=yy, month=mm,
    era5_t2m_C=as.numeric(mt[i,])-273.15, era5_pr_mm=as.numeric(mp[i,])*1000*ndays_of(yy,mm)))) }
coords <- as.matrix(newm[, .(lon,lat)])
era5 <- rbindlist(list(extract_era5(file.path(ERA5DIR,"era5_land_monthly_means_2012_2021.nc"), coords),
                       extract_era5(file.path(ERA5DIR,"era5_land_monthly_means_2022_2025.nc"), coords)))
era5 <- era5[!is.na(year) & era5_t2m_C > -40 & era5_t2m_C < 50 & era5_pr_mm >= 0 & era5_pr_mm < 1500]
ov <- merge(mon_cr[year>=2012 & year<=2021], era5[year>=2012 & year<=2021], by=c("operation","year","month"))
bc <- ov[, .(t_delta=median(tmean_c-era5_t2m_C,na.rm=TRUE),
             pr_ratio=(mean(pr_mm,na.rm=TRUE)+0.1)/(mean(era5_pr_mm,na.rm=TRUE)+0.1)), by=.(operation,month)]
bc[pr_ratio>3, pr_ratio:=3][pr_ratio<0.2, pr_ratio:=0.2]
ext_new <- merge(era5[year>=2022 & year<=2024], bc, by=c("operation","month"))
ext_new <- merge(ext_new, newm[,.(operation,codregion)], by="operation")
ext_new[, `:=`(pr_mm=pmax(0, era5_pr_mm*pr_ratio), tmean_c=era5_t2m_C+t_delta)]
mon_full <- rbind(mon_cr[, .(operation,codregion,year,month,pr_mm,tmean_c)],
                  ext_new[, .(operation,codregion,year,month,pr_mm,tmean_c)])
mon_full <- merge(mon_full, newm[,.(operation,lat)], by="operation", all.x=TRUE)
setorder(mon_full, operation, year, month)

# ---- (b3) SPEI over 1960-2024, ref 1960-2021, clamp ------------------------
compute_spei <- function(df){
  df<-df[order(year,month)]
  if(all(is.na(df$tmean_c))||all(is.na(df$pr_mm))){df[,spei12:=NA_real_];return(df)}
  pet<-tryCatch(as.numeric(SPEI::thornthwaite(df$tmean_c,lat=df$lat[1],na.rm=TRUE)),error=function(e)rep(NA_real_,nrow(df)))
  wb<-df$pr_mm-pet
  s<-tryCatch(SPEI::spei(ts(wb,start=c(min(df$year),1),frequency=12),scale=12,ref.start=c(1960,1),ref.end=c(2021,12),na.rm=TRUE)$fitted,
              error=function(e)rep(NA_real_,nrow(df)))
  s<-as.numeric(s); s[is.nan(s)]<-NA_real_; s<-pmax(-3,pmin(3,s)); df[,`:=`(spei12=s, pet_mm=pet, water_balance=wb)]; df }
ext <- mon_full[, compute_spei(.SD), by=operation]
ext[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
ext <- ext[, .(operation, codregion, year, month, date, pr_mm, tmean_c, pet_mm, water_balance, spei12, source="extract")]
cat(sprintf("[55] extract: %d new ops, %d-%d, SPEI non-NA %.0f%%\n",
            uniqueN(ext$operation), min(ext$year), max(ext$year), 100*mean(!is.na(ext$spei12))))

# ---- combine + write -------------------------------------------------------
allspei <- rbindlist(list(reuse, ext), use.names=TRUE)
setorder(allspei, operation, year, month)
write_parquet(allspei, file.path(PANELS, "55__annuario_minecell_spei_1960_2024.parquet"))
cat(sprintf("[55] wrote 55__annuario_minecell_spei_1960_2024.parquet (%d ops, %d rows, %d-%d)\n",
            uniqueN(allspei$operation), nrow(allspei), min(allspei$year), max(allspei$year)))
print(allspei[year>=2022, .(n=.N, spei_nonNA=sum(!is.na(spei12))), by=.(year)][order(year)])
