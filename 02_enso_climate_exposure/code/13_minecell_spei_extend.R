# =============================================================================
# 13_minecell_spei_extend.R   (DROUGHT coverage fix, 2022-2025 extension)
#
# PURPOSE
#   Replace the FABRICATED 2022-2025 mine-cell drought zeros with REAL SPEI-12,
#   computed from ERA5-Land monthly temperature + precipitation at each mine
#   cell, bias-corrected against the CR2MET mine-cell climatology on the
#   2012-2021 overlap.  History (1960-2021) is reused verbatim from 01k.
#
#   Method bridge (documented):
#     - 1960-2021 : CR2MET mine-cell monthly P, Tmean  (from 01k, unchanged)
#     - 2022-2025 : ERA5-Land monthly t2m, tp at the mine cell, bias-corrected
#                   per (mine, calendar-month) by additive delta (temperature)
#                   and multiplicative ratio (precipitation) on 2012-2021.
#     - SPEI-12   : Thornthwaite PET -> water balance -> SPEI::spei(scale=12),
#                   standardisation reference window = 1960-2021 (so historical
#                   values are unchanged and 2022-2025 are anomalies vs the same
#                   climatology).  Identical engine to 01k.
#
# ISOLATION: reads 01k (READ-ONLY) and Data_raw ERA5 NetCDF (READ-ONLY).
#            writes ONLY mining_module_alternative/panels/13__*.
#
# Inputs:
#   mining_module/panels/01k__minesite_spei12_monthly.parquet         (history)
#   Data_raw/ERA5_Land/monthly_means/era5_land_monthly_means_2012_2021.nc
#   Data_raw/ERA5_Land/monthly_means/era5_land_monthly_means_2022_2025.nc
# Output:
#   mining_module_alternative/panels/13__minecell_spei12_1960_2025.parquet
#   mining_module_alternative/panels/13__spei_extension_validation.csv
# =============================================================================

.find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "CLAUDE.md"))) {
    p <- dirname(d); if (p == d) stop("CLAUDE.md not found"); d <- p
  }
  d
}
ROOT <- .find_root()
source(file.path(ROOT, "mining_module_alternative", "scripts", "00_setup.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table); library(terra); library(SPEI) })

ERA5DIR <- file.path(ROOT, "Data_raw", "ERA5_Land", "monthly_means")

# ---- 1. history (1960-2021) from 01k ---------------------------------------
hist <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01k__minesite_spei12_monthly.parquet")))
hist <- hist[, .(mine_operation, codregion, year, month, pr_mm, tmean_c, lat, lon,
                 spei12_orig = spei12)]
mines <- unique(hist[, .(mine_operation, codregion, lat, lon)])
cat(sprintf("[13] history: %d mine-months, %d mines, %d-%d\n",
            nrow(hist), nrow(mines), min(hist$year), max(hist$year)))

# ---- 2. ERA5 monthly extraction at mine cells ------------------------------
ndays_of <- function(y, m) {
  dm <- c(31,28,31,30,31,30,31,31,30,31,30,31)[m]
  leap <- (y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0))
  dm + as.integer(m == 2 & leap)
}
extract_era5 <- function(ncfile, pts) {
  sds <- terra::sds(ncfile)
  t2m <- sds["t2m"]; tp <- sds["tp"]
  vt  <- as.integer(sub(".*valid_time=", "", names(t2m)))          # epoch seconds
  dts <- as.POSIXct(vt, origin = "1970-01-01", tz = "UTC")
  yy  <- as.integer(format(dts, "%Y")); mm <- as.integer(format(dts, "%m"))
  coords <- as.matrix(pts[, .(lon, lat)])                         # MATRIX input => no ID column
  m_t <- as.matrix(terra::extract(t2m, coords))                  # rows=mines, cols=months
  m_p <- as.matrix(terra::extract(tp,  coords))
  out <- rbindlist(lapply(seq_len(nrow(pts)), function(i) data.table(
    mine_operation = pts$mine_operation[i], year = yy, month = mm,
    era5_t2m_C  = as.numeric(m_t[i, ]) - 273.15,
    era5_pr_mm  = as.numeric(m_p[i, ]) * 1000 * ndays_of(yy, mm)   # m/day -> mm/month
  )))
  out
}
cat("[13] extracting ERA5-Land at mine cells ...\n")
era5 <- rbindlist(list(
  extract_era5(file.path(ERA5DIR, "era5_land_monthly_means_2012_2021.nc"), mines),
  extract_era5(file.path(ERA5DIR, "era5_land_monthly_means_2022_2025.nc"), mines)
))
era5 <- era5[!is.na(year)]
cat(sprintf("[13] ERA5 extracted: %d rows, %d-%d\n", nrow(era5), min(era5$year), max(era5$year)))

# ---- 3. bias correction on 2012-2021 overlap (per mine x calendar-month) ----
ov <- merge(hist[year >= 2012 & year <= 2021, .(mine_operation, year, month, pr_mm, tmean_c)],
            era5[year >= 2012 & year <= 2021], by = c("mine_operation", "year", "month"))
bc <- ov[, .(t_delta  = mean(tmean_c - era5_t2m_C, na.rm = TRUE),
             pr_ratio = (mean(pr_mm, na.rm = TRUE) + 0.1) /
                        (mean(era5_pr_mm, na.rm = TRUE) + 0.1)),
         by = .(mine_operation, month)]
bc[pr_ratio > 5,  pr_ratio := 5]      # guard hyper-arid ratio blow-ups
bc[pr_ratio < 0.2, pr_ratio := 0.2]
cat(sprintf("[13] bias factors: t_delta [%.2f, %.2f] C ; pr_ratio [%.2f, %.2f]\n",
            min(bc$t_delta), max(bc$t_delta), min(bc$pr_ratio), max(bc$pr_ratio)))

# ---- 4. corrected 2022-2025 monthly P, Tmean -------------------------------
ext <- merge(era5[year >= 2022], bc, by = c("mine_operation", "month"))
ext[, tmean_c := era5_t2m_C + t_delta]
ext[, pr_mm   := pmax(0, era5_pr_mm * pr_ratio)]
ext <- merge(ext, mines, by = "mine_operation")
ext <- ext[, .(mine_operation, codregion, year, month, pr_mm, tmean_c, lat, lon)]

# ---- 5. full 1960-2025 monthly series, recompute PET + SPEI -----------------
full <- rbind(hist[, .(mine_operation, codregion, year, month, pr_mm, tmean_c, lat, lon)],
              ext, fill = TRUE)
setorder(full, mine_operation, year, month)

compute_spei <- function(df) {
  df <- df[order(year, month)]
  lat <- df$lat[1]
  pet <- as.numeric(SPEI::thornthwaite(df$tmean_c, lat = lat, na.rm = TRUE))
  wb  <- df$pr_mm - pet
  s <- tryCatch(
    SPEI::spei(ts(wb, start = c(min(df$year), 1), frequency = 12),
               scale = 12, ref.start = c(1960, 1), ref.end = c(2021, 12),
               na.rm = TRUE)$fitted,
    error = function(e) rep(NA_real_, length(wb)))
  s <- as.numeric(s)
  s[is.nan(s)] <- NA_real_
  s <- pmax(-3, pmin(3, s))           # clamp to standard SPEI range (handles +/-Inf tails)
  df$pet_mm <- pet; df$water_balance <- wb; df$spei12 <- s
  df
}
cat("[13] recomputing SPEI-12 over 1960-2025 (ref window 1960-2021) ...\n")
out <- full[, compute_spei(.SD), by = mine_operation]
out[, source := ifelse(year <= 2021, "cr2met", "era5_ext")]
out[, drought_flag_minesite := as.integer(!is.na(spei12) & spei12 <= -1)]

write_parquet(out[, .(mine_operation, codregion, year, month,
                      date = as.Date(sprintf("%04d-%02d-01", year, month)),
                      pr_mm, tmean_c, pet_mm, water_balance, spei12,
                      drought_flag_minesite, source, lat, lon)],
              file.path(PANELS, "13__minecell_spei12_1960_2025.parquet"))
cat(sprintf("[13] wrote 13__minecell_spei12_1960_2025.parquet (%d rows, %d-%d)\n",
            nrow(out), min(out$year), max(out$year)))

# ---- 6. validation ----------------------------------------------------------
cat("\n[13] VALIDATION\n")
# (a) history preserved vs original 01k
chk <- merge(out[year <= 2021, .(mine_operation, year, month, spei12_new = spei12)],
             hist[, .(mine_operation, year, month, spei12_orig)],
             by = c("mine_operation", "year", "month"))
chk <- chk[!is.na(spei12_new) & !is.na(spei12_orig)]
cat(sprintf("  (a) history 1960-2021 vs 01k: cor=%.4f ; mean|diff|=%.4f ; flag agreement=%.2f%%\n",
            cor(chk$spei12_new, chk$spei12_orig),
            mean(abs(chk$spei12_new - chk$spei12_orig)),
            100*mean((chk$spei12_new<=-1) == (chk$spei12_orig<=-1))))
# (b) seam continuity: |SPEI(2022-01) - SPEI(2021-12)| should be modest (12-mo index)
seam <- merge(out[year==2021 & month==12, .(mine_operation, dec21 = spei12)],
              out[year==2022 & month==1,  .(mine_operation, jan22 = spei12)], by="mine_operation")
cat(sprintf("  (b) seam |Jan22 - Dec21| across mines: mean=%.2f max=%.2f\n",
            mean(abs(seam$jan22-seam$dec21), na.rm=TRUE), max(abs(seam$jan22-seam$dec21), na.rm=TRUE)))
# (c) extension vs regional SPEI (01c) direction, 2022-2024 by region
rc <- fread(file.path(ORIG_PANELS, "01c__spei12_monthly.csv"))
reg_yr <- rc[, .(reg_spei = mean(spei12, na.rm=TRUE)), by=.(codregion, year)]
mine_yr <- out[year %in% 2022:2024, .(mine_spei = mean(spei12, na.rm=TRUE)), by=.(codregion, year)]
val <- merge(mine_yr, reg_yr, by=c("codregion","year"))
cat("  (c) extended mine-cell vs regional SPEI, 2022-2024 (annual mean by region):\n")
print(val[order(codregion, year)])
cat(sprintf("      correlation(mine, regional) over these cells: %.3f\n",
            cor(val$mine_spei, val$reg_spei, use="complete.obs")))
fwrite(val, file.path(PANELS, "13__spei_extension_validation.csv"))
cat("\n[13] done.\n")
