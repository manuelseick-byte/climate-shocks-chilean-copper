# =============================================================================
# 53_annuario_minecell_spei.R   (DROUGHT-channel Annuario extension, Step 2)
#
# Build a mine-cell SPEI-12 panel for the ~37 Annuario operations, 1960-2021.
#
# ISOLATION (do not break):
#   * Reads CR2MET rasters and the existing 19-mine SPEI panel (01k) READ-ONLY.
#   * Writes ONLY to mining_module_alternative/panels/53__*.
#   * Does NOT touch the master ledger (20), the acute panels (30), the original
#     drought panel (31), or any mining_module/panels/01* file.
#
# Two sources of SPEI per Annuario operation:
#   (a) REUSE  - operations that coincide with one of the original 19 mine cells
#                take that mine's SPEI-12 from 01k__minesite_spei12_monthly
#                (read-only).  Mapped via ANN2ORIG below.
#   (b) EXTRACT - the new operations (registry/annuario_new_mines_coords.csv)
#                get a fresh CR2MET daily extraction at their 0.05deg cell,
#                aggregated to monthly P / Tmean, Thornthwaite PET, SPEI-12
#                (mirrors mining_module/01k exactly).
#
# Output:
#   mining_module_alternative/panels/53__annuario_minecell_spei.parquet
#       operation, codregion, year, month, date, spei12, source ("reuse"/"extract")
# =============================================================================

source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages({
  library(terra); library(data.table); library(arrow); library(stringr); library(fs); library(SPEI)
})

DATA_RAW <- file.path(PROJECT_ROOT, "Data_raw")

# ---- (a) REUSE: map Annuario canonical -> original-19 mine in 01k ----------
ANN2ORIG <- c(
  "Codelco - Andina"           = "Codelco - Andina",
  "Codelco - Teniente"         = "Codelco - Teniente",
  "Codelco - Salvador"         = "Codelco - Salvador",
  "Codelco - Gabriela Mistral" = "Codelco - Gaby",
  "Codelco - Chuqui/RT/MH"     = "Codelco - Chuqui, RT y MH",
  "Escondida" = "Escondida", "Collahuasi" = "Collahuasi", "Los Pelambres" = "Los Pelambres",
  "Spence" = "Spence", "Candelaria" = "Candelaria", "El Abra" = "El Abra",
  "Quebrada Blanca" = "Quebrada Blanca", "Zaldivar" = "Zaldivar",
  "Cerro Colorado" = "Cerro Colorado", "Lomas Bayas" = "Lomas Bayas",
  "Anglo American Sur" = "Anglo American Sur", "Anglo American Norte" = "Anglo American Norte"
)
spei_orig <- as.data.table(read_parquet(file.path(ORIG_PANELS, "01k__minesite_spei12_monthly.parquet")))
spei_orig <- spei_orig[, .(orig = mine_operation, codregion, year, month, date, spei12)]
reuse <- rbindlist(lapply(names(ANN2ORIG), function(op) {
  d <- spei_orig[orig == ANN2ORIG[op]]
  if (nrow(d) == 0) { cat("  WARN reuse: no 01k rows for", ANN2ORIG[op], "\n"); return(NULL) }
  d[, .(operation = op, codregion, year, month, date, spei12, source = "reuse")]
}))
cat(sprintf("[53] reuse: %d operations from 01k\n", uniqueN(reuse$operation)))

# ---- (b) EXTRACT: fresh CR2MET daily at the new mine cells ------------------
newm <- fread(file.path(REGISTRY, "annuario_new_mines_coords.csv"))
cat(sprintf("[53] extract: %d new operations\n", nrow(newm)))
pts <- terra::vect(as.data.frame(newm[, .(lon, lat)]), geom = c("lon","lat"), crs = "EPSG:4326")

extract_daily <- function(nc_dir, regex, subds_list) {
  files <- fs::dir_ls(nc_dir, regexp = regex) |> sort()
  out <- vector("list", length(files)); t0 <- Sys.time()
  for (k in seq_along(files)) {
    ym <- str_extract(basename(files[k]), "\\d{4}_\\d{2}")
    yr <- as.integer(substr(ym,1,4)); mo <- as.integer(substr(ym,6,7))
    start <- as.Date(sprintf("%04d-%02d-01", yr, mo))
    mats <- lapply(subds_list, function(sd) {
      r <- terra::rast(files[k], subds = sd)
      list(n = terra::nlyr(r), m = terra::extract(r, pts, ID = FALSE))
    })
    n_days <- mats[[1]]$n
    dates <- seq(start, by = "day", length.out = n_days)
    rows <- vector("list", nrow(newm))
    for (i in seq_len(nrow(newm))) {
      dt <- data.table(operation = newm$operation[i], codregion = newm$codregion[i], date = dates)
      for (s in seq_along(subds_list)) dt[[subds_list[s]]] <- as.numeric(mats[[s]]$m[i, ])
      rows[[i]] <- dt
    }
    out[[k]] <- rbindlist(rows)
    if (k %% 120 == 0 || k == length(files))
      cat(sprintf("[53]   %s %d/%d (%.0fs)\n", basename(nc_dir), k, length(files),
                  as.numeric(Sys.time()-t0, units="secs")))
  }
  rbindlist(out)
}

cat("[53] extracting CR2MET precip ...\n")
pr <- extract_daily(file.path(DATA_RAW,"CR2MET","pr"),
                    "CR2MET_pr_v2\\.5_day_\\d{4}_\\d{2}_005deg\\.nc$", c("pr"))
setnames(pr, "pr", "pr_mm")
cat("[53] extracting CR2MET tmin/tmax ...\n")
tx <- extract_daily(file.path(DATA_RAW,"CR2MET","txn"),
                    "CR2MET_tmin_tmax_v2\\.5_day_\\d{4}_\\d{2}_005deg\\.nc$", c("tmax","tmin"))

# ---- monthly aggregate + SPEI-12 (mirror 01k) ------------------------------
pr[, c("year","month") := .(as.integer(format(date,"%Y")), as.integer(format(date,"%m")))]
tx[, c("year","month") := .(as.integer(format(date,"%Y")), as.integer(format(date,"%m")))]
mon_pr <- pr[, .(pr_mm = sum(pr_mm, na.rm=TRUE)), by=.(operation, codregion, year, month)]
mon_tx <- tx[, .(tmean_c = mean((tmax+tmin)/2, na.rm=TRUE)), by=.(operation, year, month)]
mon <- merge(mon_pr, mon_tx, by=c("operation","year","month"), all=TRUE)
mon <- merge(mon, newm[, .(operation, lat)], by="operation", all.x=TRUE)
mon[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
setorder(mon, operation, year, month)

compute_spei <- function(df) {
  df <- df[order(date)]
  if (all(is.na(df$tmean_c)) || all(is.na(df$pr_mm))) { df[, spei12 := NA_real_]; return(df) }
  pet <- tryCatch(as.numeric(SPEI::thornthwaite(df$tmean_c, lat = df$lat[1], na.rm = TRUE)),
                  error = function(e) rep(NA_real_, nrow(df)))
  wb <- df$pr_mm - pet
  s <- tryCatch(SPEI::spei(ts(wb, start=c(min(df$year),1), frequency=12), scale=12, na.rm=TRUE)$fitted,
                error = function(e) rep(NA_real_, nrow(df)))
  df[, spei12 := as.numeric(s)]
  df
}
ext <- mon[, compute_spei(.SD), by = operation]
ext <- ext[, .(operation, codregion, year, month, date, spei12, source = "extract")]
cat(sprintf("[53] extract: %d operations, SPEI non-NA in %.0f%% of rows\n",
            uniqueN(ext$operation), 100*mean(!is.na(ext$spei12))))
# coverage check per new mine
covchk <- ext[, .(n = .N, n_spei = sum(!is.na(spei12))), by = operation][order(n_spei)]
cat("[53] new-mine SPEI coverage (lowest first):\n"); print(head(covchk, 8))

# ---- combine and write -----------------------------------------------------
allspei <- rbindlist(list(reuse, ext), use.names = TRUE)
setorder(allspei, operation, year, month)
out <- file.path(PANELS, "53__annuario_minecell_spei.parquet")
write_parquet(allspei, out)
cat(sprintf("[53] wrote %s  (%d operations, %d rows, %d-%d)\n", out,
            uniqueN(allspei$operation), nrow(allspei), min(allspei$year), max(allspei$year)))
