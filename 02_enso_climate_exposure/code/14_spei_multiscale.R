# =============================================================================
# 14_spei_multiscale.R
#
# PURPOSE
#   Build shorter SPEI horizons from the already-corrected monthly water-balance
#   inputs.  This avoids mixing the project data with an external SPEI product
#   whose grid, PET method, and reference climatology may differ.
#
#   Outputs:
#     panels/14__minecell_spei_multiscale_1960_2025.parquet
#     panels/14__minecell_spei_multiscale_1960_2025.csv
#     panels/14__minecell_spei_multiscale_annual.csv
#     panels/14__regional_spei_multiscale_1960_2025.parquet
#     panels/14__regional_spei_multiscale_1960_2025.csv
#     panels/14__regional_spei_multiscale_annual.csv
#     tables/14__spei_multiscale_validation.csv
#
# ISOLATION
#   Reads existing SPEI-12/water-balance panels; writes only 14__ outputs.
# =============================================================================

.find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "CLAUDE.md"))) {
    p <- dirname(d)
    if (p == d) stop("CLAUDE.md not found from ", start)
    d <- p
  }
  d
}

ROOT <- .find_root()
source(file.path(ROOT, "mining_module_alternative", "scripts", "00_setup.R"))
suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(SPEI)
})

SCALES <- c(1L, 3L, 6L, 12L)

spei_fit <- function(water_balance, start_year, start_month, scale) {
  fit <- tryCatch(
    {
      obj <- NULL
      invisible(capture.output({
        obj <- suppressWarnings(SPEI::spei(
        ts(water_balance, start = c(start_year, start_month), frequency = 12),
        scale = scale,
        ref.start = c(1960, 1),
        ref.end = c(2021, 12),
        na.rm = TRUE
        ))
      }))
      obj$fitted
    },
    error = function(e) rep(NA_real_, length(water_balance))
  )
  fit <- as.numeric(fit)
  fit[is.nan(fit)] <- NA_real_
  pmax(-3, pmin(3, fit))
}

add_spei_scales <- function(df) {
  df <- as.data.table(df)
  setorder(df, year, month)
  y0 <- df$year[1]
  m0 <- df$month[1]
  for (s in SCALES) {
    df[[paste0("spei", s, "_calc")]] <- spei_fit(df$water_balance, y0, m0, s)
  }
  df
}

add_flags <- function(dt) {
  for (s in SCALES) {
    nm <- paste0("spei", s)
    dt[[paste0("drought_flag_spei", s)]] <- as.integer(!is.na(dt[[nm]]) & dt[[nm]] <= -1)
  }
  dt
}

annualize <- function(dt, id_cols) {
  pieces <- lapply(SCALES, function(s) {
    nm <- paste0("spei", s)
    out <- dt[, .(
      valid_months = sum(!is.na(get(nm))),
      drought_share = ifelse(
        sum(!is.na(get(nm))) == 12L,
        mean(get(nm) <= -1, na.rm = TRUE),
        NA_real_
      ),
      mean_spei = ifelse(
        sum(!is.na(get(nm))) == 12L,
        mean(get(nm), na.rm = TRUE),
        NA_real_
      )
    ), by = c(id_cols, "year")]
    setnames(out,
             c("valid_months", "drought_share", "mean_spei"),
             paste0(c("valid_months_", "drought_share_", "mean_"), "spei", s))
    out
  })
  Reduce(function(x, y) merge(x, y, by = c(id_cols, "year"), all = TRUE), pieces)
}

summarize_scales <- function(dt, dataset) {
  rbindlist(lapply(SCALES, function(s) {
    x <- dt[[paste0("spei", s)]]
    data.table(
      dataset = dataset,
      scale = paste0("SPEI-", s),
      rows = nrow(dt),
      non_missing = sum(!is.na(x)),
      first_nonmissing_year = min(dt$year[!is.na(x)], na.rm = TRUE),
      last_nonmissing_year = max(dt$year[!is.na(x)], na.rm = TRUE),
      drought_months = sum(x <= -1, na.rm = TRUE),
      drought_month_share = mean(x <= -1, na.rm = TRUE),
      min_value = min(x, na.rm = TRUE),
      max_value = max(x, na.rm = TRUE)
    )
  }))
}

compare_spei12 <- function(dt, dataset) {
  ok <- dt[!is.na(spei12) & !is.na(spei12_calc)]
  data.table(
    dataset = dataset,
    check = "recomputed_SPEI12_vs_existing",
    pairs = nrow(ok),
    correlation = cor(ok$spei12, ok$spei12_calc),
    mean_abs_diff = mean(abs(ok$spei12 - ok$spei12_calc)),
    max_abs_diff = max(abs(ok$spei12 - ok$spei12_calc)),
    flag_agreement = mean((ok$spei12 <= -1) == (ok$spei12_calc <= -1))
  )
}

cat("[14] Building mine-cell multiscale SPEI from corrected panel 13 ...\n")
mine <- as.data.table(read_parquet(file.path(PANELS, "13__minecell_spei12_1960_2025.parquet")))
needed <- c("mine_operation", "codregion", "year", "month", "date",
            "pr_mm", "tmean_c", "pet_mm", "water_balance", "spei12",
            "source", "lat", "lon")
miss <- setdiff(needed, names(mine))
if (length(miss)) stop("Panel 13 is missing required columns: ", paste(miss, collapse = ", "))

setorder(mine, mine_operation, year, month)
mine_multi <- mine[, add_spei_scales(.SD), by = mine_operation]
mine_check <- compare_spei12(mine_multi, "minecell_19")
mine_multi[, `:=`(
  spei1 = spei1_calc,
  spei3 = spei3_calc,
  spei6 = spei6_calc,
  spei12 = spei12_calc
)]
mine_multi[, c("spei1_calc", "spei3_calc", "spei6_calc", "spei12_calc") := NULL]
mine_multi <- add_flags(mine_multi)
mine_multi <- mine_multi[, .(
  mine_operation, codregion, year, month, date,
  pr_mm, tmean_c, pet_mm, water_balance,
  spei1, spei3, spei6, spei12,
  drought_flag_spei1, drought_flag_spei3, drought_flag_spei6, drought_flag_spei12,
  source, lat, lon
)]

write_parquet(mine_multi, file.path(PANELS, "14__minecell_spei_multiscale_1960_2025.parquet"))
fwrite(mine_multi, file.path(PANELS, "14__minecell_spei_multiscale_1960_2025.csv"))
mine_annual <- annualize(mine_multi, c("mine_operation", "codregion"))
fwrite(mine_annual, file.path(PANELS, "14__minecell_spei_multiscale_annual.csv"))
cat(sprintf("[14] wrote mine-cell multiscale panels: %d rows, %d mines\n",
            nrow(mine_multi), uniqueN(mine_multi$mine_operation)))

cat("[14] Building regional multiscale SPEI from original regional water-balance panel ...\n")
regional_path <- file.path(ORIG_PANELS, "01c__spei12_monthly.csv")
regional <- fread(regional_path)
needed_reg <- c("codregion", "year", "month", "date", "pr_mm", "tmean_c",
                "pet_mm", "water_balance", "spei12")
miss_reg <- setdiff(needed_reg, names(regional))
if (length(miss_reg)) stop("Regional panel is missing required columns: ", paste(miss_reg, collapse = ", "))

setorder(regional, codregion, year, month)
regional_multi <- regional[, add_spei_scales(.SD), by = codregion]
regional_check <- compare_spei12(regional_multi, "regional_all")
regional_multi[, `:=`(
  spei1 = spei1_calc,
  spei3 = spei3_calc,
  spei6 = spei6_calc,
  spei12 = spei12_calc
)]
regional_multi[, c("spei1_calc", "spei3_calc", "spei6_calc", "spei12_calc") := NULL]
regional_multi <- add_flags(regional_multi)
regional_multi <- regional_multi[, .(
  codregion, year, month, date,
  pr_mm, tmean_c, pet_mm, water_balance,
  spei1, spei3, spei6, spei12,
  drought_flag_spei1, drought_flag_spei3, drought_flag_spei6, drought_flag_spei12
)]

write_parquet(regional_multi, file.path(PANELS, "14__regional_spei_multiscale_1960_2025.parquet"))
fwrite(regional_multi, file.path(PANELS, "14__regional_spei_multiscale_1960_2025.csv"))
regional_annual <- annualize(regional_multi, "codregion")
fwrite(regional_annual, file.path(PANELS, "14__regional_spei_multiscale_annual.csv"))
cat(sprintf("[14] wrote regional multiscale panels: %d rows, %d regions\n",
            nrow(regional_multi), uniqueN(regional_multi$codregion)))

validation <- rbindlist(list(
  summarize_scales(mine_multi, "minecell_19"),
  summarize_scales(regional_multi, "regional_all")
), fill = TRUE)

checks <- rbindlist(list(mine_check, regional_check), fill = TRUE)
fwrite(validation, file.path(TABLES, "14__spei_multiscale_validation.csv"))
fwrite(checks, file.path(TABLES, "14__spei_multiscale_spei12_recompute_check.csv"))

cat("[14] validation summary:\n")
print(validation)
cat("[14] SPEI-12 recomputation check:\n")
print(checks)
cat("[14] done.\n")
