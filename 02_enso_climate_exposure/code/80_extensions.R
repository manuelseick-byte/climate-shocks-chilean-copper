# =============================================================================
# 80_extensions.R  --  ad-hoc extensions, not part of the report.
#
# (b) Water-source heterogeneity: does the drought coefficient differ for
#     mines that adopted seawater (raw or desalinated) vs continental-only?
# (c) Pre-2010 placebo + rolling-window endpoint analysis: how does cum3
#     evolve as the sample endpoint moves from pre-megadrought toward 2025?
# =============================================================================

# share-package: locate scripts/00_setup.R next to this file
.find_setup <- function() {
  here <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  cand <- here
  for (i in 1:6) {
    p <- file.path(cand, "scripts", "00_setup.R")
    if (file.exists(p)) return(p)
    parent <- dirname(cand); if (parent == cand) break; cand <- parent
  }
  stop("scripts/00_setup.R not found from ", here)
}
source(.find_setup())

suppressPackageStartupMessages({ library(fixest) })

# Load drought annual panel
ann <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
ann <- ann[in_sample == TRUE]
ann[, mine := factor(mine_operation)]
ann[, yr   := factor(year)]
ann[, region_clust := factor(codregion)]

# ============================================================================
# (b) Water-source heterogeneity
# ============================================================================
# Classification: mine x year indicator = 1 if mine had operational
# seawater intake (raw or desalinated) in that year.  Sources: company
# sustainability reports + Cochilco water-consumption publications.
#
# Esperanza, El Tesoro  : raw seawater since 2011 (Centinela district, AMSA)
# Escondida             : EWS desal from 2018, EWS+ from 2020
# Spence                : SGO desal from 2021 (BHP)
# Codelco - Chuqui, RT  : DAS seawater pipeline from 2022
# Quebrada Blanca       : QB2 desal from late 2023 (Teck)
# Los Pelambres         : INCO desal from 2024 (AMSA)
# Other 13 mines        : continental water throughout 2003-2025
# ============================================================================

sw_start <- list(
  "Esperanza" = 2011L,
  "El Tesoro" = 2011L,
  "Escondida" = 2018L,
  "Spence"    = 2021L,
  "Codelco - Chuqui, RT y MH" = 2022L,
  "Quebrada Blanca" = 2023L,
  "Los Pelambres"   = 2024L
)
ann[, seawater_yr := 0L]
for (m in names(sw_start)) {
  ann[mine_operation == m & year >= sw_start[[m]], seawater_yr := 1L]
}
ann[, ever_seawater := as.integer(mine_operation %in% names(sw_start))]

cat("\n========== (b) Water-source heterogeneity ==========\n")
cat("Mine-years with operational seawater intake:",
    sum(ann$seawater_yr), "of", nrow(ann), "\n")
cat("Mines that ever adopt seawater (in sample):",
    length(intersect(unique(ann$mine_operation), names(sw_start))), "\n\n")

# --- Headline interaction on SPEI cum3 ---
cat("--- SPEI-12 cum3 x time-varying seawater indicator ---\n")
m_spei <- feols(log_kt_year ~ cum3_str_meteo * seawater_yr | mine + yr,
                data = ann, cluster = ~ region_clust, notes = FALSE)
print(coeftable(m_spei))

cat("\n--- SSI-12 cum3 x time-varying seawater indicator ---\n")
m_ssi <- feols(log_kt_year ~ cum3_str_hydro * seawater_yr | mine + yr,
               data = ann, cluster = ~ region_clust, notes = FALSE)
print(coeftable(m_ssi))

# --- Split-sample: continental-throughout subset ---
cat("\n--- Continental-throughout mines only (drop ever-seawater) ---\n")
cont <- ann[ever_seawater == 0]
cat("Subset rows:", nrow(cont),
    "  mines:", uniqueN(cont$mine_operation),
    "  regions:", uniqueN(cont$codregion), "\n")
m_cont_spei <- feols(log_kt_year ~ cum3_str_meteo | mine + yr,
                     data = cont, cluster = ~ region_clust, notes = FALSE)
m_cont_ssi  <- feols(log_kt_year ~ cum3_str_hydro | mine + yr,
                     data = cont, cluster = ~ region_clust, notes = FALSE)
cat("SPEI-12 cum3 (continental-only):\n"); print(coeftable(m_cont_spei))
cat("SSI-12 cum3 (continental-only):\n");  print(coeftable(m_cont_ssi))

# --- Pre-/post-seawater within Centinela (Esperanza + El Tesoro), 2011 shift ---
cat("\n--- Esperanza+ElTesoro: pre vs post 2011 seawater startup ---\n")
cent <- ann[mine_operation %in% c("Esperanza","El Tesoro")]
cent[, post := as.integer(year >= 2011)]
cat("Centinela rows:", nrow(cent),
    "  pre-2011 rows:", sum(cent$post == 0),
    "  post-2011 rows:", sum(cent$post == 1), "\n")
# Single-mine fixed effects + interaction
if (sum(cent$post == 0) >= 5) {
  m_cent <- feols(log_kt_year ~ cum3_str_meteo * post | mine + yr,
                  data = cent, cluster = ~ mine, notes = FALSE)
  print(coeftable(m_cent))
} else {
  cat("(skipped, not enough pre-2011 mine-years in active sample)\n")
}

# ============================================================================
# (c) Pre-2010 placebo + rolling-window endpoint analysis
# ============================================================================
cat("\n========== (c) Pre-2010 placebo & rolling endpoint ==========\n")

# (c.1) Pre-2010 placebo: just years <= 2010
cat("\n--- Sub-sample 2006-2010 placebo (pre-megadrought) ---\n")
pre <- ann[year <= 2010]
cat("Pre-2010 rows:", nrow(pre),
    "  mines:", uniqueN(pre$mine_operation),
    "  years:", paste(sort(unique(pre$year)), collapse = ",") , "\n")
m_pre_spei <- try(feols(log_kt_year ~ cum3_str_meteo | mine + yr,
                        data = pre, cluster = ~ region_clust, notes = FALSE),
                  silent = TRUE)
if (!inherits(m_pre_spei, "try-error")) {
  cat("SPEI-12 cum3 (2006-2010):\n"); print(coeftable(m_pre_spei))
} else {
  cat("SPEI-12 pre-2010 regression failed:", as.character(m_pre_spei), "\n")
}
m_pre_ssi <- try(feols(log_kt_year ~ cum3_str_hydro | mine + yr,
                       data = pre, cluster = ~ region_clust, notes = FALSE),
                 silent = TRUE)
if (!inherits(m_pre_ssi, "try-error")) {
  cat("SSI-12 cum3 (2006-2010):\n"); print(coeftable(m_pre_ssi))
} else {
  cat("SSI-12 pre-2010 regression failed:", as.character(m_pre_ssi), "\n")
}

# (c.2) Rolling-window: sub-sample 2006..end_year, estimate cum3
cat("\n--- Rolling-endpoint cum3 coefficient (2006..Y) ---\n")
end_years <- 2010:2025
rw <- data.table()
for (Y in end_years) {
  sub <- ann[year <= Y]
  if (uniqueN(sub$year) < 3) next
  m1 <- try(feols(log_kt_year ~ cum3_str_meteo | mine + yr,
                  data = sub, cluster = ~ region_clust, notes = FALSE),
            silent = TRUE)
  m2 <- try(feols(log_kt_year ~ cum3_str_hydro | mine + yr,
                  data = sub, cluster = ~ region_clust, notes = FALSE),
            silent = TRUE)
  row <- data.table(end_year = Y, n_obs = nrow(sub),
                    n_years = uniqueN(sub$year))
  if (!inherits(m1, "try-error") && "cum3_str_meteo" %in% rownames(coeftable(m1))) {
    ct <- coeftable(m1)
    row[, `:=`(spei_beta = round(ct["cum3_str_meteo","Estimate"], 3),
               spei_se   = round(ct["cum3_str_meteo","Std. Error"], 3),
               spei_p    = round(ct["cum3_str_meteo","Pr(>|t|)"], 3))]
  }
  if (!inherits(m2, "try-error") && "cum3_str_hydro" %in% rownames(coeftable(m2))) {
    ct <- coeftable(m2)
    row[, `:=`(ssi_beta = round(ct["cum3_str_hydro","Estimate"], 3),
               ssi_se   = round(ct["cum3_str_hydro","Std. Error"], 3),
               ssi_p    = round(ct["cum3_str_hydro","Pr(>|t|)"], 3))]
  }
  rw <- rbind(rw, row, fill = TRUE)
}
print(rw)

cat("\n[80] done.\n")
