# =============================================================================
# 60_figures.R   --  the six journal-grade figures
#
#   fig1  Mining context map (19 mines x 7 regions; altitude tier; output)
#   fig2  Anatomy of climate stress (ONI + mine-cell wet/hot days, 1960-2025)
#   fig3  Validation ladder against 20 documented flood events
#   fig4  Coefficient forest plot (FLOOD mine vs regional-mean; HEAT; DROUGHT)
#   fig5  Case study - mine-level output during named ENSO floods
#   fig6  Drought cumulative-effect profile (SPEI vs SSI, 0/1/2/3 lag years)
# =============================================================================

source(file.path("code", "00_setup.R"))

suppressPackageStartupMessages({
  library(ggplot2); library(scales); library(patchwork); library(sf)
})

dpi <- 300

# Project palette ---------------------------------------------------------
ink   <- "#1A1A1A"; rule <- "#B8B8B8"
brand <- "#1F4E79"; warm <- "#A6321F"
flood_lo <- "#2C7FB8"; flood_hi <- "#9FBAE6"
heat_c   <- "#D2691E"; drought_c <- "#8B5A3C"
nino_c <- "#C0392B"; nina_c <- "#1F77B4"

theme_journal <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      text  = element_text(colour = ink),
      plot.title    = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle = element_text(colour = ink, size = rel(0.95)),
      plot.caption  = element_text(colour = "#555555", size = rel(0.8), hjust = 0),
      axis.text     = element_text(colour = ink),
      axis.title    = element_text(colour = ink),
      panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text       = element_text(face = "bold", colour = ink),
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key.height = unit(0.4, "cm")
    )
}

# Inputs ------------------------------------------------------------------
mines  <- fread(file.path(REGISTRY, "mines.csv"))
ledger <- as.data.table(read_parquet(file.path(LEDGER, "20__master_stress_ledger.parquet")))
mc_mon <- as.data.table(read_parquet(file.path(PANELS, "12__minecell_monthly.parquet")))
ec     <- as.data.table(read_parquet(file.path(PANELS, "11__enso_context_monthly.parquet")))
acute  <- as.data.table(read_parquet(file.path(PANELS, "30__acute_mine_month.parquet")))
ann    <- as.data.table(read_parquet(file.path(PANELS, "31__drought_mine_year.parquet")))
co     <- fread(file.path(ORIG_PANELS, "01a__cochilco_copper_mine_monthly.csv"))

# =========================================================================
# fig1 - Mining context map ----------------------------------------------
# =========================================================================
shp_path <- file.path(PROJECT_ROOT, "Data_raw", "INE_Regiones_2017", "Regiones.shp")
chile_ok <- file.exists(shp_path) && requireNamespace("sf", quietly = TRUE)
copper_outputs <- co[, .(cum_kt = sum(copper_kt, na.rm = TRUE)),
                     by = .(mine_operation)]
mines_plot <- merge(mines, copper_outputs, by = "mine_operation", all.x = TRUE)
mines_plot[, output_label := sprintf("%s\n%.0f kt", mine_operation, cum_kt)]

if (chile_ok) {
  chile <- sf::st_read(shp_path, quiet = TRUE)
  chile <- sf::st_transform(chile, 4326)
  if (!"codregion" %in% names(chile)) {
    # try common alternatives
    if      ("COD_REGION" %in% names(chile)) chile$codregion <- as.integer(chile$COD_REGION)
    else if ("COD_REGI"   %in% names(chile)) chile$codregion <- as.integer(chile$COD_REGI)
    else if ("Cod_Reg"    %in% names(chile)) chile$codregion <- as.integer(chile$Cod_Reg)
    else if ("codregi"    %in% names(chile)) chile$codregion <- as.integer(chile$codregi)
    else                                     chile$codregion <- NA_integer_
  }
  chile$is_copper <- chile$codregion %in% COPPER_REGIONS

  # Standard three-letter Chilean region abbreviations (per INE convention)
  region_abbr <- c(`1`="TPC", `2`="ANT", `3`="ATA", `4`="COQ", `5`="VAL",
                   `6`="OHI", `7`="MAU", `8`="BIO", `9`="ARA", `10`="LLA",
                   `11`="AYS", `12`="MAG", `13`="RM",  `14`="LRI",
                   `15`="ARI", `16`="NUB")
  chile$abbr <- region_abbr[as.character(chile$codregion)]

  # Compute label points - centroid of each region polygon
  suppressWarnings({
    chile_lbl <- sf::st_centroid(chile)
    chile_lbl_coords <- sf::st_coordinates(chile_lbl)
  })
  lbl_df <- data.frame(
    codregion = chile$codregion,
    abbr      = chile$abbr,
    is_copper = chile$is_copper,
    lbl_x     = chile_lbl_coords[, "X"],
    lbl_y     = chile_lbl_coords[, "Y"],
    stringsAsFactors = FALSE
  )
  # Place labels in the Pacific west of each region for copper regions (so they don't
  # overlap the mine dots).  Non-copper regions: centroid in-region.
  # The 3-letter abbreviations are wider than Roman numerals so push them a bit further west.
  lbl_df$lbl_x_offset <- ifelse(lbl_df$is_copper, -73.8, lbl_df$lbl_x)
  lbl_df <- lbl_df[!is.na(lbl_df$abbr) & lbl_df$codregion %in% c(1:13, 14, 15, 16), ]

  fig1 <- ggplot() +
    geom_sf(data = chile, aes(fill = is_copper),
            colour = "#555555", linewidth = 0.4) +
    scale_fill_manual(values = c(`TRUE` = "#A8BFD8", `FALSE` = "#D0D0D0"),
                      guide = "none") +
    geom_text(data = lbl_df,
              aes(x = lbl_x_offset, y = lbl_y, label = abbr),
              size = 2.7, colour = "#1A1A1A", fontface = "bold",
              inherit.aes = FALSE) +
    geom_point(data = mines_plot,
               aes(x = lon, y = lat, size = cum_kt, colour = altitude_tier),
               alpha = 0.9) +
    scale_size(range = c(2, 8), name = "Cumulative kt 2003-25",
               breaks = c(5000, 15000, 40000)) +
    scale_colour_manual(values = c(high = "#1F4E79", low = "#D2691E"),
                        name = "Mine altitude",
                        labels = c(high = "High cordillera (>=2,000 m)",
                                   low  = "Low altitude (<2,000 m)")) +
    coord_sf(xlim = c(-76, -64), ylim = c(-37, -17), expand = FALSE) +
    labs(title = "Chile's copper mining geography",
         x = NULL, y = NULL,
         caption = "Source: Cochilco mine-level archive; INE 2017 region polygons.") +
    guides(colour = guide_legend(order = 1, override.aes = list(size = 4)),
           size   = guide_legend(order = 2, nrow = 1)) +
    theme_journal() +
    theme(panel.grid.major = element_line(colour = "#F0F0F0", linewidth = 0.25),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin(0, 0, 0, 0),
          plot.title = element_text(size = rel(1.0)),
          plot.caption = element_text(size = rel(0.75)))
} else {
  fig1 <- ggplot(mines_plot, aes(lon, lat, size = cum_kt, colour = altitude_tier)) +
    geom_point(alpha = 0.85) +
    scale_size(range = c(2, 9), name = "Cumulative kt 2003-2025") +
    scale_colour_manual(values = c(high = "#1F4E79", low = "#D2691E"),
                        name = "Mine altitude tier") +
    coord_fixed(ratio = 1.1) +
    labs(title = "Chile's copper mining geography",
         x = "Longitude", y = "Latitude") +
    theme_journal()
}
ggsave(file.path(FIGURES, "fig1_map.png"), fig1, width = 5.6, height = 6.4, dpi = dpi)
ggsave(file.path(FIGURES, "fig1_map.svg"), fig1, width = 5.6, height = 6.4)
cat("[60] wrote fig1_map\n")

# =========================================================================
# fig2 - Anatomy of climate stress: ONI + mine-cell exposures 1960-2025
# =========================================================================
# Build annual ONI and channel firing counts at the mine-cell level
mc_ann <- mc_mon[year >= 1960 & year <= 2025,
                 .(n_wet_days_year = sum(n_wet_days, na.rm = TRUE)),
                 by = .(year)]
oni_ann <- ec[, .(oni_year = mean(oni_rolling, na.rm = TRUE)),
              by = .(year)]
oni_ann <- unique(oni_ann)
events <- rbindlist(lapply(ENSO_EVENT_WINDOWS, as.data.table))

p2a <- ggplot(oni_ann, aes(year, oni_year)) +
  geom_hline(yintercept = c(-0.5, 0.5), colour = rule, linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = 0, colour = rule, linewidth = 0.3) +
  geom_col(aes(fill = oni_year > 0), width = 0.8) +
  scale_fill_manual(values = c(`TRUE` = nino_c, `FALSE` = nina_c), guide = "none") +
  labs(title = "(a) Oceanic Nino Index (ONI), annual mean",
       subtitle = "Red = El Nino, blue = La Nina; |ONI| >= 0.5 = active phase",
       y = "ONI (annual mean)", x = NULL) +
  scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2025, 10)) +
  theme_journal()

p2b <- ggplot(mc_ann, aes(year, n_wet_days_year)) +
  geom_col(fill = flood_lo, width = 0.8) +
  labs(title = "(b) Days per year with mine-cell P >= 20 mm",
       subtitle = "Total across 19 mines; lower bound on operationally exposed days",
       y = "Mine-days", x = "Year",
       caption = "Source: CR2MET v2.5 daily 1960-2021; ERA5-Land bias-corrected (quantile-mapped) 2022-2025.") +
  scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2025, 10)) +
  theme_journal()

fig2 <- p2a / p2b
ggsave(file.path(FIGURES, "fig2_anatomy.png"), fig2, width = 8.0, height = 5.6, dpi = dpi)
ggsave(file.path(FIGURES, "fig2_anatomy.svg"), fig2, width = 8.0, height = 5.6)
cat("[60] wrote fig2_anatomy\n")

# =========================================================================
# fig3 - Recovery rate of documented flood events
# =========================================================================
rec <- fread(file.path(TABLES, "42__recovery_rate_summary.csv"))
rec_long <- melt(rec, id.vars = c("channel"),
                 measure.vars = c("pct_thr", "pct_aligned", "pct_stress"),
                 variable.name = "step", value.name = "pct")
rec_long[, step := factor(step,
                          levels = c("pct_thr","pct_aligned","pct_stress"),
                          labels = c("Threshold crossed", "ENSO aligned", "Stress flag (both)"))]
rec_long[, channel_label := factor(channel,
                                   levels = c("FLOOD_regmean_M", "FLOOD_mine"),
                                   labels = c("Regional-mean monthly\n(headline)",
                                              "Mine-cell daily\n(conservative lower bound)"))]
rec_long_lbl <- rec_long[, .(channel_label, step, pct,
                             pct_lbl = paste0(round(pct), "%"))]
n_events <- unique(rec$n_events)[1]

fig3 <- ggplot(rec_long_lbl, aes(step, pct, fill = channel_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, colour = NA) +
  geom_text(aes(label = pct_lbl), position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.0, colour = ink) +
  scale_fill_manual(values = c("Regional-mean monthly\n(headline)"             = flood_hi,
                               "Mine-cell daily\n(conservative lower bound)"   = flood_lo),
                    name = "FLOOD definition") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(title = sprintf("Recovery rate against %d documented Chilean flood-disruption episodes", n_events),
       subtitle = "Daily-resolution thresholds recover twice as many events as the monthly-mean variant.",
       x = NULL, y = "Share of documented events with the flag firing",
       caption = "Events: Mar 2015 Atacama aluvion, Apr-May 2017 Cajon del Maipo cluster, Feb / Jun / Aug 2023 storms, etc.") +
  theme_journal() +
  theme(legend.position = "top")
ggsave(file.path(FIGURES, "fig3_validation.png"), fig3, width = 8.0, height = 5.0, dpi = dpi)
ggsave(file.path(FIGURES, "fig3_validation.svg"), fig3, width = 8.0, height = 5.0)
cat("[60] wrote fig3_validation\n")

cat("[60] figures 1-3 complete.  Figures 4-6 require regression outputs (write after 40/41 done).\n")
