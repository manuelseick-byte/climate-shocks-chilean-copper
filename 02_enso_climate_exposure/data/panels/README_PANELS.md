# Shared panels — what to consume (and what NOT to)

**Vintage:** see [`../ledger/PANEL_VERSION.txt`](../ledger/PANEL_VERSION.txt). If you
embed a copy of these files in another module, check that stamp to confirm you are
on head before analysing.

**Channel meanings:** the single source of truth is
[`../registry/channel_dictionary.csv`](../registry/channel_dictionary.csv).

## `30__acute_mine_month.parquet` (FLOOD / HEAT, mine-month)

FLOOD has **two** spatial variants — pick deliberately:

| You want… | Use column | Channel string |
|---|---|---|
| the **regional** FLOOD signal (headline, −4.7%) | `stress_flood_regmean_M` / `thr_flood_regmean_M` | `FLOOD_regmean_M` |
| a conservative **point-level** lower bound | `stress_flood_mine` / `thr_flood_mine` | `FLOOD_mine` |

> **`FLOOD_corridor` (region-MAX daily) was dropped entirely on 2026-05-31.**
> The columns `*_flood_corridor` no longer exist in this panel, in the master
> ledger, or in any downstream table/figure. It had been mistaken for the
> regional channel once too often (a region-MAX is an extreme-value envelope, not
> a regional mean). If a join on `stress_flood_corridor` / `thr_flood_corridor`
> errors, that is intentional — use `stress_flood_regmean_M` for the regional
> signal, or `stress_flood_mine` for the point-level lower bound. There is no
> drop-in replacement for the region-MAX; it was never a reportable channel.

HEAT: `stress_heat_mine` / `thr_heat_mine` (channel string `HEAT_mine`, threshold
monthly-mean Tmax ≥ 23.5 °C). The strings are `*_mine`, **not** `*_mine_cell`.

## `31__drought_mine_year.parquet` (DROUGHT, annual)

Headline regressor: `cum3_str_meteo` (3-yr cumulative ENSO-aligned SPEI-12 share,
complete-window only — missing SPEI stays `NA`, never coerced to 0). All-drought
contrast: `cum3_thr_meteo`. SSI is `*_hydro` (appendix triangulation,
non-corroborating). See the dictionary for the full column list.
