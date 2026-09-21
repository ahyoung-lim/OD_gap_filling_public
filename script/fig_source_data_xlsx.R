# ==============================================================================
# Source data workbooks for the main-text figures (Fig. 1-4)
# ==============================================================================
# The journal asks for the values underlying each figure as one spreadsheet per
# figure, with the panels clearly labelled. Each figure script writes the data
# frames it plots to output/source_data/*_source_data*.csv; this script collects
# them into one workbook per figure (one sheet per panel, plus a README sheet
# that defines every column). Nothing is recomputed here.
#
# Run after:  fig1_consistency.R, fig2_region_share.R, fig3a_stacked_bar_MI.R,
#             fig3b_aroc_MI.R, fig4_wavelet_MI.R
# Output   :  output/source_data/Fig1_source_data.xlsx ... Fig4_source_data.xlsx
# ==============================================================================

suppressMessages(library(openxlsx))

in_dir  <- "output/source_data"
out_dir <- "output/source_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rd <- function(f) {
  p <- file.path(in_dir, f)
  if (!file.exists(p)) stop("Missing ", p, " - run the corresponding figure script first.")
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}

mi_note <- "Model-based estimates and their 95% confidence intervals are pooled across 50 imputed datasets with Rubin's rules."

# figure -> list(sheets = named list of data frames, readme = data frame sheet/column/description)
figs <- list(
  Fig1 = list(
    sheets = list(
      "Fig1a"   = rd("fig1_source_data_a.csv"),
      "Fig1b-e" = rd("fig1_source_data_b-e.csv")
    ),
    readme = rbind(
      c("Fig1a", "Year", "Calendar year"),
      c("Fig1a", "gap_filled_cumulative", "Cumulative reported dengue cases since 1990, gap-filled estimates, global (cases; plotted in millions)"),
      c("Fig1a", "who_all_cumulative", "Cumulative cases in all published WHO sources (cases)"),
      c("Fig1a", "who_db_cumulative", "Cumulative cases in WHO digitised databases only (cases)"),
      c("Fig1b-e", "panel", "Panel letter in Fig. 1"),
      c("Fig1b-e", "who_region", "WHO region shown in the panel"),
      c("Fig1b-e", "Year, *_cumulative", "As in Fig1a, for the WHO region")
    )
  ),
  Fig2 = list(
    sheets = list("Fig2" = rd("fig2_source_data.csv")),
    readme = rbind(
      c("Fig2", "Year", "Calendar year"),
      c("Fig2", "od_region", "OpenDengue region"),
      c("Fig2", "cases", "Gap-filled annual cases in the region"),
      c("Fig2", "percentage", "Share of the global gap-filled total in that year (%)")
    )
  ),
  Fig3 = list(
    sheets = list(
      "Fig3a_bars"          = rd("fig3a_source_data_bars.csv"),
      "Fig3a_trend"         = rd("fig3a_source_data_trend.csv"),
      "Fig3a_growth"        = rd("fig3a_source_data_growth.csv"),
      "Fig3b_countries"     = rd("fig3b_source_data_countries.csv"),
      "Fig3b_region_median" = rd("fig3b_source_data_region_median.csv")
    ),
    readme = rbind(
      c("Fig3a_bars", "od_region, Year, total", "Gap-filled annual cases by region (bars)"),
      c("Fig3a_trend", "fit, lwr, upr", "Fitted quasi-Poisson trend and its 95% CI (dashed line and shaded band), in cases"),
      c("Fig3a_growth", "growth_pct_per_year, lwr, upr", "Population-adjusted annual growth rate of the regional series and its 95% CI (% per year; panel subtitles)"),
      c("Fig3b_countries", "growth_pct_per_year, lwr, upr", "Population-adjusted annual growth rate per country and its 95% CI (% per year; one point per country; the plotted y-axis is truncated)"),
      c("Fig3b_region_median", "median_growth_pct_per_year, n_countries", "Median of the plotted country estimates per region (horizontal lines) and number of countries"),
      c("All sheets", "", mi_note)
    )
  ),
  Fig4 = list(
    sheets = list(
      "Fig4a"       = rd("fig4_source_data_a.csv"),
      "Fig4b"       = rd("fig4_source_data_b.csv"),
      "Fig4c_pairs" = rd("fig4_source_data_c_pairs.csv"),
      "Fig4c_diff"  = rd("fig4_source_data_c_diff.csv")
    ),
    readme = rbind(
      c("Fig4a", "pct_change_per_year, ci_lower, ci_upper", "Trend in wavelet power (% per year) and its 95% CI, by cycle and geographic scope"),
      c("Fig4b", "pct_change_per_year, ci_lower, ci_upper", "Trend in pairwise synchrony (% per year) and its 95% CI, by cycle and geographic scope"),
      c("Fig4c_pairs", "mean_sync", "Mean pairwise synchrony (0-1) of each country pair, averaged across the imputed datasets (violin and box plots)"),
      c("Fig4c_pairs", "hemisphere_pairing", "Within = both countries in the same hemisphere; Between = opposite hemispheres"),
      c("Fig4c_diff", "model_mean, mean_lower, mean_upper", "Model marginal mean synchrony per group and its 95% CI"),
      c("Fig4c_diff", "difference, diff_lower, diff_upper, p_value", "Within minus Between difference on the synchrony scale, its 95% CI and p-value (brackets)"),
      c("All sheets", "", mi_note)
    )
  )
)

for (fig in names(figs)) {
  wb <- createWorkbook()
  readme <- as.data.frame(figs[[fig]]$readme, stringsAsFactors = FALSE)
  names(readme) <- c("sheet", "column", "description")
  addWorksheet(wb, "README")
  writeData(wb, "README", readme)
  setColWidths(wb, "README", cols = 1:3, widths = c(22, 42, 110))
  for (sh in names(figs[[fig]]$sheets)) {
    addWorksheet(wb, sh)
    writeData(wb, sh, figs[[fig]]$sheets[[sh]])
  }
  out <- file.path(out_dir, sprintf("%s_source_data.xlsx", fig))
  saveWorkbook(wb, out, overwrite = TRUE)
  cat(sprintf("wrote %s (%s)\n", out,
    paste(sprintf("%s: %d rows", names(figs[[fig]]$sheets), sapply(figs[[fig]]$sheets, nrow)), collapse = "; ")))
}
