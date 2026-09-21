# CV_tab3c_fig_metrics_by_region_heatmap.R
#
# Heatmaps of region-stratified cross-validation performance of the final
# models, from the tables written by script/CV/CV_tab3b_metrics_by_region.R:
#   weekly imputation      : INLA          (metrics_table_weekly_by_region.csv)
#   monthly imputation     : INLA          (metrics_table_monthly_by_region.csv)
#   disaggregation         : INLA scaled   (metrics_table_dm_inla_by_region.csv)
#
# Layout per figure: x = CV task (interpolation, extrapolation backcast / forecast),
# y = OpenDengue region, cell = metric value, one panel per metric
# (nMAE, 80% coverage). nMAE uses a sequential scale (lower is better);
# coverage uses a diverging scale centred on the nominal 0.80.
#
# Outputs: output/figures/sfig_metrics_by_region_{weekly,monthly,disaggregation}.png

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

run_dir <- file.path("runs", "CV", "20260126")
fig_dir <- file.path("output", "figures")

region_levels <- c(
  "South America", "North & Central America", "Caribbean",
  "East & Southeast Asia", "South Asia", "Pacific Islands",
  "Sub-Saharan Africa", "Europe, Middle East & North Africa"
)
all_label <- "All regions"
task_levels <- c(interp = "Interpolation", extrap_past = "Extrapolation\n(backcast)", extrap_future = "Extrapolation\n(forecast)")

tables <- list(
  weekly         = list(file = "metrics_table_weekly_by_region.csv",   model = "INLA"),
  monthly        = list(file = "metrics_table_monthly_by_region.csv",  model = "INLA"),
  disaggregation = list(file = "metrics_table_dm_inla_by_region.csv",  model = "INLA scaled")
)

read_final_model <- function(file, model) {
  read_csv(file.path(run_dir, file), show_col_types = FALSE) %>%
    filter(Model == model) %>%
    transmute(
      task = factor(task_levels[mask_type], levels = task_levels),
      # "All regions" at the top, then the regions in the order used in the tables
      region = factor(od_region, levels = rev(c(all_label, region_levels))),
      nMAE = nMAE_median,
      COV80 = cov80
    )
}

heat_panel <- function(df, value, title, fill_scale, label_fmt) {
  ggplot(df, aes(x = task, y = region, fill = .data[[value]])) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf(label_fmt, .data[[value]])), size = 3.2) +
    fill_scale +
    scale_x_discrete(position = "top") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0),
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

make_figure <- function(df, nmae_limits, legend = TRUE) {
  p_nmae <- heat_panel(
    df, "nMAE", "nMAE",
    scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "nMAE", limits = nmae_limits),
    "%.2f"
  )
  p_cov <- heat_panel(
    df, "COV80", "80% coverage",
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0.80, limits = c(0.5, 1.0), name = "Coverage"),
    "%.2f"
  ) + theme(axis.text.y = element_blank())
  if (!legend) {
    p_nmae <- p_nmae + theme(legend.position = "none")
    p_cov <- p_cov + theme(legend.position = "none")
  }
  p_nmae + p_cov
}

dfs <- lapply(tables, function(t) read_final_model(t$file, t$model))

# Same nMAE colour range in all three figures so they can be compared
nmae_limits <- range(unlist(lapply(dfs, function(d) d$nMAE)))

for (nm in names(tables)) {
  df <- dfs[[nm]]
  fig <- make_figure(df, nmae_limits)
  out <- file.path(fig_dir, sprintf("sfig_metrics_by_region_%s.png", nm))
  ggsave(out, fig, width = 9, height = 4.8, dpi = 300, bg = "white")
  cat("Saved:", out, "\n")
}

# Combined figure: one row per resolution (a weekly, b monthly,
# c disaggregation). All rows share the same colour scales, so the legends
# are shown once, under the last row.
rows <- list(
  wrap_elements(full = make_figure(dfs$weekly, nmae_limits, legend = FALSE)),
  wrap_elements(full = make_figure(dfs$monthly, nmae_limits, legend = FALSE)),
  wrap_elements(full = make_figure(dfs$disaggregation, nmae_limits, legend = TRUE))
)
combined <- rows[[1]] / rows[[2]] / rows[[3]] +
  plot_layout(heights = c(1, 1, 1.22)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 14))
out <- file.path(fig_dir, "sfig_metrics_by_region.png")
ggsave(out, combined, width = 9, height = 13.5, dpi = 300, bg = "white")
cat("Saved:", out, "\n")
