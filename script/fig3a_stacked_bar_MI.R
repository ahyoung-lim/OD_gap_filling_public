# ==============================================================================
# FIG 3A (MI release) — regional annual case bars with population-adjusted trends
# ==============================================================================
# Drawn from the multiple-imputation release:
#   - input: runs/mi_full/mi50/opendengue_gap_filled_MI.csv (written by mi_write_release()
#     in script/03c_mi_datasets.R)
#   - the HEADLINE numbers printed to the console (global/period growth, fold,
#     global total) are read from the pooled MI result files (growth_pooled.csv
#     for growth and fold, aggregate_totals_MI.csv) — the files the manuscript's numbers
#     come from — so the figure/caption text matches the cited values; the
#     data-side computation is printed next to them as a cross-check.
#   - the panel SUBTITLES (regional summed-series growth, 95% CI) are the pooled
#     MI estimates (region_total_growth_pct in growth_pooled.csv, Rubin's rules),
#     and the dashed trend line and grey band are the pooled fitted curve of the
#     same regressions (region_trend_pred_MI.csv: per-dataset quasi-Poisson
#     predictions combined with Rubin's rules) — every displayed quantity carries
#     the imputation uncertainty. The median-of-country-rates regional metric is
#     a different quantity, reported in fig3b.
# Output: output/figures/fig3a_MI.png
# ==============================================================================

library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(cowplot)
library(tidyr)


df <- read.csv(file.path(getwd(), "runs/mi_full/mi50/opendengue_gap_filled_MI.csv"))

# ---- headline numbers: manuscript-cited files + data-side cross-check --------
agg <- read.csv("runs/mi_full/descriptive_summary/aggregate_totals_MI.csv")
growth <- read.csv("runs/mi_full/growth/growth_pooled.csv")
fold <- growth %>% filter(metric == "fold_increase")

global_total_data <- sum(df$dengue_total_scaled)
global_total_cited <- agg$dengue_total[agg$scope == "global"]
stopifnot(global_total_data == global_total_cited)
cat(sprintf(
  "Global total 1990-2024: %s (95%% CI %s-%s)",
  comma(global_total_cited),
  comma(agg$lwr[agg$scope == "global"]), comma(agg$upr[agg$scope == "global"])
))

imputed <- sum(df$dengue_total_scaled[df$imputed_weekly | df$imputed_monthly])
disaggregated <- sum(df$dengue_total_scaled[df$disaggregated_yearly])
cat(sprintf(
  "imputed (weekly/monthly gap-fill): %s (%.1f%%) | disaggregated from annual: %s (%.1f%%)\n",
  comma(imputed), 100 * imputed / global_total_data,
  comma(disaggregated), 100 * disaggregated / global_total_data
))

observed <- global_total_data - imputed - disaggregated
observed / global_total_data * 100

cat("\n=== GROWTH (Rubin 95% CI from growth_pooled.csv) ===\n")
g <- growth %>% filter(metric %in% c("global_growth_pct", "period_growth_pct"))
for (i in seq_len(nrow(g))) {
  cat(sprintf(
    "  %-10s %5.1f%% [%.1f, %.1f]\n",
    g$scope[i], g$point[i], g$lwr[i], g$upr[i]
  ))
}
cat("\n=== FOLD INCREASE (95% UI from growth_pooled.csv) ===\n")
for (i in seq_len(nrow(fold))) {
  cat(sprintf(
    "  %-10s %.1fx [%.1f, %.1f]\n",
    fold$scope[i], fold$point[i], fold$lwr[i], fold$upr[i]
  ))
}

# data-side cross-check of the global growth rate (single quasi-Poisson on the
# release point estimates; should sit inside the pooled estimate's
# between-imputation range)
gby <- df %>%
  group_by(Year) %>%
  summarise(total = sum(dengue_total_scaled), pop = sum(pop_est), .groups = "drop")
m0 <- glm(total ~ Year + offset(log(pop)), data = gby, family = quasipoisson(link = "log"))
cat(sprintf(
  "\n[cross-check] release-data global growth: %.2f%%/yr (pooled MI point %.2f)\n",
  (exp(coef(m0)["Year"]) - 1) * 100,
  growth$point[growth$metric == "global_growth_pct"]
))

# peak years: the year with the largest annual total, globally and per region
cat("\n=== PEAK YEARS (largest annual total) ===\n")
peak_g <- gby %>% slice_max(order_by = total, n = 5)
cat(sprintf("  %-36s %d (%s cases)\n", "Global", peak_g$Year, comma(peak_g$total)))


# ---- regional panels (identical logic to fig3a_stacked_bar.R) ----------------
region_cols <- c(
  "South America" = "#F6D49B",
  "North & Central America" = "#E28E49",
  "Caribbean" = "#F2B06D",
  "East & Southeast Asia" = "#2A6F9E",
  "South Asia" = "#A57DB8",
  "Pacific Islands" = "#B58EA8",
  "Sub-Saharan Africa" = "#6AA84F",
  "Europe, Middle East & North Africa" = "#2A9D8F"
)


df_plot <- df %>%
  group_by(Year, od_region) %>%
  summarise(
    total = sum(dengue_total_scaled, na.rm = TRUE),
    pop = sum(pop_est, na.rm = TRUE), .groups = "drop"
  ) %>%
  group_by(od_region) %>%
  mutate(region_total = sum(total)) %>%
  ungroup() %>%
  mutate(od_region = factor(od_region, levels = c(
    "North & Central America", "Europe, Middle East & North Africa", "East & Southeast Asia",
    "Caribbean", "South Asia", "Pacific Islands",
    "South America", "Sub-Saharan Africa"
  )))


regions <- levels(df_plot$od_region)

# pooled fitted curve per region-year (line + band), from the Rubin pooling in 04b_mi_growth_pool.R of
# the per-dataset quasi-Poisson predictions
region_pred <- read.csv("runs/mi_full/growth/region_trend_pred_MI.csv")
stopifnot(all(regions %in% region_pred$od_region))
trend_data_list <- lapply(setNames(regions, regions), function(r) {
  region_pred %>%
    filter(od_region == r) %>%
    arrange(Year) %>%
    transmute(Year, predicted = fit, lower_ci = lwr, upper_ci = upr)
})

# pooled MI growth of each region's summed series, for the panel subtitles
reg_growth <- growth %>% filter(metric == "region_total_growth_pct")
stopifnot(all(regions %in% reg_growth$scope))

p_list <- list()
for (r in regions) {
  df_region <- df_plot %>% filter(od_region == r)
  trend_data <- trend_data_list[[r]]
  rg <- reg_growth %>% filter(scope == r)
  # short form so the one-line subtitle fits the panel width; the metric
  # (population-adjusted growth) is defined once in the figure caption
  subtitle <- sprintf(
    "Growth: %.1f%%/yr (95%% CI %.1f-%.1f)",
    rg$point, rg$lwr, rg$upr
  )
  p <- ggplot() +
    geom_col(
      data = df_region, aes(x = Year, y = total),
      fill = region_cols[[r]], color = "white", width = 0.8,
      linewidth = 0.1, alpha = 0.85
    ) +
    {
      if (nrow(trend_data) > 0 && any(!is.na(trend_data$lower_ci))) {
        geom_ribbon(
          data = trend_data, aes(x = Year, ymin = lower_ci, ymax = upper_ci),
          alpha = 0.15, fill = "grey20"
        )
      }
    } +
    {
      if (nrow(trend_data) > 0) {
        geom_line(
          data = trend_data, aes(x = Year, y = predicted),
          color = "black", linewidth = 1.2, linetype = "dashed"
        )
      }
    } +
    scale_x_continuous(
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      breaks = pretty_breaks(n = 5), expand = expansion(mult = c(0, 0.05))
    ) +
    labs(title = r, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = rel(1.5), face = "plain", color = "grey15", margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(1.3), color = "grey30", margin = margin(b = 8)),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "grey30"),
      axis.text.y = element_text(color = "grey30"),
      axis.title.y = element_text(size = rel(1.2), color = "grey30", margin = margin(r = 8)),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.3)
    )
  p_list[[r]] <- p
}

combined <- wrap_plots(p_list, ncol = 3) +
  plot_annotation(theme = theme(plot.title = ggtext::element_markdown(
    size = 20, face = "plain", color = "grey15", hjust = 0.07
  ))) &
  theme(
    axis.text.y = element_text(margin = margin(r = 8)),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 15)
  )

fig3a <- ggdraw(combined) +
  draw_label("Number of dengue cases (gap-filled estimates)",
    x = 0.02, y = 0.51, angle = 90, vjust = -0.1, size = 18,
    fontface = "plain", color = "grey15", hjust = 0.5
  )

# print(fig3a)
ggsave(
  plot = fig3a, "output/figures/fig3a_MI.png", width = 15, height = 9,
  units = "in", dpi = 300, bg = "white"
)

# ---- Source data for Fig. 3a: bars, fitted trend with 95% CI, and regional growth rates ----
# Assembled into one workbook per figure by script/fig_source_data_xlsx.R.
dir.create("output/source_data", recursive = TRUE, showWarnings = FALSE)
write.csv(df_plot %>% transmute(od_region, Year, total) %>% arrange(od_region, Year) %>% as.data.frame(),
  "output/source_data/fig3a_source_data_bars.csv",
  row.names = FALSE
)
write.csv(region_pred %>% transmute(od_region, Year, fit, lwr, upr) %>% arrange(od_region, Year),
  "output/source_data/fig3a_source_data_trend.csv",
  row.names = FALSE
)
write.csv(reg_growth %>% transmute(od_region = scope, growth_pct_per_year = point, lwr, upr),
  "output/source_data/fig3a_source_data_growth.csv",
  row.names = FALSE
)
