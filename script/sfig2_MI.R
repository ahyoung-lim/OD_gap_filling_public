# ==============================================================================
# SUPP FIG (manuscript Supplementary Figure 2) — temporal
# trends in sub-annual dengue data availability by region, 1990-2024
# ==============================================================================
# Input: runs/mi_full/mi50/opendengue_gap_filled_MI.csv, with the provenance flags
# imputed_weekly / imputed_monthly / disaggregated_yearly. A month counts as
# observed sub-annual data when none of the provenance flags is set and the
# country-year has cases.
# Output: output/figures/sfig2.png
# ==============================================================================

library(tidyverse)
library(patchwork)
library(slider)
library(ggtext)
library(cowplot)

df <- read.csv(file.path(getwd(), "runs/mi_full/mi50/opendengue_gap_filled_MI.csv")) %>%
  group_by(adm_0_name, Year) %>%
  mutate(annual_total = sum(dengue_total_scaled)) %>%
  ungroup() %>%
  mutate(
    has_complete_subannual = case_when(
      !imputed_weekly & !imputed_monthly & !disaggregated_yearly & annual_total != 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    decade = factor(floor(Year / 10) * 10, labels = c("1990s", "2000s", "2010s", "2020s"))
  )

cat("\n=== share of country-months with observed sub-annual data, by decade ===\n")
df %>%
  group_by(Year) %>%
  summarise(
    n_countries = n_distinct(adm_0_name) * 12,
    n_with_subannual = sum(has_complete_subannual),
    pct_subannual = 100 * n_with_subannual / n_countries,
    .groups = "drop"
  ) %>%
  mutate(decade = factor(floor(Year / 10) * 10, labels = c("1990s", "2000s", "2010s", "2020s"))) %>%
  group_by(decade) %>%
  summarise(average_pct = mean(pct_subannual)) %>%
  as.data.frame() %>%
  print(row.names = FALSE, digits = 3)

cat("\n=== share of country-months with observed sub-annual data, by decade ===\n")
df %>%
  group_by(Year, od_region) %>%
  summarise(
    n_countries = n_distinct(adm_0_name) * 12,
    n_with_subannual = sum(has_complete_subannual),
    pct_subannual = 100 * n_with_subannual / n_countries,
    .groups = "drop"
  ) %>%
  mutate(decade = factor(floor(Year / 10) * 10, labels = c("1990s", "2000s", "2010s", "2020s"))) %>%
  group_by(decade, od_region) %>%
  summarise(average_pct = mean(pct_subannual)) %>%
  as.data.frame() %>%
  print(row.names = FALSE, digits = 3)

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

region_year_summary <- df %>%
  group_by(Year, od_region) %>%
  summarise(
    n_countries = n_distinct(adm_0_name) * 12,
    n_with_subannual = sum(has_complete_subannual),
    pct_subannual = 100 * n_with_subannual / n_countries,
    .groups = "drop"
  ) %>%
  group_by(od_region) %>%
  arrange(Year) %>%
  mutate(
    pct_smooth = slide_dbl(pct_subannual, mean, .before = 2, .after = 2, .complete = FALSE),
    pct_sd = slide_dbl(pct_subannual, ~ {
      if (length(.x) <= 1) {
        return(0)
      }
      sd(.x)
    }, .before = 2, .after = 2, .complete = FALSE),
    n_obs = slide_int(pct_subannual, length, .before = 2, .after = 2, .complete = FALSE),
    se = ifelse(n_obs > 1, pct_sd / sqrt(n_obs), 0),
    pct_lower = pmax(0, pct_smooth - 1.96 * se),
    pct_upper = pmin(100, pct_smooth + 1.96 * se)
  ) %>%
  ungroup()

region_year_summary <- region_year_summary %>%
  mutate(
    od_region = factor(od_region, levels = c(
      "North & Central America", "Europe, Middle East & North Africa", "East & Southeast Asia",
      "Caribbean", "South Asia", "Pacific Islands",
      "South America", "Sub-Saharan Africa"
    ))
  )

regions <- levels(region_year_summary$od_region)
p_list <- list()

for (r in regions) {
  df_region <- region_year_summary[region_year_summary$od_region == r, ]
  p <- ggplot(df_region, aes(x = Year)) +
    geom_line(aes(y = pct_smooth),
      color = region_cols[[r]], linewidth = 2, na.rm = TRUE
    ) +
    geom_line(aes(y = pct_subannual),
      color = "gray50", linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(aes(y = pct_subannual),
      size = 1.1, alpha = 0.6, color = "gray30"
    ) +
    scale_x_continuous(
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100),
      breaks = seq(0, 100, by = 25)
    ) +
    labs(title = paste0(r), x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 18, face = "plain", color = "grey15", margin = margin(b = 2)),
      axis.text.x = element_text(size = 15, color = "grey40", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 15, color = "grey40"),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      legend.position = "none",
      plot.margin = margin(15, 15, 15, 15),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.3)
    )
  p_list[[r]] <- p
}

combined <- wrap_plots(p_list, ncol = 3) +
  plot_annotation(
    theme = theme(plot.title = ggtext::element_markdown(size = 18, face = "plain", color = "grey15", hjust = 0.07))
  ) &
  theme(
    axis.text.y = element_text(margin = margin(r = 10)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15)
  )

final <- ggdraw(combined) +
  draw_label(
    "Countries with sub-annual data (%)",
    x = 0.03, y = 0.51, angle = 90, vjust = -0.9, size = 20,
    fontface = "plain", color = "grey15", hjust = 0.5
  )

print(final)
ggsave(
  plot = final,
  "output/figures/sfig2.png", width = 15, height = 9, bg = "white",
  units = "in", dpi = 300
)
