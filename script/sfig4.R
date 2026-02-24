library(tidyverse)
library(patchwork)
library(slider)
library(ggtext)
library(cowplot)

source("functions/fn_OD_region.R")

tab <- read.csv("data/processed_data/dt_heatmap_calibrated_2025_10_22.csv") %>%
  add_od_regions()

df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_with_ci_V3.csv")) %>%
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

df %>%
  group_by(adm_0_name) %>%
  summarise(
    n_subannual = sum(has_complete_subannual),
    pct_subannual = n_subannual / (35 * 12),
    total_cases = sum(dengue_total_scaled)
  ) %>%
  arrange(desc(total_cases), pct_subannual) %>%
  filter(n_subannual < 60)


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
  summarise(
    average_pct = mean(pct_subannual)
  )

#   decade average_pct
#   <fct>        <dbl>
# 1 1990s         8.47
# 2 2000s        12.1
# 3 2010s        29.2
# 4 2020s        46.4

df %>%
  group_by(Year, od_region) %>%
  mutate(
    n_countries = n_distinct(adm_0_name) * 12,
    n_with_subannual = sum(has_complete_subannual),
    pct_subannual = 100 * n_with_subannual / n_countries,
    .groups = "drop"
  ) %>%
  group_by(decade, od_region) %>%
  summarise(
    average_pct = mean(pct_subannual)
  ) %>%
  pivot_wider(names_from = "decade", values_from = "average_pct")

#   od_region                          `1990s` `2000s` `2010s` `2020s`
#   <chr>                                <dbl>   <dbl>   <dbl>   <dbl>
# 1 Caribbean                             0       0      24.1     54.5
# 2 East & Southeast Asia                25      57.5    69.3     58.5
# 3 Europe, Middle East & North Africa    0       4      27.7     52.2
# 4 North & Central America               0      27.7    66.7     97.4
# 5 Pacific Islands                      31.4     4.55   13.5     23.6
# 6 South America                         6.15   20.8    49.2     80
# 7 South Asia                            4.44   12.2    35.6     57.8
# 8 Sub-Saharan Africa                    0       1.14    6.67    18.1


region_cols <- c(
  "South America" = "#F6D49B", # light muted amber (less dominant)
  "North & Central America" = "#E28E49", # warm burnt orange (Americas family)
  "Caribbean" = "#F2B06D", # peachy/orange (related but distinct)
  "East & Southeast Asia" = "#2A6F9E", # desaturated deep blue/teal (strong but calm contrast)
  "South Asia" = "#A57DB8", # soft muted purple
  "Pacific Islands" = "#B58EA8", # muted mauve/pastel
  "Sub-Saharan Africa" = "#6AA84F", # mid green (intuitive)
  "Europe, Middle East & North Africa" = "#2A9D8F" # teal (cool, distinct from green)
  # "Global"                             = "#BDBDBD"   # neutral grey for "global"
)



region_year_summary <- df %>%
  group_by(Year, od_region) %>%
  summarise(
    n_countries = n_distinct(adm_0_name) * 12,
    n_with_subannual = sum(has_complete_subannual),
    pct_subannual = 100 * n_with_subannual / n_countries
  ) %>%
  group_by(od_region) %>%
  arrange(Year) %>%
  mutate(
    # Trailing 5-year average
    pct_smooth = slide_dbl(pct_subannual, mean, .before = 2, .after = 2, .complete = FALSE),

    # Trailing 5-year SD
    pct_sd = slide_dbl(pct_subannual, ~ {
      if (length(.x) <= 1) {
        return(0)
      }
      sd(.x)
    }, .before = 2, .after = 2, .complete = FALSE),

    # Count observations
    n_obs = slide_int(pct_subannual, length, .before = 2, .after = 2, .complete = FALSE),

    # SE and CI
    se = ifelse(n_obs > 1, pct_sd / sqrt(n_obs), 0),
    pct_lower = pmax(0, pct_smooth - 1.96 * se),
    pct_upper = pmin(100, pct_smooth + 1.96 * se)
  ) %>%
  ungroup()

region_year_summary <- region_year_summary %>%
  mutate(
    od_region = factor(od_region, levels = c(
      # Row 1: Northern/Top (West to East)
      "North & Central America", # Northwest
      "Europe, Middle East & North Africa", # North-Center
      "East & Southeast Asia", # Northeast
      # Row 2: Middle latitude (West to East)
      "Caribbean", # West-Center
      "South Asia", # Center
      "Pacific Islands", # East
      # Row 3: Southern (West to East)
      "South America", # Southwest
      "Sub-Saharan Africa" # South-Center
    ))
  )


regions <- levels(region_year_summary$od_region)

p_list <- list()

for (r in regions) {
  df_region <- region_year_summary[region_year_summary$od_region == r, ]

  # Plot with uncertainty ribbon
  p <- ggplot(df_region, aes(x = Year)) +

    # 5-year rolling average line
    geom_line(aes(y = pct_smooth),
      color = region_cols[[r]],
      linewidth = 2, na.rm = TRUE
    ) +
    # Raw data line
    geom_line(aes(y = pct_subannual),
      color = "gray50", linewidth = 0.5, alpha = 0.7
    ) +
    # Raw data points
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
    # Improved labels with informative subtitle
    labs(
      title = paste0(r),
      # subtitle = df_region$facet_label[1],
      x = NULL, # Remove x-axis title for cleaner look in grid
      y = NULL # Remove y-axis title for cleaner look in grid
    ) +
    # Clean theme optimized for small multiples
    theme_minimal(base_size = 10) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),

      # Typography
      plot.title = element_text(
        size = 18,
        face = "plain",
        color = "grey15",
        margin = margin(b = 2)
      ),
      # plot.subtitle = element_text(
      #   size = 12,
      #   color = "grey40",
      #   margin = margin(b = 8)
      # ),

      # Axes
      axis.text.x = element_text(size = 15, color = "grey40", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 15, color = "grey40"),

      # Grid
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),

      # Remove legend (not needed for single-region plots)
      legend.position = "none",

      # Margins
      plot.margin = margin(15, 15, 15, 15),

      # Panel border for definition
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.3)
    )

  p_list[[r]] <- p
}


# combine with patchwork and add a main title
combined <- wrap_plots(p_list, ncol = 3) +
  plot_annotation(
    theme = theme(plot.title = ggtext::element_markdown(size = 18, face = "plain", color = "grey15", hjust = 0.07))
  ) &
  theme(
    axis.text.y = element_text(margin = margin(r = 10)), # increase r (right) margin
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15) # give overall left room if needed
  )


# now add common axis labels with cowplot
final <- ggdraw(combined) +
  draw_label(
    "Countries with sub-annual data (%)",
    x = 0.03, # Position in left margin
    y = 0.51, # Centered vertically
    angle = 90,
    vjust = -0.9,
    size = 20,
    fontface = "plain",
    color = "grey15",
    hjust = 0.5
  )

print(final)
ggsave(
  plot = final,
  "output/figures/sfig4.png", width = 15, height = 9, , bg = "white",
  units = "in", dpi = 300
)
