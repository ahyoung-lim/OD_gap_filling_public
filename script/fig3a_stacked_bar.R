library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(cowplot)

df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_with_ci_V3.csv"))
source("functions/fn_OD_region.R") # regional classification

global_total <- sum(df$dengue_total_scaled) / 10e5 # 73.9052 million globally 1990-2024

imputed <- sum(df$dengue_total_scaled[df$imputed_weekly | df$imputed_monthly]) / 10e5 # 0.225247
disaggregated <- sum(df$dengue_total_scaled[df$disaggregated_yearly]) / 10e5 # 12.67967

round(imputed / global_total * 100, 1)
round(disaggregated / global_total * 100, 1)


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


# Prepare the data
df_plot <- df %>%
  # region_class() %>%
  group_by(Year, od_region) %>%
  summarise(
    total = sum(dengue_total_scaled, na.rm = T),
    pop = sum(pop_est, na.rm = T),
    .groups = "drop"
  ) %>%
  # Order regions by total contribution for consistent stacking
  group_by(od_region) %>%
  mutate(region_total = sum(total)) %>%
  ungroup() %>%
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


# Calculate total cases per year for Poisson regression
df_global <- df_plot %>%
  mutate(od_region = forcats::fct_reorder(od_region, region_total, .desc = FALSE))

global_by_year <- df_global %>%
  group_by(Year) %>%
  summarise(
    total = sum(total, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop"
  )

# Fit population-adjusted Poisson regression model
poisson_model <- glm(total ~ Year + offset(log(pop)),
  data = global_by_year,
  family = quasipoisson(link = "log")
)

# Extract global model statistics
global_year_coef <- coef(poisson_model)["Year"]
global_year_se <- summary(poisson_model)$coefficients["Year", "Std. Error"]
recent_trend <- (exp(global_year_coef) - 1) * 100
recent_trend_ci_lower <- (exp(global_year_coef - 1.96 * global_year_se) - 1) * 100
recent_trend_ci_upper <- (exp(global_year_coef + 1.96 * global_year_se) - 1) * 100

cat(sprintf(
  "\n=== GLOBAL POPULATION-ADJUSTED GROWTH (1990-2024) ===\n  Annual growth rate: %.1f%% (95%% CI: %.1f%% to %.1f%%)\n",
  recent_trend, recent_trend_ci_lower, recent_trend_ci_upper
))

# --- Multi-period fold increase and growth rates ---
periods <- list(
  "1990-2024" = c(1990, 2024),
  "2000-2024" = c(2000, 2024),
  "2010-2024" = c(2010, 2024)
)

period_results <- data.frame(
  Period = character(),
  Start_Year = numeric(),
  End_Year = numeric(),
  Start_Cases = numeric(),
  End_Cases = numeric(),
  Fold_Increase = numeric(),
  Annual_Growth_Rate = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

for (period_name in names(periods)) {
  start_year <- periods[[period_name]][1]
  end_year <- periods[[period_name]][2]

  period_data <- global_by_year %>%
    filter(Year >= start_year & Year <= end_year)

  # Fit population-adjusted model for this period
  period_model <- glm(total ~ Year + offset(log(pop)),
    data = period_data,
    family = quasipoisson(link = "log")
  )
  period_coef <- coef(period_model)["Year"]
  period_se <- summary(period_model)$coefficients["Year", "Std. Error"]

  start_cases <- period_data$total[period_data$Year == start_year]
  end_cases <- period_data$total[period_data$Year == end_year]

  fold_increase <- end_cases / start_cases
  annual_growth <- (exp(period_coef) - 1) * 100
  ci_lower <- (exp(period_coef - 1.96 * period_se) - 1) * 100
  ci_upper <- (exp(period_coef + 1.96 * period_se) - 1) * 100

  period_results <- rbind(period_results, data.frame(
    Period = period_name,
    Start_Year = start_year,
    End_Year = end_year,
    Start_Cases = start_cases,
    End_Cases = end_cases,
    Fold_Increase = fold_increase,
    Annual_Growth_Rate = annual_growth,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  ))
}

# Print period comparison table
period_table <- period_results %>%
  mutate(
    Start_Cases = scales::comma(Start_Cases),
    End_Cases = scales::comma(End_Cases),
    Fold_Increase = sprintf("%.1fx", Fold_Increase),
    Annual_Growth_Rate = sprintf("%.1f%%", Annual_Growth_Rate),
    CI_Range = sprintf("%.1f%% to %.1f%%", CI_Lower, CI_Upper)
  ) %>%
  select(Period, Start_Cases, End_Cases, Fold_Increase, Annual_Growth_Rate, CI_Range)

cat("\n=== POPULATION-ADJUSTED PERIOD COMPARISON ===\n")
print(period_table)

# # Generate predictions for trend line using the full-period model
# years_for_prediction <- global_by_year %>%
#   select(Year, pop)
#
# predictions <- predict(poisson_model,
#   newdata = years_for_prediction,
#   type = "response",
#   se.fit = TRUE
# )
#
# # Create data frame with predictions and confidence intervals
# trend_data <- data.frame(
#   Year = years_for_prediction$Year,
#   predicted = predictions$fit,
#   se = predictions$se.fit
# ) %>%
#   mutate(
#     lower_ci = predicted - 1.96 * se,
#     upper_ci = predicted + 1.96 * se,
#     lower_ci = pmax(0, lower_ci)
#   )
#
# total_cases <- sum(df_global$total, na.rm = TRUE)
#
#
# # # Create a version using both fill and color in one legend
# # Define your desired legend order
#
# p_global <- ggplot() +
#   geom_col(
#     data = df_global,
#     aes(x = Year, y = total, fill = od_region, color = od_region),
#     width = 0.9,
#     linewidth = 0.2,
#     alpha = 0.85,
#     key_glyph = "rect"
#   ) +
#   geom_ribbon(
#     data = trend_data,
#     aes(x = Year, ymin = lower_ci, ymax = upper_ci),
#     alpha = 0.2,
#     fill = "grey30"
#   ) +
#   geom_line(
#     data = trend_data %>% mutate(trend = "Poisson regression fit"),
#     aes(x = Year, y = predicted, color = trend),
#     linewidth = 1,
#     linetype = "dashed",
#     key_glyph = draw_key_path
#   ) +
#   scale_fill_manual(
#     name = "Legend",
#     values = region_cols,
#     breaks = c(legend_order[1:8], "Poisson regression fit"), # Force geographical order in legend
#     guide = "none"
#   ) +
#   scale_color_manual(
#     name = "",
#     values = c(region_cols, "Poisson regression fit" = "black"),
#     breaks = c(legend_order, "Poisson regression fit"), # Force geographical order in legend
#     guide = guide_legend(
#       override.aes = list(
#         fill = c(region_cols[legend_order[1:8]], NA),
#         shape = c(rep(15, 8), NA),
#         size = c(rep(1.5, 8), 1),
#         linetype = c(rep("blank", 8), "dashed"),
#         linewidth = c(rep(0, 8), 1)
#       ),
#       keywidth = unit(0.8, "cm"),
#       keyheight = unit(0.5, "cm"),
#       byrow = TRUE
#     )
#   ) +
#   scale_x_continuous(
#     breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
#     expand = expansion(mult = c(0.02, 0.02))
#   ) +
#   scale_y_continuous(
#     labels = function(x) {
#       case_when(
#         x >= 1e6 ~ paste0(round(x / 1e6, 1), "M"),
#         x >= 1e3 ~ paste0(round(x / 1e3, 0), "K"),
#         TRUE ~ as.character(round(x, 0))
#       )
#     },
#     breaks = pretty_breaks(n = 5),
#     expand = expansion(mult = c(0, 0.15))
#   ) +
#   labs(
#     title = "Annual reported dengue cases by region",
#     subtitle = paste0("Annual growth: ", sprintf("%.1f", recent_trend), "%"),
#     x = "Year",
#     y = "Annual dengue cases"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.title = element_text(
#       size = 35, face = "bold", color = "grey15", margin = margin(b = 5)
#     ),
#     plot.subtitle = element_text(
#       size = 30, color = "grey40", margin = margin(b = 8)
#     ),
#     axis.title.x = element_text(size = 30, color = "grey30", margin = margin(t = 8)),
#     axis.title.y = element_text(size = 30, color = "grey30", margin = margin(r = 10)),
#     axis.text.x = element_text(size = 28, color = "grey40"),
#     axis.text.y = element_text(size = 28, color = "grey40"),
#     panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     legend.position = c(0.05, 0.95),
#     legend.justification = c(0, 1),
#     legend.title = element_text(size = 20, face = "bold", color = "grey30"),
#     legend.text = element_text(size = 18, color = "grey30"),
#     legend.background = element_rect(
#       fill = alpha("white", 0.9),
#       color = NA
#     ),
#     legend.margin = margin(8, 10, 8, 8),
#     legend.key.spacing.y = unit(0.08, "cm"),
#     plot.margin = margin(15, 15, 15, 15)
#   )
# print(p_global)
#

# region-by-region ===========================================
regions <- levels(df_plot$od_region)
trend_data_list <- list()
model_stats <- list() # Store model statistics separately

for (region in regions) {
  df_region <- df_plot %>%
    filter(od_region == region) %>%
    arrange(Year)

  # Skip if insufficient data
  if (nrow(df_region) < 3) {
    trend_data_list[[region]] <- data.frame()
    model_stats[[region]] <- list(aroc = NA, aroc_ci_lower = NA, aroc_ci_upper = NA)
    next
  }

  # Fit Poisson regression model
  poisson_model <- glm(total ~ Year + offset(log(pop)),
    data = df_region,
    family = quasipoisson(link = "log")
  )

  # Extract model statistics ONCE
  year_coef <- coef(poisson_model)["Year"]
  year_se <- summary(poisson_model)$coefficients["Year", "Std. Error"]
  aroc <- (exp(year_coef) - 1) * 100
  aroc_ci_lower <- (exp(year_coef - 1.96 * year_se) - 1) * 100
  aroc_ci_upper <- (exp(year_coef + 1.96 * year_se) - 1) * 100

  model_stats[[region]] <- list(
    aroc = aroc,
    aroc_ci_lower = aroc_ci_lower,
    aroc_ci_upper = aroc_ci_upper,
    pval = summary(poisson_model)$coefficients["Year", "Pr(>|t|)"]
  )

  # Prepare prediction data with proper population matching
  years_for_prediction <- df_region %>%
    select(Year, pop) %>%
    complete(Year = 1990:2024) %>%
    fill(pop, .direction = "downup") # Fill missing populations

  # Generate predictions
  predictions <- predict(poisson_model,
    newdata = years_for_prediction,
    type = "response",
    se.fit = TRUE
  )

  # Create trend data
  trend_data_list[[region]] <- years_for_prediction %>%
    mutate(
      predicted = predictions$fit,
      lower_ci = pmax(0, predicted - 1.96 * predictions$se.fit),
      upper_ci = predicted + 1.96 * predictions$se.fit
    )
}

# Plotting with improved efficiency
p_list <- list()

for (r in regions) {
  df_region <- df_plot %>% filter(od_region == r)
  trend_data <- trend_data_list[[r]]
  stats <- model_stats[[r]]

  # Calculate summary statistics
  total_cases <- sum(df_region$total, na.rm = TRUE)
  max_year <- df_region %>%
    filter(total == max(total, na.rm = TRUE)) %>%
    pull(Year) %>%
    first()
  max_cases <- max(df_region$total, na.rm = TRUE)

  # Create better subtitle
  subtitle <- sprintf("Population-adj. growth: %.1f%%/yr", stats$aroc)

  # Calculate appropriate y-axis breaks
  y_max <- max(c(df_region$total, trend_data$upper_ci), na.rm = TRUE)

  p <- ggplot() +
    # Bars
    geom_col(
      data = df_region,
      aes(x = Year, y = total),
      fill = region_cols[[r]],
      color = "white",
      width = 0.8,
      linewidth = 0.1,
      alpha = 0.85
    ) +

    # Confidence interval (only if data exists)
    {
      if (nrow(trend_data) > 0 && any(!is.na(trend_data$lower_ci))) {
        geom_ribbon(
          data = trend_data,
          aes(x = Year, ymin = lower_ci, ymax = upper_ci),
          alpha = 0.15,
          fill = "grey20"
        )
      }
    } +

    # Trend line
    {
      if (nrow(trend_data) > 0) {
        geom_line(
          data = trend_data,
          aes(x = Year, y = predicted),
          color = "black",
          linewidth = 1.2,
          linetype = "dashed"
        )
      }
    } +

    # Scales
    scale_x_continuous(
      breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      breaks = pretty_breaks(n = 5),
      expand = expansion(mult = c(0, 0.05))
    ) +

    # Labels
    labs(
      title = r,
      subtitle = subtitle,
      x = NULL,
      y = NULL
    ) +

    # Theme
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(
        size = rel(1.5),
        face = "plain",
        color = "grey15",
        margin = margin(b = 2)
      ),
      plot.subtitle = element_text(
        size = rel(1.3),
        color = "grey30",
        margin = margin(b = 8)
      ),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "grey30"),
      axis.text.y = element_text(color = "grey30"),
      axis.title.y = element_text(
        size = rel(1.2), color = "grey30",
        margin = margin(r = 8)
      ),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.3)
    )

  p_list[[r]] <- p
}

# Combined plot
combined <- wrap_plots(p_list, ncol = 3) +
  plot_annotation(
    # title = "Estimated annual dengue cases and growth trends by region, 1990-2024",
    theme = theme(plot.title = ggtext::element_markdown(size = 20, face = "plain", color = "grey15", hjust = 0.07))
  ) &
  theme(
    axis.text.y = element_text(margin = margin(r = 8)), # increase r (right) margin
    plot.margin = margin(t = 15, r = 5, b = 5, l = 15) # give overall left room if needed
  )

# now add common axis labels with cowplot
fig3a <- ggdraw(combined) +
  draw_label(
    "Number of dengue cases (gap-filled estimates)",
    x = 0.02, # Position in left margin
    y = 0.51, # Centered vertically
    angle = 90,
    vjust = -0.1,
    size = 18,
    fontface = "plain",
    color = "grey15",
    hjust = 0.5
  )

print(fig3a)
ggsave(
  plot = final, "output/figures/fig3a.png", width = 15, height = 9,
  units = "in", dpi = 300, bg = "white"
)
