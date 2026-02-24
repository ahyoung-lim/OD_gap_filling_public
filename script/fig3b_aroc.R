library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(ggbeeswarm)
library(tidyverse)
library(ggrepel)

source("functions/fn_OD_region.R") # regional classification

df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_with_ci_V3.csv"))

annual <- df %>%
  group_by(adm_0_name, ISO_A0, lat_band, od_region, Year) %>%
  summarise(
    total_cases = sum(dengue_total_scaled, na.rm = TRUE),
    total_pop = first(pop_est),
    incidence_per_100k = (total_cases / total_pop) * 100000
  )


region_cols <- c(
  "South America" = "#F6D49B", # light muted amber (less dominant)
  "North & Central America" = "#E28E49", # warm burnt orange (Americas family)
  "Caribbean" = "#F2B06D", # peachy/orange (related but distinct)
  "East & Southeast Asia" = "#2A6F9E", # desaturated deep blue/teal (strong but calm contrast)
  "South Asia" = "#A57DB8", # soft muted purple
  "Pacific Islands" = "#B58EA8", # muted mauve/pastel
  "Sub-Saharan Africa" = "#6AA84F", # mid green (intuitive)
  "Europe, Middle East & North Africa" = "#2A9D8F", # teal (cool, distinct from green)
  "Global" = "#BDBDBD" # neutral grey for "global"
)


# average incidence between 1990-2024
# annual %>%
#   mutate(decade = factor(floor(Year / 10) * 10, labels = c("1990s", "2000s", "2010s", "2020s"))) %>%
#   group_by(decade, adm_0_name) %>%
#   summarise(mean_incidence = mean(incidence_per_100k)) %>%
#   pivot_wider(id_cols = c("adm_0_name"), names_from = decade, values_from = mean_incidence) %>%
#   arrange(desc(`2020s`)) %>%
#   slice_head(n = 30)



calculate_aroc_full_period_poisson <- function(df, grouping_var = "od_region", min_years = 3) {
  results <- df %>%
    filter(
      !is.na(incidence_per_100k), !is.na(Year), !is.na(total_pop),
      !is.na(total_cases)
    ) %>%
    group_by(adm_0_name, ISO_A0, !!sym(grouping_var)) %>%
    # Count non-zero years
    mutate(years_with_cases = sum(total_cases > 0)) %>%
    # Require minimum years WITH CASES
    filter(years_with_cases >= min_years) %>%
    nest() %>%
    mutate(
      model_results = map(data, function(country_data) {
        tryCatch(
          {
            # POISSON REGRESSION with population offset
            model <- glm(total_cases ~ Year + offset(log(total_pop)),
              data = country_data,
              family = quasipoisson(link = "log")
            )

            if (!model$converged) {
              return(NULL)
            }

            coef_summary <- summary(model)$coefficients

            if (nrow(coef_summary) < 2) {
              return(NULL)
            }

            # Extract coefficient and SE
            year_coef <- coef_summary[2, 1]
            year_se <- coef_summary[2, 2]

            # CRITICAL: Exponentiate for Poisson!
            annual_rate <- (exp(year_coef) - 1) * 100
            ci_lower <- (exp(year_coef - 1.96 * year_se) - 1) * 100
            ci_upper <- (exp(year_coef + 1.96 * year_se) - 1) * 100

            # Additional metrics
            years_with_cases <- sum(country_data$total_cases > 0, na.rm = TRUE)

            first_case_year <- if (years_with_cases > 0) {
              min(country_data$Year[country_data$total_cases > 0], na.rm = TRUE)
            } else {
              NA_integer_
            }

            # Safe extraction
            get_year_value <- function(data, year) {
              val <- data$incidence_per_100k[data$Year == year]
              if (length(val) == 0) {
                return(NA_real_)
              }
              return(val[1])
            }

            incidence_1990 <- get_year_value(country_data, 1990)
            incidence_2024 <- get_year_value(country_data, 2024)

            data.frame(
              annual_rate_of_change = annual_rate,
              p_value = coef_summary[2, 4],
              ci_lower = ci_lower,
              ci_upper = ci_upper,

              # Context
              n_years = nrow(country_data),
              first_case_year = first_case_year,
              years_with_cases = years_with_cases,
              years_before_emergence = if (!is.na(first_case_year)) {
                first_case_year - 1990
              } else {
                NA_integer_
              },
              incidence_1990 = incidence_1990,
              incidence_2024 = incidence_2024
            )
          },
          error = function(e) {
            message("Error: ", e$message)
            return(NULL)
          }
        )
      })
    ) %>%
    unnest(model_results) %>%
    select(-data) %>%
    ungroup() %>%
    filter(!is.na(annual_rate_of_change)) %>%
    mutate(
      significant = p_value < 0.05,
      trend_direction = case_when(
        !significant ~ "No significant trend",
        annual_rate_of_change > 0 ~ "Increasing",
        annual_rate_of_change < 0 ~ "Decreasing"
      )
    ) %>%
    rename(group = !!sym(grouping_var))

  return(results)
}


results_pois <- calculate_aroc_full_period_poisson(annual, grouping_var = "od_region", min_years = 6) %>%
  add_od_regions()


results_pois %>%
  mutate(direction = case_when(
    annual_rate_of_change > 0 ~ "increasing",
    annual_rate_of_change < 0 ~ "decreasing",
    TRUE ~ NA
  )) %>%
  group_by(direction) %>%
  tally()

results_pois

# Calculate medians
medians <- results_pois %>%
  group_by(od_region) %>%
  summarise(median_rate = median(annual_rate_of_change, na.rm = TRUE))

# Regional standard deviation-based outliers
outlier_threshold <- 1.5

outliers <- results_pois %>%
  left_join(regional_stats, by = "od_region") %>%
  mutate(
    is_outlier = abs(annual_rate_of_change - median_growth) > 2 * sd_growth,
    is_outlier2 = abs(annual_rate_of_change - median_growth) > 1.5 * sd_growth,
    is_outlier3 = abs(annual_rate_of_change - median_growth) > 1 * sd_growth
  ) %>%
  mutate(
    labels = ifelse((od_region %in% c("South Asia", "Sub-Saharan Africa", "Europe, Middle East & North Africa") & is_outlier3), TRUE, FALSE),
    labels = ifelse((!od_region %in% c("South Asia", "Sub-Saharan Africa", "Europe, Middle East & North Africa") & is_outlier2), TRUE, labels)
  ) %>%
  filter(labels)

# Create the plot
results_pois <- results_pois %>%
  mutate(od_region = factor(od_region, levels = names(region_cols)))

p <- ggplot(results_pois, aes(x = od_region, y = annual_rate_of_change)) +
  # Add horizontal reference line at y = 0
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_beeswarm(
    aes(
      color = od_region
      # alpha = ifelse(p_value < alpha_threshold, 0.8, 0.4),
    ),
    size = 4.5,
    cex = 1, # Controls spacing between points
    method = "swarm",
    priority = "density" # Arranges by density for better spacing
  ) +
  # Add median segments with custom data
  geom_segment(
    data = medians,
    aes(
      x = as.numeric(factor(od_region)) - 0.1,
      xend = as.numeric(factor(od_region)) + 0.1,
      y = median_rate,
      yend = median_rate
    ),
    linewidth = 1,
    color = "black"
  ) +
  ggrepel::geom_text_repel(
    data = outliers,
    aes(label = adm_0_name),
    size = 5,
    color = "black",
    fontface = "bold",
    bg.color = "white",
    bg.r = 0.1,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.size = 0.3,
    max.overlaps = Inf
  ) +
  # Customize colors and shapes
  scale_color_manual(values = region_cols) +
  scale_alpha_identity() +
  scale_y_continuous(
    breaks = seq(-50, 100, by = 50),
    limits = c(-60, 100)
  ) +
  scale_x_discrete(labels = function(x) gsub(" & ", "\n& ", x)) +

  # Customize theme
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 18, face = "bold", vjust = 0),
    axis.title.y = element_text(size = 18, face = "plain", vjust = 2),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(30, 10, 20, 10)
  ) +

  # Labels
  labs(
    x = NULL,
    y = "Population-adjusted annual growth rate (%)"
  ) +

  # Remove color legend since it matches x-axis
  guides(color = "none")

print(p)

ggsave(
  plot = p,
  "output/figures/fig3b.png", bg = "white",
  width = 14, height = 8, dpi = 300
)


# Supp Fig 7: only for the period from first year with recorded cases to recet peak years


calculate_aroc_to_recent_peak_cagr <- function(df, grouping_var = "od_region",
                                               min_years_span = 3,
                                               peak_year_start = 2020) {
  results <- df %>%
    filter(
      !is.na(incidence_per_100k), !is.na(Year), !is.na(total_pop),
      !is.na(total_cases)
    ) %>%
    group_by(adm_0_name, ISO_A0, !!sym(grouping_var)) %>%
    # FILTER: Remove countries with NO cases between 2020-2024
    filter(sum(total_cases[Year >= peak_year_start] > 0) > 0) %>%
    # Find first case year and recent peak year (since 2020)
    mutate(
      first_case_year = min(Year[total_cases > 0], na.rm = TRUE),
      # Peak year = year with highest incidence since peak_year_start
      peak_year = {
        recent_years <- Year >= peak_year_start
        if (sum(recent_years) > 0) {
          Year[recent_years][which.max(incidence_per_100k[recent_years])]
        } else {
          NA_integer_
        }
      }
    ) %>%
    # Filter to countries with valid peak in recent period
    filter(
      !is.infinite(first_case_year),
      !is.na(peak_year),
      peak_year >= peak_year_start
    ) %>%
    # Calculate years span
    mutate(years_span = peak_year - first_case_year) %>%
    # Require minimum years span (not number of data points)
    filter(years_span >= min_years_span) %>%
    # Categorize by emergence period
    mutate(
      emergence_period = case_when(
        first_case_year < 2000 ~ "Long-established (pre-2000)",
        first_case_year < 2015 ~ "Established (2000-2014)",
        TRUE ~ "Recent (2015+)"
      ),
      emergence_period = factor(emergence_period,
        levels = c(
          "Long-established (pre-2000)",
          "Established (2000-2014)",
          "Recent (2015+)"
        )
      )
    ) %>%
    # Get data for first year and peak year only
    summarise(
      first_case_year = first(first_case_year),
      peak_year = first(peak_year),
      emergence_period = first(emergence_period),
      years_span = first(years_span),

      # First year data
      cases_first = total_cases[Year == first_case_year][1],
      pop_first = total_pop[Year == first_case_year][1],
      incidence_first = incidence_per_100k[Year == first_case_year][1],

      # Peak year data
      cases_peak = total_cases[Year == peak_year][1],
      pop_peak = total_pop[Year == peak_year][1],
      incidence_peak = incidence_per_100k[Year == peak_year][1],

      # Count years with cases in between (for context)
      years_with_cases = sum(total_cases[Year >= first_case_year & Year <= peak_year] > 0),
      .groups = "drop"
    ) %>%
    # Calculate growth rate between two points
    mutate(
      # Compound Annual Growth Rate (CAGR)
      # Using incidence: (peak/first)^(1/years) - 1
      annual_rate_of_change = if_else(
        incidence_first > 0 & incidence_peak > 0,
        ((incidence_peak / incidence_first)^(1 / years_span) - 1) * 100,
        NA_real_
      ),

      # Fold change
      fold_change = if_else(
        incidence_first > 0,
        incidence_peak / incidence_first,
        NA_real_
      ),

      # Years analyzed label
      years_analyzed = paste0(
        first_case_year, " → ", peak_year,
        " (", years_span, " yrs span, ", years_with_cases, " yrs with cases)"
      ),

      # For consistency with previous version
      peak_incidence = incidence_peak,
      significant = TRUE # No statistical test for two-point calculation
    ) %>%
    # Filter out invalid calculations
    filter(
      !is.na(annual_rate_of_change),
      !is.infinite(annual_rate_of_change)
    ) %>%
    mutate(
      trend_direction = case_when(
        annual_rate_of_change > 0 ~ "Increasing",
        annual_rate_of_change < 0 ~ "Decreasing",
        TRUE ~ "Stable"
      ),
      emergence_period = factor(emergence_period,
        levels = c(
          "Long-established (pre-2000)",
          "Established (2000-2014)",
          "Recent (2015+)"
        )
      )
    ) %>%
    rename(group = !!sym(grouping_var))

  return(results)
}


# Run analysis
results_to_recent_peak <- calculate_aroc_to_recent_peak_cagr(
  annual,
  grouping_var = "od_region",
  min_years = 3,
  peak_year_start = 2022
)

results_to_recent_peak <- add_od_regions(results_to_recent_peak) %>%
  mutate(od_region = factor(od_region, levels = names(region_cols)))


# Calculate medians
medians_peak <- results_to_recent_peak %>%
  group_by(od_region) %>%
  summarise(median_rate = median(annual_rate_of_change, na.rm = TRUE))


# Identify outliers
outliers_peak <- results_to_recent_peak %>%
  group_by(od_region) %>%
  mutate(
    median_rate = median(annual_rate_of_change),
    is_outlier = abs(annual_rate_of_change - median_rate) > 2 * sd(annual_rate_of_change),
    is_outlier2 = abs(annual_rate_of_change - median_rate) > 1.5 * sd(annual_rate_of_change),
    is_outlier3 = abs(annual_rate_of_change - median_rate) > 1 * sd(annual_rate_of_change)
  ) %>%
  mutate(
    labels = ifelse((emergence_period == "Recent (2015+)" & is_outlier3), TRUE, FALSE),
    labels = ifelse((emergence_period == "Established (2000-2014)" & is_outlier3 & annual_rate_of_change > median_rate), TRUE, labels),
    labels = ifelse((emergence_period == "Long-established (pre-2000)" & is_outlier), TRUE, labels)
  ) %>%
  filter(labels) %>%
  filter(!grepl("BONAIRE|CAMEROON", adm_0_name))


# Main plot

p_recent_peak <- ggplot(
  results_to_recent_peak,
  aes(x = od_region, y = annual_rate_of_change)
) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_beeswarm(
    aes(color = od_region),
    size = 4.5,
    cex = 1.2,
    method = "swarm",
    priority = "density"
  ) +
  # Add median segments with custom data
  geom_segment(
    data = medians_peak,
    aes(
      x = as.numeric(factor(od_region)) - 0.1,
      xend = as.numeric(factor(od_region)) + 0.1,
      y = median_rate,
      yend = median_rate
    ),
    linewidth = 1,
    color = "black"
  ) +
  geom_text_repel(
    data = outliers_peak,
    aes(label = paste0(adm_0_name, "\n(", first_case_year, "→", peak_year, ")")),
    size = 4,
    color = "black",
    fontface = "bold",
    bg.color = "white",
    bg.r = 0.1,
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.size = 0.3,
    max.overlaps = Inf
  ) +
  # Customize colors and shapes
  scale_color_manual(values = region_cols) +
  scale_alpha_identity() +
  scale_y_continuous(
    breaks = seq(-50, 200, by = 50),
    limits = c(-60, 200)
  ) +
  scale_x_discrete(labels = function(x) gsub(" & ", "\n& ", x)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15, face = "bold", vjust = 0),
    axis.title.y = element_text(size = 15, face = "bold", vjust = 2),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(
    # title = "Growth rate from emergence to recent peak",
    # subtitle = paste0(
    #   "n=", nrow(results_to_recent_peak),
    #   " countries with ≥3 years data and recent peak since 2022"
    # ),
    x = NULL,
    y = "Annual growth rate to recent peak (%)"
  ) +
  guides(color = "none")

print(p_recent_peak)



# # comparison with WHO 2030 target
# # by 2024 dengue burden should be 10.8% lower than the 2020 baseline if on track to meet the 2030 goal
# calculate_aroc_target <- function(df, grouping_var = "od_region", min_years = 3) {
#   results <- df %>%
#     filter(
#       !is.na(incidence_per_100k), !is.na(Year), !is.na(total_pop),
#       !is.na(total_cases)
#     ) %>%
#     group_by(adm_0_name, ISO_A0, !!sym(grouping_var)) %>%
#     # # Count non-zero years
#     # mutate(years_with_cases = sum(total_cases > 0)) %>%
#     # # Require minimum years WITH CASES
#     # filter(years_with_cases >= min_years) %>%
#     nest() %>%
#     mutate(
#       model_results = map(data, function(country_data) {
#         tryCatch(
#           {
#             # Safe extraction
#             get_year_value <- function(data, year) {
#               val <- data$incidence_per_100k[data$Year == year]
#               if (length(val) == 0) {
#                 return(NA_real_)
#               }
#               return(val[1])
#             }
#
#             incidence_2020 <- get_year_value(country_data, 2020)
#             incidence_2024 <- get_year_value(country_data, 2024)
#
#
#             data.frame(
#               annual_rate_of_change = if_else(
#                 incidence_2020 > 0 & incidence_2024 > 0,
#                 ((incidence_2024 / incidence_2020)^(1 / (2024 - 2020)) - 1) * 100,
#                 NA_real_
#               ),
#
#               # Context
#               n_years = nrow(country_data),
#               incidence_2020 = incidence_2020,
#               incidence_2024 = incidence_2024
#             )
#           },
#           error = function(e) {
#             message("Error: ", e$message)
#             return(NULL)
#           }
#         )
#       })
#     ) %>%
#     unnest(model_results) %>%
#     select(-data) %>%
#     ungroup() %>%
#     # filter(!is.na(annual_rate_of_change)) %>%
#     rename(group = !!sym(grouping_var))
#
#   return(results)
# }
#
#
# results_target <- calculate_aroc_target(annual, grouping_var = "od_region", min_years = 1) %>%
#   add_od_regions()
#
# results_target %>%
#   filter(annual_rate_of_change < -10) %>%
#   arrange(annual_rate_of_change) %>%
#   print(n = 25)
#
# # Calculate medians
# results_target %>%
#   group_by(od_region) %>%
#   summarise(median_rate = median(annual_rate_of_change, na.rm = TRUE))
#
# results_target %>%
#   filter(annual_rate_of_change > 0)
