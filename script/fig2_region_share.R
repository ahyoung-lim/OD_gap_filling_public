# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)
library(ggtext)
source("functions/fn_OD_region.R") # regional classification

# od gap filled
df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_with_ci_V3.csv"))
sum(df$dengue_total_scaled)

df <- df %>%
  group_by(adm_0_name, Year) %>%
  mutate(
    annual_total = sum(dengue_total_scaled, na.rm = T),
    missing = case_when(
      imputed_weekly | imputed_monthly | disaggregated_yearly ~ TRUE,
      TRUE ~ FALSE
    ),
    calendar_date = make_date(Year, month, 1)
  )


df_region <- df %>%
  group_by(Year, od_region) %>%
  summarise(cases = sum(dengue_total_scaled)) %>%
  transmute(Year,
    od_region,
    cases,
    source = "Gap-filled Estimates"
  )

df_global <- df %>%
  group_by(Year) %>%
  summarise(cases = sum(dengue_total_scaled)) %>%
  transmute(Year,
    od_region = "Global",
    cases,
    source = "Gap-filled Estimates"
  )

df_summary <- bind_rows(df_region, df_global)


# Source shared WHO data loading/processing (produces de, who_db, who_combined)
source("script/04_consistency_analysis.R")

# Re-aggregate by OD regions (consistency_analysis uses WHO regions)
de_region <- de %>%
  add_od_regions() %>%
  group_by(Year, od_region) %>%
  summarise(cases = sum(dengue_total)) %>%
  transmute(Year,
    od_region,
    cases,
    source = "WHO Dengue Explorer"
  )

de_global <- de %>%
  group_by(Year) %>%
  summarise(cases = sum(dengue_total)) %>%
  transmute(Year,
    od_region = "Global",
    cases,
    source = "WHO Dengue Explorer"
  )

de_summary <- bind_rows(de_region, de_global)

who_region <- who_db %>%
  filter(Year < 2025) %>%
  add_od_regions(iso_col = "ISO_A0") %>%
  group_by(Year, od_region) %>%
  summarise(cases = sum(cases, na.rm = T))

who_global <- who_db %>%
  filter(Year < 2025) %>%
  group_by(Year) %>%
  summarise(cases = sum(cases, na.rm = T)) %>%
  mutate(od_region = "Global")

who_db_combined <- bind_rows(who_region, who_global) %>%
  mutate(source = "WHO Dashboard")

who_combined <- bind_rows(de_summary, who_db_combined)


# Combine data for plotting
combined_data <- bind_rows(who_combined, df_summary)

combined_data <- combined_data %>%
  filter(od_region != "Global") %>%
  mutate(source2 = case_when(
    grepl("WHO", source) ~ "WHO",
    TRUE ~ source
  )) %>%
  mutate(
    od_region = factor(od_region, levels = c(
      # Americas (red → orange → yellow)
      "South America",
      "North & Central America",
      "Caribbean",
      # Asia-Pacific (yellow-green → green → cyan → blue)
      "East & Southeast Asia",
      "South Asia",
      "Pacific Islands",
      # Africa-Europe (dark blue → purple)
      "Sub-Saharan Africa",
      "Europe, Middle East & North Africa"
    ))
  )

combined_data_filtered <- combined_data %>%
  arrange(Year, od_region, source2) %>%
  group_by(Year, od_region, source2) %>%
  slice_max(cases, n = 1)




region_cols <- c(
  "South America" = "#F6D49B", # light muted amber (less dominant)
  "North & Central America" = "#E28E49", # warm burnt orange (Americas family)
  "Caribbean" = "#F2B06D", # peachy/orange (related but distinct)
  "East & Southeast Asia" = "#2A6F9E", # desaturated deep blue/teal (strong but calm contrast)
  "South Asia" = "#A57DB8", # soft muted purple
  "Pacific Islands" = "#B58EA8", # muted mauve/pastel
  "Sub-Saharan Africa" = "#6AA84F", # mid green (intuitive)
  "Europe, Middle East & North Africa" = "#2A9D8F" # teal (cool, distinct from green)
  # "Global"                             = "#BDBDBD" # neutral grey for "global"
)



composition_data <- combined_data_filtered %>%
  filter(od_region != "Global") %>%
  droplevels() %>%
  mutate(source2 = case_when(
    source2 == "Gap-filled Estimates" ~ "gap_filled",
    TRUE ~ "who"
  )) %>%
  group_by(Year, source2) %>%
  mutate(
    total = sum(cases),
    percentage = cases / total * 100
  ) %>%
  ungroup()

composition_data <- composition_data %>%
  mutate(source2 = case_when(
    source2 == "who" ~ "WHO dengue databases",
    TRUE ~ "Gap-filled estimates"
  )) %>%
  mutate(source2 = factor(source2, levels = c("WHO dengue databases", "Gap-filled estimates")))


p <- composition_data %>%
  # filter(Year > 1999) %>%
  ggplot(aes(x = Year, y = percentage, fill = od_region)) +
  geom_area(color = "grey80", position = "fill", alpha = 0.8) +
  facet_wrap(~source2, ncol = 1, scales = "free_x") +
  scale_fill_manual(
    values = region_cols, na.value = "grey90", drop = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2024, 2),
    expand = c(0, 0.2)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), name = "Percentage of global totals") +
  labs(
    # title = "Shifting burden: How gap-filling changes the global dengue map",
    x = "Year",
    y = "Percentage of global totals",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 18, face = "plain",
      hjust = 0, margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 14, color = "#333333",
      hjust = 0, margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 9, color = "#666666",
      hjust = 0, margin = margin(t = 10)
    ),

    # Clean axes
    axis.text = element_text(size = 14, color = "#333333"),
    axis.title = element_text(size = 20, color = "#333333"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),

    # strip
    strip.text = element_text(size = 18, face = "bold", color = "#333333"),

    # Minimal grid
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"), # Increase spacing

    # legend
    legend.position = "bottom",
    legend.text = element_text(size = 11, color = "#333333"),
    legend.spacing.y = unit(0.2, "cm"),

    # Clean background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15)
  )


print(p)


p_v2 <- composition_data %>%
  filter(source2 == "Gap-filled estimates") %>%
  ggplot(aes(x = Year, y = percentage, fill = od_region)) +
  geom_area(color = "grey80", position = "fill", alpha = 0.8) +
  scale_fill_manual(
    values = region_cols, na.value = "grey90", drop = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2024, 2),
    expand = c(0, 0.2)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), name = "Percentage of global totals") +
  labs(
    # title = "Shifting burden: How gap-filling changes the global dengue map",
    x = "Year",
    y = "Percentage of global totals",
    fill = NULL
  ) +
  theme_minimal() +
  theme(

    # Clean axes
    axis.text = element_text(size = 14, color = "#333333"),
    axis.title = element_text(size = 16, color = "#333333"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),

    # strip
    # strip.text = element_text(size = 18, face = "bold", color = "#333333"),

    # Minimal grid
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"), # Increase spacing

    # legend
    legend.position = "bottom",
    legend.text = element_text(size = 11, color = "#333333"),
    legend.spacing.y = unit(0.2, "cm"),

    # Clean background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15)
  )

ggsave(
  plot = p_v2, "output/figures/fig2.png",
  width = 12,
  height = 6, dpi = 300
)

# Supp Fig 6
p_v3 <- composition_data %>%
  filter(source2 != "Gap-filled estimates") %>%
  ggplot(aes(x = Year, y = percentage, fill = od_region)) +
  geom_area(color = "grey80", position = "fill", alpha = 0.8) +
  scale_fill_manual(
    values = region_cols, na.value = "grey90", drop = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2024, 2),
    expand = c(0, 0.2)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), name = "Percentage of global totals") +
  labs(
    # title = "Shifting burden: How gap-filling changes the global dengue map",
    x = "Year",
    y = "Percentage of global totals",
    fill = NULL
  ) +
  theme_minimal() +
  theme(

    # Clean axes
    axis.text = element_text(size = 14, color = "#333333"),
    axis.title = element_text(size = 16, color = "#333333"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),

    # strip
    # strip.text = element_text(size = 18, face = "bold", color = "#333333"),

    # Minimal grid
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"), # Increase spacing

    # legend
    legend.position = "bottom",
    legend.text = element_text(size = 11, color = "#333333"),
    legend.spacing.y = unit(0.2, "cm"),

    # Clean background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15)
  )

ggsave(
  plot = p_v3, "output/figures/sfig6.png",
  width = 12,
  height = 6, dpi = 300
)





# Calculate regional shares by decade ------------
regional_shares <- composition_data %>%
  mutate(decade = case_when(
    Year >= 1990 & Year < 2000 ~ "1990s",
    Year >= 2000 & Year < 2010 ~ "2000s",
    Year >= 2010 & Year < 2020 ~ "2010s",
    Year >= 2020 ~ "2020s",
    TRUE ~ NA_character_
  )) %>%
  group_by(decade, od_region, source2) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  group_by(decade, source2) %>%
  mutate(
    share = (total_cases / sum(total_cases)) * 100,
    decade_total = sum(total_cases)
  ) %>%
  ungroup()

# Calculate change in shares from 1990s to 2020s
share_change_od <- regional_shares %>%
  filter(decade %in% c("1990s", "2000s", "2020s") & source2 == "Gap-filled estimates") %>%
  select(decade, od_region, share) %>%
  pivot_wider(names_from = decade, values_from = share, names_prefix = "share_") %>%
  mutate(
    change = share_2020s - share_2000s,
    relative_change = (share_2020s / share_2000s - 1) * 100
  ) %>%
  arrange(desc(abs(change)))

# Calculate change in shares from 1990s to 2020s
share_change_who <- regional_shares %>%
  filter(decade %in% c("1990s", "2000s", "2020s") & source2 == "WHO dengue databases") %>%
  select(decade, od_region, share) %>%
  pivot_wider(names_from = decade, values_from = share, names_prefix = "share_") %>%
  mutate(
    change = share_2020s - share_2000s,
    relative_change = (share_2020s / share_2000s - 1) * 100
  ) %>%
  arrange(desc(abs(change)))

print(share_change_od)
print(share_change_who)


# =========================================================#
# annual growth rates using WHO digitised databases only
who_digital_annual <- composition_data %>%
  filter(source2 == "WHO dengue databases") %>%
  group_by(Year) %>%
  summarise(total = sum(cases))


# Prepare the data
global_pop <- df %>%
  group_by(Year) %>%
  summarise(
    pop = sum(pop_est, na.rm = TRUE),
    .groups = "drop"
  )

who_digital_annual <- merge(who_digital_annual, global_pop, by = "Year", all = TRUE)


# Fit population-adjusted Poisson regression model
poisson_model <- glm(total ~ Year + offset(log(pop)),
  data = who_digital_annual,
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


# ==========================================================
# cumulative cases by OD region: gap filled vs who data

# cumulative_data <- combined_data_filtered %>%
#   mutate(source2 = case_when(
#     source2 == "Gap-filled Estimates" ~ "Gap_filled",
#     TRUE ~ source2
#   )) %>%
#   pivot_wider(
#     id_cols = c("Year", "od_region"), names_from = "source2",
#     values_from = cases
#   ) %>%
#   mutate(WHO = case_when(is.na(WHO) ~ 0, TRUE ~ as.integer(WHO))) %>%
#   arrange(Year) %>%
#   group_by(od_region) %>%
#   mutate(
#     # Calculate cumulative sums
#     gap_filled_cumulative = cumsum(Gap_filled),
#     who_cumulative = cumsum(WHO),
#
#     # Calculate the hidden burden (cumulative difference)
#     hidden_burden = gap_filled_cumulative - who_cumulative,
#
#     # Calculate percentage hidden at each point
#     percent_hidden = (hidden_burden / gap_filled_cumulative) * 100,
#
#     # Annual hidden burden (not cumulative)
#     annual_hidden = Gap_filled - WHO
#   )
#
#
#
#
# regions <- levels(cumulative_data$od_region)
# p_list <- list()
#
# for (r in regions) {
#   region_cum_data <- cumulative_data[cumulative_data$od_region == r, ]
#
#   region_title_data <- region_cum_data %>%
#     filter(Year == 2024) %>%
#     mutate(
#       diff = (gap_filled_cumulative - who_cumulative) / 1e+6,
#       diff_pct = (gap_filled_cumulative - who_cumulative) / gap_filled_cumulative * 100
#     ) %>%
#     select(diff, diff_pct)
#
#   p <- ggplot(region_cum_data) +
#     # Subtle background grid
#     geom_vline(
#       xintercept = seq(1990, 2024, 5),
#       color = "grey96", size = 0.3
#     ) +
#     # geom_hline(
#     #   yintercept = seq(0, 70, 10),
#     #   color = "grey96", size = 0.3
#     # ) +
#
#
#
#     # # The hidden burden - using gradient for depth without being flashy
#     geom_ribbon(aes(
#       x = Year,
#       ymin = who_cumulative / 1e6,
#       ymax = gap_filled_cumulative / 1e6,
#       fill = "Surveillance gap"
#     ), alpha = 0.25) +
#
#     # Subtle area under WHO line
#     geom_area(aes(x = Year, y = who_cumulative / 1e6),
#       fill = "#4575b4", alpha = 0.1
#     ) +
#     geom_line(
#       aes(
#         x = Year, y = gap_filled_cumulative / 1e6,
#         color = "Gap-filled estimates"
#       ),
#       size = 1.2
#     ) +
#     geom_line(
#       aes(
#         x = Year, y = who_cumulative / 1e6,
#         color = "WHO reports"
#       ),
#       linetype = "dashed",
#       size = 1.2
#     ) +
#
#     # Professional color schemes
#     scale_color_manual(
#       values = c(
#         "Gap-filled estimates" = "#2c2c2c",
#         "WHO reports" = "#4575b4"
#       ),
#       name = NULL
#     ) +
#     scale_fill_manual(
#       values = c("Surveillance gap" = "#d73027"),
#       name = NULL
#     ) +
#
#     # Clean, academic scales
#     scale_y_continuous(
#       # breaks = seq(0, 80, 10),
#       labels = function(x) format(x, big.mark = ","),
#       # limits = c(0, 80),
#       expand = c(0.01, 0.01)
#     ) +
#     scale_x_continuous(
#       breaks = seq(1990, 2024, 5),
#       limits = c(1990, 2024),
#       expand = c(0.01, 0.01)
#     ) +
#     labs(
#       title = region_cum_data$od_region[1],
#       subtitle = paste0(
#         format(region_title_data$diff, nsmall = 1, digits = 1, , trim = TRUE),
#         " million cases, ",
#         format(region_title_data$diff_pct, nsmall = 1, digits = 2, trim = TRUE),
#         "% of total burden"
#       ),
#       x = "Year",
#       y = "Cumulative reported dengue cases (millions)",
#     ) +
#     theme_bw() +
#     theme(
#       # Professional typography
#       plot.title = element_text(
#         size = 18, face = "plain",
#         hjust = 0, margin = margin(b = 3)
#       ),
#       plot.subtitle = element_text(
#         size = 14, color = "#333333",
#         hjust = 0, margin = margin(b = 15)
#       ),
#       plot.caption = element_text(
#         size = 9, color = "#666666",
#         hjust = 0, margin = margin(t = 10)
#       ),
#
#       # Clean axes
#       axis.text = element_text(size = 8, color = "#333333"),
#       axis.title = element_text(size = 8, color = "#333333"),
#       axis.title.x = element_text(margin = margin(t = 8)),
#       axis.title.y = element_text(margin = margin(r = 8)),
#
#       # Minimal grid
#       panel.grid.major = element_line(color = "grey94", size = 0.3),
#       panel.grid.minor = element_blank(),
#       panel.border = element_rect(color = "grey80", size = 0.5),
#
#       # legend
#       legend.position = "bottom",
#       legend.text = element_text(size = 14, color = "#333333"),
#       legend.spacing.y = unit(0.2, "cm"),
#
#       # Clean background
#       plot.background = element_rect(fill = "white", color = NA),
#       panel.background = element_rect(fill = "white", color = NA),
#       plot.margin = margin(15, 20, 15, 15)
#     )
#   p_list[[r]] <- p
# }
#
# wrap_plots(p_list, guides = "collect") &
#   theme(legend.position = "bottom")
