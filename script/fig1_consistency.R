# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)
library(ggtext)
source("functions/fn_OD_region.R") # regional classification

# od gap filled
df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_with_ci_V3.csv"))

df <- df %>%
  group_by(adm_0_name, Year) %>%
  mutate(
    annual_total = sum(dengue_total_scaled, na.rm = T),
    missing = case_when(
      imputed_weekly | imputed_monthly | disaggregated_yearly ~ TRUE,
      TRUE ~ FALSE
    ),
    calendar_date = make_date(Year, month, 1)
  ) %>%
  filter(adm_0_name != "JAPAN") %>% # exclude for a fair comparison
  region_class()

unique(df$adm_0_name)
unique(df$region)


df_region <- df %>%
  group_by(Year, region) %>%
  summarise(
    cases_all = sum(dengue_total_scaled),
    cases_no_modelling = sum(dengue_total_scaled[!imputed_weekly & !imputed_monthly])
  ) %>%
  pivot_longer(
    cols = c(cases_all, cases_no_modelling),
    names_to = "source",
    values_to = "cases"
  ) %>%
  transmute(Year,
    who_region = region,
    cases,
    source = case_when(
      source == "cases_all" ~ "Gap-filled Estimates",
      TRUE ~ "Gap-filled without modelling"
    )
  )

df_global <- df %>%
  group_by(Year) %>%
  summarise(
    cases_all = sum(dengue_total_scaled),
    cases_no_modelling = sum(dengue_total_scaled[!imputed_weekly & !imputed_monthly])
  ) %>%
  pivot_longer(
    cols = c(cases_all, cases_no_modelling),
    names_to = "source",
    values_to = "cases"
  ) %>%
  transmute(Year,
    who_region = "Global",
    cases,
    source = case_when(
      source == "cases_all" ~ "Gap-filled Estimates",
      TRUE ~ "Gap-filled without modelling"
    )
  )

df_summary <- bind_rows(df_region, df_global)

# load in WHO data -----------------------------------------
source("script/04_consistency_analysis.R")
# who_combined: WHO all sources (dashboard, dengue explorer, and regional summaries)
# who_db: data from WHO global and SEARO dengue dashboards


# Combine OD gap filled and WHO data for plotting ----------
# Combine data for plotting
combined_data <- bind_rows(
  who_combined,
  df_summary
) %>%
  arrange(Year, who_region, source)

combined_data <- combined_data %>%
  mutate(source2 = case_when(
    grepl("WHO", source) ~ "WHO",
    TRUE ~ source
  ))

combined_data %>%
  group_by(Year, who_region, source2) %>%
  filter(n() > 1)

comparison_df <- combined_data %>%
  group_by(Year, who_region) %>%
  summarize(
    gap_filled = max(cases[source == "Gap-filled Estimates"], na.rm = TRUE),
    gap_filled_no_modelling = max(cases[source == "Gap-filled without modelling"], na.rm = TRUE),
    WHO_all = ifelse(any(source2 == "WHO"),
      max(cases[source2 == "WHO"], na.rm = TRUE),
      NA_real_
    ),
    WHO_db = ifelse(any(source != "WHO Reports" & source2 == "WHO"), # digitised dashboards only
      max(cases[source != "WHO Reports" & source2 == "WHO"], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

comparison_df <- comparison_df %>%
  arrange(Year) %>%
  group_by(who_region) %>%
  mutate(
    # Calculate cumulative sums
    gap_filled_cumulative = cumsum(gap_filled),
    gap_filled_no_modelling_cumulative = cumsum(gap_filled_no_modelling),
    who_all_cumulative = cumsum(replace_na(WHO_all, 0)),
    who_db_cumulative = cumsum(replace_na(WHO_db, 0)),

    # Calculate the hidden burden (cumulative difference)
    hidden_burden_top = gap_filled_cumulative - who_all_cumulative,
    hidden_burden_bottom = who_all_cumulative - who_db_cumulative,

    # Calculate percentage hidden at each point
    percent_hidden_top = (hidden_burden_top / gap_filled_cumulative) * 100,
    percent_hidden_bottom = (hidden_burden_bottom / gap_filled_cumulative) * 100
  ) %>%
  mutate(who_region = case_when(
    who_region == "SEARO" ~ "WHO South-East Asia Region",
    who_region == "WPRO" ~ "WHO Western Pacific Region",
    who_region == "AFRO" ~ "WHO African Region",
    who_region == "EMRO" ~ "WHO Eastern Mediterranean Region",
    who_region == "PAHO" ~ "WHO Americas Region",
    who_region == "EURO" ~ "WHO European Region",
    TRUE ~ "Global"
  ))

# checking
comparison_df %>%
  filter(gap_filled < WHO_all) %>%
  print(n = 25)


regions <- unique(comparison_df$who_region)
p_list <- list()

for (r in regions) {
  region_cum_data <- comparison_df[comparison_df$who_region == r, ]

  region_title_data <- region_cum_data %>%
    filter(Year == max(Year)) %>%
    mutate(
      diff_top = (gap_filled_cumulative - who_all_cumulative) / 1e+6,
      diff_pct_top = (gap_filled_cumulative - who_all_cumulative) / gap_filled_cumulative * 100,
      diff_bottom = (who_all_cumulative - who_db_cumulative) / 1e+6,
      diff_pct_bottom = (who_all_cumulative - who_db_cumulative) / gap_filled_cumulative * 100
    ) %>%
    select(diff_top, diff_pct_top, diff_bottom, diff_pct_bottom)

  p <- ggplot(region_cum_data) +
    geom_vline(
      xintercept = seq(1990, 2024, 5),
      color = "grey96", linewidth = 0.3
    ) +

    # Layer 2: WHO all sources → Gap-filled (darker)
    geom_ribbon(aes(
      x = Year,
      ymin = who_all_cumulative / 1e6,
      ymax = gap_filled_cumulative / 1e6
    ), fill = "#d73027", alpha = 0.6, show.legend = FALSE) +

    # Layer 1: WHO database → WHO all sources (lighter)
    geom_ribbon(aes(
      x = Year,
      ymin = who_db_cumulative / 1e6,
      ymax = who_all_cumulative / 1e6
    ), fill = "#d73027", alpha = 0.3, show.legend = FALSE) +

    # Lines - keep in normal legend
    geom_line(aes(
      x = Year,
      y = gap_filled_cumulative / 1e6,
      color = "OpenDengue gap-filled",
      linetype = "OpenDengue gap-filled"
    ), linewidth = 1.3) +
    geom_line(aes(
      x = Year,
      y = who_all_cumulative / 1e6,
      color = "WHO all sources",
      linetype = "WHO all sources"
    ), linewidth = 1.1) +
    geom_line(aes(
      x = Year,
      y = who_db_cumulative / 1e6,
      color = "WHO public databases",
      linetype = "WHO public databases"
    ), linewidth = 1.1) +
    geom_area(aes(x = Year, y = who_db_cumulative / 1e6), fill = "#4575b4", alpha = 0.1) +

    # Line color scale
    scale_color_manual(
      values = c(
        "OpenDengue gap-filled" = "#000000",
        "WHO all sources" = "#4575b4",
        "WHO public databases" = "#4575b4"
      ),
      name = NULL
    ) +

    # Line type scale
    scale_linetype_manual(
      values = c(
        "OpenDengue gap-filled" = "solid",
        "WHO all sources" = "dashed",
        "WHO public databases" = "dotted"
      ),
      name = NULL
    ) +
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ","),
      expand = c(0.01, 0.01)
    ) +
    coord_cartesian(ylim = c(0.01, NA)) +
    scale_x_continuous(
      breaks = c(seq(1990, 2020, 5), 2024),
      limits = c(1990, 2024),
      expand = c(0.01, 0.01)
    ) +
    labs(
      title = region_cum_data$who_region[1],
      subtitle = NULL,
      x = "Year",
      y = "Cumulative reported dengue cases (millions)"
    ) +

    # Box 2 - Darker color
    annotate("rect",
      xmin = 1992, xmax = 1993,
      ymin = max(region_cum_data$gap_filled_cumulative / 1e6) * (0.75 - 0.015),
      ymax = max(region_cum_data$gap_filled_cumulative / 1e6) * (0.75 + 0.015),
      fill = "#d73027", alpha = 0.6, color = "grey40", linewidth = 0.3
    ) +
    annotate("text",
      x = 1993.5,
      y = max(region_cum_data$gap_filled_cumulative / 1e6) * 0.75,
      label =
        sprintf(
          "Gap-filling beyond all WHO sources\n(+%.1fM, %.1f%% of total estimated cases)",
          region_title_data$diff_top,
          region_title_data$diff_pct_top
        ),
      hjust = 0, vjust = 0.5,
      size = 5, color = "#333333", lineheight = 1.1
    ) +

    # INSIDE PANEL: Ribbon fill legend with colored boxes
    # Box 1 - Lighter color
    annotate("rect",
      xmin = 1992, xmax = 1993,
      ymin = max(region_cum_data$gap_filled_cumulative / 1e6) * (0.58 - 0.015),
      ymax = max(region_cum_data$gap_filled_cumulative / 1e6) * (0.58 + 0.015),
      fill = "#d73027", alpha = 0.3, color = "grey40", linewidth = 0.3
    ) +
    annotate("text",
      x = 1993.5,
      y = max(region_cum_data$gap_filled_cumulative / 1e6) * 0.58,
      label = sprintf(
        "WHO reports beyond databases\n(+%.1fM, %.1f%% of total estimated cases)",
        region_title_data$diff_bottom,
        region_title_data$diff_pct_bottom
      ),
      hjust = 0, vjust = 0.5,
      size = 5, color = "#333333", lineheight = 1.1
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 3)),
      axis.text = element_text(size = 12, color = "#333333"),
      axis.title = element_text(size = 14, color = "#333333"),
      panel.grid.major = element_line(color = "grey94", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", linewidth = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 16, color = "#333333"),
      legend.spacing.y = unit(0.25, "cm"),
      legend.key.width = unit(1.5, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 15, 10, 10)
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 1, override.aes = list(linewidth = 1)),
      linetype = guide_legend(order = 1, nrow = 1, override.aes = list(linewidth = 1))
    )


  p_list[[r]] <- p
}



fig1b <- wrap_plots(p_list[c(6, 7, 2, 1)]) &
  theme(
    legend.position = "none",
    legend.spacing.y = unit(0.25, "cm"),
  )

print(fig1b)

ggsave(
  plot = fig1b, "output/figures/fig1b.png",
  width = 18 * 0.8, height = 12 * 0.8, dpi = 300
)

# Supp Fig 5
sfig5 <- wrap_plots(p_list[c(5, 3)]) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.spacing.y = unit(0.25, "cm"),
  )

print(sfig5)
ggsave(
  plot = sfig5, "output/figures/sfig5.png",
  width = 15, height = 6, dpi = 300
)

# global

global_title_data <- comparison_df %>%
  ungroup() %>%
  filter(Year == max(Year) & who_region == "Global") %>%
  mutate(
    diff_modelling = (gap_filled_cumulative - gap_filled_no_modelling_cumulative) / 1e+6,
    diff_top = (gap_filled_cumulative - who_all_cumulative) / 1e+6,
    diff_pct_top = (gap_filled_cumulative - who_all_cumulative) / gap_filled_cumulative * 100,
    diff_bottom = (who_all_cumulative - who_db_cumulative) / 1e+6,
    diff_pct_bottom = (who_all_cumulative - who_db_cumulative) / gap_filled_cumulative * 100
  ) %>%
  select(diff_modelling, diff_top, diff_pct_top, diff_bottom, diff_pct_bottom)

global_cum_data <- comparison_df %>%
  filter(who_region == "Global")

round(global_title_data$diff_top + global_title_data$diff_bottom, 1)
round(global_title_data$diff_pct_top + global_title_data$diff_pct_bottom, 1)

global_title_data$diff_modelling # number of cases added by modelling


p_global <- ggplot(global_cum_data) +
  geom_vline(
    xintercept = seq(1990, 2024, 5),
    color = "grey96", size = 0.3
  ) +
  geom_hline(
    yintercept = seq(0, 70, 10),
    color = "grey96", size = 0.3
  ) +

  # Layer 2: WHO all sources → Gap-filled (darker)
  geom_ribbon(aes(
    x = Year,
    ymin = who_all_cumulative / 1e6,
    ymax = gap_filled_cumulative / 1e6
  ), fill = "#d73027", alpha = 0.6, show.legend = FALSE) +

  # Layer 1: WHO database → WHO all sources (lighter)
  geom_ribbon(aes(
    x = Year,
    ymin = who_db_cumulative / 1e6,
    ymax = who_all_cumulative / 1e6
  ), fill = "#d73027", alpha = 0.3, show.legend = FALSE) +

  # Lines - keep in normal legend
  geom_line(aes(
    x = Year,
    y = gap_filled_cumulative / 1e6,
    color = "OpenDengue gap-filled",
    linetype = "OpenDengue gap-filled"
  ), linewidth = 1.3) +
  geom_line(aes(
    x = Year,
    y = who_all_cumulative / 1e6,
    color = "WHO all sources",
    linetype = "WHO all sources"
  ), linewidth = 1.1) +
  geom_line(aes(
    x = Year,
    y = who_db_cumulative / 1e6,
    color = "WHO public databases",
    linetype = "WHO public databases"
  ), linewidth = 1.1) +
  geom_area(aes(x = Year, y = who_db_cumulative / 1e6), fill = "#4575b4", alpha = 0.1) +

  # Line color scale
  scale_color_manual(
    values = c(
      "OpenDengue gap-filled" = "#000000",
      "WHO all sources" = "#4575b4",
      "WHO public databases" = "#4575b4"
    ),
    name = NULL
  ) +

  # Line type scale
  scale_linetype_manual(
    values = c(
      "OpenDengue gap-filled" = "solid",
      "WHO all sources" = "dashed",
      "WHO public databases" = "dotted"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ","),
    expand = c(0.01, 0.01)
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, 2020, 5), 2024),
    limits = c(1990, 2024),
    expand = c(0.01, 0.01)
  ) +

  # Box 2 - Darker color
  annotate("rect",
    xmin = 2000, xmax = 2001,
    ymin = max(global_cum_data$gap_filled_cumulative / 1e6) * (0.75 - 0.015),
    ymax = max(global_cum_data$gap_filled_cumulative / 1e6) * (0.75 + 0.015),
    fill = "#d73027", alpha = 0.6, color = "grey40", linewidth = 0.3
  ) +
  annotate("text",
    x = 2001.5,
    y = max(global_cum_data$gap_filled_cumulative / 1e6) * 0.75,
    label =
      sprintf(
        "Gap-filling beyond all WHO sources\n(+%.1fM, %.1f%% of total estimated cases)",
        global_title_data$diff_top,
        global_title_data$diff_pct_top
      ),
    hjust = 0, vjust = 0.5,
    size = 5, color = "#333333", lineheight = 1.1
  ) +

  # INSIDE PANEL: Ribbon fill legend with colored boxes
  # Box 1 - Lighter color
  annotate("rect",
    xmin = 2000, xmax = 2001,
    ymin = max(global_cum_data$gap_filled_cumulative / 1e6) * (0.63 - 0.015),
    ymax = max(global_cum_data$gap_filled_cumulative / 1e6) * (0.63 + 0.015),
    fill = "#d73027", alpha = 0.3, color = "grey40", linewidth = 0.3
  ) +
  annotate("text",
    x = 2001.5,
    y = max(global_cum_data$gap_filled_cumulative / 1e6) * 0.63,
    label = sprintf(
      "WHO reports beyond databases\n(+%.1fM, %.1f%% of total estimated cases)",
      global_title_data$diff_bottom,
      global_title_data$diff_pct_bottom
    ),
    hjust = 0, vjust = 0.5,
    size = 5, color = "#333333", lineheight = 1.1
  ) +
  labs(
    title = "Global",
    # subtitle = "Shaded area represents consistency gap",
    x = "Year",
    y = "Cumulative reported dengue cases (millions)",
  ) +
  theme_bw() +
  theme(
    # Professional typography
    plot.title = element_text(
      size = 20, face = "bold",
      hjust = 0, margin = margin(b = 3)
    ),

    # Clean axes
    axis.text = element_text(size = 18, color = "#333333"),
    axis.title = element_text(size = 20, color = "#333333"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),

    # Minimal grid
    panel.grid.major = element_line(color = "grey94", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", size = 0.5),

    # legend
    legend.position = "top",
    legend.text = element_text(size = 20, color = "#333333"),
    legend.spacing.y = unit(0.2, "cm"),

    # Clean background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15)
  )

print(p_global)

leg <- get_legend(
  p + theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )
)

p_noleg <- p_global + theme(legend.position = "none")

p_global_new <- plot_grid(leg, p_noleg, ncol = 1, rel_heights = c(0.1, 1))

ggsave("output/figures/fig1a.png", p_global_new,
  width = 12, height = 8, dpi = 300, bg = "white"
)



# country-level comparison:

who_country <- who_db %>%
  filter(Year < 2025) %>%
  mutate(
    who_region = case_when(
      who_region == "AMR" ~ "PAHO",
      who_region == "SEAR" ~ "SEARO",
      who_region == "AFR" ~ "AFRO",
      who_region == "EMR" ~ "EMRO",
      who_region == "EUR" ~ "EURO",
      who_region == "WPR" ~ "WPRO",
      TRUE ~ NA
    ),
    source = "WHO DB"
  )

df_country <- df %>%
  group_by(Year, region, ISO_A0) %>%
  summarise(
    cases_all = sum(dengue_total_scaled),
    cases_no_modelling = sum(dengue_total_scaled[!imputed_weekly & !imputed_monthly])
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(cases_all, cases_no_modelling),
    names_to = "source",
    values_to = "cases"
  ) %>%
  transmute(Year,
    ISO_A0,
    who_region = region,
    cases,
    source = case_when(
      source == "cases_all" ~ "Gap-filled Estimates",
      TRUE ~ "Gap-filled without modelling"
    )
  )

country_comparison <- rbind(
  df_country %>% select(Year, ISO_A0, who_region, cases, source),
  who_country %>% select(Year, ISO_A0, who_region, cases, source)
)

country_comparison <- country_comparison %>%
  group_by(Year, who_region, ISO_A0) %>%
  summarize(
    gap_filled = max(cases[source == "Gap-filled Estimates"], na.rm = TRUE),
    gap_filled_no_modelling = max(cases[source == "Gap-filled without modelling"], na.rm = TRUE),
    WHO_db = ifelse(any(source == "WHO DB"),
      max(cases[source == "WHO DB"], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

country_comparison <- country_comparison %>%
  arrange(Year) %>%
  group_by(who_region, ISO_A0) %>%
  mutate(
    # Calculate cumulative sums
    gap_filled_cumulative = cumsum(gap_filled),
    gap_filled_no_modelling_cumulative = cumsum(gap_filled_no_modelling),
    who_db_cumulative = cumsum(replace_na(WHO_db, 0))
  )

country_comparison %>%
  ungroup() %>%
  filter(Year == max(Year)) %>%
  group_by(who_region) %>%
  mutate(
    diff_top = (gap_filled_cumulative - who_db_cumulative) / 1e+6,
    diff_top_region_sum = sum(diff_top)
  ) %>%
  ungroup() %>%
  # select(who_region, ISO_A0, diff_top, diff_top_region_sum )
  mutate(
    # diff_modelling = (gap_filled_cumulative - gap_filled_no_modelling_cumulative) / 1e+6,
    diff_pct_top = (diff_top / diff_top_region_sum) * 100 # which country explains the most of gaps in this region
  ) %>%
  select(who_region, ISO_A0, gap_filled_cumulative, who_db_cumulative, diff_top_region_sum, diff_top, diff_pct_top) %>%
  group_by(who_region) %>%
  slice_max(diff_pct_top, n = 2)

#   who_region ISO_A0 gap_filled_cumulative who_db_cumulative diff_top_region_sum diff_top diff_pct_top
#   <chr>      <chr>                  <dbl>             <dbl>               <dbl>    <dbl>        <dbl>
# 1 AFRO       BFA                   335195             73808              0.449    0.261          58.2
# 2 EMRO       PAK                   766234             65071              1.10     0.701          63.9
# 3 EURO       REU                    75501              1227              0.0807   0.0743         92.0
# 4 PAHO       BRA                 35407550          25814627             16.3      9.59           59.0
# 5 SEARO      IDN                  3042737             26187              7.46     3.02           40.5
# 6 WPRO       VNM                  3613165            108433              9.97     3.50           35.2
