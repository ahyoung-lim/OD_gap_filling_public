library(dplyr)
source("functions/fn_OD_region.R")

# numbers for sankey diagram (Supp Fig 3): https://app.flourish.studio/visualisation/25960928/edit

tab <- read.csv("data/processed_data/dt_heatmap_calibrated_2025_10_22.csv") %>%
  add_od_regions()

comparison <- read.csv("data/processed_data/ad_hoc_comparison.csv")

# cys with >0 cases in original od but replaced with 0 cases
first_year_violations <- comparison %>%
  filter(data_source.x == "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y) & annual_total.y != 0) %>%
  pull(country_year) # 22

# cys that were also included in original od, but overwritten for first year violations
original_zero_first_year <- comparison %>%
  filter(data_source.x == "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y) & annual_total.y == 0) %>%
  pull(country_year) # 48

tab$data_source[tab$country_year %in% first_year_violations] <- "first_year_violations"
tab$data_source[tab$country_year %in% original_zero_first_year] <- "OD"

# found higher annual total through ad hoc and scaled to higher annual totals
scaled_up <- comparison %>%
  filter(data_source.x == "OD") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  filter(annual_total.x > annual_total.y) %>%
  pull(country_year) # 47

tab$data_source[tab$country_year %in% scaled_up] <- "ad_hoc_data"

# smaller
tab$data_source[tab$country_year == "JAPAN_2014"] <- "first_year_violations"

# same data but randomly chose ad_hoc_data
tab$data_source[tab$country_year %in% c("INDIA_2010", "INDIA_2011")] <- "OD"

tab <- tab %>%
  mutate(
    data = case_when(
      data_source %in% c("OD", "ad_hoc_data") ~ TRUE,
      TRUE ~ FALSE
    ),
    sub_annual = case_when(
      T_res %in% c("Week", "Month") ~ TRUE,
      TRUE ~ FALSE
    ),
    zero_total = case_when(
      annual_total == 0 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# tab <- tab %>%
#   mutate(label = case_when(
#     country_year %in% new_cys$country_year ~ "new_cys",
#     country_year %in% higher$country_year ~ "higher",
#     country_year %in% t_res_up$country_year ~ "t_res_up",
#     country_year %in% same$country_year ~ "same",
#     country_year %in% no_data$country_year ~ "no_data",
#     country_year %in% smaller ~ "smaller",
#     data_source == "first_year_violations" ~ "first_year_violations",
#     TRUE ~ NA
#   ))

# axis 1: data sources
tab %>%
  # filter(label %in% c("higher", "new_cys", "same", "smaller", "t_res_up"))%>%
  group_by(data_source) %>%
  tally()


# axis 2: annual vs. sub-annual

tab %>%
  filter(data) %>%
  group_by(data_source, sub_annual) %>%
  tally()

#   data_source sub_annual     n
#   <chr>       <lgl>      <int>
# 1 OD          FALSE       1991
# 2 OD          TRUE        1008
# 3 ad_hoc_data FALSE        280
# 4 ad_hoc_data TRUE         233

tab %>%
  filter(!data) %>%
  group_by(data_source, sub_annual) %>%
  tally()

#   data_source           sub_annual     n
#   <chr>                 <lgl>      <int>
# 1 IHME_calibrated       FALSE          7
# 2 Median_from_neighbors FALSE          5
# 3 ad_hoc_data           FALSE        300
# 4 assumed_absence       FALSE       1158
# 5 first_year_violations FALSE         22
# 6 first_year_violations TRUE           1

# axis 3: modelling strategy

tab %>%
  filter(data) %>%
  group_by(sub_annual, cat_model, zero_total) %>%
  tally()

#   sub_annual cat_model             zero_total     n
#   <lgl>      <chr>                 <lgl>      <int>
# 1 FALSE      Annual_disaggregation FALSE       1471
# 2 FALSE      No_modelling_required TRUE         800
# 3 TRUE       No_modelling_required FALSE        988
# 4 TRUE       No_modelling_required TRUE          80
# 5 TRUE       Sub_annual_imputation FALSE        130
# 6 TRUE       Sub_annual_imputation TRUE          43

tab %>%
  filter(!data) %>%
  group_by(data_source) %>%
  tally()

#   data_source               n
#   <chr>                 <int>
# 1 IHME_calibrated           7
# 2 Median_from_neighbors     5
# 3 ad_hoc_data             300
# 4 assumed_absence        1158
# 5 first_year_violations    23

# tab <- tab %>%
#   mutate(
#     T_res_cat = case_when(
#       T_res == "Year" & annual_total != 0 ~ "Annual data",
#       T_res != "Year" & cat_model == "No_modelling_required" & annual_total != 0 ~ "Complete sub-annual",
#       T_res != "Year" & cat_model != "No_modelling_required" ~ "Incomplete sub-annual",
#       annual_total == 0 ~ "Annual zeros",
#       TRUE ~ "No_data"
#     ),
#     data_source = case_when(
#       data_source == "first_year" ~ "ad_hoc_data",
#       data_source == "ees" ~ "assumed_absence",
#       TRUE ~ data_source
#     )
#   )
#
# tab %>%
#   group_by(cat_model) %>%
#   tally()
# #   cat_model                 n
# #   <chr>                 <int>
# # 1 Annual_disaggregation  1483
# # 2 No_modelling_required  3349
# # 3 Sub_annual_imputation   173
#
#
# tab %>%
#   mutate(
#     has_complete_subannual = case_when(
#       data_source %in% c("OD", "ad_hoc_data") & T_res != "Year" & cat_model == "No_modelling_required" ~ TRUE,
#       TRUE ~ FALSE
#     ),
#     has_any_data = case_when(
#       data_source %in% c("OD", "ad_hoc_data") ~ TRUE,
#       TRUE ~ FALSE
#     )
#   ) %>%
#   summarise(
#     has_any_data = sum(has_any_data),
#     has_complete_subannual = sum(has_complete_subannual)
#   )
#
#
# tab %>%
#   filter(data_source %in% c("OD", "ad_hoc_data")) %>%
#   group_by(T_res_cat, cat_model) %>%
#   tally()
#
# tab %>%
#   filter(!data_source %in% c("OD", "ad_hoc_data")) %>%
#   group_by(T_res_cat, cat_model) %>%
#   tally()
#
# tab %>%
#   filter(!data_source %in% c("OD", "ad_hoc_data")) %>%
#   group_by(data_source, T_res_cat, cat_model) %>%
#   tally()
