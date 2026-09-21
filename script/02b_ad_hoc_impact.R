# ==============================================================================
# AD HOC IMPACT — compare best-record selection WITH vs WITHOUT ad hoc data
# ==============================================================================
# Both inputs are produced by the SAME 01b pipeline (only the ad hoc merge toggled):
#   Best_T_data_V1_3.csv            = OD + ad hoc   (default 01b run)
#   Best_T_data_V1_3_excl_ad_hoc.csv = OD only      (01b with EXCLUDE_AD_HOC=true)
# so any difference here is attributable to ad hoc presence, not code drift.
#
# Run the two 01b passes first (or use the wrapper that chains all three):
#   Rscript script/02a_run_ad_hoc_impact.R
# This script alone just does the comparison and writes ad_hoc_comparison.csv.
# ==============================================================================

source("script/00_setup.R") # load libraries and functions
source("functions/fn_Year_checker.R") # adjusting Year column
source("functions/fn_select_best_rec.R")
source("functions/fn_consecutive_gap_counter.R") # counting the number of consecutive gaps
source("functions/fn_make_week_complete.R") # make weekly data complete
source("functions/fn_OD_region.R") # regional classification

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

add_country_year <- function(df) {
  df %>% mutate(country_year = paste0(adm_0_name, "_", Year))
}

min_year <- 1990
max_year <- 2024
# ------------------------------------------------------------------------------
# 1. LOAD AND PREPARE DATA
# ------------------------------------------------------------------------------

# 1.1 Load best temporal data -------------------------------------------------
T_data_new <- read.csv("data/processed_data/Best_T_data_V1_3.csv") %>%
  filter(between(Year, min_year, max_year)) %>% # keep target period
  region_class() %>% # add WHO region
  Year_checker() %>% # fix year alignment
  add_country_year()

T_data_old <- read.csv("data/processed_data/Best_T_data_V1_3_excl_ad_hoc.csv") %>%
  filter(between(Year, min_year, max_year)) %>% # keep target period
  region_class() %>% # add WHO region
  Year_checker() %>% # fix year alignment
  add_country_year()


# 1.2 Check for missing regional classifications ------------------------------
T_data_old %>%
  filter(is.na(region)) %>%
  distinct(adm_0_name)

# 1.3 Summarize record counts and coverage ------------------------------------
build_coverage_tbl <- function(T_data) {
  d_cov <- T_data %>%
    group_by(adm_0_name, Year, T_res) %>%
    summarise(
      n_record = n(), # raw number of rows
      .groups  = "drop"
    ) %>%
    mutate(
      n_time_den = case_when( # denominator for coverage %
        T_res == "Month" ~ 12L,
        T_res == "Year" ~ 1L,
        TRUE ~ 52L # default for weekly; refined below
      )
    ) %>%
    add_country_year()

  # Replace weekly denominator with country-specific epiweek counts
  year_range <- range(T_data$Year, na.rm = TRUE)
  epiweek_tbl <- build_epiweek_lookup(
    start_year = year_range[1],
    end_year = year_range[2]
  )

  d_cov <- d_cov %>%
    left_join(epiweek_tbl, by = "Year") %>%
    mutate(
      n_time_den = if_else(T_res == "Week", epiweeks, n_time_den),
      prop       = n_record / n_time_den
    ) %>%
    select(-epiweeks)

  # 1.4 Initialize coverage table -----------------------------------------------
  # Start with complete country-year grid
  coverage_tbl <- T_data %>%
    group_by(adm_0_name, Year) %>%
    summarise(
      annual_total = sum(dengue_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    add_country_year()

  # 1.5 Determine data source priority ------------------------------------------
  # When both OD and ad_hoc_data exist, prioritise ad_hoc_data
  data_source <- T_data %>%
    distinct(adm_0_name, Year, cat) %>% # all combos present in the file
    complete(adm_0_name, Year) %>% # add holes = No_data
    group_by(adm_0_name, Year) %>%
    mutate(priority = case_when(
      cat == "ad_hoc_data" ~ 1, # prioritise ad_hoc_data over OD
      cat == "OD" ~ 2,
      TRUE ~ 3
    )) %>%
    arrange(priority) %>%
    slice(1) %>%
    select(-priority) %>%
    ungroup() %>%
    add_country_year() %>%
    mutate(
      data_source = case_when(
        is.na(cat) ~ "No_data",
        TRUE ~ cat
      )
    ) %>%
    select(-cat)



  # Merge data source labels into coverage table
  coverage_tbl <- coverage_tbl %>%
    merge(., data_source %>% select(country_year, adm_0_name, Year, data_source),
      by = c("country_year", "adm_0_name", "Year"), all.y = TRUE
    )

  # Verify no duplicates
  coverage_tbl %>%
    group_by(country_year) %>%
    filter(n() > 1)

  summary(is.na(coverage_tbl))

  # 1.6 Categorize data completeness --------------------------------------------
  coverage_tbl <- coverage_tbl %>%
    # Join coverage metrics
    left_join(d_cov %>% select(country_year, T_res, n_record, prop),
      by = "country_year"
    ) %>%
    # Assign modelling category
    mutate(
      cat_model = case_when(
        !is.na(prop) & prop == 1 & T_res != "Year" ~ "No_modelling_required",
        !is.na(prop) & prop == 1 & T_res == "Year" & annual_total == 0 ~ "No_modelling_required",
        !is.na(prop) & prop == 1 & T_res == "Year" & annual_total != 0 ~ "Annual_disaggregation",
        !is.na(prop) & prop < 1 ~ "Sub_annual_imputation",
        is.na(prop) ~ "No_data"
      ),
      T_res = factor(T_res, levels = c("Week", "Month", "Year"))
    )

  # Add regional classification
  coverage_tbl <- coverage_tbl %>%
    region_class()

  summary(is.na(coverage_tbl$region))
  unique(coverage_tbl$cat_model)

  # Set factor levels for modelling categories
  coverage_tbl$cat_model <- factor(coverage_tbl$cat_model, levels = c(
    "No_data",
    "Annual_disaggregation",
    "Sub_annual_imputation",
    "No_modelling_required"
  ))

  return(list(
    coverage_tbl = coverage_tbl,
    T_data = T_data
  ))
}

result_old <- build_coverage_tbl(T_data_old)
result_new <- build_coverage_tbl(T_data_new)

coverage_tbl_old <- result_old$coverage_tbl
coverage_tbl_new <- result_new$coverage_tbl

coverage_tbl_old %>%
  group_by(cat_model) %>%
  tally()


# 2.2 Handle first year of local transmission ---------------------------------
# Load documented first year of local dengue transmission
first_year <- read.csv("data/processed_data/first_year_summary.csv") %>%
  mutate(adm_0_name = toupper(adm_0_name))

# Identify violations: cases reported before documented first transmission
violations <- coverage_tbl_new %>%
  left_join(first_year, by = "adm_0_name") %>%
  filter(
    Year < first_year, annual_total != 0
  ) %>%
  add_country_year()

# Update coverage table: set years before first transmission to zero
coverage_tbl_new <- coverage_tbl_new %>%
  left_join(first_year %>% select(adm_0_name, first_year), by = "adm_0_name") %>%
  mutate(
    before_first_year = Year < first_year & !is.na(first_year),

    # Update all columns for years before first transmission
    annual_total = if_else(before_first_year, 0, annual_total),
    data_source = if_else(before_first_year, "first_year", data_source),
    T_res = if_else(before_first_year, "Year", T_res),
    n_record = if_else(before_first_year, 1, n_record),
    prop = if_else(before_first_year, 1, prop),
    cat_model = if_else(before_first_year, "No_modelling_required", cat_model)
  ) %>%
  select(-first_year, -before_first_year)

# 2.3 Remove violations from time series data ---------------------------------
T_data_new <- T_data_new %>%
  anti_join(violations, by = c("adm_0_name", "Year")) %>%
  filter(!country_year %in% violations$country_year)

# Manual exclusions: additional incomplete zero records
T_data_new <- T_data_new %>%
  filter(!country_year %in% c("GUAM_2017", "GUAM_2018"))

# 2.4 Add back corrected records -----------------------------------------------
# Prepare violations as zero-case annual records
toadd2 <- tibble(
  adm_0_name = c(rep("GUAM", 2)),
  Year = c(2017, 2018),
  region = c("WPRO", "WPRO")
) %>%
  add_country_year() %>%
  mutate(Source = NA)

toadd <- violations %>%
  select(adm_0_name, Year, region, country_year, Source) %>%
  rbind(., toadd2) %>%
  transmute(
    adm_0_name,
    adm_1_name = NA_character_,
    adm_2_name = NA_character_,
    full_name = adm_0_name,
    ISO_A0 = countrycode(adm_0_name, "country.name", "iso3c"),
    FAO_GAUL_code = NA, RNE_iso_code = NA, IBGE_code = NA,
    calendar_start_date = as.character(lubridate::make_date(Year, 1, 1)),
    calendar_end_date = as.character(lubridate::make_date(Year, 12, 31)),
    Year,
    dengue_total = 0,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Year",
    UUID = Source,
    cat = "first_year",
    scaled_to_annual = FALSE,
    region,
    country_year
  ) %>%
  select(names(T_data_new)) # maintain column order

T_data_new <- rbind(toadd, T_data_new)


zero_cases_only <- c(
  "BURUNDI", "CONGO", "EQUATORIAL GUINEA", "GAMBIA",
  "GUINEA-BISSAU", "MALAWI", "NAMIBIA", "RWANDA", "UGANDA", "ZAMBIA", "ZIMBABWE"
)

coverage_tbl_new <- coverage_tbl_new %>% filter(!adm_0_name %in% zero_cases_only)
T_data_new <- T_data_new %>% filter(!adm_0_name %in% zero_cases_only)

comparison <- merge(
  coverage_tbl_new %>% select(country_year:prop),
  coverage_tbl_old %>% select(country_year:prop),
  by = c("country_year", "adm_0_name", "Year"), all = T
)

first_year_new <- comparison %>%
  filter(data_source.x == "first_year") %>%
  filter(!is.na(annual_total.x) & is.na(annual_total.y)) # 300

comparison_filtered <- comparison %>%
  filter(!country_year %in% first_year_new$country_year)

comparison %>%
  filter(data_source.x == "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  pull(country_year)

write.csv(comparison, "data/processed_data/ad_hoc_comparison.csv", row.names = F)

# original OD version 1.3
length(unique(T_data_old$country_year)) # 3268 country-years
length(unique(T_data_old$adm_0_name)) # 128 countries

# new OD + ad hoc data + first year
length(unique(T_data_new$country_year)) # 3835 country-years
length(unique(T_data_new$adm_0_name)) # 143 countries

# newly added:
new_cys <- comparison_filtered %>%
  filter(!is.na(annual_total.x) & is.na(annual_total.y))

# adding previously undocumented country-years
new_cys %>%
  group_by(data_source.x) %>%
  tally()
#   data_source.x     n
#   <chr>         <int>
# 1 ad_hoc_data     267

new_cys %>%
  mutate(ISO_A0 = countrycode::countrycode(
    adm_0_name, "country.name", "iso3c"
  )) %>%
  add_od_regions() %>%
  group_by(od_region) %>%
  tally()


# available in both but with some updates:
# 1) higher annual total
higher <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  # filter(data_source.x == "OD" & data_source.y == "OD")%>%
  filter(annual_total.x > annual_total.y) # 162 -> 164

# 2) same total but different resolution
t_res_up <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  # filter(data_source.x == "OD" & data_source.y == "OD")%>%
  filter(annual_total.x == annual_total.y & T_res.x != T_res.y) %>%
  select(country_year, T_res.x, T_res.y, data_source.x) # 84 -> 87

# 3) smaller total (but higher T_res)
smaller <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  # filter(data_source.x == "OD" & data_source.y == "OD")%>%
  filter(annual_total.x < annual_total.y) # 1 (japan) + 22 (first year violations) -> 1+22

# Everything remains the same:
same <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  filter(annual_total.x == annual_total.y & T_res.x == T_res.y) # 2951 -> 2946

# no data in either:
no_data <- comparison_filtered %>%
  filter(is.na(annual_total.x) & is.na(annual_total.y)) # 1170

# first year of dengue introduction
# first_year <- comparison %>%
#   filter(data_source.x == "first_year")
# filter(!is.na(annual_total.x) & !is.na(annual_total.y)) # first year violations

nrow(new_cys) + nrow(higher) + nrow(t_res_up) + nrow(smaller) + nrow(same) + nrow(no_data) + nrow(first_year_new) # 4935


sum(comparison$annual_total.x, na.rm = T) - sum(comparison$annual_total.y, na.rm = T) # 1801254 -> 1813434

#
# coverage_tbl_new %>%
#   group_by(data_source) %>%
#   tally()
#
# coverage_tbl_new %>%
#   filter(data_source %in% c("OD", "ad_hoc_data", "first_year")) %>%
#   group_by(T_res) %>%
#   tally()
#
# coverage_tbl_new %>%
#   filter(data_source %in% c("OD", "ad_hoc_data", "first_year")) %>%
#   filter(T_res != "Year") %>%
#   group_by(cat_model) %>%
#   tally()
#
# coverage_tbl_new %>%
#   group_by(T_res, cat_model)%>%
#   tally()
#
#
# tab_cy <- coverage_tbl_new %>%
#   filter(data_source == "ad_hoc_data") %>%
#   pull(country_year) # 467
#
# higher_od_cy <- higher %>%
#   select(country_year, data_source.x) %>%
#   filter(data_source.x != "ad_hoc_data") # 47
#
# higher_cy <- higher %>%
#   select(country_year, data_source.x) %>%
#   filter(data_source.x == "ad_hoc_data") %>%
#   pull(country_year) # 113
#
# t_res_up_cy <- t_res_up %>%
#   select(country_year, data_source.x) %>%
#   filter(data_source.x == "ad_hoc_data") %>%
#   pull(country_year) # 84
#
# tab_cy <- setdiff(tab_cy, new_cys$country_year) # 200
# tab_cy <- setdiff(tab_cy, higher_cy)
# tab_cy <- setdiff(tab_cy, t_res_up_cy) # three cys actually didnt benefit from ad-hoc
#
# scaled <- T_data_new %>%
#   filter(scaled_to_annual) %>%
#   distinct(country_year)
#
# summary(scaled$country_year %in% higher_od_cy$country_year)
#
# coverage_tbl_new$data_source[coverage_tbl_new$country_year %in% scaled$country_year]
