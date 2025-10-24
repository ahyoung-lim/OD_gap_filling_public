# ==============================================================================
# DATA COVERAGE ASSESSMENT AND CALIBRATION SCRIPT
# ==============================================================================
# Purpose: Assess data coverage, fill gaps, and calibrate using IHME estimates
# Process: 1) Load best temporal data and assess coverage
#          2) Handle first-year transmission and emerging epidemic settings
#          3) Calibrate missing data using IHME estimates
#          4) Impute remaining gaps from nearest neighbors
# Output: Calibrated time series with complete coverage metadata
# ==============================================================================

source("script/00_setup.R") # load libraries and functions
source("functions/fn_check_epiweek_span.R")
source("functions/fn_consecutive_gap_counter.R") # counting the number of consecutive gaps
source("functions/fn_make_week_complete.R") # make weekly data complete
source("functions/fn_Year_checker.R") # adjusting Year column
source("functions/fn_OD_region.R") # regional classification

git_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/master-repo-alim/master-repo/data/releases/V1.3/"

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

add_country_year <- function(df) {
  df %>% mutate(country_year = paste0(adm_0_name, "_", Year))
}

# ------------------------------------------------------------------------------
# 1. LOAD AND PREPARE DATA
# ------------------------------------------------------------------------------

# 1.1 Load Best Temporal Data -------------------------------------------------
T_data <- read.csv("data/processed_data/Best_T_data_V1_3_2025_10_22.csv") %>%
  filter(between(Year, min_year, max_year)) %>% # keep target period
  region_class() %>% # add WHO region
  Year_checker() %>% # fix year alignment
  add_country_year()

# 1.2 Check for Missing Regional Classifications ------------------------------
T_data %>%
  filter(is.na(region)) %>%
  distinct(adm_0_name)

# 1.3 Summarize Record Counts and Coverage ------------------------------------
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

# 1.4 Initialize Coverage Table -----------------------------------------------
# Start with complete country-year grid
coverage_tbl <- T_data %>%
  group_by(adm_0_name, Year) %>%
  summarise(
    annual_total = sum(dengue_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_country_year()

# 1.5 Determine Data Source Priority ------------------------------------------
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

# 1.6 Categorize Data Completeness --------------------------------------------
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


# ------------------------------------------------------------------------------
# 2. HANDLE FIRST-YEAR TRANSMISSION AND EMERGING EPIDEMIC SETTINGS
# ------------------------------------------------------------------------------

# 2.1 Define Geographic Groups ------------------------------------------------
# Pacific Island Countries
PIC_names <- T_data %>%
  filter(grepl("PICs", UUID)) %>%
  distinct(adm_0_name) %>%
  pull()

# Caribbean countries
caribbean_names <- toupper(c(
  "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Belize",
  "Bermuda", "Bonaire, Saint Eustatius and Saba", "Cayman Islands", "Cuba",
  "Curacao", "Dominica", "Dominican Republic", "Grenada", "Guadeloupe", "Haiti",
  "Jamaica", "Martinique", "Montserrat", "Puerto Rico", "Saint Barthelemy",
  "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin",
  "Saint Vincent and the Grenadines", "Sint Maarten",
  "Trinidad and Tobago", "Turks and Caicos Islands"
))

# 2.2 Handle First Year of Local Transmission ---------------------------------
# Load documented first year of local dengue transmission
first_year <- read.csv("data/ad_hoc/first_year_summary.csv") %>%
  mutate(adm_0_name = toupper(adm_0_name))

# Identify violations: cases reported before documented first transmission
violations <- coverage_tbl %>%
  left_join(first_year, by = "adm_0_name") %>%
  filter(
    Year < first_year, annual_total != 0
  ) %>%
  add_country_year()

# Update coverage table: set years before first transmission to zero
coverage_tbl <- coverage_tbl %>%
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

# 2.3 Remove Violations from Time Series Data ---------------------------------
T_data_clean <- T_data %>%
  anti_join(violations, by = c("adm_0_name", "Year")) %>%
  filter(!country_year %in% violations$country_year)

# Manual exclusions: additional incomplete zero records
T_data_clean <- T_data_clean %>%
  filter(!country_year %in% c("GUAM_2017", "GUAM_2018"))

# 2.4 Add Back Corrected Records -----------------------------------------------
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
  select(names(T_data_clean)) # maintain column order

T_data_clean <- rbind(toadd, T_data_clean)

summary(is.na(coverage_tbl))
nrow(coverage_tbl[coverage_tbl$data_source == "No_data", ])

# Clean up temporary objects
rm(first_year, violations, d_cov, data_source, epiweek_tbl, toadd, toadd2)

# 2.5 Define Emerging Epidemic Settings (EES) ---------------------------------
# EES: Regions with sporadic transmission where missing years are assumed zero
# Includes: Africa, select Middle East, Europe, small island nations

# Get all African and EMRO countries
Afro_names <- unique(coverage_tbl$adm_0_name[coverage_tbl$region == "AFRO"])
Emro_names <- unique(coverage_tbl$adm_0_name[coverage_tbl$region == "EMRO"])

# Define EES countries (sporadic transmission, assume zeros for missing years)
ees <- c(
  "Afghanistan", "Djibouti", "Egypt", "Oman", "Somalia", "Yemen",
  "Iran (ISLAMIC REPUBLIC OF)", "Sudan"
)
ees <- c(PIC_names, caribbean_names, ees, Afro_names)

# Apply EES assumptions: missing years = 0 cases, no modelling required
coverage_tbl <- coverage_tbl %>%
  mutate(ees = case_when(
    adm_0_name %in% toupper(ees) & is.na(annual_total) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(
    annual_total = case_when(ees ~ 0, TRUE ~ annual_total),
    cat_model = case_when(ees ~ "No_modelling_required", TRUE ~ cat_model),
    prop = case_when(ees ~ 1, TRUE ~ prop),
    n_record = case_when(ees ~ 1, TRUE ~ n_record),
    T_res = case_when(ees ~ "Year", TRUE ~ T_res),
    data_source = case_when(ees ~ "ees", TRUE ~ data_source)
  )

# Count EES countries
ees_final <- coverage_tbl %>%
  filter(data_source == "ees") %>%
  distinct(adm_0_name) %>%
  pull(adm_0_name)

length(ees_final) # 74 countries

# ------------------------------------------------------------------------------
# 3. IHME CALIBRATION FOR REMAINING "NO_DATA" GAPS
# ------------------------------------------------------------------------------

# 3.1 Harmonize ISO Codes -----------------------------------------------------
coverage_tbl <- coverage_tbl %>%
  mutate(
    ISO_A0 = countrycode(adm_0_name, "country.name", "iso3c"),
    ISO_A0 = if_else(adm_0_name == "SAINT MARTIN", "MAF", ISO_A0)
  )

# 3.2 Load IHME GBD 2021 Estimates ---------------------------------------------
ihme_raw <- read.csv("data/ad_hoc/IHME-GBD_2021_DATA.csv") %>%
  transmute(
    adm_0_name = location_name,
    Year = year,
    IHME_est = as.integer(val),
    ISO_A0 = countrycode(location_name, "country.name", "iso3c")
  )

# Check IHME countries not in OpenDengue
ihme_only_iso <- unique(ihme_raw$ISO_A0[!ihme_raw$ISO_A0 %in% coverage_tbl$ISO_A0])

# Visualize IHME-only countries with non-zero estimates
ihme_raw %>%
  filter(ISO_A0 %in% ihme_only_iso) %>%
  group_by(adm_0_name) %>%
  mutate(total = sum(IHME_est)) %>%
  filter(total != 0) %>%
  ggplot() +
  geom_line(aes(x = Year, y = IHME_est)) +
  facet_wrap(adm_0_name ~ ., scales = "free")

# Note: Jordan, Kuwait, Lebanon, Palestine, Syrian Arab Republic have IHME estimates
# but no confirmed local dengue outbreaks or sentinel cases per literature
# Source: https://pmc.ncbi.nlm.nih.gov/articles/PMC5142774/

# 3.3 Identify Overlap: OD Gaps with IHME Data --------------------------------
od_no_data_iso <- coverage_tbl %>%
  filter(data_source == "No_data", Year < 2022) %>%
  distinct(ISO_A0) %>%
  pull()

iso_overlap <- intersect(od_no_data_iso, ihme_raw$ISO_A0)

# IHME calibration will be applied to these countries:
unique(coverage_tbl$adm_0_name[coverage_tbl$ISO_A0 %in% iso_overlap])
# [1] "BRUNEI DARUSSALAM" "INDIA" "MALDIVES" "TIMOR-LESTE"

# 3.4 Calculate Country-Level Scaling Factors ---------------------------------
# Scale IHME estimates using ratio: (OD mean) / (IHME mean)
coverage_tbl_cal <- coverage_tbl %>%
  left_join(
    ihme_raw %>%
      filter(ISO_A0 %in% iso_overlap) %>%
      select(ISO_A0, IHME_est, Year),
    by = c("ISO_A0", "Year")
  ) %>%
  group_by(adm_0_name) %>%
  mutate(
    OD_mean    = mean(annual_total, na.rm = TRUE),
    IHME_mean  = mean(IHME_est, na.rm = TRUE),
    scalar     = OD_mean / IHME_mean,
    IHME_cal   = as.integer(IHME_est * scalar) # calibrated estimate
  ) %>%
  ungroup()

# Check for countries where scaling factor cannot be estimated
no_scaling <- coverage_tbl_cal %>%
  filter(cat_model == "No_data" & (is.na(scalar) | is.infinite(scalar))) %>%
  filter(adm_0_name %in% iso_overlap) %>%
  distinct(adm_0_name)
no_scaling

# 3.5 Fill No_data Slots with Calibrated IHME Estimates -----------------------
coverage_tbl_cal <- coverage_tbl_cal %>%
  mutate(
    # Fill annual total with calibrated IHME estimate
    annual_total2 = case_when(
      cat_model == "No_data" & is.na(annual_total) & !is.na(IHME_cal) ~ IHME_cal,
      TRUE ~ annual_total
    ),

    # Update data source label
    data_source = case_when(
      cat_model == "No_data" & is.na(annual_total) & !is.na(IHME_cal) ~ "IHME_calibrated",
      cat_model == "No_data" & is.na(annual_total) & is.na(IHME_cal) ~ "No_data",
      TRUE ~ data_source
    ),

    # Update modelling category
    cat_model = case_when(
      data_source == "IHME_calibrated" & annual_total2 != 0 ~ "Annual_disaggregation",
      data_source == "IHME_calibrated" & annual_total2 == 0 ~ "No_modelling_required",
      data_source == "No_data" ~ "No_data",
      TRUE ~ cat_model
    ),

    # Update coverage metrics for IHME-calibrated records
    prop = case_when(
      data_source == "IHME_calibrated" ~ 1,
      data_source == "No_data" ~ NA,
      TRUE ~ prop
    ),
    n_record = case_when(
      data_source == "IHME_calibrated" ~ 1,
      data_source == "No_data" ~ NA,
      TRUE ~ n_record
    ),
    T_res = case_when(
      data_source == "IHME_calibrated" ~ "Year",
      data_source == "No_data" ~ NA_character_,
      TRUE ~ T_res
    )
  ) %>%
  mutate(
    cat_model = factor(cat_model, levels = c(
      "No_data", "Annual_disaggregation", "Sub_annual_imputation",
      "No_modelling_required"
    ))
  )

summary(is.na(coverage_tbl_cal))
nrow(coverage_tbl_cal[coverage_tbl_cal$data_source == "No_data", ])

# 3.6 Diagnostic Plot: Calibrated Time Series ---------------------------------
coverage_tbl_cal %>%
  filter(ISO_A0 %in% iso_overlap) %>%
  ggplot() +
  geom_line(aes(Year, annual_total2)) +
  geom_point(aes(Year, annual_total2, color = data_source)) +
  facet_wrap(adm_0_name ~ ., scales = "free")

# 3.7 Data Source Summary ------------------------------------------------------
coverage_tbl_cal %>%
  group_by(data_source) %>%
  tally()

#   data_source         n
# 1 IHME_calibrated     7
# 2 No_data             5
# 3 OD               3076
# 4 ad_hoc_data       400
# 5 ees              1532
# 6 first_year        370

# 3.8 Remove Countries with Only Zero Cases -----------------------------------
# Identify countries with zero cases across all years
zero_cases_only <- coverage_tbl_cal %>%
  group_by(adm_0_name) %>%
  summarise(annual_total2 = sum(annual_total2)) %>%
  filter(annual_total2 == 0) %>%
  pull(adm_0_name)

length(zero_cases_only) # 11 countries
# [1] "BURUNDI"           "CONGO"             "EQUATORIAL GUINEA" "GAMBIA"            "GUINEA-BISSAU"
#  [6] "MALAWI"            "NAMIBIA"           "RWANDA"            "UGANDA"            "ZAMBIA"
# [11] "ZIMBABWE"

# Remove from both coverage table and time series data
coverage_tbl_cal <- coverage_tbl_cal %>%
  filter(!adm_0_name %in% zero_cases_only)

T_data_cal <- T_data_clean %>%
  filter(!adm_0_name %in% zero_cases_only)

# African countries with only zero cases - verified against literature
# Source: https://www.mdpi.com/1999-4915/14/2/233#B35-viruses-14-00233
# BURUNDI, CONGO, EQUATORIAL GUINEA, GAMBIA, GUINEA-BISSAU, MALAWI,
# NAMIBIA, RWANDA, UGANDA, ZAMBIA, ZIMBABWE - all confirmed no outbreaks

# ------------------------------------------------------------------------------
# 4. IMPUTE REMAINING GAPS FROM NEAREST NEIGHBORS
# ------------------------------------------------------------------------------

# For remaining gaps, impute using median from k=10 nearest years
impute_nearest_median <- function(df, k = 10) {
  df %>%
    arrange(adm_0_name, Year) %>%
    # Flag rows to fill before altering data
    mutate(to_fill = data_source == "No_data" & is.na(annual_total2)) %>%
    group_by(adm_0_name) %>%
    mutate(
      # Fill annual_total3 with median from k nearest observed years
      annual_total3 = map_dbl(row_number(), function(i) {
        if (!to_fill[i]) {
          return(annual_total2[i])
        } # leave observed rows untouched

        obs_idx <- which(!is.na(annual_total2)) # indices of observed rows
        if (length(obs_idx) == 0) {
          return(NA_real_)
        } # no neighbors available

        d <- abs(Year[obs_idx] - Year[i]) # temporal distance
        nearest <- obs_idx[order(d)][seq_len(min(k, length(d)))]
        as.integer(median(annual_total2[nearest], na.rm = TRUE))
      }),

      # Mark provenance for imputed values
      data_source = if_else(to_fill, "Median_from_neighbors", data_source),
      cat_model = if_else(to_fill & annual_total3 != 0, "Annual_disaggregation",
        if_else(to_fill & annual_total3 == 0, "No_modelling_required", as.character(cat_model))
      ),
      prop = if_else(to_fill, 1, prop),
      n_record = if_else(to_fill, 1, n_record),
      T_res = if_else(to_fill, "Year", as.character(T_res))
    ) %>%
    ungroup() %>%
    select(-to_fill) # remove helper column
}

# Apply nearest neighbor imputation
coverage_tbl_cal <- impute_nearest_median(coverage_tbl_cal, k = 10)

summary(is.na(coverage_tbl_cal))
nrow(coverage_tbl_cal[coverage_tbl_cal$data_source == "No_data", ])

# ------------------------------------------------------------------------------
# 5. APPEND CALIBRATED/IMPUTED RECORDS TO TIME SERIES DATA
# ------------------------------------------------------------------------------

# Prepare calibrated annual records for addition to time series
toadd <- coverage_tbl_cal %>%
  filter(data_source %in% c("IHME_calibrated", "Median_from_neighbors", "ees")) %>%
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
    dengue_total = as.integer(annual_total3),
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Year",
    region,
    UUID = data_source,
    cat = "assumed",
    scaled_to_annual = NA,
    country_year
  ) %>%
  select(names(T_data)) # maintain column order

# Combine with existing time series data
T_data_cal <- bind_rows(T_data_cal, toadd)

# ------------------------------------------------------------------------------
# 6. FINAL VALIDATION AND EXPORT
# ------------------------------------------------------------------------------

# Check for duplicate records (should be 0)
T_data_cal %>%
  group_by(adm_0_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1)

summary(is.na(T_data_cal))
T_data_cal$ISO_A0 <- countrycode::countrycode(
  T_data_cal$adm_0_name, "country.name", "iso3c"
)
T_data_cal$ISO_A0[T_data_cal$adm_0_name == "SAINT MARTIN"] <- "MAF"

# Check for duplicate country-years in coverage table (should be 0)
coverage_tbl_cal %>%
  group_by(adm_0_name, Year) %>%
  filter(n() > 1)

# Export calibrated time series data
write.csv(T_data_cal,
  "data/processed_data/Best_T_data_calibrated_V1_3_2025_10_22.csv",
  row.names = F
)

# Export coverage metadata
write.csv(
  coverage_tbl_cal %>%
    select(-annual_total, -(IHME_est:annual_total2)) %>%
    mutate(annual_total = annual_total3) %>%
    select(-annual_total3),
  "data/processed_data/dt_heatmap_calibrated_2025_10_22.csv",
  row.names = F
)
