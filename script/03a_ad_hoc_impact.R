# ==============================================================================
# TEMPORAL DATA SELECTION AND AGGREGATION SCRIPT
# ==============================================================================
# Purpose: Select best record (highest sum) per country-year and aggregate to national level
# Process: 1) Load and merge data sources
#          2) Resolve duplicates and data quality issues
#          3) Select best record per country-year (prioritizing highest sum)
#          4) Extract and aggregate records with optional scaling
# Output: National-level dataset with best available records
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP AND LOAD BASE DATA
# ------------------------------------------------------------------------------

source("script/00_setup.R") # load libraries and functions
source("functions/fn_Year_checker.R") # adjusting Year column
source("functions/fn_select_best_rec.R")
source("functions/fn_consecutive_gap_counter.R") # counting the number of consecutive gaps
source("functions/fn_make_week_complete.R") # make weekly data complete
source("functions/fn_OD_region.R") # regional classification

# 1.1 Load OpenDengue temporal extract ----------------------------------------
git_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/master-repo-alim/master-repo/data/releases/V1.3/"

od <- readr::read_csv(paste0(git_path, "/Temporal_extract_V1_3_2025_08_01.csv")) %>%
  filter(!adm_0_name == "PITCAIRN") %>%
  filter(between(Year, min_year, max_year)) %>%
  Year_checker()

# Extract Admin0-level country codes for reference
adm0_codes <- od[od$S_res == "Admin0", c("adm_0_name", "FAO_GAUL_code", "RNE_iso_code")] %>%
  distinct()

# ------------------------------------------------------------------------------
# 2. DATA QUALITY FIXES
# ------------------------------------------------------------------------------

# 2.1 Taiwan spatial resolution correction ------------------------------------
# Revise S_res for Taiwan records with 0 cases
unique(od$S_res[od$adm_0_name == "TAIWAN" & od$UUID == "MOH-TWN-19982024-Y01-01"])

od$S_res <- ifelse(od$adm_0_name == "TAIWAN" & od$UUID == "MOH-TWN-19982024-Y01-01", "Admin2", od$S_res)
od <- od %>%
  filter(!(adm_0_name == "TAIWAN" & Year == 1997 & T_res == "Week"))

# 2.2 Remove records with known errors ----------------------------------------
# Country-specific data quality issues identified during validation
od <- od %>%
  filter(!UUID == "WHOEMRO-AFG-2023-W02-52") %>%
  filter(!(UUID == "WHOWPRO-ALL-2016-W01-03" & adm_0_name == "MALAYSIA" & Year == 2014)) %>%
  filter(!(adm_0_name == "MALAYSIA" & Year == 2020 & ymd(calendar_start_date) > "2020-12-12")) %>%
  filter(!(adm_0_name == "NEW CALEDONIA" & Year == 2019 & T_res == "Week")) %>%
  filter(!(adm_0_name == "SRI LANKA" & Year == 2018 & calendar_start_date == "2018-12-23")) %>%
  filter(!(adm_0_name == "SRI LANKA" & Year == 2019 & calendar_start_date == "2019-12-22")) %>%
  filter(!(adm_0_name == "YEMEN" & Year == 2016))

# Check for mixed temporal resolutions within country-years
od %>%
  group_by(adm_0_name, Year) %>%
  summarise(n = n_distinct(T_res))

# ------------------------------------------------------------------------------
# 3. MERGE WITH AD HOC DATA
# ------------------------------------------------------------------------------
# od$cat <- "OD"
# ad <- read.csv("data/processed_data/ad_hoc_data_all_2025_10_22.csv")

# Combine OpenDengue and ad hoc data with category labels
od <- rbind(
  od %>% mutate(cat = "OD")
  # ad
)

# Create country-year identifier for all analyses
od$country_year <- paste0(od$adm_0_name, "_", od$Year)

# ------------------------------------------------------------------------------
# 4. SOURCE PRIORITIZATION AND REPLACEMENT
# ------------------------------------------------------------------------------

# 4.1 Replace with higher quality sources -------------------------------------
# Solomon Islands: Use PLOS ONE article data for better coverage
# Vanuatu: Use MOH data source with more complete records
od_clean <- od %>%
  filter(!(UUID == "WHOWPRO-PICs-2017-Y01-00" & adm_0_name == "SOLOMON ISLANDS" & calendar_start_date >= ymd("2017-04-02") &
    calendar_end_date <= ymd("2017-12-16"))) %>%
  filter(!(UUID == "WHOWPRO-PICs-2019-Y01-02" & country_year == "VANUATU_2019" & calendar_start_date == "2018-12-30"))

# 4.2 Remove travel-related cases only -----------------------------------------
# Oman: Between 2001-2017, all cases were travel-related (not local transmission)
# Keep only literature source documenting this
od_clean <- od_clean %>%
  mutate(
    to_remove = adm_0_name == "OMAN" &
      calendar_start_date >= ymd("2001-01-01") &
      calendar_end_date <= ymd("2017-12-31") &
      UUID != "https://pmc.ncbi.nlm.nih.gov/articles/PMC6425053/"
  ) %>%
  filter(!to_remove) %>%
  select(-to_remove)

# ------------------------------------------------------------------------------
# 5. SYSTEMATIC DUPLICATE RESOLUTION
# ------------------------------------------------------------------------------

# Identify all duplicates (same location and time period)
x <- od_clean %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1)

# 5.1 Monthly duplicates -------------------------------------------------------
monthly_dups <- x %>%
  filter(T_res == "Month")

# Review monthly duplicate totals by UUID
monthly_dups %>%
  group_by(adm_0_name, Year, UUID) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  print(n = 23)

# Identify UUIDs with maximum totals to keep
keep_uuids_month <- monthly_dups %>%
  group_by(adm_0_name, Year, UUID, T_res) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  group_by(adm_0_name, Year, start_date, end_date) %>%
  slice_max(total, n = 1, with_ties = FALSE)

# Identify UUIDs to remove (those NOT in keep list)
uuids_to_remove_month <- monthly_dups %>%
  group_by(adm_0_name, Year, UUID, T_res) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  anti_join(keep_uuids_month, by = c("adm_0_name", "Year", "UUID", "T_res"))

# Remove records matching UUIDs to remove within date range
od_clean <- od_clean %>%
  left_join(uuids_to_remove_month, by = c("adm_0_name", "Year", "UUID", "T_res"), suffix = c("", "_remove")) %>%
  filter(
    is.na(start_date) | # Not a UUID to remove
      calendar_start_date < start_date | # Outside removal date range
      calendar_end_date > end_date # Outside removal date range
  ) %>%
  select(-start_date, -end_date, -total)

# 5.2 Weekly duplicates --------------------------------------------------------
weekly_dups <- x %>%
  filter(T_res == "Week")

# Review weekly duplicate totals by UUID
weekly_dups %>%
  group_by(adm_0_name, Year, UUID, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  print(n = 23)

# Identify UUIDs with maximum totals to keep
keep_uuids_week <- weekly_dups %>%
  group_by(adm_0_name, Year, UUID, T_res, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  group_by(adm_0_name, Year, start_date, end_date) %>%
  slice_max(total, n = 1, with_ties = FALSE)

# Identify UUIDs to remove
uuids_to_remove_week <- weekly_dups %>%
  group_by(adm_0_name, Year, UUID, T_res, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  anti_join(keep_uuids_week, by = c("adm_0_name", "Year", "UUID", "T_res", "cat"))

# Remove weekly duplicates
od_clean <- od_clean %>%
  left_join(uuids_to_remove_week, by = c("adm_0_name", "Year", "UUID", "T_res", "cat"), suffix = c("", "_remove")) %>%
  filter(
    is.na(start_date) | # Not a UUID to remove
      calendar_start_date < start_date | # Outside removal date range
      calendar_end_date > end_date # Outside removal date range
  ) %>%
  select(-start_date, -end_date, -total)

# 5.3 Yearly duplicates --------------------------------------------------------
yearly_dups <- x %>%
  filter(T_res == "Year")

# Review yearly duplicate totals
yearly_dups %>%
  group_by(adm_0_name, Year, UUID, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  print(n = 23)

# Identify UUIDs with maximum totals to keep
keep_uuids_year <- yearly_dups %>%
  group_by(adm_0_name, Year, UUID, T_res, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  group_by(adm_0_name, Year, start_date, end_date) %>%
  slice_max(total, n = 1, with_ties = FALSE)

# Identify UUIDs to remove
uuids_to_remove_year <- yearly_dups %>%
  group_by(adm_0_name, Year, UUID, T_res, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  anti_join(keep_uuids_year, by = c("adm_0_name", "Year", "UUID", "T_res", "cat"))

# Remove yearly duplicates
od_clean <- od_clean %>%
  left_join(uuids_to_remove_year, by = c("adm_0_name", "Year", "UUID", "T_res", "cat"), suffix = c("", "_remove")) %>%
  filter(
    is.na(start_date) | # Not a UUID to remove
      calendar_start_date < start_date | # Outside removal date range
      calendar_end_date > end_date # Outside removal date range
  ) %>%
  select(-start_date, -end_date, -total)

# 5.4 Manual resolution of remaining duplicates --------------------------------
# Check for any remaining duplicates after systematic removal
x2 <- od_clean %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1)

# Review remaining duplicates
x2 %>%
  group_by(adm_0_name, Year, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  )

unique(x2$T_res)

# Manually exclude specific problematic records
# Afghanistan 2024: Remove EMRO monthly source (prefer ad hoc data)
# Bangladesh 2021: Remove OD monthly data (prefer ad hoc data)
# Maldives 2022: Remove likely digitized plot data
# New Caledonia 2024: Remove partial year ad hoc data (prefer complete OD data)
od_clean <- od_clean[!(od_clean$adm_0_name == "AFGHANISTAN" & od_clean$Year == 2024 & grepl("WHOEMRO-AFG-2024-M01", od_clean$UUID)), ]
od_clean <- od_clean[!(od_clean$adm_0_name == "BANGLADESH" & od_clean$Year == 2021 & od_clean$T_res == "Month" & od_clean$cat == "OD"), ]
od_clean <- od_clean[!(od_clean$adm_0_name == "MALDIVES" & od_clean$Year == 2022 & od_clean$T_res == "Month" & od_clean$UUID == "WHOSEARO-ALL-2022-W02-12"), ]
od_clean <- od_clean[!(od_clean$adm_0_name == "NEW CALEDONIA" & od_clean$Year == 2024 & od_clean$T_res == "Month" & od_clean$calendar_end_date <= "2024-06-30" & od_clean$cat == "ad_hoc_data"), ]

# Verify no duplicates remain
x3 <- od_clean %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1)

stopifnot(nrow(x3) == 0)

# Clean up temporary objects
rm(od, ad, x, x2, x3, keep_uuids_week, keep_uuids_month, keep_uuids_year, monthly_dups, weekly_dups, yearly_dups, uuids_to_remove_month, uuids_to_remove_week, uuids_to_remove_year)

# ------------------------------------------------------------------------------
# 6. SPLIT BY SPATIAL RESOLUTION (ADMIN LEVELS)
# ------------------------------------------------------------------------------

od_adm0 <- od_clean[od_clean$S_res == "Admin0", ]
od_adm1 <- od_clean[od_clean$S_res == "Admin1", ]
od_adm2 <- od_clean[od_clean$S_res == "Admin2", ]

# 6.1 Count records and sum cases by resolution -------------------------------
# Admin0 (national level)
a0_count <- od_adm0 %>%
  group_by(adm_0_name, Year, T_res) %>%
  summarise(
    a0_count = n(),
    a0_sum = sum(dengue_total)
  ) %>%
  ungroup()

# Admin1 (province/state level)
a1_count <- od_adm1 %>%
  group_by(adm_0_name, Year, T_res) %>%
  summarise(
    a1_count = n_distinct(calendar_start_date, calendar_end_date),
    a1_sum = sum(dengue_total)
  )

# Admin2 (district level)
a2_count <- od_adm2 %>%
  group_by(adm_0_name, Year, T_res) %>%
  summarise(
    a2_count = n_distinct(calendar_start_date, calendar_end_date),
    a2_sum = sum(dengue_total)
  )

# 6.2 Create country-year keys ------------------------------------------------
a0_count$country_year <- paste0(a0_count$adm_0_name, "_", a0_count$Year)
a1_count$country_year <- paste0(a1_count$adm_0_name, "_", a1_count$Year)
a2_count$country_year <- paste0(a2_count$adm_0_name, "_", a2_count$Year)

# 6.3 Merge all spatial levels ------------------------------------------------
all_count <- merge(a0_count[, c("country_year", "T_res", "a0_count", "a0_sum")],
  a1_count[, c("country_year", "T_res", "a1_count", "a1_sum")],
  by = c("country_year", "T_res"), all = T
)

all_count <- merge(all_count,
  a2_count[, c("country_year", "T_res", "a2_count", "a2_sum")],
  by = c("country_year", "T_res"), all = T
)

# ------------------------------------------------------------------------------
# 7. SELECT BEST RECORD PER COUNTRY-YEAR
# ------------------------------------------------------------------------------

# 7.1 Apply selection algorithm -----------------------------------------------
# Selects best record per country-year based on:
# - Highest sum (primary criterion)
# - Completeness of sub-annual data
# - Special handling for known high-quality countries (Hong Kong, Taiwan, Japan)

# Build the lookup table
epiweek_tbl <- build_epiweek_lookup(
  start_year = 1990,
  end_year = 2024
)

# Run the selection algorithm with year-specific week counts
result <- select_best_record_multi_new(
  all_count,
  # discrepancy_threshold = 0.2,
  epiweek_tbl = epiweek_tbl
)

selection_outcome <- result$selected
selection_outcome <- create_clean_dataset(selection_outcome)

# 7.2 Classify selection cases ------------------------------------------------
# Analyze which scenario each country-year falls into
case_classification <- all_count %>%
  group_by(country_year) %>%
  summarise(
    has_annual = any(T_res == "Year"),
    has_subannual = any(T_res != "Year"),
    country = str_extract(country_year[1], "^[^_]+"),
    year = as.numeric(str_extract(country_year[1], "\\d{4}")),
    .groups = "drop"
  ) %>%
  mutate(
    case = case_when(
      !has_subannual ~ "Case 1: Annual only",
      !has_annual ~ "Case 2: Sub-annual only",
      has_annual & has_subannual ~ "Case 3: Both",
      TRUE ~ "Other"
    )
  )

# Summary of selection cases
case_classification %>%
  group_by(case) %>%
  summarise(
    count = n(),
    countries = n_distinct(country),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

# 7.3 Analyze annual vs sub-annual relationships ------------------------------
# For Case 3 (both annual and sub-annual available)
relationships <- result$relationships
relationships %>%
  group_by(subannual_is_complete) %>%
  tally()

# Detailed relationship summary
relationship_summary <- result$relationships %>%
  group_by(relationship_category, subannual_is_complete) %>%
  summarise(
    count = n(),
    pct = round(n() / nrow(result$relationships) * 100, 1),
    cys = n_distinct(country_year),
    special_countries = sum(is_special_country),
    cys_true = cys - special_countries,
  ) %>%
  arrange(desc(count))

print(relationship_summary)

# 7.4 Add case labels to selection outcome ------------------------------------
# Label Case 2 (sub-annual only)
case2 <- case_classification$country_year[case_classification$case == "Case 2: Sub-annual only"]
selection_outcome$relationship_category[selection_outcome$country_year %in% case2] <- "Case 2: Sub-annual only"

# Label Case 1 (annual only)
case1 <- case_classification$country_year[case_classification$case == "Case 1: Annual only"]
selection_outcome$relationship_category[selection_outcome$country_year %in% case1] <- "Case 1: Annual only"

# 7.5 Summary statistics -------------------------------------------------------
selection_outcome %>%
  mutate(
    Year = str_extract(country_year, "[^_]+$"),
    adm_0_name = str_extract(country_year, "^[^_]+")
  ) %>%
  group_by(relationship_category) %>%
  summarise(
    count = n(),
    pct = round(n() / nrow(result$relationships) * 100, 1),
    cys = n_distinct(country_year),
    special_countries = sum(adm_0_name %in% c("HONG KONG", "TAIWAN", "JAPAN")),
    subannual_complete = sum(subannual_is_complete),
    cys_true = cys - special_countries
  )

# Summary for complete sub-annual data only
selection_outcome %>%
  mutate(
    Year = str_extract(country_year, "[^_]+$"),
    adm_0_name = str_extract(country_year, "^[^_]+")
  ) %>%
  filter(!grepl("Case", relationship_category)) %>%
  group_by(relationship_category, subannual_is_complete) %>%
  summarise(
    count = n(),
    pct = round(n() / nrow(result$relationships) * 100, 1),
    cys = n_distinct(country_year),
    special_countries = sum(adm_0_name %in% c("HONG KONG", "TAIWAN", "JAPAN")),
    cys_true = cys - special_countries
  ) %>%
  filter(subannual_is_complete)

# 7.6 Validate case 2 selections -----------------------------------------------
# Check Case 2 records (sub-annual only) outside PAHO and AFRO regions
case2_recs <- selection_outcome %>%
  filter(relationship_category == "Case 2: Sub-annual only") %>%
  mutate(
    Year = str_extract(country_year, "[^_]+$"),
    adm_0_name = str_extract(country_year, "^[^_]+")
  ) %>%
  region_class() %>%
  filter(!region %in% c("PAHO", "AFRO")) %>%
  select(country_year, adm_0_name, Year, region, T_res, final_sum)

# Pacific Island Countries - no annual totals available from sources
PIC_names <- od_clean %>%
  filter(grepl("PICs", UUID)) %>%
  distinct(adm_0_name) %>%
  pull()

# WHO WPRO reports consulted (years >2020)
wpro <- case2_recs %>%
  filter(region == "WPRO" & !adm_0_name %in% PIC_names & Year > 2020) %>%
  pull(country_year)

# WHO global dashboard coverage check
who <- read.csv("data/raw_data/who_dengue_global_2025_09_26.csv") %>%
  filter(year(date) < 2025 & !who_region %in% c("EUR", "AMR", "AFR")) %>%
  distinct(country, Year = year(date)) %>%
  mutate(country_year = toupper(paste0(country, "_", Year))) %>%
  pull(country_year)

# Remaining Case 2 records not covered by above checks
case2_recs %>%
  filter(!adm_0_name %in% PIC_names) %>%
  filter(!country_year %in% wpro) %>%
  filter(!adm_0_name %in% c("HONG KONG", "TAIWAN", "OMAN", "PORTUGAL")) %>%
  filter(!country_year %in% who) # Mostly from SEARO dashboard except Maldives 2020

# 7.7 Regional summary ---------------------------------------------------------
# Calculate regional totals with appropriate scaling
selection_outcome %>%
  mutate(
    Year = str_extract(country_year, "[^_]+$"),
    adm_0_name = str_extract(country_year, "^[^_]+"),
    final_sum_scaled = case_when(
      grepl("% of Annual", relationship_category) &
        !adm_0_name %in% c("HONG KONG", "TAIWAN", "JAPAN") ~ annual_total,
      TRUE ~ final_sum
    )
  ) %>%
  region_class() %>%
  group_by(region, Year) %>%
  summarise(total = sum(final_sum_scaled)) %>%
  filter(Year %in% c(1990, 2000, 2010, 2015, 2020) & region %in% c("SEARO"))

# ------------------------------------------------------------------------------
# 8. EXTRACT AND AGGREGATE RECORDS
# ------------------------------------------------------------------------------

# 8.1 Identify special scaling cases ------------------------------------------
# Cases where annual total > complete sub-annual total
# Extract temporal sub-annual data, aggregate spatially, then scale to annual
scaling_cases <- selection_outcome %>%
  filter(
    relationship_category == "Annual > sub-annual",
    subannual_is_complete == TRUE,
    T_res == "Year" # Annual was selected TEMPORALLY
  )

# Get sub-annual information from relationships
if (exists("result") && "relationships" %in% names(result)) {
  scaling_cases <- scaling_cases %>%
    left_join(
      result$relationships %>%
        select(country_year, subannual_column, subannual_type),
      by = "country_year"
    )
} else {
  # If relationships not available, infer from data
  scaling_cases$subannual_column <- NA
  scaling_cases$subannual_type <- NA
}

# 8.2 Separate regular vs scaling selections ----------------------------------
regular_selections <- selection_outcome %>%
  anti_join(scaling_cases %>% select(country_year, T_res), by = c("country_year", "T_res"))

# Initialize final dataset
final_dataset <- data.frame()

# ==============================================================================
# PART 1: REGULAR SELECTIONS (NO SCALING)
# ==============================================================================

# 8.3 Extract Admin0 records (national level) ---------------------------------
regular_a0 <- regular_selections %>% filter(selected_column == "a0")

if (nrow(regular_a0) > 0) {
  match_key_a0 <- paste0(regular_a0$country_year, regular_a0$T_res)
  od_adm0_selected <- od_adm0 %>%
    mutate(match_key = paste0(country_year, T_res)) %>%
    filter(match_key %in% match_key_a0) %>%
    select(-match_key) %>%
    mutate(
      UUID = if ("UUID" %in% names(.)) UUID else NA_character_,
      case_definition_standardised = if ("case_definition_standardised" %in% names(.)) case_definition_standardised else NA_character_,
      cat = if ("cat" %in% names(.)) cat else NA_character_,
      scaled_to_annual = FALSE
    )

  final_dataset <- bind_rows(final_dataset, od_adm0_selected)
}

# 8.4 Extract and aggregate Admin1 records (province level) -------------------
regular_a1 <- regular_selections %>% filter(selected_column == "a1")

if (nrow(regular_a1) > 0) {
  match_key_a1 <- paste0(regular_a1$country_year, regular_a1$T_res)

  od_adm1_selected <- od_adm1 %>%
    mutate(match_key = paste0(country_year, T_res)) %>%
    filter(match_key %in% match_key_a1) %>%
    select(-match_key)

  # Aggregate to national level (sum across provinces)
  od_adm1_aggregated <- od_adm1_selected %>%
    group_by(adm_0_name, calendar_start_date, calendar_end_date, Year, T_res, country_year) %>%
    summarise(
      dengue_total = sum(dengue_total, na.rm = TRUE),
      UUID = if ("UUID" %in% names(pick(everything()))) first(UUID) else NA_character_,
      case_definition_standardised = if ("case_definition_standardised" %in% names(pick(everything()))) first(case_definition_standardised) else NA_character_,
      cat = if ("cat" %in% names(pick(everything()))) first(cat) else NA_character_,
      .groups = "drop"
    ) %>%
    mutate(
      adm_1_name = NA,
      adm_2_name = NA,
      S_res = "Admin0",
      full_name = adm_0_name,
      scaled_to_annual = FALSE
    )

  # Remove extra columns if present
  if ("FAO_GAUL_code" %in% names(od_adm1_aggregated)) {
    od_adm1_aggregated <- od_adm1_aggregated %>% select(-FAO_GAUL_code)
  }
  if ("RNE_iso_code" %in% names(od_adm1_aggregated)) {
    od_adm1_aggregated <- od_adm1_aggregated %>% select(-RNE_iso_code)
  }

  final_dataset <- bind_rows(final_dataset, od_adm1_aggregated)
}

# 8.5 Extract and aggregate Admin2 records (district level) -------------------
regular_a2 <- regular_selections %>% filter(selected_column == "a2")

if (nrow(regular_a2) > 0) {
  match_key_a2 <- paste0(regular_a2$country_year, regular_a2$T_res)

  od_adm2_selected <- od_adm2 %>%
    mutate(match_key = paste0(country_year, T_res)) %>%
    filter(match_key %in% match_key_a2) %>%
    select(-match_key)

  # Aggregate to national level (sum across districts)
  od_adm2_aggregated <- od_adm2_selected %>%
    group_by(adm_0_name, calendar_start_date, calendar_end_date, Year, T_res, country_year) %>%
    summarise(
      dengue_total = sum(dengue_total, na.rm = TRUE),
      UUID = if ("UUID" %in% names(pick(everything()))) first(UUID) else NA_character_,
      case_definition_standardised = if ("case_definition_standardised" %in% names(pick(everything()))) first(case_definition_standardised) else NA_character_,
      cat = if ("cat" %in% names(pick(everything()))) first(cat) else NA_character_,
      .groups = "drop"
    ) %>%
    mutate(
      adm_1_name = NA,
      adm_2_name = NA,
      S_res = "Admin0",
      full_name = adm_0_name,
      scaled_to_annual = FALSE
    )

  # Remove extra columns if present
  if ("FAO_GAUL_code" %in% names(od_adm2_aggregated)) {
    od_adm2_aggregated <- od_adm2_aggregated %>% select(-FAO_GAUL_code)
  }
  if ("RNE_iso_code" %in% names(od_adm2_aggregated)) {
    od_adm2_aggregated <- od_adm2_aggregated %>% select(-RNE_iso_code)
  }

  final_dataset <- bind_rows(final_dataset, od_adm2_aggregated)
}

# ==============================================================================
# PART 2: SPECIAL SCALING CASES
# ==============================================================================
# Extract sub-annual TEMPORAL data (Month/Week), aggregate spatially to
# national level, then scale proportionally to match annual total

scaling_summary <- data.frame()

if (nrow(scaling_cases) > 0) {
  for (i in 1:nrow(scaling_cases)) {
    case <- scaling_cases[i, ]
    cy <- case$country_year
    annual_target <- case$annual_total

    # 8.6 Determine sub-annual data source -------------------------------------
    subannual_spatial_level <- case$subannual_column # a0, a1, or a2
    subannual_temporal_res <- case$subannual_type # Month or Week

    # If not available from relationships, infer from data
    if (is.na(subannual_spatial_level) || is.na(subannual_temporal_res)) {
      # Check all spatial levels for sub-annual temporal data
      a0_subannual <- od_adm0 %>%
        filter(country_year == cy, T_res %in% c("Month", "Week"))

      a1_subannual <- od_adm1 %>%
        filter(country_year == cy, T_res %in% c("Month", "Week"))

      a2_subannual <- od_adm2 %>%
        filter(country_year == cy, T_res %in% c("Month", "Week"))

      # Prioritize by data quality: a0 > a1 > a2
      if (nrow(a0_subannual) > 0) {
        subannual_spatial_level <- "a0"
        subannual_temporal_res <- unique(a0_subannual$T_res)[1]
      } else if (nrow(a1_subannual) > 0) {
        subannual_spatial_level <- "a1"
        subannual_temporal_res <- unique(a1_subannual$T_res)[1]
      } else if (nrow(a2_subannual) > 0) {
        subannual_spatial_level <- "a2"
        subannual_temporal_res <- unique(a2_subannual$T_res)[1]
      } else {
        warning(paste("No sub-annual temporal data found for", cy, "- skipping scaling"))
        next
      }
    }

    # 8.7 Extract sub-annual data at appropriate spatial level ----------------
    if (subannual_spatial_level == "a0") {
      subannual_data <- od_adm0 %>%
        filter(country_year == cy, T_res == subannual_temporal_res)
    } else if (subannual_spatial_level == "a1") {
      subannual_data <- od_adm1 %>%
        filter(country_year == cy, T_res == subannual_temporal_res)
    } else if (subannual_spatial_level == "a2") {
      subannual_data <- od_adm2 %>%
        filter(country_year == cy, T_res == subannual_temporal_res)
    } else {
      warning(paste("Unknown spatial level:", subannual_spatial_level, "for", cy))
      next
    }

    if (nrow(subannual_data) == 0) {
      warning(paste("No sub-annual data found for", cy, subannual_temporal_res, "at", subannual_spatial_level))
      next
    }

    # 8.8 Aggregate spatially to national level -------------------------------
    if (subannual_spatial_level == "a0") {
      # Already at national level, no spatial aggregation needed
      cols_to_keep <- c("adm_0_name", "calendar_start_date", "calendar_end_date", "Year", "T_res", "country_year", "dengue_total")
      if ("UUID" %in% names(subannual_data)) cols_to_keep <- c(cols_to_keep, "UUID")
      if ("case_definition_standardised" %in% names(subannual_data)) cols_to_keep <- c(cols_to_keep, "case_definition_standardised")
      if ("cat" %in% names(subannual_data)) cols_to_keep <- c(cols_to_keep, "cat")

      subannual_aggregated <- subannual_data %>%
        select(all_of(cols_to_keep))
    } else {
      # Aggregate to national level (sum across sub-national units)
      group_cols <- c("adm_0_name", "calendar_start_date", "calendar_end_date", "Year", "T_res", "country_year")

      subannual_aggregated <- subannual_data %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          dengue_total = sum(dengue_total, na.rm = TRUE),
          UUID = if ("UUID" %in% names(pick(everything()))) first(UUID) else NA_character_,
          case_definition_standardised = if ("case_definition_standardised" %in% names(pick(everything()))) first(case_definition_standardised) else NA_character_,
          cat = if ("cat" %in% names(pick(everything()))) first(cat) else NA_character_,
          .groups = "drop"
        )
    }

    # 8.9 Scale sub-annual data to match annual total -------------------------
    current_sum <- sum(subannual_aggregated$dengue_total, na.rm = TRUE)
    scaling_factor <- annual_target / current_sum

    # Apply proportional scaling
    subannual_scaled <- subannual_aggregated %>%
      mutate(
        dengue_total_scaled = dengue_total * scaling_factor,
        dengue_total_floor = floor(dengue_total_scaled),
        fractional_part = dengue_total_scaled - dengue_total_floor
      )

    # Calculate units to distribute to reach exact annual target
    sum_floor <- sum(subannual_scaled$dengue_total_floor)
    units_to_distribute <- round(annual_target - sum_floor)

    # Handle NA cases
    if (is.na(units_to_distribute) || is.na(annual_target)) {
      warning(paste("Cannot calculate units_to_distribute for", cy, "- using floor values without adjustment"))
      subannual_scaled <- subannual_scaled %>%
        mutate(dengue_total = as.integer(dengue_total_floor))
    } else if (units_to_distribute > 0) {
      # Add 1 to records with largest fractional parts
      subannual_scaled <- subannual_scaled %>%
        arrange(desc(fractional_part)) %>%
        mutate(
          adjustment = if_else(row_number() <= units_to_distribute, 1, 0),
          dengue_total = as.integer(dengue_total_floor + adjustment)
        )
    } else if (units_to_distribute < 0) {
      # Subtract 1 from records with smallest fractional parts
      subannual_scaled <- subannual_scaled %>%
        arrange(fractional_part) %>%
        mutate(
          adjustment = if_else(row_number() <= abs(units_to_distribute), -1, 0),
          dengue_total = as.integer(dengue_total_floor + adjustment)
        )
    } else {
      # Perfect match, use floor values
      subannual_scaled <- subannual_scaled %>%
        mutate(dengue_total = as.integer(dengue_total_floor))
    }

    # 8.10 Clean up and standardize format ------------------------------------
    cols_to_remove <- c("dengue_total_scaled", "dengue_total_floor", "fractional_part")
    if ("adjustment" %in% names(subannual_scaled)) {
      cols_to_remove <- c(cols_to_remove, "adjustment")
    }

    subannual_scaled <- subannual_scaled %>%
      select(-all_of(cols_to_remove)) %>%
      mutate(
        adm_1_name = NA,
        adm_2_name = NA,
        S_res = "Admin0",
        full_name = adm_0_name,
        scaled_to_annual = TRUE
      )

    # Remove extra columns if present
    if ("FAO_GAUL_code" %in% names(subannual_scaled)) {
      subannual_scaled <- subannual_scaled %>% select(-FAO_GAUL_code)
    }
    if ("RNE_iso_code" %in% names(subannual_scaled)) {
      subannual_scaled <- subannual_scaled %>% select(-RNE_iso_code)
    }

    # Add to final dataset
    final_dataset <- bind_rows(final_dataset, subannual_scaled)

    # Track scaling operations for verification
    scaling_summary <- bind_rows(scaling_summary, data.frame(
      country_year = cy,
      temporal_res = subannual_temporal_res,
      spatial_level = subannual_spatial_level,
      annual_target = annual_target,
      subannual_sum_before_scaling = current_sum,
      scaling_factor = scaling_factor,
      subannual_sum_after_scaling = sum(subannual_scaled$dengue_total, na.rm = TRUE)
    ))
  }
}

# ------------------------------------------------------------------------------
# 9. FINAL SUMMARY AND VERIFICATION
# ------------------------------------------------------------------------------

cat("\n=== Extraction and Aggregation Complete ===\n")
cat("Total records in final dataset:", nrow(final_dataset), "\n")
cat("Regular selections processed:", nrow(regular_selections), "\n")
cat("Special scaling cases processed:", nrow(scaling_cases), "\n")
if (nrow(final_dataset) > 0) {
  cat("Scaled records in final dataset:", sum(final_dataset$scaled_to_annual, na.rm = TRUE), "\n")
}
cat("\n")

if (nrow(scaling_summary) > 0) {
  cat("=== Scaling Summary ===\n")
  cat("(Temporal sub-annual data scaled to match annual totals)\n\n")
  print(scaling_summary)
  cat("\nVerification: subannual_sum_after_scaling should equal annual_target\n")
  cat("\nScaled records are flagged with scaled_to_annual = TRUE in final_dataset\n")
}

write.csv(final_dataset, "data/processed_data/Best_T_data_V1_3_excl_ad_hoc.csv", row.names = F)
write.csv(selection_outcome, "data/processed_data/selection_outcome_V1_3_excl_ad_hoc.csv", row.names = F)






rm(list = ls())


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
first_year <- read.csv("data/ad_hoc/first_year_summary.csv") %>%
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
  filter(annual_total.x > annual_total.y) # 162

# 2) same total but different resolution
t_res_up <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  # filter(data_source.x == "OD" & data_source.y == "OD")%>%
  filter(annual_total.x == annual_total.y & T_res.x != T_res.y) %>%
  select(country_year, T_res.x, T_res.y, data_source.x) # 84

smaller <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  # filter(data_source.x == "OD" & data_source.y == "OD")%>%
  filter(annual_total.x < annual_total.y) # 1 (japan) + 22 (first year violations)

# Everything remains the same:
same <- comparison_filtered %>%
  filter(data_source.x != "first_year") %>%
  filter(!is.na(annual_total.x) & !is.na(annual_total.y)) %>%
  filter(annual_total.x == annual_total.y & T_res.x == T_res.y) # 2951

# no data in either:
no_data <- comparison_filtered %>%
  filter(is.na(annual_total.x) & is.na(annual_total.y)) # 1170

# first year of dengue introduction
# first_year <- comparison %>%
#   filter(data_source.x == "first_year")
# filter(!is.na(annual_total.x) & !is.na(annual_total.y)) # first year violations

nrow(new_cys) + nrow(higher) + nrow(t_res_up) + nrow(smaller) + nrow(same) + nrow(no_data) + nrow(first_year_new)


sum(comparison$annual_total.x, na.rm = T) - sum(comparison$annual_total.y, na.rm = T) # 1801254

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


# dt_heatmap <- read.csv("data/processed_data/dt_heatmap_calibrated_2025_10_08.csv")
#
# setdiff(coverage_tbl_new$country_year[coverage_tbl_new$cat_model == "Sub_annual_imputation"], dt_heatmap$country_year[dt_heatmap$cat_model == "Sub_annual_imputation"])
