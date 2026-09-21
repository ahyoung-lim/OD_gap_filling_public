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

# >>> SUB-ANNUAL SCALING SENSITIVITY (0% = no scale, 100% = scale all).
#   Two controls, each routing outputs to a SEPARATE folder so they never mix:
#   - EXCLUDE_SUBANNUAL_SCALING=true  -> 0% (no scale): scale NOTHING.
#   - SCALE_MIN_COMPLETENESS=<0-100>  -> scale only cases whose sub-annual completeness
#       (pct_of_annual) is >= this value; drop the rest (they become model-disaggregated).
#       0 (default) = scale all = 100%.  e.g. 50 = scale only >=50% complete.
#   Usage:  SCALE_MIN_COMPLETENESS=50 Rscript script/01b_select_best_record.R
EXCLUDE_SUBANNUAL_SCALING <- tolower(Sys.getenv("EXCLUDE_SUBANNUAL_SCALING", "false")) %in% c("true", "1", "yes")
SCALE_MIN_COMPLETENESS <- suppressWarnings(as.numeric(Sys.getenv("SCALE_MIN_COMPLETENESS", "0")))
if (is.na(SCALE_MIN_COMPLETENESS)) SCALE_MIN_COMPLETENESS <- 0
# tag drives the output folder: "" (default 100%), "noscale" (0%), or "thr<pct>" (cutoff)
SENS_TAG <- if (EXCLUDE_SUBANNUAL_SCALING) "noscale" else if (SCALE_MIN_COMPLETENESS > 0) sprintf("thr%g", SCALE_MIN_COMPLETENESS) else ""
psens <- function(path) {
  if (identical(SENS_TAG, "")) {
    return(path)
  }
  d <- file.path("data", "sensitivity", "sens01_scaling_sensitivity", SENS_TAG)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, basename(path))
}

# >>> SENSITIVITY when TRUE, do NOT merge ad hoc data (OD sources only). Drives the
#   ad hoc impact comparison in 02b_ad_hoc_impact.R, so the ONLY difference between the
#   two runs is ad hoc presence (same 01b logic, no stale copy).
#   Usage:  EXCLUDE_AD_HOC=true Rscript script/01b_select_best_record.R
EXCLUDE_AD_HOC <- tolower(Sys.getenv("EXCLUDE_AD_HOC", "false")) %in% c("true", "1", "yes")
# excl-ad-hoc outputs get an "_excl_ad_hoc" suffix (same folder) so they never overwrite
# the default (ad hoc included) files
pah <- function(path) {
  if (!EXCLUDE_AD_HOC) {
    return(path)
  }
  sub("\\.csv$", "_excl_ad_hoc.csv", path)
}

# 1.1 Load OpenDengue temporal extract ----------------------------------------
# OD_RELEASE_DIR is set in 00_setup.R
od <- readr::read_csv(file.path(OD_RELEASE_DIR, "Temporal_extract_V1_3_2026_07_29.csv")) %>%
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
# Combine OpenDengue and ad hoc data with category labels.
# >>> SENSITIVITY EXCLUDE_AD_HOC=true skips the merge (OD only) for the ad hoc
#     impact comparison (02a runs this script both ways, 02b compares them).
if (!EXCLUDE_AD_HOC) {
  ad <- read.csv("data/processed_data/ad_hoc_data_all.csv")
  od <- rbind(
    od %>% mutate(cat = "OD"),
    ad
  )
} else {
  message(">>> EXCLUDE_AD_HOC: ad hoc data NOT merged (OD sources only)")
  od <- od %>% mutate(cat = "OD")
}

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
      UUID != "LITERATURE-OMN-2019-Y01-00"
  ) %>%
  filter(!to_remove) %>%
  select(-to_remove)

# 4.3 Correct implausible annual total -----------------------------------------
# Nepal 2020: the WHO SEARO annual figure (WHOSEARO-ALL-2021, 10,808 cases) is an
# outlier that conflicts with EDCD and multiple peer-reviewed sources reporting ~530
# cases for 2020 (a COVID-suppressed year; cf. 2019 = 17,992, 2021 = 540, 2022 = 54,784).
# Left in, its "highest sum wins" selection scales the (correct) monthly series up ~27x
# to an implausible total. Drop it so the lower literature annual (630, close to EDCD's
# 530) is selected instead.
od_clean <- od_clean %>%
  filter(!(adm_0_name == "NEPAL" & Year == 2020 & T_res == "Year" &
    UUID == "WHOSEARO-ALL-2021-Y01-00"))

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
if (exists("ad")) rm(ad) # not created when EXCLUDE_AD_HOC
rm(od, x, x2, x3, keep_uuids_week, keep_uuids_month, keep_uuids_year, monthly_dups, weekly_dups, yearly_dups, uuids_to_remove_month, uuids_to_remove_week, uuids_to_remove_year)

# ------------------------------------------------------------------------------
# 6. SPLIT BY SPATIAL RESOLUTION (ADMIN LEVELS)
# ------------------------------------------------------------------------------

# 6.0 Combine mixed Admin1 + Admin2 reporting into the national total -------------
# Some MOH sources report a country-year at MIXED spatial resolution within a single
# source (same UUID + T_res): cases with a known department but an UNKNOWN municipality
# are coded at Admin1 (e.g. "... SIN MUNICIPIO"), while cases with a known municipality
# are at Admin2. These are COMPLEMENTARY (different cases), so the national total =
# Admin1 + Admin2. The default "pick the single highest level" selection keeps only one
# and undercounts. For the confirmed countries below we relabel their Admin2 rows to
# Admin1 so the Admin1 aggregation sums both levels into the national total (this avoids
# creating a new Admin0 record that would collide with an existing national source).
#
# Confirmed to SUM (Admin1 = unknown-sub-geography, complementary):
#   COLOMBIA, PANAMA, PERU
# Checked and NOT summed (left as-is):
#   - PHILIPPINES: Admin1 is NOT complementary here (verified separately) -> keep pick-one
#   - BHUTAN / MYANMAR / NEPAL: Admin0 + Admin1 where Admin0 ≈ sum(Admin1) = national
#     rollup (redundant; already handled by picking one level)
#   - TAIWAN: the Admin0 records are zero placeholders (no effect)
# Checked and NOT summed — tracked per (country, UUID) below, so a NEW source (even for a
# country already reviewed) re-triggers the sanity check instead of being silently excluded:
#   - PHILIPPINES / TYCHO-ALL-19242017-SV_DF01-00: Admin1 is NOT complementary (verified)
# (Other mixed patterns are not Admin1+Admin2 and never reach this check: BHUTAN/MYANMAR/
#  NEPAL are Admin0+Admin1 rollups where Admin0 ≈ sum(Admin1); TAIWAN's Admin0 rows are 0.)
combine_a1a2_countries <- c("COLOMBIA", "PANAMA", "PERU")

# Mixed Admin1+Admin2 sources reviewed and confirmed NOT to combine (keep pick-one),
# tracked at (country, UUID) so a new source triggers review rather than silent exclusion.
reviewed_no_combine <- data.frame(
  adm_0_name = "PHILIPPINES",
  UUID = "TYCHO-ALL-19242017-SV_DF01-00",
  stringsAsFactors = FALSE
)

# (country, Year, T_res, UUID) groups that report BOTH Admin1 and Admin2 in one source
mixed_a1a2 <- od_clean %>%
  filter(S_res %in% c("Admin1", "Admin2")) %>%
  distinct(adm_0_name, Year, T_res, UUID, S_res) %>%
  count(adm_0_name, Year, T_res, UUID, name = "n_levels") %>%
  filter(n_levels > 1)

# SANITY CHECK: stop on any mixed Admin1+Admin2 source that is neither confirmed-to-combine
# (its country in combine_a1a2_countries) nor already reviewed-and-excluded (its exact
# country+UUID in reviewed_no_combine). Catches new countries AND new sources for a country
# already reviewed (e.g. a new PHILIPPINES UUID) in future data updates.
unreviewed <- mixed_a1a2 %>%
  filter(!(adm_0_name %in% combine_a1a2_countries)) %>%
  anti_join(reviewed_no_combine, by = c("adm_0_name", "UUID"))
if (nrow(unreviewed) > 0) {
  stop(
    "Unreviewed mixed Admin1+Admin2 source(s): ",
    paste(unique(paste(unreviewed$adm_0_name, unreviewed$UUID, sep = " / ")), collapse = "; "),
    ". Review each: complementary -> add its country to combine_a1a2_countries; ",
    "rollup/duplicate -> add its (adm_0_name, UUID) to reviewed_no_combine."
  )
}

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

# 8.1b Report the magnitude of the sub-annual -> annual scale-up ---------------
# Each scaling case stretches a COMPLETE sub-annual series up to its annual total. The
# scale factor = annual / sub-annual sum (= 100 / pct_of_annual). A large factor means the
# sub-annual source captured only a small fraction of the annual, so its seasonal SHAPE is
# imposed on a much larger total (e.g. Brazil 1993-2000 used Tycho's Alagoas alone, 33-360x).
# This block quantifies how many cases are scaled and flags the extreme ones for review.
if (nrow(scaling_cases) > 0 && "pct_of_annual" %in% names(scaling_cases)) {
  scaling_report <- scaling_cases %>%
    mutate(scale_factor = round(100 / pct_of_annual, 1)) %>%
    arrange(desc(annual_total), desc(scale_factor)) %>%
    # filter(pct_of_annual < 90)%>%
    select(country_year, pct_of_annual, scale_factor, annual_total)

  scaling_report_print <- scaling_cases %>%
    mutate(scale_factor = round(100 / pct_of_annual, 1)) %>%
    arrange(desc(scale_factor)) %>%
    filter(pct_of_annual < 90) %>%
    select(country_year, pct_of_annual, scale_factor, annual_total)

  cat(sprintf("\n=== Sub-annual -> annual scale-up: %d country-years ===\n", nrow(scaling_report)))
  cat(sprintf(
    "  sub-annual >=90%% of annual (factor ~1, negligible): %d | <90%%: %d | factor >=5x (extreme): %d\n",
    sum(scaling_report$pct_of_annual >= 90, na.rm = TRUE),
    sum(scaling_report$pct_of_annual < 90, na.rm = TRUE),
    sum(scaling_report$scale_factor >= 5, na.rm = TRUE)
  ))
  cat("  largest scale-ups (small, potentially unrepresentative sub-annual stretched to a large annual):\n")
  print(utils::head(as.data.frame(scaling_report_print), 20), row.names = FALSE)
}

# >>> SENSITIVITY: drop (some or all) sub-annual scaling. Removing a case from scaling_cases
#   makes that country-year fall through to the regular path (its selected Year record ->
#   disaggregation) and skips PART 2 below. The 8.1b report above still prints the full count.
if (EXCLUDE_SUBANNUAL_SCALING) {
  # 0% (no scale): scale nothing
  cat(sprintf(
    ">>> 0%% MODEL: excluding sub-annual scaling for %d country-years (treated as annual-only)\n",
    nrow(scaling_cases)
  ))
  scaling_cases <- scaling_cases[0, , drop = FALSE]
} else if (SCALE_MIN_COMPLETENESS > 0 && "pct_of_annual" %in% names(scaling_cases)) {
  # completeness cutoff: scale only cases with pct_of_annual >= threshold; drop the rest
  n0 <- nrow(scaling_cases)
  scaling_cases <- scaling_cases[!is.na(scaling_cases$pct_of_annual) &
                                   scaling_cases$pct_of_annual >= SCALE_MIN_COMPLETENESS, , drop = FALSE]
  cat(sprintf(
    ">>> COMPLETENESS CUTOFF %g%%: scaling %d of %d cases; dropped %d with completeness < %g%% (treated as annual-only)\n",
    SCALE_MIN_COMPLETENESS, nrow(scaling_cases), n0, n0 - nrow(scaling_cases), SCALE_MIN_COMPLETENESS
  ))
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

write.csv(final_dataset, psens(pah("data/processed_data/Best_T_data_V1_3.csv")), row.names = F)
write.csv(selection_outcome, psens(pah("data/processed_data/selection_outcome_V1_3.csv")), row.names = F)
