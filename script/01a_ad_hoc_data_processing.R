# ==============================================================================
# AD HOC DATA COMPILATION AND CLEANING SCRIPT
# ==============================================================================
# Purpose: Compile dengue surveillance data from multiple ad hoc sources
# Output: Standardised dataset with duplicates resolved
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP AND LOAD BASE OPENDENGUE DATA
# ------------------------------------------------------------------------------
source("script/00_setup.R") # load libraries and functions
source("functions/fn_check_epiweek_span.R")
source("functions/fn_consecutive_gap_counter.R") # counting the number of consecutive gaps
source("functions/fn_make_week_complete.R") # make weekly data complete
source("functions/fn_OD_region.R") # regional classification

git_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/master-repo-alim/master-repo/data/releases/V1.3/"

# Load base OpenDengue dataset and apply regional classification
od <- readr::read_csv(paste0(git_path, "/Temporal_extract_V1_3_2025_08_01.csv")) %>%
  filter(!adm_0_name == "PITCAIRN") %>%
  region_class()

summary(is.na(od$region))

# ------------------------------------------------------------------------------
# 2. WHO DATA SOURCES
# ------------------------------------------------------------------------------

# 2.1 WHO SEARO Dashboard Data ------------------------------------------------
searo <- read.csv("data/raw_data/SEARO_ad_hoc_2025_06_04.csv") %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name,
    adm_2_name = NA,
    full_name = if_else(is.na(adm_1_name),
      adm_0_name,
      paste0(adm_0_name, ", ", adm_1_name)
    ),
    ISO_A0,
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date,
    calendar_end_date,
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res,
    T_res,
    UUID = "SEARO_ad_hoc_2025_06_04.csv"
  )

# 2.2 WHO Global Dashboard - African Countries --------------------------------
who_afr <- read.csv("data/raw_data/who_dengue_global_2025_09_26.csv") %>%
  filter(who_region == "AFR") %>%
  merge(., od %>% distinct(adm_0_name, ISO_A0), by.x = "iso3", by.y = "ISO_A0", all.x = T) %>%
  rename(dengue_total = cases) %>%
  mutate(
    Year = year(ymd(date)),
    adm_0_name = case_when(
      is.na(adm_0_name) ~ toupper(country),
      TRUE ~ adm_0_name
    )
  )

# Check data summary
who_afr %>%
  filter(Year < 2025) %>%
  group_by(adm_0_name, Year) %>%
  summarise(dengue_total = sum(dengue_total))

# Standardize WHO AFR data to common format
who_afr_toadd <- who_afr %>%
  filter(Year < 2025) %>%
  transmute(
    adm_0_name,
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = adm_0_name,
    ISO_A0 = iso3,
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(date),
    calendar_end_date = as.character(
      ceiling_date(ymd(calendar_start_date), "month") - days(1)
    ),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Month",
    UUID = "who_global_dashboard_2025_09_26.csv"
  )

# ------------------------------------------------------------------------------
# 3. AD HOC DATA BY TEMPORAL RESOLUTION
# ------------------------------------------------------------------------------

# 3.1 Weekly Data --------------------------------------------------------------
ad_weekly <- read.csv("data/ad_hoc/ad_hoc_weekly.csv") %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    Year,
    Week = week,
    dengue_total,
    source_file
  )

# Convert epiweeks to calendar dates
ad_weekly$calendar_end_date <- NA
for (i in 1:nrow(ad_weekly)) {
  ad_weekly$calendar_end_date[i] <- as.character(epiweekToDate(ad_weekly$Year[i], ad_weekly$Week[i])$d1)
}

# Standardize weekly data format
ad_weekly <- ad_weekly %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(ymd(calendar_end_date) - 6),
    calendar_end_date = as.character(calendar_end_date),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Week",
    UUID = source_file
  )

check_epiweek_span(ad_weekly)

# 3.2 Japan Missing Week 53 (2014) --------------------------------------------
# Add missing week 53 for Japan 2014 with assumed 0 cases
jpn <- tibble(
  adm_0_name = "JAPAN",
  adm_1_name = NA_character_,
  adm_2_name = NA_character_,
  full_name = adm_0_name,
  ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
  FAO_GAUL_code = NA,
  RNE_iso_code = NA,
  IBGE_code = NA,
  calendar_start_date = as.character(EpiWeek::epiweekToDate(2014, 53)$d0),
  calendar_end_date = as.character(EpiWeek::epiweekToDate(2014, 53)$d1),
  Year = 2014,
  dengue_total = 0,
  case_definition_standardised = NA,
  S_res = "Admin1",
  T_res = "Week",
  UUID = "assumed 0 cases"
)

# 3.3 Monthly Data -------------------------------------------------------------
ad_monthly <- read.csv("data/ad_hoc/ad_hoc_monthly.csv")

# Fill in 0 cases for European countries (complete all 12 months)
eur <- ad_monthly %>%
  filter(adm_0_name %in% c("SPAIN", "FRANCE", "ITALY")) %>%
  group_by(adm_0_name, Year) %>%
  complete(
    Month = 1:12,
    fill = list(dengue_total = 0)
  ) %>%
  fill(source_file, .direction = "downup") %>%
  ungroup()

# PIC monthly data
pic_monthly <- read.csv("data/ad_hoc/SPEHSIS_monthly.csv") %>%
  mutate(Notes = NA) %>%
  filter(adm_0_name != "Guam")

# Standardize monthly data format
ad_monthly_clean <- ad_monthly %>%
  filter(!adm_0_name %in% c("SPAIN", "FRANCE", "ITALY")) %>%
  rbind(., eur) %>%
  rbind(., pic_monthly) %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(make_date(Year, Month, 1)),
    calendar_end_date = as.character((make_date(Year, Month, 1) + months(1)) - days(1)),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Month",
    UUID = source_file
  )

# 3.4 Yearly Data --------------------------------------------------------------
ad_yearly <- read.csv("data/ad_hoc/ad_hoc_yearly.csv") %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(make_date(Year, 1, 1)),
    calendar_end_date = as.character(make_date(Year, 12, 31)),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Year",
    UUID = source_file
  )

# Exclude Portugal yearly data where weekly data is available
ad_yearly_clean <- ad_yearly %>%
  filter(!(adm_0_name == "PORTUGAL" & Year %in% c(2012, 2013)))

# ------------------------------------------------------------------------------
# 4. AFRICA DATA PROCESSING
# ------------------------------------------------------------------------------

# 4.1 Annual Data --------------------------------------------------------------
annual <- read.csv("data/ad_hoc/Africa_data_annual_AL.csv") %>%
  rename(
    Year = Country,
    adm_0_name = Year
  ) %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(lubridate::make_date(Year, 1, 1)),
    calendar_end_date = as.character(lubridate::make_date(Year, 12, 31)),
    Year,
    dengue_total = Cases,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Year",
    UUID = "Africa_data_annual.csv"
  )

# 4.2 Subannual Data - Multiple Resolutions -----------------------------------
suban <- read.csv("data/ad_hoc/Africa_data_subannual_AL.csv")
erit <- read.csv("data/ad_hoc/Eritrea_subannual.csv")

# Check available temporal resolutions
unique(suban$Resolution) # Weekly, Daily, Monthly, Quarterly

# Prepare subannual data
suban <- suban %>%
  transmute(
    adm_0_name = Country,
    calendar_start_date = as.character(lubridate::dmy(Start.time)),
    calendar_end_date = as.character(lubridate::dmy(End.time)),
    dengue_total = Count,
    Resolution,
    Source
  )

# 4.2.1 Aggregate Daily to Weekly Data ----------------------------------------
suban_daily <- suban %>%
  filter(Resolution == "Daily")

# Convert daily data to epiweeks
suban_daily$Year <- NA
suban_daily$epiweek <- NA

for (i in 1:nrow(suban_daily)) {
  suban_daily$epiweek[i] <- dateToEpiweek(suban_daily$calendar_start_date[i])$weekno
  suban_daily$Year[i] <- dateToEpiweek(suban_daily$calendar_start_date[i])$year
}

# Sum cases by epiweek
suban_daily <- suban_daily %>%
  group_by(adm_0_name, Year, epiweek, Source) %>%
  summarise(dengue_total = sum(dengue_total, na.rm = T), .groups = "drop")

# Convert back to calendar dates
suban_daily$calendar_end_date <- NA

for (i in 1:nrow(suban_daily)) {
  suban_daily$calendar_end_date[i] <- as.character(
    epiweekToDate(
      suban_daily$Year[i],
      suban_daily$epiweek[i]
    )$d1
  )
}

suban_daily$calendar_start_date <- as.character(ymd(suban_daily$calendar_end_date) - 6)
suban_daily$Resolution <- "Week"

# 4.2.2 Aggregate Quarterly to Yearly Data ------------------------------------
suban_quarterly <- suban %>%
  filter(Resolution == "Quarterly")

suban_quarterly <- suban_quarterly %>%
  mutate(
    Year = year(calendar_start_date),
    calendar_start_date = as.character(make_date(Year, 1, 1)),
    calendar_end_date = as.character(make_date(Year, 12, 31))
  ) %>%
  group_by(adm_0_name, Year, calendar_start_date, calendar_end_date, Source) %>%
  summarise(dengue_total = sum(dengue_total), .groups = "drop") %>%
  mutate(Resolution = "Year")

# 4.2.3 Fix Weekly Data Epiweek Mismatches ------------------------------------
suban_weekly <- suban %>%
  filter(Resolution %in% c("Weekly"))

check_epiweek_span(suban_weekly)
print(mismatches)

# Manual inspection notes for each data source:
# Cape Verde: follow calendar_start_date
# Sudan (most): follow calendar_start_date
# Sudan (2010): follow calendar_end_date
# Ethiopia: follow calendar_end_date
# Reunion: follow calendar_end_date

# Assign correct epiweek based on country-specific rules
suban_weekly <- check_epiweek_span(suban_weekly)

suban_weekly <- suban_weekly %>%
  mutate(epiweek = ifelse(
    Year_epiweek_start != Year_epiweek_end & (adm_0_name %in% c("Ethiopia", "Reunion") | (adm_0_name == "Sudan" & Year_start == 2010)), epiweek_end, NA
  )) %>%
  mutate(epiweek = ifelse(
    Year_epiweek_start != Year_epiweek_end & (adm_0_name == "Cape Verde" | (adm_0_name == "Sudan" & Year_start != 2010)), epiweek_start, epiweek
  )) %>%
  mutate(epiweek = ifelse(Year_epiweek_start == Year_epiweek_end, epiweek_start, epiweek))

# Assign correct Year based on country-specific rules
suban_weekly <- suban_weekly %>%
  mutate(Year = ifelse(
    Year_epiweek_start != Year_epiweek_end & (adm_0_name %in% c("Ethiopia", "Reunion") | (adm_0_name == "Sudan" & Year_start == 2010)), Year_end, NA
  )) %>%
  mutate(Year = ifelse(
    Year_epiweek_start != Year_epiweek_end & (adm_0_name == "Cape Verde" | (adm_0_name == "Sudan" & Year_start != 2010)), Year_start, Year
  )) %>%
  mutate(Year = ifelse(Year_epiweek_start == Year_epiweek_end, Year_start, Year))

summary(is.na(suban_weekly$Year))
summary(is.na(suban_weekly$epiweek))

suban_weekly$calendar_start_date2 <- NA
suban_weekly$calendar_end_date2 <- NA

# Reassign calendar dates based on corrected epiweeks
for (i in 1:nrow(suban_weekly)) {
  suban_weekly$calendar_end_date2[i] <- as.character(epiweekToDate(suban_weekly$Year[i], suban_weekly$epiweek[i])$d1)
  suban_weekly$calendar_start_date2[i] <- as.character(epiweekToDate(suban_weekly$Year[i], suban_weekly$epiweek[i])$d0)
}

suban_weekly_clean <- suban_weekly %>%
  transmute(adm_0_name, Year,
    calendar_start_date = calendar_start_date2,
    calendar_end_date = calendar_end_date2,
    dengue_total,
    Resolution = "Week",
    Source
  )

# Verify epiweek corrections
check_epiweek_span(suban_weekly_clean)

# 4.2.4 Eritrea Weekly Data ----------------------------------------------------
erit <- erit %>%
  transmute(
    adm_0_name = Country,
    calendar_start_date = Start.time,
    calendar_end_date = End.time,
    dengue_total = Count,
    Resolution = "Week",
    Source
  )

erit$Year <- NA

for (i in 1:nrow(erit)) {
  erit$Year[i] <- dateToEpiweek(erit$calendar_start_date[i])$year
}

check_epiweek_span(erit)

# 4.3 Combine All Africa Subannual Data ---------------------------------------
suban_new <-
  rbind(
    suban_daily %>%
      select(adm_0_name, Year, calendar_start_date, calendar_end_date, dengue_total, Resolution, Source),
    suban_quarterly %>%
      select(adm_0_name, Year, calendar_start_date, calendar_end_date, dengue_total, Resolution, Source),
    suban_weekly_clean %>%
      select(adm_0_name, Year, calendar_start_date, calendar_end_date, dengue_total, Resolution, Source),
    suban %>%
      filter(Resolution %in% c("Monthly")) %>%
      mutate(
        Year = year(calendar_start_date),
        Resolution = "Month"
      ) %>%
      select(
        adm_0_name, Year,
        calendar_start_date,
        calendar_end_date,
        dengue_total, Resolution, Source
      ),
    erit %>%
      select(adm_0_name, Year, calendar_start_date, calendar_end_date, dengue_total, Resolution, Source)
  )

# Standardize Africa subannual data format
suban_new <- suban_new %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(calendar_start_date),
    calendar_end_date = as.character(calendar_end_date),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = Resolution,
    UUID = Source
  )

# 4.4 Combine All Africa Data --------------------------------------------------
afro_all <- rbind(annual, suban_new)

# Verify row counts match
nrow(afro_all)
sum(
  nrow(annual),
  nrow(suban %>% filter(Resolution %in% c("Monthly"))),
  nrow(suban_daily), nrow(suban_quarterly), nrow(erit), nrow(suban_weekly_clean)
)

# ------------------------------------------------------------------------------
# 5. MERGE ALL AD HOC DATA SOURCES
# ------------------------------------------------------------------------------

ad_hoc <- rbind(
  searo,
  who_afr_toadd,
  jpn,
  ad_weekly,
  ad_monthly_clean,
  ad_yearly_clean,
  afro_all
)

ad_hoc <- ad_hoc %>%
  mutate(cat = "ad_hoc_data")

# ------------------------------------------------------------------------------
# 6. FIRST YEAR OF LOCAL OUTBREAK
# ------------------------------------------------------------------------------

first_year <- read.csv("data/ad_hoc/first_year_details.csv") %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = countrycode(adm_0_name, origin = "country.name", destination = "iso3c"),
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(make_date(Year, 1, 1)),
    calendar_end_date = as.character(make_date(Year, 12, 31)),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Year",
    UUID = source_file,
    cat = "first_year"
  )

ad_hoc <- rbind(ad_hoc, first_year)

# ------------------------------------------------------------------------------
# 7. STANDARDIZE COUNTRY NAMES TO WHO NAMES
# ------------------------------------------------------------------------------

names <- unique(ad_hoc$adm_0_name)
od_names <- unique(od$adm_0_name)
unmatched <- names[!names %in% od_names]

od_countries <- od %>%
  distinct(adm_0_name, ISO_A0)

# Check for countries not in base OpenDengue dataset
all(unique(ad_hoc$ISO_A0) %in% od_countries$ISO_A0)
missing_in_od <- unique(ad_hoc$adm_0_name[ad_hoc$ISO_A0 %in% setdiff(ad_hoc$ISO_A0, od_countries$ISO_A0)])

setdiff(unmatched, missing_in_od)

# Standardize country names to match WHO nomenclature
ad_hoc$adm_0_name[ad_hoc$adm_0_name == "TANZANIA"] <- "UNITED REPUBLIC OF TANZANIA"
ad_hoc$full_name[ad_hoc$full_name == "TANZANIA"] <- "UNITED REPUBLIC OF TANZANIA"

ad_hoc$adm_0_name[ad_hoc$adm_0_name == "CAPE VERDE"] <- "CABO VERDE"
ad_hoc$full_name[ad_hoc$full_name == "CAPE VERDE"] <- "CABO VERDE"

ad_hoc$adm_0_name[ad_hoc$adm_0_name == "FED. STATES OF MICRONESIA"] <- "MICRONESIA (FEDERATED STATES OF)"
ad_hoc$full_name[ad_hoc$full_name == "FED. STATES OF MICRONESIA"] <- "MICRONESIA (FEDERATED STATES OF)"

ad_hoc$adm_0_name[ad_hoc$adm_0_name == "WESTERN SAMOA"] <- "SAMOA"
ad_hoc$full_name[ad_hoc$full_name == "WESTERN SAMOA"] <- "SAMOA"

# ------------------------------------------------------------------------------
# 8. MANUAL DUPLICATE RESOLUTION - COUNTRY-SPECIFIC CASES
# ------------------------------------------------------------------------------

# Identify and review duplicates
ad_hoc %>%
  select(full_name, calendar_start_date, calendar_end_date, dengue_total, UUID) %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1) %>%
  arrange(full_name, calendar_start_date, calendar_end_date) %>%
  group_by(full_name, calendar_start_date, calendar_end_date, dengue_total) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  print(n = 100)

# Resolution notes for specific duplicates:
# Burkina Faso 2016: 2929 (wrong reference) vs 1327 (Oct-Nov) - keep 1327
# Burkina Faso 2023: 146878 (suspected) vs 237291 (confirmed+probable+suspected) - keep 237291
# Cabo Verde 2023: take higher value
# Madagascar 2006: 55 (Jan-Mar sample) vs 2942 (Jan-Mar weekly) - keep 2942
# Reunion 2016: take 221 (local cases only)
# Sao Tome 2022: 1150 (Apr-Dec) vs 1161 (quarterly full year) - keep 1161
# Senegal 2018: 2981 (Sep-Dec) vs 832 (Oct-Nov) - keep 2981
# Sudan 2019: sources conflict, keep 76 (Sep-Dec documented)

# Reunion 2016: Resolve between two data sources
ad_hoc %>%
  filter(adm_0_name == "REUNION" & calendar_start_date >= as.Date("2016-01-10") &
    calendar_end_date <= as.Date("2016-07-02")) %>%
  group_by(UUID) %>%
  summarise(dengue_total = sum(dengue_total))

# Keep pmc.ncbi source for consistency (2015-2018 period)
ad_hoc <- ad_hoc %>%
  filter(!(adm_0_name == "REUNION" & calendar_start_date == "2016-01-01" & dengue_total != 221)) %>%
  filter(!(adm_0_name == "REUNION" & calendar_start_date >= as.Date("2015-12-27") &
    calendar_start_date <= as.Date("2016-07-02") & grepl("https://www.lareunion", UUID)))

# Sudan weekly: Check for digitized duplicates
ad_hoc %>%
  filter(full_name == "SUDAN" & calendar_start_date >= as.Date("2022-07-24") & calendar_end_date <= as.Date("2022-12-24")) %>%
  group_by(UUID) %>%
  summarise(sum = sum(dengue_total))

# Remove Sudan weekly duplicates from EMRO source
ad_hoc <- ad_hoc %>%
  filter(!(full_name == "SUDAN" & calendar_start_date >= as.Date("2022-07-24") & calendar_end_date <= as.Date("2022-12-24") & grepl("https://applications.emro.who.int/", UUID)))

# Remove Burkina Faso 2016 incorrect reference
ad_hoc <- ad_hoc %>%
  filter(!(full_name == "BURKINA FASO" & dengue_total == 2929))

# ------------------------------------------------------------------------------
# 9. FILL IN ZEROS OUTSIDE OUTBREAK WINDOWS
# ------------------------------------------------------------------------------

outbreak <- read.csv("data/ad_hoc/check_outbreak.csv") %>%
  mutate(
    across(
      c(zero_end_date, zero_start_date),
      ~ na_if(.x, "")
    )
  ) %>%
  filter(!is.na(zero_end_date) | !is.na(zero_start_date)) %>%
  filter(!country_year == "SUDAN_2014")

outbreak <- outbreak %>%
  select(region, country_year, UUID, Year, zero_end_date, zero_start_date) %>%
  mutate(cat = ifelse(!is.na(zero_end_date) & is.na(zero_start_date), "leading_zeros",
    ifelse(is.na(zero_end_date) & !is.na(zero_start_date), "trailing_zeros", NA)
  ))

# Determine start/end dates for zero-filling
for (i in seq_len(nrow(outbreak))) {
  # Leading zeros: from week 1 to outbreak start
  if (outbreak$cat[i] == "leading_zeros") {
    outbreak$zero_start_date[i] <-
      as.character(EpiWeek::epiweekToDate(as.integer(outbreak$Year[i]), 1)$d0)
  }

  # Trailing zeros: from outbreak end to week 52
  if (outbreak$cat[i] == "trailing_zeros") {
    outbreak$zero_end_date[i] <-
      as.character(EpiWeek::epiweekToDate(as.integer(outbreak$Year[i]), 52)$d1)
  }
}

# Generate weekly zero records for non-outbreak periods
outbreak_clean <- outbreak %>%
  mutate(
    calendar_start_date = map2(
      ymd(zero_start_date), ymd(zero_end_date) - 1,
      ~ seq(.x, .y, by = "1 week")
    )
  ) %>%
  unnest(calendar_start_date) %>%
  mutate(
    calendar_end_date = calendar_start_date + 6,
    dengue_total = 0
  ) %>%
  transmute(
    adm_0_name = str_remove(country_year, "_\\d+$"),
    adm_1_name = NA_character_,
    adm_2_name = NA_character_,
    full_name = adm_0_name,
    ISO_A0 = countrycode(adm_0_name, "country.name", "iso3c"),
    FAO_GAUL_code = NA, RNE_iso_code = NA, IBGE_code = NA,
    calendar_start_date = as.character(calendar_start_date),
    calendar_end_date = as.character(calendar_end_date),
    Year = as.integer(Year),
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Week",
    UUID = "Outbreak_assumption",
    cat = "OD",
    region,
    country_year
  ) %>%
  select(names(ad_hoc))

# Remove any duplicate zero records
outbreak_clean <- outbreak_clean[!duplicated(outbreak_clean[c("adm_0_name", "calendar_start_date", "calendar_end_date")]), ]

ad_hoc <- rbind(ad_hoc, outbreak_clean)

# ------------------------------------------------------------------------------
# 10. WHO DENGUE EXPLORER DATA
# ------------------------------------------------------------------------------

de <- read.csv("C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/OpenDengue-Dev/open_dengue_1.3/source_files/original_name/dengue_explorer_all_countries.csv")

# Filter to WPRO and SEARO regions only
de <- de %>%
  filter(WHO_Region %in% c("WPRO", "SEARO")) %>%
  transmute(
    adm_0_name = Country,
    Year,
    dengue_total = Cases
  ) %>%
  filter(!is.na(dengue_total))

# Handle Netherlands Antilles: expand to constituent territories
na_rows <- de %>%
  filter(adm_0_name == "Netherlands Antilles" & dengue_total == 0)

territories <- c("Curacao", "Sint Maarten", "Bonaire, Saint Eustatius and Saba")

expanded_rows <- na_rows %>%
  slice(rep(1:n(), each = length(territories))) %>%
  mutate(
    adm_0_name = rep(territories, times = nrow(na_rows)),
    dengue_total = 0
  )

de <- de %>%
  filter(!(adm_0_name == "Netherlands Antilles")) %>%
  bind_rows(expanded_rows)

# Add ISO codes
de$ISO_A0 <- countrycode::countrycode(
  sourcevar = de$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)

# Standardize country names to match OpenDengue nomenclature
de <- de %>%
  merge(., od %>% distinct(adm_0_name, ISO_A0),
    by = "ISO_A0", all.x = T
  ) %>%
  filter(!is.na(adm_0_name.y)) %>%
  rename(adm_0_name = adm_0_name.y)

# Remove non-endemic countries with primarily imported cases
de <- de %>%
  filter(!adm_0_name %in% c("CHINA", "JAPAN", "SPAIN", "FRANCE", "ITALY"))

# Standardize to common format
de <- de %>%
  transmute(
    adm_0_name = toupper(adm_0_name),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = paste0(adm_0_name),
    ISO_A0 = ISO_A0,
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(make_date(Year, 1, 1)),
    calendar_end_date = as.character(make_date(Year, 12, 31)),
    Year,
    dengue_total,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Year",
    UUID = "WHO_dengue_explorer",
    cat = "ad_hoc_data"
  )

ad_hoc <- rbind(ad_hoc, de)

# ------------------------------------------------------------------------------
# 11. WHO GLOBAL DENGUE DASHBOARD (SEAR/WPR/EMR REGIONS)
# ------------------------------------------------------------------------------

who <- read.csv("data/raw_data/who_dengue_global_2025_09_26.csv") %>%
  filter(year(date) < 2025) %>%
  filter(who_region %in% c("SEAR", "WPR", "EMR")) %>%
  merge(., od %>% distinct(adm_0_name, ISO_A0), by.x = "iso3", by.y = "ISO_A0", all.x = T) %>%
  transmute(
    adm_0_name = case_when(
      is.na(adm_0_name) ~ toupper(country),
      TRUE ~ adm_0_name
    ),
    adm_1_name = NA,
    adm_2_name = NA,
    full_name = adm_0_name,
    ISO_A0 = iso3,
    FAO_GAUL_code = NA,
    RNE_iso_code = NA,
    IBGE_code = NA,
    calendar_start_date = as.character(date),
    calendar_end_date = as.character(
      ceiling_date(ymd(calendar_start_date), "month") - days(1)
    ),
    Year = year(ymd(date)),
    dengue_total = cases,
    case_definition_standardised = NA,
    S_res = "Admin0",
    T_res = "Month",
    UUID = "who_global_dashboard_2025_09_26.csv",
    cat = "ad_hoc_data"
  )

ad_hoc <- rbind(ad_hoc, who)

# ------------------------------------------------------------------------------
# 12. SYSTEMATIC DUPLICATE HANDLING
# ------------------------------------------------------------------------------

# Identify all remaining duplicates
x1 <- ad_hoc %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1)

# 12.1 Monthly Duplicates ------------------------------------------------------
monthly_dups <- x1 %>%
  filter(T_res == "Month")

# Review monthly duplicates by UUID
monthly_dups %>%
  group_by(adm_0_name, Year, UUID, cat) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop",
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
ad_clean <- ad_hoc %>%
  left_join(uuids_to_remove_month, by = c("adm_0_name", "Year", "UUID", "T_res"), suffix = c("", "_remove")) %>%
  filter(
    is.na(start_date) | # Not a UUID to remove
      calendar_start_date < start_date | # Outside removal date range
      calendar_end_date > end_date # Outside removal date range
  ) %>%
  select(-start_date, -end_date, -total)

# 12.2 Weekly Duplicates -------------------------------------------------------
weekly_dups <- x1 %>%
  filter(T_res == "Week")

# Review weekly duplicates
weekly_dups %>%
  group_by(adm_0_name, Year, UUID) %>%
  summarise(
    start_date = min(calendar_start_date),
    end_date = max(calendar_end_date),
    total = sum(dengue_total), .groups = "drop"
  ) %>%
  print(n = 23)

# For weekly duplicates, keep record with maximum dengue_total
uuids_to_remove_week <- weekly_dups %>%
  group_by(adm_0_name, calendar_start_date, calendar_end_date) %>%
  slice_min(dengue_total, n = 1, with_ties = FALSE)

# Remove weekly duplicates
ad_clean <- ad_clean %>%
  anti_join(uuids_to_remove_week, by = c("adm_0_name", "calendar_start_date", "calendar_end_date", "UUID"))

# 12.3 Yearly Duplicates -------------------------------------------------------
yearly_dups <- x1 %>%
  filter(T_res == "Year")

# Keep records with maximum dengue_total for each year
keep_uuids_year <- yearly_dups %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  slice_max(dengue_total, n = 1, with_ties = FALSE) %>%
  ungroup()

# Remove yearly duplicates not in keep list
ad_clean <- ad_clean %>%
  anti_join(
    yearly_dups %>%
      distinct(adm_0_name, Year, UUID, T_res, cat, dengue_total) %>%
      anti_join(keep_uuids_year, by = c("adm_0_name", "Year", "UUID", "T_res", "cat", "dengue_total")),
    by = c("adm_0_name", "Year", "UUID", "T_res", "cat", "dengue_total")
  )

# Verify no duplicates remain
x2 <- ad_clean %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1)

stopifnot(nrow(x2) == 0)

# ------------------------------------------------------------------------------
# 13. FINAL DATA CLEANING AND EXPORT
# ------------------------------------------------------------------------------

# Final duplicate check (should return 0 rows)
ad_clean %>%
  group_by(full_name, calendar_start_date, calendar_end_date) %>%
  filter(n() > 1) %>%
  group_by(adm_0_name, calendar_start_date, calendar_end_date, UUID) %>%
  summarise(dengue_total = sum(dengue_total))

# Review data quality
unique(ad_clean$adm_0_name)
unique(ad_clean$T_res)
unique(ad_clean$ISO_A0)
summary(is.na(ad_clean$ISO_A0))

# Remove Pitcairn Islands (if present)
ad_clean <- ad_clean %>%
  filter(!grepl("PITCAIRN", adm_0_name))

# Verify country name consistency (should return 0 rows)
ad_clean %>%
  distinct(ISO_A0, adm_0_name) %>%
  group_by(ISO_A0) %>%
  filter(n() > 1)

# Export final cleaned dataset
write.csv(ad_clean, "data/processed_data/ad_hoc_data_all_2025_10_22.csv", row.names = F)
