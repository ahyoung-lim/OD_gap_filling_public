# ==============================================================================
# MODEL DATA PREPARATION SCRIPT
# ==============================================================================
# Purpose: Prepare complete time series data for statistical modeling
# Process: 1) Load calibrated data and coverage metadata
#          2) Process weekly data (complete time series)
#          3) Process monthly data (aggregate weekly + monthly)
#          4) Process yearly data (expand to monthly)
#          5) Add population and geographic variables
#          6) Export training and prediction datasets
# Output: Model-ready datasets for CV and prediction
# ==============================================================================

source("script/00_setup.R")
source("functions/fn_consecutive_gap_counter.R") # counting the number of consecutive gaps
source("functions/fn_make_week_complete.R") # make weekly data complete
source("functions/fn_Year_checker.R") # adjusting Year column
source("functions/fn_OD_region.R") # regional classification

# ------------------------------------------------------------------------------
# HELPER FUNCTION
# ------------------------------------------------------------------------------

add_country_year <- function(df) {
  df %>% mutate(country_year = paste0(adm_0_name, "_", Year))
}

# ------------------------------------------------------------------------------
# 1. LOAD CALIBRATED DATA
# ------------------------------------------------------------------------------

# Load coverage assessment table with modelling categories
tab <- read.csv("data/processed_data/dt_heatmap_calibrated_2025_10_08.csv")

# Load calibrated temporal extract
T_data <- read.csv("data/processed_data/Best_T_data_calibrated_V1_3_2025_10_08.csv")
summary(is.na(T_data))

# 1.1 Data Summary Statistics --------------------------------------------------
# Temporal resolution distribution
tab %>%
  group_by(T_res) %>%
  tally()

# Modelling category distribution
tab %>%
  group_by(cat_model) %>%
  tally()

# 1 Annual_disaggregation  1537
# 2 No_modelling_required  3300
# 3 Sub_annual_imputation   168

# No modelling required by temporal resolution
tab %>%
  filter(cat_model == "No_modelling_required") %>%
  group_by(T_res) %>%
  tally()

# Country count by WHO region
tab %>%
  distinct(adm_0_name, region) %>%
  group_by(region) %>%
  tally()


# ------------------------------------------------------------------------------
# 2. PROCESS WEEKLY DATA
# ------------------------------------------------------------------------------

# 2.1 Extract Weekly Records ---------------------------------------------------
dt_w <- T_data %>%
  filter(T_res == "Week") %>%
  region_class()

unique(dt_w$adm_0_name) # 96 countries



# 2.2 Complete Time Series (Fill Date Gaps) -----------------------------------
# Ensure all countries have consistent start/end dates with complete weekly sequence
dt_w_complete <- make_week_complete_clean(data = dt_w, keep_vars = TRUE)

# 2.3 Diagnostic Time Series Plot ----------------------------------------------
dt_w_complete %>%
  ggplot() +
  geom_point(
    aes(x = as.Date(calendar_start_date), y = dengue_total, group = 1),
    size = 0.7
  ) +
  geom_line(
    aes(x = as.Date(calendar_start_date), y = dengue_total, group = 1),
    linewidth = 0.3
  ) +
  facet_wrap(adm_0_name ~ ., scales = "free_y")

# 2.4 Validate Time Sequence Consistency ---------------------------------------
# Check if same time_seq assigned for same calendar_start_date across countries
consistent_time_seq_check <- dt_w_complete %>%
  group_by(calendar_start_date) %>%
  summarize(is_consistent = n_distinct(time_seq) == 1) %>%
  ungroup()

# Identify inconsistent dates
inconsistent_dates <- consistent_time_seq_check %>%
  filter(!is_consistent)

if (nrow(inconsistent_dates) != 0) {
  message(
    "Some calendar_start_date have inconsistent time_seq across countries."
  )
}

# Verify one-to-one mapping between dates and time_seq
stopifnot(length(unique(dt_w_complete$calendar_start_date)) ==
  length(unique(dt_w_complete$time_seq)))

# 2.5 Calculate Missing Data Proportion ----------------------------------------
n_gap <- nrow(dt_w_complete[is.na(dt_w_complete$dengue_total), ])
n_total <- nrow(dt_w_complete)
test_ratio <- n_gap / n_total * 100 # 9.4

# ------------------------------------------------------------------------------
# 3. PROCESS MONTHLY DATA
# ------------------------------------------------------------------------------

# 3.1 Extract Monthly Records --------------------------------------------------
dt_m <- T_data %>%
  filter(T_res == "Month") %>%
  mutate(month = month(calendar_start_date))

# 3.2 Aggregate Weekly to Monthly ----------------------------------------------
# Convert weekly data to monthly for countries without direct monthly data
dt_w_m <- dt_w_complete %>%
  mutate(month = as.integer(month(calendar_start_date))) %>%
  # Handle weeks spanning year boundaries
  mutate(
    month = case_when(
      year(as.Date(calendar_end_date)) != year(as.Date(calendar_start_date)) &
        year(as.Date(calendar_end_date)) == Year ~ 1,
      TRUE ~ month
    )
  ) %>%
  group_by(adm_0_name, ISO_A0, Year, month) %>%
  summarise(dengue_total = sum(dengue_total, na.rm = F))

# 3.3 Combine Direct Monthly + Aggregated Weekly ------------------------------
dt_m <- rbind(
  dt_m[, c("adm_0_name", "ISO_A0", "Year", "month", "dengue_total")],
  dt_w_m
) %>%
  mutate(calendar_start_date = as.character(make_date(Year, month, 1))) %>%
  add_country_year() %>%
  region_class()

unique(dt_m$adm_0_name) # 136 countries
length(unique(dt_m$country_year)) # 1158

# 3.4 Check for Duplicates (should be 0) ---------------------------------------
dt_m %>%
  group_by(adm_0_name, Year, month) %>%
  tally() %>%
  filter(n > 1)

# 3.5 Complete Time Series (Fill Date Gaps) -----------------------------------
dt_m_complete <- make_month_complete_clean(data = dt_m %>% select(-month), keep_vars = TRUE)

# 3.6 Validate Time Sequence Consistency ---------------------------------------
consistent_time_seq_check <- dt_m_complete %>%
  group_by(calendar_start_date) %>%
  summarize(is_consistent = n_distinct(time_seq) == 1) %>%
  ungroup()

inconsistent_dates <- consistent_time_seq_check %>%
  filter(!is_consistent)

if (nrow(inconsistent_dates) != 0) {
  message(
    "Some calendar_start_date have inconsistent time_seq across countries."
  )
}

stopifnot(length(unique(dt_m_complete$calendar_start_date)) ==
  length(unique(dt_m_complete$time_seq)))

# 3.7 Calculate Missing Data Proportion ----------------------------------------
n_gap <- nrow(dt_m_complete[is.na(dt_m_complete$dengue_total), ])
n_total <- nrow(dt_m_complete)
test_ratio <- n_gap / n_total * 100 # 9.4

# Clean up temporary objects
rm(dt_w_m, consistent_time_seq_check, inconsistent_dates)

# 3.9 Verify Coverage Table Consistency ----------------------------------------
# All sub-annual records in coverage table should match dt_m
stopifnot(
  nrow(tab[tab$T_res != "Year", ]) ==
    length(unique(dt_m$country_year))
)

sub_in_tab <- unique(dt_m$country_year)
sub_in_t_data <- unique(tab$country_year[tab$T_res != "Year"])

setdiff(sub_in_tab, sub_in_t_data)

# ------------------------------------------------------------------------------
# 4. PROCESS YEARLY DATA (FOR DISAGGREGATION)
# ------------------------------------------------------------------------------

# 4.1 Extract Yearly Records ---------------------------------------------------
dt_y <- T_data %>%
  filter(T_res == "Year")

# 4.2 Expand Annual to Monthly -------------------------------------------------
# Create 12 monthly records for each annual record (for disaggregation modeling)
dt_y_expand <- dt_y %>%
  rowwise() %>%
  mutate(
    # Create list of monthly start dates
    month_starts = list(seq(
      from = as.Date(calendar_start_date),
      to = as.Date(calendar_end_date),
      by = "month"
    ))
  ) %>%
  unnest(month_starts) %>%
  mutate(
    calendar_start_date = month_starts,
    calendar_end_date = month_starts %m+% months(1) - days(1), # End of each month
    annual_total = dengue_total,
    dengue_total = NA, # Monthly values to be imputed
  ) %>%
  select(-month_starts) %>%
  mutate(month = month(calendar_start_date)) %>%
  add_country_year()

unique(dt_y_expand$adm_0_name) # 143 countries
length(unique(dt_y_expand$country_year)) # 3847

# 4.3 Complete Time Series (add time_seq column) --------------------------------
dt_y_complete <- make_month_complete_clean(data = dt_y_expand %>% select(-month), keep_vars = TRUE)

# 4.4 Validate Time Sequence Consistency ---------------------------------------
consistent_time_seq_check <- dt_y_complete %>%
  group_by(calendar_start_date) %>%
  summarize(is_consistent = n_distinct(time_seq) == 1) %>%
  ungroup()

inconsistent_dates <- consistent_time_seq_check %>%
  filter(!is_consistent)

if (nrow(inconsistent_dates) != 0) {
  message(
    "Some calendar_start_date have inconsistent time_seq across countries."
  )
}

stopifnot(length(unique(dt_y_complete$calendar_start_date)) ==
  length(unique(dt_y_complete$time_seq)))

# ------------------------------------------------------------------------------
# 5. PREPARE MODEL INPUT DATASETS
# ------------------------------------------------------------------------------

# 5.1 Weekly Data --------------------------------------------------------------
data_w <- dt_w_complete %>%
  select(
    adm_0_name,
    dengue_total,
    calendar_start_date,
    Year,
    week,
    time_seq
  ) %>%
  arrange(adm_0_name, Year, week, time_seq)

# 5.2 Monthly Data -------------------------------------------------------------
data_m <- dt_m_complete %>%
  mutate(calendar_start_date = as.character(make_date(Year, month, 1))) %>%
  select(
    adm_0_name,
    dengue_total,
    calendar_start_date,
    Year,
    month,
    time_seq
  ) %>%
  arrange(adm_0_name, Year, month, time_seq)

# 5.3 Monthly Data (Downscaling with Annual Totals) ---------------------------
# For countries with complete sub-annual data to train downscaling model
data_m_down <- merge(dt_m_complete,
  tab[tab$T_res != "Year", c("country_year", "annual_total")],
  by = c("country_year"), all.x = T
) %>%
  select(
    adm_0_name,
    dengue_total,
    calendar_start_date,
    Year,
    month,
    time_seq, annual_total
  ) %>%
  arrange(adm_0_name, Year, month, time_seq)

# 5.4 Yearly Data (Disaggregation) ---------------------------------------------
data_y <- dt_y_complete %>%
  select(
    adm_0_name,
    dengue_total,
    calendar_start_date,
    Year,
    month,
    time_seq, annual_total
  ) %>%
  arrange(adm_0_name, Year, month, time_seq)

# ------------------------------------------------------------------------------
# 6. ADD POPULATION AND GEOGRAPHIC VARIABLES
# ------------------------------------------------------------------------------

source("functions/fn_load_map_shp.R")

# Create population data frame
pop_data <- map_final[, c("adm_0_name", "pop_est", "Year", "Latitude", "Longitude", "lat_band")] %>%
  st_drop_geometry()

# 6.1 Merge Population Data with Weekly Dataset --------------------------------
data_w <- merge(
  data_w,
  pop_data,
  by = c("adm_0_name", "Year"),
  all.x = T
)

# 6.2 Merge Population Data with Monthly Dataset -------------------------------
data_m <- merge(
  data_m,
  pop_data,
  by = c("adm_0_name", "Year"),
  all.x = T
)

# 6.3 Merge Population Data with Downscaling Dataset ---------------------------
data_m_down <- merge(
  data_m_down,
  pop_data,
  by = c("adm_0_name", "Year"),
  all.x = T
)

# 6.4 Merge Population Data with Disaggregation Dataset ------------------------
data_y <- merge(
  data_y,
  pop_data,
  by = c("adm_0_name", "Year"),
  all.x = T
)

# ------------------------------------------------------------------------------
# 7. CREATE MODEL-SPECIFIC VARIABLES
# ------------------------------------------------------------------------------

# 7.1 Weekly Data Variables ----------------------------------------------------
data_w$yearx <- as.integer(as.factor(data_w$Year))
data_w$countryx <- as.integer(factor(
  data_w$adm_0_name,
  levels = unique(data_w$adm_0_name)[order(unique(data_w$adm_0_name))],
  ordered = TRUE
))
# data_w$logpop <- log(data_w$pop_est + 1)

# 7.2 Monthly Data Variables ---------------------------------------------------
data_m$yearx <- as.integer(as.factor(data_m$Year))
data_m$countryx <- as.integer(factor(
  data_m$adm_0_name,
  levels = unique(data_m$adm_0_name)[order(unique(data_m$adm_0_name))],
  ordered = TRUE
))
# data_m$logpop <- log(data_m$pop_est + 1)

# 7.3 Downscaling Data Variables -----------------------------------------------
data_m_down$yearx <- as.integer(as.factor(data_m_down$Year))
data_m_down$countryx <- as.integer(factor(
  data_m_down$adm_0_name,
  levels = unique(data_m_down$adm_0_name)[order(unique(data_m_down$adm_0_name))],
  ordered = TRUE
))

# Filter to complete years only (for training downscaling model)
# Keep only country-years where monthly sum equals annual total
data_m_down_filtered <- data_m_down %>%
  group_by(adm_0_name, Year) %>%
  mutate(sum_of_monthly = sum(dengue_total, na.rm = T)) %>%
  filter(sum_of_monthly == annual_total) %>%
  ungroup() %>%
  na.omit() # 12192 records

# quick sanity check -- why sum_of_monthly != annual_total?
mismatches <- data_m_down %>%
  group_by(adm_0_name, Year) %>%
  mutate(sum_of_monthly = sum(dengue_total, na.rm = T)) %>%
  filter(sum_of_monthly != annual_total) %>%
  add_country_year()

so <- read.csv("data/processed_data/selection_outcome_V1_3_2025_10_08.csv")

inspect <- so %>%
  filter(country_year %in% data_m_down_filtered$country_year) %>%
  pull(relationship_category)


# 7.4 Disaggregation Data Variables --------------------------------------------
data_y$yearx <- as.integer(as.factor(data_y$Year))
data_y$countryx <- as.integer(factor(
  data_y$adm_0_name,
  levels = unique(data_y$adm_0_name)[order(unique(data_y$adm_0_name))],
  ordered = TRUE
))

# ------------------------------------------------------------------------------
# 8. EXPORT MODEL INPUT FILES
# ------------------------------------------------------------------------------

# 8.1 Training Data (Complete Cases Only) --------------------------------------
# Data for cross-validation and model training
write.csv(data_w %>% na.omit(),
  "data/model_input/model_data_weekly_new.csv",
  row.names = F
)

write.csv(data_m %>% na.omit(),
  "data/model_input/model_data_monthly_new.csv",
  row.names = F
)

write.csv(data_m_down_filtered,
  "data/model_input/model_data_downscaling_new.csv",
  row.names = F
)

# 8.2 Prediction Data (Including Missing Values) -------------------------------
# Full datasets with gaps for model prediction/imputation
write.csv(data_w,
  "data/model_input/pred_data_weekly.csv",
  row.names = F
)

write.csv(data_m,
  "data/model_input/pred_data_monthly.csv",
  row.names = F
)

write.csv(data_y,
  "data/model_input/pred_data_disaggregate.csv",
  row.names = F
)
