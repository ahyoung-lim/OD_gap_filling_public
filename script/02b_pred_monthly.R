source(file.path(getwd(), "script/imp_model_inla_spec.R"))
source(file.path(getwd(), "script/eval_helpers.R"))
source(file.path(getwd(), "functions/fn_make_week_complete.R"))


pred_w_out <- read.csv(file.path(getwd(), "runs/pred/pred_imp_weekly.csv"))


library(dplyr)
library(rlang)
library(lubridate)
library(countrycode)


# Replace small NA runs (1–4 weeks) in dengue_total with pred
fill_small_gaps <- function(df,
                            country_col = "adm_0_name",
                            time_col = "time_seq",
                            year_col = "Year", # Add year column parameter
                            y_col = "dengue_total",
                            pred_col = "pred_mean",
                            min_gap = 1,
                            max_gap = 12) {
  country <- ensym(country_col)
  time <- ensym(time_col)
  year <- ensym(year_col) # Add year symbol
  y <- ensym(y_col)
  pred <- ensym(pred_col)


  x <- df %>%
    arrange(!!country, !!time) %>%
    group_by(!!country) %>%
    # tag NA vs non-NA runs, and reset when year changes
    mutate(
      .is_na = is.na(!!y),
      .year_change = !!year != dplyr::lag(!!year, default = first(!!year)), # Detect year changes
      .run_id = cumsum(coalesce(.is_na != dplyr::lag(.is_na) | .year_change, TRUE)) # Reset run_id on year change
    ) %>%
    group_by(!!country, .run_id) %>%
    mutate(.run_len = n()) %>%
    ungroup() %>%
    # fill only small NA runs where pred is available
    mutate(
      .to_fill = .is_na & .run_len >= min_gap & .run_len <= max_gap & !is.na(!!pred),
      !!y := if_else(.to_fill, !!pred, !!y),
      filled_small_gap = .to_fill # keep a flag so you can audit what was filled
    ) %>%
    select(-.is_na, -.run_id, -.run_len, -.to_fill, -.year_change) # Remove helper columns


  return(x)
}



pred_w_filled <- fill_small_gaps(pred_w_out,
  min_gap     = 1,
  max_gap     = 4
)



# summary: how many points were filled?
sum(pred_w_filled$filled_small_gap, na.rm = TRUE)


sum(is.na(pred_w_filled$dengue_total)) # remaining gaps


# More sophisticated classification
coverage_stats <- pred_w_filled %>%
  group_by(adm_0_name, Year) %>%
  summarise(
    n_weeks_with_data = sum(!is.na(dengue_total)),
    coverage = n_weeks_with_data / 52,
    total_cases = sum(dengue_total, na.rm = TRUE)
  ) %>%
  ungroup()


# Check monthly completeness
monthly_completeness <- pred_w_filled %>%
  mutate(month = month(calendar_start_date)) %>%
  mutate(
    month = case_when(
      year(as.Date(calendar_start_date) + 6) != year(as.Date(calendar_start_date)) &
        year(as.Date(calendar_start_date) + 6) == Year ~
        1,
      TRUE ~ month
    )
  ) %>%
  group_by(adm_0_name, Year, month) %>%
  summarise(
    weeks_in_month = n(),
    weeks_with_data = sum(!is.na(dengue_total)),
    month_complete = (weeks_with_data == weeks_in_month)
  ) %>%
  group_by(adm_0_name, Year) %>%
  summarise(
    n_complete_months = sum(month_complete),
    n_partial_months = sum(weeks_with_data > 0 & !month_complete),
    n_empty_months = sum(weeks_with_data == 0)
  )


# Classify based on BOTH coverage and distribution
data_classification <- coverage_stats %>%
  left_join(monthly_completeness, by = c("adm_0_name", "Year")) %>%
  mutate(
    data_pattern = case_when(
      n_complete_months >= 6 ~ "dense", # At least 6 complete months
      n_complete_months >= 1 ~ "moderate", # Some complete months
      n_partial_months > 0 ~ "sporadic", # Only partial months
      TRUE ~ "no_data"
    )
  )


sporadic_country_years <- data_classification %>%
  filter(data_pattern == "sporadic") %>%
  select(adm_0_name, Year) %>%
  mutate(country_year = paste0(adm_0_name, "_", Year))




# Split the data by pattern
regular_data <- pred_w_filled %>%
  anti_join(sporadic_country_years, by = c("adm_0_name", "Year"))


sporadic_data <- pred_w_filled %>%
  semi_join(sporadic_country_years, by = c("adm_0_name", "Year"))


# Regular aggregation for dense/moderate data
regular_monthly <- regular_data %>%
  mutate(month = as.integer(lubridate::month(calendar_start_date))) %>%
  mutate(
    month = dplyr::case_when(
      lubridate::year(as.Date(calendar_start_date) + 6) != lubridate::year(as.Date(calendar_start_date)) &
        lubridate::year(as.Date(calendar_start_date) + 6) == Year ~ 1L,
      TRUE ~ month
    )
  ) %>%
  group_by(adm_0_name, Year, lat_band, month, pop_est) %>%
  summarise(
    n_weeks = dplyr::n(),
    n_weeks_obs = sum(!is.na(dengue_total)),
    # monthly total only if all weeks present (same behavior as na.rm = FALSE)
    dengue_total = dplyr::if_else(
      n_weeks_obs == n_weeks,
      as.integer(sum(dengue_total, na.rm = TRUE)),
      NA_integer_
    ),
    has_small_gap = any(dplyr::coalesce(filled_small_gap, FALSE)),
    # mark as "imputed from weekly" only when the month is complete AND had any small-gap fills
    imputed_weekly = !is.na(dengue_total) & has_small_gap,
    .groups = "drop"
  ) %>%
  dplyr::select(-n_weeks, -n_weeks_obs)


# Special handling for sporadic data
sporadic_monthly <- sporadic_data %>%
  mutate(month = as.integer(month(calendar_start_date))) %>%
  mutate(
    month = case_when(
      year(as.Date(calendar_start_date) + 6) != year(as.Date(calendar_start_date)) &
        year(as.Date(calendar_start_date) + 6) == Year ~ 1L,
      TRUE ~ month
    )
  ) %>%
  group_by(adm_0_name, Year, lat_band, month, pop_est) %>%
  summarise(
    n_weeks = dplyr::n(),
    n_weeks_observed = sum(!is.na(dengue_total)),
    dengue_partial = sum(dengue_total, na.rm = TRUE),

    # keep partial sum if any weeks observed; else NA
    dengue_total = case_when(
      n_weeks_observed > 0 ~ as.integer(dengue_partial),
      TRUE ~ NA_integer_
    ),

    # flags
    has_small_gap = any(dplyr::coalesce(filled_small_gap, FALSE)),
    imputed_weekly = n_weeks_observed > 0,
    .groups = "drop"
  ) %>%
  select(-n_weeks, -n_weeks_observed, -dengue_partial)


# Combine back
pred_w_m <- bind_rows(regular_monthly, sporadic_monthly) %>%
  arrange(adm_0_name, Year, month)


# ==========================================================#
# Monthly
pred_data <- read.csv(file.path(getwd(), "data/model_input/pred_data_monthly.csv")) %>%
  mutate(imputed_weekly = FALSE)


# merge monthly + weekly
df <- dplyr::bind_rows(
  pred_data[, c("adm_0_name", "Year", "lat_band", "month", "pop_est", "dengue_total", "imputed_weekly")],
  pred_w_m[, c("adm_0_name", "Year", "lat_band", "month", "pop_est", "dengue_total", "imputed_weekly")]
) %>%
  mutate(calendar_start_date = as.character(lubridate::make_date(Year, month, 1)))

# check duplicates- should be 0
df %>%
  group_by(adm_0_name, Year, month) %>%
  tally() %>%
  filter(n > 1)


df <- df %>%
  group_by(adm_0_name, Year, month) %>% # Add other grouping variables as needed
  filter(
    # Keep non-NA values if they exist, otherwise keep NA
    !is.na(dengue_total) | all(is.na(dengue_total))
  ) %>%
  slice(1) %>% # In case there are multiple non-NA values, keep one
  ungroup()


df$countryx <- as.integer(factor(
  df$adm_0_name,
  levels = unique(df$adm_0_name)[order(unique(df$adm_0_name))],
  ordered = TRUE
))


df$yearx <- as.integer(as.factor(df$Year))


df$ISO_A0 <- countrycode(
  df$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)


df$ISO_A0[df$adm_0_name == "SAINT MARTIN"] <- "MAF"


df <- make_month_complete_clean(data = df %>% select(-month), keep_vars = TRUE)


consistent_time_seq_check <- df %>%
  group_by(calendar_start_date) %>%
  summarize(is_consistent = n_distinct(time_seq) == 1) %>%
  ungroup()


# Identify any calendar_start_date where time_seq is inconsistent
inconsistent_dates <- consistent_time_seq_check %>%
  filter(!is_consistent)


if (nrow(inconsistent_dates) != 0) {
  message(
    "Some calendar_start_date have inconsistent time_seq across countries."
  )
}


length(unique(df$calendar_start_date)) ==
  length(unique(df$time_seq))


df <- df %>%
  mutate(
    y = dengue_total,
    region = factor(lat_band),
    regionx = as.integer(region),
    month_shared = month, # shared seasonal curve
    month_dev = month, # country-specific deviation curve
    month_dev_country = month
  ) %>%
  arrange(adm_0_name, time_seq)




mod <- inla_m_hier_shared_formula



seed <- 123
set.seed(seed)



Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)


# Sequential mode:
# Give INLA most physical cores for sequential mode
INLA::inla.setOption(num.threads = 8, save.memory = FALSE)


fit <- INLA::inla(
  formula = update(mod, y ~ .),
  data = df,
  family = ctrl_fam$family,
  control.family = ctrl_fam$control.family,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = FALSE, waic = FALSE, config = TRUE)
)


df$pred_mean <- fit$summary.fitted.values$mean
df$pred_lwr <- fit$summary.fitted.values$`0.025quant`
df$pred_upr <- fit$summary.fitted.values$`0.975quant`

saveRDS(fit, file.path(getwd(), "runs/pred/pred_imp_monthly_fit.rds"))

write.csv(df, file.path(getwd(), "runs/pred/pred_imp_monthly.csv"), row.names = FALSE)
