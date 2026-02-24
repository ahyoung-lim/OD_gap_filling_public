source(file.path(getwd(), "script/imp_model_inla_spec.R"))
source(file.path(getwd(), "script/eval_helpers.R"))
source(file.path(getwd(), "functions/fn_make_week_complete.R"))

pred_m_out <- read.csv(file.path(getwd(), "runs/pred/pred_imp_monthly.csv"))

library(dplyr)
library(rlang)
library(lubridate)
library(countrycode)

# Replace medium NA runs (1–11 months) in dengue_total with pred
fill_medium_gaps <- function(df,
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
      imputed_monthly = .to_fill # keep a flag so you can audit what was filled
    ) %>%
    select(-.is_na, -.run_id, -.run_len, -.to_fill, -.year_change) # Remove helper columns

  return(x)
}


pred_m_filled <- fill_medium_gaps(pred_m_out,
  min_gap     = 1,
  max_gap     = 11
)

# summary: how many points were filled?
w_filled <- sum(pred_m_filled$imputed_weekly, na.rm = TRUE)
m_filled <- sum(pred_m_filled$imputed_monthly, na.rm = TRUE)
total_imp <- w_filled + m_filled

sum(is.na(pred_m_filled$dengue_total)) # remaining gaps
pred_m_filled[is.na(pred_m_filled$dengue_total), ]

# aadd annual_total
pred_m_filled <- pred_m_filled %>%
  group_by(adm_0_name, Year) %>%
  mutate(
    dengue_total = as.integer(dengue_total),
    annual_total = as.integer(sum(dengue_total, na.rm = F))
  ) %>%
  ungroup()

# yearly expanded
pred_data <- read.csv(file.path(getwd(), "data/model_input/pred_data_disaggregate.csv")) %>%
  select(-Latitude, -Longitude) %>%
  mutate(imputed_weekly = FALSE, imputed_monthly = FALSE)


# merge monthly + weekly
df <- rbind(
  pred_data,
  pred_m_filled %>% select(names(pred_data))
)

stopifnot(
  total_imp == (sum(df$imputed_weekly, na.rm = TRUE) + sum(df$imputed_monthly, na.rm = TRUE))
)

# check duplicates- should be 0
dup <- df %>%
  group_by(adm_0_name, Year, month) %>%
  tally() %>%
  filter(n > 1)

stopifnot(nrow(dup) == 0)

df <- df %>%
  mutate(dengue_total = case_when(
    annual_total == 0 ~ 0,
    TRUE ~ dengue_total
  ))

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

n_month_check <- df %>%
  group_by(adm_0_name, Year) %>%
  tally() %>%
  filter(n != 12)

stopifnot(nrow(n_month_check) == 0)

df <- make_month_complete_clean(data = df %>% select(-month, -time_seq), keep_vars = TRUE)

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

stopifnot(length(unique(df$calendar_start_date)) ==
  length(unique(df$time_seq)))

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

saveRDS(fit, file.path(getwd(), "runs/pred/pred_downscale_fit_V3.rds"))

write.csv(df, file.path(getwd(), "runs/pred/pred_downscale_out_V3.csv"), row.names = FALSE)
