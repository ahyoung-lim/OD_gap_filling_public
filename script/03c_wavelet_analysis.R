library(tidyverse)
library(nlme)
library(MASS)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(patchwork)
library(conflicted)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(lme4::lmer)

set.seed(123)

df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_with_ci_V3.csv"))

# Load map shapefile with coordinates and population
source("functions/fn_load_map_shp.R")
# This creates map_final with: adm_0_name, Latitude, Longitude, pop_est, Year

# ==============================================================================
# COUNTRY ELIGIBILITY FILTERS
# ==============================================================================

# --- Calculate active years per country ---
# Active year = year with >50 total cases
country_active_years <- df %>%
  group_by(od_region, adm_0_name, Year) %>%
  summarise(total_cases = sum(dengue_total_scaled, na.rm = TRUE), .groups = "drop") %>%
  mutate(active = total_cases > 20) %>%
  group_by(od_region, adm_0_name) %>%
  summarise(
    n_active_years = sum(active),
    total_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    .groups = "drop"
  )

# --- ELIGIBLE SET: Based on minimum active years ---
# Annual: at least 10 years with >20 cases
# Multiannual: at least 20 years with >20 cases
ann_countries <- country_active_years %>%
  filter(n_active_years >= 10) %>%
  pull(adm_0_name)

mlt_countries <- country_active_years %>%
  filter(n_active_years >= 20) %>%
  pull(adm_0_name)

cat("\n=== ELIGIBLE COUNTRIES ===\n")
cat("Annual analysis (10+ active years):", length(ann_countries), "countries\n")
cat("Multiannual analysis (20+ active years):", length(mlt_countries), "countries\n")

country_active_years %>%
  mutate(
    ann_countries = ifelse(adm_0_name %in% ann_countries, TRUE, FALSE),
    mlt_countries = ifelse(adm_0_name %in% mlt_countries, TRUE, FALSE)
  ) %>%
  group_by(od_region) %>%
  summarise(
    total_countries = n(),
    prop_ann = sum(ann_countries) / total_countries,
    prop_mlt = sum(mlt_countries) / total_countries
  ) %>%
  arrange(prop_ann)

# Filter out emerging/low-incidence regions
df <- df %>%
  filter(!od_region %in% c("Sub-Saharan Africa", "Europe, Middle East & North Africa"))

country_active_years <- country_active_years %>%
  filter(!od_region %in% c("Sub-Saharan Africa", "Europe, Middle East & North Africa"))


# --- ENDEMIC SET: At least one 10-year window with ≥7 active years, starting ≤2005 ---
# A country is endemic if:
#   1. It has at least one 10-year window with ≥7 active years, AND
#   2. The start of that window is ≤2005 (established transmission before recent expansion)
calculate_endemic_status <- function(country_data) {
  # Get yearly active status
  yearly <- country_data %>%
    group_by(Year) %>%
    summarise(active = sum(dengue_total_scaled, na.rm = TRUE) > 20, .groups = "drop")

  if (nrow(yearly) < 10) {
    return(FALSE)
  }

  min_year <- min(yearly$Year)
  max_year <- max(yearly$Year)

  # Check 10-year windows starting at or before 2005
  for (start_year in seq(min_year, min(max_year - 9, 2000), by = 1)) {
    window_active <- yearly %>%
      filter(Year >= start_year, Year < start_year + 10) %>%
      pull(active) %>%
      sum(na.rm = TRUE)

    if (window_active >= 7) {
      return(TRUE) # Found a qualifying window
    }
  }
  return(FALSE)
}

# Calculate endemic status for each country
endemic_status <- df %>%
  group_by(adm_0_name) %>%
  group_modify(~ tibble(is_endemic = calculate_endemic_status(.x))) %>%
  ungroup()

endemic_countries <- endemic_status %>%
  filter(is_endemic) %>%
  pull(adm_0_name)

cat("\n=== ENDEMIC COUNTRIES ===\n")
cat(
  "Countries with established transmission (≥7 active years in 10-year window starting ≤2000):",
  length(endemic_countries), "countries\n"
)

# Store for later use
eligible_ann_countries <- ann_countries
eligible_mlt_countries <- mlt_countries

cat("\n--- Summary ---\n")
cat("Eligible for annual:", length(eligible_ann_countries), "\n")
cat("Eligible for multiannual:", length(eligible_mlt_countries), "\n")
cat("Endemic & eligible for annual:", length(intersect(endemic_countries, eligible_ann_countries)), "\n")
cat("Endemic & eligible for multiannual:", length(intersect(endemic_countries, eligible_mlt_countries)), "\n")

eligible_non_endemic <- setdiff(eligible_ann_countries, endemic_countries)

df %>%
  filter(adm_0_name %in% eligible_non_endemic) %>%
  group_by(adm_0_name, Year) %>%
  summarise(annual_total = sum(dengue_total_scaled)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = annual_total)) +
  facet_wrap(adm_0_name ~ ., scales = "free")

# Use eligible set for the main analysis
# Multiannual analysis will use mlt_countries filter
countries_to_include <- ann_countries
df <- df %>% filter(adm_0_name %in% countries_to_include)

# Filter to endemic & eligible countries
endemic_and_eligible_ann <- intersect(endemic_countries, eligible_ann_countries)
endemic_and_eligible_mlt <- intersect(endemic_countries, eligible_mlt_countries)

cat("  Endemic & eligible (annual):", length(endemic_and_eligible_ann), "countries\n")
cat("  Endemic & eligible (multiannual):", length(endemic_and_eligible_mlt), "countries\n")

# ==============================================================================
# FORMAT DATA FOR PIPELINE
# ==============================================================================
# Wavelet analysis pipeline adapted from Quandelacy et al. (2025).
# https://github.com/tquandelacy/denv_am_synchrony
# The adapted functions expect "province" and "co_province" as location identifiers
# For country-level analysis, use country as both country and province

all_cases <- df %>%
  mutate(
    month = as.integer(month),
    Year = as.integer(Year),
    time = Year + (month - 1) / 12,
    ln_cases = log(dengue_total_scaled + 1),
    country = adm_0_name,
    province = adm_0_name,
    co_province = adm_0_name
  ) %>%
  arrange(country, time) %>%
  select(country, province, co_province, time, ln_cases, od_region)

cat("Time range:", min(all_cases$time), "-", max(all_cases$time), "\n")

# ==============================================================================
# CREATE COORDINATE AND POPULATION DATA
# ==============================================================================

# Create prov_coords from map_final (coordinates per country)
prov_coords <- map_final %>%
  st_drop_geometry() %>%
  select(adm_0_name, Latitude, Longitude) %>%
  distinct() %>%
  rename(
    co_province = adm_0_name,
    lat = Latitude,
    long = Longitude
  ) %>%
  mutate(country = co_province) # Same since country-level




# ==============================================================================
# CALCULATE WAVELETS FOR EACH COUNTRY
# ==============================================================================
# wavelet functions adapted from Quandelacy et al. (2025).
# https://github.com/tquandelacy/denv_am_synchrony
source("denv_am_synchrony-main/code/WaveletPackage_functions.R")
source("denv_am_synchrony-main/code/calculate_country_wavelets.R")

unique_countries <- unique(all_cases$country)

prov_df <- lapply(unique_countries, function(c) {
  country_data <- all_cases %>% filter(country == c)
  tryCatch(
    calculate_country_wavelets(country_data),
    error = function(e) {
      cat("    Error:", e$message, "\n")
      return(NULL)
    }
  )
}) %>%
  bind_rows() %>%
  filter(!is.null(wave))

cat("Wavelets calculated for", nrow(prov_df), "countries\n")


cat("\n=== EXTRACTING WAVELET COMPONENTS ===\n")

prov_df$ann_wave <- list(NULL)
prov_df$mlt_wave <- list(NULL)

for (i in 1:dim(prov_df)[1]) {
  # Annual (8-16 months)
  prov_df$ann_wave[[i]] <- extract_wavelet_coi(
    wave = prov_df$wave[[i]],
    scale = prov_df$scale[[i]],
    coi = prov_df$coi[[i]],
    time = prov_df$time[[i]],
    low = 8,
    high = 16
  )

  # Multiannual (>16 months)
  prov_df$mlt_wave[[i]] <- extract_wavelet_coi(
    wave = prov_df$wave[[i]],
    scale = prov_df$scale[[i]],
    coi = prov_df$coi[[i]],
    time = prov_df$time[[i]],
    low = 17
  )
}

# Unnest annual wavelets
ann_wavelet <- prov_df %>%
  select(country, province, ann_wave) %>%
  tidyr::unnest(cols = c(ann_wave))

ann_wavelet <- left_join(ann_wavelet, prov_coords,
  by = c("country" = "country", "province" = "co_province")
)

# Unnest multiannual wavelets (only for 10+ year countries)
mlt_wavelet <- prov_df %>%
  filter(country %in% mlt_countries) %>%
  select(country, province, mlt_wave) %>%
  tidyr::unnest(cols = c(mlt_wave))

mlt_wavelet <- left_join(mlt_wavelet, prov_coords,
  by = c("province" = "co_province", "country" = "country")
)

cat("Annual wavelets extracted for", length(unique(ann_wavelet$country)), "countries\n")
cat("Multiannual wavelets extracted for", length(unique(mlt_wavelet$country)), "countries\n")



# ==============================================================================
#### Dengue Spectra Analysis ####
# ==============================================================================
# Estimate the power spectrum for each country and test if cycles
# (annual, multiannual) are statistically significant above red noise.
# ==============================================================================

# ------------------------------------------------------------------------------
# STEP 1: Extract time series and compute AR(1) parameters
# ------------------------------------------------------------------------------
# prov_ts: Long-format time series data
#   - Columns: country, province, time (numeric), Ts (log-transformed cases)
#   - One row per time point per country
prov_ts <- (prov_df) %>%
  select(country, province, time, Ts) %>%
  tidyr::unnest(cols = c(time, Ts))

# alphas: AR(1) autocorrelation parameters per country
#   - alpha: AR(1) coefficient (how correlated is value with previous month)
#   - var: residual variance of the AR(1) model
#   - n: length of time series
#   Used to characterize the background noise for significance testing
locations <- unique(prov_ts$province)
alphas <- tibble()
for (i in 1:length(locations)) {
  this_ts <- filter(prov_ts, province == locations[i])$Ts
  this_ts <- normalizeSeries(this_ts)
  this_ar1 <- ar1(this_ts)
  alphas <- bind_rows(
    alphas,
    tibble(
      province = locations[i],
      alpha = this_ar1$ar,
      var = this_ar1$var,
      n = length(this_ts)
    )
  )
}

# alpha_df: Adds country name to alphas
alpha_df <- left_join(
  alphas,
  select(prov_ts, country, province) %>% filter(!duplicated(.))
)

# alpha_country: Summary statistics of AR(1) parameters per country
#   - median/mean/min/max: Statistics of alpha across provinces (if multiple)
#   - median_var: median residual variance
#   - max_n: longest time series length (for significance calculation)
#   - n_prov: number of provinces (1 for country-level analysis)
alpha_country <- group_by(alpha_df, country) %>%
  summarize(
    median = median(alpha, na.rm = T),
    mean = mean(alpha, na.rm = T),
    min = min(alpha, na.rm = T),
    max = max(alpha, na.rm = T),
    median_var = median(var, na.rm = T),
    max_n = max(n),
    n_prov = n()
  )

# ------------------------------------------------------------------------------
# Extract YEARLY power directly from wavelet (not by averaging monthly)
# ------------------------------------------------------------------------------
cat("\n=== EXTRACTING YEARLY POWER DIRECTLY FROM WAVELET ===\n")

yearly_power_direct <- tibble()

for (i in 1:nrow(prov_df)) {
  this_country <- prov_df$country[i]
  this_wave <- prov_df$wave[[i]]
  this_scale <- prov_df$scale[[i]]
  this_time <- prov_df$time[[i]]
  this_coi <- prov_df$coi[[i]]

  # Convert scales to periods
  omega0 <- 6
  this_period <- this_scale * (4 * pi) / (omega0 + sqrt(2 + omega0^2))

  # Calculate power and apply COI masking
  power_matrix <- abs(this_wave)^2
  for (s in 1:length(this_period)) {
    power_matrix[s, this_coi < this_period[s]] <- NA
  }

  # Identify period bands
  ann_idx <- which(this_period >= 8 & this_period <= 16)
  mlt_idx <- which(this_period > 17)

  # Get years in this time series
  years <- floor(this_time)
  unique_years <- sort(unique(years))

  for (yr in unique_years) {
    yr_mask <- years == yr
    n_months <- sum(yr_mask)

    # Skip years with too few months
    if (n_months < 6) next

    # Extract power for this year (average across time points AND scales in band)
    if (length(ann_idx) > 0) {
      ann_power_yr <- mean(power_matrix[ann_idx, yr_mask], na.rm = TRUE)
      ann_valid <- sum(!is.na(power_matrix[ann_idx, yr_mask]))
    } else {
      ann_power_yr <- NA
      ann_valid <- 0
    }

    if (length(mlt_idx) > 0 && this_country %in% mlt_countries) {
      mlt_power_yr <- mean(power_matrix[mlt_idx, yr_mask], na.rm = TRUE)
      mlt_valid <- sum(!is.na(power_matrix[mlt_idx, yr_mask]))
    } else {
      mlt_power_yr <- NA
      mlt_valid <- 0
    }

    yearly_power_direct <- bind_rows(
      yearly_power_direct,
      tibble(
        country = this_country,
        year = yr,
        ann_power_yearly = ann_power_yr,
        mlt_power_yearly = mlt_power_yr,
        n_ann_valid = ann_valid,
        n_mlt_valid = mlt_valid
      )
    )
  }
}

cat("Yearly power extracted directly for", n_distinct(yearly_power_direct$country), "countries\n")
cat("Year range:", min(yearly_power_direct$year), "-", max(yearly_power_direct$year), "\n")
cat("Total year-country observations:", nrow(yearly_power_direct), "\n")

# Use yearly_power_direct (extracted directly from wavelet)
yearly_power <- yearly_power_direct

# Add epsilon to yearly power to prevent log(0) issues
epsilon_ann_yearly <- 1e-6 * median(yearly_power$ann_power_yearly, na.rm = TRUE)
epsilon_mlt_yearly <- 1e-6 * median(yearly_power$mlt_power_yearly, na.rm = TRUE)

yearly_power <- yearly_power %>%
  mutate(
    ann_power_yearly = ann_power_yearly + epsilon_ann_yearly,
    mlt_power_yearly = mlt_power_yearly + epsilon_mlt_yearly
  )

cat("Epsilon (annual yearly):", epsilon_ann_yearly, "\n")
cat("Epsilon (multiannual yearly):", epsilon_mlt_yearly, "\n")

# --- Prepare Data  ---

# ENDEMIC-ONLY (must be both eligible AND endemic)
yearly_power_endemic <- yearly_power %>%
  filter(country %in% endemic_and_eligible_ann, !is.na(ann_power_yearly)) %>%
  mutate(year_centered = (year - mean(year)) / sd(year))

# Store scaling factors
year_sd_endemic <- sd(yearly_power_endemic$year - mean(yearly_power_endemic$year))


# ==============================================================================
# POWER: Interaction model
# log(power) ~ year_centered * region + (year_centered | country)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0) Data prep
# ------------------------------------------------------------------------------
# Add region info to yearly power data
country_region_map <- df %>%
  dplyr::select(adm_0_name, od_region) %>%
  distinct() %>%
  rename(country = adm_0_name)

# Create Americas vs Asia classification
americas_regions <- c("North & Central America", "South America", "Caribbean Islands")

country_broad_region_map <- country_region_map %>%
  mutate(broad_region = ifelse(od_region %in% americas_regions, "Americas", "Asia"))

cat("Americas regions:", paste(americas_regions, collapse = ", "), "\n")
cat("Asia regions:", paste(setdiff(unique(country_region_map$od_region), americas_regions), collapse = ", "), "\n")


# Annual endemic dataset
power_interaction_ann <- yearly_power_endemic %>%
  left_join(country_broad_region_map, by = "country") %>%
  filter(!is.na(broad_region)) %>%
  mutate(
    log_power = log(ann_power_yearly),
    year_centered = (year - mean(year)) / sd(year),
    region = factor(broad_region, levels = c("Americas", "Asia"))
  ) %>%
  filter(is.finite(log_power))

# Multiannual endemic dataset
power_interaction_mlt <- yearly_power_endemic %>%
  left_join(country_broad_region_map, by = "country") %>%
  filter(!is.na(broad_region)) %>%
  mutate(
    log_power = log(mlt_power_yearly),
    year_centered = (year - mean(year)) / sd(year),
    region = factor(broad_region, levels = c("Americas", "Asia"))
  ) %>%
  filter(is.finite(log_power))


# ------------------------------------------------------------------------------
# Fit power interaction models
# ------------------------------------------------------------------------------

cat("\nFitting Annual power interaction model...\n")
power_int_model_ann <- tryCatch(
  nlme::lme(
    log_power ~ year_centered * region,
    random = ~ year_centered | country,
    data = power_interaction_ann,
    method = "REML",
    control = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  ),
  error = function(e) {
    cat("  Warning: random slope failed; using random intercept only.\n")
    nlme::lme(
      log_power ~ year_centered * region,
      random = ~ 1 | country,
      data = power_interaction_ann,
      method = "REML",
      control = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )
  }
)

cat("Fitting Multiannual power interaction model...\n")
power_int_model_mlt <- tryCatch(
  nlme::lme(
    log_power ~ year_centered * region,
    random = ~ year_centered | country,
    data = power_interaction_mlt,
    method = "REML",
    control = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  ),
  error = function(e) {
    cat("  Warning: random slope failed; using random intercept only.\n")
    nlme::lme(
      log_power ~ year_centered * region,
      random = ~ 1 | country,
      data = power_interaction_mlt,
      method = "REML",
      control = lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
    )
  }
)

# Helper: simulation-based two-sided p-value from draws
p_from_draws <- function(x) {
  # two-sided sign test around 0 using posterior/normal draws
  2 * min(mean(x > 0, na.rm = TRUE), mean(x < 0, na.rm = TRUE))
}

# Extract results in standard table format
extract_power_interaction_results <- function(model, data, cycle_name,
                                              n_sim = 5000,
                                              weight_by = c("countries", "obs", "pairs"),
                                              seed = 123,
                                              start_year = 1990,
                                              end_year = 2024) {
  weight_by <- match.arg(weight_by)
  if (weight_by == "pairs") weight_by <- "obs" # alias

  # Fixed effects + covariance (order-safe)
  mu <- nlme::fixef(model)
  V <- as.matrix(vcov(model))

  # Summary table (used only for interaction p-value, since trend p-values are simulation-based)
  tt <- summary(model)$tTable

  # Year scaling used in centring (must match how year_centered was constructed)
  sd_yr <- sd(data$year)

  # Coeff availability
  has_region <- "regionAsia" %in% names(mu)
  has_int <- "year_centered:regionAsia" %in% names(mu)

  b0 <- mu["(Intercept)"]
  b_year <- mu["year_centered"]
  b_region <- if (has_region) mu["regionAsia"] else 0
  b_int <- if (has_int) mu["year_centered:regionAsia"] else 0

  # --- Weights for "Global" ---
  nA_cty <- dplyr::n_distinct(data$country[data$region == "Americas"])
  nAs_cty <- dplyr::n_distinct(data$country[data$region == "Asia"])
  nTot_cty <- dplyr::n_distinct(data$country)

  nA_obs <- sum(data$region == "Americas")
  nAs_obs <- sum(data$region == "Asia")
  nTot_obs <- nrow(data)

  if (weight_by == "countries") {
    wA <- nA_cty
    wAs <- nAs_cty
  } else {
    wA <- nA_obs
    wAs <- nAs_obs
  }

  w_sum <- wA + wAs
  if (!is.finite(w_sum) || w_sum <= 0) stop("Global weights sum to zero.")
  wA <- wA / w_sum
  wAs <- wAs / w_sum

  # --- Per-year log slopes (fixed effects) ---
  betaA_per_year <- b_year / sd_yr
  betaAs_per_year <- (b_year + b_int) / sd_yr
  betaG_per_year <- wA * betaA_per_year + wAs * betaAs_per_year

  # Convert to multiplicative % change per year
  pctA <- (exp(betaA_per_year) - 1) * 100
  pctAs <- (exp(betaAs_per_year) - 1) * 100
  pctG <- (exp(betaG_per_year) - 1) * 100

  # --- CUMULATIVE % change from 1990 to 2024   ---
  delta_years <- 2024 - 1990

  pctA_cum <- (exp(betaA_per_year * delta_years) - 1) * 100
  pctAs_cum <- (exp(betaAs_per_year * delta_years) - 1) * 100
  pctG_cum <- (exp(betaG_per_year * delta_years) - 1) * 100

  # --- Simulation-based CI + simulation-based p-values (CONSISTENT across rows) ---
  set.seed(seed)
  coef_draws <- MASS::mvrnorm(n_sim, mu = mu, Sigma = V)

  b_year_draws <- coef_draws[, "year_centered"]
  b_int_draws <- if (has_int) coef_draws[, "year_centered:regionAsia"] else rep(0, n_sim)

  betaA_draws <- b_year_draws / sd_yr
  betaAs_draws <- (b_year_draws + b_int_draws) / sd_yr
  betaG_draws <- wA * betaA_draws + wAs * betaAs_draws

  pctA_draws <- (exp(betaA_draws) - 1) * 100
  pctAs_draws <- (exp(betaAs_draws) - 1) * 100
  pctG_draws <- (exp(betaG_draws) - 1) * 100

  ciA <- quantile(pctA_draws, c(0.025, 0.975), na.rm = TRUE)
  ciAs <- quantile(pctAs_draws, c(0.025, 0.975), na.rm = TRUE)
  ciG <- quantile(pctG_draws, c(0.025, 0.975), na.rm = TRUE)

  pctA_cum_draws <- (exp(betaA_draws * delta_years) - 1) * 100
  pctAs_cum_draws <- (exp(betaAs_draws * delta_years) - 1) * 100
  pctG_cum_draws <- (exp(betaG_draws * delta_years) - 1) * 100

  ciA_cum <- quantile(pctA_cum_draws, c(0.025, 0.975), na.rm = TRUE)
  ciAs_cum <- quantile(pctAs_cum_draws, c(0.025, 0.975), na.rm = TRUE)
  ciG_cum <- quantile(pctG_cum_draws, c(0.025, 0.975), na.rm = TRUE)

  # Differences (Asia - Americas) + CI
  pct_diff_draws <- pctAs_draws - pctA_draws
  pct_diff <- pctAs - pctA
  ci_diff <- quantile(pct_diff_draws, c(0.025, 0.975), na.rm = TRUE)

  # Trend ≠ 0 tests (simulation p-values on per-year log slope) - now for ALL rows
  pA <- p_from_draws(betaA_draws)
  pAs <- p_from_draws(betaAs_draws)
  pG <- p_from_draws(betaG_draws)

  # Interaction p-value (Asia slope differs from Americas) - model-based (single test)
  p_interaction <- if (has_int) tt["year_centered:regionAsia", "p-value"] else NA_real_

  # For display only: predicted power at start/end using fixed effects
  min_yr <- min(data$year)
  max_yr <- max(data$year)
  mean_yr <- mean(data$year)

  x_start <- (min_yr - mean_yr) / sd_yr
  x_end <- (max_yr - mean_yr) / sd_yr

  power_start_A <- exp(b0 + b_year * x_start)
  power_end_A <- exp(b0 + b_year * x_end)
  power_start_As <- exp(b0 + b_region + (b_year + b_int) * x_start)
  power_end_As <- exp(b0 + b_region + (b_year + b_int) * x_end)

  power_start_G <- wA * power_start_A + wAs * power_start_As
  power_end_G <- wA * power_end_A + wAs * power_end_As

  # N shown in the table: keep it aligned with your "increasing" denominator (countries with enough data)
  # (You can switch these to obs if you prefer, but then also change the increasing-stats method.)
  N_display <- c(nTot_cty, nA_cty, nAs_cty)

  out <- data.frame(
    Cycle = cycle_name,
    Region = c("Global", "Americas", "Asia"),
    N = N_display,
    Power_Start = c(power_start_G, power_start_A, power_start_As),
    Power_End = c(power_end_G, power_end_A, power_end_As),
    Pct_Change = c(pctG, pctA, pctAs),
    CI_Lower = c(ciG[[1]], ciA[[1]], ciAs[[1]]),
    CI_Upper = c(ciG[[2]], ciA[[2]], ciAs[[2]]),

    # CONSISTENT p-values (all simulation-based trend tests)
    P_value = c(pG, pA, pAs),

    # Show interaction p-value only once (on Asia row is typical)
    Interaction_P = c(NA_real_, NA_real_, p_interaction),
    Diff_AsiaMinusAmericas = c(NA_real_, NA_real_, pct_diff),
    Diff_CI_Lower = c(NA_real_, NA_real_, ci_diff[[1]]),
    Diff_CI_Upper = c(NA_real_, NA_real_, ci_diff[[2]]),
    Pct_Change_Cum_1990_2024 = c(pctG_cum, pctA_cum, pctAs_cum),
    Cum_CI_Lower_1990_2024 = c(ciG_cum[[1]], ciA_cum[[1]], ciAs_cum[[1]]),
    Cum_CI_Upper_1990_2024 = c(ciG_cum[[2]], ciA_cum[[2]], ciAs_cum[[2]]),
    Weighting = weight_by,
    stringsAsFactors = FALSE
  )

  out
}

# % Increasing counts (simple LM per country; matches your earlier approach)
get_trend_stats <- function(df, val_col) {
  if (nrow(df) == 0) {
    return(data.frame(N_Inc = 0, N_Tot = 0, Pct_Inc = NA_real_))
  }

  res <- df %>%
    group_by(country) %>%
    do({
      sub <- .
      if (sum(!is.na(sub[[val_col]])) >= 5) {
        fit <- lm(log(sub[[val_col]]) ~ year, data = sub)
        ci <- confint(fit, "year", level = 0.95)
        data.frame(is_inc = (ci[1] > 0))
      } else {
        data.frame(is_inc = NA)
      }
    }) %>%
    ungroup()

  n_tot <- sum(!is.na(res$is_inc))
  n_inc <- sum(res$is_inc, na.rm = TRUE)
  pct <- if (n_tot > 0) (n_inc / n_tot) * 100 else NA_real_

  data.frame(N_Inc = n_inc, N_Tot = n_tot, Pct_Inc = pct)
}

# Run extraction for annual + multiannual, merge increasing stats, print table
power_int_results_ann <- extract_power_interaction_results(
  power_int_model_ann, power_interaction_ann, "Annual",
  n_sim = 5000, weight_by = "countries"
)

power_int_results_mlt <- extract_power_interaction_results(
  power_int_model_mlt, power_interaction_mlt, "Multiannual",
  n_sim = 5000, weight_by = "countries"
)

power_int_results <- dplyr::bind_rows(power_int_results_ann, power_int_results_mlt)

# Increasing stats (Annual)
stats_ann_G <- get_trend_stats(power_interaction_ann, "ann_power_yearly")
stats_ann_A <- get_trend_stats(power_interaction_ann %>% filter(region == "Americas"), "ann_power_yearly")
stats_ann_As <- get_trend_stats(power_interaction_ann %>% filter(region == "Asia"), "ann_power_yearly")

# Increasing stats (Multiannual)
stats_mlt_G <- get_trend_stats(power_interaction_mlt, "mlt_power_yearly")
stats_mlt_A <- get_trend_stats(power_interaction_mlt %>% filter(region == "Americas"), "mlt_power_yearly")
stats_mlt_As <- get_trend_stats(power_interaction_mlt %>% filter(region == "Asia"), "mlt_power_yearly")

trend_stats_df <- rbind(
  stats_ann_G, stats_ann_A, stats_ann_As,
  stats_mlt_G, stats_mlt_A, stats_mlt_As
)

power_int_results <- dplyr::bind_cols(power_int_results, trend_stats_df)

# Arrange in a sensible print order
power_int_results <- power_int_results %>%
  mutate(Region = factor(Region, levels = c("Global", "Americas", "Asia"))) %>%
  arrange(Cycle, Region)

# Print consolidated table
fmt_p <- function(p, digits = 2, cutoff = 1e-3) {
  if (is.na(p)) {
    return("")
  }

  # Main tiny cutoff label, e.g. <0.001
  cutoff_digits <- max(0, ceiling(-log10(cutoff)))
  cutoff_label <- paste0("<", formatC(cutoff, format = "f", digits = cutoff_digits))

  # Smallest value representable with requested decimals, e.g. 0.01 for digits=2
  min_printable <- 10^(-digits)
  min_label <- paste0("<", formatC(min_printable, format = "f", digits = digits))

  if (p < cutoff) {
    cutoff_label
  } else if (p < min_printable) {
    min_label
  } else {
    formatC(p, format = "f", digits = digits)
  }
}

fmt_diff <- function(r) {
  if (is.na(r$Diff_AsiaMinusAmericas)) {
    return("")
  }
  sprintf("%+6.2f (%+6.2f,%+6.2f)", r$Diff_AsiaMinusAmericas, r$Diff_CI_Lower, r$Diff_CI_Upper)
}

{
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                    POWER INTERACTION MODEL SUMMARY (Endemic Only) - CONSOLIDATED                                                             ║\n")
  cat("╠═══════════╦══════════════╦════╦═════════════╦═══════════╦════════════╦═══════════════╦══════════╦═══════════════════════════╦════════════╦══════════════════════════════╣\n")
  cat("║ Scope     ║ Cycle        ║ N  ║ Power Start  ║ Power End ║ %Change/yr ║    95% CI     ║ p-value  ║ % Increasing (X/Y)       ║ Int. p-val ║ Asia−Am Δ%/yr (95% CI)       ║\n")
  cat("╟───────────╫──────────────╫────╫─────────────╫───────────╫────────────╫───────────────╫──────────╫───────────────────────────╫────────────╫──────────────────────────────╢\n")

  for (i in seq_len(nrow(power_int_results))) {
    r <- power_int_results[i, ]

    p_tr <- fmt_p(r$P_value, digits = 2, cutoff = 1e-3)
    p_int <- if (is.na(r$Interaction_P)) "" else fmt_p(r$Interaction_P, digits = 2, cutoff = 1e-3)

    cat(sprintf(
      "║ %-9s ║ %-12s ║ %2d ║ %11.2f ║ %9.2f ║ %9.2f%% ║ (%6.2f,%6.2f) ║ %8s ║ %2d/%2d (%5.2f%%)            ║ %10s ║ %-28s ║\n",
      as.character(r$Region), r$Cycle, r$N,
      r$Power_Start, r$Power_End, r$Pct_Change,
      r$CI_Lower, r$CI_Upper, p_tr,
      r$N_Inc, r$N_Tot, r$Pct_Inc,
      p_int,
      fmt_diff(r)
    ))
  }
}

# Export results table
write.csv(power_int_results, "output/tables//wavelet_power_interaction_results.csv", row.names = FALSE)


# ==============================================================================
# COHERENCE AND PHASE EXTRACTION
# ==============================================================================
# This section computes wavelet coherence and phase difference between all
# country pairs. Coherence measures how similar the frequency patterns are
# between two time series (0 = no similarity, 1 = identical patterns).
# Phase measures the timing offset between cycles.
#
# Key outputs:
#   - prov_combos: All pairs with coherence matrices (time × scale)
#   - uniq_prov_pairs: Unique pairs (A-B, not B-A duplicates)
#   - Global coherence: Single summary value per pair
#   - Band-specific coherence: Annual (8-16 mo) and multiannual (>17 mo)
# ==============================================================================

# ------------------------------------------------------------------------------
# CREATE COUNTRY PAIR COMBINATIONS
# ------------------------------------------------------------------------------
# Create all possible pairs of countries for comparison
# Note: This includes both A-B and B-A pairs (needed for some calculations)

prov_combos <- tidyr::crossing(
  ref_prov = prov_df$province,
  other_prov = prov_df$province
) %>%
  filter(ref_prov != other_prov)

# Create unique pairs only (A-B, not B-A) to avoid duplicate analyses
uniq_prov_pairs <- data.frame(t(combn(unique(prov_combos$ref_prov), 2))) %>%
  rename(
    ref_prov = X1,
    other_prov = X2
  )

# Initialize list columns to store results for each pair
prov_combos$coherence <- list(NULL) # Coherence matrix per pair
prov_combos$phase <- list(NULL) # Phase difference matrix per pair
prov_combos$scales <- list(NULL) # Matched wavelet scales
prov_combos$coi <- list(NULL) # Cone of influence (edge effects)
prov_combos$times <- list(NULL) # Matched time points

# ------------------------------------------------------------------------------
# COMPUTE PAIRWISE COHERENCE (Full Spectrum)
# ------------------------------------------------------------------------------
# For each country pair, compute:
#   - Coherence at each scale/period (e.g., 12mo, 24mo, 36mo)
#   - Phase difference at each scale/period
# Output: Matrix (time × scale) showing how coherence varies over time and period

cat("\n=== COMPUTING PAIRWISE COHERENCE ===\n")
cat("Total pairs:", nrow(prov_combos), "\n")
pb <- txtProgressBar(min = 0, max = nrow(prov_combos), style = 3)

for (i in 1:dim(prov_combos)[1]) {
  setTxtProgressBar(pb, i)

  # Get wavelet transforms for both countries
  ref_prov <- filter(prov_df, province == prov_combos$ref_prov[i])
  other_prov <- filter(prov_df, province == prov_combos$other_prov[i])

  # Match transforms to common time/scale grid
  match_obj <- match_transforms(
    wave1 = ref_prov$wave[[1]], wave2 = other_prov$wave[[1]],
    time1 = ref_prov$time[[1]], time2 = other_prov$time[[1]],
    scale1 = ref_prov$scale[[1]], scale2 = other_prov$scale[[1]]
  )

  # Store matched scales, COI, and times
  prov_combos$scales[[i]] <- match_obj$scales
  prov_combos$coi[[i]] <- get_coi(n = length(match_obj$times), dt = 1)
  prov_combos$times[[i]] <- match_obj$times

  # Compute coherence (0-1, how similar are the frequency patterns)
  prov_combos$coherence[[i]] <- coh(
    cwt1 = match_obj$wave1,
    cwt2 = match_obj$wave2,
    scales = match_obj$scales,
    dt = 1,
    dj = 1 / 4
  )

  # Compute phase difference (timing offset between cycles)
  prov_combos$phase[[i]] <- phase_updated(
    cwt1 = match_obj$wave1, cwt2 = match_obj$wave2,
    scales = match_obj$scales,
    times = match_obj$times,
    dt = 1,
    dj = 1 / 4
  )
}
close(pb)
cat("\nPairwise coherence complete\n")


# ------------------------------------------------------------------------------
# EXTRACT ANNUAL BAND COHERENCE AND PHASE (8-16 months)
# ------------------------------------------------------------------------------
# This section extracts coherence and phase for the ANNUAL band (8-16 months).
# Two types of extraction:
#   A. Time-varying: Coherence/phase at EACH time point (for trend analysis)
#   B. Time-averaged: Single average value per pair (for distance analysis)
#
# Key outputs:
#   - ann_coh: Time-varying annual coherence (for Fig 3-style plots)
#   - avg_ann_coh: Time-averaged annual coherence (for Fig 5-style plots)
#   - ann_phase: Time-varying annual phase difference
#   - avg_ann_phase: Time-averaged annual phase difference

cat("\n=== EXTRACTING ANNUAL COHERENCE/PHASE (8-16 months) ===\n")

# 1: Time-varying annual coherence (for temporal trend analysis)
cat("Extracting annual coherence over time...\n")
pb <- txtProgressBar(min = 0, max = nrow(prov_combos), style = 3)

ann_coh <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  setTxtProgressBar(pb, i)
  ann_coh[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = prov_combos$coherence[[i]],
    time = prov_combos$times[[i]],
    scales = prov_combos$scales[[i]],
    coi = prov_combos$coi[[i]],
    low = 8, high = 16 # Annual band: 8-16 months
  ))

  ann_coh[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  ann_coh[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
close(pb)
cat("\n")
ann_coh <- bind_rows(ann_coh)
uniq_ann_coh <- suppressWarnings(left_join(uniq_prov_pairs, ann_coh,
  by = c("ref_prov", "other_prov")
))

# 2: Time-averaged annual coherence (for distance analysis)
avg_ann_coh <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  avg_ann_coh[[i]] <- extract_avg_coh_phase_coi_updated(prov_combos$coherence[[i]],
    prov_combos$scales[[i]],
    prov_combos$coi[[i]],
    low = 8, high = 16
  )
  avg_ann_coh[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  avg_ann_coh[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
avg_ann_coh <- bind_rows(avg_ann_coh)

# Create unique pairs dataset
uniq_avg_ann_coh <- suppressWarnings(left_join(uniq_prov_pairs, avg_ann_coh,
  by = c("ref_prov", "other_prov")
))

# 3: Time-varying annual phase difference (timing offset between country cycles)
ann_phase <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  ann_phase[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = prov_combos$phase[[i]],
    time = prov_combos$times[[i]],
    scales = prov_combos$scales[[i]],
    coi = prov_combos$coi[[i]],
    low = 8, high = 16
  ))
  ann_phase[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  ann_phase[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
ann_phase <- bind_rows(ann_phase)
ann_phase$est_lag <- phaseToTime(ann_phase$est, p = 11.3, dt = 1)

# 4: Time-averaged annual phase difference
avg_ann_phase <- list(NULL)
for (i in 1:nrow(prov_combos)) {
  avg_ann_phase[[i]] <- extract_avg_coh_phase_coi_updated(prov_combos$phase[[i]],
    prov_combos$scales[[i]],
    prov_combos$coi[[i]],
    low = 8, high = 16
  )
  avg_ann_phase[[i]]$ref_prov <- prov_combos$ref_prov[[i]]
  avg_ann_phase[[i]]$other_prov <- prov_combos$other_prov[[i]]
}
avg_ann_phase <- bind_rows(avg_ann_phase)
avg_ann_phase$phase_lag <- phaseToTime(avg_ann_phase$est, p = 11.3, dt = 1)

uniq_avg_ann_phase <- suppressWarnings(left_join(uniq_prov_pairs, avg_ann_phase,
  by = c("ref_prov", "other_prov")
))

# ------------------------------------------------------------------------------
# EXTRACT MULTIANNUAL BAND COHERENCE AND PHASE (>17 months)
# ------------------------------------------------------------------------------
# Same as earlier but for the MULTIANNUAL band (periods > 17 months).
# Only includes countries with 10+ years of data (mlt_countries).
#
# Key outputs:
#   - mlt_coh: Time-varying multiannual coherence
#   - avg_mlt_coh: Time-averaged multiannual coherence
#   - mlt_phase: Time-varying multiannual phase difference
#   - avg_mlt_phase: Time-averaged multiannual phase difference

# Filter to countries with 10+ years of data for multiannual analysis
mlt_combos <- prov_df %>% filter(country %in% mlt_countries)
mlt_combos <- tidyr::crossing(
  ref_prov = mlt_combos$province,
  other_prov = mlt_combos$province
) %>%
  filter(ref_prov != other_prov)
mlt_combos <- left_join(mlt_combos, prov_combos)

# Time-varying multiannual coherence
mlt_coh <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  mlt_coh[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = mlt_combos$coherence[[i]],
    time = mlt_combos$times[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  ))

  mlt_coh[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  mlt_coh[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
mlt_coh <- bind_rows(mlt_coh)
# Unique pairs
uniq_mlt_coh <- suppressWarnings(left_join(uniq_prov_pairs, mlt_coh,
  by = c("ref_prov", "other_prov")
))

# Extracting the average multiannual coherence of dengue cycles comparing location-pairs
avg_mlt_coh <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  avg_mlt_coh[[i]] <- extract_avg_coh_phase_coi_updated(
    dat = mlt_combos$coherence[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  )
  avg_mlt_coh[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  avg_mlt_coh[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
avg_mlt_coh <- bind_rows(avg_mlt_coh)

# unique pairs
uniq_avg_mlt_coh <- suppressWarnings(left_join(uniq_prov_pairs,
  avg_mlt_coh,
  by = c("ref_prov", "other_prov")
))

# Extracting the multiannual phase over time for location-pairs
mlt_phase <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  mlt_phase[[i]] <- bind_rows(extract_coh_phase_time_coi_updated(
    dat = mlt_combos$phase[[i]], time = mlt_combos$times[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  ))
  mlt_phase[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  mlt_phase[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
mlt_phase <- bind_rows(mlt_phase)
mlt_phase$est_lag <- phaseToTime(mlt_phase$est, p = 33, dt = 1)

# Extracting the average annual phase differences of dengue cycles comparing location-pairs
avg_mlt_phase <- list(NULL)
for (i in 1:nrow(mlt_combos)) {
  avg_mlt_phase[[i]] <- extract_avg_coh_phase_coi_updated(
    dat = mlt_combos$phase[[i]],
    scales = mlt_combos$scales[[i]],
    coi = mlt_combos$coi[[i]],
    low = 17
  )
  avg_mlt_phase[[i]]$ref_prov <- mlt_combos$ref_prov[[i]]
  avg_mlt_phase[[i]]$other_prov <- mlt_combos$other_prov[[i]]
}
avg_mlt_phase <- bind_rows(avg_mlt_phase)
avg_mlt_phase$phase_lag <- phaseToTime(avg_mlt_phase$est, p = 33, dt = 1)

uniq_avg_mlt_phase <- suppressWarnings(left_join(uniq_prov_pairs, avg_mlt_phase,
  by = c("ref_prov", "other_prov")
))


# ==============================================================================
# NON-OVERLAPPING WINDOW EXTRACTION
# ==============================================================================
# Helper function for circular mean (for phase)
mean_phase <- function(phase, na.rm = FALSE) {
  atan2(mean(sin(phase), na.rm = na.rm), mean(cos(phase), na.rm = na.rm))
}

min_year <- floor(min(all_cases$time))
max_year <- floor(max(all_cases$time))

# Helper function to create non-overlapping windows
create_stage1_nonoverlap <- function(coh_data, phase_data, window_size) {
  # Create non-overlapping windows
  begin_years <- seq(min_year, max_year - window_size + 1, by = window_size)
  end_years <- begin_years + window_size

  # Coherence
  coh_result <- bind_rows(lapply(seq_along(begin_years), function(i) {
    start_yr <- begin_years[i]
    end_yr <- end_years[i]
    window_data <- coh_data %>% filter(time >= start_yr & time < end_yr)
    if (nrow(window_data) > 0) {
      window_data %>%
        group_by(ref_prov, other_prov) %>%
        summarize(est = mean(est, na.rm = TRUE), .groups = "drop") %>%
        mutate(yr_midpoint = median(c(start_yr, end_yr - 1)))
    }
  }))

  # Phase
  phase_result <- bind_rows(lapply(seq_along(begin_years), function(i) {
    start_yr <- begin_years[i]
    end_yr <- end_years[i]
    window_data <- phase_data %>% filter(time >= start_yr & time < end_yr)
    if (nrow(window_data) > 0) {
      window_data %>%
        group_by(ref_prov, other_prov) %>%
        summarize(est = mean_phase(est, na.rm = TRUE), .groups = "drop") %>%
        mutate(yr_midpoint = median(c(start_yr, end_yr - 1)))
    }
  }))

  list(coh = coh_result, phase = phase_result)
}

# 5-year non-overlapping for annual
result_5yr_nonoverlap <- create_stage1_nonoverlap(ann_coh, ann_phase, 5)
ann_coh_5yr_nonoverlap <- result_5yr_nonoverlap$coh
ann_phase_5yr_nonoverlap <- result_5yr_nonoverlap$phase
cat("  5-year non-overlapping:", nrow(ann_coh_5yr_nonoverlap), "coherence rows\n")


# 5-year non-overlapping for multiannual
result_mlt_5yr_nonoverlap <- create_stage1_nonoverlap(mlt_coh, mlt_phase, 5)
mlt_coh_5yr_nonoverlap <- result_mlt_5yr_nonoverlap$coh
mlt_phase_5yr_nonoverlap <- result_mlt_5yr_nonoverlap$phase
cat("  Multiannual 5-year non-overlapping:", nrow(mlt_coh_5yr_nonoverlap), "coherence rows\n")





# ==============================================================================
# SYNCHRONY TREND ANALYSIS - Using 5-year NON-OVERLAPPING windows
# ==============================================================================

# Annual synchrony
pairwise_sync_ann <- left_join(
  ann_coh_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, coh_est = est),
  ann_phase_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, phase_est = est),
  by = c("ref_prov", "other_prov", "yr_midpoint")
) %>%
  mutate(phase_adj = 1 - abs(phase_est / pi), synchrony = coh_est * phase_adj) %>%
  filter(!is.na(synchrony)) %>%
  filter(ref_prov < other_prov) %>%
  mutate(pair_id = paste(ref_prov, other_prov, sep = "_"))

# Multiannual synchrony
pairwise_sync_mlt <- left_join(
  mlt_coh_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, coh_est = est),
  mlt_phase_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, phase_est = est),
  by = c("ref_prov", "other_prov", "yr_midpoint")
) %>%
  mutate(phase_adj = 1 - abs(phase_est / pi), synchrony = coh_est * phase_adj) %>%
  filter(!is.na(synchrony)) %>%
  filter(ref_prov < other_prov) %>%
  mutate(pair_id = paste(ref_prov, other_prov, sep = "_"))

cat("  Annual pairs (unique):", dplyr::n_distinct(pairwise_sync_ann$pair_id), "\n")
cat("  Annual observations:", nrow(pairwise_sync_ann), "(", nrow(pairwise_sync_ann) / dplyr::n_distinct(pairwise_sync_ann$pair_id), "windows/pair avg)\n")
cat("  Multiannual pairs (unique):", dplyr::n_distinct(pairwise_sync_mlt$pair_id), "\n")
cat("  Multiannual observations:", nrow(pairwise_sync_mlt), "(", nrow(pairwise_sync_mlt) / dplyr::n_distinct(pairwise_sync_mlt$pair_id), "windows/pair avg)\n")


# ==============================================================================
# INTERACTION MODELS: Regional Slope Comparison
# ==============================================================================

# Helpers
logit_safe <- function(p, eps = 1e-6) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p / (1 - p))
}
inv_logit <- function(x) 1 / (1 + exp(-x))

p_from_draws <- function(x) {
  2 * min(mean(x > 0, na.rm = TRUE), mean(x < 0, na.rm = TRUE))
}

# Robust coefficient name picker (avoids silent “wrong name => treated as 0”)
pick_coef <- function(patterns, coef_names) {
  hits <- coef_names
  for (p in patterns) hits <- hits[grepl(p, hits)]
  if (length(hits) == 1) hits else NA_character_
}


# Pair type classification ###
create_pair_type <- function(sync_data, country_region_broad_map) {
  region_lookup <- country_region_broad_map %>%
    dplyr::select(country, broad_region)

  sync_data %>%
    left_join(region_lookup, by = c("ref_prov" = "country")) %>%
    rename(ref_region = broad_region) %>%
    left_join(region_lookup, by = c("other_prov" = "country")) %>%
    rename(other_region = broad_region) %>%
    filter(!is.na(ref_region), !is.na(other_region)) %>%
    mutate(
      pair_type = case_when(
        ref_region == "Americas" & other_region == "Americas" ~ "Within Americas",
        ref_region == "Asia" & other_region == "Asia" ~ "Within Asia",
        TRUE ~ "Americas-Asia"
      ),
      pair_type = factor(pair_type, levels = c("Within Americas", "Within Asia", "Americas-Asia"))
    )
}

# Prepare synchrony data (annual + multiannual)
sync_int_ann <- pairwise_sync_ann %>%
  filter(
    ref_prov %in% endemic_and_eligible_ann,
    other_prov %in% endemic_and_eligible_ann
  ) %>%
  create_pair_type(country_broad_region_map) %>%
  mutate(
    sync_logit = logit_safe(synchrony),
    year_centered = as.numeric(scale(yr_midpoint))
  )

sync_int_mlt <- pairwise_sync_mlt %>%
  filter(
    ref_prov %in% endemic_and_eligible_mlt,
    other_prov %in% endemic_and_eligible_mlt
  ) %>%
  create_pair_type(country_broad_region_map) %>%
  mutate(
    sync_logit = logit_safe(synchrony),
    year_centered = as.numeric(scale(yr_midpoint))
  )

# Fit interaction models
cat("\nFitting Annual synchrony interaction model...\n")
sync_int_model_ann <- tryCatch(
  {
    lmerTest::lmer(
      sync_logit ~ year_centered * pair_type +
        (1 | ref_prov) + (1 | other_prov) + (year_centered | pair_id),
      data = sync_int_ann,
      REML = TRUE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
    )
  },
  error = function(e) {
    cat("  Warning: (year_centered|pair_id) failed; using (1|pair_id) only.\n")
    lmerTest::lmer(
      sync_logit ~ year_centered * pair_type +
        (1 | ref_prov) + (1 | other_prov) + (1 | pair_id),
      data = sync_int_ann,
      REML = TRUE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
    )
  }
)

cat("Fitting Multiannual synchrony interaction model...\n")
sync_int_model_mlt <- tryCatch(
  {
    lmerTest::lmer(
      sync_logit ~ year_centered * pair_type +
        (1 | ref_prov) + (1 | other_prov) + (year_centered | pair_id),
      data = sync_int_mlt,
      REML = TRUE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
    )
  },
  error = function(e) {
    cat("  Warning: (year_centered|pair_id) failed; using (1|pair_id) only.\n")
    lmerTest::lmer(
      sync_logit ~ year_centered * pair_type +
        (1 | ref_prov) + (1 | other_prov) + (1 | pair_id),
      data = sync_int_mlt,
      REML = TRUE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
    )
  }
)


# Extraction helper
extract_sync_interaction_results_raw_global <- function(
    model,
    data,
    cycle_name,
    n_sim = 2000,
    seed = 123,
    weight_by = c("pairs", "countries"),
    eps_sync = 1e-6,
    use_nearPD_if_needed = TRUE) {
  weight_by <- match.arg(weight_by)
  stopifnot(all(c("pair_type", "pair_id", "ref_prov", "other_prov", "yr_midpoint", "sync_logit", "year_centered") %in% names(data)))

  mu <- lme4::fixef(model)
  V <- as.matrix(vcov(model))
  V <- (V + t(V)) / 2

  if (use_nearPD_if_needed) {
    evmin <- min(eigen(V, symmetric = TRUE, only.values = TRUE)$values)
    if (!is.finite(evmin) || evmin <= 0) {
      if (!requireNamespace("Matrix", quietly = TRUE)) {
        stop("vcov(model) not PD and Matrix package not available for nearPD().")
      }
      V <- as.matrix(Matrix::nearPD(V, corr = FALSE)$mat)
    }
  }

  # lmerTest summary table for interaction p-values
  tt <- summary(model)$coefficients

  min_year <- min(data$yr_midpoint, na.rm = TRUE)
  max_year <- max(data$yr_midpoint, na.rm = TRUE)
  mean_year <- mean(data$yr_midpoint, na.rm = TRUE)
  sd_year <- sd(data$yr_midpoint, na.rm = TRUE)
  n_years <- max_year - min_year

  if (!is.finite(sd_year) || sd_year <= 0 || !is.finite(n_years) || n_years <= 0) {
    return(data.frame(
      Cycle = cycle_name,
      Pair_Type = c("Global", "Within Americas", "Within Asia", "Americas-Asia"),
      Weighting = weight_by,
      N_pairs = NA_integer_,
      Start_Year = round(min_year), End_Year = round(max_year),
      Sync_Start = NA_real_, Sync_End = NA_real_,
      Pct_Change_Per_Year = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_,
      Trend_P = NA_real_, Interaction_P = NA_real_,
      Diff_vs_Americas = NA_real_, Diff_CI_Lower = NA_real_, Diff_CI_Upper = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  x_start <- (min_year - mean_year) / sd_year
  x_end <- (max_year - mean_year) / sd_year

  coef_names <- names(mu)

  # Core names
  nm_year <- "year_centered"

  # Prefer exact names; otherwise try robust pattern match
  nm_type_asia <- if ("pair_typeWithin Asia" %in% coef_names) {
    "pair_typeWithin Asia"
  } else if ("pair_typeWithin.Asia" %in% coef_names) {
    "pair_typeWithin.Asia"
  } else {
    pick_coef(c("^pair_type", "Within", "Asia"), coef_names)
  }

  nm_type_cross <- if ("pair_typeAmericas-Asia" %in% coef_names) {
    "pair_typeAmericas-Asia"
  } else if ("pair_typeAmericas.Asia" %in% coef_names) {
    "pair_typeAmericas.Asia"
  } else {
    pick_coef(c("^pair_type", "Americas", "Asia"), coef_names)
  }

  nm_int_asia <- if ("year_centered:pair_typeWithin Asia" %in% coef_names) {
    "year_centered:pair_typeWithin Asia"
  } else if ("year_centered:pair_typeWithin.Asia" %in% coef_names) {
    "year_centered:pair_typeWithin.Asia"
  } else {
    pick_coef(c("^year_centered:pair_type", "Within", "Asia"), coef_names)
  }

  nm_int_cross <- if ("year_centered:pair_typeAmericas-Asia" %in% coef_names) {
    "year_centered:pair_typeAmericas-Asia"
  } else if ("year_centered:pair_typeAmericas.Asia" %in% coef_names) {
    "year_centered:pair_typeAmericas.Asia"
  } else {
    pick_coef(c("^year_centered:pair_type", "Americas", "Asia"), coef_names)
  }

  getb <- function(nm_i, default = 0) if (!is.na(nm_i) && nm_i %in% coef_names) unname(mu[[nm_i]]) else default

  b0 <- getb("(Intercept)")
  bY <- getb(nm_year)
  b_asia <- getb(nm_type_asia)
  b_cross <- getb(nm_type_cross)
  b_int_asia <- getb(nm_int_asia)
  b_int_cross <- getb(nm_int_cross)

  calc_point <- function(b_type, b_int) {
    eta_s <- b0 + b_type + (bY + b_int) * x_start
    eta_e <- b0 + b_type + (bY + b_int) * x_end
    s_s <- inv_logit(eta_s)
    s_e <- inv_logit(eta_e)
    s_s <- pmin(pmax(s_s, eps_sync), 1 - eps_sync)
    s_e <- pmin(pmax(s_e, eps_sync), 1 - eps_sync)
    annual_change <- (s_e - s_s) / n_years
    pct <- (annual_change / s_s) * 100
    list(sync_start = s_s, sync_end = s_e, pct = pct)
  }

  pe_A <- calc_point(0, 0)
  pe_As <- calc_point(b_asia, b_int_asia)
  pe_C <- calc_point(b_cross, b_int_cross)

  # weights for global row
  n_pairs_A <- dplyr::n_distinct(data$pair_id[data$pair_type == "Within Americas"])
  n_pairs_As <- dplyr::n_distinct(data$pair_id[data$pair_type == "Within Asia"])
  n_pairs_C <- dplyr::n_distinct(data$pair_id[data$pair_type == "Americas-Asia"])
  n_pairs_tot <- dplyr::n_distinct(data$pair_id)

  if (weight_by == "pairs") {
    w <- c(A = n_pairs_A, As = n_pairs_As, C = n_pairs_C)
  } else {
    get_n_countries_for_type <- function(pt) {
      d <- data %>% dplyr::filter(pair_type == pt)
      dplyr::n_distinct(c(d$ref_prov, d$other_prov))
    }
    w <- c(
      A  = get_n_countries_for_type("Within Americas"),
      As = get_n_countries_for_type("Within Asia"),
      C  = get_n_countries_for_type("Americas-Asia")
    )
  }

  w_sum <- sum(w)
  if (!is.finite(w_sum) || w_sum <= 0) stop("Global weights sum to zero.")

  wA <- w["A"] / w_sum
  wAs <- w["As"] / w_sum
  wC <- w["C"] / w_sum

  sync_start_global <- wA * pe_A$sync_start + wAs * pe_As$sync_start + wC * pe_C$sync_start
  sync_end_global <- wA * pe_A$sync_end + wAs * pe_As$sync_end + wC * pe_C$sync_end
  pct_global <- (((sync_end_global - sync_start_global) / n_years) / sync_start_global) * 100

  # ---- Simulation CI + Trend p-values from %Change/yr draws (RAW SCALE) ----
  set.seed(seed)
  draws <- MASS::mvrnorm(n_sim, mu = mu, Sigma = V)

  dcol <- function(nm_i) if (!is.na(nm_i) && nm_i %in% colnames(draws)) draws[, nm_i] else rep(0, n_sim)

  d_b0 <- dcol("(Intercept)")
  d_bY <- dcol(nm_year)
  d_b_asia <- dcol(nm_type_asia)
  d_b_cross <- dcol(nm_type_cross)
  d_int_asia <- dcol(nm_int_asia)
  d_int_cross <- dcol(nm_int_cross)

  sim_pct_type <- function(d_type, d_int) {
    eta_s <- d_b0 + d_type + (d_bY + d_int) * x_start
    eta_e <- d_b0 + d_type + (d_bY + d_int) * x_end
    s_s <- inv_logit(eta_s)
    s_e <- inv_logit(eta_e)
    s_s <- pmin(pmax(s_s, eps_sync), 1 - eps_sync)
    s_e <- pmin(pmax(s_e, eps_sync), 1 - eps_sync)
    (((s_e - s_s) / n_years) / s_s) * 100
  }

  pctA_draws <- sim_pct_type(0, 0)
  pctAs_draws <- sim_pct_type(d_b_asia, d_int_asia)
  pctC_draws <- sim_pct_type(d_b_cross, d_int_cross)

  sim_starts_ends <- function(d_type, d_int) {
    eta_s <- d_b0 + d_type + (d_bY + d_int) * x_start
    eta_e <- d_b0 + d_type + (d_bY + d_int) * x_end
    s_s <- inv_logit(eta_s)
    s_e <- inv_logit(eta_e)
    s_s <- pmin(pmax(s_s, eps_sync), 1 - eps_sync)
    s_e <- pmin(pmax(s_e, eps_sync), 1 - eps_sync)
    list(s_start = s_s, s_end = s_e)
  }

  seA <- sim_starts_ends(0, 0)
  seAs <- sim_starts_ends(d_b_asia, d_int_asia)
  seC <- sim_starts_ends(d_b_cross, d_int_cross)

  s_start_g <- wA * seA$s_start + wAs * seAs$s_start + wC * seC$s_start
  s_end_g <- wA * seA$s_end + wAs * seAs$s_end + wC * seC$s_end
  pctG_draws <- (((s_end_g - s_start_g) / n_years) / s_start_g) * 100

  clean <- function(x) x[is.finite(x)]
  safe_ci <- function(x) if (length(x) >= 10) quantile(x, c(0.025, 0.975), na.rm = TRUE) else c(NA, NA)

  ciA <- safe_ci(clean(pctA_draws))
  ciAs <- safe_ci(clean(pctAs_draws))
  ciC <- safe_ci(clean(pctC_draws))
  ciG <- safe_ci(clean(pctG_draws))

  # --- CUMULATIVE % change from start year to end year ---
  # Point estimates
  pctA_cum <- ((pe_A$sync_end - pe_A$sync_start) / pe_A$sync_start) * 100
  pctAs_cum <- ((pe_As$sync_end - pe_As$sync_start) / pe_As$sync_start) * 100
  pctC_cum <- ((pe_C$sync_end - pe_C$sync_start) / pe_C$sync_start) * 100
  pctG_cum <- ((sync_end_global - sync_start_global) / sync_start_global) * 100

  # Simulation-based CIs for cumulative change
  pctA_cum_draws <- ((seA$s_end - seA$s_start) / seA$s_start) * 100
  pctAs_cum_draws <- ((seAs$s_end - seAs$s_start) / seAs$s_start) * 100
  pctC_cum_draws <- ((seC$s_end - seC$s_start) / seC$s_start) * 100
  pctG_cum_draws <- ((s_end_g - s_start_g) / s_start_g) * 100

  ciA_cum <- safe_ci(clean(pctA_cum_draws))
  ciAs_cum <- safe_ci(clean(pctAs_cum_draws))
  ciC_cum <- safe_ci(clean(pctC_cum_draws))
  ciG_cum <- safe_ci(clean(pctG_cum_draws))

  # Trend p-values (raw-scale %Change/yr)
  p_trend_A <- p_from_draws(pctA_draws)
  p_trend_As <- p_from_draws(pctAs_draws)
  p_trend_C <- p_from_draws(pctC_draws)
  p_trend_G <- p_from_draws(pctG_draws)

  # Differences vs Americas (raw-scale %Change/yr) + CI
  diffAs <- pe_As$pct - pe_A$pct
  diffC <- pe_C$pct - pe_A$pct
  ciDiffAs <- safe_ci(clean(pctAs_draws - pctA_draws))
  ciDiffC <- safe_ci(clean(pctC_draws - pctA_draws))

  # Interaction p-values (model-based slope differences vs reference)
  p_int_asia <- if (!is.na(nm_int_asia) && nm_int_asia %in% rownames(tt)) tt[nm_int_asia, "Pr(>|t|)"] else NA_real_
  p_int_cross <- if (!is.na(nm_int_cross) && nm_int_cross %in% rownames(tt)) tt[nm_int_cross, "Pr(>|t|)"] else NA_real_

  data.frame(
    Cycle = cycle_name,
    Pair_Type = c("Global", "Within Americas", "Within Asia", "Americas-Asia"),
    Weighting = weight_by,
    N_pairs = c(n_pairs_tot, n_pairs_A, n_pairs_As, n_pairs_C),
    Start_Year = round(min_year),
    End_Year = round(max_year),
    Sync_Start = c(sync_start_global, pe_A$sync_start, pe_As$sync_start, pe_C$sync_start),
    Sync_End = c(sync_end_global, pe_A$sync_end, pe_As$sync_end, pe_C$sync_end),
    Pct_Change_Per_Year = c(pct_global, pe_A$pct, pe_As$pct, pe_C$pct),
    CI_Lower = c(ciG[[1]], ciA[[1]], ciAs[[1]], ciC[[1]]),
    CI_Upper = c(ciG[[2]], ciA[[2]], ciAs[[2]], ciC[[2]]),
    Trend_P = c(p_trend_G, p_trend_A, p_trend_As, p_trend_C),
    Interaction_P = c(NA_real_, NA_real_, p_int_asia, p_int_cross),
    Diff_vs_Americas = c(NA_real_, NA_real_, diffAs, diffC),
    Diff_CI_Lower = c(NA_real_, NA_real_, ciDiffAs[[1]], ciDiffC[[1]]),
    Diff_CI_Upper = c(NA_real_, NA_real_, ciDiffAs[[2]], ciDiffC[[2]]),
    Pct_Change_Cum = c(pctG_cum, pctA_cum, pctAs_cum, pctC_cum),
    Cum_CI_Lower = c(ciG_cum[[1]], ciA_cum[[1]], ciAs_cum[[1]], ciC_cum[[1]]),
    Cum_CI_Upper = c(ciG_cum[[2]], ciA_cum[[2]], ciAs_cum[[2]], ciC_cum[[2]]),
    stringsAsFactors = FALSE
  )
}

# Run extraction (annual + multiannual)
sync_int_results_ann <- extract_sync_interaction_results_raw_global(
  sync_int_model_ann, sync_int_ann, "Annual",
  n_sim = 1000, seed = 123, weight_by = "pairs"
)

sync_int_results_mlt <- extract_sync_interaction_results_raw_global(
  sync_int_model_mlt, sync_int_mlt, "Multiannual",
  n_sim = 1000, seed = 123, weight_by = "pairs"
)

sync_int_results <- dplyr::bind_rows(sync_int_results_ann, sync_int_results_mlt)

# Pair-level % increasing counts
pair_trend_counts <- function(df, cycle_name, min_windows = 5) {
  per_pair <- df %>%
    dplyr::filter(!is.na(synchrony), !is.na(yr_midpoint)) %>%
    dplyr::group_by(pair_type, pair_id) %>%
    dplyr::filter(dplyr::n() >= min_windows) %>%
    dplyr::group_modify(~ {
      tmp <- data.frame(t = .x$yr_midpoint, y = logit_safe(.x$synchrony))
      fit <- tryCatch(stats::lm(y ~ t, data = tmp), error = function(e) NULL)
      if (is.null(fit)) {
        return(data.frame(is_inc = NA))
      }
      ci <- tryCatch(stats::confint(fit, "t", level = 0.95), error = function(e) c(NA_real_, NA_real_))
      data.frame(is_inc = is.finite(ci[1]) && is.finite(ci[2]) && (ci[1] > 0))
    }) %>%
    dplyr::ungroup()

  by_type <- per_pair %>%
    dplyr::group_by(pair_type) %>%
    dplyr::summarise(
      N_Tot = sum(!is.na(is_inc)),
      N_Inc = sum(is_inc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Cycle = cycle_name,
      Pair_Type = as.character(pair_type),
      Pct_Inc = ifelse(N_Tot > 0, 100 * N_Inc / N_Tot, NA_real_)
    ) %>%
    dplyr::select(Cycle, Pair_Type, N_Inc, N_Tot, Pct_Inc)

  global <- per_pair %>%
    dplyr::group_by(pair_id) %>%
    dplyr::summarise(is_inc = dplyr::first(is_inc), .groups = "drop") %>%
    dplyr::summarise(
      N_Tot = sum(!is.na(is_inc)),
      N_Inc = sum(is_inc, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      Cycle = cycle_name,
      Pair_Type = "Global",
      Pct_Inc = ifelse(N_Tot > 0, 100 * N_Inc / N_Tot, NA_real_)
    ) %>%
    dplyr::select(Cycle, Pair_Type, N_Inc, N_Tot, Pct_Inc)

  dplyr::bind_rows(global, by_type)
}

inc_df <- dplyr::bind_rows(
  pair_trend_counts(sync_int_ann, "Annual", min_windows = 5),
  pair_trend_counts(sync_int_mlt, "Multiannual", min_windows = 5)
)

sync_int_results <- sync_int_results %>%
  dplyr::left_join(inc_df, by = c("Cycle", "Pair_Type")) %>%
  mutate(Pair_Type = factor(Pair_Type, levels = c("Global", "Within Americas", "Within Asia", "Americas-Asia"))) %>%
  arrange(Cycle, Pair_Type)

# Print table
fmt_p <- function(p, digits = 2, cutoff = 1e-3) {
  if (is.na(p)) {
    return("")
  }

  # Main tiny cutoff label, e.g. <0.001
  cutoff_digits <- max(0, ceiling(-log10(cutoff)))
  cutoff_label <- paste0("<", formatC(cutoff, format = "f", digits = cutoff_digits))

  # Smallest value representable with requested decimals, e.g. 0.01 for digits=2
  min_printable <- 10^(-digits)
  min_label <- paste0("<", formatC(min_printable, format = "f", digits = digits))

  if (p < cutoff) {
    cutoff_label
  } else if (p < min_printable) {
    min_label
  } else {
    formatC(p, format = "f", digits = digits)
  }
}


{
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                                   SYNCHRONY INTERACTION MODEL RESULTS (Endemic Only)                                                               ║\n")
  cat("║                        Model: logit(sync) ~ year * pair_type + (1|ref) + (1|other) + (year|pair)                                                    ║\n")
  cat("║                        %Change/yr shown on original synchrony scale [0–1] (average over period)                                                     ║\n")
  cat("╠════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╣\n")
  cat("║ Cycle       | Pair Type        | N pairs | Start Sync | End Sync | %Change/yr (95% CI)        | p(trend)   | p(diff vs Americas) | % Increasing (X/Y)   ║\n")
  cat("╟─────────────┼──────────────────┼─────────┼────────────┼──────────┼───────────────────────────┼────────────┼─────────────────────┼──────────────────────╢\n")

  for (i in 1:nrow(sync_int_results)) {
    r <- sync_int_results[i, ]

    p_tr <- fmt_p(r$Trend_P, digits = 2, cutoff = 1e-3)

    p_int <- dplyr::case_when(
      as.character(r$Pair_Type) == "Within Americas" ~ "(reference)",
      is.na(r$Interaction_P) ~ "(n/a)",
      TRUE ~ fmt_p(r$Interaction_P, digits = 2, cutoff = 1e-3)
    )


    inc_str <- ifelse(
      is.na(r$N_Inc) || is.na(r$N_Tot) || r$N_Tot == 0,
      "(n/a)",
      sprintf("%d/%d (%5.1f%%)", r$N_Inc, r$N_Tot, r$Pct_Inc)
    )

    cat(sprintf(
      "║ %-11s | %-16s | %7d | %10.2f | %8.2f | %6.2f%% (%6.2f, %6.2f) | %10s | %19s | %-20s ║\n",
      r$Cycle, as.character(r$Pair_Type), r$N_pairs,
      r$Sync_Start, r$Sync_End,
      r$Pct_Change_Per_Year, r$CI_Lower, r$CI_Upper,
      p_tr, p_int, inc_str
    ))
  }
}

write.csv(sync_int_results, "output/tables/wavelet_sync_interaction_results.csv", row.names = FALSE)


# ==============================================================================
# HEMISPHERE PAIR-GROUP SYNCHRONY ANALYSIS
# ==============================================================================
# Compare synchrony by hemisphere pair-groups (N-N, S-S, N-S)

# Step 1: Add hemisphere label to each country based on centroid latitude
hemisphere_lookup <- prov_coords %>%
  select(co_province, lat) %>%
  distinct() %>%
  mutate(hemisphere = ifelse(lat >= 0, "North", "South"))

cat("Countries by hemisphere:\n")
cat("  Northern:", sum(hemisphere_lookup$hemisphere == "North"), "\n")
cat("  Southern:", sum(hemisphere_lookup$hemisphere == "South"), "\n")

# Annual synchrony - from 5-year non-overlapping
pairwise_sync_ann <- left_join(
  ann_coh_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, coh_est = est),
  ann_phase_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, phase_est = est),
  by = c("ref_prov", "other_prov", "yr_midpoint")
) %>%
  mutate(phase_adj = 1 - abs(phase_est / pi), synchrony = coh_est * phase_adj) %>%
  filter(!is.na(synchrony)) %>%
  filter(ref_prov < other_prov) %>%
  mutate(pair_id = paste(ref_prov, other_prov, sep = "_"))

# Multiannual synchrony - from 5-year non-overlapping
pairwise_sync_mlt <- left_join(
  mlt_coh_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, coh_est = est),
  mlt_phase_5yr_nonoverlap %>% dplyr::select(ref_prov, other_prov, yr_midpoint, phase_est = est),
  by = c("ref_prov", "other_prov", "yr_midpoint")
) %>%
  mutate(phase_adj = 1 - abs(phase_est / pi), synchrony = coh_est * phase_adj) %>%
  filter(!is.na(synchrony)) %>%
  filter(ref_prov < other_prov) %>%
  mutate(pair_id = paste(ref_prov, other_prov, sep = "_"))


# Step 2: Add hemisphere labels to annual and multiannual sync data
add_hemisphere_info <- function(sync_data) {
  sync_data %>%
    left_join(hemisphere_lookup %>% select(co_province, hemisphere),
      by = c("ref_prov" = "co_province")
    ) %>%
    rename(ref_hemisphere = hemisphere) %>%
    left_join(hemisphere_lookup %>% select(co_province, hemisphere),
      by = c("other_prov" = "co_province")
    ) %>%
    rename(other_hemisphere = hemisphere) %>%
    filter(!is.na(ref_hemisphere), !is.na(other_hemisphere)) %>%
    mutate(
      hemi_pair = case_when(
        ref_hemisphere == "North" & other_hemisphere == "North" ~ "N-N",
        ref_hemisphere == "South" & other_hemisphere == "South" ~ "S-S",
        TRUE ~ "N-S"
      ),
      # Within vs Between hemisphere classification (collapsed)
      hemi_type = ifelse(hemi_pair == "N-S", "Between", "Within")
    )
}

pairwise_sync_hemi_ann <- add_hemisphere_info(pairwise_sync_ann) %>%
  filter(
    ref_prov %in% endemic_and_eligible_ann,
    other_prov %in% endemic_and_eligible_ann
  )
pairwise_sync_hemi_mlt <- add_hemisphere_info(pairwise_sync_mlt) %>%
  filter(
    ref_prov %in% endemic_and_eligible_mlt,
    other_prov %in% endemic_and_eligible_mlt
  )

# Summarize pair counts
cat("\nAnnual - Pairs by hemisphere type:\n")
pairwise_sync_hemi_ann %>%
  distinct(pair_id, hemi_type) %>%
  count(hemi_type) %>%
  print()

cat("\nMultiannual - Pairs by hemisphere type:\n")
pairwise_sync_hemi_mlt %>%
  distinct(pair_id, hemi_type) %>%
  count(hemi_type) %>%
  print()

# Step 3: Test difference in mean synchrony: Within vs Between hemisphere
# Mixed effects model: logit(sync) ~ hemi_type + (1|ref_prov) + (1|other_prov)
# Within = N-N + S-S (same hemisphere), Between = N-S (cross-equatorial)

# ---- helpers ----
logit_safe <- function(p, eps = 1e-6) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p / (1 - p))
}
inv_logit <- function(x) plogis(x)

# ---- main function ----
fit_hemi_model <- function(sync_data_hemi,
                           cycle_name,
                           group_var = "hemi_type", # e.g., "Within"/"Between"
                           ref_level = "Between", # set reference
                           eps = 1e-6,
                           n_sims = 20000,
                           optimizer = "bobyqa") {
  # 0) Ensure unordered pair_id exists and is consistent
  df0 <- sync_data_hemi

  # 1) Collapse to ONE synchrony per unordered pair (across all time windows)
  pair_means <- df0 %>%
    group_by(pair_id, ref_prov, other_prov, !!sym(group_var)) %>%
    summarise(mean_sync = mean(synchrony, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      group = factor(.data[[group_var]]),
      group = relevel(group, ref = ref_level),
      sync_logit = logit_safe(mean_sync, eps = eps)
    )

  # Sanity: one row per pair_id
  stopifnot(nrow(pair_means) == n_distinct(pair_means$pair_id))

  # 2) Fit mixed model with crossed country random intercepts
  model <- lmer(
    sync_logit ~ group + (1 | ref_prov) + (1 | other_prov),
    data = pair_means,
    REML = TRUE,
    control = lmerControl(optimizer = optimizer)
  )

  # 3) Model-estimated marginal means (logit scale) and back-transform to raw scale
  emm_logit <- emmeans(model, ~group)
  emm_ci <- confint(emm_logit) # logit-scale CI

  emm_raw <- as.data.frame(emm_ci) %>%
    transmute(
      cycle = cycle_name,
      group = group,
      emmean_logit = emmean,
      lo_logit = lower.CL,
      hi_logit = upper.CL,
      emmean = inv_logit(emmean),
      lo = inv_logit(lower.CL),
      hi = inv_logit(upper.CL)
    )

  # 4) Contrasts on logit scale + OR (odds ratio of the mean synchrony metric)
  contrasts_logit <- as.data.frame(pairs(emm_logit, adjust = "none")) %>%
    mutate(
      cycle = cycle_name,
      OR = exp(estimate),
      OR_lo = exp(estimate - 1.96 * SE),
      OR_hi = exp(estimate + 1.96 * SE)
    )

  # 5) Observed summaries (raw scale) + n_pairs
  obs_summary <- pair_means %>%
    group_by(group) %>%
    summarise(
      cycle = cycle_name,
      n_pairs = n(),
      mean_obs = mean(mean_sync, na.rm = TRUE),
      se_obs = sd(mean_sync, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # 6) Raw-scale difference and 95% CI via simulation from emmeans covariance
  #    We'll compute Within - Between if those exist; otherwise the second - first level.
  emm_tbl <- as.data.frame(emm_logit)
  V <- vcov(emm_logit) # covariance matrix of marginal means (logit scale)

  sims <- MASS::mvrnorm(n_sims, mu = emm_tbl$emmean, Sigma = V)
  colnames(sims) <- as.character(emm_tbl$group)

  levs <- levels(pair_means$group)
  # Prefer explicit labels if present
  if (all(c("Within", "Between") %in% levs)) {
    g_hi <- "Within"
    g_lo <- "Between"
  } else {
    # otherwise take last - first
    g_lo <- levs[1]
    g_hi <- levs[length(levs)]
  }

  diff_raw <- inv_logit(sims[, g_hi]) - inv_logit(sims[, g_lo])
  diff_ci <- quantile(diff_raw, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)

  diff_summary <- tibble::tibble(
    cycle = cycle_name,
    contrast = paste0(g_hi, " - ", g_lo, " (raw scale)"),
    diff_mean = mean(diff_raw, na.rm = TRUE),
    diff_median = diff_ci[[2]],
    diff_lo = diff_ci[[1]],
    diff_hi = diff_ci[[3]],
    n_sims = n_sims
  )

  list(
    cycle = cycle_name,
    model = model,
    pair_means = pair_means,
    emm_raw = emm_raw,
    contrasts_logit = contrasts_logit,
    obs_summary = obs_summary,
    diff_raw_summary = diff_summary
  )
}

# RUN
hemi_model_ann <- fit_hemi_model(pairwise_sync_hemi_ann, "Annual",
  group_var = "hemi_type", ref_level = "Between"
)

hemi_model_mlt <- fit_hemi_model(pairwise_sync_hemi_mlt, "Multiannual",
  group_var = "hemi_type", ref_level = "Between"
)

# CONSOLIDATED OUTPUT TABLES
results_means <- bind_rows(
  hemi_model_ann$emm_raw %>%
    left_join(hemi_model_ann$obs_summary, by = c("cycle", "group")),
  hemi_model_mlt$emm_raw %>%
    left_join(hemi_model_mlt$obs_summary, by = c("cycle", "group"))
) %>%
  select(cycle, group, n_pairs, emmean, lo, hi, mean_obs, se_obs) %>%
  arrange(cycle, group)

results_contrasts <- bind_rows(hemi_model_ann$contrasts_logit, hemi_model_mlt$contrasts_logit) %>%
  select(cycle, contrast, estimate, SE, df, t.ratio, p.value, OR, OR_lo, OR_hi)

results_diff_raw <- bind_rows(hemi_model_ann$diff_raw_summary, hemi_model_mlt$diff_raw_summary)

print(results_means)
print(results_contrasts)
print(results_diff_raw)

# Means table
means_tbl <- results_means %>%
  mutate(
    mean_ci = sprintf("%.2f (%.2f–%.2f)", emmean, lo, hi),
    obs = sprintf("%.3f (%.4f)", mean_obs, se_obs)
  ) %>%
  select(cycle, group, n_pairs, mean_ci, obs)

# Differences table
diff_tbl <- results_diff_raw %>%
  mutate(
    group = "Difference",
    diff_ci = sprintf("%.2f (%.2f–%.2f)", diff_mean, diff_lo, diff_hi)
  ) %>%
  left_join(results_contrasts %>% select(cycle, p.value), by = "cycle") %>%
  transmute(
    cycle,
    group,
    n_pairs = NA_integer_,
    mean_ci = NA_character_,
    obs = NA_character_,
    diff_ci,
    as.character(p.value)
  )

# Combine: add empty diff columns to means, empty mean columns to diffs
final_tbl <- means_tbl %>%
  mutate(diff_ci = "", p.value = "") %>%
  bind_rows(diff_tbl %>% mutate(mean_ci = "", obs = "", diff_ci = diff_ci)) %>%
  arrange(cycle, factor(group, levels = c("Between", "Within", "Difference")))

print(final_tbl)

# Combine pair_means from both models
pair_data_combined <- bind_rows(
  hemi_model_ann$pair_means %>% mutate(Cycle = "Annual"),
  hemi_model_mlt$pair_means %>% mutate(Cycle = "Multiannual")
) %>%
  mutate(
    Cycle = factor(Cycle, levels = c("Annual", "Multiannual")),
    Group = factor(group,
      levels = c("Between", "Within"),
      labels = c("Between", "Within")
    )
  )

write.csv(pair_data_combined, "output/tables/wavelet_hemi_full_results.csv", row.names = F)

# Get p-values and differences for annotation
annotations <- results_contrasts %>%
  left_join(results_diff_raw, by = "cycle") %>%
  mutate(
    Cycle = factor(cycle, levels = c("Annual", "Multiannual")),
    p_label = ifelse(p.value < 0.001, "p < 0.001",
      ifelse(p.value < 0.01, sprintf("p = %.2f", p.value),
        sprintf("p = %.2f", p.value)
      )
    ),
    diff_label = sprintf("Δ = %.2f [%.2f, %.2f]", diff_mean, diff_lo, diff_hi),
    signif = p.value < 0.05
  )

write.csv(annotations, "output/tables/wavelet_hemi_diff_results.csv", row.names = F)
