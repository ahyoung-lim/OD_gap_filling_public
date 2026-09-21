# ==============================================================================
# WAVELET TRENDS UNDER MULTIPLE IMPUTATION — shared functions
# ==============================================================================
# Two groups of functions, used by script/08_mi_wavelet_trends.R (main results),
# script/09_mi_wavelet_disaggregation_exclusion.R
#
# 1. wavelet_dataset(df, ...): the complete wavelet analysis of ONE monthly
#    dataset — country eligibility, continuous wavelet transform, yearly band
#    power and its trend model, pairwise coherence / phase, 5-year-window
#    synchrony and its trend model, hemisphere pair-group model. Only the
#    fitted fixed-effect coefficients and covariance matrices are returned
#    (plus the tables the models were fitted to), never the wavelets.
#    Optional exclusion of wavelet coefficients whose support is mostly
#    disaggregated months (mask_max_imp) repeats the band extraction and the
#    trend models per threshold from the same transform.
#
# 2. Rubin's-rules pooling of the per-dataset coefficients across imputed
#    datasets (rubin_scalar, rubin_vector, pool_power_cycle, pool_sync_cycle,
#    pool_hemi_cycle), returning the result tables on the reported scale.
#
# Analysis definitions
#   Eligibility: >= 10 years with > 20 cases (annual analysis), >= 20 such
#     years (multiannual); endemic = at least one 10-year window starting in
#     1990-1999 with >= 7 such years; Sub-Saharan Africa and Europe / Middle
#     East / North Africa excluded.
#   Wavelet: Morlet transform of log(cases + 1); yearly power = mean of the
#     cone-of-influence-masked power over the band and the months of the year.
#     Bands (BAND_ANN, BAND_MLT below) are inclusive ranges of the wavelet
#     scale, applied identically to power and to coherence / phase: annual
#     scales 8-16, multiannual scales 17-64. With the Morlet(6) transform these
#     are Fourier periods of 8.3-16.5 months and 19.7-66 months.
#   Power trend: log(power) ~ year * region + (year | country), regions
#     Americas vs Asia; random intercept only if the random slope fails.
#   Synchrony: coherence x (1 - |phase| / pi), averaged in non-overlapping
#     5-year windows; trend: logit(synchrony) ~ year * pair type +
#     (1 | country1) + (1 | country2) + (year | pair).
#   Hemisphere: one mean synchrony per pair; logit(mean synchrony) ~ Within vs
#     Between hemisphere + crossed country intercepts; marginal means pooled.
#
# Pooling: Rubin's rules on the model scale (log power slope, logit synchrony
# coefficients, logit marginal means). Linear functions of the coefficients get
# point, 95% CI and a two-sided t test with Rubin's degrees of freedom; CI end
# points are exp() / plogis() transformed. Non-linear functions (synchrony %/yr
# on the 0-1 scale, regional differences, cumulative change, raw-scale
# hemisphere difference) are evaluated on draws from N(Q, T), the pooled
# coefficient vector and its total covariance, with percentile CIs.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(nlme)
  library(MASS)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(conflicted)
})
conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicted::conflicts_prefer(lme4::lmer, .quiet = TRUE)

# wavelet functions adapted from Quandelacy et al. (2025)
# https://github.com/tquandelacy/denv_am_synchrony
source("denv_am_synchrony-main/code/WaveletPackage_functions.R")
source("denv_am_synchrony-main/code/calculate_country_wavelets.R")

# ==============================================================================
# PART 1 — per-dataset analysis
# ==============================================================================

# --- band definitions (inclusive ranges of the wavelet scale) ------------------
BAND_ANN <- c(8, 16) # annual cycles: Fourier periods 8.3-16.5 months
BAND_MLT <- c(17, 64) # multiannual cycles: Fourier periods 19.7-66 months

# --- helpers ------------------------------------------------------------------
calculate_endemic_status <- function(country_data) {
  yearly <- country_data %>%
    group_by(Year) %>%
    summarise(active = sum(dengue_total_scaled, na.rm = TRUE) > 20, .groups = "drop")
  if (nrow(yearly) < 10) {
    return(FALSE)
  }
  min_year <- min(yearly$Year)
  max_year <- max(yearly$Year)
  for (start_year in seq(min_year, min(max_year - 9, 1999), by = 1)) {
    window_active <- yearly %>%
      filter(Year >= start_year, Year < start_year + 10) %>%
      pull(active) %>%
      sum(na.rm = TRUE)
    if (window_active >= 7) {
      return(TRUE)
    }
  }
  FALSE
}

logit_safe <- function(p, eps = 1e-6) {
  p <- pmin(pmax(p, eps), 1 - eps)
  log(p / (1 - p))
}
inv_logit <- function(x) 1 / (1 + exp(-x))

mean_phase <- function(phase, na.rm = FALSE) {
  atan2(mean(sin(phase), na.rm = na.rm), mean(cos(phase), na.rm = na.rm))
}

create_stage1_nonoverlap <- function(coh_data, phase_data, window_size, min_year, max_year) {
  begin_years <- seq(min_year, max_year - window_size + 1, by = window_size)
  end_years <- begin_years + window_size
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

create_pair_type <- function(sync_data, country_region_broad_map) {
  region_lookup <- country_region_broad_map %>% dplyr::select(country, broad_region)
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

# Share of disaggregated months inside each wavelet coefficient's support. For
# coefficient (scale j, time t) the months are weighted by the squared Morlet
# time envelope exp(-((t' - t) / s)^2), truncated at trunc * s, and the weight
# mass is renormalised to the months that exist (series edges). Values in [0, 1].
imputed_fraction_matrix <- function(imp, scales, trunc = 4) {
  n_t <- length(imp)
  conv_same <- function(x, k, h) {
    xp <- c(rep(0, h), x, rep(0, h))
    vapply(seq_len(n_t), function(t) sum(xp[t:(t + 2 * h)] * k), numeric(1))
  }
  Fm <- matrix(NA_real_, nrow = length(scales), ncol = n_t)
  for (j in seq_along(scales)) {
    s <- scales[j]
    h <- ceiling(trunc * s)
    k <- exp(-(((-h):h) / s)^2)
    Fm[j, ] <- conv_same(imp, k, h) / conv_same(rep(1, n_t), k, h)
  }
  Fm
}

# --- model fits: coefficients + covariance + which structure converged --------
POWER_COEF <- c("(Intercept)", "year_centered", "regionAsia", "year_centered:regionAsia")
SYNC_COEF <- c(
  "(Intercept)", "year_centered", "pair_typeWithin Asia", "pair_typeAmericas-Asia",
  "year_centered:pair_typeWithin Asia", "year_centered:pair_typeAmericas-Asia"
)

fit_power_model <- function(dat) {
  ctrl <- lmeControl(opt = "optim", maxIter = 500, msMaxIter = 500)
  m <- tryCatch(nlme::lme(log_power ~ year_centered * region,
    random = ~ year_centered | country,
    data = dat, method = "REML", control = ctrl
  ), error = function(e) NULL)
  re <- "random_slope"
  if (is.null(m)) {
    m <- nlme::lme(log_power ~ year_centered * region,
      random = ~ 1 | country,
      data = dat, method = "REML", control = ctrl
    )
    re <- "random_intercept"
  }
  mu <- nlme::fixef(m)
  V <- as.matrix(vcov(m))
  stopifnot(setequal(names(mu), POWER_COEF))
  list(mu = mu[POWER_COEF], V = V[POWER_COEF, POWER_COEF], re_structure = re)
}

fit_sync_model <- function(dat) {
  ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
  m <- tryCatch(lmerTest::lmer(
    sync_logit ~ year_centered * pair_type +
      (1 | ref_prov) + (1 | other_prov) + (year_centered | pair_id),
    data = dat, REML = TRUE, control = ctrl
  ), error = function(e) NULL)
  re <- "random_slope"
  if (is.null(m)) {
    m <- lmerTest::lmer(
      sync_logit ~ year_centered * pair_type +
        (1 | ref_prov) + (1 | other_prov) + (1 | pair_id),
      data = dat, REML = TRUE, control = ctrl
    )
    re <- "random_intercept"
  }
  mu <- lme4::fixef(m)
  V <- as.matrix(vcov(m))
  V <- (V + t(V)) / 2
  if (!setequal(names(mu), SYNC_COEF)) {
    stop("unexpected synchrony coefficient names: ", paste(names(mu), collapse = " | "))
  }
  list(mu = mu[SYNC_COEF], V = V[SYNC_COEF, SYNC_COEF], re_structure = re)
}

# hemisphere model: logit(mean pair synchrony) ~ group + crossed country
# intercepts; marginal means (logit) and their covariance are what gets pooled
fit_hemi_model <- function(pair_means) {
  m <- lmer(sync_logit ~ group + (1 | ref_prov) + (1 | other_prov),
    data = pair_means, REML = TRUE, control = lmerControl(optimizer = "bobyqa")
  )
  emm <- emmeans(m, ~group)
  tb <- as.data.frame(emm)
  mu <- setNames(tb$emmean, as.character(tb$group))
  V <- as.matrix(vcov(emm))
  dimnames(V) <- list(names(mu), names(mu))
  stopifnot(setequal(names(mu), c("Between", "Within")))
  list(
    mu = mu[c("Between", "Within")], V = V[c("Between", "Within"), c("Between", "Within")],
    n_pairs = table(pair_means$group)[c("Between", "Within")]
  )
}

# per-country log-linear trend in yearly power (input to the "% increasing" count)
country_trend_lm <- function(dat, val_col) {
  dat %>%
    group_by(country, region) %>%
    filter(sum(!is.na(.data[[val_col]])) >= 5) %>%
    group_modify(~ {
      fit <- lm(log(.x[[val_col]]) ~ year, data = .x)
      cs <- summary(fit)$coefficients
      data.frame(slope = cs["year", 1], se = cs["year", 2], n = nrow(.x))
    }) %>%
    ungroup()
}

# per-pair linear trend in logit synchrony over 5-yr windows (input to "% increasing")
pair_trend_lm <- function(dat, min_windows = 5) {
  dat %>%
    filter(!is.na(synchrony), !is.na(yr_midpoint)) %>%
    group_by(pair_type, pair_id) %>%
    filter(dplyr::n() >= min_windows) %>%
    group_modify(~ {
      tmp <- data.frame(t = .x$yr_midpoint, y = logit_safe(.x$synchrony))
      fit <- tryCatch(stats::lm(y ~ t, data = tmp), error = function(e) NULL)
      if (is.null(fit)) {
        return(data.frame(slope = NA_real_, se = NA_real_, n = nrow(tmp)))
      }
      cs <- summary(fit)$coefficients
      data.frame(slope = cs["t", 1], se = cs["t", 2], n = nrow(tmp))
    }) %>%
    ungroup()
}

# --- yearly band power from the transforms -------------------------------------
# One row per country x year: band means of the COI-masked power (annual band
# BAND_ANN; multiannual band BAND_MLT, multiannual-eligible countries only) and
# the share of band cells that survived the masks. With a threshold `th`,
# coefficients whose support is more than `th` disaggregated (imp_frac) are
# dropped, and a band-year mean is kept only if at least `min_valid` of its
# cells survive.
yearly_power_table <- function(prov_df, mlt_countries, th = NA_real_, imp_frac = NULL, min_valid = 0.5) {
  out <- list()
  for (i in 1:nrow(prov_df)) {
    this_country <- prov_df$country[i]
    this_wave <- prov_df$wave[[i]]
    this_scale <- prov_df$scale[[i]]
    this_time <- prov_df$time[[i]]
    this_coi <- prov_df$coi[[i]]
    omega0 <- 6
    this_period <- this_scale * (4 * pi) / (omega0 + sqrt(2 + omega0^2))
    power_matrix <- abs(this_wave)^2
    for (s in 1:length(this_period)) power_matrix[s, this_coi < this_period[s]] <- NA
    if (!is.na(th)) power_matrix[imp_frac[[i]] > th] <- NA
    ann_idx <- which(this_scale >= BAND_ANN[1] & this_scale <= BAND_ANN[2])
    mlt_idx <- which(this_scale >= BAND_MLT[1] & this_scale <= BAND_MLT[2])
    is_mlt <- this_country %in% mlt_countries
    years <- floor(this_time)
    for (yr in sort(unique(years))) {
      yr_mask <- years == yr
      n_months <- sum(yr_mask)
      if (n_months < 6) next
      ann_power_yr <- if (length(ann_idx) > 0) mean(power_matrix[ann_idx, yr_mask], na.rm = TRUE) else NA
      mlt_power_yr <- if (length(mlt_idx) > 0 && is_mlt) mean(power_matrix[mlt_idx, yr_mask], na.rm = TRUE) else NA
      ann_frac <- if (length(ann_idx) > 0) sum(!is.na(power_matrix[ann_idx, yr_mask])) / (length(ann_idx) * n_months) else NA_real_
      mlt_frac <- if (length(mlt_idx) > 0 && is_mlt) sum(!is.na(power_matrix[mlt_idx, yr_mask])) / (length(mlt_idx) * n_months) else NA_real_
      if (!is.na(th)) {
        if (is.finite(ann_frac) && ann_frac < min_valid) ann_power_yr <- NA
        if (is.finite(mlt_frac) && mlt_frac < min_valid) mlt_power_yr <- NA
      }
      out[[length(out) + 1]] <- tibble(
        country = this_country, year = yr,
        ann_power_yearly = ann_power_yr, mlt_power_yearly = mlt_power_yr,
        ann_frac_valid = ann_frac, mlt_frac_valid = mlt_frac
      )
    }
  }
  bind_rows(out)
}

# --- power trend models from a yearly power table -------------------------------
# keep: countries entering the models (endemic and eligible for the annual
# analysis). cells: optional data.frame(country, year) restricting the
# country-years used (fixed-support comparisons).
power_fits <- function(yearly_power, keep, region_map, cells = NULL) {
  epsilon_ann <- 1e-6 * median(yearly_power$ann_power_yearly, na.rm = TRUE)
  epsilon_mlt <- 1e-6 * median(yearly_power$mlt_power_yearly, na.rm = TRUE)
  yearly_power <- yearly_power %>%
    mutate(
      ann_power_yearly = ann_power_yearly + epsilon_ann,
      mlt_power_yearly = mlt_power_yearly + epsilon_mlt
    )
  yearly_power_endemic <- yearly_power %>%
    filter(country %in% keep, !is.na(ann_power_yearly))
  if (!is.null(cells)) {
    yearly_power_endemic <- semi_join(yearly_power_endemic, cells, by = c("country", "year"))
  }
  one <- function(val_col) {
    dat <- yearly_power_endemic %>%
      left_join(region_map, by = "country") %>%
      filter(!is.na(broad_region)) %>%
      mutate(
        log_power = log(.data[[val_col]]),
        year_centered = (year - mean(year)) / sd(year),
        region = factor(broad_region, levels = c("Americas", "Asia"))
      ) %>%
      filter(is.finite(log_power))
    fit <- fit_power_model(dat)
    c(fit, list(
      consts = list(
        sd_yr = sd(dat$year), mean_yr = mean(dat$year),
        min_yr = min(dat$year), max_yr = max(dat$year),
        nA = n_distinct(dat$country[dat$region == "Americas"]),
        nAs = n_distinct(dat$country[dat$region == "Asia"]),
        nTot = n_distinct(dat$country), n_obs = nrow(dat)
      ),
      country_lm = country_trend_lm(dat, val_col),
      countries = sort(unique(dat$country)),
      cells = dat %>% dplyr::select(country, year)
    ))
  }
  list(Annual = one("ann_power_yearly"), Multiannual = one("mlt_power_yearly"))
}

# --- synchrony trend model from a pairwise synchrony panel ---------------------
# ps: pair x 5-year-window synchrony (ref_prov, other_prov, yr_midpoint,
# synchrony, pair_id). keep: countries whose pairs enter. cells: optional
# data.frame(pair_id, yr_midpoint) restricting the pair-windows used.
sync_fit <- function(ps, keep, region_map, cells = NULL) {
  dat <- ps %>%
    filter(ref_prov %in% keep, other_prov %in% keep)
  if (!is.null(cells)) dat <- semi_join(dat, cells, by = c("pair_id", "yr_midpoint"))
  dat <- dat %>%
    create_pair_type(region_map) %>%
    mutate(sync_logit = logit_safe(synchrony), year_centered = as.numeric(scale(yr_midpoint)))
  fit <- fit_sync_model(dat)
  c(fit, list(
    consts = list(
      min_year = min(dat$yr_midpoint), max_year = max(dat$yr_midpoint),
      mean_year = mean(dat$yr_midpoint), sd_year = sd(dat$yr_midpoint),
      n_pairs_A = n_distinct(dat$pair_id[dat$pair_type == "Within Americas"]),
      n_pairs_As = n_distinct(dat$pair_id[dat$pair_type == "Within Asia"]),
      n_pairs_C = n_distinct(dat$pair_id[dat$pair_type == "Americas-Asia"]),
      n_pairs_tot = n_distinct(dat$pair_id), n_obs = nrow(dat)
    ),
    pair_lm = pair_trend_lm(dat, min_windows = 5),
    panel = dat %>% dplyr::select(pair_id, ref_prov, other_prov, pair_type, yr_midpoint, synchrony)
  ))
}

# --- hemisphere pair-group model from a pairwise synchrony panel ---------------
hemi_fit <- function(ps, keep, hemi_lookup) {
  pm <- ps %>%
    filter(ref_prov %in% keep, other_prov %in% keep) %>%
    left_join(hemi_lookup, by = c("ref_prov" = "adm_0_name")) %>%
    rename(ref_hemi = hemisphere) %>%
    left_join(hemi_lookup, by = c("other_prov" = "adm_0_name")) %>%
    rename(other_hemi = hemisphere) %>%
    filter(!is.na(ref_hemi), !is.na(other_hemi)) %>%
    mutate(hemi_type = ifelse(ref_hemi == other_hemi, "Within", "Between")) %>%
    group_by(pair_id, ref_prov, other_prov, hemi_type) %>%
    summarise(mean_sync = mean(synchrony, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      group = relevel(factor(hemi_type), ref = "Between"),
      sync_logit = logit_safe(mean_sync)
    )
  stopifnot(nrow(pm) == n_distinct(pm$pair_id))
  fit <- fit_hemi_model(pm)
  c(fit, list(pair_means = pm %>% select(pair_id, ref_prov, other_prov, hemi_type, mean_sync)))
}

# --- country eligibility, broad regions and hemispheres -----------------------
# df: one complete monthly dataset. Returns the dataset restricted to the
# eligible countries (annual analysis) together with the country sets, the
# broad-region map and the hemisphere lookup used by every analysis.
eligibility_sets <- function(df) {
  country_active_years <- df %>%
    group_by(od_region, adm_0_name, Year) %>%
    summarise(total_cases = sum(dengue_total_scaled, na.rm = TRUE), .groups = "drop") %>%
    mutate(active = total_cases > 20) %>%
    group_by(od_region, adm_0_name) %>%
    summarise(n_active_years = sum(active), .groups = "drop")
  ann_countries <- country_active_years %>%
    filter(n_active_years >= 10) %>%
    pull(adm_0_name)
  mlt_countries <- country_active_years %>%
    filter(n_active_years >= 20) %>%
    pull(adm_0_name)
  df <- df %>% filter(!od_region %in% c("Sub-Saharan Africa", "Europe, Middle East & North Africa"))
  endemic_countries <- df %>%
    group_by(adm_0_name) %>%
    group_modify(~ tibble(is_endemic = calculate_endemic_status(.x))) %>%
    ungroup() %>%
    filter(is_endemic) %>%
    pull(adm_0_name)
  df <- df %>% filter(adm_0_name %in% ann_countries)

  hemi_lookup <- df %>%
    distinct(adm_0_name, Latitude) %>%
    mutate(hemisphere = ifelse(Latitude >= 0, "North", "South")) %>%
    select(adm_0_name, hemisphere)
  stopifnot(!anyDuplicated(hemi_lookup$adm_0_name))

  # broad regions: every od_region present must be assigned explicitly (an
  # unlisted label stops the run rather than falling into either group)
  americas_regions <- c("North & Central America", "South America", "Caribbean")
  asia_regions <- c("East & Southeast Asia", "South Asia", "Pacific Islands")
  region_map <- df %>%
    dplyr::select(adm_0_name, od_region) %>%
    distinct() %>%
    rename(country = adm_0_name) %>%
    mutate(broad_region = case_when(
      od_region %in% americas_regions ~ "Americas",
      od_region %in% asia_regions ~ "Asia",
      TRUE ~ NA_character_
    ))
  if (anyNA(region_map$broad_region)) {
    stop(
      "od_region labels without a broad region: ",
      paste(unique(region_map$od_region[is.na(region_map$broad_region)]), collapse = ", ")
    )
  }

  list(
    df = df, ann_countries = ann_countries, mlt_countries = mlt_countries,
    endemic_countries = endemic_countries,
    endemic_and_eligible_ann = intersect(endemic_countries, ann_countries),
    endemic_and_eligible_mlt = intersect(endemic_countries, mlt_countries),
    hemi_lookup = hemi_lookup, region_map = region_map
  )
}

# --- the per-dataset analysis -------------------------------------------------
# df: one complete monthly dataset (adm_0_name, Year, month, dengue_total_scaled,
#     od_region, Latitude, ...). With mask_max_imp (thresholds in [0, 1]) df must
#     also carry mask_imp (1 = disaggregated month, 0 otherwise).
# Returns: countries (wavelet / ann / mlt sets), power and sync fits per cycle,
# hemi fits per cycle, the yearly power table and the synchrony panel the models
# used, and — when thresholds are given — `excluded`, one such set of power and
# sync results per threshold (named by the threshold).
wavelet_dataset <- function(df, mask_max_imp = NULL, mask_min_valid = 0.5) {
  set.seed(123)
  if (!is.null(mask_max_imp) && !"mask_imp" %in% names(df)) {
    stop("mask_max_imp set but df has no mask_imp column")
  }
  ths <- if (is.null(mask_max_imp)) numeric(0) else sort(unique(as.numeric(mask_max_imp)))

  # country eligibility, regions, hemispheres
  el <- eligibility_sets(df)
  df <- el$df
  mlt_countries <- el$mlt_countries
  endemic_and_eligible_ann <- el$endemic_and_eligible_ann
  endemic_and_eligible_mlt <- el$endemic_and_eligible_mlt
  hemi_lookup <- el$hemi_lookup
  region_map <- el$region_map

  all_cases <- df %>%
    mutate(
      month = as.integer(month), Year = as.integer(Year),
      time = Year + (month - 1) / 12, ln_cases = log(dengue_total_scaled + 1),
      country = adm_0_name, province = adm_0_name, co_province = adm_0_name
    ) %>%
    arrange(country, time) %>%
    select(country, province, co_province, time, ln_cases, od_region)

  # wavelets per country
  unique_countries <- unique(all_cases$country)
  prov_df <- lapply(unique_countries, function(c) {
    country_data <- all_cases %>% filter(country == c)
    tryCatch(calculate_country_wavelets(country_data), error = function(e) NULL)
  }) %>%
    bind_rows() %>%
    filter(!is.null(wave))

  # disaggregated-month share per coefficient (same in every imputed dataset)
  imp_frac <- NULL
  if (length(ths) > 0) {
    imp_lookup <- df %>%
      mutate(time = Year + (as.integer(month) - 1) / 12, imp = as.numeric(mask_imp)) %>%
      dplyr::select(country = adm_0_name, time, imp)
    imp_frac <- lapply(seq_len(nrow(prov_df)), function(i) {
      tt <- prov_df$time[[i]]
      v <- imp_lookup$imp[match(
        paste(prov_df$country[i], round(tt, 4)),
        paste(imp_lookup$country, round(imp_lookup$time, 4))
      )]
      stopifnot(!anyNA(v))
      imputed_fraction_matrix(v, prov_df$scale[[i]])
    })
  }

  # yearly power -> power trend models (unmasked, then per threshold)
  yearly_na <- yearly_power_table(prov_df, mlt_countries)
  power_na <- power_fits(yearly_na, endemic_and_eligible_ann, region_map)
  yearly_th <- list()
  power_th <- list()
  for (th in ths) {
    key <- as.character(th)
    yearly_th[[key]] <- yearly_power_table(prov_df, mlt_countries, th, imp_frac, mask_min_valid)
    power_th[[key]] <- tryCatch(power_fits(yearly_th[[key]], endemic_and_eligible_ann, region_map),
      error = function(e) {
        message(sprintf("power models failed at threshold %s: %s", key, conditionMessage(e)))
        NULL
      }
    )
  }

  # pairwise coherence and phase (computed once)
  prov_combos <- tidyr::crossing(ref_prov = prov_df$province, other_prov = prov_df$province) %>%
    filter(ref_prov != other_prov)
  prov_combos$coherence <- list(NULL)
  prov_combos$phase <- list(NULL)
  prov_combos$scales <- list(NULL)
  prov_combos$coi <- list(NULL)
  prov_combos$times <- list(NULL)
  if (length(ths) > 0) prov_combos$Fpair <- list(NULL)
  for (i in 1:nrow(prov_combos)) {
    ref_prov <- filter(prov_df, province == prov_combos$ref_prov[i])
    other_prov <- filter(prov_df, province == prov_combos$other_prov[i])
    match_obj <- match_transforms(
      wave1 = ref_prov$wave[[1]], wave2 = other_prov$wave[[1]],
      time1 = ref_prov$time[[1]], time2 = other_prov$time[[1]],
      scale1 = ref_prov$scale[[1]], scale2 = other_prov$scale[[1]]
    )
    prov_combos$scales[[i]] <- match_obj$scales
    prov_combos$coi[[i]] <- get_coi(n = length(match_obj$times), dt = 1)
    prov_combos$times[[i]] <- match_obj$times
    prov_combos$coherence[[i]] <- coh(
      cwt1 = match_obj$wave1, cwt2 = match_obj$wave2,
      scales = match_obj$scales, dt = 1, dj = 1 / 4
    )
    prov_combos$phase[[i]] <- phase_updated(
      cwt1 = match_obj$wave1, cwt2 = match_obj$wave2,
      scales = match_obj$scales, times = match_obj$times, dt = 1, dj = 1 / 4
    )
    # pair-level disaggregated share on the matched grid = max of the two countries
    if (length(ths) > 0) {
      ri <- match(prov_combos$ref_prov[i], prov_df$country)
      oi <- match(prov_combos$other_prov[i], prov_df$country)
      sub_F <- function(Fm, sc, tm) {
        Fm[match(match_obj$scales, sc),
          match(round(match_obj$times, 4), round(tm, 4)),
          drop = FALSE
        ]
      }
      prov_combos$Fpair[[i]] <- pmax(
        sub_F(imp_frac[[ri]], prov_df$scale[[ri]], prov_df$time[[ri]]),
        sub_F(imp_frac[[oi]], prov_df$scale[[oi]], prov_df$time[[oi]])
      )
    }
  }
  mlt_combos <- prov_df %>% filter(country %in% mlt_countries)
  mlt_combos <- tidyr::crossing(ref_prov = mlt_combos$province, other_prov = mlt_combos$province) %>%
    filter(ref_prov != other_prov) %>%
    left_join(prov_combos, by = c("ref_prov", "other_prov"))

  # band mean of a pair's coherence or phase matrix at every time point: time
  # points inside the cone of influence of the band's lower edge are dropped,
  # and the scales inside the band are combined with the circular mean (the
  # same computation as extract_coh_phase_time_coi_updated, with the band as
  # an inclusive scale range)
  band_series <- function(combos, what, band, th = NA_real_) {
    out <- vector("list", nrow(combos))
    for (i in 1:nrow(combos)) {
      M <- combos[[what]][[i]]
      if (!is.na(th)) M[combos$Fpair[[i]] > th] <- NA
      sc <- combos$scales[[i]]
      inds <- which(sc >= band[1] & sc <= band[2])
      M[, combos$coi[[i]] < band[1]] <- NA
      est <- apply(M[inds, , drop = FALSE], 2, mean_phase, na.rm = TRUE)
      est[is.nan(est)] <- NA
      out[[i]] <- data.frame(
        time = combos$times[[i]], est = est,
        ref_prov = combos$ref_prov[[i]], other_prov = combos$other_prov[[i]]
      )
    }
    bind_rows(out)
  }
  min_year <- floor(min(all_cases$time))
  max_year <- floor(max(all_cases$time))
  pairwise_sync <- function(coh_data, phase_data) {
    w <- create_stage1_nonoverlap(coh_data, phase_data, 5, min_year, max_year)
    left_join(w$coh %>% dplyr::select(ref_prov, other_prov, yr_midpoint, coh_est = est),
      w$phase %>% dplyr::select(ref_prov, other_prov, yr_midpoint, phase_est = est),
      by = c("ref_prov", "other_prov", "yr_midpoint")
    ) %>%
      mutate(phase_adj = 1 - abs(phase_est / pi), synchrony = coh_est * phase_adj) %>%
      filter(!is.na(synchrony)) %>%
      filter(ref_prov < other_prov) %>%
      mutate(pair_id = paste(ref_prov, other_prov, sep = "_"))
  }
  # band extraction -> 5-year windows -> synchrony panels, per threshold
  sync_panels <- function(th) {
    list(
      Annual = pairwise_sync(
        band_series(prov_combos, "coherence", BAND_ANN, th),
        band_series(prov_combos, "phase", BAND_ANN, th)
      ),
      Multiannual = pairwise_sync(
        band_series(mlt_combos, "coherence", BAND_MLT, th),
        band_series(mlt_combos, "phase", BAND_MLT, th)
      )
    )
  }
  sync_from_panels <- function(pn) {
    list(
      Annual = sync_fit(pn$Annual, endemic_and_eligible_ann, region_map),
      Multiannual = sync_fit(pn$Multiannual, endemic_and_eligible_mlt, region_map)
    )
  }
  panels_na <- sync_panels(NA_real_)
  sync_na <- sync_from_panels(panels_na)
  hemi_na <- list(
    Annual = hemi_fit(panels_na$Annual, endemic_and_eligible_ann, hemi_lookup),
    Multiannual = hemi_fit(panels_na$Multiannual, endemic_and_eligible_mlt, hemi_lookup)
  )
  sync_th <- list()
  panels_th <- list()
  for (th in ths) {
    key <- as.character(th)
    panels_th[[key]] <- sync_panels(th)
    sync_th[[key]] <- tryCatch(sync_from_panels(panels_th[[key]]),
      error = function(e) {
        message(sprintf("sync models failed at threshold %s: %s", key, conditionMessage(e)))
        NULL
      }
    )
  }

  res <- list(
    countries = list(
      wavelet = sort(prov_df$country),
      ann = sort(endemic_and_eligible_ann), mlt = sort(endemic_and_eligible_mlt)
    ),
    region_map = region_map,
    power = power_na, sync = sync_na, hemi = hemi_na,
    yearly = yearly_na, sync_panel = panels_na
  )
  if (length(ths) > 0) {
    res$excluded <- lapply(setNames(as.character(ths), as.character(ths)), function(key) {
      list(
        threshold = as.numeric(key), power = power_th[[key]], sync = sync_th[[key]],
        yearly = yearly_th[[key]], sync_panel = panels_th[[key]]
      )
    })

    # fixed-support fits: every threshold (and the unmasked baseline) refitted on
    # exactly the country-years / pair-windows that survive the STRICTEST
    # threshold, so thresholds differ only in the values, not in the cells.
    strict <- as.character(min(ths))
    if (!is.null(power_th[[strict]]) && !is.null(sync_th[[strict]])) {
      cells_ann <- power_th[[strict]]$Annual$cells
      cells_mlt <- power_th[[strict]]$Multiannual$cells
      cells_sy <- lapply(sync_th[[strict]], function(f) f$panel %>% dplyr::select(pair_id, yr_midpoint))
      fixed_fits <- function(yearly, panels) {
        list(
          power = tryCatch(
            list(
              Annual = power_fits(yearly, endemic_and_eligible_ann, region_map, cells_ann)$Annual,
              Multiannual = power_fits(yearly, endemic_and_eligible_ann, region_map, cells_mlt)$Multiannual
            ),
            error = function(e) {
              message("fixed-support power models failed: ", conditionMessage(e))
              NULL
            }
          ),
          sync = tryCatch(
            list(
              Annual = sync_fit(panels$Annual, endemic_and_eligible_ann, region_map, cells_sy$Annual),
              Multiannual = sync_fit(panels$Multiannual, endemic_and_eligible_mlt, region_map, cells_sy$Multiannual)
            ),
            error = function(e) {
              message("fixed-support sync models failed: ", conditionMessage(e))
              NULL
            }
          )
        )
      }
      res$fixed <- fixed_fits(yearly_na, panels_na)
      for (key in names(res$excluded)) {
        res$excluded[[key]]$fixed <- fixed_fits(yearly_th[[key]], panels_th[[key]])
      }
    }
  }
  res
}

# ==============================================================================
# PART 2 — Rubin's-rules pooling across datasets
# ==============================================================================

# scalar: Q = per-dataset estimates, U = their squared SEs
rubin_scalar <- function(Q, U) {
  keep <- is.finite(Q) & is.finite(U)
  Q <- Q[keep]
  U <- U[keep]
  m <- length(Q)
  Qbar <- mean(Q)
  Ubar <- mean(U)
  B <- if (m > 1) stats::var(Q) else 0
  Tt <- Ubar + (1 + 1 / m) * B
  df <- if (B > 0) (m - 1) * (1 + Ubar / ((1 + 1 / m) * B))^2 else Inf
  tc <- if (is.finite(df)) stats::qt(0.975, df) else stats::qnorm(0.975)
  tstat <- Qbar / sqrt(Tt)
  p <- if (is.finite(df)) 2 * stats::pt(-abs(tstat), df) else 2 * stats::pnorm(-abs(tstat))
  data.frame(
    m = m, point = Qbar, se = sqrt(Tt), lwr = Qbar - tc * sqrt(Tt), upr = Qbar + tc * sqrt(Tt),
    p = p, df = df, within_sd = sqrt(Ubar), between_sd = sqrt(B),
    fmi = if (Tt > 0) (1 + 1 / m) * B / Tt else NA_real_
  )
}
# vector: list of coefficient vectors and covariance matrices (same names/order)
rubin_vector <- function(mu_list, V_list) {
  Mx <- do.call(rbind, mu_list)
  m <- nrow(Mx)
  Qbar <- colMeans(Mx)
  Ubar <- Reduce(`+`, V_list) / m
  B <- stats::cov(Mx)
  Tt <- Ubar + (1 + 1 / m) * B
  Tt <- (Tt + t(Tt)) / 2
  if (min(eigen(Tt, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
    Tt <- as.matrix(Matrix::nearPD(Tt, corr = FALSE)$mat)
  }
  list(Q = Qbar, T = Tt, U = Ubar, B = B, m = m)
}
# a linear combination c'mu of one dataset's coefficients and its variance
lincomb <- function(fit, cvec) {
  nm <- names(cvec)
  c(Q = sum(cvec * fit$mu[nm]), U = as.numeric(t(cvec) %*% fit$V[nm, nm] %*% cvec))
}
# scalar Rubin over datasets for a per-dataset linear combination builder
pool_lincomb <- function(fits, cvec_fun) {
  qu <- t(sapply(fits, function(f) lincomb(f, cvec_fun(f))))
  rubin_scalar(qu[, "Q"], qu[, "U"])
}
# a constant that should be identical across datasets: take the mean, warn otherwise
const_check <- function(vals, what, tol = 1e-8) {
  if (diff(range(vals)) > tol * max(1, abs(mean(vals)))) {
    warning(sprintf("%s differs across datasets (range %.6g to %.6g); mean used", what, min(vals), max(vals)))
  }
  mean(vals)
}
# count increasing units (pooled CI lower bound > 0) from stacked per-dataset lm slopes
count_increasing <- function(lm_tab, unit) {
  pooled <- lm_tab %>%
    group_by(across(all_of(c(unit, "grp")))) %>%
    group_modify(~ rubin_scalar(.x$slope, .x$se^2)) %>%
    ungroup() %>%
    mutate(is_inc = lwr > 0)
  by_grp <- pooled %>%
    group_by(grp) %>%
    summarise(N_Inc = sum(is_inc, na.rm = TRUE), N_Tot = sum(!is.na(is_inc)), .groups = "drop")
  bind_rows(data.frame(
    grp = "Global", N_Inc = sum(pooled$is_inc, na.rm = TRUE),
    N_Tot = sum(!is.na(pooled$is_inc))
  ), by_grp) %>%
    mutate(Pct_Inc = 100 * N_Inc / N_Tot)
}
scalar_row <- function(analysis, cycle, region, quantity, r) {
  data.frame(analysis = analysis, cycle = cycle, region = region, quantity = quantity, r)
}

# --- POWER ----------------------------------------------------------------------
# fits: one power fit (as returned by power_fits()[[cycle]]) per dataset.
# Reported slope = per-year log-power slope. Americas: b_year/sd_yr; Asia:
# (b_year + b_int)/sd_yr; Global: mean of the two weighted by number of countries.
# Returns list(table = result table, scalars = the pooled scalars behind it).
# Draws N_SIM values from N(Q, T) for the regional difference of %/yr.
pool_power_cycle <- function(fits, cycle, n_sim = 5000) {
  M <- length(fits)
  sd_yr <- sapply(fits, function(f) f$consts$sd_yr)
  nA <- const_check(sapply(fits, function(f) f$consts$nA), paste(cycle, "power nA"))
  nAs <- const_check(sapply(fits, function(f) f$consts$nAs), paste(cycle, "power nAs"))
  nTot <- const_check(sapply(fits, function(f) f$consts$nTot), paste(cycle, "power nTot"))
  min_yr <- const_check(sapply(fits, function(f) f$consts$min_yr), paste(cycle, "power min_yr"))
  max_yr <- const_check(sapply(fits, function(f) f$consts$max_yr), paste(cycle, "power max_yr"))
  mean_yr <- const_check(sapply(fits, function(f) f$consts$mean_yr), paste(cycle, "power mean_yr"))
  sd_yr_c <- const_check(sd_yr, paste(cycle, "power sd_yr"))

  cA <- function(f) c(year_centered = 1 / f$consts$sd_yr)
  cAs <- function(f) c(year_centered = 1 / f$consts$sd_yr, `year_centered:regionAsia` = 1 / f$consts$sd_yr)
  cG <- function(f) {
    w <- f$consts$nA / (f$consts$nA + f$consts$nAs)
    c(year_centered = 1 / f$consts$sd_yr, `year_centered:regionAsia` = (1 - w) / f$consts$sd_yr)
  }
  cInt <- function(f) c(`year_centered:regionAsia` = 1 / f$consts$sd_yr)
  slopes <- list(Global = pool_lincomb(fits, cG), Americas = pool_lincomb(fits, cA), Asia = pool_lincomb(fits, cAs))
  inter <- pool_lincomb(fits, cInt)
  scalars <- bind_rows(
    lapply(names(slopes), function(rg) scalar_row("power", cycle, rg, "log_slope_per_year", slopes[[rg]])),
    list(scalar_row("power", cycle, "Asia", "log_slope_diff_Asia_minus_Americas", inter))
  )

  # pooled coefficient vector for the non-linear regional difference and for display
  pv <- rubin_vector(lapply(fits, `[[`, "mu"), lapply(fits, `[[`, "V"))
  draws <- MASS::mvrnorm(n_sim, mu = pv$Q, Sigma = pv$T)
  pctA_d <- (exp(draws[, "year_centered"] / sd_yr_c) - 1) * 100
  pctAs_d <- (exp((draws[, "year_centered"] + draws[, "year_centered:regionAsia"]) / sd_yr_c) - 1) * 100
  ci_diff <- quantile(pctAs_d - pctA_d, c(0.025, 0.975))

  delta_years <- 2024 - 1990
  x_start <- (min_yr - mean_yr) / sd_yr_c
  x_end <- (max_yr - mean_yr) / sd_yr_c
  b0 <- pv$Q["(Intercept)"]
  bY <- pv$Q["year_centered"]
  bR <- pv$Q["regionAsia"]
  bI <- pv$Q["year_centered:regionAsia"]
  ps_A <- exp(b0 + bY * x_start)
  pe_A <- exp(b0 + bY * x_end)
  ps_As <- exp(b0 + bR + (bY + bI) * x_start)
  pe_As <- exp(b0 + bR + (bY + bI) * x_end)
  w <- nA / (nA + nAs)

  pct <- function(r) (exp(r$point) - 1) * 100
  pct_lo <- function(r) (exp(r$lwr) - 1) * 100
  pct_hi <- function(r) (exp(r$upr) - 1) * 100
  cum <- function(r) (exp(r$point * delta_years) - 1) * 100
  cum_lo <- function(r) (exp(r$lwr * delta_years) - 1) * 100
  cum_hi <- function(r) (exp(r$upr * delta_years) - 1) * 100

  inc <- count_increasing(bind_rows(lapply(seq_along(fits), function(i) {
    fits[[i]]$country_lm %>% mutate(grp = as.character(region), k = i)
  })), unit = "country") %>%
    rename(Region = grp)

  out <- data.frame(
    Cycle = cycle, Region = c("Global", "Americas", "Asia"),
    N = c(nTot, nA, nAs),
    Power_Start = c(w * ps_A + (1 - w) * ps_As, ps_A, ps_As),
    Power_End = c(w * pe_A + (1 - w) * pe_As, pe_A, pe_As),
    Pct_Change = sapply(slopes, pct), CI_Lower = sapply(slopes, pct_lo), CI_Upper = sapply(slopes, pct_hi),
    P_value = sapply(slopes, `[[`, "p"),
    Interaction_P = c(NA, NA, inter$p),
    Diff_AsiaMinusAmericas = c(NA, NA, pct(slopes$Asia) - pct(slopes$Americas)),
    Diff_CI_Lower = c(NA, NA, ci_diff[[1]]), Diff_CI_Upper = c(NA, NA, ci_diff[[2]]),
    Pct_Change_Cum_1990_2024 = sapply(slopes, cum),
    Cum_CI_Lower_1990_2024 = sapply(slopes, cum_lo), Cum_CI_Upper_1990_2024 = sapply(slopes, cum_hi),
    Weighting = "countries",
    m = M, Within_SD_logslope = sapply(slopes, `[[`, "within_sd"),
    Between_SD_logslope = sapply(slopes, `[[`, "between_sd"), FMI = sapply(slopes, `[[`, "fmi"),
    stringsAsFactors = FALSE
  )
  list(table = left_join(out, inc, by = "Region"), scalars = scalars)
}

# --- SYNCHRONY ------------------------------------------------------------------
# fits: one sync fit (sync_fit()) per dataset. Pooled on the logit scale. %/yr on
# the 0-1 synchrony scale is the model synchrony at the last window minus that at
# the first, per year, relative to the first: (sync_end - sync_start) / n_years /
# sync_start; CI from draws of N(Q, T). Global = pair-weighted mean of the three
# pair types' start and end values.
pool_sync_cycle <- function(fits, cycle, n_sim = 5000) {
  M <- length(fits)
  cs <- function(nm) sapply(fits, function(f) f$consts[[nm]])
  min_year <- const_check(cs("min_year"), paste(cycle, "sync min_year"))
  max_year <- const_check(cs("max_year"), paste(cycle, "sync max_year"))
  mean_year <- const_check(cs("mean_year"), paste(cycle, "sync mean_year"))
  sd_year <- const_check(cs("sd_year"), paste(cycle, "sync sd_year"))
  nP <- sapply(
    c("n_pairs_tot", "n_pairs_A", "n_pairs_As", "n_pairs_C"),
    function(nm) const_check(cs(nm), paste(cycle, "sync", nm))
  )
  n_years <- max_year - min_year
  x_start <- (min_year - mean_year) / sd_year
  x_end <- (max_year - mean_year) / sd_year
  wts <- nP[c("n_pairs_A", "n_pairs_As", "n_pairs_C")] / nP["n_pairs_tot"]

  # linear quantities: per-year logit slope by pair type (and pair-weighted global)
  yr <- "year_centered"
  iAs <- "year_centered:pair_typeWithin Asia"
  iC <- "year_centered:pair_typeAmericas-Asia"
  cA <- function(f) setNames(1 / f$consts$sd_year, yr)
  cAs <- function(f) setNames(c(1, 1) / f$consts$sd_year, c(yr, iAs))
  cC <- function(f) setNames(c(1, 1) / f$consts$sd_year, c(yr, iC))
  cG <- function(f) {
    w <- c(f$consts$n_pairs_A, f$consts$n_pairs_As, f$consts$n_pairs_C) / f$consts$n_pairs_tot
    setNames(c(1, w[2], w[3]) / f$consts$sd_year, c(yr, iAs, iC))
  }
  slopes <- list(
    Global = pool_lincomb(fits, cG), `Within Americas` = pool_lincomb(fits, cA),
    `Within Asia` = pool_lincomb(fits, cAs), `Americas-Asia` = pool_lincomb(fits, cC)
  )
  inter <- list(
    `Within Asia` = pool_lincomb(fits, function(f) setNames(1 / f$consts$sd_year, iAs)),
    `Americas-Asia` = pool_lincomb(fits, function(f) setNames(1 / f$consts$sd_year, iC))
  )
  scalars <- bind_rows(
    lapply(names(slopes), function(pt) scalar_row("synchrony", cycle, pt, "logit_slope_per_year", slopes[[pt]])),
    lapply(names(inter), function(pt) scalar_row("synchrony", cycle, pt, "logit_slope_diff_vs_Americas", inter[[pt]]))
  )

  # non-linear quantities from the pooled coefficient vector
  pv <- rubin_vector(lapply(fits, `[[`, "mu"), lapply(fits, `[[`, "V"))
  eps <- 1e-6
  start_end <- function(b, type, int) {
    eta_s <- b[, "(Intercept)"] + (if (is.null(type)) 0 else b[, type]) + (b[, yr] + (if (is.null(int)) 0 else b[, int])) * x_start
    eta_e <- b[, "(Intercept)"] + (if (is.null(type)) 0 else b[, type]) + (b[, yr] + (if (is.null(int)) 0 else b[, int])) * x_end
    list(s = pmin(pmax(inv_logit(eta_s), eps), 1 - eps), e = pmin(pmax(inv_logit(eta_e), eps), 1 - eps))
  }
  types <- list(
    `Within Americas` = list(NULL, NULL),
    `Within Asia` = list("pair_typeWithin Asia", iAs),
    `Americas-Asia` = list("pair_typeAmericas-Asia", iC)
  )
  eval_all <- function(b) {
    se <- lapply(types, function(t) start_end(b, t[[1]], t[[2]]))
    se$Global <- list(
      s = wts[1] * se[[1]]$s + wts[2] * se[[2]]$s + wts[3] * se[[3]]$s,
      e = wts[1] * se[[1]]$e + wts[2] * se[[2]]$e + wts[3] * se[[3]]$e
    )
    se <- se[c("Global", "Within Americas", "Within Asia", "Americas-Asia")]
    list(
      start = sapply(se, `[[`, "s"), end = sapply(se, `[[`, "e"),
      pct = sapply(se, function(x) ((x$e - x$s) / n_years) / x$s * 100),
      cum = sapply(se, function(x) (x$e - x$s) / x$s * 100)
    )
  }
  point <- eval_all(matrix(pv$Q, nrow = 1, dimnames = list(NULL, names(pv$Q))))
  draws <- eval_all(MASS::mvrnorm(n_sim, mu = pv$Q, Sigma = pv$T))
  qlo <- function(x) apply(x, 2, quantile, 0.025, na.rm = TRUE)
  qhi <- function(x) apply(x, 2, quantile, 0.975, na.rm = TRUE)
  diff_d <- draws$pct[, c("Within Asia", "Americas-Asia")] - draws$pct[, "Within Americas"]

  inc <- count_increasing(bind_rows(lapply(seq_along(fits), function(i) {
    fits[[i]]$pair_lm %>% mutate(grp = as.character(pair_type), k = i)
  })), unit = "pair_id") %>%
    rename(Pair_Type = grp)

  out <- data.frame(
    Cycle = cycle, Pair_Type = c("Global", "Within Americas", "Within Asia", "Americas-Asia"),
    Weighting = "pairs", N_pairs = nP,
    Start_Year = round(min_year), End_Year = round(max_year),
    Sync_Start = as.numeric(point$start), Sync_End = as.numeric(point$end),
    Pct_Change_Per_Year = as.numeric(point$pct), CI_Lower = qlo(draws$pct), CI_Upper = qhi(draws$pct),
    Trend_P = sapply(slopes, `[[`, "p"),
    Interaction_P = c(NA, NA, inter[[1]]$p, inter[[2]]$p),
    Diff_vs_Americas = c(NA, NA, point$pct[3] - point$pct[2], point$pct[4] - point$pct[2]),
    Diff_CI_Lower = c(NA, NA, qlo(diff_d)), Diff_CI_Upper = c(NA, NA, qhi(diff_d)),
    Pct_Change_Cum = as.numeric(point$cum), Cum_CI_Lower = qlo(draws$cum), Cum_CI_Upper = qhi(draws$cum),
    m = M, Within_SD_logitslope = sapply(slopes, `[[`, "within_sd"),
    Between_SD_logitslope = sapply(slopes, `[[`, "between_sd"), FMI = sapply(slopes, `[[`, "fmi"),
    stringsAsFactors = FALSE
  )
  list(table = left_join(out, inc, by = "Pair_Type"), scalars = scalars)
}

# --- HEMISPHERE -----------------------------------------------------------------
# fits: one hemi fit (hemi_fit()) per dataset. Group marginal means pooled on the
# logit scale; Within - Between contrast on the logit scale (Rubin t test, odds
# ratio) and on the 0-1 scale (draws of N(Q, T)).
pool_hemi_cycle <- function(fits, cycle, n_sim = 5000) {
  M <- length(fits)
  nB <- const_check(sapply(fits, function(f) f$n_pairs[["Between"]]), paste(cycle, "hemi n Between"))
  nW <- const_check(sapply(fits, function(f) f$n_pairs[["Within"]]), paste(cycle, "hemi n Within"))
  mB <- pool_lincomb(fits, function(f) c(Between = 1))
  mW <- pool_lincomb(fits, function(f) c(Within = 1))
  dl <- pool_lincomb(fits, function(f) c(Between = -1, Within = 1))
  scalars <- bind_rows(
    scalar_row("hemisphere", cycle, "Between", "logit_mean", mB),
    scalar_row("hemisphere", cycle, "Within", "logit_mean", mW),
    scalar_row("hemisphere", cycle, "Within - Between", "logit_diff", dl)
  )
  pv <- rubin_vector(lapply(fits, `[[`, "mu"), lapply(fits, `[[`, "V"))
  draws <- MASS::mvrnorm(n_sim, mu = pv$Q, Sigma = pv$T)
  diff_raw <- plogis(draws[, "Within"]) - plogis(draws[, "Between"])
  obs <- bind_rows(lapply(fits, `[[`, "pair_means")) %>%
    group_by(pair_id, hemi_type) %>%
    summarise(mean_sync = mean(mean_sync), .groups = "drop") %>%
    group_by(hemi_type) %>%
    summarise(mean_obs = mean(mean_sync), se_obs = sd(mean_sync) / sqrt(dplyr::n()), .groups = "drop")
  table <- data.frame(
    cycle = cycle, group = c("Between", "Within", "Difference"),
    n_pairs = c(nB, nW, NA),
    emmean = c(plogis(mB$point), plogis(mW$point), NA),
    lo = c(plogis(mB$lwr), plogis(mW$lwr), NA), hi = c(plogis(mB$upr), plogis(mW$upr), NA),
    mean_obs = c(obs$mean_obs[obs$hemi_type == "Between"], obs$mean_obs[obs$hemi_type == "Within"], NA),
    se_obs = c(obs$se_obs[obs$hemi_type == "Between"], obs$se_obs[obs$hemi_type == "Within"], NA),
    diff_raw = c(NA, NA, plogis(pv$Q[["Within"]]) - plogis(pv$Q[["Between"]])),
    diff_lo = c(NA, NA, quantile(diff_raw, 0.025)), diff_hi = c(NA, NA, quantile(diff_raw, 0.975)),
    logit_diff = c(NA, NA, dl$point), OR = c(NA, NA, exp(dl$point)),
    OR_lo = c(NA, NA, exp(dl$lwr)), OR_hi = c(NA, NA, exp(dl$upr)),
    p_value = c(NA, NA, dl$p),
    m = M, FMI = c(mB$fmi, mW$fmi, dl$fmi),
    stringsAsFactors = FALSE
  )
  list(table = table, scalars = scalars)
}
