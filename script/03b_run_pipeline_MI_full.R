# ==============================================================================
# FULL-PIPELINE UNCERTAINTY PROPAGATION
# (STAGE 1 weekly -> STAGE 2 monthly -> STAGE 3 downscale -> STAGE 4 sample and
#  constrain to annual totals, repeated N_RUNS times)
# ==============================================================================
# The pipeline has four stages: a weekly model (fitted ONCE and shared by every
# run), a monthly model on the weekly series aggregated to months, a
# disaggregation model for the country-years available only as annual totals, and
# a final sampling stage in which every draw is constrained to its country-year
# annual total. Run once, this gives one set of numbers with no measure of how
# much they depend on the imputation.
#
# Here the four stages are repeated N_RUNS times, and the places where a value is
# DRAWN from the posterior instead of taken as a point estimate are marked with
# ">>> MI:"  :
#   1. Missing weeks / months are filled with a POSTERIOR DRAW instead of the
#      point estimate pred_mean.
#   2. For yearly totals that are only estimates (IHME or neighbour-median), each
#      run draws a different total within its plausible range, instead of using
#      the single value.
# The spread of the final numbers across the runs is the imputation uncertainty.
#
# Extras for a long job:  results of each run are saved so an interrupted job
# resumes;  each stage prints a short diagnostic and a timer;  a VALIDATION block
# at the end checks the method behaved as intended.
# ==============================================================================

source(file.path(getwd(), "script/CV/00_imp_model_inla_spec.R"))
source(file.path(getwd(), "script/CV/00_inla_eval_helpers.R"))
source(file.path(getwd(), "functions/fn_make_week_complete.R"))

library(dplyr)
library(rlang)
library(lubridate)
library(countrycode)
library(tidyr)
stopifnot(requireNamespace("INLA", quietly = TRUE))

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------
N_RUNS  <- as.integer(Sys.getenv("MI_N_RUNS", "30"))  # full pipeline runs (imputations). 30 follows
                                   # the MI convention (m ~ 30-50); the convergence diagnostic below
                                   # confirms whether fewer would have sufficed. SMOKE-TEST cheaply
                                   # first, then scale up (runs are resumable + seeded, so the smoke
                                   # runs are reused, not wasted):
                                   #   MI_N_RUNS=2 Rscript script/03b_run_pipeline_MI_full.R   # check guards + validation PASS
                                   #   Rscript script/03b_run_pipeline_MI_full.R               # then the full 30
R       <- 100                      # posterior draws of the final split, per run
seed    <- 123
SAVE_STAGE_DATA <- TRUE             # save each run's monthly and downscale
                                    # stage tables so you can open
                                    # them and see where a run went wrong. Set
                                    # FALSE to save disk on the full dataset.
# Guard: expected number of estimated yearly totals. If the counts differ, the
# calibrated data is likely stale (with an earlier regional classification of
# Mayotte/Reunion the median count is ~46 instead of 5) -> the run stops before
# wasting time.
EXPECTED_IHME   <- 7
EXPECTED_MEDIAN <- 5
# >>> SENSITIVITY (0% model): read the *_noscale inputs and default to an isolated output dir
#   when EXCLUDE_SUBANNUAL_SCALING=true, so the no-scale run never collides with the 100% run.
# sensitivity inputs live in a SEPARATE folder (same basenames): "noscale" (0%), "thr<pct>"
# (completeness cutoff), or none (100%). Redirect reads + the output dir accordingly.
.excl <- tolower(Sys.getenv("EXCLUDE_SUBANNUAL_SCALING", "false")) %in% c("true", "1", "yes")
.minpct <- suppressWarnings(as.numeric(Sys.getenv("SCALE_MIN_COMPLETENESS", "0"))); if (is.na(.minpct)) .minpct <- 0
SENS_TAG <- if (.excl) "noscale" else if (.minpct > 0) sprintf("thr%g", .minpct) else ""
psens <- function(path) {
  if (identical(SENS_TAG, "")) return(path)
  file.path("data", "sensitivity", "sens01_scaling_sensitivity", SENS_TAG, basename(path))
}
OUT_DIR <- file.path(getwd(), Sys.getenv("MI_OUT_DIR",
  if (identical(SENS_TAG, "")) "runs/mi_full" else file.path("runs", "sensitivity", "sens01_scaling_sensitivity", SENS_TAG)))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
# Output layout inside OUT_DIR (the production run and every scaling-sensitivity run share it):
#   mi50/weekly/          shared weekly fit and its input stamp
#   mi50/monthly/         monthly-stage table of run 1
#   mi50/disaggregation/  disaggregation-stage table of run 1; posterior draws of every run
#   mi50/                 pooled cells, annual-total uncertainty cache, released dataset (script 03c)
#   descriptive_summary/  aggregate totals, country-year totals, MI convergence, INLA posterior summaries
#                         (weekly fit; monthly and disaggregation fits per run and pooled)
MI_DIR      <- file.path(OUT_DIR, "mi50")
WEEKLY_DIR  <- file.path(MI_DIR, "weekly")
MONTHLY_DIR <- file.path(MI_DIR, "monthly")
DOWN_DIR    <- file.path(MI_DIR, "disaggregation")
SUMMARY_DIR <- file.path(OUT_DIR, "descriptive_summary")
for (.d in c(WEEKLY_DIR, MONTHLY_DIR, DOWN_DIR, SUMMARY_DIR)) dir.create(.d, recursive = TRUE, showWarnings = FALSE)

# >>> MI (input stamp): a content hash of every upstream input. It is written into each
#   run file and re-checked before EXTENDING or POOLING a run set, so runs built on
#   DIFFERENT inputs (e.g. a run set built on earlier inputs) can never be silently
#   mixed. If any input changes, the fingerprint changes and the guards below force a
#   clean regenerate. (Not a method tag — purely an inputs-changed detector.)
INPUT_FILES <- c(
  psens("data/model_input/pred_data_weekly.csv"),
  psens("data/model_input/pred_data_monthly.csv"),
  psens("data/model_input/pred_data_disaggregate.csv"),
  psens("data/processed_data/dt_heatmap_calibrated.csv"),
  "data/ad_hoc/IHME-GBD_2021_DATA.csv")
input_fingerprint <- function(files = INPUT_FILES) {
  paths <- file.path(getwd(), files)
  miss <- !file.exists(paths)
  if (any(miss)) stop("Missing MI input file(s): ", paste(files[miss], collapse = ", "))
  paste(unname(tools::md5sum(paths)), collapse = "-")
}
INPUT_FP <- input_fingerprint()

set.seed(seed)
Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)
INLA::inla.setOption(num.threads = as.integer(Sys.getenv("MI_INLA_THREADS", "8")), save.memory = FALSE)

secs <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))

# Posterior summaries of the fixed effects and hyperparameters of an INLA fit
# (median, 95% interval, mean, sd), in the layout of the supplementary tables.
inla_param_summary <- function(fit, model, run_id = NA_integer_) {
  pick <- function(s, type) data.frame(
    model = model, run = run_id, Parameter = rownames(s), Type = type,
    Median = s[, "0.5quant"], CI_0.025 = s[, "0.025quant"], CI_0.975 = s[, "0.975quant"],
    Mean = s[, "mean"], SD = s[, "sd"], row.names = NULL, check.names = FALSE)
  rbind(pick(as.data.frame(fit$summary.fixed), "Fixed effect"),
        pick(as.data.frame(fit$summary.hyperpar), "Hyperparameter"))
}
say  <- function(...) cat(sprintf(...))

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------

# >>> MI: draw ONE posterior sample of counts for chosen rows (used to fill gaps
#         instead of pred_mean, and to sample the final split in STAGE 4).
#         selection = Predictor -> only the nodes we need, not the whole field.
sample_pred_counts <- function(fit, target_idx, n_draws, seed) {
  samp <- INLA::inla.posterior.sample(
    n_draws, fit, seed = seed, selection = list(Predictor = target_idx))
  eta <- matrix(vapply(samp, function(s) as.numeric(s$latent[, 1]),
                       numeric(length(target_idx))), nrow = length(target_idx))
  size <- vapply(samp, function(s) {
    hp <- s$hyperpar
    j <- grep("size.*nbinom|size.*nbinomial|size.*negative", names(hp), ignore.case = TRUE)
    if (length(j) > 0) as.numeric(hp[j[1]]) else Inf
  }, numeric(1))
  mu <- exp(eta)
  Y <- matrix(NA_real_, nrow = length(target_idx), ncol = n_draws)
  for (d in seq_len(n_draws))
    Y[, d] <- if (is.finite(size[d])) rnbinom(length(target_idx), mu = mu[, d], size = size[d])
              else rpois(length(target_idx), mu[, d])
  Y
}

# --- fill_small_gaps : fill runs of up to max_gap missing weeks inside a year -
fill_small_gaps <- function(df, country_col = "adm_0_name", time_col = "time_seq",
                            year_col = "Year", y_col = "dengue_total",
                            pred_col = "pred_mean", min_gap = 1, max_gap = 12) {
  country <- ensym(country_col); time <- ensym(time_col); year <- ensym(year_col)
  y <- ensym(y_col); pred <- ensym(pred_col)
  df %>%
    arrange(!!country, !!time) %>% group_by(!!country) %>%
    mutate(.is_na = is.na(!!y),
      .year_change = !!year != dplyr::lag(!!year, default = first(!!year)),
      .run_id = cumsum(coalesce(.is_na != dplyr::lag(.is_na) | .year_change, TRUE))) %>%
    group_by(!!country, .run_id) %>% mutate(.run_len = n()) %>% ungroup() %>%
    mutate(.to_fill = .is_na & .run_len >= min_gap & .run_len <= max_gap & !is.na(!!pred),
      !!y := if_else(.to_fill, !!pred, !!y), filled_small_gap = .to_fill) %>%
    select(-.is_na, -.run_id, -.run_len, -.to_fill, -.year_change)
}

# --- fill_medium_gaps : same rule at the monthly stage (flags imputed_monthly) -
fill_medium_gaps <- function(df, country_col = "adm_0_name", time_col = "time_seq",
                             year_col = "Year", y_col = "dengue_total",
                             pred_col = "pred_mean", min_gap = 1, max_gap = 12) {
  country <- ensym(country_col); time <- ensym(time_col); year <- ensym(year_col)
  y <- ensym(y_col); pred <- ensym(pred_col)
  df %>%
    arrange(!!country, !!time) %>% group_by(!!country) %>%
    mutate(.is_na = is.na(!!y),
      .year_change = !!year != dplyr::lag(!!year, default = first(!!year)),
      .run_id = cumsum(coalesce(.is_na != dplyr::lag(.is_na) | .year_change, TRUE))) %>%
    group_by(!!country, .run_id) %>% mutate(.run_len = n()) %>% ungroup() %>%
    mutate(.to_fill = .is_na & .run_len >= min_gap & .run_len <= max_gap & !is.na(!!pred),
      !!y := if_else(.to_fill, !!pred, !!y), imputed_monthly = .to_fill) %>%
    select(-.is_na, -.run_id, -.run_len, -.to_fill, -.year_change)
}

# --- rake_one : spread a country-year's annual total over its unobserved months --
# Every constrained group uses one rule: draw month weights from
#   Gamma(shape = nbinom size) so they carry the negative-binomial overdispersion,
#   normalise them to probabilities, then rmultinom the annual budget across the
#   months. This is the conditional allocation of a Poisson sum (Poisson-sum ->
#   Multinomial), identical for zero and nonzero draws, and it keeps the
#   month-to-month uncertainty inside the annual constraint. Observed months stay
#   locked at their reported values; budgets <= 0, all-zero mu and groups without an
#   annual anchor (is.na(at)) are left untouched.
rake_one <- function(y, mu, idxG, annualG, lock_obs, dengue_obs, size = Inf) {
  y[lock_obs] <- dengue_obs[lock_obs]
  for (g in seq_along(idxG)) {
    ix <- idxG[[g]]; at <- annualG[g]
    if (is.na(at)) next
    locked_g <- lock_obs[ix]; free_ix <- ix[!locked_g]
    locked_sum <- sum(y[ix[locked_g]]); budget <- at - locked_sum
    if (budget <= 0) { y[free_ix] <- 0; next }
    mu_free <- mu[free_ix]
    # month allocation probabilities: Gamma weights (mean mu, NB overdispersion via
    # shape = size) -> normalise. Falls back to mu (Poisson limit) if size is Inf, and
    # to uniform if all mu are zero.
    if (sum(mu_free) == 0) {
      p <- rep(1 / length(free_ix), length(free_ix))
    } else if (is.finite(size)) {
      w <- rgamma(length(free_ix), shape = size, rate = size / pmax(mu_free, 1e-12))
      p <- if (sum(w) == 0) mu_free / sum(mu_free) else w / sum(w)
    } else {
      p <- mu_free / sum(mu_free)
    }
    y[free_ix] <- as.numeric(rmultinom(1, size = budget, prob = p))
  }
  y
}

# >>> MI: uncertainty range for each ESTIMATED yearly total (built once, cached).
#         IHME cells -> GBD's published low/high band; median cells -> spread of
#         the nearby years. Reported totals are absent here and stay fixed.
build_annual_uncertainty <- function() {
  # the cache file is _v2 because a median cell's sdlog ABSORBS its IHME-neighbour
  # uncertainty; a cache written before that (recentre-based) must not be reused.
  path <- file.path(MI_DIR, "annual_uncertainty_v2.rds")
  if (file.exists(path)) {
    cached <- readRDS(path)
    if (is.list(cached) && !is.null(cached$tab)) return(cached)
  }
  heat <- read.csv(file.path(getwd(), psens("data/processed_data/dt_heatmap_calibrated.csv"))) %>%
    mutate(adm_0_name = toupper(adm_0_name))
  ihme_raw <- read.csv(file.path(getwd(), "data/ad_hoc/IHME-GBD_2021_DATA.csv")) %>%
    transmute(adm_0_name = toupper(location_name), Year = year,
              rel_lo = lower / val, rel_hi = upper / val) %>%
    filter(is.finite(rel_lo), is.finite(rel_hi))
  spread_range <- function(v) {
    v <- v[is.finite(v) & v > 0]; if (length(v) < 2) return(c(NA, NA))
    m <- mean(log(v)); s <- sd(log(v)); exp(c(m - 1.96 * s, m + 1.96 * s))
  }
  est <- heat %>% filter(data_source %in% c("IHME_calibrated", "Median_from_neighbors")) %>%
    transmute(adm_0_name, Year, source = data_source, point = annual_total)
  ihme_cells <- est %>% filter(source == "IHME_calibrated") %>%
    left_join(ihme_raw, by = c("adm_0_name", "Year")) %>%
    transmute(adm_0_name, Year, source, point, lower = point * rel_lo, upper = point * rel_hi)
  # IHME (meanlog, sdlog) lookup — used to DRAW the IHME neighbours inside the median MC below.
  ihme_band <- ihme_cells %>%
    mutate(lo = ifelse(is.na(lower) | lower <= 0, point * 0.7, lower),
           hi = ifelse(is.na(upper) | upper <= 0, point * 1.3, upper),
           meanlog = log(pmax(point, 0.5)),
           sdlog = (log(hi) - log(lo)) / (2 * 1.96),
           key = paste(adm_0_name, Year))
  ihme_meanlog <- setNames(ihme_band$meanlog, ihme_band$key)
  ihme_sdlog   <- setNames(ihme_band$sdlog,   ihme_band$key)
  med_cells <- est %>% filter(source == "Median_from_neighbors")
  # >>> MI: a median cell's sdlog = two independent pieces in quadrature:
  #     (1) sdlog_spread = how much this country's neighbour YEARS bounce (spread_range);
  #     (2) s_ihme       = extra width because some neighbours are IHME estimates. Keep
  #         the robust MEDIAN as centre (no closed form) and estimate (2) by Monte Carlo:
  #         draw the IHME neighbours from their lognormal band, recompute the median,
  #         take sd(log(median)) over N_MC draws.
  #   The centre (point) is UNCHANGED; only the interval widens. IHME uncertainty now
  #   lives inside sdlog, so the draw loop does NOT recentre (no double-counting).
  N_MC <- 1000
  .old_seed <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  set.seed(20260731L)                       # reproducible widths, RNG stream restored below
  med_rows <- vector("list", nrow(med_cells))
  for (i in seq_len(nrow(med_cells))) {
    cc <- med_cells$adm_0_name[i]; yy <- med_cells$Year[i]
    nb <- heat %>% filter(adm_0_name == cc,
      !data_source %in% c("Median_from_neighbors", "No_data")) %>%
      mutate(dist = abs(Year - yy)) %>% arrange(dist) %>% slice(seq_len(min(10, n())))
    rng <- spread_range(nb$annual_total)
    nb_val <- as.numeric(nb$annual_total)
    nb_key <- paste(nb$adm_0_name, nb$Year)
    is_ihme_nb <- nb$data_source == "IHME_calibrated" & nb_key %in% names(ihme_sdlog)
    s_ihme <- 0
    if (any(is_ihme_nb)) {
      k <- nb_key[is_ihme_nb]
      sims <- vapply(seq_len(N_MC), function(m) {
        vv <- nb_val
        vv[is_ihme_nb] <- rlnorm(length(k), ihme_meanlog[k], ihme_sdlog[k])
        median(vv, na.rm = TRUE)
      }, numeric(1))
      sims <- sims[is.finite(sims) & sims > 0]
      if (length(sims) > 1) s_ihme <- sd(log(sims))
      if (!is.finite(s_ihme)) s_ihme <- 0
    }
    med_rows[[i]] <- data.frame(adm_0_name = cc, Year = yy, source = "Median_from_neighbors",
               point = med_cells$point[i], lower = rng[1], upper = rng[2], s_ihme = s_ihme)
  }
  if (!is.null(.old_seed)) assign(".Random.seed", .old_seed, envir = .GlobalEnv)
  med_out <- do.call(rbind, med_rows)
  out <- bind_rows(ihme_cells, med_out) %>%
    mutate(lower = ifelse(is.na(lower) | lower <= 0, point * 0.7, lower),
           upper = ifelse(is.na(upper) | upper <= 0, point * 1.3, upper),
           meanlog = log(pmax(point, 0.5)),
           sdlog = (log(upper) - log(lower)) / (2 * 1.96),
           key = paste(adm_0_name, Year))
  # median cells: widen sdlog to ABSORB the IHME-neighbour uncertainty (quadrature).
  out$s_ihme <- ifelse(is.na(out$s_ihme), 0, out$s_ihme)
  out$sdlog  <- ifelse(out$source == "Median_from_neighbors",
                       sqrt(out$sdlog^2 + out$s_ihme^2), out$sdlog)
  res <- list(tab = out)
  saveRDS(res, path); res
}

# ==============================================================================
# STAGE 1  WEEKLY MODEL  — fit ONCE, shared by every run (cached)
# ==============================================================================
cat("\n================= STAGE 1: weekly fit (shared) ==================\n")
.au <- build_annual_uncertainty()
annual_unc    <- .au$tab      # per-cell (point, lower, upper, meanlog, sdlog, key, source)
n_ihme <- sum(annual_unc$source == "IHME_calibrated")
n_med  <- sum(annual_unc$source == "Median_from_neighbors")
say("annual uncertainty: %d estimated yearly totals (%d IHME, %d median)\n",
    nrow(annual_unc), n_ihme, n_med)

# >>> guard: stop BEFORE the weekly fit if the estimated-total counts are wrong
if (n_ihme != EXPECTED_IHME || n_med != EXPECTED_MEDIAN) {
  stop(sprintf(paste0(
    "Unexpected annual-total counts: IHME=%d (expected %d), median=%d (expected %d).\n",
    "  Most likely dt_heatmap_calibrated.csv is stale (median ~46 means an earlier\n",
    "  regional classification of Mayotte/Reunion). Re-run 01c, then DELETE the\n",
    "  cached runs/mi_full/mi50/annual_uncertainty_v2.rds and run this script again."),
    n_ihme, EXPECTED_IHME, n_med, EXPECTED_MEDIAN))
}

data <- read.csv(file.path(getwd(), psens("data/model_input/pred_data_weekly.csv")))
df <- data %>%
  mutate(y = dengue_total, region = factor(lat_band), regionx = as.integer(region),
    week52 = ((as.integer(week) - 1L) %% 52L) + 1L,
    week_shared = week52, week_dev = week52, week_dev_country = week52) %>%
  arrange(adm_0_name, time_seq)
mod <- inla_w_hier_shared_formula

weekly_fit_path <- file.path(WEEKLY_DIR, "pred_imp_weekly_fit.rds")
# >>> MI (input stamp): the 3.3 GB weekly fit is a cache of pred_data_weekly.csv. Stamp it
#   with that file's hash and refit automatically if the input changed, so a stale fit is
#   never silently reused after the inputs change.
weekly_fp      <- unname(tools::md5sum(file.path(getwd(), psens("data/model_input/pred_data_weekly.csv"))))
weekly_fp_path <- file.path(WEEKLY_DIR, "pred_imp_weekly_fit.fp")
weekly_cache_ok <- file.exists(weekly_fit_path) && file.exists(weekly_fp_path) &&
  identical(readLines(weekly_fp_path, warn = FALSE)[1], weekly_fp)
if (weekly_cache_ok) {
  fit <- readRDS(weekly_fit_path); say("weekly fit: loaded cached\n")
} else {
  if (file.exists(weekly_fit_path))
    say("weekly fit: cache stale (pred_data_weekly.csv changed) -> refitting\n")
  t1 <- Sys.time()
  fit <- INLA::inla(update(mod, y ~ .), data = df, family = ctrl_fam$family,
    control.family = ctrl_fam$control.family,
    control.predictor = list(compute = TRUE, link = 1),  # need summary.fitted.values$mean
    # below (df$pred_mean); compute = FALSE would leave it NULL and break the small-gap fill.
    control.compute = list(dic = FALSE, waic = FALSE, config = TRUE))
  saveRDS(fit, weekly_fit_path); writeLines(weekly_fp, weekly_fp_path)
  say("weekly fit: %.1fs\n", secs(t1))
}
df$pred_mean <- fit$summary.fitted.values$mean          # point estimate of the weekly fit
# >>> posterior summaries of the shared weekly fit (supplementary table of the weekly fit)
write.csv(inla_param_summary(fit, "weekly"),
          file.path(SUMMARY_DIR, "inla_posterior_summaries_weekly.csv"), row.names = FALSE)
weekly_fit  <- fit                                       # keep; loop reuses the name 'fit'
weekly_base <- df                                        # per-run copy overwrites pred_mean
stopifnot(!is.null(weekly_fit$misc$configs))            # need config=TRUE to sample
# which weekly rows will the monthly stage fill (1..4 wk gaps)? fixed across runs
small_idx <- which(fill_small_gaps(weekly_base, min_gap = 1, max_gap = 4)$filled_small_gap)
say("[diag] weekly mlik=%.1f | small-gap weeks to fill: %d\n", weekly_fit$mlik[1], length(small_idx))

# ==============================================================================
# ONE FULL RUN = STAGES 2-4 with draws instead of point estimates
# ==============================================================================
one_run <- function(run_id) {
  rs <- seed + 1000L * run_id
  tr <- Sys.time()

  # ========================== STAGE 2: MONTHLY ==============================
  pred_w_out <- weekly_base
  # >>> MI: fill the small gaps with a posterior DRAW (not pred_mean)
  pred_w_out$pred_mean[small_idx] <- sample_pred_counts(weekly_fit, small_idx, 1, rs)[, 1]
  pred_w_filled <- fill_small_gaps(pred_w_out, min_gap = 1, max_gap = 4)

  # (aggregation to monthly: regular and sporadic series handled separately) --
  coverage_stats <- pred_w_filled %>% group_by(adm_0_name, Year) %>%
    summarise(total_cases = sum(dengue_total, na.rm = TRUE), .groups = "drop")
  monthly_completeness <- pred_w_filled %>%
    mutate(month = month(calendar_start_date),
      month = case_when(year(as.Date(calendar_start_date) + 6) != year(as.Date(calendar_start_date)) &
        year(as.Date(calendar_start_date) + 6) == Year ~ 1, TRUE ~ month)) %>%
    group_by(adm_0_name, Year, month) %>%
    summarise(weeks_in_month = n(), weeks_with_data = sum(!is.na(dengue_total)),
      month_complete = weeks_with_data == weeks_in_month, .groups = "drop") %>%
    group_by(adm_0_name, Year) %>%
    summarise(n_complete_months = sum(month_complete),
      n_partial_months = sum(weeks_with_data > 0 & !month_complete), .groups = "drop")
  data_classification <- coverage_stats %>%
    left_join(monthly_completeness, by = c("adm_0_name", "Year")) %>%
    mutate(data_pattern = case_when(n_complete_months >= 6 ~ "dense",
      n_complete_months >= 1 ~ "moderate", n_partial_months > 0 ~ "sporadic", TRUE ~ "no_data"))
  sporadic_country_years <- data_classification %>% filter(data_pattern == "sporadic") %>%
    select(adm_0_name, Year)
  regular_data  <- pred_w_filled %>% anti_join(sporadic_country_years, by = c("adm_0_name", "Year"))
  sporadic_data <- pred_w_filled %>% semi_join(sporadic_country_years, by = c("adm_0_name", "Year"))
  regular_monthly <- regular_data %>%
    mutate(month = as.integer(month(calendar_start_date)),
      month = case_when(year(as.Date(calendar_start_date) + 6) != year(as.Date(calendar_start_date)) &
        year(as.Date(calendar_start_date) + 6) == Year ~ 1L, TRUE ~ month)) %>%
    group_by(adm_0_name, Year, lat_band, month, pop_est) %>%
    summarise(n_weeks = n(), n_weeks_obs = sum(!is.na(dengue_total)),
      dengue_total = if_else(n_weeks_obs == n_weeks, as.integer(sum(dengue_total, na.rm = TRUE)), NA_integer_),
      has_small_gap = any(coalesce(filled_small_gap, FALSE)),
      imputed_weekly = !is.na(dengue_total) & has_small_gap, .groups = "drop") %>%
    select(-n_weeks, -n_weeks_obs)
  sporadic_monthly <- sporadic_data %>%
    mutate(month = as.integer(month(calendar_start_date)),
      month = case_when(year(as.Date(calendar_start_date) + 6) != year(as.Date(calendar_start_date)) &
        year(as.Date(calendar_start_date) + 6) == Year ~ 1L, TRUE ~ month)) %>%
    group_by(adm_0_name, Year, lat_band, month, pop_est) %>%
    summarise(n_weeks_observed = sum(!is.na(dengue_total)), dengue_partial = sum(dengue_total, na.rm = TRUE),
      dengue_total = case_when(n_weeks_observed > 0 ~ as.integer(dengue_partial), TRUE ~ NA_integer_),
      has_small_gap = any(coalesce(filled_small_gap, FALSE)),
      imputed_weekly = n_weeks_observed > 0, .groups = "drop") %>%
    select(-n_weeks_observed, -dengue_partial)
  pred_w_m <- bind_rows(regular_monthly, sporadic_monthly) %>% arrange(adm_0_name, Year, month)

  pred_data <- read.csv(file.path(getwd(), psens("data/model_input/pred_data_monthly.csv"))) %>%
    mutate(imputed_weekly = FALSE)
  df <- bind_rows(
    pred_data[, c("adm_0_name", "Year", "lat_band", "month", "pop_est", "dengue_total", "imputed_weekly")],
    pred_w_m[, c("adm_0_name", "Year", "lat_band", "month", "pop_est", "dengue_total", "imputed_weekly")]
  ) %>% mutate(calendar_start_date = as.character(make_date(Year, month, 1))) %>%
    group_by(adm_0_name, Year, month) %>%
    filter(!is.na(dengue_total) | all(is.na(dengue_total))) %>% slice(1) %>% ungroup()
  df$countryx <- as.integer(factor(df$adm_0_name,
    levels = unique(df$adm_0_name)[order(unique(df$adm_0_name))], ordered = TRUE))
  df$yearx <- as.integer(as.factor(df$Year))
  df$ISO_A0 <- countrycode(df$adm_0_name, "country.name", "iso3c")
  df$ISO_A0[df$adm_0_name == "SAINT MARTIN"] <- "MAF"
  df <- make_month_complete_clean(data = df %>% select(-month), keep_vars = TRUE)
  df <- df %>% mutate(y = dengue_total, region = factor(lat_band), regionx = as.integer(region),
    month_shared = month, month_dev = month, month_dev_country = month) %>% arrange(adm_0_name, time_seq)

  # >>> NA-check: fail fast if any model index/replicate is NA (localises the bug
  #     in seconds instead of waiting for INLA). idx=time_seq, replicate=countryx/regionx.
  .chk <- sapply(c("countryx", "yearx", "regionx", "time_seq", "month"), function(v) sum(is.na(df[[v]])))
  say("[na-check] run %d monthly: %s | rows=%d\n", run_id,
      paste(names(.chk), .chk, sep = "=", collapse = " "), nrow(df))
  if (any(.chk > 0)) stop(sprintf("run %d monthly: NA in model index(es): %s",
    run_id, paste(names(.chk)[.chk > 0], collapse = ", ")))
  t_mfit <- Sys.time()
  fit <- INLA::inla(update(inla_m_hier_shared_formula, y ~ .), data = df, family = ctrl_fam$family,
    control.family = ctrl_fam$control.family, control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = FALSE, waic = FALSE, config = TRUE))
  df$pred_mean <- fit$summary.fitted.values$mean
  param_monthly <- inla_param_summary(fit, "monthly", run_id)   # kept in the run file (supplementary table of the monthly fit)
  say("[diag] run %d monthly fit: %.1fs | mlik=%.1f\n", run_id, secs(t_mfit), fit$mlik[1])
  # >>> save the monthly-stage table for run 1 only:
  #     the stage tables serve inspection, and the lock structure is identical in every run
  if (SAVE_STAGE_DATA && run_id == 1L) write.csv(df,
    file.path(MONTHLY_DIR, sprintf("run_%02d_monthly.csv", run_id)), row.names = FALSE)

  # ========================= STAGE 3: DOWNSCALE ==============================
  # >>> MI: fill medium gaps with a posterior DRAW at the missing months
  na_idx <- which(is.na(df$dengue_total))
  if (length(na_idx) > 0) df$pred_mean[na_idx] <- sample_pred_counts(fit, na_idx, 1, rs + 1L)[, 1]
  pred_m_filled <- fill_medium_gaps(df, min_gap = 1, max_gap = 11)
  say("[diag] run %d downscale medium gaps filled: %d\n", run_id, sum(pred_m_filled$imputed_monthly, na.rm = TRUE))

  pred_m_filled <- pred_m_filled %>% group_by(adm_0_name, Year) %>%
    mutate(dengue_total = as.integer(dengue_total),
           annual_total = as.integer(sum(dengue_total, na.rm = FALSE))) %>% ungroup()
  pred_data <- read.csv(file.path(getwd(), psens("data/model_input/pred_data_disaggregate.csv"))) %>%
    select(-Latitude, -Longitude) %>% mutate(imputed_weekly = FALSE, imputed_monthly = FALSE)
  # >>> the monthly table carries calendar_start_date as a Date object, the
  #     disaggregate table (read from CSV) as text. rbind() would coerce the Date
  #     to its numeric form ("18262"), which as.Date() cannot parse, breaking the
  #     month-grid join in make_month_complete_clean (it then invents blank rows
  #     with NA model indices). Force both to text first.
  pred_data$calendar_start_date <- as.character(pred_data$calendar_start_date)
  pred_m_filled$calendar_start_date <- as.character(pred_m_filled$calendar_start_date)
  df <- rbind(pred_data, pred_m_filled %>% select(names(pred_data)))
  df <- df %>% mutate(dengue_total = case_when(annual_total == 0 ~ 0, TRUE ~ dengue_total))
  df$countryx <- as.integer(factor(df$adm_0_name,
    levels = unique(df$adm_0_name)[order(unique(df$adm_0_name))], ordered = TRUE))
  df$yearx <- as.integer(as.factor(df$Year))
  df$ISO_A0 <- countrycode(df$adm_0_name, "country.name", "iso3c")
  df$ISO_A0[df$adm_0_name == "SAINT MARTIN"] <- "MAF"
  df <- make_month_complete_clean(data = df %>% select(-month, -time_seq), keep_vars = TRUE)
  df <- df %>% mutate(y = dengue_total, region = factor(lat_band), regionx = as.integer(region),
    month_shared = month, month_dev = month, month_dev_country = month) %>% arrange(adm_0_name, time_seq)

  # >>> NA-check (as at the monthly stage): fail fast if any model index/replicate is NA
  .chk <- sapply(c("countryx", "yearx", "regionx", "time_seq", "month"), function(v) sum(is.na(df[[v]])))
  say("[na-check] run %d downscale: %s | rows=%d\n", run_id,
      paste(names(.chk), .chk, sep = "=", collapse = " "), nrow(df))
  if (any(.chk > 0)) stop(sprintf("run %d downscale: NA in model index(es): %s",
    run_id, paste(names(.chk)[.chk > 0], collapse = ", ")))
  t_dfit <- Sys.time()
  fit <- INLA::inla(update(inla_m_hier_shared_formula, y ~ .), data = df, family = ctrl_fam$family,
    control.family = ctrl_fam$control.family, control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = FALSE, waic = FALSE, config = TRUE))
  param_downscale <- inla_param_summary(fit, "disaggregation", run_id)   # kept in the run file (supplementary table of the disaggregation fit)
  say("[diag] run %d downscale fit: %.1fs | mlik=%.1f | rows=%d\n", run_id, secs(t_dfit), fit$mlik[1], nrow(df))
  # >>> save the downscale-stage table for this run;
  #     lets you inspect annual_total and the lock structure the constraint uses
  if (SAVE_STAGE_DATA && run_id == 1L) write.csv(df,
    file.path(DOWN_DIR, sprintf("run_%02d_downscale.csv", run_id)), row.names = FALSE)

  # ============ STAGE 4: SAMPLE + CONSTRAIN TO ANNUAL TOTALS ================
  n_pred <- nrow(df)
  samples <- INLA::inla.posterior.sample(R, fit, seed = rs + 2L,
    selection = list(Predictor = seq_len(n_pred)))
  mu_mat <- pmax(exp(vapply(samples, function(s) as.numeric(s$latent[, 1]), numeric(n_pred))), 0)
  size_vec <- vapply(samples, function(s) {
    hp <- s$hyperpar
    j <- grep("size.*nbinom|size.*nbinomial|size.*negative", names(hp), ignore.case = TRUE)
    if (length(j) > 0) as.numeric(hp[j[1]]) else Inf }, numeric(1))

  lock_obs   <- (!is.na(df$dengue_total)) & !df$imputed_weekly & !df$imputed_monthly
  dengue_obs <- as.integer(ifelse(is.na(df$dengue_total), 0L, df$dengue_total))
  cy   <- as.integer(factor(interaction(df$adm_0_name, df$Year, drop = TRUE)))
  idxG <- split(seq_len(n_pred), cy)
  annualG <- as.numeric(tapply(df$annual_total, cy, function(x) x[1]))
  # >>> MI: for estimated yearly totals, look up the (meanlog, sdlog, source) to draw from
  grp_key    <- vapply(idxG, function(ix) paste(df$adm_0_name[ix[1]], df$Year[ix[1]]), character(1))
  m_idx      <- match(grp_key, annual_unc$key)
  grp_sdlog  <- ifelse(is.na(m_idx), 0, annual_unc$sdlog[m_idx])
  grp_mean   <- ifelse(is.na(m_idx), NA, annual_unc$meanlog[m_idx])
  grp_source <- ifelse(is.na(m_idx), "fixed", annual_unc$source[m_idx])
  ihme_g <- which(grp_source == "IHME_calibrated")
  med_g  <- which(grp_source == "Median_from_neighbors")

  Ydraw <- matrix(0L, nrow = n_pred, ncol = R)
  for (r in seq_len(R)) {
    mu <- mu_mat[, r]; sz <- size_vec[r]
    y <- if (is.finite(sz)) rnbinom(n_pred, mu = mu, size = sz) else rpois(n_pred, mu)
    # >>> MI: draw this replicate's yearly total for estimated groups (else fixed).
    #   Median cells' sdlog already ABSORBS the IHME-neighbour uncertainty
    #   (see build_annual_uncertainty), so IHME and median cells are drawn identically:
    #   one lognormal draw around the fixed centre. No per-replicate recentring, hence
    #   no double-counting of the IHME uncertainty.
    annualG_draw <- annualG
    est_g <- c(ihme_g, med_g)
    if (length(est_g) > 0)
      annualG_draw[est_g] <- exp(rnorm(length(est_g), grp_mean[est_g], grp_sdlog[est_g]))
    Ydraw[, r] <- rake_one(y, mu, idxG, annualG_draw, lock_obs, dengue_obs, size = sz)
  }

  say("[time] run %d TOTAL: %.1fs\n", run_id, secs(tr))
  list(keys = paste(df$adm_0_name, df$Year, df$month, sep = "|"),
       draws = Ydraw, lock = lock_obs, est_group = grp_sdlog[cy] > 0,
       input_fp = INPUT_FP,
       param_monthly = param_monthly, param_downscale = param_downscale)
}

# ==============================================================================
# RUN LOOP (resumable: a run whose file already exists is skipped)
# ==============================================================================
cat("\n==================== RUNS ====================\n")
# >>> parallelism: runs are independent, resumable and seeded, so several processes can generate
#   them at once. Each worker takes a disjoint run-id range (MI_RUN_START..MI_RUN_END) with
#   MI_GENERATE_ONLY=1 (it exits before pooling); a final plain invocation then pools. INLA threads
#   per run = MI_INLA_THREADS (default 8). Example on 20 cores (4 workers x 5 threads), 30 runs:
#     for s in 1 9 17 25; do e=$((s+7)); [ $e -gt 30 ] && e=30; \
#       MI_RUN_START=$s MI_RUN_END=$e MI_INLA_THREADS=5 MI_GENERATE_ONLY=1 \
#         Rscript script/03b_run_pipeline_MI_full.R & done; wait
#     Rscript script/03b_run_pipeline_MI_full.R          # pool + convergence + validation
RUN_START     <- as.integer(Sys.getenv("MI_RUN_START", "1"))
RUN_END       <- as.integer(Sys.getenv("MI_RUN_END", as.character(N_RUNS)))
GENERATE_ONLY <- Sys.getenv("MI_GENERATE_ONLY", "0") %in% c("1", "true", "TRUE")

# >>> guard (input stamp): refuse to EXTEND a run set built on different/older inputs.
#   Skipped under MI_GENERATE_ONLY because concurrent workers are mid-writing each other's run
#   files (a partial read would falsely look stale); the pool step re-checks every run's fingerprint.
if (!GENERATE_ONLY) {
  .existing <- list.files(DOWN_DIR, "^run_\\d+\\.rds$", full.names = TRUE)
  if (length(.existing) > 0) {
    .fps <- vapply(.existing, function(f) {
      v <- tryCatch(readRDS(f)$input_fp, error = function(e) NULL)
      if (is.null(v)) NA_character_ else v
    }, character(1))
    .stale <- is.na(.fps) | .fps != INPUT_FP
    if (any(.stale)) stop(sprintf(paste0(
      "%d of %d existing run_*.rds were built on different/older inputs (fingerprint mismatch or\n",
      "  pre-stamp) and cannot be pooled with new runs. Delete them and re-run:\n",
      "    rm -f runs/mi_full/mi50/disaggregation/run_*.rds runs/mi_full/mi50/mi_pooled_cells.csv\n"),
      sum(.stale), length(.existing)))
  }
}
say("input fingerprint OK (%s) | runs %d-%d of %d | INLA threads %s%s\n",
    substr(INPUT_FP, 1, 12), RUN_START, RUN_END, N_RUNS,
    Sys.getenv("MI_INLA_THREADS", "8"), if (GENERATE_ONLY) " | generate-only" else "")
t_runs <- Sys.time()
for (run_id in RUN_START:RUN_END) {
  run_path <- file.path(DOWN_DIR, sprintf("run_%02d.rds", run_id))
  if (file.exists(run_path)) { say("run %d: already done, skipping\n", run_id); next }
  say("---- run %d/%d ----\n", run_id, N_RUNS)
  res <- tryCatch(one_run(run_id), error = function(e) {
    say("[ERROR] run %d failed: %s\n", run_id, conditionMessage(e)); NULL })
  if (is.null(res)) { say("run %d not saved; fix and re-run to resume.\n", run_id); next }
  saveRDS(res, run_path); say("run %d: saved\n", run_id)
}
say("[time] all runs: %.1f min\n", secs(t_runs) / 60)
if (GENERATE_ONLY) {
  say("generate-only: skipping pool. Run a plain invocation (no MI_GENERATE_ONLY) to pool.\n")
  quit(save = "no")
}

# ==============================================================================
# POOL DRAWS ACROSS RUNS -> point + 95% interval per cell
# ==============================================================================
cat("\n==================== POOL + SUMMARISE ====================\n")
run_files <- list.files(DOWN_DIR, "^run_\\d+\\.rds$", full.names = TRUE)
if (length(run_files) == 0) stop("No completed runs in ", DOWN_DIR)
runs <- lapply(run_files, readRDS)
keys0 <- runs[[1]]$keys
for (rr in runs) stopifnot(identical(rr$keys, keys0))
# >>> guard (input stamp): every pooled run must carry the CURRENT input fingerprint.
.rfp <- vapply(runs, function(x) if (is.null(x$input_fp)) NA_character_ else x$input_fp, character(1))
if (any(is.na(.rfp)) || any(.rfp != INPUT_FP))
  stop("Pooling aborted: some runs were built on different/older inputs (fingerprint mismatch).\n",
       "  Delete runs/mi_full/mi50/disaggregation/run_*.rds and regenerate so all runs share the current inputs.")
# >>> posterior summaries of the monthly and disaggregation fits (supplementary
#     tables of the final INLA fits): every run's summaries side by side, and a pooled version
#     (median over runs of the per-run median and interval bounds, with the range
#     of the per-run medians). Run files saved before this field existed carry no
#     summaries and are skipped.
#     NOTE: the production run set (run_01-50, generated 2026-07-31 to 2026-08-09)
#     predates the param_monthly / param_downscale fields, so this block writes
#     nothing for it; the summaries appear once the runs are regenerated. Until
#     then script/CV/CV_tab2_final_inla_fit_summaries_refit_run01.R provides the tables by
#     refitting the two models on the input of run 1.
.par <- do.call(rbind, lapply(runs, function(x) rbind(x$param_monthly, x$param_downscale)))
if (!is.null(.par) && nrow(.par) > 0) {
  write.csv(.par, file.path(SUMMARY_DIR, "inla_posterior_summaries_by_run.csv"), row.names = FALSE)
  .pooled <- .par %>%
    group_by(model, Parameter, Type) %>%
    summarise(n_runs = dplyr::n(),
              Median_min = min(Median), Median_max = max(Median),
              Median = median(Median), CI_0.025 = median(CI_0.025), CI_0.975 = median(CI_0.975),
              .groups = "drop") %>%
    select(model, Parameter, Type, n_runs, Median, CI_0.025, CI_0.975, Median_min, Median_max)
  write.csv(.pooled, file.path(SUMMARY_DIR, "inla_posterior_summaries_pooled.csv"), row.names = FALSE)
  say("wrote %s (fit summaries from %d runs)\n",
      file.path(SUMMARY_DIR, "inla_posterior_summaries_pooled.csv"), length(unique(.par$run)))
} else {
  say("no per-run fit summaries in the run files (runs predate this field); regenerate the runs for the supplementary fit tables\n")
}
Ydraw <- do.call(cbind, lapply(runs, function(x) x$draws))   # n_cells x (n_runs*R)

q <- function(m, p) apply(m, 1, quantile, probs = p, type = 8, na.rm = TRUE)

# >>> point estimate = MEAN, integerised per country-year (largest-remainder) so each
#   country-year's monthly integer sum preserves its annual total (additive).
#   A MEDIAN point estimate is NOT additive: for
#   right-skewed imputed cells sum(monthly medians) < annual, so aggregating monthly
#   medians would undershoot the calibrated annual / global headline totals.
row_mean <- rowMeans(Ydraw)
lock0    <- runs[[1]]$lock
cy_id    <- sub("\\|[0-9]+$", "", keys0)            # "adm_0_name|Year" (drop "|month")
point    <- numeric(length(row_mean))
point[lock0] <- round(row_mean[lock0])              # locked months = observed (integer)
for (ix in split(seq_along(row_mean), cy_id)) {
  lk   <- lock0[ix]
  free <- ix[!lk]
  if (length(free) == 0L) next                      # fully observed country-year
  budget <- round(sum(row_mean[ix])) - sum(point[ix[lk]])  # annual total - locked months
  if (budget <= 0) { point[free] <- 0L; next }
  vals <- floor(pmax(row_mean[free], 0))
  rem  <- budget - sum(vals)                         # remainder to distribute (0..n_free)
  if (rem > 0) {
    ord <- order(pmax(row_mean[free] - vals, 0), decreasing = TRUE)[seq_len(min(rem, length(free)))]
    vals[ord] <- vals[ord] + 1                       # +1 to the largest fractional parts
  }
  point[free] <- vals
}

out <- tibble(key = keys0) %>%
  separate(key, into = c("adm_0_name", "Year", "month"), sep = "\\|", convert = TRUE) %>%
  mutate(dengue_total_scaled = point,
         dengue_lwr_scaled = floor(q(Ydraw, 0.025)),
         dengue_upr_scaled = ceiling(q(Ydraw, 0.975)),
         width = dengue_upr_scaled - dengue_lwr_scaled,
         is_locked = runs[[1]]$lock, est_group = runs[[1]]$est_group)

# >>> restore the deterministic assumed-zero country-years FOR THE RELEASED FILE.
#   Assumed_zero_cases in dt_heatmap_calibrated.csv (emerging-setting country-years set to
#   0 by the 01c EES rule) are never modelled -- they carry no uncertainty (0, interval 0)
#   -- so they are absent from `out`/Ydraw by design. rbind them here so the release has the
#   full 60,060-cell country set of the released dataset. `out` itself is left
#   modelled-only, so the AGGREGATE block below stays aligned with Ydraw (zeros add 0 to
#   every total anyway). The analysis helper (03c_mi_datasets.R) restores the same set.
zero_cells <- read.csv(file.path(getwd(), psens("data/processed_data/dt_heatmap_calibrated.csv"))) %>%
  filter(data_source == "Assumed_zero_cases") %>%
  transmute(adm_0_name = toupper(adm_0_name), Year) %>% distinct() %>%
  tidyr::crossing(month = 1:12) %>%
  mutate(dengue_total_scaled = 0L, dengue_lwr_scaled = 0L, dengue_upr_scaled = 0L,
         width = 0L, is_locked = TRUE, est_group = FALSE)
stopifnot(nrow(dplyr::inner_join(zero_cells, out, by = c("adm_0_name", "Year", "month"))) == 0)
out_release <- dplyr::bind_rows(out, zero_cells) %>% arrange(adm_0_name, Year, month)
write.csv(out_release, file.path(MI_DIR, "mi_pooled_cells.csv"), row.names = FALSE)
say("wrote %s (%d cells = %d modelled + %d assumed-zero, pooled from %d runs)\n",
    file.path(MI_DIR, "mi_pooled_cells.csv"),
    nrow(out_release), nrow(out), nrow(zero_cells), length(runs))
say("NOTE: the released file (opendengue_gap_filled_MI.csv) is written by\n")
say("      mi_write_release() in script/03c_mi_datasets.R from this cell file.\n")

# ==============================================================================
# AGGREGATE TOTALS — global + per-year, with a 95% interval from the pooled draws
# ==============================================================================
# The global (and per-year) total carries uncertainty too: sum EACH pooled draw
# (one complete dataset) over the relevant cells, then take the 2.5/97.5 percentiles
# across draws. The point is the additive integerised cell total (so it matches
# mi_pooled_cells.csv); the interval comes from the draw spread. The interval
# is narrow because most of the total is observed annual data held fixed — the
# propagated uncertainty enters only via the estimated-annual cells and the imputation
# (it does NOT include uncertainty in the observed annual counts themselves).
cat("\n==================== AGGREGATE TOTALS ====================\n")
qv <- function(x, p) as.numeric(quantile(x, probs = p, type = 8, na.rm = TRUE))

# >>> region mapping for per-region totals. od_region is an ANALYSIS-layer concept
#   (functions/fn_OD_region.R); it is sourced HERE, in the POOL block only, so
#   one_run() / the RUN LOOP / the input-fingerprint guards are untouched and the
#   run_*.rds are reused with no INLA refit. A guard stops if any modelled country
#   fails to map -- a silent NA region is the only new risk of this design.
source(file.path(getwd(), "functions/fn_OD_region.R"))
out$ISO_A0 <- countrycode(out$adm_0_name, "country.name", "iso3c")
out$ISO_A0[out$adm_0_name == "SAINT MARTIN"] <- "MAF"
out <- add_od_regions(out, iso_col = "ISO_A0")
stopifnot(!any(is.na(out$od_region)))

# per-year totals
agg <- do.call(rbind, lapply(sort(unique(out$Year)), function(y) {
  ii <- which(out$Year == y)
  d  <- colSums(Ydraw[ii, , drop = FALSE])                 # per-draw total for year y
  data.frame(scope = "annual", Year = y, region = NA_character_,
             dengue_total = sum(out$dengue_total_scaled[ii]),   # additive point (matches cell file)
             lwr = floor(qv(d, 0.025)), upr = ceiling(qv(d, 0.975)))
}))
# per-region totals — same rule as global/annual (additive point + draw-quantile UI)
reg <- do.call(rbind, lapply(sort(unique(out$od_region)), function(r) {
  ii <- which(out$od_region == r)
  d  <- colSums(Ydraw[ii, , drop = FALSE])
  data.frame(scope = "region", Year = NA_integer_, region = r,
             dengue_total = sum(out$dengue_total_scaled[ii]),
             lwr = floor(qv(d, 0.025)), upr = ceiling(qv(d, 0.975)))
}))
gd  <- colSums(Ydraw)                                        # per-draw global total
glob <- data.frame(scope = "global", Year = NA_integer_, region = NA_character_,
                   dengue_total = sum(out$dengue_total_scaled),
                   lwr = floor(qv(gd, 0.025)), upr = ceiling(qv(gd, 0.975)))
write.csv(rbind(glob, reg, agg), file.path(SUMMARY_DIR, "aggregate_totals_MI.csv"), row.names = FALSE)
say("GLOBAL total: %s (95%% CI %s - %s)\n",
    format(glob$dengue_total, big.mark = ","), format(glob$lwr, big.mark = ","),
    format(glob$upr, big.mark = ","))
say("wrote %s (global + %d regions + %d years)\n",
    file.path(SUMMARY_DIR, "aggregate_totals_MI.csv"), nrow(reg), nrow(agg))
# The fold increase per period (2024 total / start-year total) is computed in
# script/04b_mi_growth_pool.R from these annual totals and the same draw matrix,
# alongside the growth rates it is reported with.

# per country-year totals — same rule (additive point + draw-quantile UI). Every draw is
# constrained to its country-year annual total, so the annual interval depends only on how
# that total was obtained:
#   (a) observed or calibrated annual total: identical in every draw -> interval
#       collapses to the point (width 0);
#   (b) est_group (IHME / neighbour-median annual total): drawn lognormally per
#       draw -> non-zero width;
#   (c) sub-annual imputation years (partial weekly/monthly data, no usable annual
#       total): the annual total = observed months + months imputed at the weekly /
#       monthly stage, one realisation per run, so it is constant within a run but
#       differs between runs -> non-zero width (between-run uncertainty).
# This is why the monthly lwr/upr in mi_pooled_cells.csv must NEVER be summed to an
# annual interval: quantiles are not additive, and summing them ignores the annual-total
# constraint (a fixed annual total would get a wide spurious interval). Assumed-zero
# country-years are appended as 0/0/0 so the file covers the same country-years as
# the released dataset. The two classification columns are used for the checks
# below and dropped before the file is written.
cy_idx <- split(seq_len(nrow(out)), paste(out$adm_0_name, out$Year, sep = "|"))
cyt <- do.call(rbind, lapply(cy_idx, function(ii) {
  d <- colSums(Ydraw[ii, , drop = FALSE])                 # per-draw annual total
  data.frame(adm_0_name = out$adm_0_name[ii[1]], ISO_A0 = out$ISO_A0[ii[1]],
             od_region = out$od_region[ii[1]], Year = out$Year[ii[1]],
             dengue_total = sum(out$dengue_total_scaled[ii]),
             lwr = floor(qv(d, 0.025)), upr = ceiling(qv(d, 0.975)),
             annual_fixed = diff(range(d)) == 0,           # (a) above
             annual_est_group = any(out$est_group[ii]),   # (b) above
             stringsAsFactors = FALSE)
}))
rownames(cyt) <- NULL
# guard: a fixed annual total must give a degenerate interval (lwr == point == upr)
stopifnot(all(cyt$lwr[cyt$annual_fixed] == cyt$dengue_total[cyt$annual_fixed]),
          all(cyt$upr[cyt$annual_fixed] == cyt$dengue_total[cyt$annual_fixed]),
          !any(cyt$annual_fixed & cyt$annual_est_group))
zero_cy <- zero_cells %>% distinct(adm_0_name, Year) %>%
  mutate(ISO_A0 = countrycode(adm_0_name, "country.name", "iso3c"))
zero_cy$ISO_A0[zero_cy$adm_0_name == "SAINT MARTIN"] <- "MAF"
zero_cy <- add_od_regions(zero_cy, iso_col = "ISO_A0") %>%
  transmute(adm_0_name, ISO_A0, od_region, Year, dengue_total = 0L, lwr = 0L, upr = 0L,
            annual_fixed = TRUE, annual_est_group = FALSE)
stopifnot(!any(is.na(zero_cy$od_region)),
          nrow(dplyr::inner_join(zero_cy, cyt, by = c("adm_0_name", "Year"))) == 0)
cyt <- dplyr::bind_rows(cyt, zero_cy) %>% arrange(adm_0_name, Year)
say("country-year totals: %d country-years = %d modelled + %d assumed-zero\n",
    nrow(cyt), length(cy_idx), nrow(zero_cy))
say("  annual total fixed (width 0): %d | est_group: %d | sub-annual imputation (between-run): %d\n",
    sum(cyt$annual_fixed), sum(cyt$annual_est_group), sum(!cyt$annual_fixed & !cyt$annual_est_group))
say("  interval width > 0 in %d country-years (all non-fixed: %s)\n",
    sum(cyt$upr > cyt$lwr), ifelse(all(!cyt$annual_fixed[cyt$upr > cyt$lwr]), "yes", "NO - review"))
write.csv(cyt %>% dplyr::select(adm_0_name, ISO_A0, od_region, Year, dengue_total, lwr, upr),
          file.path(SUMMARY_DIR, "country_year_totals_MI.csv"), row.names = FALSE)
say("wrote %s\n", file.path(SUMMARY_DIR, "country_year_totals_MI.csv"))

# ==============================================================================
# CONVERGENCE DIAGNOSTIC — is N_RUNS enough? (cumulative pooled interval width)
# ==============================================================================
# Pool the first k runs (k = 1..N) and track the median 95% interval width, split by
# cell type: estimated-annual cells (IHME / neighbour-median totals — the uncertainty-
# dominant group) vs ordinary gap-filled cells. If the width flattens well before the
# last run, N_RUNS is more than enough. Each run adds R draws, so k runs = k*R pooled
# draws. Mirrors the 2026-07-21 convergence check; writes convergence_by_run.csv for a plot.
cat("\n==================== CONVERGENCE DIAGNOSTIC ====================\n")
free_idx    <- which(!out$is_locked)
est_in_free <- out$est_group[free_idx]
conv <- data.frame(runs = seq_along(runs), draws = NA_integer_,
                   w_est = NA_real_, w_oth = NA_real_)
for (k in seq_along(runs)) {
  Yk <- do.call(cbind, lapply(runs[seq_len(k)], function(x) x$draws[free_idx, , drop = FALSE]))
  wk <- ceiling(q(Yk, 0.975)) - floor(q(Yk, 0.025))
  conv$draws[k] <- ncol(Yk)
  conv$w_est[k] <- median(wk[est_in_free],  na.rm = TRUE)
  conv$w_oth[k] <- median(wk[!est_in_free], na.rm = TRUE)
}
conv$d_est_pct <- c(NA, 100 * abs(diff(conv$w_est)) / conv$w_est[-nrow(conv)])
conv$d_oth_pct <- c(NA, 100 * abs(diff(conv$w_oth)) / conv$w_oth[-nrow(conv)])
print(conv, row.names = FALSE, digits = 4)
write.csv(conv, file.path(SUMMARY_DIR, "convergence_by_run.csv"), row.names = FALSE)
# earliest run from which the (dominant) estimated-cell width stays within tol for the rest
stable_tol <- 2  # percent step-change
stable_from <- NA_integer_
for (k in seq_len(nrow(conv) - 1)) {
  if (all(conv$d_est_pct[(k + 1):nrow(conv)] < stable_tol, na.rm = TRUE)) { stable_from <- k; break }
}
say("estimated-annual width stable (<%d%% step-change) from run %s of %d%s\n",
    stable_tol, ifelse(is.na(stable_from), "never", as.character(stable_from)), length(runs),
    if (!is.na(stable_from)) sprintf(" -> N_RUNS=%d is more than sufficient", length(runs)) else
      " -> not yet flat; consider more runs")
say("wrote %s\n", file.path(SUMMARY_DIR, "convergence_by_run.csv"))

# ==============================================================================
# VALIDATION — did the intended method actually happen?
# ==============================================================================
cat("\n==================== VALIDATION ====================\n")
cell_var <- apply(Ydraw, 1, function(v) stats::var(v, na.rm = TRUE))
locked <- out$is_locked; free <- !locked

a_bad <- sum(locked & cell_var > 1e-9, na.rm = TRUE)
say("A. observed months constant across draws : %s (%d of %d locked varied)\n",
    ifelse(a_bad == 0, "PASS", "FAIL"), a_bad, sum(locked))
b_frac <- mean(cell_var[free] > 0, na.rm = TRUE)
say("B. gap-filled months vary across draws    : %s (%.0f%% of %d free cells vary)\n",
    ifelse(b_frac > 0.5, "PASS", "CHECK"), 100 * b_frac, sum(free))
w_est <- median(out$width[out$est_group & free], na.rm = TRUE)
w_oth <- median(out$width[!out$est_group & free], na.rm = TRUE)
say("C. median width estimated-annual=%.0f vs other=%.0f (%s)\n", w_est, w_oth,
    ifelse(is.finite(w_est) && is.finite(w_oth) && w_est >= w_oth, "as expected", "review"))
say("D. the annual-total constraint is enforced inside rake_one every draw (held by construction)\n")
say("\nDONE -> %s\n", file.path(MI_DIR, "mi_pooled_cells.csv"))
