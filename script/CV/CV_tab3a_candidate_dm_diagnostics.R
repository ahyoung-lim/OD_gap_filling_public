# CV_tab3a_candidate_dm_diagnostics.R
#
# Supplementary tables for the Dirichlet-multinomial (DM) candidate model, from
# its repeated cross-validation re-run with 4 chains x 1,000 draws
# (script/CV/03_down_DM_full_repeatedCV_v2.py; outputs in
# runs/CV/20260126/dm_rerun_4chains/).
#
# Tables in the layout of the supplementary document (output/tables/):
#   CV_tab3a_dm_convergence.csv
#       Convergence diagnostics per mask type for two parameter sets: the
#       hyperparameters (key scalars monitored by the run) and the interpolated
#       monthly proportions of the held-out country-years (p_missing). For each
#       fit the largest R-hat and the smallest bulk / tail ESS over the set are
#       taken; the table reports the median and the worst value over the nine
#       fits (3 repetitions x 3 folds) of each mask type, and the share of fits
#       with divergent transitions.
#   CV_tab3a_dm_hyperparameters_by_mask.csv
#       Posterior median and 95% interval of the key hyperparameters per mask
#       type: the 9-fit median of the per-fit median and interval bounds.
#
# Intermediate record (runs/CV/20260126/dm_rerun_4chains/CV_tab3a/):
#   CV_tab3a_dm_convergence_full.csv, CV_tab3a_dm_hyperparameters_by_mask_full.csv
#       the two tables above, unrounded
#   CV_tab3a_dm_hyperparameters_all_fits.csv
#       hyperparameters pooled over all 27 fits, with the range of the per-fit medians
#   CV_tab3a_dm_prior_spec.csv
#       prior specification as exported by the run

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

BASE_DIR <- "runs/CV/20260126/dm_rerun_4chains"
INT_DIR  <- file.path(BASE_DIR, "CV_tab3a")
DOCX_DIR <- file.path("output", "tables")
dir.create(INT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(DOCX_DIR, showWarnings = FALSE, recursive = TRUE)

conv_fp  <- file.path(BASE_DIR, "pymc_convergence_diagnostics_fold.csv")
pmis_fp  <- file.path(BASE_DIR, "pymc_convergence_p_missing_fold.csv")
hyp_fp   <- file.path(BASE_DIR, "pymc_hyperparam_summaries_fold.csv")
prior_fp <- list.files(BASE_DIR, pattern = "_prior_spec\\.csv$", full.names = TRUE)
prior_fp <- if (length(prior_fp)) prior_fp[1] else NA_character_
stopifnot(file.exists(conv_fp), file.exists(hyp_fp))

mask_levels <- c(interp = "Interpolation",
                 extrap_past = "Extrapolation (backcast)",
                 extrap_future = "Extrapolation (forecast)")
mask_label <- function(x) factor(unname(mask_levels[x]), levels = mask_levels)

# =============================================================================
# Convergence diagnostics
# =============================================================================
conv <- read_csv(conv_fp, show_col_types = FALSE)

# per-fit worst value over the key hyperparameters (the *_key columns when the
# run wrote them, otherwise the columns over the monitored variables)
pick_col <- function(df, cands) cands[cands %in% names(df)][1]
rhat_col <- pick_col(conv, c("rhat_max_key", "rhat_max"))
essb_col <- pick_col(conv, c("ess_bulk_min_key", "ess_bulk_min"))
esst_col <- pick_col(conv, c("ess_tail_min_key", "ess_tail_min"))

per_fit_hyper <- conv %>%
  transmute(mask_type, rep, fold,
            divergences = as.integer(divergences),
            rhat_max = .data[[rhat_col]],
            ess_bulk_min = .data[[essb_col]],
            ess_tail_min = .data[[esst_col]],
            parameter_set = "Hyperparameters")

# per-fit worst value over the interpolated monthly proportions (one row per
# element of p_missing in the input file)
per_fit_pmis <- NULL
if (file.exists(pmis_fp)) {
  per_fit_pmis <- read_csv(pmis_fp, show_col_types = FALSE) %>%
    group_by(mask_type, rep, fold) %>%
    summarise(rhat_max = max(r_hat), ess_bulk_min = min(ess_bulk), ess_tail_min = min(ess_tail),
              .groups = "drop") %>%
    left_join(per_fit_hyper %>% select(mask_type, rep, fold, divergences),
              by = c("mask_type", "rep", "fold")) %>%
    mutate(parameter_set = "Interpolated monthly proportions")
} else {
  message("No p_missing diagnostics file; the convergence table has the hyperparameter block only.")
}

conv_tab <- bind_rows(per_fit_hyper, per_fit_pmis) %>%
  mutate(parameter_set = factor(parameter_set,
                                levels = c("Hyperparameters", "Interpolated monthly proportions")),
         mask = mask_label(mask_type)) %>%
  group_by(parameter_set, mask) %>%
  summarise(
    n_fits = n(),
    pct_fits_with_divergences = 100 * mean(divergences > 0),
    rhat_median = median(rhat_max),
    rhat_max = max(rhat_max),
    ess_bulk_median = median(ess_bulk_min),
    ess_bulk_min = min(ess_bulk_min),
    ess_tail_median = median(ess_tail_min),
    ess_tail_min = min(ess_tail_min),
    .groups = "drop"
  ) %>%
  arrange(parameter_set, mask)

write_csv(conv_tab, file.path(INT_DIR, "CV_tab3a_dm_convergence_full.csv"))

conv_docx <- conv_tab %>%
  transmute(
    `Parameter set` = parameter_set, `Mask type` = mask, `N fits` = n_fits,
    `% fits with divergences` = sprintf("%.0f", pct_fits_with_divergences),
    `R-hat median` = sprintf("%.2f", rhat_median), `R-hat max` = sprintf("%.2f", rhat_max),
    `ESS bulk median` = sprintf("%.0f", ess_bulk_median), `ESS bulk min` = sprintf("%.0f", ess_bulk_min),
    `ESS tail median` = sprintf("%.0f", ess_tail_median), `ESS tail min` = sprintf("%.0f", ess_tail_min)
  )
write_csv(conv_docx, file.path(DOCX_DIR, "CV_tab3a_dm_convergence.csv"))

# =============================================================================
# Key hyperparameter posterior summaries
# =============================================================================
hyp <- read_csv(hyp_fp, show_col_types = FALSE)

KEY_PARAMS <- c(
  "tau_country_month",
  "sigma_region_month_mean",
  "tau_region_year_mean",
  "log_concentration_mean",
  "total_concentration_mean"
)

hyp_filt <- hyp %>%
  filter(prior_tag == "baseline", param %in% KEY_PARAMS)

# per mask type: 9-fit median of the per-fit median and interval bounds
hyp_tab_by_mask <- hyp_filt %>%
  mutate(mask = mask_label(mask_type)) %>%
  group_by(mask, param) %>%
  summarise(
    n_fits = n(),
    Median = median(median),
    CI_0.025 = median(q025),
    CI_0.975 = median(q975),
    .groups = "drop"
  ) %>%
  arrange(mask, match(param, KEY_PARAMS)) %>%
  rename(Parameter = param)

write_csv(hyp_tab_by_mask, file.path(INT_DIR, "CV_tab3a_dm_hyperparameters_by_mask_full.csv"))

hyp_docx <- hyp_tab_by_mask %>%
  transmute(`Mask type` = mask, Parameter,
            Median = sprintf("%.3f", Median),
            CI_0.025 = sprintf("%.3f", CI_0.025), CI_0.975 = sprintf("%.3f", CI_0.975))
write_csv(hyp_docx, file.path(DOCX_DIR, "CV_tab3a_dm_hyperparameters_by_mask.csv"))

# pooled over all 27 fits, with the range of the per-fit medians
hyp_tab_all <- hyp_filt %>%
  group_by(param) %>%
  summarise(
    n_fits = n(),
    Median = median(median),
    CI_0.025 = median(q025),
    CI_0.975 = median(q975),
    Median_min = min(median),
    Median_max = max(median),
    .groups = "drop"
  ) %>%
  arrange(match(param, KEY_PARAMS)) %>%
  rename(Parameter = param)

write_csv(hyp_tab_all, file.path(INT_DIR, "CV_tab3a_dm_hyperparameters_all_fits.csv"))

# =============================================================================
# Prior specification
# =============================================================================
if (!is.na(prior_fp)) {
  pri <- read_csv(prior_fp, show_col_types = FALSE) %>%
    transmute(parameter_block = param_block, parameter = param_name, prior = prior, note = comment)
  write_csv(pri, file.path(INT_DIR, "CV_tab3a_dm_prior_spec.csv"))
} else {
  message("No *_prior_spec.csv found in ", BASE_DIR)
}

# =============================================================================
# Console
# =============================================================================
cat("\n=== DM convergence diagnostics (output/tables/CV_tab3a_dm_convergence.csv) ===\n")
print(as.data.frame(conv_docx), row.names = FALSE)
cat("\n=== DM key hyperparameters by mask type (output/tables/CV_tab3a_dm_hyperparameters_by_mask.csv) ===\n")
print(as.data.frame(hyp_docx), row.names = FALSE)
cat("\nintermediate files in", INT_DIR, "\n")
