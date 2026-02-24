rm(list = ls())
gc()
library(ggplot2)
library(patchwork)
library(future)
library(furrr)
library(dplyr)
library(tidyr)
library(purrr)

# GAM CV on full dataset
source("script/CV/00_build_gap_mask_ext.R")
source("script/CV/00_build_gap_mask.R")
source("script/CV/00_imp_model_gam_spec.R")
source("script/CV/00_gam_eval_helpers.R")


data <- read.csv("data/model_input/model_data_monthly_new.csv")
data <- data %>%
  mutate(
    adm_0_name = factor(adm_0_name),
    Year = factor(Year), # re term should be a factor, not large numeric
    region = factor(lat_band),
    regionx = as.integer(region),
    month_shared = month, # shared seasonal curve
    month_dev = month # country-specific deviation curve
  ) %>%
  arrange(adm_0_name, time_seq)



seed <- 123
set.seed(seed)


formulas <- list(
  gam_bench_org = gam_m_bench_original
)


# ==========================================================
# Repeated CV (GAM) using INLA masks
# ==========================================================

base_dir <- "runs/CV/20260126"
reps <- 1:3
folds <- 1:3

# Match the INLA mask seeds used
mask_seeds <- c(123, 456, 789)

# Final output collectors
all_overall <- list()
all_fold <- list()

plan(multisession, workers = 4)

# Helper: find where INLA masks live for each repetition
find_inla_mask_dir <- function(base_dir, rep_i) {
  rep_tag <- sprintf("rep%02d", rep_i)
  candidates <- c(
    file.path(base_dir, "masks", paste0("monthly_", rep_tag)), # e.g. masks/monthly_rep01
    file.path(base_dir, "masks", paste0("monthly_rep", sprintf("%02d", rep_i))), # e.g. masks/monthly_rep01
    file.path(base_dir, "masks", rep_tag), # e.g. masks/rep01
    file.path(base_dir, "masks") # fallback (single-run masks)
  )
  for (d in candidates) {
    if (file.exists(file.path(d, "inla_full_mask_m_interp.rds"))) {
      return(d)
    }
  }
  stop(
    "Could not find INLA mask directory for rep ", rep_tag,
    "\nTried:\n", paste(candidates, collapse = "\n")
  )
}

for (rep_i in reps) {
  seed <- mask_seeds[rep_i]
  rep_tag <- sprintf("rep%02d", rep_i)
  out_dir_rep <- file.path(base_dir, sprintf("gam_full_CV_monthly_%s", rep_tag)) # <- your requested naming
  dir.create(out_dir_rep, recursive = TRUE, showWarnings = FALSE)

  inla_mask_dir_rep <- find_inla_mask_dir(base_dir, rep_i)

  mask_files <- list(
    interp        = file.path(inla_mask_dir_rep, "inla_full_mask_m_interp.rds"),
    extrap_future = file.path(inla_mask_dir_rep, "inla_full_mask_m_extrap_future.rds"),
    extrap_past   = file.path(inla_mask_dir_rep, "inla_full_mask_m_extrap_past.rds")
  )

  missing <- names(mask_files)[!file.exists(unlist(mask_files))]
  if (length(missing)) {
    stop(
      "Rep ", rep_tag, ": missing INLA masks: ", paste(missing, collapse = ", "),
      "\nChecked:\n", paste(unlist(mask_files), collapse = "\n")
    )
  }

  # Pass file paths; run_job_gam_pp_cached MUST read them via read_mask_obj (see fix above)
  masks <- mask_files

  cat("\n====================\n")
  cat("GAM repeated CV:", rep_tag, "\n")
  cat("Using INLA masks from:", inla_mask_dir_rep, "\n")
  cat("Saving outputs to:", out_dir_rep, "\n")
  cat("====================\n")

  jobs_gam <- tidyr::crossing(
    model = names(formulas),
    mask_type = names(masks),
    fold = folds
  )

  res_gam <- purrr::pmap_dfr(
    jobs_gam, run_job_gam_pp_cached,
    nsamp = 300,
    seed = 20260126 + rep_i,
    data = data, masks = masks, formulas = formulas, family = nb(),
    out_dir = out_dir_rep
  )


  # MAE/RMSE (point) - BOTH mean & median
  tabs_mm <- summarise_gam_metrics_incidence_mean_median(res_gam, data = data, per = 1e5)

  # Make overall wide: mean_based vs median_based -> *_mean / *_median
  overall_wide <- tabs_mm$metrics_overall %>%
    tidyr::pivot_wider(
      names_from = point_est,
      values_from = c(n_test, MAE_i, RMSE_i)
    ) %>%
    dplyr::transmute(
      model, mask_type,
      n_test_mean = n_test_mean_based,
      MAE_inc_mean = MAE_i_mean_based,
      RMSE_inc_mean = RMSE_i_mean_based,
      n_test_median = n_test_median_based,
      MAE_inc_median = MAE_i_median_based,
      RMSE_inc_median = RMSE_i_median_based
    )

  fold_wide <- tabs_mm$metrics_fold %>%
    tidyr::pivot_wider(
      names_from = point_est,
      values_from = c(n_test, MAE_i, RMSE_i)
    ) %>%
    dplyr::transmute(
      model, mask_type, fold,
      n_test_mean = n_test_mean_based,
      MAE_inc_mean = MAE_i_mean_based,
      RMSE_inc_mean = RMSE_i_mean_based,
      n_test_median = n_test_median_based,
      MAE_inc_median = MAE_i_median_based,
      RMSE_inc_median = RMSE_i_median_based
    )

  # COV80/CRPS (probabilistic) - unchanged
  res_prob <- gam_cov_crps_all_new(
    models = names(formulas),
    masks = masks,
    folds = folds,
    data = data,
    out_dir = out_dir_rep,
    nsamp = 300,
    per = 1e5,
    seed = 123 + rep_i
  )

  consolidated_overall <- overall_wide %>%
    dplyr::left_join(
      res_prob$overall %>% dplyr::select(model, mask_type, COV80, CRPS),
      by = c("model", "mask_type")
    ) %>%
    dplyr::mutate(rep = rep_i, mask_seed = seed)

  consolidated_fold <- fold_wide %>%
    dplyr::left_join(
      res_prob$fold %>% dplyr::select(model, mask_type, fold, COV80, CRPS),
      by = c("model", "mask_type", "fold")
    ) %>%
    dplyr::mutate(rep = rep_i, mask_seed = seed)

  write.csv(consolidated_overall,
    file.path(out_dir_rep, "gam_metrics_monthly.csv"),
    row.names = FALSE
  )
  write.csv(consolidated_fold,
    file.path(out_dir_rep, "gam_metrics_monthly_fold.csv"),
    row.names = FALSE
  )

  all_overall[[rep_tag]] <- consolidated_overall
  all_fold[[rep_tag]] <- consolidated_fold
}

# Combine repetitions into a single file (optional but handy)
overall_allreps <- dplyr::bind_rows(all_overall)
fold_allreps <- dplyr::bind_rows(all_fold)

write.csv(overall_allreps,
  file.path(base_dir, "gam_metrics_monthly_repeatedCV_overall.csv"),
  row.names = FALSE
)
write.csv(fold_allreps,
  file.path(base_dir, "gam_metrics_monthly_repeatedCV_fold.csv"),
  row.names = FALSE
)

# Optional: summary across repetitions
summary_overall <- overall_allreps %>%
  dplyr::group_by(model, mask_type) %>%
  dplyr::summarise(
    MAE_mean_based_mean = mean(MAE_inc_mean, na.rm = TRUE),
    MAE_mean_based_sd = sd(MAE_inc_mean, na.rm = TRUE),
    RMSE_mean_based_mean = mean(RMSE_inc_mean, na.rm = TRUE),
    RMSE_mean_based_sd = sd(RMSE_inc_mean, na.rm = TRUE),
    MAE_median_based_mean = mean(MAE_inc_median, na.rm = TRUE),
    MAE_median_based_sd = sd(MAE_inc_median, na.rm = TRUE),
    RMSE_median_based_mean = mean(RMSE_inc_median, na.rm = TRUE),
    RMSE_median_based_sd = sd(RMSE_inc_median, na.rm = TRUE),
    COV80_mean = mean(COV80, na.rm = TRUE),
    COV80_sd = sd(COV80, na.rm = TRUE),
    CRPS_mean = mean(CRPS, na.rm = TRUE),
    CRPS_sd = sd(CRPS, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(summary_overall,
  file.path(base_dir, "gam_metrics_monthly_repeatedCV_summary.csv"),
  row.names = FALSE
)

print(summary_overall)
