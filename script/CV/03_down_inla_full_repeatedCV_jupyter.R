# ============================================================================
# Downscaling INLA — repeated CV (3 reps)
# ============================================================================
source("script/CV/00_build_gap_mask_ext.R")
source("script/CV/00_build_gap_mask.R")
source("script/CV/00_build_gap_mask_downscaling.R")
source("script/CV/00_imp_model_inla_spec.R")
source("script/CV/00_inla_eval_helpers_new.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(future)
  library(furrr)
})



# ============================================================================
# Data
# ============================================================================

seed_global <- 123
set.seed(seed_global)

# IMPORTANT: keep the data ordering stable BEFORE creating row_id
# (row_id must match between mask creation, model fitting, and evaluation)

data <- read.csv("data/model_input/model_data_downscaling_new.csv")

data <- data %>%
  mutate(
    region = factor(lat_band),
    regionx = as.integer(region),
    month_shared = month,
    month_dev = month,
    month_dev_country = month
  ) %>%
  arrange(adm_0_name, time_seq)

# Keep only country-years with internally consistent monthly sums (if present)
if (all(c("annual_total", "Year", "dengue_total") %in% names(data))) {
  data <- data %>%
    group_by(adm_0_name, Year) %>%
    mutate(sum_of_monthly = sum(dengue_total, na.rm = TRUE)) %>%
    filter(sum_of_monthly == annual_total) %>%
    ungroup()
}

# Enforce factor types consistent with other scripts
if ("adm_0_name" %in% names(data)) data <- data %>% mutate(adm_0_name = factor(adm_0_name))
if ("Year" %in% names(data)) data <- data %>% mutate(Year = factor(Year))

# Create stable row_id ONCE
if (!"row_id" %in% names(data)) {
  data <- data %>% mutate(row_id = row_number())
}

# ============================================================================
# Repeated CV settings
# ============================================================================

base_dir <- "runs/CV/20260126"

n_folds <- 3
folds <- 1:n_folds

# 3 repetitions
mask_seeds <- c(123, 456, 789)

# Keep these aligned with your other scripts
run_seed_base <- 20250811
crps_seed_base <- 123

# Model formulas (keep names stable: these become the `model` field in outputs)
formulas <- list(
  inla_h_shared = inla_m_hier_shared_formula
)

# Threading
Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OMP_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)
INLA::inla.setOption(num.threads = "1:1", save.memory = FALSE)

workers <- max(1, floor(parallel::detectCores(logical = FALSE) / 2))
plan(multisession, workers = workers)

# Collectors across repetitions
all_overall <- list()
all_fold <- list()

# Rebase settings (scaled monthly predictions)
rebase_group_cols <- intersect(c("adm_0_name", "Year"), names(data))
rebase_target_col <- if ("annual_total" %in% names(data)) "annual_total" else NA_character_

for (rep_i in seq_along(mask_seeds)) {
  seed <- mask_seeds[rep_i]
  set.seed(seed)

  rep_tag <- sprintf("rep%02d", rep_i)

  out_dir_rep <- file.path(base_dir, paste0("down_inla_full_CV_", rep_tag))
  mask_dir_rep <- file.path(base_dir, "masks", paste0("downscaling_", rep_tag))
  dir.create(out_dir_rep, recursive = TRUE, showWarnings = FALSE)
  dir.create(mask_dir_rep, recursive = TRUE, showWarnings = FALSE)

  cat("\n====================\n")
  cat("Running DOWNSCALING repeated CV:", rep_tag, "mask_seed =", seed, "\n")
  cat("Masks:", mask_dir_rep, "\n")
  cat("Out:", out_dir_rep, "\n")
  cat("====================\n")

  # ------------------------------------------------------------------------
  # Build masks (downscaling: yearwise masks; set.seed() controls sampling)
  # ------------------------------------------------------------------------

  mask_i <- build_gap_mask_yearwise_interpolation(
    data,
    n_folds = 3,
    edge_years = 2,
    test_frac_target = 0.30
  )

  if (rep_i == 1) {
    fold_train_test_ratio(mask_i, data)
    plot_fold_train_test(data, mask_i, fold_id = 1)
  }

  mask_i <- stamp_mask(
    mask_i, data,
    name = "down_interp",
    params = list(cv_kind = "balanced", gap_len = 12, edge = 2, data_id = "down_full"),
    seed = seed
  )

  save_mask(mask_i, dir = mask_dir_rep, filename = "inla_down_mask_interp.rds")
  if (rep_i == 1) summarise_mask(mask_i)

  mask_ex_past <- build_extrapolation_mask_yearwise(
    data,
    n_folds = 3,
    direction = "past",
    min_interior_years = 1,
    test_frac_target = 0.2
  )

  if (rep_i == 1) {
    fold_train_test_ratio(mask_ex_past, data)
    plot_fold_train_test(data, mask_ex_past, fold_id = 3, reorder_countries = FALSE)
  }

  mask_ex_past <- stamp_mask(
    mask_ex_past, data,
    name = "down_extrap",
    params = list(
      cv_kind = "rolling", direction = "past",
      gap_len = 12, min_interior_years = 1, test_frac_target = 0.2,
      boundary_mode = "k", data_id = "down_full"
    ),
    seed = seed
  )

  save_mask(mask_ex_past, dir = mask_dir_rep, filename = "inla_down_mask_extrap_past.rds")
  if (rep_i == 1) summarise_mask(mask_ex_past)

  mask_ex_future <- build_extrapolation_mask_yearwise(
    data,
    n_folds = 3,
    direction = "future",
    min_interior_years = 1,
    test_frac_target = 0.2
  )

  if (rep_i == 1) {
    fold_train_test_ratio(mask_ex_future, data)
    plot_fold_train_test(data, mask_ex_future, fold_id = 2, reorder_countries = FALSE)
  }

  mask_ex_future <- stamp_mask(
    mask_ex_future, data,
    name = "down_extrap",
    params = list(
      cv_kind = "rolling", direction = "future",
      gap_len = 12, min_interior_years = 1, test_frac_target = 0.2,
      boundary_mode = "k", data_id = "down_full"
    ),
    seed = seed
  )

  save_mask(mask_ex_future, dir = mask_dir_rep, filename = "inla_down_mask_extrap_future.rds")
  if (rep_i == 1) summarise_mask(mask_ex_future)

  masks <- list(
    interp = mask_i,
    extrap_future = mask_ex_future,
    extrap_past = mask_ex_past
  )

  # ------------------------------------------------------------------------
  # Run INLA CV (per repetition)
  # ------------------------------------------------------------------------
  # Uncomment to run fits. Keep out_dir_rep consistent between fitting + eval.
  #
  # run_inla_parallel_jupyter(
  #   data = data, masks = masks, formulas = formulas, ctrl_fam = ctrl_fam,
  #   folds = folds,
  #   out_dir = out_dir_rep,
  #   workers = 10,
  #   nsamp = 300,
  #   seed = run_seed_base + rep_i
  # )

  # =========================================================
  # Quick INLA adequacy + scaling sanity checks (CV fits)
  # - Numerical success: fit/meta file exists + readable + finite hyperparameter summary
  # - Hyperparameter sanity: rho not stuck at +/-1
  # - Scaling sanity: per-draw rebasing makes country-year sums match annual_total
  # =========================================================

  # ---- run diagnostics over all CV jobs for this rep ----
  models_cv <- names(formulas)
  mask_types_cv <- names(masks)
  folds_cv <- folds

  jobs_cv <- expand.grid(
    model = models_cv,
    mask_type = mask_types_cv,
    fold = folds_cv,
    stringsAsFactors = FALSE
  )

  diag_list <- Map(
    f = check_one_fit_with_rebase,
    model = jobs_cv$model,
    mask_type = jobs_cv$mask_type,
    fold = jobs_cv$fold,
    MoreArgs = list(
      out_dir = out_dir_rep,
      data = data,
      nsamp_check = 30,
      seed = 1000 + rep_i,
      group_cols = c("adm_0_name", "Year"),
      target_col = "annual_total",
      min_group_size = 12,
      n_groups_check = 3,
      rel_tol = 1e-6
    )
  )

  diag_df <- do.call(rbind, diag_list)
  diag_df$rep <- rep_i
  diag_df$mask_seed <- seed

  diag_path <- file.path(out_dir_rep, "inla_algorithm_adequacy_downscaling.csv")
  write.csv(diag_df, diag_path, row.names = FALSE)

  n_total <- nrow(diag_df)
  n_ok <- sum(diag_df$ok, na.rm = TRUE)
  n_missing_fit <- sum(!diag_df$exists_fit, na.rm = TRUE)
  n_missing_meta <- sum(!diag_df$exists_meta, na.rm = TRUE)
  n_rho_boundary <- sum(diag_df$rho_boundary_flag %in% TRUE, na.rm = TRUE)
  n_rebase_warn <- sum(grepl("rebase_warn", diag_df$note %||% ""), na.rm = TRUE)

  cat(sprintf(
    "\nINLA diagnostic (%s): ok=%d/%d | missing_fit=%d | missing_meta=%d | rho_boundary=%d | rebase_warn=%d\nSaved: %s\n\n",
    rep_tag, n_ok, n_total, n_missing_fit, n_missing_meta, n_rho_boundary, n_rebase_warn, diag_path
  ))


  # ------------------------------------------------------------------------
  # Metrics + save (per repetition)
  # - point metrics (mean + median)
  # - prob metrics (COV80/CRPS) for unscaled and rebased (scaled)
  # ------------------------------------------------------------------------

  check_inla_data_fit_alignment(data, out_dir_rep, n_spotcheck = 3)
  arts <- read_inla_artifacts(out_dir = out_dir_rep)

  # Point metrics: unscaled (mean)
  tabs_mean <- make_metrics_counts_incidence_prob(
    res_point_df = arts$point,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_mean"
  )

  # Point metrics: unscaled (median)
  tabs_median <- make_metrics_counts_incidence_prob(
    res_point_df = arts$point,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_median"
  )

  # Add rebased point predictions (scaled)
  point_w_rebase <- arts$point
  if (length(rebase_group_cols) == 2 && is.character(rebase_target_col) && !is.na(rebase_target_col)) {
    point_w_rebase <- add_rebased_point_predictions(
      res_point_df = arts$point,
      data_df = data,
      group_cols = rebase_group_cols,
      target_col = rebase_target_col,
      pred_cols = c("pred_mean", "pred_median"),
      clamp_nonneg = TRUE
    )
  }

  # Ensure expected columns exist even if rebasing is disabled
  if (!"pred_mean_rebase" %in% names(point_w_rebase)) point_w_rebase$pred_mean_rebase <- NA_real_
  if (!"pred_median_rebase" %in% names(point_w_rebase)) point_w_rebase$pred_median_rebase <- NA_real_

  tabs_mean_rebase <- make_metrics_counts_incidence_prob(
    res_point_df = point_w_rebase,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_mean_rebase"
  )

  tabs_median_rebase <- make_metrics_counts_incidence_prob(
    res_point_df = point_w_rebase,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_median_rebase"
  )

  # Probabilistic metrics (unscaled + rebased)
  res_prob <- inla_cov_crps_all_saved_rebase(
    models = names(formulas),
    masks = masks,
    folds = folds,
    data = data,
    out_dir = out_dir_rep,
    nsamp = 300,
    per = 1e5,
    seed = crps_seed_base + rep_i,
    group_cols = rebase_group_cols,
    target_col = rebase_target_col
  )

  normalise_join_keys <- function(df) {
    df %>%
      mutate(
        model = str_trim(as.character(model)),
        mask_type = str_trim(as.character(mask_type)),
        mask_type = recode(mask_type, future = "extrap_future", past = "extrap_past"),
        model = str_replace(model, "_extrap$", "")
      )
  }

  # ---- Overall consolidation ----
  tabs_mean_overall <- normalise_join_keys(tabs_mean$overall)
  tabs_median_overall <- normalise_join_keys(tabs_median$overall)
  tabs_mean_reb_overall <- normalise_join_keys(tabs_mean_rebase$overall)
  tabs_median_reb_overall <- normalise_join_keys(tabs_median_rebase$overall)
  res_overall <- normalise_join_keys(res_prob$overall)

  consolidated_overall <- tabs_mean_overall %>%
    select(model, mask_type, n_test, MAE_inc, RMSE_inc) %>%
    rename(
      n_test_mean = n_test,
      MAE_inc_mean = MAE_inc,
      RMSE_inc_mean = RMSE_inc
    ) %>%
    left_join(
      tabs_median_overall %>%
        select(model, mask_type, n_test, MAE_inc, RMSE_inc) %>%
        rename(
          n_test_median = n_test,
          MAE_inc_median = MAE_inc,
          RMSE_inc_median = RMSE_inc
        ),
      by = c("model", "mask_type")
    ) %>%
    left_join(
      tabs_mean_reb_overall %>%
        select(model, mask_type, MAE_inc, RMSE_inc) %>%
        rename(
          MAE_inc_mean_rebase = MAE_inc,
          RMSE_inc_mean_rebase = RMSE_inc
        ),
      by = c("model", "mask_type")
    ) %>%
    left_join(
      tabs_median_reb_overall %>%
        select(model, mask_type, MAE_inc, RMSE_inc) %>%
        rename(
          MAE_inc_median_rebase = MAE_inc,
          RMSE_inc_median_rebase = RMSE_inc
        ),
      by = c("model", "mask_type")
    ) %>%
    left_join(
      res_overall %>%
        select(model, mask_type, COV80, CRPS, COV80_rebase, CRPS_rebase),
      by = c("model", "mask_type")
    ) %>%
    mutate(rep = rep_i, mask_seed = seed)

  # ---- Fold consolidation ----
  tabs_mean_fold <- normalise_join_keys(tabs_mean$fold)
  tabs_median_fold <- normalise_join_keys(tabs_median$fold)
  tabs_mean_reb_fold <- normalise_join_keys(tabs_mean_rebase$fold)
  tabs_median_reb_fold <- normalise_join_keys(tabs_median_rebase$fold)
  res_fold <- normalise_join_keys(res_prob$fold)

  consolidated_fold <- tabs_mean_fold %>%
    select(model, mask_type, fold, n_test, MAE_inc, RMSE_inc) %>%
    rename(
      n_test_mean = n_test,
      MAE_inc_mean = MAE_inc,
      RMSE_inc_mean = RMSE_inc
    ) %>%
    left_join(
      tabs_median_fold %>%
        select(model, mask_type, fold, n_test, MAE_inc, RMSE_inc) %>%
        rename(
          n_test_median = n_test,
          MAE_inc_median = MAE_inc,
          RMSE_inc_median = RMSE_inc
        ),
      by = c("model", "mask_type", "fold")
    ) %>%
    left_join(
      tabs_mean_reb_fold %>%
        select(model, mask_type, fold, MAE_inc, RMSE_inc) %>%
        rename(
          MAE_inc_mean_rebase = MAE_inc,
          RMSE_inc_mean_rebase = RMSE_inc
        ),
      by = c("model", "mask_type", "fold")
    ) %>%
    left_join(
      tabs_median_reb_fold %>%
        select(model, mask_type, fold, MAE_inc, RMSE_inc) %>%
        rename(
          MAE_inc_median_rebase = MAE_inc,
          RMSE_inc_median_rebase = RMSE_inc
        ),
      by = c("model", "mask_type", "fold")
    ) %>%
    left_join(
      res_fold %>%
        select(model, mask_type, fold, COV80, CRPS, COV80_rebase, CRPS_rebase),
      by = c("model", "mask_type", "fold")
    ) %>%
    mutate(rep = rep_i, mask_seed = seed)

  write.csv(
    consolidated_overall,
    file.path(out_dir_rep, "inla_metrics_downscaling.csv"),
    row.names = FALSE
  )
  write.csv(
    consolidated_fold,
    file.path(out_dir_rep, "inla_metrics_downscaling_fold.csv"),
    row.names = FALSE
  )

  print(consolidated_overall)

  # ------------------------------------------------------------------------
  # Save predictions (per repetition)
  # ------------------------------------------------------------------------
  join_cols <- c("row_id", "adm_0_name", "time_seq", "pop_est", "dengue_total")

  preds_all <- point_w_rebase %>%
    left_join(
      data %>%
        select(row_id, pop_est, time_seq, dengue_total) %>%
        rename(truth = dengue_total),
      by = "row_id"
    ) %>%
    mutate(
      truth_inc = 1e5 * truth / pop_est,
      pred_inc_mean = 1e5 * pmax(pred_mean, 0) / pop_est,
      pred_inc_median = 1e5 * pmax(pred_median, 0) / pop_est,
      pred_inc_mean_rebase = 1e5 * pmax(pred_mean_rebase, 0) / pop_est,
      pred_inc_median_rebase = 1e5 * pmax(pred_median_rebase, 0) / pop_est
    ) %>%
    arrange(model, mask_type, fold, adm_0_name, time_seq) %>%
    mutate(rep = rep_i, mask_seed = seed)

  write.csv(
    preds_all,
    file.path(out_dir_rep, "inla_predictions_all_downscaling.csv"),
    row.names = FALSE
  )

  # Store for aggregation
  all_overall[[rep_tag]] <- consolidated_overall
  all_fold[[rep_tag]] <- consolidated_fold

  gc()
}

# ============================================================================
# Combine across repetitions
# ============================================================================

overall_allreps <- bind_rows(all_overall)
fold_allreps <- bind_rows(all_fold)

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(
  overall_allreps,
  file.path(base_dir, "inla_metrics_downscaling_repeatedCV_overall.csv"),
  row.names = FALSE
)

write.csv(
  fold_allreps,
  file.path(base_dir, "inla_metrics_downscaling_repeatedCV_fold.csv"),
  row.names = FALSE
)

# Optional summary across repetitions (mean + sd)
metric_cols <- c(
  "MAE_inc_mean", "RMSE_inc_mean", "MAE_inc_median", "RMSE_inc_median",
  "MAE_inc_mean_rebase", "RMSE_inc_mean_rebase", "MAE_inc_median_rebase", "RMSE_inc_median_rebase",
  "COV80", "CRPS", "COV80_rebase", "CRPS_rebase"
)
metric_cols <- intersect(metric_cols, names(overall_allreps))

summary_overall <- overall_allreps %>%
  group_by(model, mask_type) %>%
  summarise(
    across(all_of(metric_cols), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

write.csv(
  summary_overall,
  file.path(base_dir, "inla_metrics_downscaling_repeatedCV_summary.csv"),
  row.names = FALSE
)

print(summary_overall)
