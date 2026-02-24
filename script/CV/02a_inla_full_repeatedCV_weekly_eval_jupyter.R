source("script/CV/00_build_gap_mask_ext.R")
source("script/CV/00_build_gap_mask.R")
source("script/CV/00_imp_model_inla_spec.R")
source("script/CV/00_inla_eval_helpers.R")

library(future)
library(furrr)
library(stringr)


data <- read.csv("data/model_input/model_data_weekly_new.csv")
data <- data %>%
  mutate(
    region = factor(lat_band),
    regionx = as.integer(region),
    week52 = ((as.integer(week) - 1L) %% 52L) + 1L, # 1..52
    week_shared = week52, # global backbone index
    week_dev = week52, # region deviations use same index, replicated by region
    week_dev_country = week52 # country deviations use same index, replicated by country
  ) %>%
  dplyr::mutate(
    adm_0_name = factor(adm_0_name), # for bs="re" & fs
    Year = factor(Year), # re term should be a factor, not large numeric
    # month      = as.numeric(month) # cyclic spline wants numeric (1..12)
    week = as.numeric(week) # do this in weekly runs
  ) %>%
  arrange(adm_0_name, time_seq)

# Create stable row_id ONCE (must match what was used when CV fits were created)
if (!"row_id" %in% names(data)) {
  data <- data %>% dplyr::mutate(row_id = dplyr::row_number())
}


# =========================
# Repeated 3-fold CV settings
# =========================
base_dir <- "runs/CV/20260126"
n_folds <- 3
folds <- 1:n_folds

# Different seeds => different fold allocations inside the masks
mask_seeds <- c(123, 456, 789)

# Keep these separate from mask_seeds (controls INLA caching + posterior sampling seeds)
run_seed_base <- 20250811
crps_seed_base <- 123

# seed <- 123
# set.seed(seed)

# settings for parallel run
Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)

## (A) Cap BLAS threads globally (optional but recommended when parallelising)
Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1")

## (B) INLA threading (this option *is* supported)
INLA::inla.setOption(num.threads = "1:1", save.memory = FALSE)

workers <- max(1, floor(parallel::detectCores(logical = FALSE) / 2))
plan(multisession, workers = workers)


all_overall <- list()
all_fold <- list()

for (rep_i in seq_along(mask_seeds)) {
  # ---- repetition identifiers and directories ----
  seed <- mask_seeds[rep_i]
  set.seed(seed)

  rep_tag <- sprintf("rep%02d", rep_i)
  out_dir_rep <- file.path(base_dir, paste0("inla_full_CV_weekly_", rep_tag))
  mask_dir_rep <- file.path(base_dir, "masks", paste0("weekly_", rep_tag))
  dir.create(mask_dir_rep, recursive = TRUE, showWarnings = FALSE)

  cat("\n====================\n")
  cat("Running weekly CV:", rep_tag, "mask_seed =", seed, "\n")
  cat("====================\n")

  # ==========================================================
  # BUILD MASKS (weekly) — this is your existing mask logic,
  # but saving into mask_dir_rep so each repetition is separate
  # ==========================================================

  # ---- interpolation (balanced) ----
  mask_w_interp <- build_gap_mask_balanced(
    data        = data,
    country_col = "adm_0_name",
    time_col    = "time_seq",
    n_folds     = 3,
    gap_len     = 2,
    edge        = 80,
    seed        = seed
  )

  if (rep_i == 1) {
    fold_train_test_ratio(mask_w_interp, data, digits = 0)
    plot_fold_train_test(data, mask_w_interp, fold_id = 3)
  }

  mask_w_interp <- stamp_mask(
    mask_w_interp, data,
    name = "w_interp",
    params = list(cv_kind = "balanced", gap_len = 2, edge = 80, data_id = "weekly_full"),
    seed = seed
  )

  save_mask(
    mask_w_interp,
    dir = mask_dir_rep,
    filename = "inla_full_mask_w_interp.rds"
  )
  if (rep_i == 1) summarise_mask(mask_w_interp)

  # ---- extrapolation: future (rolling) ----
  mask_w_extrap_future <- build_extrapolation_mask_rolling(
    data,
    country_col = "adm_0_name",
    time_col = "time_seq",
    n_folds = 3,
    gap_len = 2,
    min_interior = 12,
    direction = "future",
    test_frac_target = 0.5,
    stride = 8,
    boundary_mode = "k",
    k_eval = 2,
    seed = seed
  )

  if (rep_i == 1) {
    fold_train_test_ratio(mask_w_extrap_future, data, digits = 0)
    plot_fold_train_test(
      data, mask_w_extrap_future,
      fold_id = 1,
      countries = unique(data$adm_0_name)[1:10]
    )
  }

  mask_w_extrap_future <- stamp_mask(
    mask_w_extrap_future, data,
    name = "w_extrap",
    params = list(
      cv_kind = "rolling", direction = "future",
      gap_len = 2, min_interior = 12, test_frac_target = 0.5,
      boundary_mode = "k", k_eval = 2, stride = 8, data_id = "weekly_full"
    ),
    seed = seed
  )

  save_mask(
    mask_w_extrap_future,
    dir = mask_dir_rep,
    filename = "inla_full_mask_w_extrap_future.rds"
  )
  if (rep_i == 1) summarise_mask(mask_w_extrap_future)

  # ---- extrapolation: past (rolling) ----
  mask_w_extrap_past <- build_extrapolation_mask_rolling(
    data,
    country_col = "adm_0_name",
    time_col = "time_seq",
    n_folds = 3,
    gap_len = 2,
    min_interior = 12,
    direction = "past",
    test_frac_target = 0.5,
    stride = 8,
    boundary_mode = "k",
    k_eval = 2,
    seed = seed
  )

  if (rep_i == 1) {
    fold_train_test_ratio(mask_w_extrap_past, data, digits = 0)
    plot_fold_train_test(data, mask_w_extrap_past, fold_id = 3)
  }

  mask_w_extrap_past <- stamp_mask(
    mask_w_extrap_past, data,
    name = "w_extrap",
    params = list(
      cv_kind = "rolling", direction = "past",
      gap_len = 2, min_interior = 12, test_frac_target = 0.5,
      boundary_mode = "k", k_eval = 2, stride = 8, data_id = "weekly_full"
    ),
    seed = seed
  )

  save_mask(
    mask_w_extrap_past,
    dir = mask_dir_rep,
    filename = "inla_full_mask_w_extrap_past.rds"
  )
  if (rep_i == 1) summarise_mask(mask_w_extrap_past)

  # ==========================================================
  # FINAL MODEL ONLY (weekly): formula + masks list
  # ==========================================================
  formulas <- list(inla_h_shared = inla_w_hier_shared_formula)

  masks <- list(
    interp = mask_w_interp,
    extrap_future = mask_w_extrap_future,
    extrap_past = mask_w_extrap_past
  )

  # ==========================================================
  # RUN INLA CV
  # ==========================================================
  # run_inla_parallel_jupyter(
  #   data = data, masks = masks, formulas = formulas, ctrl_fam = ctrl_fam,
  #   folds = folds,
  #   out_dir = out_dir_rep,
  #   workers = 10,
  #   nsamp = 300,
  #   seed = run_seed_base + rep_i
  # )

  # =========================================================
  # Quick INLA algorithm adequacy checks (CV fits)
  # - Numerical success: fit file exists + readable + finite hyperparameter summary
  # - Hyperparameter sanity: rho not stuck at +/-1; no NA/Inf in key summaries
  # =========================================================

  models_cv <- names(formulas)
  mask_types_cv <- names(masks)
  folds_cv <- folds

  jobs_cv <- expand.grid(
    model = models_cv,
    mask_type = mask_types_cv,
    fold = folds_cv,
    stringsAsFactors = FALSE
  )

  # check_one_fit <- function(model, mask_type, fold, out_dir) {
  #   fp <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold))
  #
  #   exists <- file.exists(fp)
  #   if (!exists) {
  #     return(data.frame(
  #       model = model, mask_type = mask_type, fold = fold,
  #       fit_path = fp,
  #       exists = FALSE, ok_read = FALSE,
  #       has_hyperpar = FALSE, hyperpar_all_finite = FALSE,
  #       rho_boundary_flag = NA, rho_means = NA_character_,
  #       note = "missing_fit_file",
  #       ok = FALSE,
  #       stringsAsFactors = FALSE
  #     ))
  #   }
  #
  #   fit <- tryCatch(readRDS(fp), error = function(e) e)
  #   if (inherits(fit, "error")) {
  #     return(data.frame(
  #       model = model, mask_type = mask_type, fold = fold,
  #       fit_path = fp,
  #       exists = TRUE, ok_read = FALSE,
  #       has_hyperpar = FALSE, hyperpar_all_finite = FALSE,
  #       rho_boundary_flag = NA, rho_means = NA_character_,
  #       note = paste0("read_error: ", conditionMessage(fit)),
  #       ok = FALSE,
  #       stringsAsFactors = FALSE
  #     ))
  #   }
  #
  #   shp <- fit$summary.hyperpar
  #   has_hyperpar <- !is.null(shp) && nrow(shp) > 0
  #
  #   hyperpar_all_finite <- FALSE
  #   rho_boundary_flag <- NA
  #   rho_means <- NA_character_
  #   note <- NA_character_
  #
  #   if (has_hyperpar) {
  #     cols <- intersect(c("mean", "sd", "0.025quant", "0.5quant", "0.975quant"), colnames(shp))
  #     if (length(cols) == 0) {
  #       note <- "summary.hyperpar_has_no_standard_cols"
  #     } else {
  #       vals <- as.matrix(shp[, cols, drop = FALSE])
  #       hyperpar_all_finite <- all(is.finite(vals))
  #
  #       rho_rows <- grep("rho", rownames(shp), ignore.case = TRUE)
  #       if (length(rho_rows) > 0 && "mean" %in% colnames(shp)) {
  #         rhos <- as.numeric(shp[rho_rows, "mean"])
  #         rho_means <- paste(round(rhos, 3), collapse = "; ")
  #         rho_boundary_flag <- any(abs(rhos) > 0.999, na.rm = TRUE)
  #       }
  #     }
  #   } else {
  #     note <- "missing_summary.hyperpar"
  #   }
  #
  #   ok <- isTRUE(has_hyperpar) && isTRUE(hyperpar_all_finite)
  #
  #   data.frame(
  #     model = model, mask_type = mask_type, fold = fold,
  #     fit_path = fp,
  #     exists = TRUE, ok_read = TRUE,
  #     has_hyperpar = has_hyperpar,
  #     hyperpar_all_finite = hyperpar_all_finite,
  #     rho_boundary_flag = rho_boundary_flag,
  #     rho_means = rho_means,
  #     note = note,
  #     ok = ok,
  #     stringsAsFactors = FALSE
  #   )
  # }
  #
  # diag_list <- Map(
  #   f = check_one_fit,
  #   model = jobs_cv$model,
  #   mask_type = jobs_cv$mask_type,
  #   fold = jobs_cv$fold,
  #   MoreArgs = list(out_dir = out_dir_rep)
  # )
  # diag_df <- do.call(rbind, diag_list) %>%
  #   dplyr::mutate(rep = rep_i, mask_seed = seed)
  #
  # diag_path <- file.path(out_dir_rep, "inla_algorithm_adequacy_weekly.csv")
  # write.csv(diag_df, diag_path, row.names = FALSE)
  #
  # n_total <- nrow(diag_df)
  # n_ok <- sum(diag_df$ok, na.rm = TRUE)
  # n_missing <- sum(!diag_df$exists, na.rm = TRUE)
  # n_rho_boundary <- sum(diag_df$rho_boundary_flag %in% TRUE, na.rm = TRUE)
  #
  # cat(sprintf(
  #   "\nINLA adequacy check (%s): ok=%d/%d | missing=%d | rho_boundary_flags=%d\nSaved: %s\n\n",
  #   rep_tag, n_ok, n_total, n_missing, n_rho_boundary, diag_path
  # ))

  # ==========================================================
  # build model performance tables (per repetition)
  # ==========================================================
  check_inla_data_fit_alignment(data, out_dir_rep, n_spotcheck = 5)

  arts <- read_inla_artifacts(out_dir = out_dir_rep)

  # now recompute tabs
  tabs_mean <- make_metrics_counts_incidence_prob(
    res_point_df = arts$point,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_mean"
  )

  tabs_median <- make_metrics_counts_incidence_prob(
    res_point_df = arts$point,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_median"
  )

  # --- probabilistic metrics (unchanged) ---
  res_filtered <- inla_cov_crps_all_saved(
    models = names(formulas),
    masks = masks,
    folds = folds,
    data = data,
    out_dir = out_dir_rep,
    nsamp = 300,
    per = 1e5,
    seed = crps_seed_base + rep_i
  )

  normalise_join_keys <- function(df) {
    df %>%
      mutate(
        model = str_trim(as.character(model)),
        mask_type = str_trim(as.character(mask_type)),

        # harmonise legacy mask labels
        mask_type = recode(mask_type,
          future = "extrap_future",
          past   = "extrap_past"
        ),

        # harmonise legacy model labels
        model = str_replace(model, "_extrap$", "")
      )
  }
  tabs_mean_overall <- normalise_join_keys(tabs_mean$overall)
  tabs_median_overall <- normalise_join_keys(tabs_median$overall)
  res_overall <- normalise_join_keys(res_filtered$overall)

  consolidated_overall <- tabs_mean_overall %>%
    select(model, mask_type, n_test, MAE_inc, RMSE_inc) %>%
    rename(MAE_inc_mean = MAE_inc, RMSE_inc_mean = RMSE_inc, n_test_mean = n_test) %>%
    left_join(
      tabs_median_overall %>%
        select(model, mask_type, n_test, MAE_inc, RMSE_inc) %>%
        rename(MAE_inc_median = MAE_inc, RMSE_inc_median = RMSE_inc, n_test_median = n_test),
      by = c("model", "mask_type")
    ) %>%
    left_join(
      res_overall %>% select(model, mask_type, COV80, CRPS),
      by = c("model", "mask_type")
    ) %>%
    mutate(rep = rep_i, mask_seed = seed)

  tabs_mean_fold <- normalise_join_keys(tabs_mean$fold)
  tabs_median_fold <- normalise_join_keys(tabs_median$fold)
  res_fold <- normalise_join_keys(res_filtered$fold)

  # Consolidate fold-level metrics
  consolidated_fold <- tabs_mean_fold %>%
    select(model, mask_type, fold, n_test, MAE_inc, RMSE_inc) %>%
    rename(MAE_inc_mean = MAE_inc, RMSE_inc_mean = RMSE_inc, n_test_mean = n_test) %>%
    left_join(
      tabs_median_fold %>%
        select(model, mask_type, fold, n_test, MAE_inc, RMSE_inc) %>%
        rename(MAE_inc_median = MAE_inc, RMSE_inc_median = RMSE_inc, n_test_median = n_test),
      by = c("model", "mask_type", "fold")
    ) %>%
    left_join(
      res_fold %>% select(model, mask_type, fold, COV80, CRPS),
      by = c("model", "mask_type", "fold")
    ) %>%
    mutate(rep = rep_i, mask_seed = seed)

  write.csv(consolidated_overall,
    file.path(out_dir_rep, "inla_metrics_weekly.csv"),
    row.names = FALSE
  )

  write.csv(consolidated_fold,
    file.path(out_dir_rep, "inla_metrics_weekly_fold.csv"),
    row.names = FALSE
  )

  # ==========================================================
  # save model predictions (per repetition)
  # ==========================================================

  predictions_all <- arts$point %>%
    dplyr::left_join(
      data %>%
        dplyr::select(
          row_id, adm_0_name, time_seq, Year, week,
          truth = dengue_total, pop_est
        ),
      by = "row_id"
    ) %>%
    dplyr::mutate(
      truth_inc = 1e5 * truth / pop_est,
      pred_inc_mean = 1e5 * pmax(pred_mean, 0) / pop_est,
      pred_inc_median = 1e5 * pmax(pred_median, 0) / pop_est

      # pred_inc = pred_inc_median # keep pred/pred_inc for backwards compatibility
    ) %>%
    dplyr::select(
      model, mask_type, fold,
      adm_0_name, Year, week, time_seq,
      truth, pop_est,
      pred_mean, pred_median,
      truth_inc, pred_inc_mean, pred_inc_median
    ) %>%
    dplyr::arrange(model, mask_type, fold, adm_0_name, time_seq) %>%
    dplyr::mutate(rep = rep_i, mask_seed = seed)

  write.csv(
    predictions_all,
    file.path(out_dir_rep, "inla_predictions_all_weekly.csv"),
    row.names = FALSE
  )


  # store for across-repetition aggregation
  all_overall[[rep_tag]] <- consolidated_overall
  all_fold[[rep_tag]] <- consolidated_fold

  gc()
} # end for (rep_i ...)


# ==========================================================
# Aggregate across repetitions (overall + fold-level)
# ==========================================================
overall_allreps <- dplyr::bind_rows(all_overall)
fold_allreps <- dplyr::bind_rows(all_fold)

write.csv(
  overall_allreps,
  file.path(base_dir, "inla_metrics_weekly_repeatedCV_overall.csv"),
  row.names = FALSE
)
write.csv(
  fold_allreps,
  file.path(base_dir, "inla_metrics_weekly_repeatedCV_fold.csv"),
  row.names = FALSE
)

# Optional: summary across repetitions (mean + sd)
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


write.csv(
  summary_overall,
  file.path(base_dir, "inla_metrics_weekly_repeatedCV_summary.csv"),
  row.names = FALSE
)

print(summary_overall)
