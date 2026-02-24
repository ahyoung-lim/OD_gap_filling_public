# formatting_table_normalised.R
#
# Same structure as formatting_table.R but adds:
#   - obs_mean_inc  : weighted observed mean incidence per mask_type x Model
#   - nMAE_median   : normalised MAE  (MAE  / observed mean) for the median predictor
#   - nRMSE_median  : normalised RMSE (RMSE / observed mean) for the median predictor
#   - nMAE_mean     : normalised MAE  (MAE  / observed mean) for the mean predictor  (disaggregation only)
#   - nRMSE_mean    : normalised RMSE (RMSE / observed mean) for the mean predictor  (disaggregation only)
#
# Observed incidence is recovered from existing prediction / row-index files
# so NO model re-runs are needed.

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# ============================================================
# Shared helpers
# ============================================================

wmean_na <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) NA_real_ else stats::weighted.mean(x[ok], w[ok])
}

normalise_keys <- function(df) {
  df %>%
    mutate(
      model     = str_trim(as.character(model)),
      mask_type = str_trim(as.character(mask_type)),
      mask_type = recode(mask_type, future = "extrap_future", past = "extrap_past"),
      model     = str_replace(model, "_extrap$", "")
    )
}

parse_numeric_cols <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ suppressWarnings(as.numeric(.))))
}

check_required_numeric <- function(df, cols, label) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) stop(label, ": missing columns: ", paste(miss, collapse = ", "))

  counts <- df %>%
    summarise(across(all_of(cols), ~ sum(is.finite(.)), .names = "{.col}")) %>%
    pivot_longer(everything(), names_to = "col", values_to = "n_finite")

  bad <- counts %>% filter(n_finite == 0)
  if (nrow(bad) > 0) {
    print(counts)
    stop(
      label, ": these columns have zero finite numeric values: ",
      paste(bad$col, collapse = ", ")
    )
  }
  invisible(TRUE)
}


# ============================================================
# PART 1: IMPUTATION TABLE  (weekly / monthly GAM vs INLA)
# ============================================================

# --- readers (unchanged from formatting_table.R) ---

read_overall <- function(run_dir, which = c("inla", "gam"), resolution = c("weekly", "monthly")) {
  which <- match.arg(which)
  resolution <- match.arg(resolution)

  pat <- if (which == "inla") {
    sprintf("^inla_metrics_%s_repeatedCV_fold.*\\.csv$", resolution)
  } else {
    sprintf("^gam_metrics_%s_repeatedCV_fold.*\\.csv$", resolution)
  }

  fp <- list.files(run_dir, pattern = pat, full.names = TRUE)[1]
  stopifnot(!is.na(fp), file.exists(fp))

  df <- read_csv(fp, show_col_types = FALSE) %>% normalise_keys()

  if (!"COV80" %in% names(df) && "cov80" %in% names(df)) df <- rename(df, COV80 = cov80)
  if (!"CRPS" %in% names(df) && "crps" %in% names(df)) df <- rename(df, CRPS = crps)

  num_cols <- c(
    "n_test_mean", "n_test_median",
    "MAE_inc_mean", "RMSE_inc_mean",
    "MAE_inc_median", "RMSE_inc_median",
    "COV80", "CRPS"
  )
  miss <- setdiff(num_cols, names(df))
  stopifnot(length(miss) == 0)

  df %>%
    mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.))))
}

check_n_test_mismatch <- function(inla_df, gam_df) {
  inla_n <- inla_df %>%
    group_by(mask_type) %>%
    summarise(n_test_inla = max(pmax(n_test_mean, n_test_median, na.rm = TRUE), na.rm = TRUE), .groups = "drop")

  gam_n <- gam_df %>%
    group_by(mask_type) %>%
    summarise(n_test_gam = max(pmax(n_test_mean, n_test_median, na.rm = TRUE), na.rm = TRUE), .groups = "drop")

  compare <- full_join(inla_n, gam_n, by = "mask_type")

  mismatches <- compare %>%
    filter(!is.na(n_test_inla) & !is.na(n_test_gam) & n_test_inla != n_test_gam)

  if (nrow(mismatches) > 0) {
    for (i in seq_len(nrow(mismatches))) {
      warning(sprintf(
        "n_test mismatch for mask_type '%s': INLA=%d, GAM=%d",
        mismatches$mask_type[i],
        as.integer(mismatches$n_test_inla[i]),
        as.integer(mismatches$n_test_gam[i])
      ), call. = FALSE)
    }
  }

  invisible(NULL)
}

# --- Compute observed mean incidence from INLA prediction files ---
# INLA and GAM share test rows within a resolution, so INLA predictions
# serve as the single source of truth for both models.

get_obs_mean_imputation <- function(run_dir, resolution = c("weekly", "monthly")) {
  resolution <- match.arg(resolution)
  reps <- 1:3

  obs_rows <- list()
  for (rep_i in reps) {
    rep_tag <- sprintf("rep%02d", rep_i)
    pred_dir <- file.path(run_dir, paste0("inla_full_CV_", resolution, "_", rep_tag))
    pred_file <- file.path(pred_dir, paste0("inla_predictions_all_", resolution, ".csv"))

    if (!file.exists(pred_file)) {
      warning("Missing: ", pred_file)
      next
    }

    preds <- read_csv(pred_file, show_col_types = FALSE) %>%
      mutate(
        mask_type = str_trim(as.character(mask_type)),
        mask_type = recode(mask_type, future = "extrap_future", past = "extrap_past"),
        fold = as.integer(fold)
      )

    # Compute observed mean incidence per mask_type × fold
    obs_fold <- preds %>%
      group_by(mask_type, fold) %>%
      summarise(
        obs_mean_inc = mean(truth_inc, na.rm = TRUE),
        n_obs = sum(!is.na(truth_inc)),
        .groups = "drop"
      ) %>%
      mutate(rep = rep_i)

    obs_rows[[length(obs_rows) + 1]] <- obs_fold
  }

  bind_rows(obs_rows)
}

# --- Sanity check: prediction rows match metrics n_test (imputation) ---

sanity_check_obs_vs_metrics_imputation <- function(obs_df, run_dir, resolution) {
  # Read INLA metrics fold file (one row per fold × rep)
  pat <- sprintf("^inla_metrics_%s_repeatedCV_fold.*\\.csv$", resolution)
  fp <- list.files(run_dir, pattern = pat, full.names = TRUE)[1]
  if (is.na(fp) || !file.exists(fp)) {
    warning("Cannot sanity-check: INLA metrics fold file not found for ", resolution)
    return(invisible(NULL))
  }

  metrics <- read_csv(fp, show_col_types = FALSE) %>%
    normalise_keys() %>%
    mutate(
      rep = as.integer(rep),
      fold = as.integer(fold),
      n_test_metrics = suppressWarnings(as.numeric(n_test_mean))
    ) %>%
    select(mask_type, rep, fold, n_test_metrics)

  check <- obs_df %>%
    select(mask_type, rep, fold, n_obs) %>%
    full_join(metrics, by = c("mask_type", "rep", "fold")) %>%
    mutate(match = n_obs == n_test_metrics)

  n_mismatch <- sum(!check$match | is.na(check$match), na.rm = TRUE)

  if (n_mismatch == 0) {
    cat(sprintf(
      "  ✓ Sanity check PASSED (%s): n_obs from predictions == n_test from metrics for all %d fold×rep combos\n",
      resolution, nrow(check)
    ))
  } else {
    cat(sprintf(
      "  ⚠ Sanity check WARNING (%s): %d / %d fold×rep combos have MISMATCHED n_test:\n",
      resolution, n_mismatch, nrow(check)
    ))
    print(check %>% filter(!match | is.na(match)))
  }

  invisible(check)
}


# --- Build imputation table with nMAE ---

build_metrics_table_normalised <- function(run_dir, resolution = c("weekly", "monthly")) {
  resolution <- match.arg(resolution)

  inla_df <- read_overall(run_dir, "inla", resolution) %>% mutate(Model = "INLA")
  gam_df <- read_overall(run_dir, "gam", resolution) %>% mutate(Model = "GAM")

  check_n_test_mismatch(inla_df, gam_df)

  df <- bind_rows(inla_df, gam_df) %>%
    mutate(
      w_mean   = n_test_mean,
      w_median = n_test_median,
      n_test   = pmax(n_test_mean, n_test_median, na.rm = TRUE)
    )

  # --- observed mean (same for GAM and INLA within a resolution) ---
  obs <- get_obs_mean_imputation(run_dir, resolution)
  sanity_check_obs_vs_metrics_imputation(obs, run_dir, resolution)

  # Weighted obs_mean across folds within a rep, then across reps
  # (weighted by n_obs to match how original script weights metrics by n_test)
  obs_by_mask <- obs %>%
    group_by(mask_type, rep) %>%
    summarise(
      obs_mean_inc = wmean_na(obs_mean_inc, n_obs),
      n_obs_rep = sum(n_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(mask_type) %>%
    summarise(obs_mean_inc = wmean_na(obs_mean_inc, n_obs_rep), .groups = "drop")

  # --- aggregate metrics (same logic as original) ---
  out <- df %>%
    group_by(mask_type, Model) %>%
    summarise(
      n_test = as.integer(round(sum(n_test, na.rm = TRUE))),
      MAE_median = wmean_na(MAE_inc_median, w_median),
      RMSE_median = wmean_na(RMSE_inc_median, w_median),
      cov80 = wmean_na(COV80, w_mean),
      crps = wmean_na(CRPS, w_mean),
      .groups = "drop"
    ) %>%
    left_join(obs_by_mask, by = "mask_type") %>%
    mutate(
      nMAE_median  = MAE_median / obs_mean_inc,
      nRMSE_median = RMSE_median / obs_mean_inc
    ) %>%
    mutate(
      MAE_median   = sprintf("%.1f", round(MAE_median, 1)),
      RMSE_median  = sprintf("%.1f", round(RMSE_median, 1)),
      cov80        = sprintf("%.2f", round(cov80, 2)),
      crps         = sprintf("%.1f", round(crps, 1)),
      obs_mean_inc = sprintf("%.1f", round(as.numeric(obs_mean_inc), 1)),
      nMAE_median  = sprintf("%.2f", round(as.numeric(nMAE_median), 2)),
      nRMSE_median = sprintf("%.2f", round(as.numeric(nRMSE_median), 2))
    ) %>%
    arrange(
      factor(mask_type, levels = c("interp", "extrap_past", "extrap_future")),
      Model
    )

  out
}

write_table_outputs <- function(tbl, run_dir, stem) {
  csv <- file.path(run_dir, paste0(stem, ".csv"))
  tsv <- file.path(run_dir, paste0(stem, ".tsv"))
  write_csv(tbl, csv, na = "")
  write_tsv(tbl, tsv, na = "")
  list(csv = csv, tsv = tsv)
}




# ============================================================
# PART 2: DISAGGREGATION TABLE  (DM vs INLA downscaling)
# ============================================================

# --- readers (unchanged from formatting_table.R) ---

read_dm_fold <- function(run_dir) {
  fp <- file.path(run_dir, "pymc_metrics_monthly_repeatedCV_fold.csv")
  stopifnot(file.exists(fp))
  df <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      mask_type = str_trim(as.character(mask_type)),
      rep = as.integer(rep),
      fold = as.integer(fold)
    )

  num_cols <- c(
    "n_test_mean", "n_test_median", "n_test_used",
    "MAE_inc_mean", "RMSE_inc_mean", "MAE_inc_median", "RMSE_inc_median",
    "COV80", "CRPS"
  )
  df <- parse_numeric_cols(df, intersect(num_cols, names(df)))
  check_required_numeric(
    df, c("n_test_mean", "n_test_median", "MAE_inc_mean", "RMSE_inc_mean", "MAE_inc_median", "RMSE_inc_median", "COV80", "CRPS"),
    "DM fold file"
  )
  df
}

read_inla_fold <- function(run_dir) {
  fp <- file.path(run_dir, "inla_metrics_downscaling_dmrows_repeatedCV_fold.csv")
  stopifnot(file.exists(fp))
  df <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      mask_type = str_trim(as.character(mask_type)),
      rep = as.integer(rep),
      fold = as.integer(fold)
    )

  num_cols <- c(
    "n_test_mean", "n_test_median",
    "MAE_inc_mean", "RMSE_inc_mean", "MAE_inc_median", "RMSE_inc_median",
    "MAE_inc_mean_rebase", "RMSE_inc_mean_rebase", "MAE_inc_median_rebase", "RMSE_inc_median_rebase",
    "COV80", "CRPS", "COV80_rebase", "CRPS_rebase"
  )
  df <- parse_numeric_cols(df, intersect(num_cols, names(df)))
  check_required_numeric(
    df, c(
      "n_test_mean", "n_test_median",
      "MAE_inc_mean", "RMSE_inc_mean", "MAE_inc_median", "RMSE_inc_median",
      "MAE_inc_mean_rebase", "RMSE_inc_mean_rebase", "MAE_inc_median_rebase", "RMSE_inc_median_rebase",
      "COV80", "CRPS", "COV80_rebase", "CRPS_rebase"
    ),
    "INLA fold file"
  )
  df
}

sanity_check_ntest <- function(dm, inla) {
  dm_key <- dm %>%
    group_by(mask_type, rep, fold) %>%
    summarise(n_dm = max(n_test_mean, na.rm = TRUE), .groups = "drop")
  inla_key <- inla %>%
    group_by(mask_type, rep, fold) %>%
    summarise(n_inla = max(n_test_mean, na.rm = TRUE), .groups = "drop")

  bad <- full_join(dm_key, inla_key, by = c("mask_type", "rep", "fold")) %>%
    filter(is.na(n_dm) | is.na(n_inla) | n_dm != n_inla)

  if (nrow(bad) > 0) {
    print(bad)
    stop("Sanity check failed: n_test_mean differs between DM and INLA for some mask_type/rep/fold.")
  }
  invisible(TRUE)
}

# --- Compute observed mean incidence from dm_eval_rowids_from_masks.csv ---

get_obs_mean_disaggregation <- function(run_dir) {
  fp <- file.path(run_dir, "dm_eval_rowids_from_masks.csv")
  stopifnot(file.exists(fp))

  dm_idx <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      mask_type = str_trim(as.character(mask_type)),
      rep = as.integer(rep),
      fold = as.integer(fold),
      truth_inc = dengue_total / pop_est * 1e5
    )

  dm_idx %>%
    group_by(mask_type, rep, fold) %>%
    summarise(
      obs_mean_inc = mean(truth_inc, na.rm = TRUE),
      n_obs = sum(!is.na(truth_inc)),
      .groups = "drop"
    )
}

# --- Sanity check: dm_eval_rowids n_obs matches DM metrics n_test ---

sanity_check_obs_vs_metrics_disaggregation <- function(obs_df, run_dir) {
  fp <- file.path(run_dir, "pymc_metrics_monthly_repeatedCV_fold.csv")
  if (!file.exists(fp)) {
    warning("Cannot sanity-check: DM metrics fold file not found")
    return(invisible(NULL))
  }

  dm_metrics <- read_csv(fp, show_col_types = FALSE) %>%
    mutate(
      mask_type = str_trim(as.character(mask_type)),
      rep = as.integer(rep),
      fold = as.integer(fold),
      n_test_metrics = suppressWarnings(as.numeric(n_test))
    ) %>%
    select(mask_type, rep, fold, n_test_metrics) %>%
    distinct()

  check <- obs_df %>%
    select(mask_type, rep, fold, n_obs) %>%
    full_join(dm_metrics, by = c("mask_type", "rep", "fold")) %>%
    mutate(match = n_obs == n_test_metrics)

  n_mismatch <- sum(!check$match | is.na(check$match), na.rm = TRUE)

  if (n_mismatch == 0) {
    cat(sprintf(
      "  ✓ Sanity check PASSED (disaggregation): n_obs from dm_eval_rowids == n_test from DM metrics for all %d fold×rep combos\n",
      nrow(check)
    ))
  } else {
    cat(sprintf(
      "  ⚠ Sanity check WARNING (disaggregation): %d / %d fold×rep combos have MISMATCHED n_test:\n",
      n_mismatch, nrow(check)
    ))
    print(check %>% filter(!match | is.na(match)))
  }

  invisible(check)
}


# --- fold → rep aggregation (extended with obs_mean) ---

fold_to_rep <- function(df, model_label,
                        mae_mean_col, rmse_mean_col,
                        mae_med_col, rmse_med_col,
                        cov_col, crps_col,
                        use_n_test_used_for_prob = FALSE) {
  df <- df %>%
    mutate(
      w_mean   = n_test_mean,
      w_median = n_test_median,
      w_prob   = if (use_n_test_used_for_prob && "n_test_used" %in% names(df)) n_test_used else n_test_mean
    )

  df %>%
    group_by(mask_type, rep) %>%
    summarise(
      Model = model_label,
      n_test = sum(w_mean, na.rm = TRUE),
      MAE_mean = wmean_na(.data[[mae_mean_col]], w_mean),
      RMSE_mean = wmean_na(.data[[rmse_mean_col]], w_mean),
      MAE_median = wmean_na(.data[[mae_med_col]], w_median),
      RMSE_median = wmean_na(.data[[rmse_med_col]], w_median),
      cov80 = wmean_na(.data[[cov_col]], w_prob),
      crps = wmean_na(.data[[crps_col]], w_prob),
      .groups = "drop"
    )
}

rep_to_final <- function(rep_tbl) {
  rep_tbl %>%
    group_by(mask_type, Model) %>%
    summarise(
      MAE_mean = wmean_na(MAE_mean, n_test),
      RMSE_mean = wmean_na(RMSE_mean, n_test),
      MAE_median = wmean_na(MAE_median, n_test),
      RMSE_median = wmean_na(RMSE_median, n_test),
      cov80 = wmean_na(cov80, n_test),
      crps = wmean_na(crps, n_test),
      n_test = as.integer(round(max(n_test, na.rm = TRUE))),
      .groups = "drop"
    )
}


build_dm_inla_table_normalised <- function(run_dir = file.path("runs", "CV", "20260126")) {
  dm <- read_dm_fold(run_dir)
  inla <- read_inla_fold(run_dir)
  sanity_check_ntest(dm, inla)

  # --- observed mean (shared test rows for DM and INLA dmrows) ---
  obs_fold <- get_obs_mean_disaggregation(run_dir)
  sanity_check_obs_vs_metrics_disaggregation(obs_fold, run_dir)

  # Weighted obs_mean: fold → rep → overall  (same aggregation as metrics)
  obs_by_mask <- obs_fold %>%
    group_by(mask_type, rep) %>%
    summarise(
      obs_mean_inc = wmean_na(obs_mean_inc, n_obs),
      n_obs_rep = sum(n_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(mask_type) %>%
    summarise(obs_mean_inc = wmean_na(obs_mean_inc, n_obs_rep), .groups = "drop")

  # --- metrics aggregation (same as original) ---
  dm_rep <- fold_to_rep(dm, "DM",
    "MAE_inc_mean", "RMSE_inc_mean",
    "MAE_inc_median", "RMSE_inc_median",
    "COV80", "CRPS",
    use_n_test_used_for_prob = TRUE
  )

  inla_unscaled <- fold_to_rep(
    inla, "INLA unscaled",
    "MAE_inc_mean", "RMSE_inc_mean",
    "MAE_inc_median", "RMSE_inc_median",
    "COV80", "CRPS"
  )

  inla_scaled <- fold_to_rep(
    inla, "INLA scaled",
    "MAE_inc_mean_rebase", "RMSE_inc_mean_rebase",
    "MAE_inc_median_rebase", "RMSE_inc_median_rebase",
    "COV80_rebase", "CRPS_rebase"
  )

  tbl <- bind_rows(dm_rep, inla_unscaled, inla_scaled) %>%
    rep_to_final() %>%
    left_join(obs_by_mask, by = "mask_type") %>%
    mutate(
      # nMAE_mean    = MAE_mean / obs_mean_inc,
      nMAE_median  = MAE_median / obs_mean_inc,
      # nRMSE_mean   = RMSE_mean / obs_mean_inc,
      nRMSE_median = RMSE_median / obs_mean_inc
    ) %>%
    mutate(
      # MAE_mean     = round(MAE_mean, 1),
      # RMSE_mean    = round(RMSE_mean, 1),
      MAE_median   = round(MAE_median, 1),
      RMSE_median  = round(RMSE_median, 1),
      cov80        = round(cov80, 2),
      crps         = round(crps, 1),
      obs_mean_inc = round(obs_mean_inc, 1),
      # nMAE_mean    = round(nMAE_mean, 2),
      nMAE_median  = round(nMAE_median, 2),
      # nRMSE_mean   = round(nRMSE_mean, 2),
      nRMSE_median = round(nRMSE_median, 2)
    ) %>%
    arrange(
      factor(mask_type, levels = c("interp", "extrap_past", "extrap_future")),
      factor(Model, levels = c("DM", "INLA unscaled", "INLA scaled"))
    )

  tbl
}


# ============================================================
# Run
# ============================================================

run_dir <- file.path("runs", "CV", "20260126")

# --- Imputation tables ---
cat("\n=== Weekly Metrics (normalised) ===\n")
tbl_weekly <- build_metrics_table_normalised(run_dir, resolution = "weekly")
print(tbl_weekly)
paths_weekly <- write_table_outputs(tbl_weekly, run_dir, stem = "metrics_table_weekly_normalised")

cat("\n=== Monthly Metrics (normalised) ===\n")
tbl_monthly <- build_metrics_table_normalised(run_dir, resolution = "monthly")
print(tbl_monthly)
paths_monthly <- write_table_outputs(tbl_monthly, run_dir, stem = "metrics_table_monthly_normalised")

# --- Disaggregation table ---
cat("\n=== Disaggregation Metrics (normalised) ===\n")
tbl_dm <- build_dm_inla_table_normalised(run_dir)
print(tbl_dm)
paths_dm <- write_table_outputs(tbl_dm, run_dir, stem = "metrics_table_dm_inla_normalised")
