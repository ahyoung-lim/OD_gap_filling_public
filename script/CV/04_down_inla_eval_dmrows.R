# down_inla_eval_dmrows.R
#
# Evaluate INLA CV fits restricted to the same rows used by the DM model.
# This enables fair comparison: INLA metrics computed on DM's test set.
#
# Outputs (per-rep in out_dir_rep):
#   - inla_metrics_downscaling_dmrows.csv
#   - inla_metrics_downscaling_dmrows_fold.csv
#
# Outputs (combined in base_dir):
#   - dm_eval_rowids_from_masks.csv           (row index for DM test sets)
#   - inla_metrics_downscaling_dmrows_repeatedCV_overall.csv
#   - inla_metrics_downscaling_dmrows_repeatedCV_fold.csv
#   - inla_metrics_downscaling_dmrows_repeatedCV_summary.csv

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

source("script/CV/00_inla_eval_helpers_new.R")

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------

BASE_DIR <- "runs/CV/20260126"
MIN_TOTAL_TEST <- 1L # match DM
N_FOLDS <- 3
FOLDS <- 1:N_FOLDS
SEEDS <- c(123, 456, 789)

rebase_group_cols <- c("adm_0_name", "Year")
rebase_target_col <- "annual_total"
crps_seed_base <- 123

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

# -----------------------------------------------------------------------------
# Helper: infer mask_type from filename
# -----------------------------------------------------------------------------

infer_mask_type <- function(path) {
  nm <- tolower(basename(path))
  if (grepl("interp", nm)) {
    return("interp")
  }
  if (grepl("extrap[_-]?past|\\bpast\\b", nm)) {
    return("extrap_past")
  }
  if (grepl("extrap[_-]?future|\\bfuture\\b", nm)) {
    return("extrap_future")
  }
  NA_character_
}


# -----------------------------------------------------------------------------
# Build DM test row_id list for one mask + fold
# (matches Python's build_dm_arrays_from_mask logic)
# -----------------------------------------------------------------------------

dm_test_rowids_from_mask <- function(mask_path, fold_id, min_total_test = 1L) {
  m <- readRDS(mask_path)

  m <- m %>%
    mutate(
      month = as.integer(month),
      Year_chr = as.character(Year),
      adm_chr = as.character(adm_0_name)
    )

  fold_num <- suppressWarnings(as.numeric(m$fold))
  m$is_test <- !is.na(fold_num) & (fold_num == fold_id)

  # keep only complete 12-month years (as in DM logic)
  yr <- m %>%
    group_by(adm_chr, Year_chr) %>%
    summarise(
      n_months = n_distinct(month),
      any_test = any(is_test),
      annual_total = dplyr::first(annual_total),
      .groups = "drop"
    ) %>%
    filter(n_months == 12)

  test_pairs <- yr %>%
    filter(any_test, annual_total >= min_total_test) %>%
    select(adm_chr, Year_chr)

  # Month-cells actually scoreable: pop must be present and >= 1
  test_cells <- m %>%
    semi_join(test_pairs, by = c("adm_chr", "Year_chr")) %>%
    group_by(adm_chr, Year_chr, month) %>%
    summarise(pop_est = dplyr::first(pop_est), .groups = "drop") %>%
    filter(is.finite(pop_est), pop_est >= 1, month %in% 1:12)

  # Join to INLA-prepped data to recover row_id
  idx <- data %>%
    mutate(
      adm_chr = as.character(adm_0_name),
      Year_chr = as.character(Year)
    ) %>%
    semi_join(test_cells, by = c("adm_chr", "Year_chr", "month")) %>%
    select(row_id, adm_0_name, Year, month, dengue_total, pop_est)

  idx
}


# -----------------------------------------------------------------------------
# Step 1: Build DM row index across all reps/masks/folds
# -----------------------------------------------------------------------------

cat("Building DM row index from masks...\n")
rows <- list()

for (rep_i in 1:3) {
  rep_tag <- sprintf("rep%02d", rep_i)
  mask_dir <- file.path(BASE_DIR, "masks", paste0("downscaling_", rep_tag))

  mask_files <- list.files(
    mask_dir,
    pattern = "^inla_down_mask_(interp|extrap_past|extrap_future).*\\.rds$",
    full.names = TRUE
  )

  # keep only one file per mask_type (prefer the plain name if both exist)
  mask_files <- mask_files[!grepl("_gzip_v2\\.rds$", mask_files)]


  for (mask_path in mask_files) {
    mask_type <- infer_mask_type(mask_path)
    if (is.na(mask_type)) next

    for (fold in FOLDS) {
      idx <- dm_test_rowids_from_mask(mask_path, fold, MIN_TOTAL_TEST) %>%
        mutate(rep = rep_i, mask_type = mask_type, fold = fold)
      rows[[length(rows) + 1]] <- idx
    }
  }
}

dm_index <- bind_rows(rows)
dm_index <- dm_index %>%
  distinct(rep, mask_type, fold, row_id, .keep_all = TRUE)

write_csv(dm_index, file.path(BASE_DIR, "dm_eval_rowids_from_masks.csv"))
cat(sprintf("Saved DM row index: %d rows\n", nrow(dm_index)))

# -----------------------------------------------------------------------------
# Step 1b: Sanity check - compare n_test with DM's pymc_metrics_monthly_repeatedCV_fold.csv
# -----------------------------------------------------------------------------

dm_metrics_path <- file.path(BASE_DIR, "pymc_metrics_monthly_repeatedCV_fold.csv")
if (file.exists(dm_metrics_path)) {
  cat("\n=== Sanity Check: n_test alignment with DM (before metric calculations) ===\n")

  dm_fold <- read_csv(dm_metrics_path, show_col_types = FALSE) %>%
    select(mask_type, fold, rep, n_test_dm = n_test) %>%
    distinct()

  # Build expected n_test from dm_index (number of rows per rep/mask_type/fold)
  inla_expected <- dm_index %>%
    group_by(rep, mask_type, fold) %>%
    summarise(n_test_inla = n_distinct(row_id), .groups = "drop")


  check <- dm_fold %>%
    full_join(inla_expected, by = c("mask_type", "fold", "rep")) %>%
    mutate(
      match = n_test_dm == n_test_inla,
      diff = n_test_inla - n_test_dm
    ) %>%
    arrange(rep, mask_type, fold)

  n_match <- sum(check$match, na.rm = TRUE)
  n_total <- nrow(check)
  n_mismatch <- n_total - n_match

  if (n_mismatch == 0) {
    cat(sprintf("✓ All %d fold-level n_test values match between INLA dm_index and DM!\n\n", n_total))
  } else {
    cat(sprintf("⚠ WARNING: %d/%d fold-level n_test values MISMATCH!\n", n_mismatch, n_total))
    cat("Mismatches:\n")
    print(check %>% filter(!match | is.na(match)))
    cat("\nProceeding anyway, but metrics may not be comparable.\n\n")
  }

  # Save alignment check
  # write_csv(check, file.path(BASE_DIR, "inla_dm_ntest_alignment_check.csv"))
  cat(sprintf("Saved: %s\n\n", file.path(BASE_DIR, "inla_dm_ntest_alignment_check.csv")))
} else {
  cat(sprintf("\n⚠ DM metrics file not found: %s\n", dm_metrics_path))
  cat("Skipping n_test alignment check (will proceed with metrics calculation).\n\n")
}

# -----------------------------------------------------------------------------
# Step 2: Evaluate INLA fits restricted to DM rows
# -----------------------------------------------------------------------------

all_overall_dm <- list()
all_fold_dm <- list()

for (rep_i in 1:3) {
  rep_tag <- sprintf("rep%02d", rep_i)
  seed <- SEEDS[rep_i]
  out_dir_rep <- file.path(BASE_DIR, paste0("down_inla_full_CV_", rep_tag))


  cat(sprintf("\n=== Evaluating rep %d ===\n", rep_i))

  # Filter DM index to this rep

  dm_rep <- dm_index %>% filter(rep == rep_i)

  # Read INLA artifacts
  arts <- read_inla_artifacts(out_dir = out_dir_rep)

  cat("arts$point columns:\n")
  print(names(arts$point))
  cat("\nHead:\n")
  print(head(arts$point))

  # ---- Point metrics: filter to DM rows ----
  point_dm <- arts$point %>%
    semi_join(dm_rep, by = c("mask_type", "fold", "row_id"))

  # Add rebased predictions
  point_dm_reb <- add_rebased_point_predictions(
    res_point_df = point_dm,
    data_df = data,
    group_cols = rebase_group_cols,
    target_col = rebase_target_col,
    pred_cols = c("pred_mean", "pred_median"),
    clamp_nonneg = TRUE
  )

  # Ensure columns exist

  if (!"pred_mean_rebase" %in% names(point_dm_reb)) point_dm_reb$pred_mean_rebase <- NA_real_
  if (!"pred_median_rebase" %in% names(point_dm_reb)) point_dm_reb$pred_median_rebase <- NA_real_

  tabs_mean_dm <- make_metrics_counts_incidence_prob(
    res_point_df = point_dm,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_mean"
  )

  tabs_median_dm <- make_metrics_counts_incidence_prob(
    res_point_df = point_dm,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_median"
  )

  tabs_mean_dm_reb <- make_metrics_counts_incidence_prob(
    res_point_df = point_dm_reb,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_mean_rebase"
  )

  tabs_median_dm_reb <- make_metrics_counts_incidence_prob(
    res_point_df = point_dm_reb,
    data_df = data,
    per = 1e5,
    pop_floor = 1,
    pred_col = "pred_median_rebase"
  )

  # ---- Prob metrics: call fold function with subset_df filtered to DM rows ----
  models <- unique(arts$point$model)
  mask_types <- unique(dm_rep$mask_type)

  jobs <- tidyr::crossing(model = models, mask_type = mask_types, fold = FOLDS) %>%
    dplyr::mutate(seed_job = (crps_seed_base + rep_i) + (fold - 1L) +
      dplyr::dense_rank(paste(model, mask_type)) * 1000L)

  res_prob_dm_fold <- purrr::pmap_dfr(jobs, function(model, mask_type, fold, seed_job) {
    dm_rows <- dm_rep %>% dplyr::filter(.data$mask_type == mask_type, .data$fold == fold)
    subset_df <- data %>% dplyr::filter(row_id %in% dm_rows$row_id)

    inla_cov_crps_fold_from_saved_rebase(
      model = model, mask_type = mask_type, fold = fold,
      subset_df = subset_df,
      out_dir = out_dir_rep,
      nsamp = 300,
      per = 1e5,
      seed = seed_job,
      group_cols = rebase_group_cols,
      target_col = rebase_target_col
    )
  })



  # ---- Consolidate: overall ----
  normalise_join_keys <- function(df) {
    df %>%
      mutate(
        model = str_trim(as.character(model)),
        mask_type = str_trim(as.character(mask_type)),
        mask_type = recode(mask_type, future = "extrap_future", past = "extrap_past"),
        model = str_replace(model, "_extrap$", "")
      )
  }

  tabs_mean_overall <- normalise_join_keys(tabs_mean_dm$overall)
  tabs_median_overall <- normalise_join_keys(tabs_median_dm$overall)
  tabs_mean_reb_overall <- normalise_join_keys(tabs_mean_dm_reb$overall)
  tabs_median_reb_overall <- normalise_join_keys(tabs_median_dm_reb$overall)

  res_prob_dm_overall <- res_prob_dm_fold %>%
    group_by(model, mask_type) %>%
    summarise(
      COV80 = safe_wmean(COV80, n_test_used),
      CRPS = safe_wmean(CRPS, n_test_used),
      COV80_rebase = safe_wmean(COV80_rebase, n_test_used_rebase),
      CRPS_rebase = safe_wmean(CRPS_rebase, n_test_used_rebase),
      n_test = sum(n_test, na.rm = TRUE),
      n_test_used = sum(n_test_used, na.rm = TRUE),
      n_test_used_rebase = sum(n_test_used_rebase, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    normalise_join_keys()

  consolidated_overall_dm <- tabs_mean_overall %>%
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
      res_prob_dm_overall %>%
        select(model, mask_type, COV80, CRPS, COV80_rebase, CRPS_rebase),
      by = c("model", "mask_type")
    ) %>%
    mutate(rep = rep_i, mask_seed = seed)

  # ---- Consolidate: fold ----
  tabs_mean_fold <- normalise_join_keys(tabs_mean_dm$fold)
  tabs_median_fold <- normalise_join_keys(tabs_median_dm$fold)
  tabs_mean_reb_fold <- normalise_join_keys(tabs_mean_dm_reb$fold)
  tabs_median_reb_fold <- normalise_join_keys(tabs_median_dm_reb$fold)
  res_fold <- normalise_join_keys(res_prob_dm_fold)

  consolidated_fold_dm <- tabs_mean_fold %>%
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

  # ---- Save per-rep outputs ----
  write.csv(
    consolidated_overall_dm,
    file.path(out_dir_rep, "inla_metrics_downscaling_dmrows.csv"),
    row.names = FALSE
  )
  write.csv(
    consolidated_fold_dm,
    file.path(out_dir_rep, "inla_metrics_downscaling_dmrows_fold.csv"),
    row.names = FALSE
  )

  cat(sprintf("Saved: %s\n", file.path(out_dir_rep, "inla_metrics_downscaling_dmrows.csv")))
  print(consolidated_overall_dm)

  # Store for aggregation
  all_overall_dm[[rep_tag]] <- consolidated_overall_dm
  all_fold_dm[[rep_tag]] <- consolidated_fold_dm

  gc()
}

# -----------------------------------------------------------------------------
# Step 3: Combine across repetitions
# -----------------------------------------------------------------------------

overall_allreps_dm <- bind_rows(all_overall_dm)
fold_allreps_dm <- bind_rows(all_fold_dm)

dir.create(BASE_DIR, recursive = TRUE, showWarnings = FALSE)

write.csv(
  overall_allreps_dm,
  file.path(BASE_DIR, "inla_metrics_downscaling_dmrows_repeatedCV_overall.csv"),
  row.names = FALSE
)

write.csv(
  fold_allreps_dm,
  file.path(BASE_DIR, "inla_metrics_downscaling_dmrows_repeatedCV_fold.csv"),
  row.names = FALSE
)

# Summary across reps (mean + sd)
metric_cols <- c(
  "MAE_inc_mean", "RMSE_inc_mean", "MAE_inc_median", "RMSE_inc_median",
  "MAE_inc_mean_rebase", "RMSE_inc_mean_rebase", "MAE_inc_median_rebase", "RMSE_inc_median_rebase",
  "COV80", "CRPS", "COV80_rebase", "CRPS_rebase"
)
metric_cols <- intersect(metric_cols, names(overall_allreps_dm))

summary_overall_dm <- overall_allreps_dm %>%
  group_by(model, mask_type) %>%
  summarise(
    across(all_of(metric_cols), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

write.csv(
  summary_overall_dm,
  file.path(BASE_DIR, "inla_metrics_downscaling_dmrows_repeatedCV_summary.csv"),
  row.names = FALSE
)

cat("\n=== Final Summary (DM-matched rows) ===\n")
print(summary_overall_dm)
