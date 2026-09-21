# CV_tab3b_metrics_by_region.R
#
# Region-stratified cross-validation performance for the final imputation
# (weekly, monthly: GAM and INLA) and disaggregation (Dirichlet-multinomial,
# INLA unscaled / scaled) models. Regions are the OpenDengue regions (od_region).
#
# Everything is computed from the saved repeated-CV outputs in
# runs/CV/20260126 (no model re-fitting). The model data are taken from the
# copy stored in the mask files, so the script does not depend on the
# current data/model_input files.
# Scores:
#   - MAE / RMSE (median predictor) from the saved point predictions
#     (INLA) or from posterior-predictive draws regenerated from the saved
#     GAM fits with the same seeds as the original evaluation.
#   - COV80 / CRPS from posterior-predictive draws regenerated from the saved
#     INLA / GAM fits with the same seeds and number of draws as the original
#     evaluation (00_inla_eval_helpers.R, 00_inla_eval_helpers_new.R,
#     00_gam_eval_helpers.R). The per-row results are then grouped by region.
#
# Dirichlet-multinomial (DM) model: row-level posterior-predictive scores are
# read from the CV re-run in runs/CV/20260126/dm_rerun_4chains
# (script/CV/03_down_DM_full_repeatedCV_v2.py, 4 chains x 1000 draws) and
# grouped by region. The DM and INLA disaggregation models are scored on the
# same test rows.
#
# Consistency check: the "All regions" rows reproduce the fold-level metrics in
# *_repeatedCV_fold.csv (which underlie the supplementary CV performance tables) and, for the
# DM model, in dm_rerun_4chains/pymc_metrics_monthly_repeatedCV_fold.csv. The
# maximum absolute difference per metric is printed to the console.
#
# Outputs (in runs/CV/20260126):
#   metrics_by_region_{weekly,monthly,downscaling}_fold.csv
#       fold x rep x region scores (cache: delete to force recomputation)
#   metrics_table_{weekly,monthly,dm_inla}_by_region.csv / .tsv
#       aggregated tables in the layout of the supplementary CV performance tables

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(mgcv)
  library(mvtnorm)
})

source("script/CV/00_inla_eval_helpers_new.R") # add_rebased_point_predictions, crps_mc_safe
source("script/CV/00_gam_eval_helpers.R")      # get_nb_theta_from_fit, coerce_time_numerics

run_dir <- file.path("runs", "CV", "20260126")
dm_dir <- file.path(run_dir, "dm_rerun_4chains") # DM CV re-run (row-level output)
region_file <- file.path("runs", "mi_full", "mi50", "opendengue_gap_filled_MI.csv")

reps <- 1:3
folds <- 1:3
per <- 1e5
mask_types <- c("interp", "extrap_future", "extrap_past") # order used by 02a/02b

# Seeds and draw counts used by the original evaluation scripts
crps_seed_base <- 123      # 02a / 02b / 04 (INLA COV80/CRPS), 01a / 01b (GAM COV80/CRPS)
gam_point_seed_base <- 20260126 # 01a / 01b (GAM posterior-predictive median)
nsamp <- 300

region_levels <- c(
  "South America", "North & Central America", "Caribbean",
  "East & Southeast Asia", "South Asia", "Pacific Islands",
  "Sub-Saharan Africa", "Europe, Middle East & North Africa"
)
all_label <- "All regions"

# ============================================================
# Helpers
# ============================================================

wmean_na <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) NA_real_ else stats::weighted.mean(x[ok], w[ok])
}

region_map <- read_csv(region_file, show_col_types = FALSE) %>%
  distinct(adm_0_name, od_region)
stopifnot(!anyDuplicated(region_map$adm_0_name))

add_region <- function(df) {
  df <- df %>%
    mutate(adm_0_name = as.character(adm_0_name)) %>%
    left_join(region_map, by = "adm_0_name")
  miss <- unique(df$adm_0_name[is.na(df$od_region)])
  if (length(miss)) stop("No od_region for: ", paste(miss, collapse = ", "))
  df
}

# Summarise row-level point scores (truth_inc, pred_inc) and probabilistic
# scores (covered, crps) per region, plus an "All regions" row.
summarise_by_region <- function(point_rows, prob_rows) {
  point_rows <- bind_rows(point_rows, mutate(point_rows, od_region = all_label))
  prob_rows <- bind_rows(prob_rows, mutate(prob_rows, od_region = all_label))

  pt <- point_rows %>%
    group_by(od_region) %>%
    summarise(
      n_test = n(),
      MAE_inc_median = mean(abs(pred_inc - truth_inc)),
      RMSE_inc_median = sqrt(mean((pred_inc - truth_inc)^2)),
      obs_mean_inc = mean(truth_inc),
      .groups = "drop"
    )

  pr <- prob_rows %>%
    group_by(od_region) %>%
    summarise(
      n_used = n(),
      COV80 = mean(covered, na.rm = TRUE),
      CRPS = mean(crps, na.rm = TRUE),
      .groups = "drop"
    )

  full_join(pt, pr, by = "od_region")
}

# ------------------------------------------------------------
# Model data as used when the CV fits were created. Each mask file holds a
# copy of the prepared model data (one row per data row, in the order used
# for fitting), so the data are taken from the mask files rather than from
# data/model_input/*.csv, which may have been updated after the CV run.
# row_id is the row position, as in the CV scripts.
# ------------------------------------------------------------

load_cv_data <- function(mask) {
  mask %>%
    select(-fold) %>%
    mutate(
      adm_0_name = factor(as.character(adm_0_name)),
      Year = factor(as.character(Year)),
      row_id = row_number()
    )
}

# Test rows of a fold. Mask rows are aligned with data rows (same order);
# this is checked before use.
mask_test_idx <- function(mask, data, fold) {
  stopifnot(
    nrow(mask) == nrow(data),
    identical(as.character(mask$adm_0_name), as.character(data$adm_0_name)),
    all(mask$time_seq == data$time_seq)
  )
  which(mask$fold == fold)
}

# ------------------------------------------------------------
# INLA (weekly / monthly): row-level COV80 / CRPS from the saved fit.
# Same draws as inla_cov_crps_fold_from_saved() in 00_inla_eval_helpers.R
# (the version used by 02a/02b), returned per row instead of averaged.
# ------------------------------------------------------------

inla_pp_rows_saved <- function(model, mask_type, fold, data, out_dir,
                               nsamp, per, seed, eta_cap = 20,
                               min_draws = 20, min_prop = 0.1) {
  set.seed(seed)

  fit <- readRDS(file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold)))
  meta <- readRDS(file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s_meta.rds", model, mask_type, fold)))
  on.exit({ rm(fit); gc(verbose = FALSE) }, add = TRUE)

  hold_df_idx <- meta$hold_df_idx
  test_row_ids <- meta$row_id[hold_df_idx]
  idx <- match(test_row_ids, data$row_id)
  if (anyNA(idx)) stop("row_id mismatch between saved INLA meta and current data.")

  y_true <- data$dengue_total[idx]
  pop <- data$pop_est[idx]
  ok <- is.finite(y_true) & !is.na(y_true) & is.finite(pop) & !is.na(pop) & pop > 0

  y_true <- y_true[ok]
  pop <- pop[ok]
  hold_ok <- hold_df_idx[ok]
  rid <- test_row_ids[ok]

  samp <- INLA::inla.posterior.sample(nsamp, fit, seed = seed, selection = list(Predictor = hold_ok))

  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))
  eta_mat[eta_mat > eta_cap] <- eta_cap
  eta_mat[eta_mat < -eta_cap] <- -eta_cap
  mu_mat <- exp(eta_mat)

  N <- nrow(mu_mat)
  S <- ncol(mu_mat)
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  fam <- tolower(as.character(if (!is.null(fit$.args$family)) fit$.args$family else fit$family)[1])
  if (grepl("pois", fam)) {
    for (s in seq_len(S)) {
      mu_s <- mu_mat[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- stats::rpois(sum(ok_mu), lambda = mu_s[ok_mu])
    }
  } else if (grepl("nb", fam)) {
    shp <- fit$summary.hyperpar
    theta_row <- grep("size|theta", rownames(shp), ignore.case = TRUE)
    if (length(theta_row) < 1L) stop("Could not find NB size/theta in summary.hyperpar.")
    theta <- as.numeric(shp[theta_row[1], "mean"])
    if (!is.finite(theta) || theta <= 0) theta <- 1e-6
    for (s in seq_len(S)) {
      mu_s <- mu_mat[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- stats::rnbinom(sum(ok_mu), size = theta, mu = mu_s[ok_mu])
    }
  } else {
    stop("Family not handled: ", fam)
  }

  scale_fac <- pop / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- y_true / scale_fac

  Ys_i[!is.finite(Ys_i)] <- NA
  thr <- max(min_draws, ceiling(min_prop * S))
  keep <- rowSums(is.finite(Ys_i)) >= thr

  Ys_i <- Ys_i[keep, , drop = FALSE]
  y_true_i <- y_true_i[keep]

  q10 <- apply(Ys_i, 1, stats::quantile, probs = 0.10, na.rm = TRUE)
  q90 <- apply(Ys_i, 1, stats::quantile, probs = 0.90, na.rm = TRUE)

  tibble(
    row_id = rid[keep],
    covered = as.numeric(y_true_i >= q10 & y_true_i <= q90),
    crps = crps_mc_safe(y_true_i, Ys_i)
  )
}

# ------------------------------------------------------------
# GAM (weekly / monthly): posterior-predictive draws from the saved fit.
#  - gam_pp_rows_point(): median predictor (q50) as in run_job_gam_pp_cached()
#  - gam_pp_rows_prob():  COV80 / CRPS rows as in gam_cov_crps_fold_new()
# Both follow the original functions step by step so that the same random
# draws are generated for the same seed.
# ------------------------------------------------------------

gam_pp_rows_point <- function(model, mask_type, fold, data, mask, out_dir, nsamp, per, seed) {
  set.seed(seed)

  test_idx <- mask_test_idx(mask, data, fold)
  newd <- data[test_idx, , drop = FALSE]

  fit <- readRDS(file.path(out_dir, "gam_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold)))

  X <- predict(fit, newd, type = "lpmatrix")
  stopifnot(nrow(X) == nrow(newd))

  b0 <- coef(fit)
  V <- fit$Vp
  B <- mvtnorm::rmvnorm(nsamp, mean = b0, sigma = V)
  Eta <- X %*% t(B)
  Eta[Eta > 20] <- 20
  Eta[Eta < -20] <- -20
  Mu <- exp(Eta)

  N <- nrow(newd)
  S <- nsamp
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  fam <- tolower(fit$family$family)
  if (grepl("pois", fam)) {
    for (s in seq_len(S)) {
      mu_s <- Mu[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- rpois(sum(ok_mu), lambda = mu_s[ok_mu])
    }
  } else if (grepl("neg|nb", fam)) {
    theta <- get_nb_theta_from_fit(fit)
    for (s in seq_len(S)) {
      mu_s <- Mu[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- rnbinom(sum(ok_mu), size = theta, mu = mu_s[ok_mu])
    }
  } else {
    stop("Unsupported family for PP draws: ", fit$family$family)
  }

  scale_fac <- newd$pop_est / per
  bad_scale <- !is.finite(scale_fac) | is.na(scale_fac) | scale_fac <= 0
  if (any(bad_scale)) scale_fac[bad_scale] <- NA_real_

  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  q50 <- as.numeric(apply(Ys_i, 1, stats::quantile, probs = 0.50, na.rm = TRUE))

  # Same rows and definitions as summarise_gam_metrics_incidence_mean_median()
  tibble(
    row_id = newd$row_id,
    truth_inc = newd$dengue_total / scale_fac,
    pred_inc = pmax(q50, 0)
  ) %>%
    filter(is.finite(truth_inc), is.finite(pred_inc))
}

gam_pp_rows_prob <- function(model, mask_type, fold, data, mask, out_dir, nsamp, per, seed) {
  set.seed(seed)

  fit <- readRDS(file.path(out_dir, "gam_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold)))

  test_idx <- mask_test_idx(mask, data, fold)
  test <- data[test_idx, , drop = FALSE]
  test <- test %>% filter(!is.na(pop_est), pop_est > 0, !is.na(dengue_total))
  test <- coerce_time_numerics(test)

  X <- predict(fit, test, type = "lpmatrix")
  bad <- which(!is.finite(rowSums(abs(X))))
  if (length(bad)) {
    test <- test[-bad, , drop = FALSE]
    X <- X[-bad, , drop = FALSE]
  }

  B <- mvtnorm::rmvnorm(nsamp, mean = coef(fit), sigma = fit$Vp)
  Eta <- X %*% t(B)
  Eta[Eta > 20] <- 20
  Eta[Eta < -20] <- -20
  Mu <- exp(Eta)

  N <- nrow(test)
  S <- nsamp
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  fam <- tolower(fit$family$family)
  if (grepl("pois", fam)) {
    for (s in seq_len(S)) {
      mu_s <- Mu[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- rpois(sum(ok_mu), lambda = mu_s[ok_mu])
    }
  } else if (grepl("neg|nb", fam)) {
    theta <- get_nb_theta_from_fit(fit)
    for (s in seq_len(S)) {
      mu_s <- Mu[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- rnbinom(sum(ok_mu), size = theta, mu = mu_s[ok_mu])
    }
  } else {
    stop("Unsupported family for PP draws: ", fit$family$family)
  }

  scale_fac <- test$pop_est / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- test$dengue_total / scale_fac

  Ys_i[!is.finite(Ys_i)] <- NA
  keep <- rowSums(is.finite(Ys_i)) >= max(20, ceiling(0.1 * S))

  Ys_i <- Ys_i[keep, , drop = FALSE]
  y_true_i <- y_true_i[keep]

  q10 <- apply(Ys_i, 1, quantile, probs = 0.10, na.rm = TRUE)
  q90 <- apply(Ys_i, 1, quantile, probs = 0.90, na.rm = TRUE)

  tibble(
    row_id = test$row_id[keep],
    covered = as.numeric(y_true_i >= q10 & y_true_i <= q90),
    crps = crps_mc_safe(y_true_i, Ys_i)
  )
}

# ------------------------------------------------------------
# INLA disaggregation: row-level COV80 / CRPS, unscaled and rebased to the
# annual total. Same draws as inla_cov_crps_fold_from_saved_rebase() in
# 00_inla_eval_helpers_new.R (used by 04_down_inla_eval_dmrows.R).
# ------------------------------------------------------------

inla_pp_rows_saved_rebase <- function(model, mask_type, fold, subset_df, out_dir,
                                      nsamp, per, seed,
                                      group_cols = c("adm_0_name", "Year"),
                                      target_col = "annual_total",
                                      min_group_size = 12L, thr_min = 20L, thr_frac = 0.10) {
  crps_sample_row <- function(y, x) {
    x <- x[is.finite(x)]
    S <- length(x)
    if (!S) return(NA_real_)
    x <- sort(x)
    term1 <- mean(abs(x - y))
    j <- seq_len(S)
    A <- sum((2 * j - S - 1) * x)
    term1 - (A / (S^2))
  }
  crps_sample_mat <- function(y, X) {
    vapply(seq_along(y), function(i) crps_sample_row(y[i], X[i, ]), numeric(1))
  }

  subset_df <- subset_df %>%
    mutate(row_id = as.integer(row_id), .pop = as.numeric(pop_est), .y = as.numeric(dengue_total))

  fit <- readRDS(file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold)))
  meta <- readRDS(file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s_meta.rds", model, mask_type, fold)))
  on.exit({ rm(fit); gc(verbose = FALSE) }, add = TRUE)

  hold_row_id <- if (!is.null(meta$hold_row_id)) meta$hold_row_id else meta$row_id[meta$hold_df_idx]
  hold_row_id <- as.integer(hold_row_id)

  pos_in_hold <- match(subset_df$row_id, hold_row_id)
  keep <- which(!is.na(pos_in_hold))
  if (length(keep) != nrow(subset_df)) stop("Some DM rows are not hold-out rows of the INLA fit.")

  subset_df <- subset_df[keep, , drop = FALSE]
  sel_df_idx <- meta$hold_df_idx[pos_in_hold[keep]]

  N <- nrow(subset_df)
  S <- as.integer(nsamp)

  set.seed(seed)
  samp <- INLA::inla.posterior.sample(S, fit, seed = seed, selection = list(Predictor = sel_df_idx))

  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))
  eta_mat <- pmin(eta_mat, 30)
  mu_mat <- exp(eta_mat)

  fam <- tolower(as.character(if (!is.null(fit$.args$family)) fit$.args$family else fit$family)[1])
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  if (grepl("pois", fam)) {
    for (s in seq_len(S)) Ys[, s] <- rpois(N, mu = mu_mat[, s])
  } else if (grepl("nb|neg", fam)) {
    hp_names <- names(samp[[1]]$hyperpar)
    i_size <- grep("size.*Negative Binomial|nbinom|theta|size", hp_names, ignore.case = TRUE)
    if (length(i_size) >= 1L) {
      i_size <- i_size[1]
      theta_s <- vapply(samp, function(z) as.numeric(z$hyperpar[i_size]), numeric(1))
    } else {
      shp <- fit$summary.hyperpar
      theta_row <- grep("size.*Negative Binomial|nbinom|theta|size", rownames(shp), ignore.case = TRUE)
      if (!length(theta_row)) stop("Couldn't find NB size/theta hyperparameter.")
      theta_s <- rep(as.numeric(shp[theta_row[1], "mean"]), S)
    }
    if (any(!is.finite(theta_s) | theta_s <= 0)) stop("Non-finite/invalid NB size in samples.")
    for (s in seq_len(S)) Ys[, s] <- rnbinom(N, size = theta_s[s], mu = mu_mat[, s])
  } else {
    stop("Family not handled: ", fam)
  }

  scale_fac <- subset_df$.pop / per
  y_true_i <- subset_df$.y / scale_fac

  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  Ys_i[!is.finite(Ys_i)] <- NA_real_

  thr <- max(as.integer(thr_min), as.integer(floor(thr_frac * S)))
  keep_raw <- rowSums(is.finite(Ys_i)) >= thr

  out <- tibble(row_id = subset_df$row_id, used_raw = keep_raw,
                covered = NA_real_, crps = NA_real_,
                used_reb = FALSE, covered_rebase = NA_real_, crps_rebase = NA_real_)

  if (any(keep_raw)) {
    q10 <- apply(Ys_i[keep_raw, , drop = FALSE], 1, stats::quantile, probs = 0.10, na.rm = TRUE)
    q90 <- apply(Ys_i[keep_raw, , drop = FALSE], 1, stats::quantile, probs = 0.90, na.rm = TRUE)
    yy <- y_true_i[keep_raw]
    out$covered[keep_raw] <- as.numeric(yy >= q10 & yy <= q90)
    out$crps[keep_raw] <- crps_sample_mat(yy, Ys_i[keep_raw, , drop = FALSE])
  }

  # draw-wise rebasing to annual totals
  g <- interaction(subset_df[, group_cols], drop = TRUE)
  gidx <- as.integer(g)
  G <- max(gidx)
  g_size <- as.integer(table(g))

  target <- tapply(subset_df[[target_col]], g, function(v) {
    u <- unique(stats::na.omit(v))
    if (!length(u)) return(NA_real_)
    as.numeric(u[1])
  })
  target <- as.numeric(target)
  target_ok <- is.finite(target) & !is.na(target) & target > 0
  size_ok <- g_size >= as.integer(min_group_size)
  group_ok <- target_ok & size_ok

  Ys_reb <- matrix(NA_real_, nrow = N, ncol = S)
  for (s in seq_len(S)) {
    sums <- as.numeric(tapply(Ys[, s], gidx, sum, na.rm = TRUE))
    f <- rep(NA_real_, G)
    ok <- group_ok & is.finite(sums) & sums > 0
    f[ok] <- target[ok] / sums[ok]
    Ys_reb[, s] <- Ys[, s] * f[gidx]
  }

  Ys_i_reb <- Ys_reb / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  Ys_i_reb[!is.finite(Ys_i_reb)] <- NA_real_

  keep_reb <- group_ok[gidx] & (rowSums(is.finite(Ys_i_reb)) >= thr)
  out$used_reb <- keep_reb

  if (any(keep_reb)) {
    q10r <- apply(Ys_i_reb[keep_reb, , drop = FALSE], 1, stats::quantile, probs = 0.10, na.rm = TRUE)
    q90r <- apply(Ys_i_reb[keep_reb, , drop = FALSE], 1, stats::quantile, probs = 0.90, na.rm = TRUE)
    yy <- y_true_i[keep_reb]
    out$covered_rebase[keep_reb] <- as.numeric(yy >= q10r & yy <= q90r)
    out$crps_rebase[keep_reb] <- crps_sample_mat(yy, Ys_i_reb[keep_reb, , drop = FALSE])
  }

  out
}

# ============================================================
# PART 1: weekly / monthly imputation (GAM, INLA)
# ============================================================

read_point_rows_inla <- function(out_dir, data) {
  pts <- list.files(file.path(out_dir, "inla_point"), pattern = "\\.csv$", full.names = TRUE)
  map_dfr(pts, read_csv, show_col_types = FALSE) %>%
    left_join(data %>% select(row_id, adm_0_name, truth = dengue_total, pop_est), by = "row_id") %>%
    filter(!is.na(truth), !is.na(pop_est), pop_est >= 1) %>%
    mutate(
      truth_inc = per * truth / pop_est,
      pred_inc = per * pmax(pred_median, 0) / pop_est
    )
}

compute_fold_by_region_imputation <- function(resolution) {
  mask_prefix <- if (resolution == "weekly") "inla_full_mask_w_" else "inla_full_mask_m_"

  rows <- list()
  for (rep_i in reps) {
    rep_tag <- sprintf("rep%02d", rep_i)
    inla_dir <- file.path(run_dir, paste0("inla_full_CV_", resolution, "_", rep_tag))
    gam_dir <- file.path(run_dir, paste0("gam_full_CV_", resolution, "_", rep_tag))
    mask_dir <- file.path(run_dir, "masks", paste0(resolution, "_", rep_tag))

    masks <- lapply(setNames(mask_types, mask_types), function(m) readRDS(file.path(mask_dir, paste0(mask_prefix, m, ".rds"))))
    data <- load_cv_data(masks$interp)

    inla_point <- read_point_rows_inla(inla_dir, data) %>%
      mutate(mask_type = recode(mask_type, future = "extrap_future", past = "extrap_past"))

    for (mask_i in seq_along(mask_types)) {
      mask_type <- mask_types[mask_i]
      for (fold in folds) {
        cat(sprintf("[%s] %s | rep %d | %s | fold %d\n", format(Sys.time(), "%H:%M:%S"), resolution, rep_i, mask_type, fold))

        # ---- INLA ----
        pt <- inla_point %>% filter(mask_type == !!mask_type, fold == !!fold) %>% add_region()
        pr <- inla_pp_rows_saved(
          "inla_h_shared", mask_type, fold, data, inla_dir,
          nsamp = nsamp, per = per,
          seed = (crps_seed_base + rep_i) + 10000L * 1L + 100L * mask_i + as.integer(fold)
        ) %>%
          left_join(data %>% select(row_id, adm_0_name), by = "row_id") %>% add_region()
        rows[[length(rows) + 1]] <- summarise_by_region(pt, pr) %>%
          mutate(Model = "INLA", mask_type = mask_type, rep = rep_i, fold = fold)

        # ---- GAM ----
        pt <- gam_pp_rows_point(
          "gam_bench_org", mask_type, fold, data, masks[[mask_type]], gam_dir,
          nsamp = nsamp, per = per, seed = gam_point_seed_base + rep_i
        ) %>%
          left_join(data %>% select(row_id, adm_0_name), by = "row_id") %>% add_region()
        pr <- gam_pp_rows_prob(
          "gam_bench_org", mask_type, fold, data, masks[[mask_type]], gam_dir,
          nsamp = nsamp, per = per, seed = crps_seed_base + rep_i
        ) %>%
          left_join(data %>% select(row_id, adm_0_name), by = "row_id") %>% add_region()
        rows[[length(rows) + 1]] <- summarise_by_region(pt, pr) %>%
          mutate(Model = "GAM", mask_type = mask_type, rep = rep_i, fold = fold)
      }
    }
  }

  bind_rows(rows) %>%
    select(Model, mask_type, rep, fold, od_region, n_test, MAE_inc_median, RMSE_inc_median,
           obs_mean_inc, n_used, COV80, CRPS)
}

check_against_fold_csv_imputation <- function(fold_region, resolution) {
  ref <- bind_rows(
    read_csv(file.path(run_dir, sprintf("inla_metrics_%s_repeatedCV_fold.csv", resolution)), show_col_types = FALSE) %>% mutate(Model = "INLA"),
    read_csv(file.path(run_dir, sprintf("gam_metrics_%s_repeatedCV_fold.csv", resolution)), show_col_types = FALSE) %>% mutate(Model = "GAM")
  ) %>%
    mutate(mask_type = recode(str_trim(mask_type), future = "extrap_future", past = "extrap_past"),
           rep = as.integer(rep), fold = as.integer(fold)) %>%
    select(Model, mask_type, rep, fold,
           n_test_ref = n_test_median, MAE_ref = MAE_inc_median, RMSE_ref = RMSE_inc_median,
           COV80_ref = COV80, CRPS_ref = CRPS)

  cmp <- fold_region %>%
    filter(od_region == all_label) %>%
    inner_join(ref, by = c("Model", "mask_type", "rep", "fold")) %>%
    group_by(Model) %>%
    summarise(
      n_folds = n(),
      n_test_maxdiff = max(abs(n_test - n_test_ref)),
      MAE_maxdiff = max(abs(MAE_inc_median - MAE_ref)),
      RMSE_maxdiff = max(abs(RMSE_inc_median - RMSE_ref)),
      COV80_maxdiff = max(abs(COV80 - COV80_ref)),
      CRPS_maxdiff = max(abs(CRPS - CRPS_ref)),
      .groups = "drop"
    )
  cat(sprintf("\nConsistency check vs *_repeatedCV_fold.csv (%s), max |difference| over %s fold x rep:\n",
              resolution, paste(unique(cmp$n_folds), collapse = "/")))
  print(as.data.frame(cmp), digits = 4)
  invisible(cmp)
}

build_region_table_imputation <- function(fold_region) {
  fold_region %>%
    group_by(mask_type, Model, od_region) %>%
    summarise(
      obs_mean_inc = wmean_na(obs_mean_inc, n_test),
      MAE_median = wmean_na(MAE_inc_median, n_test),
      RMSE_median = wmean_na(RMSE_inc_median, n_test),
      cov80 = wmean_na(COV80, n_test),
      crps = wmean_na(CRPS, n_test),
      n_test = as.integer(sum(n_test)),
      .groups = "drop"
    ) %>%
    mutate(
      nMAE_median = MAE_median / obs_mean_inc,
      nRMSE_median = RMSE_median / obs_mean_inc
    ) %>%
    format_region_table(model_levels = c("GAM", "INLA"))
}

format_region_table <- function(tbl, model_levels) {
  tbl %>%
    mutate(
      MAE_median = sprintf("%.1f", round(MAE_median, 1)),
      RMSE_median = sprintf("%.1f", round(RMSE_median, 1)),
      cov80 = sprintf("%.2f", round(cov80, 2)),
      crps = sprintf("%.1f", round(crps, 1)),
      obs_mean_inc = sprintf("%.1f", round(obs_mean_inc, 1)),
      nMAE_median = sprintf("%.2f", round(nMAE_median, 2)),
      nRMSE_median = sprintf("%.2f", round(nRMSE_median, 2))
    ) %>%
    arrange(
      factor(mask_type, levels = c("interp", "extrap_past", "extrap_future")),
      factor(od_region, levels = c(all_label, region_levels)),
      factor(Model, levels = model_levels)
    ) %>%
    select(mask_type, od_region, Model, n_test, obs_mean_inc,
           MAE_median, RMSE_median, nMAE_median, nRMSE_median, cov80, crps)
}

write_table_outputs <- function(tbl, stem) {
  write_csv(tbl, file.path(run_dir, paste0(stem, ".csv")), na = "")
  write_tsv(tbl, file.path(run_dir, paste0(stem, ".tsv")), na = "")
}

for (resolution in c("weekly", "monthly")) {
  cat(sprintf("\n=== %s imputation models by region ===\n", resolution))
  cache <- file.path(run_dir, sprintf("metrics_by_region_%s_fold.csv", resolution))
  if (file.exists(cache)) {
    cat("Using cached fold-level file:", cache, "\n")
    fold_region <- read_csv(cache, show_col_types = FALSE)
  } else {
    fold_region <- compute_fold_by_region_imputation(resolution)
    write_csv(fold_region, cache)
    cat("Saved:", cache, "\n")
  }
  check_against_fold_csv_imputation(fold_region, resolution)
  tbl <- build_region_table_imputation(fold_region)
  print(as.data.frame(tbl))
  write_table_outputs(tbl, sprintf("metrics_table_%s_by_region", resolution))
}

# ============================================================
# PART 2: annual-to-monthly disaggregation (DM, INLA unscaled / scaled),
# evaluated on the rows used by the DM model
# ============================================================

# Compare the "All regions" rows of a fold x rep x region table with a
# reference fold-level file (columns Model, mask_type, rep, fold, *_ref).
compare_all_regions <- function(fold_region, ref, ref_name) {
  cmp <- fold_region %>%
    filter(od_region == all_label) %>%
    inner_join(ref, by = c("Model", "mask_type", "rep", "fold")) %>%
    group_by(Model) %>%
    summarise(
      n_folds = n(),
      n_test_maxdiff = max(abs(n_test - n_test_ref)),
      MAE_maxdiff = max(abs(MAE_inc_median - MAE_ref)),
      RMSE_maxdiff = max(abs(RMSE_inc_median - RMSE_ref)),
      COV80_maxdiff = max(abs(COV80 - COV80_ref)),
      CRPS_maxdiff = max(abs(CRPS - CRPS_ref)),
      .groups = "drop"
    )
  cat(sprintf("\nConsistency check vs %s, max |difference| over fold x rep:\n", ref_name))
  print(as.data.frame(cmp), digits = 4)
  invisible(cmp)
}

# DM model: the Python CV run writes one row per test country-year-month with
# the observed incidence, the posterior-predictive median, the 10% / 90%
# quantiles and the CRPS (all per 100,000). Rows without a prediction (below
# the minimum-draw threshold) are excluded, as in the fold-level scoring.
compute_fold_by_region_dm <- function() {
  rows_dm <- read_csv(file.path(dm_dir, "pymc_row_predictions_fold.csv"), show_col_types = FALSE) %>%
    mutate(mask_type = str_trim(mask_type), rep = as.integer(rep), fold = as.integer(fold)) %>%
    filter(!is.na(pred_median_inc)) %>%
    add_region() %>%
    mutate(
      truth_inc = obs_inc,
      pred_inc = pred_median_inc,
      covered = obs_inc >= q10_inc & obs_inc <= q90_inc
    )

  rows_dm %>%
    group_by(mask_type, rep, fold) %>%
    group_map(~ summarise_by_region(.x, .x) %>%
                mutate(Model = "DM", mask_type = .y$mask_type, rep = .y$rep, fold = .y$fold)) %>%
    bind_rows() %>%
    select(Model, mask_type, rep, fold, od_region, n_test, MAE_inc_median, RMSE_inc_median,
           obs_mean_inc, n_used, COV80, CRPS)
}

check_against_fold_csv_dm <- function(fold_region) {
  ref <- read_csv(file.path(dm_dir, "pymc_metrics_monthly_repeatedCV_fold.csv"), show_col_types = FALSE) %>%
    transmute(Model = "DM", mask_type = str_trim(mask_type), rep = as.integer(rep), fold = as.integer(fold),
              n_test_ref = n_test, MAE_ref = MAE_inc_median, RMSE_ref = RMSE_inc_median,
              COV80_ref = COV80, CRPS_ref = CRPS)
  compare_all_regions(fold_region, ref, "dm_rerun_4chains/pymc_metrics_monthly_repeatedCV_fold.csv")
}

compute_fold_by_region_downscaling <- function() {
  dm_index <- read_csv(file.path(run_dir, "dm_eval_rowids_from_masks.csv"), show_col_types = FALSE) %>%
    mutate(rep = as.integer(rep), fold = as.integer(fold))

  rows <- list()
  for (rep_i in reps) {
    rep_tag <- sprintf("rep%02d", rep_i)
    out_dir <- file.path(run_dir, paste0("down_inla_full_CV_", rep_tag))
    data <- load_cv_data(readRDS(file.path(run_dir, "masks", paste0("downscaling_", rep_tag), "inla_down_mask_interp.rds")))
    dm_rep <- dm_index %>% filter(rep == rep_i)

    pts <- list.files(file.path(out_dir, "inla_point"), pattern = "\\.csv$", full.names = TRUE)
    point_dm <- map_dfr(pts, read_csv, show_col_types = FALSE) %>%
      semi_join(dm_rep, by = c("mask_type", "fold", "row_id"))

    # Rebased point predictions (same call as 04_down_inla_eval_dmrows.R)
    point_dm_reb <- add_rebased_point_predictions(
      res_point_df = point_dm, data_df = data,
      group_cols = c("adm_0_name", "Year"), target_col = "annual_total",
      pred_cols = c("pred_mean", "pred_median"), clamp_nonneg = TRUE
    ) %>%
      left_join(data %>% select(row_id, truth = dengue_total, pop_est), by = "row_id") %>%
      filter(!is.na(truth), !is.na(pop_est), pop_est >= 1) %>%
      mutate(
        truth_inc = per * truth / pop_est,
        pred_inc_unscaled = per * pmax(pred_median, 0) / pop_est,
        pred_inc_scaled = per * pmax(pred_median_rebase, 0) / pop_est
      )

    for (mask_type in sort(unique(dm_rep$mask_type))) {
      for (fold in folds) {
        cat(sprintf("[%s] downscaling | rep %d | %s | fold %d\n", format(Sys.time(), "%H:%M:%S"), rep_i, mask_type, fold))

        dm_rows <- dm_rep %>% filter(mask_type == !!mask_type, fold == !!fold)
        subset_df <- data %>% filter(row_id %in% dm_rows$row_id)

        # seed rule of 04_down_inla_eval_dmrows.R: dense_rank(paste(model, mask_type))
        # over the three mask types gives extrap_future = 1, extrap_past = 2, interp = 3
        mask_rank <- match(mask_type, c("extrap_future", "extrap_past", "interp"))
        seed_job <- (crps_seed_base + rep_i) + (fold - 1L) + mask_rank * 1000L

        pr <- inla_pp_rows_saved_rebase(
          "inla_h_shared", mask_type, fold, subset_df, out_dir,
          nsamp = nsamp, per = per, seed = seed_job
        ) %>%
          left_join(data %>% select(row_id, adm_0_name), by = "row_id") %>% add_region()

        pt <- point_dm_reb %>% filter(mask_type == !!mask_type, fold == !!fold) %>% add_region()

        rows[[length(rows) + 1]] <- bind_rows(
          summarise_by_region(pt %>% mutate(pred_inc = pred_inc_unscaled),
                              pr %>% filter(used_raw)) %>%
            mutate(Model = "INLA unscaled"),
          summarise_by_region(pt %>% mutate(pred_inc = pred_inc_scaled),
                              pr %>% filter(used_reb) %>% mutate(covered = covered_rebase, crps = crps_rebase)) %>%
            mutate(Model = "INLA scaled")
        ) %>%
          mutate(mask_type = mask_type, rep = rep_i, fold = fold)
      }
    }
  }

  bind_rows(rows) %>%
    select(Model, mask_type, rep, fold, od_region, n_test, MAE_inc_median, RMSE_inc_median,
           obs_mean_inc, n_used, COV80, CRPS)
}

check_against_fold_csv_downscaling <- function(fold_region) {
  ref <- read_csv(file.path(run_dir, "inla_metrics_downscaling_dmrows_repeatedCV_fold.csv"), show_col_types = FALSE) %>%
    mutate(mask_type = str_trim(mask_type), rep = as.integer(rep), fold = as.integer(fold))
  ref <- bind_rows(
    ref %>% transmute(Model = "INLA unscaled", mask_type, rep, fold, n_test_ref = n_test_median,
                      MAE_ref = MAE_inc_median, RMSE_ref = RMSE_inc_median, COV80_ref = COV80, CRPS_ref = CRPS),
    ref %>% transmute(Model = "INLA scaled", mask_type, rep, fold, n_test_ref = n_test_median,
                      MAE_ref = MAE_inc_median_rebase, RMSE_ref = RMSE_inc_median_rebase,
                      COV80_ref = COV80_rebase, CRPS_ref = CRPS_rebase)
  )

  compare_all_regions(fold_region, ref, "inla_metrics_downscaling_dmrows_repeatedCV_fold.csv")
}

build_region_table_downscaling <- function(fold_region) {
  fold_region %>%
    group_by(mask_type, Model, od_region) %>%
    summarise(
      obs_mean_inc = wmean_na(obs_mean_inc, n_test),
      MAE_median = wmean_na(MAE_inc_median, n_test),
      RMSE_median = wmean_na(RMSE_inc_median, n_test),
      cov80 = wmean_na(COV80, n_test),
      crps = wmean_na(CRPS, n_test),
      # the supplementary disaggregation performance table reports the test-set size of one repetition
      n_test = as.integer(round(max(tapply(n_test, rep, sum)))),
      .groups = "drop"
    ) %>%
    mutate(
      nMAE_median = MAE_median / obs_mean_inc,
      nRMSE_median = RMSE_median / obs_mean_inc
    ) %>%
    format_region_table(model_levels = c("DM", "INLA unscaled", "INLA scaled"))
}

cat("\n=== disaggregation models by region ===\n")
cache <- file.path(run_dir, "metrics_by_region_downscaling_fold.csv")
if (file.exists(cache)) {
  cat("Using cached fold-level file:", cache, "\n")
  fold_region_inla <- read_csv(cache, show_col_types = FALSE)
} else {
  fold_region_inla <- compute_fold_by_region_downscaling()
  write_csv(fold_region_inla, cache)
  cat("Saved:", cache, "\n")
}
check_against_fold_csv_downscaling(fold_region_inla)

fold_region_dm <- compute_fold_by_region_dm()
check_against_fold_csv_dm(fold_region_dm)

# The DM and INLA models must be scored on the same rows: compare the
# per-region test-set sizes fold by fold.
n_cmp <- fold_region_dm %>%
  select(mask_type, rep, fold, od_region, n_dm = n_test) %>%
  full_join(fold_region_inla %>% filter(Model == "INLA scaled") %>%
              select(mask_type, rep, fold, od_region, n_inla = n_test),
            by = c("mask_type", "rep", "fold", "od_region"))
cat(sprintf("\nTest rows per fold x rep x region, DM vs INLA: max |difference| = %g (%d cells)\n",
            max(abs(n_cmp$n_dm - n_cmp$n_inla), na.rm = TRUE), nrow(n_cmp)))
if (anyNA(n_cmp$n_dm) || anyNA(n_cmp$n_inla)) warning("Region cells present for only one of DM / INLA.")

tbl_dm <- build_region_table_downscaling(bind_rows(fold_region_dm, fold_region_inla))
print(as.data.frame(tbl_dm))
write_table_outputs(tbl_dm, "metrics_table_dm_inla_by_region")
