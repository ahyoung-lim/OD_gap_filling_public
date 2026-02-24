# ==========================================================================
# inla_eval_helpers_scaled.R
#
# Evaluation helpers for INLA repeated CV.
#
# Provides:
#   - run_job_inla_cached(): fit + save INLA fits/meta + cached point preds
#   - make_metrics_counts_incidence_prob(): MAE/RMSE on counts + incidence
#   - inla_cov_crps_*(): COV80/CRPS from posterior predictive draws (unscaled)
#   - inla_cov_crps_*_rebase(): same, plus annual-total rebasing (scaled)
#   - check_inla_data_fit_alignment(): sanity check meta row_id alignment
#
# Assumes:
#   - data has dengue_total, pop_est; and (for rebasing) annual_total
#   - INLA fit/meta saved under out_dir/inla_fits
#   - get_fold_split() exists in your pipeline (mask -> train/test indices)
# ==========================================================================

# ---- basic numeric helpers ----
mae_vec <- function(p, y) mean(abs(p - y), na.rm = TRUE)
rmse_vec <- function(p, y) sqrt(mean((p - y)^2, na.rm = TRUE))

# Empirical-sample CRPS using identity:
# CRPS(F,y) = E|X-y| - 0.5 E|X-X'| where X,X'~F.
# Here F is represented by Monte Carlo draws per row.
crps_mc_safe <- function(y, S_mat) {
  a <- rowMeans(abs(S_mat - y), na.rm = TRUE)
  b <- apply(S_mat, 1, function(s) {
    s <- s[is.finite(s)]
    if (length(s) < 2) {
      return(NA_real_)
    }
    s <- sort(s)
    k <- seq_along(s)
    S <- length(s)
    (2 / (S^2)) * sum((2 * k - S - 1) * s)
  })
  a - 0.5 * b
}

safe_wmean <- function(x, w) {
  if (length(x) == 0 || length(w) == 0) {
    return(NA_real_)
  }
  if (length(x) != length(w)) {
    return(NA_real_)
  }
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) {
    return(NA_real_)
  }
  if (sum(w[ok], na.rm = TRUE) <= 0) {
    return(NA_real_)
  }
  stats::weighted.mean(x, w = w, na.rm = TRUE)
}

# Read mask object from file or use as-is
read_mask_obj <- function(x) {
  if (is.character(x) && length(x) == 1L) readRDS(x) else x
}

# Read cached artifacts from disk (if you write them)
read_inla_artifacts <- function(out_dir = "runs") {
  pts <- list.files(file.path(out_dir, "inla_point"), pattern = "\\.csv$", full.names = TRUE)
  prs <- list.files(file.path(out_dir, "inla_prob"), pattern = "\\.csv$", full.names = TRUE)
  res_point <- if (length(pts)) purrr::map_dfr(pts, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
  res_prob <- if (length(prs)) purrr::map_dfr(prs, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
  list(point = res_point, prob = res_prob)
}

# Build manifest for job tracking
build_manifest <- function(models, masks, folds, out_dir = "runs") {
  tibble::tibble(model = models) %>%
    tidyr::crossing(mask_type = names(masks), fold = folds) %>%
    dplyr::mutate(
      point_path = file.path(out_dir, "inla_point", glue::glue("{model}_{mask_type}_fold{fold}.csv")),
      prob_path  = file.path(out_dir, "inla_prob", glue::glue("{model}_{mask_type}_fold{fold}.csv")),
      is_done    = file.exists(point_path) & file.exists(prob_path)
    )
}

# --------------------------------------------------------------------------
# WORKFLOW 1: Fit INLA fold + cache point preds + save fit/meta for PP draws
# --------------------------------------------------------------------------
run_job_inla_cached <- function(model, mask_type, fold,
                                data, masks, formulas, ctrl_fam,
                                train_policy = "inclusive",
                                per = 1e5,
                                seed = 20250811,
                                out_dir = "runs") {
  dir_point <- file.path(out_dir, "inla_point")
  dir_prob <- file.path(out_dir, "inla_prob")
  dir_fits <- file.path(out_dir, "inla_fits")
  dir.create(dir_point, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_prob, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_fits, recursive = TRUE, showWarnings = FALSE)

  fp_point <- file.path(dir_point, sprintf("%s_%s_fold%s.csv", model, mask_type, fold))
  fp_prob <- file.path(dir_prob, sprintf("%s_%s_fold%s.csv", model, mask_type, fold))
  fp_fit <- file.path(dir_fits, sprintf("%s_%s_fold%s.rds", model, mask_type, fold))
  fp_meta <- file.path(dir_fits, sprintf("%s_%s_fold%s_meta.rds", model, mask_type, fold))

  set.seed(seed)
  INLA::inla.setOption(num.threads = "1:1")

  if (!"row_id" %in% names(data)) data <- dplyr::mutate(data, row_id = dplyr::row_number())

  mask <- masks[[mask_type]]
  formula <- formulas[[model]]

  sp <- get_fold_split(mask, fold_id = fold, train_policy = train_policy)
  if (length(sp$test_idx) == 0L) {
    point <- tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      row_id = integer(0), pred_mean = numeric(0), pred_median = numeric(0), pred = numeric(0)
    )
    prob <- tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test = 0L
    )
    readr::write_csv(point, fp_point)
    readr::write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  train <- data[sp$train_idx, , drop = FALSE] %>% dplyr::mutate(y = .data$dengue_total)
  test <- data[sp$test_idx, , drop = FALSE] %>% dplyr::mutate(y = NA_real_)
  df <- dplyr::bind_rows(train, test)

  fit <- INLA::inla(
    update(formula, y ~ .),
    data = df,
    family = ctrl_fam$family,
    control.family = ctrl_fam$control.family,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = FALSE, waic = FALSE, config = TRUE),
    control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")
  )

  hold_df_idx <- which(is.na(df$y))

  # Save fit + meta (needed for probabilistic metrics)
  saveRDS(fit, fp_fit)
  saveRDS(list(hold_df_idx = hold_df_idx, row_id = df$row_id), fp_meta)

  # Cache point predictions
  point <- tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    row_id = df$row_id[hold_df_idx],
    pred_mean = fit$summary.fitted.values$mean[hold_df_idx],
    pred_median = fit$summary.fitted.values$`0.5quant`[hold_df_idx],
    pred = fit$summary.fitted.values$`0.5quant`[hold_df_idx]
  )
  readr::write_csv(point, fp_point)

  # Minimal prob file marker (prob metrics are computed later from saved fits)
  prob <- tibble::tibble(model = model, mask_type = mask_type, fold = fold, n_test = length(hold_df_idx))
  readr::write_csv(prob, fp_prob)

  list(point = point, prob = prob, fit_path = fp_fit)
}

# ----------------------------------------------------------------------------
# run_inla_parallel_jupyter
# Purpose: Run jobs in parallel with Jupyter-compatible progress
# Use: For Jupyter notebooks with multiple cores
# ----------------------------------------------------------------------------
run_inla_parallel_jupyter <- function(data, masks, formulas, ctrl_fam,
                                      folds = 1:3, out_dir = "runs",
                                      workers = 3, nsamp = 400, seed = 20250811) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% dplyr::filter(!is_done)
  message(glue::glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))

  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  progressr::handlers("txtprogressbar")
  future::plan(future::multisession, workers = workers)

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(todo))
    res_list <- furrr::future_pmap(
      todo[c("model", "mask_type", "fold")],
      function(model, mask_type, fold) {
        p(sprintf("%s / %s f=%s", model, mask_type, fold))
        run_job_inla_cached(
          model = model, mask_type = mask_type, fold = fold,
          data = data, masks = masks, formulas = formulas, ctrl_fam = ctrl_fam,
          seed = seed, out_dir = out_dir
        )
      },
      .options = furrr::furrr_options(
        seed = seed,
        packages = c("INLA", "dplyr", "purrr", "glue")
      )
    )
    res_list
  })
}

# --------------------------------------------------------------------------
# WORKFLOW 2: Point metrics (MAE/RMSE) on counts + incidence
# --------------------------------------------------------------------------
make_metrics_counts_incidence_prob <- function(res_point_df,
                                               data_df, per = 1e5, pop_floor = 1,
                                               pred_col = "pred") {
  stopifnot(is.data.frame(res_point_df) || inherits(res_point_df, "tbl"))

  # Ensure stable row_id in data_df
  if (!"row_id" %in% names(data_df)) {
    data_df <- dplyr::mutate(data_df, row_id = dplyr::row_number())
  } else {
    if (anyNA(data_df$row_id)) stop("data_df$row_id contains NA values.", call. = FALSE)
    if (anyDuplicated(data_df$row_id) > 0) stop("data_df$row_id contains duplicates.", call. = FALSE)
  }

  if (!pred_col %in% names(res_point_df)) stop("pred_col '", pred_col, "' not found in res_point_df.", call. = FALSE)

  df <- res_point_df %>%
    dplyr::left_join(
      data_df %>% dplyr::select(row_id, truth = dengue_total, pop_est),
      by = "row_id"
    ) %>%
    dplyr::filter(!is.na(truth), !is.na(pop_est), pop_est >= pop_floor) %>%
    dplyr::mutate(
      truth_inc = per * truth / pop_est,
      pred_used = .data[[pred_col]],
      pred_inc  = per * pmax(pred_used, 0) / pop_est
    )

  metrics_fold <- df %>%
    dplyr::summarise(
      n_test = dplyr::n(),
      MAE_counts = mae_vec(pred_used, truth),
      RMSE_counts = rmse_vec(pred_used, truth),
      MAE_inc = mae_vec(pred_inc, truth_inc),
      RMSE_inc = rmse_vec(pred_inc, truth_inc),
      .by = c(model, mask_type, fold)
    )

  overall <- metrics_fold %>%
    dplyr::summarise(
      MAE_counts = stats::weighted.mean(MAE_counts, w = n_test, na.rm = TRUE),
      RMSE_counts = stats::weighted.mean(RMSE_counts, w = n_test, na.rm = TRUE),
      MAE_inc = stats::weighted.mean(MAE_inc, w = n_test, na.rm = TRUE),
      RMSE_inc = stats::weighted.mean(RMSE_inc, w = n_test, na.rm = TRUE),
      n_test = sum(n_test, na.rm = TRUE),
      .by = c(model, mask_type)
    ) %>%
    dplyr::arrange(mask_type, model)

  list(fold = metrics_fold, overall = overall)
}

# --------------------------------------------------------------------------
# Internal: choose an NB size/theta index in hyperpar vectors
# --------------------------------------------------------------------------
.find_nb_hyperpar_index <- function(hp_names) {
  if (is.null(hp_names) || !length(hp_names)) {
    return(NA_integer_)
  }
  i <- grep("size|theta|overdisp", hp_names, ignore.case = TRUE)
  if (!length(i)) {
    return(NA_integer_)
  }
  i[1]
}

# --------------------------------------------------------------------------
# WORKFLOW 3: Probabilistic metrics from saved INLA fits (unscaled)
# --------------------------------------------------------------------------
inla_cov_crps_fold_from_saved <- function(model, mask_type, fold,
                                          data, out_dir = "runs",
                                          nsamp = 400, per = 1e5, seed = 1,
                                          eta_cap = 20,
                                          min_draws = 20, min_prop = 0.1) {
  set.seed(seed)

  fp_fit <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold))
  fp_meta <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s_meta.rds", model, mask_type, fold))
  if (!file.exists(fp_fit) || !file.exists(fp_meta)) {
    return(NULL)
  }

  fit <- readRDS(fp_fit)
  meta <- readRDS(fp_meta)

  hold_df_idx <- meta$hold_df_idx
  df_row_id <- meta$row_id
  if (!length(hold_df_idx)) {
    return(NULL)
  }

  # Ensure stable row_id
  if (!"row_id" %in% names(data)) {
    data <- dplyr::mutate(data, row_id = dplyr::row_number())
  } else {
    if (anyNA(data$row_id)) stop("data$row_id contains NA.", call. = FALSE)
    if (anyDuplicated(data$row_id) > 0) stop("data$row_id contains duplicates.", call. = FALSE)
  }

  test_row_ids <- df_row_id[hold_df_idx]
  idx <- match(test_row_ids, data$row_id)
  if (anyNA(idx)) stop("row_id mismatch between saved INLA meta and current 'data'.", call. = FALSE)

  y_true <- data$dengue_total[idx]
  pop <- data$pop_est[idx]

  n_test_raw <- length(idx)

  # incidence defined
  ok <- is.finite(y_true) & !is.na(y_true) & is.finite(pop) & !is.na(pop) & pop > 0
  n_test_obs <- sum(ok)

  if (n_test_obs == 0L) {
    return(tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test_raw = n_test_raw, n_test_obs = 0L, n_test_used = 0L,
      COV80 = NA_real_, CRPS = NA_real_,
      note = "no_rows_with_finite_truth_and_positive_pop"
    ))
  }

  y_true <- y_true[ok]
  pop <- pop[ok]
  hold_ok <- hold_df_idx[ok]

  samp <- INLA::inla.posterior.sample(
    nsamp, fit,
    seed = seed,
    selection = list(Predictor = hold_ok)
  )

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
  } else if (grepl("nb|neg", fam)) {
    # Prefer per-draw NB size/theta from posterior samples
    theta_s <- NULL
    if (!is.null(samp[[1]]$hyperpar)) {
      i_size <- .find_nb_hyperpar_index(names(samp[[1]]$hyperpar))
      if (is.finite(i_size)) {
        theta_s <- vapply(samp, function(z) as.numeric(z$hyperpar[i_size]), numeric(1))
      }
    }

    # Fallback: fixed theta from summary.hyperpar
    if (is.null(theta_s)) {
      shp <- fit$summary.hyperpar
      theta_row <- grep("size|theta|overdisp", rownames(shp), ignore.case = TRUE)
      if (length(theta_row) < 1L) stop("Could not find NB size/theta in summary.hyperpar.", call. = FALSE)
      theta0 <- as.numeric(shp[theta_row[1], "mean"])
      if (!is.finite(theta0) || theta0 <= 0) theta0 <- 1e-6
      theta_s <- rep(theta0, S)
    }

    theta_floor <- suppressWarnings(min(theta_s[is.finite(theta_s) & theta_s > 0], na.rm = TRUE))
    if (!is.finite(theta_floor)) theta_floor <- 1e-6
    theta_s[!is.finite(theta_s) | theta_s <= 0] <- theta_floor

    for (s in seq_len(S)) {
      mu_s <- mu_mat[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- stats::rnbinom(sum(ok_mu), size = theta_s[s], mu = mu_s[ok_mu])
    }
  } else {
    stop("Family not handled: ", fam, call. = FALSE)
  }

  # Convert to incidence
  scale_fac <- pop / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- y_true / scale_fac

  # Finite-draw rule (prevents nonsense quantiles/CRPS)
  Ys_i[!is.finite(Ys_i)] <- NA
  thr <- max(min_draws, ceiling(min_prop * S))
  keep <- rowSums(is.finite(Ys_i)) >= thr

  n_test_used <- sum(keep)
  if (n_test_used == 0L) {
    return(tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test_raw = n_test_raw, n_test_obs = n_test_obs, n_test_used = 0L,
      COV80 = NA_real_, CRPS = NA_real_,
      note = "too_few_finite_pp_draws_after_simulation"
    ))
  }

  Ys_i <- Ys_i[keep, , drop = FALSE]
  y_true_i <- y_true_i[keep]

  q10 <- apply(Ys_i, 1, stats::quantile, probs = 0.10, na.rm = TRUE)
  q90 <- apply(Ys_i, 1, stats::quantile, probs = 0.90, na.rm = TRUE)

  tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    n_test_raw = n_test_raw,
    n_test_obs = n_test_obs,
    n_test_used = n_test_used,
    COV80 = mean(y_true_i >= q10 & y_true_i <= q90, na.rm = TRUE),
    CRPS = mean(crps_mc_safe(y_true_i, Ys_i), na.rm = TRUE),
    note = NA_character_
  )
}

inla_cov_crps_all_saved <- function(models, masks, folds = 1:3,
                                    data, out_dir = "runs",
                                    nsamp = 400, per = 1e5, seed = 1,
                                    eta_cap = 20) {
  jobs <- tidyr::crossing(model = models, mask_type = names(masks), fold = folds) %>%
    dplyr::mutate(
      model_i = as.integer(factor(model, levels = models)),
      mask_i = as.integer(factor(mask_type, levels = names(masks))),
      seed_job = seed + 10000L * model_i + 100L * mask_i + as.integer(fold)
    )

  rows <- purrr::pmap(
    jobs[c("model", "mask_type", "fold", "seed_job")],
    function(model, mask_type, fold, seed_job) {
      inla_cov_crps_fold_from_saved(
        model, mask_type, fold,
        data = data, out_dir = out_dir,
        nsamp = nsamp, per = per, seed = seed_job,
        eta_cap = eta_cap
      )
    }
  )

  rows <- purrr::keep(rows, ~ is.data.frame(.x) && nrow(.x) > 0)
  fold_df <- dplyr::bind_rows(rows)

  if (!nrow(fold_df)) {
    message("No fold results produced. Check *.rds and *_meta.rds in: ", file.path(out_dir, "inla_fits"))
    return(list(fold = tibble::tibble(), overall = tibble::tibble()))
  }

  overall_df <- fold_df %>%
    dplyr::group_by(model, mask_type) %>%
    dplyr::summarise(
      COV80 = safe_wmean(COV80, .data$n_test_used),
      CRPS = safe_wmean(CRPS, .data$n_test_used),
      n_test_raw = sum(n_test_raw, na.rm = TRUE),
      n_test_obs = sum(n_test_obs, na.rm = TRUE),
      n_test_used = sum(n_test_used, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mask_type, model)

  list(fold = fold_df, overall = overall_df)
}

# --------------------------------------------------------------------------
# WORKFLOW 4: Probabilistic metrics + annual-total rebasing (scaled INLA)
# --------------------------------------------------------------------------

# Rebase posterior predictive draws (counts) so each group sum equals target.
# Ys: N x S (counts), gidx: length N integer group ids, target: length G numeric.
rebase_draws_to_targets <- function(Ys, gidx, target) {
  N <- nrow(Ys)
  S <- ncol(Ys)
  G <- max(gidx)

  target <- as.numeric(target)
  target_ok <- is.finite(target) & !is.na(target) & target > 0

  Ys_reb <- matrix(NA_real_, nrow = N, ncol = S)
  bad_draws_per_group <- integer(G)

  for (s in seq_len(S)) {
    sums <- tapply(Ys[, s], gidx, sum, na.rm = TRUE)
    sums <- as.numeric(sums)

    f <- rep(NA_real_, G)
    ok <- target_ok & is.finite(sums) & sums > 0
    f[ok] <- target[ok] / sums[ok]

    bad_draws_per_group[target_ok & !ok] <- bad_draws_per_group[target_ok & !ok] + 1L

    Ys_reb[, s] <- Ys[, s] * f[gidx]
  }

  list(Ys_reb = Ys_reb, target_ok = target_ok, bad_draws_per_group = bad_draws_per_group)
}



inla_cov_crps_fold_from_saved_rebase <- function(
    model, mask_type, fold,
    subset_df, # already filtered to this mask_type/fold test rows
    out_dir = "runs",
    nsamp = 400, per = 1e5, seed = 1,
    group_cols = c("adm_0_name", "Year"),
    target_col = "annual_total",
    pop_col = "pop_est",
    y_col = "dengue_total",
    min_group_size = 12L,
    thr_min = 20L,
    thr_frac = 0.10) {
  # ---- small internal helper: fast sample CRPS per row (O(S log S)) ----
  crps_sample_row <- function(y, x) {
    x <- x[is.finite(x)]
    S <- length(x)
    if (!S) {
      return(NA_real_)
    }
    x <- sort(x)
    term1 <- mean(abs(x - y))
    # sum_{i<j} (x_j - x_i) = sum_j (2j - S - 1) x_j
    j <- seq_len(S)
    A <- sum((2 * j - S - 1) * x)
    term1 - (A / (S^2))
  }

  crps_sample_mat <- function(y, X) {
    vapply(seq_along(y), function(i) crps_sample_row(y[i], X[i, ]), numeric(1))
  }

  # ---- input checks ----
  req <- c("row_id", group_cols, target_col, pop_col, y_col)
  miss <- setdiff(req, names(subset_df))
  if (length(miss)) stop("subset_df missing columns: ", paste(miss, collapse = ", "))

  subset_df <- subset_df %>%
    dplyr::mutate(
      row_id = as.integer(.data$row_id),
      .pop = as.numeric(.data[[pop_col]]),
      .y = as.numeric(.data[[y_col]])
    )

  # ---- load fit + meta ----
  fp_fit <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold))
  fp_meta <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s_meta.rds", model, mask_type, fold))

  if (!file.exists(fp_fit) || !file.exists(fp_meta)) {
    return(tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test = 0L, n_test_used = 0L,
      COV80 = NA_real_, CRPS = NA_real_,
      n_test_used_rebase = 0L,
      COV80_rebase = NA_real_, CRPS_rebase = NA_real_,
      note = "missing_fit_or_meta"
    ))
  }

  fit <- readRDS(fp_fit)
  meta <- readRDS(fp_meta)

  if (is.null(meta$hold_df_idx) || is.null(meta$row_id)) {
    return(tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test = 0L, n_test_used = 0L,
      COV80 = NA_real_, CRPS = NA_real_,
      n_test_used_rebase = 0L,
      COV80_rebase = NA_real_, CRPS_rebase = NA_real_,
      note = "meta_missing_hold_df_idx_or_row_id"
    ))
  }

  hold_row_id <- if (!is.null(meta$hold_row_id)) meta$hold_row_id else meta$row_id[meta$hold_df_idx]
  hold_row_id <- as.integer(hold_row_id)

  # Map subset_df$row_id -> position within holdout rows
  pos_in_hold <- match(subset_df$row_id, hold_row_id)
  keep <- which(!is.na(pos_in_hold))

  if (!length(keep)) {
    return(tibble::tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test = 0L, n_test_used = 0L,
      COV80 = NA_real_, CRPS = NA_real_,
      n_test_used_rebase = 0L,
      COV80_rebase = NA_real_, CRPS_rebase = NA_real_,
      note = "row_id_mapping_failed"
    ))
  }

  subset_df <- subset_df[keep, , drop = FALSE]
  sel_df_idx <- meta$hold_df_idx[pos_in_hold[keep]] # Predictor indices

  N <- nrow(subset_df)
  S <- as.integer(nsamp)

  # ---- posterior predictive draws for selected predictors ----
  set.seed(seed)
  samp <- INLA::inla.posterior.sample(
    S, fit,
    seed = seed,
    selection = list(Predictor = sel_df_idx)
  )

  # latent -> mean counts
  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))
  eta_mat <- pmin(eta_mat, 30) # overflow guard
  mu_mat <- exp(eta_mat) # N x S

  # simulate observation model
  fam <- tolower(as.character(if (!is.null(fit$.args$family)) fit$.args$family else fit$family)[1])
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  if (grepl("pois", fam)) {
    for (s in seq_len(S)) Ys[, s] <- rpois(N, mu = mu_mat[, s])
  } else if (grepl("nb|neg", fam)) {
    # Prefer per-draw size/theta from posterior samples
    hp_names <- names(samp[[1]]$hyperpar)
    i_size <- grep("size.*Negative Binomial|nbinom|theta|size", hp_names, ignore.case = TRUE)

    if (length(i_size) >= 1L) {
      i_size <- i_size[1]
      theta_s <- vapply(samp, function(z) as.numeric(z$hyperpar[i_size]), numeric(1))
    } else {
      # fallback: summary.hyperpar mean
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

  # ---- incidence scale ----
  scale_fac <- subset_df$.pop / per
  y_true_i <- subset_df$.y / scale_fac

  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  Ys_i[!is.finite(Ys_i)] <- NA_real_

  thr <- max(as.integer(thr_min), as.integer(floor(thr_frac * S)))
  keep_raw <- rowSums(is.finite(Ys_i)) >= thr

  COV80 <- CRPS <- NA_real_
  n_used <- sum(keep_raw)

  if (n_used > 0) {
    q10 <- apply(Ys_i[keep_raw, , drop = FALSE], 1, stats::quantile, probs = 0.10, na.rm = TRUE)
    q90 <- apply(Ys_i[keep_raw, , drop = FALSE], 1, stats::quantile, probs = 0.90, na.rm = TRUE)

    yy <- y_true_i[keep_raw]
    COV80 <- mean(yy >= q10 & yy <= q90, na.rm = TRUE)

    crps_v <- crps_sample_mat(yy, Ys_i[keep_raw, , drop = FALSE])
    CRPS <- mean(crps_v, na.rm = TRUE)
  }

  # ---- draw-wise rebasing to annual targets (counts) ----
  # group index
  g <- interaction(subset_df[, group_cols], drop = TRUE)
  gidx <- as.integer(g)
  G <- max(gidx)

  # group sizes and targets
  g_size <- as.integer(table(g))
  names(g_size) <- levels(g)

  target <- tapply(subset_df[[target_col]], g, function(v) {
    u <- unique(stats::na.omit(v))
    if (!length(u)) {
      return(NA_real_)
    }
    as.numeric(u[1])
  })
  target <- as.numeric(target)
  target_ok <- is.finite(target) & !is.na(target) & target > 0
  size_ok <- g_size >= as.integer(min_group_size)
  group_ok <- target_ok & size_ok

  # Rebase: if a draw has sum==0 for a group, only that draw becomes NA for that group
  Ys_reb <- matrix(NA_real_, nrow = N, ncol = S)
  bad_draws_per_group <- integer(G)

  for (s in seq_len(S)) {
    sums <- tapply(Ys[, s], gidx, sum, na.rm = TRUE)
    sums <- as.numeric(sums)

    f <- rep(NA_real_, G)
    ok <- group_ok & is.finite(sums) & sums > 0
    f[ok] <- target[ok] / sums[ok]

    # count unscalable draws (sum==0 etc.) but only for otherwise-eligible groups
    bad_draws_per_group[group_ok & !ok] <- bad_draws_per_group[group_ok & !ok] + 1L

    Ys_reb[, s] <- Ys[, s] * f[gidx]
  }

  Ys_i_reb <- Ys_reb / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  Ys_i_reb[!is.finite(Ys_i_reb)] <- NA_real_

  # rows eligible for scoring: belong to eligible groups AND have enough finite draws
  row_group_ok <- group_ok[gidx]
  keep_reb <- row_group_ok & (rowSums(is.finite(Ys_i_reb)) >= thr)

  COV80_rebase <- CRPS_rebase <- NA_real_
  n_used_reb <- sum(keep_reb)

  if (n_used_reb > 0) {
    q10r <- apply(Ys_i_reb[keep_reb, , drop = FALSE], 1, stats::quantile, probs = 0.10, na.rm = TRUE)
    q90r <- apply(Ys_i_reb[keep_reb, , drop = FALSE], 1, stats::quantile, probs = 0.90, na.rm = TRUE)

    yy <- y_true_i[keep_reb]
    COV80_rebase <- mean(yy >= q10r & yy <= q90r, na.rm = TRUE)

    crps_vr <- crps_sample_mat(yy, Ys_i_reb[keep_reb, , drop = FALSE])
    CRPS_rebase <- mean(crps_vr, na.rm = TRUE)
  }

  note <- NA_character_
  if (any(!target_ok)) note <- "some_groups_missing_or_invalid_target"
  if (is.na(note) && any(!size_ok)) note <- "some_groups_below_min_group_size"
  if (is.na(note) && any(bad_draws_per_group > 0)) note <- "some_draws_unscalable_due_to_zero_year_sum"

  tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    n_test = N, n_test_used = as.integer(n_used),
    COV80 = COV80, CRPS = CRPS,
    n_test_used_rebase = as.integer(n_used_reb),
    COV80_rebase = COV80_rebase, CRPS_rebase = CRPS_rebase,
    note = note
  )
}


inla_cov_crps_all_saved_rebase <- function(models, masks, folds = 1:3,
                                           data, out_dir = "runs",
                                           nsamp = 400, per = 1e5, seed = 1,
                                           train_policy = "inclusive",
                                           group_cols = NULL,
                                           target_col = "annual_total",
                                           pop_col = "pop_est",
                                           y_col = "dengue_total",
                                           min_group_size = 12L,
                                           thr_min = 20L,
                                           thr_frac = 0.10) {
  # Choose default grouping if not supplied
  if (is.null(group_cols)) {
    if (all(c("adm_0_name", "Year") %in% names(data))) {
      group_cols <- c("adm_0_name", "Year")
    } else if (all(c("country", "year") %in% names(data))) {
      group_cols <- c("country", "year")
    } else {
      stop("group_cols not provided and could not infer (need adm_0_name+Year or country+year).")
    }
  }

  # Basic checks
  need <- unique(c("row_id", group_cols, target_col, pop_col, y_col))
  if (!"row_id" %in% names(data)) data <- dplyr::mutate(data, row_id = dplyr::row_number())
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("data missing required columns: ", paste(miss, collapse = ", "))

  # Build manifest (jobs)
  man <- build_manifest(models, masks, folds, out_dir = out_dir) %>%
    dplyr::mutate(seed_job = seed + (fold - 1L) + dplyr::dense_rank(paste(model, mask_type)) * 1000L)

  fold_res <- purrr::pmap_dfr(
    man[, c("model", "mask_type", "fold", "seed_job")],
    function(model, mask_type, fold, seed_job) {
      mask <- masks[[mask_type]]
      sp <- get_fold_split(mask, fold_id = fold, train_policy = train_policy)

      if (!length(sp$test_idx)) {
        return(tibble::tibble(
          model = model, mask_type = mask_type, fold = fold,
          n_test = 0L, n_test_used = 0L,
          COV80 = NA_real_, CRPS = NA_real_,
          n_test_used_rebase = 0L,
          COV80_rebase = NA_real_, CRPS_rebase = NA_real_,
          note = "empty_test_set"
        ))
      }

      subset_df <- data[sp$test_idx, , drop = FALSE]

      inla_cov_crps_fold_from_saved_rebase(
        model = model, mask_type = mask_type, fold = fold,
        subset_df = subset_df,
        out_dir = out_dir,
        nsamp = nsamp, per = per, seed = seed_job,
        group_cols = group_cols,
        target_col = target_col,
        pop_col = pop_col,
        y_col = y_col,
        min_group_size = min_group_size,
        thr_min = thr_min,
        thr_frac = thr_frac
      )
    }
  )

  overall <- fold_res %>%
    dplyr::group_by(model, mask_type) %>%
    dplyr::summarise(
      COV80 = safe_wmean(COV80, .data$n_test_used),
      CRPS = safe_wmean(CRPS, .data$n_test_used),
      COV80_rebase = safe_wmean(COV80_rebase, .data$n_test_used_rebase),
      CRPS_rebase = safe_wmean(CRPS_rebase, .data$n_test_used_rebase),
      n_test = sum(n_test, na.rm = TRUE),
      n_test_used = sum(n_test_used, na.rm = TRUE),
      n_test_used_rebase = sum(n_test_used_rebase, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mask_type, model)

  list(fold = fold_res, overall = overall)
}

add_rebased_point_predictions <- function(res_point_df,
                                          data_df,
                                          group_cols,
                                          target_col,
                                          pred_cols = c("pred_mean", "pred_median"),
                                          clamp_nonneg = TRUE,
                                          min_group_size = 12L,
                                          suffix = "_rebase") {
  stopifnot("row_id" %in% names(res_point_df))
  stopifnot("row_id" %in% names(data_df))
  stopifnot(all(group_cols %in% names(data_df)))
  stopifnot(target_col %in% names(data_df))

  # Join group + target onto predictions
  p <- res_point_df %>%
    dplyr::left_join(
      data_df %>% dplyr::select(dplyr::all_of(c("row_id", group_cols, target_col))),
      by = "row_id"
    )

  # Build group key (e.g. adm_0_name + Year)
  p <- p %>%
    dplyr::mutate(.grp = interaction(dplyr::across(dplyr::all_of(group_cols)), drop = TRUE))

  # group sizes (to avoid rebasing partial years etc.)
  ginfo <- p %>%
    dplyr::group_by(.grp) %>%
    dplyr::summarise(
      .n = dplyr::n(),
      .target = {
        u <- unique(stats::na.omit(.data[[target_col]]))
        if (length(u)) as.numeric(u[1]) else NA_real_
      },
      .groups = "drop"
    )

  p <- p %>%
    dplyr::left_join(ginfo, by = ".grp")

  # Rebase each prediction column separately
  for (col in pred_cols) {
    if (!col %in% names(p)) next

    raw <- p[[col]]
    if (clamp_nonneg) raw <- pmax(raw, 0)

    # sum of predictions per group
    sum_by_g <- tapply(raw, p$.grp, sum, na.rm = TRUE)
    # factor per group
    f_by_g <- ginfo$.target / sum_by_g[ginfo$.grp]

    # invalid / unsafe cases => no rebasing
    invalid_g <- !is.finite(f_by_g) | f_by_g <= 0 | !is.finite(ginfo$.target) | ginfo$.target <= 0 | ginfo$.n < min_group_size
    f_by_g[invalid_g] <- NA_real_

    f <- f_by_g[match(p$.grp, ginfo$.grp)]
    rebased <- raw * f

    # If factor NA, fall back to original predictions
    rebased[is.na(f)] <- p[[col]][is.na(f)]

    p[[paste0(col, suffix)]] <- rebased
  }

  # optional: keep a rebasing factor for debugging
  p$rebase_factor <- {
    # compute from pred_mean if available else pred_median
    base_col <- if ("pred_mean" %in% names(p)) "pred_mean" else if ("pred_median" %in% names(p)) "pred_median" else NA_character_
    if (is.na(base_col)) {
      NA_real_
    } else {
      base_raw <- p[[base_col]]
      if (clamp_nonneg) base_raw <- pmax(base_raw, 0)
      sum_by_g <- tapply(base_raw, p$.grp, sum, na.rm = TRUE)
      f_by_g <- ginfo$.target / sum_by_g[ginfo$.grp]
      f_by_g[!is.finite(f_by_g) | f_by_g <= 0 | ginfo$.n < min_group_size] <- NA_real_
      f_by_g[match(p$.grp, ginfo$.grp)]
    }
  }

  p %>% dplyr::select(-.grp, -.n, -.target)
}


# --------------------------------------------------------------------------
# Diagnostics: verify row_id alignment between meta files and current data
# --------------------------------------------------------------------------
check_inla_data_fit_alignment <- function(data, out_dir, n_spotcheck = 5) {
  fit_dir <- file.path(out_dir, "inla_fits")
  meta_files <- list.files(fit_dir, pattern = "_meta\\.rds$", full.names = TRUE)

  if (length(meta_files) == 0) stop("No *_meta.rds files found under: ", fit_dir, call. = FALSE)

  if (!"row_id" %in% names(data)) stop("data$row_id is missing. Create it once after arranging data.", call. = FALSE)
  if (anyNA(data$row_id)) stop("data$row_id has NA values.", call. = FALSE)
  if (anyDuplicated(data$row_id) > 0) stop("data$row_id has duplicates.", call. = FALSE)

  read_meta <- function(fp) {
    m <- readRDS(fp)
    if (is.null(m$hold_df_idx) || is.null(m$row_id)) stop("Meta file missing hold_df_idx or row_id: ", fp, call. = FALSE)
    m
  }

  res <- lapply(meta_files, function(mf) {
    meta <- read_meta(mf)
    test_row_ids <- meta$row_id[meta$hold_df_idx]
    idx <- match(test_row_ids, data$row_id)
    data.frame(
      meta_file = basename(mf),
      n_test = length(test_row_ids),
      n_missing = sum(is.na(idx)),
      ok = (sum(is.na(idx)) == 0),
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, res)

  bad <- res[!res$ok, , drop = FALSE]
  if (nrow(bad) > 0) {
    print(bad)
    stop("Row_id mismatch detected in ", nrow(bad), " meta files. Do not compute metrics until fixed.", call. = FALSE)
  }

  cat("\nAlignment check passed: all ", nrow(res), " meta files match data$row_id.\n", sep = "")

  if (n_spotcheck > 0) {
    set.seed(1)
    mf <- sample(meta_files, 1)
    meta <- read_meta(mf)
    test_row_ids <- meta$row_id[meta$hold_df_idx]
    idx <- match(test_row_ids, data$row_id)

    k <- min(n_spotcheck, length(idx))
    sel <- sample(seq_along(idx), k)

    cols <- intersect(c("adm_0_name", "time_seq", "Year", "week", "month", "country", "year"), names(data))
    spot <- data[idx[sel], c("row_id", cols), drop = FALSE]

    cat("\nSpot-check from: ", basename(mf), "\n", sep = "")
    print(spot)
  }

  invisible(res)
}




check_one_fit_with_rebase <- function(model, mask_type, fold, out_dir, data,
                                      nsamp_check = 30, seed = 1,
                                      group_cols = c("adm_0_name", "Year"),
                                      target_col = "annual_total",
                                      min_group_size = 12,
                                      n_groups_check = 3,
                                      rel_tol = 1e-6) {
  fp_fit <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold))
  fp_meta <- file.path(out_dir, "inla_fits", sprintf("%s_%s_fold%s_meta.rds", model, mask_type, fold))

  exists_fit <- file.exists(fp_fit)
  exists_meta <- file.exists(fp_meta)

  base_row <- data.frame(
    model = model, mask_type = mask_type, fold = fold,
    fit_path = fp_fit, meta_path = fp_meta,
    exists_fit = exists_fit, exists_meta = exists_meta,
    ok_read = FALSE,
    has_hyperpar = FALSE, hyperpar_all_finite = FALSE,
    rho_boundary_flag = NA, rho_means = NA_character_,
    can_check_rebase = FALSE,
    rebase_groups_checked = 0L,
    rebase_draws_checked = 0L,
    rebase_frac_bad_factor = NA_real_,
    rebase_max_abs_err = NA_real_,
    rebase_max_rel_err = NA_real_,
    rebase_factor_q = NA_character_,
    note = NA_character_,
    ok = FALSE,
    stringsAsFactors = FALSE
  )

  if (!exists_fit || !exists_meta) {
    base_row$note <- "missing_fit_or_meta"
    return(base_row)
  }

  fit <- tryCatch(readRDS(fp_fit), error = function(e) e)
  meta <- tryCatch(readRDS(fp_meta), error = function(e) e)
  if (inherits(fit, "error") || inherits(meta, "error")) {
    base_row$note <- "read_fit_or_meta_error"
    return(base_row)
  }
  base_row$ok_read <- TRUE

  # ---------------- Hyperparameter sanity ----------------
  shp <- fit$summary.hyperpar
  has_hyperpar <- !is.null(shp) && nrow(shp) > 0
  base_row$has_hyperpar <- has_hyperpar

  if (has_hyperpar) {
    cols <- intersect(c("mean", "sd", "0.025quant", "0.5quant", "0.975quant"), colnames(shp))
    if (length(cols)) {
      vals <- as.matrix(shp[, cols, drop = FALSE])
      base_row$hyperpar_all_finite <- all(is.finite(vals))
      rho_rows <- grep("rho", rownames(shp), ignore.case = TRUE)
      if (length(rho_rows) && "mean" %in% colnames(shp)) {
        rhos <- as.numeric(shp[rho_rows, "mean"])
        base_row$rho_means <- paste(round(rhos, 3), collapse = "; ")
        base_row$rho_boundary_flag <- any(abs(rhos) > 0.999, na.rm = TRUE)
      }
    } else {
      base_row$note <- "summary.hyperpar_has_no_standard_cols"
    }
  } else {
    base_row$note <- "missing_summary.hyperpar"
  }

  # ---------------- Rebase check prerequisites ----------------
  needed <- c("row_id", group_cols, target_col)
  if (!all(needed %in% names(data))) {
    base_row$note <- paste0(
      "cannot_check_rebase_missing_cols: ",
      paste(setdiff(needed, names(data)), collapse = ", ")
    )
    base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
    return(base_row)
  }
  if (is.null(meta$hold_df_idx) || is.null(meta$row_id)) {
    base_row$note <- "cannot_check_rebase_missing_meta_fields"
    base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
    return(base_row)
  }

  # Holdout row_ids and mapping into data
  hold_row_id <- meta$row_id[meta$hold_df_idx]
  hold_row_id <- as.integer(hold_row_id)

  idx <- match(hold_row_id, data$row_id)
  if (anyNA(idx)) {
    base_row$note <- paste0(
      "row_id_mismatch_meta_vs_data: missing=",
      sum(is.na(idx)), "/", length(idx)
    )
    base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
    return(base_row)
  }
  hold_df <- data[idx, , drop = FALSE]

  # ---------------- FIXED grouping logic ----------------
  g <- do.call(interaction, c(hold_df[group_cols], list(drop = TRUE)))

  tab <- table(g) # keep names here!
  levs <- names(tab)
  gsizes <- as.integer(tab)

  target_by_g <- tapply(hold_df[[target_col]], g, function(v) {
    u <- unique(stats::na.omit(v))
    if (!length(u)) {
      return(NA_real_)
    }
    as.numeric(u[1])
  })
  target_vec <- as.numeric(target_by_g[levs])

  eligible <- levs[gsizes >= min_group_size & is.finite(target_vec) & target_vec > 0]

  if (!length(eligible)) {
    base_row$note <- "no_eligible_groups_for_rebase_check_in_holdout"
    base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
    return(base_row)
  }

  set.seed(seed)
  chosen <- sample(eligible, size = min(n_groups_check, length(eligible)), replace = FALSE)
  keep <- which(as.character(g) %in% chosen)

  sub_df <- hold_df[keep, , drop = FALSE]

  sub_g <- do.call(interaction, c(sub_df[group_cols], list(drop = TRUE)))
  sub_g <- droplevels(sub_g)
  sub_gidx <- as.integer(sub_g)
  G <- nlevels(sub_g)

  # Map subset rows -> Predictor indices for posterior sampling
  pos_in_hold <- match(sub_df$row_id, hold_row_id)
  if (anyNA(pos_in_hold)) {
    base_row$note <- "row_id_mapping_failed_for_rebase_check"
    base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
    return(base_row)
  }
  sel_df_idx <- meta$hold_df_idx[pos_in_hold]

  # targets aligned to subgroup levels
  target_sub <- tapply(sub_df[[target_col]], sub_g, function(v) {
    u <- unique(stats::na.omit(v))
    if (!length(u)) {
      return(NA_real_)
    }
    as.numeric(u[1])
  })
  target <- as.numeric(target_sub[levels(sub_g)])
  target_ok <- is.finite(target) & target > 0

  # ---------------- Posterior sampling ----------------
  set.seed(seed)
  samp <- INLA::inla.posterior.sample(
    nsamp_check, fit,
    seed = seed,
    selection = list(Predictor = sel_df_idx)
  )

  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))
  eta_mat <- pmin(eta_mat, 30)
  mu_mat <- exp(eta_mat)

  fam <- tolower(as.character(if (!is.null(fit$.args$family)) fit$.args$family else fit$family)[1])
  Ys <- matrix(NA_real_, nrow = nrow(sub_df), ncol = nsamp_check)

  if (grepl("pois", fam)) {
    for (s in seq_len(nsamp_check)) Ys[, s] <- rpois(nrow(sub_df), mu = mu_mat[, s])
  } else if (grepl("nb|neg", fam)) {
    hp_names <- names(samp[[1]]$hyperpar)
    i_size <- grep("size.*Negative Binomial|nbinom|theta|size", hp_names, ignore.case = TRUE)
    if (!length(i_size)) {
      base_row$note <- "nb_size_hyperpar_not_found_in_posterior_samples"
      base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
      return(base_row)
    }
    i_size <- i_size[1]
    theta_s <- vapply(samp, function(z) as.numeric(z$hyperpar[i_size]), numeric(1))
    theta_s[!is.finite(theta_s) | theta_s <= 0] <- 1e-6
    for (s in seq_len(nsamp_check)) Ys[, s] <- rnbinom(nrow(sub_df), size = theta_s[s], mu = mu_mat[, s])
  } else {
    base_row$note <- paste0("family_not_supported_for_rebase_check: ", fam)
    base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
    return(base_row)
  }

  # ---------------- Rebase per draw; check sums hit targets ----------------
  bad_factor <- 0L
  max_abs_err <- 0
  max_rel_err <- 0
  all_factors <- numeric(0)

  for (s in seq_len(nsamp_check)) {
    sums <- tapply(Ys[, s], sub_gidx, sum, na.rm = TRUE)
    sums <- as.numeric(sums)

    f <- rep(NA_real_, G)
    okf <- target_ok & is.finite(sums) & sums > 0
    f[okf] <- target[okf] / sums[okf]

    bad_factor <- bad_factor + sum(target_ok & !okf, na.rm = TRUE)
    all_factors <- c(all_factors, f[is.finite(f)])

    Ys_reb <- Ys[, s] * f[sub_gidx]
    reb_sums <- tapply(Ys_reb, sub_gidx, sum, na.rm = TRUE)

    if (any(okf)) {
      abs_err <- abs(reb_sums[okf] - target[okf])
      rel_err <- abs_err / pmax(target[okf], 1e-12)
      max_abs_err <- max(max_abs_err, max(abs_err, na.rm = TRUE))
      max_rel_err <- max(max_rel_err, max(rel_err, na.rm = TRUE))
    }
  }

  base_row$can_check_rebase <- TRUE
  base_row$rebase_groups_checked <- G
  base_row$rebase_draws_checked <- nsamp_check

  denom <- sum(target_ok) * nsamp_check
  base_row$rebase_frac_bad_factor <- if (denom > 0) bad_factor / denom else NA_real_
  base_row$rebase_max_abs_err <- max_abs_err
  base_row$rebase_max_rel_err <- max_rel_err

  if (length(all_factors)) {
    qs <- stats::quantile(all_factors, probs = c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE)
    base_row$rebase_factor_q <- paste(names(qs), round(as.numeric(qs), 4), sep = "=", collapse = "; ")
  }

  base_row$note <- if (isTRUE(base_row$rebase_max_rel_err <= rel_tol)) "rebase_ok" else "rebase_warn"
  base_row$ok <- isTRUE(base_row$has_hyperpar) && isTRUE(base_row$hyperpar_all_finite)
  base_row
}
