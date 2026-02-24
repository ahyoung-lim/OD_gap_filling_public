# ============================================================================
# INLA Model Performance Evaluation Functions
# ============================================================================

# ---- Shared Helper Functions (Top-Level Only) ----

# Standard helpers
mae_vec <- function(p, y) mean(abs(p - y), na.rm = TRUE)
rmse_vec <- function(p, y) sqrt(mean((p - y)^2, na.rm = TRUE))



# CRPS calculation (safe version with NA handling)
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

# Read mask object from file or use as-is
read_mask_obj <- function(x) {
  if (is.character(x) && length(x) == 1L) readRDS(x) else x
}

# Read saved artifacts from disk
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


# ============================================================================
# WORKFLOW 1: PREDICTION GENERATION & CACHING
# ============================================================================

# ----------------------------------------------------------------------------
# run_job_inla_cached
# Purpose: Fit INLA model, generate and cache predictions for ALL test observations
# Use: Run once to generate cached predictions and save fitted models
# Output: Used by make_metrics_counts_incidence_prob for MAE/RMSE
# ----------------------------------------------------------------------------
run_job_inla_cached <- function(model, mask_type, fold,
                                data, masks, formulas, ctrl_fam,
                                train_policy = "inclusive",
                                nsamp = 400, per = 1e5,
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
    point <- tibble::tibble(model, mask_type, fold, row_id = integer(0), pred = numeric(0))
    prob <- tibble::tibble(model, mask_type, fold, n_test = 0, cov80 = NA_real_)
    readr::write_csv(point, fp_point)
    readr::write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  train <- data[sp$train_idx, ] %>% dplyr::mutate(y = dengue_total)
  test <- data[sp$test_idx, ] %>% dplyr::mutate(y = NA_real_)
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

  # Save fit and metadata
  saveRDS(fit, fp_fit)
  saveRDS(list(hold_df_idx = which(is.na(df$y)), row_id = df$row_id), fp_meta)

  hold_df_idx <- readRDS(fp_meta)$hold_df_idx

  # Point predictions
  point <- tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    row_id = df$row_id[hold_df_idx],
    pred_mean = fit$summary.fitted.values$mean[hold_df_idx],
    pred_median = fit$summary.fitted.values$`0.5quant`[hold_df_idx],
    pred = pred_median
  )
  readr::write_csv(point, fp_point)

  # Posterior predictive draws
  samp <- INLA::inla.posterior.sample(nsamp, fit,
    seed = seed,
    selection = list(Predictor = hold_df_idx)
  )
  S <- length(samp)
  N <- length(hold_df_idx)

  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))
  mu_mat <- exp(eta_mat)

  Ys <- matrix(NA_real_, nrow = N, ncol = S)
  fam <- tolower(ctrl_fam$family)
  if (grepl("pois", fam)) {
    for (s in seq_len(S)) Ys[, s] <- rpois(N, mu = mu_mat[, s])
  } else if (grepl("nb", fam)) {
    theta_row <- grep("size|theta", rownames(fit$summary.hyperpar), ignore.case = TRUE)
    stopifnot(length(theta_row) == 1L)
    theta <- as.numeric(fit$summary.hyperpar[theta_row, "mean"])
    for (s in seq_len(S)) Ys[, s] <- rnbinom(N, size = theta, mu = mu_mat[, s])
  } else {
    stop("Family not handled: ", ctrl_fam$family)
  }

  # Convert to incidence
  scale_fac <- data$pop_est[df$row_id[hold_df_idx]] / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- data$dengue_total[df$row_id[hold_df_idx]] / scale_fac

  # Quantiles
  q_med <- apply(Ys_i, 1, stats::quantile, probs = 0.5, na.rm = TRUE)
  q80 <- apply(Ys_i, 1, stats::quantile, probs = c(0.10, 0.90), na.rm = TRUE)

  prob <- tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    n_test = length(y_true_i)
    # cov80 = mean(y_true_i >= q80[1, ] & y_true_i <= q80[2, ], na.rm = TRUE)
    # WIS80 = mean(wis80_from_q(y_true_i, q_med, q80[1, ], q80[2, ]), na.rm = TRUE)
  )
  readr::write_csv(prob, fp_prob)

  list(point = point, prob = prob, fit_path = fp_fit)
}


# ============================================================================
# WORKFLOW 2: RUNNER FUNCTIONS (Sequential/Parallel Alternatives)
# ============================================================================

# ----------------------------------------------------------------------------
# run_inla_sequential
# Purpose: Run jobs sequentially with progress bar
# Use: For local R sessions
# ----------------------------------------------------------------------------
run_inla_sequential <- function(data, masks, formulas, ctrl_fam,
                                folds = 1:3, out_dir = "runs",
                                nsamp = 400, seed = 20250811) {
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% dplyr::filter(!is_done)
  message(glue::glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))

  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  pb <- progress::progress_bar$new(
    total = nrow(todo),
    format = "INLA [:bar] :current/:total (:percent) ETA: :eta | :model/:mask f=:fold",
    clear = FALSE, show_after = 0
  )

  res_list <- purrr::pmap(
    todo[c("model", "mask_type", "fold")],
    function(model, mask_type, fold) {
      cat(sprintf(
        "[%s] start %s | %s | fold=%s\n",
        format(Sys.time(), "%H:%M:%S"), model, mask_type, fold
      ))
      flush.console()

      out <- run_job_inla_cached(
        model = model, mask_type = mask_type, fold = fold,
        data = data, masks = masks, formulas = formulas, ctrl_fam = ctrl_fam,
        nsamp = nsamp, seed = seed, out_dir = out_dir
      )

      pb$tick(tokens = list(model = model, mask = mask_type, fold = fold))
      cat(sprintf(
        "[%s] done  %s | %s | fold=%s  (saved)\n",
        format(Sys.time(), "%H:%M:%S"), model, mask_type, fold
      ))
      flush.console()
      out
    }
  )
  invisible(res_list)
}

# ----------------------------------------------------------------------------
# run_inla_parallel
# Purpose: Run jobs in parallel with progress bar
# Use: For local R sessions with multiple cores
# ----------------------------------------------------------------------------
run_inla_parallel <- function(data, masks, formulas, ctrl_fam,
                              folds = 1:3, out_dir = "runs",
                              workers = 8, nsamp = 400, seed = 20250811) {
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% dplyr::filter(!is_done)
  message(glue::glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))

  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  future::plan(future::multisession, workers = workers)
  progressr::handlers(global = TRUE)
  progressr::handlers("cli")

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(todo))
    res_list <- furrr::future_pmap(
      todo[c("model", "mask_type", "fold")],
      function(model, mask_type, fold) {
        p(sprintf("%s / %s f=%s", model, mask_type, fold))
        run_job_inla_cached(
          model = model, mask_type = mask_type, fold = fold,
          data = data, masks = masks, formulas = formulas, ctrl_fam = ctrl_fam,
          nsamp = nsamp, seed = seed, out_dir = out_dir
        )
      },
      .options = furrr::furrr_options(seed = seed)
    )
    res_list
  })
}

# ----------------------------------------------------------------------------
# run_inla_sequential_jupyter
# Purpose: Run jobs sequentially with Jupyter-compatible progress
# Use: For Jupyter notebooks
# ----------------------------------------------------------------------------
run_inla_sequential_jupyter <- function(data, masks, formulas, ctrl_fam,
                                        folds = 1:3, out_dir = "runs",
                                        nsamp = 400, seed = 20250811) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% dplyr::filter(!is_done)
  message(glue::glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))

  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  pb <- progress::progress_bar$new(
    total = nrow(todo),
    format = "INLA [:bar] :current/:total (:percent) ETA: :eta | :model/:mask f=:fold",
    clear = FALSE, show_after = 0
  )

  res_list <- purrr::pmap(
    todo[c("model", "mask_type", "fold")],
    function(model, mask_type, fold) {
      cat(sprintf(
        "[%s] start %s | %s | fold=%s\n",
        format(Sys.time(), "%H:%M:%S"), model, mask_type, fold
      ))
      flush.console()

      out <- run_job_inla_cached(
        model = model, mask_type = mask_type, fold = fold,
        data = data, masks = masks, formulas = formulas, ctrl_fam = ctrl_fam,
        nsamp = nsamp, seed = seed, out_dir = out_dir
      )

      pb$tick(tokens = list(model = model, mask = mask_type, fold = fold))
      cat(sprintf(
        "[%s] done  %s | %s | fold=%s  (saved)\n",
        format(Sys.time(), "%H:%M:%S"), model, mask_type, fold
      ))
      flush.console()
      out
    }
  )
  invisible(res_list)
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
          nsamp = nsamp, seed = seed, out_dir = out_dir
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

# ----------------------------------------------------------------------------
# WORKFLOW 3: POINT METRICS (MAE/RMSE)
# ----------------------------------------------------------------------------
make_metrics_counts_incidence_prob <- function(res_point_df,
                                               data_df, per = 1e5, pop_floor = 1,
                                               pred_col = "pred",
                                               masks = NULL, warn_only = TRUE) {
  stopifnot(is.data.frame(res_point_df) || inherits(res_point_df, "tbl"))

  # Ensure stable row_id in data_df
  if (!"row_id" %in% names(data_df)) {
    data_df <- dplyr::mutate(data_df, row_id = dplyr::row_number())
  } else {
    if (anyNA(data_df$row_id)) stop("data_df$row_id contains NA values.", call. = FALSE)
    if (anyDuplicated(data_df$row_id) > 0) stop("data_df$row_id contains duplicates.", call. = FALSE)
  }

  if (!pred_col %in% names(res_point_df)) {
    stop("pred_col '", pred_col, "' not found in res_point_df.")
  }

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

# ----------------------------------------------------------------------------
# WORKFLOW 4: PROBABILISTIC METRICS (COV80/CRPS)
# ----------------------------------------------------------------------------

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

  # Stable row_id in 'data'
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

  # Rows where incidence is defined (GAM-style)
  ok <- is.finite(y_true) & !is.na(y_true) &
    is.finite(pop) & !is.na(pop) & pop > 0

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

  # Posterior samples of Predictor (held-out rows only)
  samp <- INLA::inla.posterior.sample(
    nsamp, fit,
    seed = seed,
    selection = list(Predictor = hold_ok)
  )

  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))

  # Clamp eta before exp()
  eta_mat[eta_mat > eta_cap] <- eta_cap
  eta_mat[eta_mat < -eta_cap] <- -eta_cap
  mu_mat <- exp(eta_mat)

  N <- nrow(mu_mat)
  S <- ncol(mu_mat)
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  fam <- tolower(if (!is.null(fit$.args$family)) fit$.args$family else fit$family)

  if (grepl("pois", fam)) {
    for (s in seq_len(S)) {
      mu_s <- mu_mat[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- stats::rpois(sum(ok_mu), lambda = mu_s[ok_mu])
    }
  } else if (grepl("nb", fam)) {
    shp <- fit$summary.hyperpar
    theta_row <- grep("size|theta", rownames(shp), ignore.case = TRUE)
    if (length(theta_row) < 1L) stop("Could not find NB size/theta in summary.hyperpar.", call. = FALSE)

    theta <- as.numeric(shp[theta_row[1], "mean"])
    if (!is.finite(theta) || theta <= 0) theta <- 1e-6

    for (s in seq_len(S)) {
      mu_s <- mu_mat[, s]
      ok_mu <- is.finite(mu_s) & mu_s >= 0
      if (any(ok_mu)) Ys[ok_mu, s] <- stats::rnbinom(sum(ok_mu), size = theta, mu = mu_s[ok_mu])
    }
  } else {
    stop("Family not handled: ", fam, call. = FALSE)
  }

  # Convert to incidence
  scale_fac <- pop / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- y_true / scale_fac

  # Robust finite-draw rule (same spirit as GAM)
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
      # use the per-fold weights (vector) BEFORE any sums
      COV80 = safe_wmean(COV80, .data$n_test_used),
      CRPS = safe_wmean(CRPS, .data$n_test_used),

      # now totals (scalars) are safe to compute
      n_test_raw = sum(n_test_raw, na.rm = TRUE),
      n_test_obs = sum(n_test_obs, na.rm = TRUE),
      n_test_used = sum(n_test_used, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mask_type, model)


  list(fold = fold_df, overall = overall_df)
}










check_inla_data_fit_alignment <- function(data, out_dir, n_spotcheck = 5) {
  fit_dir <- file.path(out_dir, "inla_fits")
  meta_files <- list.files(fit_dir, pattern = "_meta\\.rds$", full.names = TRUE)

  if (length(meta_files) == 0) {
    stop("No *_meta.rds files found under: ", fit_dir)
  }

  # 1) data row_id sanity
  if (!"row_id" %in% names(data)) {
    stop("data$row_id is missing. Create it once after arranging data.")
  }
  if (anyNA(data$row_id)) stop("data$row_id has NA values.")
  if (anyDuplicated(data$row_id) > 0) stop("data$row_id has duplicates.")

  # helper to read meta safely
  read_meta <- function(fp) {
    m <- readRDS(fp)
    if (is.null(m$hold_df_idx) || is.null(m$row_id)) {
      stop("Meta file missing hold_df_idx or row_id: ", fp)
    }
    m
  }

  # 2) check match coverage per job
  results <- lapply(meta_files, function(mf) {
    meta <- read_meta(mf)
    test_row_ids <- meta$row_id[meta$hold_df_idx]
    idx <- match(test_row_ids, data$row_id)
    n_total <- length(test_row_ids)
    n_miss <- sum(is.na(idx))

    data.frame(
      meta_file = basename(mf),
      n_test = n_total,
      n_missing = n_miss,
      ok = (n_miss == 0),
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, results)

  # stop fast if any mismatch
  bad <- res[!res$ok, ]
  if (nrow(bad) > 0) {
    print(bad)
    stop(
      "Row_id mismatch detected in ", nrow(bad), " meta files. ",
      "Do not compute metrics until fixed."
    )
  }

  cat("✅ Alignment check passed: all ", nrow(res), " meta files match data$row_id.\n")

  # 3) optional spot-check: print a few matched rows from a random meta file
  if (n_spotcheck > 0) {
    set.seed(1)
    mf <- sample(meta_files, 1)
    meta <- read_meta(mf)
    test_row_ids <- meta$row_id[meta$hold_df_idx]
    idx <- match(test_row_ids, data$row_id)

    k <- min(n_spotcheck, length(idx))
    sel <- sample(seq_along(idx), k)

    cols <- intersect(c("adm_0_name", "time_seq", "Year", "week", "month"), names(data))
    spot <- data[idx[sel], c("row_id", cols), drop = FALSE]

    cat("\nSpot-check from: ", basename(mf), "\n", sep = "")
    print(spot)
  }

  invisible(res)
}
