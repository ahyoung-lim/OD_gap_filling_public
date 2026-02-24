# ============================================================================
# GAM Model Performance Evaluation Functions
# ============================================================================

# ---- Shared Helper Functions (Top-Level Only) ----

# Tiny helpers - UNUSED, kept for potential future use
# .ε <- 1e-8
# mae_vec <- function(p, y) mean(abs(p - y), na.rm = TRUE)
# rmse_vec <- function(p, y) sqrt(mean((p - y)^2, na.rm = TRUE))

# Standard WIS80 calculation - COMMENTED OUT, not currently needed
# wis80_from_q <- function(y, m, L, U) {
#   a <- 0.2
#   w0 <- 0.5
#   w1 <- a / 2
#   denom <- w0 + w1
#   IS <- (U - L) + (2 / a) * pmax(L - y, 0) + (2 / a) * pmax(y - U, 0)
#   (w0 * abs(y - m) + w1 * IS) / denom
# }

# CRPS calculation via Monte Carlo
crps_mc <- function(y, S_mat) {
  a <- rowMeans(abs(S_mat - y))
  b <- apply(S_mat, 1, function(s) {
    s <- sort(s)
    k <- seq_along(s)
    S <- length(s)
    (2 / (S^2)) * sum((2 * k - S - 1) * s)
  })
  a - 0.5 * b
}

# Safe CRPS calculation with NA handling
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

# Get fold split from mask
get_split <- function(mask, fold, policy = "inclusive") {
  mask <- read_mask_obj(mask)
  get_fold_split(mask, fold_id = fold, train_policy = policy)
}

# Extract NB theta parameter from fitted GAM
get_nb_theta_from_fit <- function(fit) {
  raw <- c(
    tryCatch(fit$family$getTheta(), error = function(e) NA_real_),
    suppressWarnings(as.numeric(fit$family$theta)),
    tryCatch(get("theta", environment(fit$family$variance)), error = function(e) NA_real_),
    tryCatch(fit$family$mgcv.theta, error = function(e) NA_real_)
  )
  raw <- raw[is.finite(raw)]
  candidates <- c(raw[raw > 0], exp(raw[raw > 0 | raw <= 0]))
  theta <- candidates[is.finite(candidates) & candidates > 0][1]
  if (!is.finite(theta)) stop("Could not recover a valid NB θ from the saved fit.")
  theta
}

# Coerce time-related columns to numeric
coerce_time_numerics <- function(df) {
  for (nm in c("year", "yearx", "time_seq", "week", "month", "month_num")) {
    if (nm %in% names(df) && !is.numeric(df[[nm]])) {
      df[[nm]] <- suppressWarnings(as.numeric(as.character(df[[nm]])))
    }
  }
  df
}

# Align factor levels in test data to match fitted model
# align_factor_levels_to_fit <- function(test, fit) {
#   facs <- names(test)[vapply(test, is.factor, TRUE)]
#   for (nm in facs) {
#     lev <- tryCatch(names(fit$var.summary[[nm]]), error = function(e) NULL)
#     if (is.null(lev) || length(lev) == 0) {
#       if (!is.null(fit$model) && nm %in% names(fit$model) && is.factor(fit$model[[nm]])) {
#         lev <- levels(fit$model[[nm]])
#       }
#     }
#     if (!is.null(lev) && length(lev)) {
#       test[[nm]] <- factor(test[[nm]], levels = lev)
#     }
#   }
#   test
# }

# UNUSED - kept for potential future use
# read_gam_artifacts <- function(out_dir = "runs") {
#   pts <- list.files(file.path(out_dir, "gam_point"), pattern = "\\.csv$", full.names = TRUE)
#   prs <- list.files(file.path(out_dir, "gam_prob"), pattern = "\\.csv$", full.names = TRUE)
#   res_point <- if (length(pts)) purrr::map_dfr(pts, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
#   res_prob <- if (length(prs)) purrr::map_dfr(prs, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
#   if (nrow(res_prob) && "WIS" %in% names(res_prob) && !"WIS80" %in% names(res_prob)) {
#     res_prob <- dplyr::rename(res_prob, WIS80 = WIS)
#   }
#   if (nrow(res_prob) && !"cov90" %in% names(res_prob)) {
#     res_prob <- dplyr::mutate(res_prob, cov90 = NA_real_)
#   }
#   list(point = res_point, prob = res_prob)
# }


# ============================================================================
# MAIN FUNCTIONS - Ordered by Workflow
# ============================================================================

# ============================================================================
# WORKFLOW 1: PREDICTION GENERATION & CACHING
# ============================================================================

# ----------------------------------------------------------------------------
# run_job_gam_pp_cached
# Purpose: Fit GAM, generate and cache predictions for ALL test observations
# Use: Run once with pmap_dfr to generate cached predictions
# Output: Used by summarise_gam_metrics_incidence for fast MAE/RMSE calculation
# ----------------------------------------------------------------------------
run_job_gam_pp_cached <- function(model, mask_type, fold,
                                  data = data,
                                  masks = masks,
                                  formulas = formulas,
                                  family = nb(),
                                  train_policy = "inclusive",
                                  nsamp = 300, per = 1e5, seed = 1,
                                  out_dir = "runs") {
  suppressPackageStartupMessages({
    library(mgcv)
    library(dplyr)
    library(readr)
    library(mvtnorm)
    library(tibble)
  })

  dir_point <- file.path(out_dir, "gam_point")
  dir_prob <- file.path(out_dir, "gam_prob")
  dir_fits <- file.path(out_dir, "gam_fits")
  dir.create(dir_point, TRUE, FALSE)
  dir.create(dir_prob, TRUE, FALSE)
  dir.create(dir_fits, TRUE, FALSE)

  fp_point <- file.path(dir_point, sprintf("%s_%s_fold%s.csv", model, mask_type, fold))
  fp_prob <- file.path(dir_prob, sprintf("%s_%s_fold%s.csv", model, mask_type, fold))
  fp_fit <- file.path(dir_fits, sprintf("%s_%s_fold%s.rds", model, mask_type, fold))

  set.seed(seed)

  # Keep ordering stable for CV splits, then create row_id ONCE if missing
  data <- data %>% arrange(adm_0_name, time_seq)
  if (!"row_id" %in% names(data)) data <- mutate(data, row_id = row_number())

  mask <- read_mask_obj(masks[[mask_type]])
  sp <- get_fold_split(mask, fold_id = fold, train_policy = train_policy)


  if (length(sp$test_idx) == 0L) {
    point <- tibble(
      model = model, mask_type = mask_type, fold = fold,
      row_id = integer(0), adm_0_name = character(0),
      Year = character(0), calendar_start_date = as.Date(character(0)),
      true_counts = numeric(0), pred = numeric(0)
    )
    prob <- tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test = 0L, cov80 = NA_real_,
      n_countries = 0L, years_range = NA_character_, date_range = NA_character_,
      total_true_counts = 0L,
      n_unseen_year_rows = 0L, mean_abs_year_term_unseen = NA_real_
    )
    write_csv(point, fp_point)
    write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  train <- data[sp$train_idx, , drop = FALSE] %>% mutate(y = dengue_total)
  newd <- data[sp$test_idx, , drop = FALSE]

  # ---- Align factor levels to FULL data levels (needed for “unseen year → 0 effect”)
  factor_cols <- names(train)[sapply(train, is.factor)]
  for (col in factor_cols) {
    all_levels <- if (col %in% names(data) && is.factor(data[[col]])) levels(data[[col]]) else levels(train[[col]])
    train[[col]] <- factor(train[[col]], levels = all_levels)
    newd[[col]] <- factor(newd[[col]], levels = all_levels)
  }

  # Fit model (KEY: keep unused levels so unseen Year RE can exist and shrink ~0)
  fit <- mgcv::bam(
    update(formulas[[model]], y ~ .),
    data = train,
    family = family,
    method = "fREML",
    discrete = TRUE,
    nthreads = 4,
    na.action = na.exclude,
    drop.unused.levels = FALSE
  )
  saveRDS(fit, fp_fit)

  # --------- Sanity check A: predictions exist for ALL rows (not dropped)
  mu_hat <- predict(fit, newd, type = "response")
  if (length(mu_hat) != nrow(newd)) {
    stop(sprintf(
      "predict(response) length %d != nrow(newd) %d (rows are being dropped).",
      length(mu_hat), nrow(newd)
    ))
  }
  if (anyNA(mu_hat)) {
    stop(sprintf(
      "Point predictions contain NA (%d/%d). Check missing covariates/factor handling.",
      sum(is.na(mu_hat)), length(mu_hat)
    ))
  }
  mu_hat <- as.numeric(mu_hat)

  point <- tibble(
    model = model, mask_type = mask_type, fold = fold,
    row_id = newd$row_id,
    adm_0_name = as.character(newd$adm_0_name),
    Year = as.character(newd$Year),
    calendar_start_date = newd$calendar_start_date,
    true_counts = newd$dengue_total,
    pred = mu_hat
  )
  write_csv(point, fp_point)

  # --------- Sanity check B: unseen years have ~0 YEAR-term effect (not NA)
  yrs_train <- unique(as.character(train$Year))
  yrs_test <- unique(as.character(newd$Year))
  unseen_years <- setdiff(yrs_test, yrs_train)
  unseen_rows <- as.character(newd$Year) %in% unseen_years
  n_unseen_year_rows <- sum(unseen_rows)

  mean_abs_year_term_unseen <- NA_real_
  if (n_unseen_year_rows > 0) {
    tm <- predict(fit, newd, type = "terms")
    yr_cols <- grep("Year", colnames(tm), value = TRUE)
    if (length(yr_cols) > 0) {
      mean_abs_year_term_unseen <- mean(abs(as.matrix(tm[unseen_rows, yr_cols, drop = FALSE])), na.rm = TRUE)
    }
    # If this is huge, your “default 0” assumption isn’t holding.
    if (!is.finite(mean_abs_year_term_unseen)) {
      warning("Unseen-year term contribution is not finite; check factor handling.")
    }
  }

  # Posterior predictive draws
  X <- predict(fit, newd, type = "lpmatrix")
  if (nrow(X) != nrow(newd)) {
    stop(sprintf(
      "predict(lpmatrix) nrow %d != nrow(newd) %d (rows are being dropped).",
      nrow(X), nrow(newd)
    ))
  }

  b0 <- coef(fit)
  V <- fit$Vp
  if (any(is.na(b0)) || any(is.na(V))) {
    warning("NA values in model coefficients or covariance matrix")
    prob <- tibble(
      model = model, mask_type = mask_type, fold = fold,
      n_test = as.integer(nrow(newd)), cov80 = NA_real_,
      n_countries = length(unique(newd$adm_0_name)),
      years_range = tryCatch(paste(range(as.numeric(as.character(newd$Year)), na.rm = TRUE), collapse = "-"), error = function(e) NA_character_),
      date_range = tryCatch(paste(range(newd$calendar_start_date, na.rm = TRUE), collapse = " to "), error = function(e) NA_character_),
      total_true_counts = sum(newd$dengue_total, na.rm = TRUE),
      n_unseen_year_rows = as.integer(n_unseen_year_rows),
      mean_abs_year_term_unseen = mean_abs_year_term_unseen
    )
    write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  B <- mvtnorm::rmvnorm(nsamp, mean = b0, sigma = V)
  Eta <- X %*% t(B)

  # Clamp (common; prevents exp overflow and extreme mu)
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

  # Incidence per 100k (avoid division by 0/NA by turning invalid scales into NA)
  scale_fac <- newd$pop_est / per
  bad_scale <- !is.finite(scale_fac) | is.na(scale_fac) | scale_fac <= 0
  if (any(bad_scale)) scale_fac[bad_scale] <- NA_real_

  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- newd$dengue_total / scale_fac

  q50 <- as.numeric(apply(Ys_i, 1, stats::quantile, probs = 0.50, na.rm = TRUE))
  q10 <- as.numeric(apply(Ys_i, 1, stats::quantile, probs = 0.10, na.rm = TRUE))
  q90 <- as.numeric(apply(Ys_i, 1, stats::quantile, probs = 0.90, na.rm = TRUE))

  coverage <- mean(y_true_i >= q10 & y_true_i <= q90, na.rm = TRUE)

  prob <- tibble(
    model = model, mask_type = mask_type, fold = fold,
    n_test = as.integer(sum(is.finite(y_true_i))), # evaluable rows (scale defined)
    cov80 = as.numeric(coverage),
    n_countries = length(unique(newd$adm_0_name)),
    years_range = tryCatch(
      paste(range(as.numeric(as.character(newd$Year)), na.rm = TRUE), collapse = "-"),
      error = function(e) NA_character_
    ),
    date_range = tryCatch(
      paste(range(newd$calendar_start_date, na.rm = TRUE), collapse = " to "),
      error = function(e) NA_character_
    ),
    total_true_counts = sum(newd$dengue_total, na.rm = TRUE),
    n_unseen_year_rows = as.integer(n_unseen_year_rows),
    mean_abs_year_term_unseen = mean_abs_year_term_unseen
  )
  write_csv(prob, fp_prob)

  list(
    point = point,
    prob = prob,
    q_df = tibble(
      row_id = newd$row_id,
      adm_0_name = as.character(newd$adm_0_name),
      Year = as.character(newd$Year),
      calendar_start_date = newd$calendar_start_date,
      model = model, mask_type = mask_type, fold = fold,
      true_counts = newd$dengue_total,
      q10_i = q10, q50_i = q50, q90_i = q90
    )
  )
}



# ============================================================================
# WORKFLOW 2: POINT PREDICTION METRICS (MAE/RMSE)
# ============================================================================

# ----------------------------------------------------------------------------
# summarise_gam_metrics_incidence
# Purpose: Calculate MAE/RMSE from cached predictions
# Input: Nested results from run_job_gam_pp_cached via pmap_dfr
# Output: MAE/RMSE metrics on ALL test observations (no filtering)
# ----------------------------------------------------------------------------
summarise_gam_metrics_incidence <- function(res_gam,
                                            data = NULL,
                                            per = 1e5,
                                            masks = NULL,
                                            warn_only = TRUE) {
  # Extract nested structure if present
  if (all(c("point", "prob", "q_df") %in% names(res_gam))) {
    message("Detected nested structure - extracting components...")

    n_rows <- nrow(res_gam)
    message(sprintf("Processing %d rows from res_gam", n_rows))

    point_df <- dplyr::bind_rows(res_gam$point)
    q_df <- dplyr::bind_rows(res_gam$q_df)

    message(sprintf(
      "Extracted %d point predictions and %d quantile predictions",
      nrow(point_df), nrow(q_df)
    ))

    df <- point_df %>%
      dplyr::left_join(
        q_df %>% dplyr::select(row_id, model, mask_type, fold, q10_i, q50_i, q90_i),
        by = c("row_id", "model", "mask_type", "fold")
      )

    if (!is.null(data)) {
      if (!"row_id" %in% names(data)) {
        data <- data %>% dplyr::mutate(row_id = dplyr::row_number())
      }
      df <- df %>%
        dplyr::left_join(
          data %>% dplyr::select(row_id, pop_est),
          by = "row_id"
        )
    }

    df <- df %>%
      dplyr::rename(truth = true_counts) %>%
      dplyr::mutate(pop_est = if ("pop_est" %in% names(.)) pop_est else NA_real_)
  } else {
    df <- res_gam
  }

  # Check required columns
  need_cols <- c("truth", "pred", "model", "mask_type", "fold")
  missing_cols <- setdiff(need_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!"pop_est" %in% names(df)) {
    stop("pop_est column required. Please provide 'data' parameter with pop_est column.")
  }

  # Scale to incidence
  message("Scaling predictions to incidence per 100k...")
  scale_fac <- df$pop_est / per
  df <- df %>%
    dplyr::mutate(
      truth_i = truth / scale_fac,
      pred_i = pmax(pred, 0) / scale_fac
    )

  # Fold-level metrics (MAE/RMSE only)
  metrics_fold <- df %>%
    dplyr::summarise(
      n_test = dplyr::n(),
      MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
      RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
      .by = c(model, mask_type, fold)
    ) %>%
    dplyr::arrange(model, mask_type, fold)

  # Overall metrics (MAE/RMSE only)
  metrics_overall <- df %>%
    dplyr::summarise(
      n_test = dplyr::n(),
      MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
      RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
      .by = c(model, mask_type)
    ) %>%
    dplyr::arrange(model, mask_type)

  # Optional mask count check
  mask_check <- NULL
  if (!is.null(masks)) {
    df_counts <- df %>% dplyr::count(mask_type, name = "n_df")

    mask_counts <- tibble::enframe(masks, name = "mask_type", value = "obj") %>%
      dplyr::mutate(obj = purrr::map(obj, read_mask_obj)) %>%
      dplyr::transmute(
        mask_type,
        n_mask = purrr::map_int(obj, ~ {
          if (!("fold" %in% names(.x))) stop("Mask for '", dplyr::cur_data()$mask_type, "' lacks a 'fold' column.")
          sum(!is.na(.x$fold))
        })
      )

    mask_check <- df_counts %>%
      dplyr::full_join(mask_counts, by = "mask_type") %>%
      dplyr::mutate(
        n_df = dplyr::coalesce(n_df, 0L),
        n_mask = dplyr::coalesce(n_mask, 0L),
        match = (n_df == n_mask),
        diff = n_df - n_mask
      ) %>%
      dplyr::arrange(mask_type)

    if (any(!mask_check$match)) {
      msg <- paste0(
        "Mismatch between df counts and mask counts by mask_type. ",
        paste(
          sprintf(
            "%s: df=%d, mask=%d (diff=%+d)",
            mask_check$mask_type, mask_check$n_df, mask_check$n_mask, mask_check$diff
          ),
          collapse = "; "
        )
      )
      if (isTRUE(warn_only)) warning(msg, call. = FALSE) else stop(msg, call. = FALSE)
    } else {
      message(
        "Mask counts match: ",
        paste(sprintf("%s=%d", mask_check$mask_type, mask_check$n_df), collapse = ", ")
      )
    }
  }

  list(df = df, metrics_fold = metrics_fold, metrics_overall = metrics_overall, mask_check = mask_check)
}


# ----------------------------------------------------------------------------
# summarise_gam_metrics_incidence_mean_median
# Purpose: report MAE/RMSE for both:
#   - mean-based point prediction: pred (counts) -> incidence
#   - median-based point prediction: q50_i (incidence) from PP draws
# ----------------------------------------------------------------------------
summarise_gam_metrics_incidence_mean_median <- function(res_gam, data, per = 1e5) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
  })

  if (!all(c("point", "q_df") %in% names(res_gam))) {
    stop("Expected nested res_gam with columns: point, q_df")
  }

  point_df <- dplyr::bind_rows(res_gam$point)
  q_df <- dplyr::bind_rows(res_gam$q_df)

  # join q50_i onto point_df
  df <- point_df %>%
    dplyr::left_join(
      q_df %>% dplyr::select(row_id, model, mask_type, fold, q50_i),
      by = c("row_id", "model", "mask_type", "fold")
    )

  # add pop for scaling
  if (!"row_id" %in% names(data)) data <- data %>% dplyr::mutate(row_id = dplyr::row_number())
  df <- df %>% dplyr::left_join(data %>% dplyr::select(row_id, pop_est), by = "row_id")

  # compute incidence truth and both point estimates
  scale_fac <- df$pop_est / per
  df <- df %>%
    dplyr::mutate(
      truth_i = true_counts / scale_fac,

      # mean-based: pred is in counts
      pred_i_mean = pmax(pred, 0) / scale_fac,

      # median-based: q50_i is already incidence per 100k from draws in run_job_gam_pp_cached()
      pred_i_median = pmax(q50_i, 0)
    )

  # make long format so we can summarise both in one go
  long <- df %>%
    tidyr::pivot_longer(
      cols = c(pred_i_mean, pred_i_median),
      names_to = "point_est",
      values_to = "pred_i"
    ) %>%
    dplyr::mutate(
      point_est = dplyr::recode(point_est,
        pred_i_mean = "mean_based",
        pred_i_median = "median_based"
      )
    ) %>%
    dplyr::filter(is.finite(truth_i), is.finite(pred_i))

  metrics_fold <- long %>%
    dplyr::summarise(
      n_test = dplyr::n(),
      MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
      RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
      .by = c(model, mask_type, fold, point_est)
    )

  metrics_overall <- metrics_fold %>%
    dplyr::summarise(
      MAE_i = stats::weighted.mean(MAE_i, w = n_test, na.rm = TRUE),
      RMSE_i = stats::weighted.mean(RMSE_i, w = n_test, na.rm = TRUE),
      n_test = sum(n_test, na.rm = TRUE),
      
      .by = c(model, mask_type, point_est)
    )

  list(metrics_fold = metrics_fold, metrics_overall = metrics_overall)
}



# ============================================================================
# WORKFLOW 3: PROBABILISTIC METRICS (COV80/CRPS) ON FILTERED DATA
# ============================================================================

# ----------------------------------------------------------------------------
# gam_cov_crps_fold
# Purpose: Calculate COV80/CRPS with robust filtering
# Output: Single-fold probabilistic metrics
# ----------------------------------------------------------------------------
gam_cov_crps_fold_new <- function(model, mask_type, fold,
                                  data, masks, out_dir = "runs",
                                  nsamp = 800, per = 1e5, seed = 1) {
  suppressPackageStartupMessages({
    library(mgcv)
    library(mvtnorm)
    library(dplyr)
    library(tibble)
  })

  set.seed(seed)
  fp_fit <- file.path(out_dir, "gam_fits", sprintf("%s_%s_fold%s.rds", model, mask_type, fold))
  if (!file.exists(fp_fit)) {
    return(tibble(model, mask_type, fold, n_test = 0, COV80 = NA_real_, CRPS = NA_real_))
  }
  fit <- readRDS(fp_fit)

  mask <- read_mask_obj(masks[[mask_type]])
  sp <- get_fold_split(mask, fold_id = fold, train_policy = "inclusive")
  test <- data[sp$test_idx, , drop = FALSE]
  if (!nrow(test)) {
    return(tibble(model, mask_type, fold, n_test = 0, COV80 = NA_real_, CRPS = NA_real_))
  }

  # Hard requirements for incidence scaling + truth
  test <- test %>% filter(!is.na(pop_est), pop_est > 0, !is.na(dengue_total))
  if (!nrow(test)) {
    return(tibble(model, mask_type, fold, n_test = 0, COV80 = NA_real_, CRPS = NA_real_))
  }

  test <- coerce_time_numerics(test)

  X <- predict(fit, test, type = "lpmatrix")
  bad <- which(!is.finite(rowSums(abs(X))))
  if (length(bad)) {
    test <- test[-bad, , drop = FALSE]
    X <- X[-bad, , drop = FALSE]
  }
  if (!nrow(test)) {
    return(tibble(model, mask_type, fold, n_test = 0, COV80 = NA_real_, CRPS = NA_real_))
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
  if (!any(keep)) {
    return(tibble(model, mask_type, fold, n_test = 0, COV80 = NA_real_, CRPS = NA_real_))
  }

  Ys_i <- Ys_i[keep, , drop = FALSE]
  y_true_i <- y_true_i[keep]

  q10 <- apply(Ys_i, 1, quantile, probs = 0.10, na.rm = TRUE)
  q90 <- apply(Ys_i, 1, quantile, probs = 0.90, na.rm = TRUE)

  tibble(
    model = model, mask_type = mask_type, fold = fold,
    n_test = length(y_true_i),
    COV80 = mean(y_true_i >= q10 & y_true_i <= q90, na.rm = TRUE),
    CRPS = mean(crps_mc_safe(y_true_i, Ys_i), na.rm = TRUE)
  )
}



# ----------------------------------------------------------------------------
# gam_cov_crps_all
# Purpose: Aggregate probabilistic metrics across folds
# Use: Combines fold-level results with proper weighted averaging
# Output: Overall COV80/CRPS metrics
# ----------------------------------------------------------------------------
gam_cov_crps_all_new <- function(models, masks, folds = 1:3,
                                 data, out_dir = "runs",
                                 nsamp = 800, per = 1e5, seed = 1) {
  jobs <- tidyr::crossing(model = models, mask_type = names(masks), fold = folds)

  rows <- purrr::pmap(jobs, ~ gam_cov_crps_fold_new(..1, ..2, ..3,
    data = data, masks = masks,
    out_dir = out_dir, nsamp = nsamp, per = per, seed = seed
  ))

  rows <- purrr::keep(rows, ~ is.data.frame(.) && nrow(.) > 0)
  fold <- dplyr::bind_rows(rows)

  if (!nrow(fold)) {
    message("No GAM fold results produced.")
    return(list(
      fold = tibble::tibble(
        model = character(), mask_type = character(), fold = integer(),
        n_test = integer(), COV80 = double(), CRPS = double()
      ),
      overall = tibble::tibble(
        model = character(), mask_type = character(),
        n_test = integer(), COV80 = double(), CRPS = double()
      )
    ))
  }

  overall <- fold %>%
    dplyr::group_by(model, mask_type) %>%
    dplyr::summarise(
      COV80 = stats::weighted.mean(COV80, w = n_test, na.rm = TRUE),
      CRPS = stats::weighted.mean(CRPS, w = n_test, na.rm = TRUE),
      n_test = sum(n_test),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mask_type, model)

  list(fold = fold, overall = overall)
}
