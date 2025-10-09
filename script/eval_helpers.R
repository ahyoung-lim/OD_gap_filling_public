# ---- tiny helpers ----
.ε <- 1e-8
mae_vec <- function(p, y) mean(abs(p - y), na.rm = TRUE)
rmse_vec <- function(p, y) sqrt(mean((p - y)^2, na.rm = TRUE))




# ---- standard WIS80 + coverage (same for all models) ----
wis80_from_q <- function(y, m, L, U) {
  a <- 0.2
  w0 <- 0.5
  w1 <- a / 2
  denom <- w0 + w1
  IS <- (U - L) + (2 / a) * pmax(L - y, 0) + (2 / a) * pmax(y - U, 0)
  (w0 * abs(y - m) + w1 * IS) / denom
}

read_gam_artifacts <- function(out_dir = "runs") {
  pts <- list.files(file.path(out_dir, "gam_point"), pattern = "\\.csv$", full.names = TRUE)
  prs <- list.files(file.path(out_dir, "gam_prob"), pattern = "\\.csv$", full.names = TRUE)
  res_point <- if (length(pts)) purrr::map_dfr(pts, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
  res_prob <- if (length(prs)) purrr::map_dfr(prs, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
  # normalise columns (your aggregator expects WIS80; cov90 optional)
  if (nrow(res_prob) && "WIS" %in% names(res_prob) && !"WIS80" %in% names(res_prob)) {
    res_prob <- dplyr::rename(res_prob, WIS80 = WIS)
  }
  if (nrow(res_prob) && !"cov90" %in% names(res_prob)) {
    res_prob <- dplyr::mutate(res_prob, cov90 = NA_real_)
  }
  list(point = res_point, prob = res_prob)
}



# ---------- GAM: fit + cached PP metrics on incidence ----------
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

  if (!"row_id" %in% names(data)) {
    data <- mutate(data, row_id = row_number())
  }

  data <- data %>%
    arrange(adm_0_name, time_seq)

  # split
  sp <- get_fold_split(masks[[mask_type]], fold_id = fold, train_policy = train_policy)

  if (length(sp$test_idx) == 0L) {
    point <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      row_id = integer(0),
      adm_0_name = character(0),
      Year = character(0),
      calendar_start_date = as.Date(character(0)),
      true_counts = numeric(0),
      pred = numeric(0)
    )
    prob <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      n_test = 0L,
      cov80 = NA_real_,
      WIS80 = NA_real_,
      n_countries = 0L,
      years_range = NA_character_,
      date_range = NA_character_,
      total_true_counts = 0L
    )
    write_csv(point, fp_point)
    write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  train <- data[sp$train_idx, ] %>% mutate(y = dengue_total)
  test <- data[sp$test_idx, ]

  # CRITICAL FIX: Properly align factor levels before creating newd
  # Identify factor columns
  factor_cols <- names(train)[sapply(train, is.factor)]

  # Freeze factor levels in training data
  for (col in factor_cols) {
    train[[col]] <- factor(train[[col]])
    # Only keep test observations that have levels present in training
    valid_levels <- levels(train[[col]])
    test[[col]] <- factor(test[[col]], levels = valid_levels)
  }

  # Remove test observations with NA factor levels (unseen levels)
  # complete_cases <- complete.cases(test[factor_cols])
  # if (!all(complete_cases)) {
  #   warning(sprintf(
  #     "Removing %d test observations with unseen factor levels",
  #     sum(!complete_cases)
  #   ))
  #   test <- test[complete_cases, ]
  # }

  # Now create newd after factor level alignment
  newd <- test

  # Check if we still have test data after filtering
  if (nrow(test) == 0) {
    point <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      row_id = integer(0),
      adm_0_name = character(0),
      Year = character(0),
      calendar_start_date = as.Date(character(0)),
      true_counts = numeric(0),
      pred = numeric(0)
    )
    prob <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      n_test = 0L,
      cov80 = NA_real_,
      WIS80 = NA_real_,
      n_countries = 0L,
      years_range = NA_character_,
      date_range = NA_character_,
      total_true_counts = 0L
    )
    write_csv(point, fp_point)
    write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  # fit & save - using bam for speed
  fit <- mgcv::bam(
    update(formulas[[model]], y ~ .),
    data = train,
    family = family,
    method = "fREML", # faster REML for bam
    discrete = TRUE, # faster computation
    nthreads = 4, # parallel processing
    na.action = na.exclude
  )
  saveRDS(fit, fp_fit)

  # point predictions on TEST
  mu_hat <- predict(fit, newd, type = "response")

  # ENSURE mu_hat is a simple numeric vector, not matrix
  mu_hat <- as.numeric(mu_hat)

  # Add reproducibility columns
  point <- tibble(
    model = model,
    mask_type = mask_type,
    fold = fold,
    row_id = newd$row_id,
    adm_0_name = as.character(newd$adm_0_name),
    Year = as.character(newd$Year),
    calendar_start_date = newd$calendar_start_date,
    true_counts = newd$dengue_total,
    pred = mu_hat
  )
  write_csv(point, fp_point)

  # posterior predictive draws
  X <- predict(fit, newd, type = "lpmatrix") # N x p
  b0 <- coef(fit)
  V <- fit$Vp # p x p

  # Check for issues with coefficient matrix
  if (any(is.na(b0)) || any(is.na(V))) {
    warning("NA values in model coefficients or covariance matrix")
    # Return NAs for this problematic case
    point <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      row_id = newd$row_id,
      adm_0_name = as.character(newd$adm_0_name),
      Year = as.character(newd$Year),
      calendar_start_date = newd$calendar_start_date,
      true_counts = newd$dengue_total,
      pred = rep(NA_real_, nrow(newd))
    )
    prob <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      n_test = as.integer(nrow(newd)),
      cov80 = NA_real_,
      WIS80 = NA_real_,
      n_countries = length(unique(newd$adm_0_name)),
      years_range = tryCatch(
        {
          year_vals <- as.numeric(as.character(newd$Year))
          paste(range(year_vals, na.rm = TRUE), collapse = "-")
        },
        error = function(e) NA_character_
      ),
      date_range = tryCatch(
        {
          paste(range(newd$calendar_start_date, na.rm = TRUE), collapse = " to ")
        },
        error = function(e) NA_character_
      ),
      total_true_counts = sum(newd$dengue_total, na.rm = TRUE)
    )
    write_csv(point, fp_point)
    write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  # Check matrix dimensions
  if (ncol(X) != length(b0)) {
    warning(sprintf("Dimension mismatch: X has %d cols, b0 has %d elements", ncol(X), length(b0)))
    # Return point predictions only
    point <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      row_id = newd$row_id,
      adm_0_name = as.character(newd$adm_0_name),
      Year = as.character(newd$Year),
      calendar_start_date = newd$calendar_start_date,
      true_counts = newd$dengue_total,
      pred = mu_hat
    )
    prob <- tibble(
      model = model,
      mask_type = mask_type,
      fold = fold,
      n_test = as.integer(nrow(newd)),
      cov80 = NA_real_,
      WIS80 = NA_real_,
      n_countries = length(unique(newd$adm_0_name)),
      years_range = paste(range(as.numeric(as.character(newd$Year)), na.rm = TRUE), collapse = "-"),
      date_range = paste(range(newd$calendar_start_date, na.rm = TRUE), collapse = " to "),
      total_true_counts = sum(newd$dengue_total, na.rm = TRUE)
    )
    write_csv(point, fp_point)
    write_csv(prob, fp_prob)
    return(list(point = point, prob = prob))
  }

  B <- mvtnorm::rmvnorm(nsamp, mean = b0, sigma = V) # S x p
  Eta <- X %*% t(B) # N x S
  Mu <- exp(Eta)

  # Check for problematic predictions
  if (any(is.na(Mu)) || any(is.infinite(Mu))) {
    warning("NA or infinite values in posterior predictions")
    n_bad <- sum(is.na(Mu) | is.infinite(Mu))
    cat(sprintf("  %d out of %d predictions are NA/Inf\n", n_bad, length(Mu)))

    # Clean up predictions
    Mu[is.na(Mu) | is.infinite(Mu)] <- mean(mu_hat, na.rm = TRUE)
  }

  N <- nrow(newd)
  S <- nsamp
  Ys <- matrix(NA_real_, nrow = N, ncol = S)

  fam <- tolower(fit$family$family)
  if (grepl("pois", fam)) {
    for (s in seq_len(S)) {
      Ys[, s] <- rpois(N, lambda = Mu[, s])
    }
  } else if (grepl("neg|nb", fam)) {
    # theta from the fitted NB family (works for mgcv::nb())
    theta <- tryCatch(
      fit$family$getTheta(),
      error = function(e) fit$family$theta
    )
    for (s in seq_len(S)) {
      Ys[, s] <- rnbinom(N, size = theta, mu = Mu[, s])
    }
  } else {
    stop("Unsupported family for PP draws: ", fit$family$family)
  }

  # counts -> incidence per 100k
  scale_fac <- newd$pop_est / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- test$dengue_total / scale_fac

  # quantiles + metrics
  q50 <- apply(Ys_i, 1, stats::quantile, probs = 0.5, na.rm = TRUE)
  q10 <- apply(Ys_i, 1, stats::quantile, probs = 0.10, na.rm = TRUE)
  q90 <- apply(Ys_i, 1, stats::quantile, probs = 0.90, na.rm = TRUE)

  # ENSURE all quantities are simple numeric vectors
  q50 <- as.numeric(q50)
  q10 <- as.numeric(q10)
  q90 <- as.numeric(q90)
  y_true_i <- as.numeric(y_true_i)

  # Additional check: if quantiles are still problematic, use point predictions
  if (sum(is.na(q10)) > N * 0.8 || sum(is.na(q90)) > N * 0.8) {
    cat("  WARNING: Too many NA quantiles, falling back to point prediction intervals\n")
    # Use more realistic uncertainty intervals based on the NB distribution
    pred_i <- mu_hat / scale_fac # convert point predictions to incidence
    q50 <- pred_i

    # For NB distribution, variance = mu + mu^2/theta
    # Use this to create more realistic intervals
    if (grepl("neg|nb", fam)) {
      theta <- tryCatch(fit$family$getTheta(), error = function(e) fit$family$theta)
      theta_safe <- max(theta, 1e-6)
      # Calculate approximate standard deviation on incidence scale
      var_counts <- mu_hat + (mu_hat^2) / theta_safe
      sd_incidence <- sqrt(var_counts) / scale_fac
      # Use +/- 1.28 * SD for 80% intervals (normal approximation)
      q10 <- pmax(0, pred_i - 1.28 * sd_incidence)
      q90 <- pred_i + 1.28 * sd_incidence
    } else {
      # For Poisson, variance = mu
      sd_incidence <- sqrt(mu_hat) / scale_fac
      q10 <- pmax(0, pred_i - 1.28 * sd_incidence)
      q90 <- pred_i + 1.28 * sd_incidence
    }
  }

  # DEBUG: Check for NAs and problematic values
  cat(sprintf("Debug info for %s_%s_fold%s:\n", model, mask_type, fold))
  cat(sprintf(
    "  y_true_i range: [%.3f, %.3f], NAs: %d\n",
    min(y_true_i, na.rm = TRUE), max(y_true_i, na.rm = TRUE), sum(is.na(y_true_i))
  ))
  cat(sprintf(
    "  scale_fac range: [%.3f, %.3f], NAs: %d\n",
    min(scale_fac, na.rm = TRUE), max(scale_fac, na.rm = TRUE), sum(is.na(scale_fac))
  ))
  cat(sprintf(
    "  Mu range: [%.3f, %.3f], NAs: %d, Infs: %d\n",
    min(Mu, na.rm = TRUE), max(Mu, na.rm = TRUE), sum(is.na(Mu)), sum(is.infinite(Mu))
  ))
  cat(sprintf(
    "  Ys range: [%.3f, %.3f], NAs: %d\n",
    min(Ys, na.rm = TRUE), max(Ys, na.rm = TRUE), sum(is.na(Ys))
  ))
  cat(sprintf(
    "  Ys_i range: [%.3f, %.3f], NAs: %d\n",
    min(Ys_i, na.rm = TRUE), max(Ys_i, na.rm = TRUE), sum(is.na(Ys_i))
  ))
  cat(sprintf(
    "  q10 range: [%.3f, %.3f], NAs: %d\n",
    min(q10, na.rm = TRUE), max(q10, na.rm = TRUE), sum(is.na(q10))
  ))
  cat(sprintf(
    "  q90 range: [%.3f, %.3f], NAs: %d\n",
    min(q90, na.rm = TRUE), max(q90, na.rm = TRUE), sum(is.na(q90))
  ))

  # Check if wis80_from_q function exists and works
  tryCatch(
    {
      test_wis <- wis80_from_q(
        y_true_i[1:min(5, length(y_true_i))],
        q50[1:min(5, length(q50))],
        q10[1:min(5, length(q10))],
        q90[1:min(5, length(q90))]
      )
      cat(sprintf("  Sample WIS80 values: %s\n", paste(round(test_wis, 3), collapse = ", ")))
    },
    error = function(e) {
      cat(sprintf("  ERROR in wis80_from_q: %s\n", e$message))
    }
  )

  # Calculate metrics with better error handling
  coverage <- tryCatch(
    {
      mean(y_true_i >= q10 & y_true_i <= q90, na.rm = TRUE)
    },
    error = function(e) {
      warning("Error calculating coverage: ", e$message)
      NA_real_
    }
  )

  wis_score <- tryCatch(
    {
      mean(wis80_from_q(y_true_i, q50, q10, q90), na.rm = TRUE)
    },
    error = function(e) {
      warning("Error calculating WIS80: ", e$message)
      # Try alternative WIS calculation if function fails
      alpha <- 0.2 # for 80% interval
      IS <- pmax(q90 - q10, 0) # interval score width
      penalties <- ifelse(y_true_i < q10, 2 * alpha * (q10 - y_true_i), 0) +
        ifelse(y_true_i > q90, 2 * alpha * (y_true_i - q90), 0)
      mean(IS + penalties, na.rm = TRUE)
    }
  )

  prob <- tibble(
    model = model,
    mask_type = mask_type,
    fold = fold,
    n_test = as.integer(length(y_true_i)),
    cov80 = as.numeric(coverage),
    WIS80 = as.numeric(wis_score),
    # Add summary info for reproducibility - FIX factor range issue
    n_countries = length(unique(newd$adm_0_name)),
    years_range = tryCatch(
      {
        year_vals <- as.numeric(as.character(newd$Year))
        paste(range(year_vals, na.rm = TRUE), collapse = "-")
      },
      error = function(e) NA_character_
    ),
    date_range = tryCatch(
      {
        paste(range(newd$calendar_start_date, na.rm = TRUE), collapse = " to ")
      },
      error = function(e) NA_character_
    ),
    total_true_counts = sum(newd$dengue_total, na.rm = TRUE)
  )
  write_csv(prob, fp_prob)

  # Return simple data structures
  list(
    point = point,
    prob = prob,
    q_df = tibble(
      row_id = newd$row_id,
      adm_0_name = as.character(newd$adm_0_name),
      Year = as.character(newd$Year),
      calendar_start_date = newd$calendar_start_date,
      true_counts = newd$dengue_total,
      q10_i = q10,
      q50_i = q50,
      q90_i = q90
    )
  )
}

summarise_gam_metrics_incidence <- function(res_gam,
                                            per = 1e5, # incidence per 100k
                                            coverage_level = 0.8, # only 0.8 supported with q10/50/90
                                            masks = NULL, # named list of RDS paths or objects
                                            warn_only = TRUE) {
  # Required columns for grouping + scaling
  need_cols <- c("truth", "pred", "pop_est", "model", "mask_type", "fold")
  stopifnot(all(need_cols %in% names(res_gam)))

  if (!all(c("q10", "q50", "q90") %in% names(res_gam))) {
    have_q <- FALSE
    if (!missing(coverage_level) && coverage_level != 0.8) {
      warning("coverage_level != 0.8 but only q10/q50/q90 would be supported; proceeding with no coverage/WIS.")
    }
  } else {
    have_q <- TRUE
    if (coverage_level != 0.8) {
      warning("Only 80% intervals supported with q10/q50/q90; computing COV80/WIS80.", call. = FALSE)
    }
  }

  # --- scale counts → incidence ---
  scale_fac <- res_gam$pop_est / per
  df <- res_gam %>%
    mutate(
      truth_i = truth / scale_fac,
      pred_i = pmax(pred, 0) / scale_fac
    )

  if (have_q) {
    df <- df %>%
      mutate(
        q10_i = q10 / scale_fac,
        q50_i = q50 / scale_fac,
        q90_i = q90 / scale_fac
      )
  }

  # --- local helpers for 80% interval/WIS ---
  interval_score <- function(y, L, U, alpha) {
    (U - L) + (2 / alpha) * pmax(L - y, 0) + (2 / alpha) * pmax(y - U, 0)
  }
  wis80 <- function(y, m, L, U) {
    alpha <- 0.2
    (abs(y - m) + interval_score(y, L, U, alpha)) / (1 + 0.5) # divide by K + 0.5 where K=1
  }

  # --- fold-level ---
  metrics_fold <- df %>%
    summarise(
      n_test = n(),
      MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
      RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
      COV80 = if (have_q) mean(truth_i >= q10_i & truth_i <= q90_i, na.rm = TRUE) else NA_real_,
      WIS80 = if (have_q) mean(wis80(truth_i, q50_i, q10_i, q90_i), na.rm = TRUE) else NA_real_,
      .by = c(model, mask_type, fold)
    ) %>%
    arrange(model, mask_type, fold)

  # --- overall (per model x mask_type) ---
  metrics_overall <- df %>%
    summarise(
      n_test = n(),
      MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
      RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
      COV80 = if (have_q) mean(truth_i >= q10_i & truth_i <= q90_i, na.rm = TRUE) else NA_real_,
      WIS80 = if (have_q) mean(wis80_from_q(truth_i, q50_i, q10_i, q90_i), na.rm = TRUE) else NA_real_,
      .by = c(model, mask_type)
    ) %>%
    arrange(model, mask_type)

  # --- optional: check df counts vs mask counts ---
  mask_check <- NULL
  if (!is.null(masks)) {
    df_counts <- df %>% count(mask_type, name = "n_df")

    read_mask_obj <- function(x) {
      if (is.character(x) && length(x) == 1L) readRDS(x) else x
    }

    mask_counts <- enframe(masks, name = "mask_type", value = "obj") %>%
      mutate(obj = map(obj, read_mask_obj)) %>%
      transmute(
        mask_type,
        n_mask = map_int(obj, ~ {
          if (!("fold" %in% names(.x))) stop("Mask for '", cur_data()$mask_type, "' lacks a 'fold' column.")
          sum(!is.na(.x$fold))
        })
      )

    mask_check <- df_counts %>%
      full_join(mask_counts, by = "mask_type") %>%
      mutate(
        n_df   = coalesce(n_df, 0L),
        n_mask = coalesce(n_mask, 0L),
        match  = (n_df == n_mask),
        diff   = n_df - n_mask
      ) %>%
      arrange(mask_type)

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

  list(metrics_fold = metrics_fold, metrics_overall = metrics_overall, mask_check = mask_check)
}


# Coverage + WIS
# coverage_rate <- function(y, lo, hi) mean(y >= lo & y <= hi, na.rm = TRUE)
# wis_from_quantiles <- function(y, q_median, q_lowers, q_uppers, alphas = c(0.1, 0.2, 0.3, 0.4)) {
#   stopifnot(ncol(q_lowers) == length(alphas), ncol(q_uppers) == length(alphas))
#   K <- length(alphas)
#   is_sum <- 0
#   for (k in seq_len(K)) {
#     L <- q_lowers[, k]
#     U <- q_uppers[, k]
#     a <- alphas[k]
#     is_k <- (U - L) + (2 / a) * pmax(L - y, 0) + (2 / a) * pmax(y - U, 0)
#     is_sum <- is_sum + is_k
#   }
#   (abs(y - q_median) + is_sum) / (K + 0.5)
# }

# ---- incidence metrics + probabilistic for GAM results ----
# summarise_gam_metrics_incidence <- function(res_gam,
#                                             per = 1e5, # incidence per 100k
#                                             coverage_level = 0.8) {
#   stopifnot(all(c("truth", "pred", "pop_est") %in% names(res_gam)))
#
#   # scale counts -> incidence
#   scale_fac <- res_gam$pop_est / per
#   truth_i <- res_gam$truth / scale_fac
#   pred_i <- res_gam$pred / scale_fac
#
#   # quantiles to incidence if present (assumes 10/50/90 → 80% central)
#   have_q <- all(c("q10", "q50", "q90") %in% names(res_gam))
#   if (coverage_level != 0.8 && !("draws" %in% names(res_gam))) {
#     warning("coverage_level != 0.8 but only q10/q50/q90 available; computing 80% coverage/WIS.")
#   }
#   q10_i <- if (have_q) res_gam$q10 / scale_fac else NA_real_
#   q50_i <- if (have_q) res_gam$q50 / scale_fac else NA_real_
#   q90_i <- if (have_q) res_gam$q90 / scale_fac else NA_real_
#
#   df <- dplyr::mutate(
#     res_gam,
#     truth_i = truth_i,
#     pred_i = pred_i,
#     q10_i = q10_i, q50_i = q50_i, q90_i = q90_i
#   )
#
#   # fold-level table
#   metrics_fold <- df %>%
#     dplyr::summarise(
#       n_test = dplyr::n(),
#       MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
#       RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
#       COV80 = if (have_q) coverage_from_quantiles(truth_i, q10_i, q90_i) else NA_real_,
#       WIS80 = if (have_q) mean(wis_from_quantiles(truth_i, q10_i, q50_i, q90_i, level = 0.8), na.rm = TRUE) else NA_real_,
#       .by = c(model, mask_type, fold)
#     ) %>%
#     dplyr::arrange(model, mask_type, fold)
#
#   # overall table
#   metrics_overall <- df %>%
#     dplyr::summarise(
#       n_test = dplyr::n(),
#       MAE_i = mean(abs(pred_i - truth_i), na.rm = TRUE),
#       RMSE_i = sqrt(mean((pred_i - truth_i)^2, na.rm = TRUE)),
#       COV80 = if (have_q) mean(truth_i >= q10_i & truth_i <= q90_i, na.rm = TRUE) else NA_real_,
#       WIS80 = if (have_q) mean(wis_from_quantiles(truth_i, q10_i, q50_i, q90_i, level = 0.8), na.rm = TRUE) else NA_real_,
#       .by = c(model, mask_type)
#     ) %>%
#     dplyr::arrange(model, mask_type)
#
#   list(metrics_fold = metrics_fold, metrics_overall = metrics_overall)
# }





# helpers for INLA --------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(glue)

# Build a manifest and mark which jobs are already on disk
build_manifest <- function(models, masks, folds, out_dir = "runs") {
  tibble(model = models) %>%
    tidyr::crossing(mask_type = names(masks), fold = folds) %>%
    mutate(
      point_path = file.path(out_dir, "inla_point", glue("{model}_{mask_type}_fold{fold}.csv")),
      prob_path  = file.path(out_dir, "inla_prob", glue("{model}_{mask_type}_fold{fold}.csv")),
      is_done    = file.exists(point_path) & file.exists(prob_path)
    )
}



read_inla_artifacts <- function(out_dir = "runs") {
  pts <- list.files(file.path(out_dir, "inla_point"), pattern = "\\.csv$", full.names = TRUE)
  prs <- list.files(file.path(out_dir, "inla_prob"), pattern = "\\.csv$", full.names = TRUE)
  res_point <- if (length(pts)) map_dfr(pts, readr::read_csv, show_col_types = FALSE) else tibble()
  res_prob <- if (length(prs)) map_dfr(prs, readr::read_csv, show_col_types = FALSE) else tibble()
  list(point = res_point, prob = res_prob)
}

# run without metrics -------------------------------------
fit_fold_inla <- function(data, mask, formula, fold_id,
                          y_col = "dengue_total",
                          country_col = "adm_0_name",
                          time_col = "time_seq",
                          ctrl_fam = ctrl_fam,
                          train_policy = "inclusive") {
  if (!"row_id" %in% names(data)) data <- dplyr::mutate(data, row_id = dplyr::row_number())

  sp <- get_fold_split(mask, fold_id = fold_id, train_policy = train_policy)
  if (length(sp$test_idx) == 0) {
    return(tibble::tibble(fold = fold_id, row_id = integer(0), pred = numeric(0)))
  }

  train <- data[sp$train_idx, ] %>% dplyr::mutate(.fold = fold_id, y = .data[[y_col]])
  test <- data[sp$test_idx, ] %>% dplyr::mutate(.fold = fold_id, y = NA_real_)
  df <- dplyr::bind_rows(train, test)

  fit <- INLA::inla(
    formula = update(formula, y ~ .),
    data = df,
    family = ctrl_fam$family,
    control.family = ctrl_fam$control.family,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE)
  )

  hold_idx <- which(is.na(df$y))
  tibble::tibble(
    fold   = fold_id,
    row_id = df$row_id[hold_idx],
    pred   = fit$summary.fitted.values$mean[hold_idx]
  )
}
# Sequential runner ---------------------------------------#

library(progress)
library(purrr)

run_inla_sequential <- function(
    data, masks, formulas, ctrl_fam,
    folds = 1:3, out_dir = "runs",
    nsamp = 400, seed = 20250811) {
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)

  todo <- jobs %>% filter(!is_done)
  message(glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))

  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  pb <- progress_bar$new(
    total = nrow(todo),
    format = "INLA [:bar] :current/:total (:percent) ETA: :eta | :model/:mask f=:fold",
    clear = FALSE, show_after = 0
  )

  res_list <- pmap(
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

# parallel runner ---------------------------------------#
library(furrr)
library(progressr)

run_inla_parallel <- function(
    data, masks, formulas, ctrl_fam,
    folds = 1:3, out_dir = "runs",
    workers = 8, nsamp = 400, seed = 20250811) {
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% filter(!is_done)
  message(glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))

  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  plan(multisession, workers = workers)
  handlers(global = TRUE)
  handlers("cli")

  with_progress({
    p <- progressor(steps = nrow(todo))
    res_list <- future_pmap(
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

run_inla_parallel_jupyter <- function(
    data, masks, formulas, ctrl_fam,
    folds = 1:3, out_dir = "runs",
    workers = 3, nsamp = 400, seed = 20250811) {
  library(dplyr)
  library(glue)
  library(furrr)
  library(progressr)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% filter(!is_done)
  message(glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))
  if (nrow(todo) == 0) {
    return(invisible(NULL))
  }

  # --- progressr setup: notebook-safe ---
  progressr::handlers("txtprogressbar")

  plan(multisession, workers = workers)
  with_progress({
    p <- progressor(steps = nrow(todo))
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
        packages = c("INLA", "dplyr", "purrr", "glue") # ok to keep
      )
    )
    res_list
  })
}


run_inla_sequential_jupyter <- function(
    data, masks, formulas, ctrl_fam,
    folds = 1:3, out_dir = "runs",
    nsamp = 400, seed = 20250811) {
  library(dplyr)
  library(glue)
  library(purrr)
  library(progress)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  jobs <- build_manifest(models = names(formulas), masks = masks, folds = folds, out_dir = out_dir)
  todo <- jobs %>% filter(!is_done)
  message(glue("Total jobs: {nrow(jobs)} | to run: {nrow(todo)} | already done: {sum(jobs$is_done)}"))
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



# ---- INLA: fit, SAVE, and compute incidence-scale PP metrics ----
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
    prob <- tibble::tibble(model, mask_type, fold, n_test = 0, cov80 = NA_real_, WIS80 = NA_real_)
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

  # save fit + indices so you can resample later without re-fitting
  saveRDS(fit, fp_fit)
  saveRDS(list(hold_df_idx = which(is.na(df$y)), row_id = df$row_id), fp_meta)

  hold_df_idx <- readRDS(fp_meta)$hold_df_idx

  # point predictions for the test rows
  point <- tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    row_id = df$row_id[hold_df_idx],
    pred = fit$summary.fitted.values$mean[hold_df_idx]
  )
  readr::write_csv(point, fp_point)

  # ---- posterior predictive draws on INCIDENCE scale ----
  samp <- INLA::inla.posterior.sample(nsamp, fit,
    seed = seed,
    selection = list(Predictor = hold_df_idx)
  )
  S <- length(samp)
  N <- length(hold_df_idx)

  # latent linear predictor (eta) -> mu
  # each sample 'latent' contains the requested Predictor nodes (N x 1)
  eta_mat <- do.call(cbind, lapply(samp, function(z) as.numeric(z$latent[, 1L])))
  mu_mat <- exp(eta_mat)

  Ys <- matrix(NA_real_, nrow = N, ncol = S)
  fam <- tolower(ctrl_fam$family)
  if (grepl("pois", fam)) {
    for (s in seq_len(S)) Ys[, s] <- rpois(N, mu = mu_mat[, s])
  } else if (grepl("nb", fam)) {
    # pull NB size/theta from hyperpar summary
    theta_row <- grep("size|theta", rownames(fit$summary.hyperpar), ignore.case = TRUE)
    stopifnot(length(theta_row) == 1L)
    theta <- as.numeric(fit$summary.hyperpar[theta_row, "mean"])
    for (s in seq_len(S)) Ys[, s] <- rnbinom(N, size = theta, mu = mu_mat[, s])
  } else {
    stop("Family not handled: ", ctrl_fam$family)
  }

  # counts -> incidence per 100k
  scale_fac <- data$pop_est[df$row_id[hold_df_idx]] / per
  Ys_i <- Ys / matrix(scale_fac, nrow = N, ncol = S, byrow = FALSE)
  y_true_i <- data$dengue_total[df$row_id[hold_df_idx]] / scale_fac

  # quantiles and metrics (80% only for comparability)
  q_med <- apply(Ys_i, 1, stats::quantile, probs = 0.5, na.rm = TRUE)
  q80 <- apply(Ys_i, 1, stats::quantile, probs = c(0.10, 0.90), na.rm = TRUE)

  prob <- tibble::tibble(
    model = model, mask_type = mask_type, fold = fold,
    n_test = length(y_true_i),
    cov80 = mean(y_true_i >= q80[1, ] & y_true_i <= q80[2, ], na.rm = TRUE),
    WIS80 = mean(wis80_from_q(y_true_i, q_med, q80[1, ], q80[2, ]), na.rm = TRUE)
  )
  readr::write_csv(prob, fp_prob)
  list(point = point, prob = prob, fit_path = fp_fit)
}


make_metrics_counts_incidence_prob <- function(res_point_df, res_prob_df,
                                               data_df, per = 1e5, pop_floor = 1,
                                               masks = NULL, warn_only = TRUE) {
  stopifnot(is.data.frame(res_point_df) || inherits(res_point_df, "tbl"))

  # --- truth + pop join ---
  data_df <- dplyr::arrange(data_df, adm_0_name, time_seq)
  df <- res_point_df %>%
    dplyr::left_join(
      dplyr::mutate(data_df, row_id = dplyr::row_number()) %>%
        dplyr::select(row_id, truth = dengue_total, pop_est),
      by = "row_id"
    ) %>%
    dplyr::filter(!is.na(truth), !is.na(pop_est), pop_est >= pop_floor) %>%
    dplyr::mutate(
      truth_inc = per * truth / pop_est,
      pred_inc  = per * pmax(pred, 0) / pop_est
    )

  mae_vec <- function(p, y) mean(abs(p - y), na.rm = TRUE)
  rmse_vec <- function(p, y) sqrt(mean((p - y)^2, na.rm = TRUE))

  metrics_counts_inc <- df %>%
    dplyr::summarise(
      n_test = dplyr::n(),
      MAE_counts = mae_vec(pred, truth),
      RMSE_counts = rmse_vec(pred, truth),
      MAE_inc = mae_vec(pred_inc, truth_inc),
      RMSE_inc = rmse_vec(pred_inc, truth_inc),
      .by = c(model, mask_type, fold)
    )

  # --- normalise prob cols: expect WIS80; cov90 optional ---
  if (nrow(res_prob_df)) {
    prob_norm <- res_prob_df %>%
      {
        if ("WIS" %in% names(.) && !"WIS80" %in% names(.)) {
          dplyr::rename(., WIS80 = WIS)
        } else {
          .
        }
      } %>%
      {
        if (!"cov90" %in% names(.)) {
          dplyr::mutate(., cov90 = NA_real_)
        } else {
          .
        }
      } %>%
      dplyr::select(model, mask_type, fold, n_test, cov80, cov90, WIS80)
  } else {
    prob_norm <- tibble::tibble(
      model = character(), mask_type = character(), fold = integer(),
      n_test = integer(), cov80 = double(), cov90 = double(), WIS80 = double()
    )
  }

  fold_joined <- dplyr::left_join(
    metrics_counts_inc, prob_norm,
    by = c("model", "mask_type", "fold"), multiple = "error"
  )

  # use whichever n_test we have (from counts block or prob block)
  fold_joined <- fold_joined %>%
    dplyr::mutate(nw = dplyr::coalesce(.data$n_test.x, .data$n_test.y, 0L))

  overall <- fold_joined %>%
    dplyr::summarise(
      n_test = sum(nw, na.rm = TRUE),
      MAE_counts = stats::weighted.mean(MAE_counts, w = nw, na.rm = TRUE),
      RMSE_counts = stats::weighted.mean(RMSE_counts, w = nw, na.rm = TRUE),
      MAE_inc = stats::weighted.mean(MAE_inc, w = nw, na.rm = TRUE),
      RMSE_inc = stats::weighted.mean(RMSE_inc, w = nw, na.rm = TRUE),
      cov80 = stats::weighted.mean(cov80, w = nw, na.rm = TRUE),
      cov90 = stats::weighted.mean(cov90, w = nw, na.rm = TRUE),
      WIS80 = stats::weighted.mean(WIS80, w = nw, na.rm = TRUE),
      .by = c(model, mask_type)
    ) %>%
    dplyr::arrange(mask_type, model)


  # --- optional: mask count check (unchanged) ---
  if (!is.null(masks)) {
    # counts from modelling df
    df_counts <- df %>%
      group_by(mask_type, model) %>%
      dplyr::count(mask_type, name = "n_df") %>%
      ungroup() %>%
      distinct(mask_type, n_df)

    read_mask_obj <- function(x) {
      if (is.character(x) && length(x) == 1L) readRDS(x) else x
    }

    mask_counts <- tibble::tibble(
      mask_type = names(masks),
      n_mask = vapply(masks, function(x) {
        x <- read_mask_obj(x)
        if (!("fold" %in% names(x))) stop("Mask '", deparse(substitute(x)), "' lacks a 'fold' column.")
        sum(!is.na(x$fold))
      }, integer(1L))
    )

    mask_check <- df_counts %>%
      dplyr::full_join(mask_counts, by = "mask_type") %>%
      dplyr::mutate(
        n_df   = dplyr::coalesce(n_df, 0L),
        n_mask = dplyr::coalesce(n_mask, 0L),
        match  = (n_df == n_mask),
        diff   = n_df - n_mask
      ) %>%
      dplyr::arrange(mask_type)

    if (any(!mask_check$match)) {
      msg <- paste0(
        "Mismatch between df counts and mask counts by mask_type.\n",
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


  list(fold = fold_joined, overall = overall, mask_check = mask_check)
}

# read_inla_artifacts <- function(out_dir = "runs") {
#   pts <- list.files(file.path(out_dir, "inla_point"), pattern = "\\.csv$", full.names = TRUE)
#   prs <- list.files(file.path(out_dir, "inla_prob"),  pattern = "\\.csv$", full.names = TRUE)
#
#   res_point <- if (length(pts)) purrr::map_dfr(pts, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
#   res_prob  <- if (length(prs)) purrr::map_dfr(prs, readr::read_csv, show_col_types = FALSE) else tibble::tibble()
#
#   # --- normalise prob schema ---
#   if (nrow(res_prob)) {
#     # old files may have WIS (rename to WIS80)
#     if ("WIS" %in% names(res_prob) && !"WIS80" %in% names(res_prob)) {
#       res_prob <- dplyr::rename(res_prob, WIS80 = WIS)
#     }
#     # cov90 optional → fill with NA if missing
#     if (!"cov90" %in% names(res_prob)) {
#       res_prob <- dplyr::mutate(res_prob, cov90 = NA_real_)
#     }
#   }
#   list(point = res_point, prob = res_prob)
# }



# # counts +data_w# counts + incidence + coverage/WIS table (per fold and overall)
# make_metrics_counts_incidence_prob <- function(res_point_df, res_prob_df,
#                                                data_df, per = 1e5, pop_floor = 1) {
#   data_df <- arrange(data_df, adm_0_name, time_seq)
#   # Join truth/pop to point preds
#   df <- res_point_df %>%
#     left_join(
#       mutate(data_df, row_id = dplyr::row_number()) %>%
#         select(row_id, truth = dengue_total, pop_est),
#       by = "row_id"
#     ) %>%
#     filter(!is.na(truth), !is.na(pop_est), pop_est >= pop_floor) %>%
#     mutate(
#       truth_inc = per * truth / pop_est,
#       pred_inc  = per * pmax(pred, 0) / pop_est
#     )
#
#
#
#   mae_vec <- function(p, y) mean(abs(p - y), na.rm = TRUE)
#   rmse_vec <- function(p, y) sqrt(mean((p - y)^2, na.rm = TRUE))
#
#   metrics_counts_inc <- df %>%
#     summarise(
#       n_test = n(),
#       MAE_counts = mae_vec(pred, truth),
#       RMSE_counts = rmse_vec(pred, truth),
#       MAE_inc = mae_vec(pred_inc, truth_inc),
#       RMSE_inc = rmse_vec(pred_inc, truth_inc),
#       .by = c(model, mask_type, fold)
#     )
#
#   metrics_prob <- res_prob_df %>%
#     select(model, mask_type, fold, n_test, cov80, cov90, WIS)
#
#   fold_joined <- left_join(metrics_counts_inc, metrics_prob,
#     by = c("model", "mask_type", "fold"), multiple = "error"
#   )
#
#   overall <- fold_joined %>%
#     summarise(
#       n_test = sum(n_test.x, na.rm = TRUE), # from counts/inc block
#       MAE_counts = weighted.mean(MAE_counts, w = n_test.x),
#       RMSE_counts = weighted.mean(RMSE_counts, w = n_test.x),
#       MAE_inc = weighted.mean(MAE_inc, w = n_test.x),
#       RMSE_inc = weighted.mean(RMSE_inc, w = n_test.x),
#       cov80 = weighted.mean(cov80, w = n_test.x, na.rm = TRUE),
#       cov90 = weighted.mean(cov90, w = n_test.x, na.rm = TRUE),
#       WIS = weighted.mean(WIS, w = n_test.x, na.rm = TRUE),
#       .by = c(model, mask_type)
#     ) %>%
#     arrange(mask_type, model)
#
#   list(fold = fold_joined, overall = overall)
# }




# assemble `res` = point preds + truth/pop/etc.
# assemble_res <- function(out_dir = "runs/inla_full_CV_monthly_eval_runs", data_df, keep_cols = c(
#   "adm_0_name", "calendar_start_date", "dengue_total", "pop_est", "week", "Year"
# )) {
#   pts <- read_inla_point(out_dir)
#   if (!nrow(pts)) return(tibble())  # nothing run yet
#
#   base <- data_df %>%
#     mutate(row_id = dplyr::row_number()) %>%
#     select(all_of(c("row_id", keep_cols)))
#
#   res <- pts %>%
#     left_join(base, by = "row_id") %>%
#     rename(truth = dengue_total) %>%
#     relocate(model, mask_type, fold, row_id, pred, truth)
#
#   res
# }

# # example usage
# res <- assemble_res(out_dir = "runs/inla_full_CV_monthly_eval_runs", data_df = data_m,
#                     keep_cols = c("adm_0_name", "calendar_start_date", "dengue_total", "pop_est", "month", "Year"))
# # (optional) also bring in per-fold prob metrics
# res_prob <- read_inla_prob(out_dir = "runs/inla_full_CV_monthly_eval_runs")
