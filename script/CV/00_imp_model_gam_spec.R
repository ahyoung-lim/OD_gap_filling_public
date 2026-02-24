gam_w_bench_original <- dengue_total ~
  s(Year, bs = "re") +
  s(week, k = 2) + # tiny cyclic seasonality
  s(time_seq) + # tiny global trend
  s(adm_0_name, bs = "re") # country intercepts


# Baseline (tiny, fast)
gam_m_bench_original <- dengue_total ~
  s(Year, bs = "re") +
  s(month, k = 2) + # tiny cyclic seasonality
  s(time_seq) + # tiny global trend
  s(adm_0_name, bs = "re") # country intercepts

gam_m_bench_original2 <- dengue_total ~
  s(Year, bs = "re") +
  s(month, k = 2) + # tiny cyclic seasonality
  s(time_seq, by = adm_0_name) +
  s(adm_0_name, bs = "re") # country intercepts

gam_m_bench_min <- dengue_total ~
  s(Year, bs = "re") +
  s(month, bs = "cc", k = 6) + # tiny cyclic seasonality
  s(time_seq, bs = "cr", k = 10) + # tiny global trend
  s(adm_0_name, bs = "re") # country intercepts

# Shared seasonality + shrunken country trend deviations (global)
gam_m_shared <- dengue_total ~
  s(Year, bs = "re") +
  s(month, bs = "cc", k = 6) + # shared backbone
  s(time_seq, bs = "cr", k = 10) + # global trend
  s(time_seq, by = adm_0_name, bs = "fs", k = 6, m = 2) + # shrunken per-country trend
  s(adm_0_name, bs = "re")







# ---- helper: predictive quantiles/draws for mgcv::bam via link-Normal approx ----
gam_predict_q <- function(fit, newdata, nsamp = 500,
                          method = c("coef", "link"),
                          fam = c("poisson", "nb")) {
  method <- match.arg(method)
  fam <- match.arg(fam)

  # NB 'size' (theta) if needed
  theta <- NA_real_
  if (fam == "nb") {
    theta <- tryCatch(
      if (!is.null(fit$family$theta)) fit$family$theta else fit$family$getTheta(TRUE),
      error = function(e) NA_real_
    )
  }

  draw_counts <- function(mu_vec) {
    if (fam == "poisson" || !is.finite(theta) || theta <= 0) {
      rpois(length(mu_vec), mu_vec)
    } else {
      rnbinom(length(mu_vec), mu = mu_vec, size = theta)
    }
  }

  if (method == "link") {
    # Link-Normal jitter (fast). Use unconditional=TRUE to widen SEs.
    pr <- predict(fit, newdata = newdata, type = "link", se.fit = TRUE, unconditional = TRUE)
    eta <- as.numeric(pr$fit)
    se <- pmax(as.numeric(pr$se.fit), 1e-8)
    n <- length(eta)
    qs <- vapply(1:n, function(i) {
      mu_s <- exp(rnorm(nsamp, eta[i], se[i]))
      y_s <- draw_counts(mu_s)
      stats::quantile(y_s, probs = c(0.10, 0.50, 0.90), names = FALSE, type = 8)
    }, numeric(3))
    t(qs)
  } else {
    # Coefficient simulation (recommended; better calibration).
    stopifnot(requireNamespace("mvtnorm", quietly = TRUE))
    X <- predict(fit, newdata = newdata, type = "lpmatrix")
    b0 <- coef(fit)
    Vb <- vcov(fit, unconditional = TRUE)
    B <- mvtnorm::rmvnorm(nsamp, mean = b0, sigma = Vb) # nsamp x p
    Eta <- X %*% t(B) # n x nsamp
    qs <- apply(Eta, 1, function(eta_row) {
      mu_s <- exp(eta_row)
      y_s <- draw_counts(mu_s)
      stats::quantile(y_s, probs = c(0.10, 0.50, 0.90), names = FALSE, type = 8)
    })
    t(qs)
  }
}

fit_fold_gam <- function(data, mask, fold_id, formula,
                         y_col = "dengue_total",
                         family = mgcv::nb(),
                         knots = NULL,
                         train_policy = "inclusive",
                         nthreads = 4,
                         # NEW:
                         nsamp = 500,
                         method = c("coef", "link")) {
  method <- match.arg(method)

  if (!"row_id" %in% names(data)) data <- dplyr::mutate(data, row_id = dplyr::row_number())
  sp <- get_fold_split(mask, fold_id = fold_id, train_policy = train_policy)
  if (length(sp$test_idx) == 0) {
    return(tibble::tibble(
      fold = fold_id, row_id = integer(0), pred = numeric(0),
      q10 = numeric(0), q50 = numeric(0), q90 = numeric(0)
    ))
  }

  tr <- data[sp$train_idx, , drop = FALSE]
  te <- data[sp$test_idx, , drop = FALSE]

  # freeze factor levels
  tr$adm_0_name <- factor(tr$adm_0_name)
  te$adm_0_name <- factor(te$adm_0_name, levels = levels(tr$adm_0_name))
  if ("Year" %in% names(tr)) {
    tr$Year <- factor(tr$Year)
    te$Year <- factor(te$Year, levels = levels(tr$Year))
  }
  if ("region" %in% names(tr)) {
    tr$region <- factor(tr$region)
    te$region <- factor(te$region, levels = levels(tr$region))
  }

  fit <- mgcv::bam(
    update(formula, reformulate(".", response = "y")),
    data = dplyr::mutate(tr, y = .data[[y_col]]),
    family = family,
    method = "fREML",
    discrete = TRUE,
    select = TRUE,
    nthreads = nthreads,
    knots = knots,
    na.action = na.exclude
  )

  # only rows with known factor levels
  ok <- rep(TRUE, nrow(te))
  if ("adm_0_name" %in% names(te)) ok <- ok & !is.na(te$adm_0_name)
  if ("Year" %in% names(te)) ok <- ok & !is.na(te$Year)
  if ("region" %in% names(te)) ok <- ok & !is.na(te$region)

  pred <- rep(NA_real_, nrow(te))
  q10 <- q50 <- q90 <- rep(NA_real_, nrow(te))

  if (any(ok)) {
    # point prediction
    pred[ok] <- as.numeric(predict(fit, newdata = te[ok, , drop = FALSE], type = "response"))
    # predictive quantiles for WIS/coverage (per-row)
    fam_lbl <- if (inherits(family, "family.nb")) "nb" else "poisson"
    Q <- gam_predict_q(fit, te[ok, , drop = FALSE], nsamp = nsamp, method = method, fam = fam_lbl)
    q10[ok] <- Q[, 1]
    q50[ok] <- Q[, 2]
    q90[ok] <- Q[, 3]
  }

  tibble::tibble(
    fold = fold_id,
    row_id = te$row_id,
    pred = pred,
    q10 = q10, q50 = q50, q90 = q90
  )
}

run_job_gam <- function(model, mask_type, fold, seed = 20250811,
                        nsamp = 500, method = "coef") {
  start <- Sys.time()
  set.seed(seed)
  mask <- masks[[mask_type]]
  formula <- formulas_gam[[model]]

  out <- fit_fold_gam(
    data = data, mask = mask, fold_id = fold, formula = formula,
    family = mgcv::nb(), nthreads = 1,
    nsamp = nsamp, method = method
  )

  end <- Sys.time()
  sp <- get_fold_split(mask, fold_id = fold)

  dplyr::mutate(
    out,
    model = model, mask_type = mask_type, fold = fold,
    n_train = length(sp$train_idx), n_test = length(sp$test_idx),
    dur_sec = as.numeric(difftime(end, start, units = "secs"))
  )
}
