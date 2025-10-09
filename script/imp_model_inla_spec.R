# WEEKLY ------------------------------------#
inla_w_formula <- y ~ 1 +
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +
  f(week,
    model = "rw1", replicate = countryx, cyclic = TRUE,
    constr = TRUE, scale.model = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(2, 0.01)))
  ) +
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +
  f(countryx, model = "iid", constr = TRUE)


inla_w_shared_formula <- y ~ 1 +
  # Temporal dependence
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +
  # (1) Global weekly seasonality (shared across all countries)
  f(week_g,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(0.7, 0.01)) # moderate smoothness penalty
    )
  ) +
  # (2) Country-specific deviation around the global curve
  f(week,
    model = "rw1", cyclic = TRUE, replicate = countryx,
    constr = TRUE, scale.model = TRUE,
    hyper = list(
      # Stronger shrinkage: most mass on small SD for deviations
      prec = list(prior = "pc.prec", param = c(0.3, 0.01))
    )
  ) +
  # Year effect and country intercept if you still want them
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +
  f(countryx, model = "iid", constr = TRUE)

inla_w_hier_shared_formula <-
  y ~ 1 +

  # Within-country serial dependence over weeks (global time index)
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +

  # (1) GLOBAL weekly seasonal backbone: one cyclic RW1 over 52 weeks shared by all
  f(week_shared,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(2, 0.01)))
  ) +

  # (2) REGION deviations from the global backbone (one curve per region; moderate shrinkage)
  f(week_dev,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    replicate = regionx,
    hyper = list(prec = list(prior = "pc.prec", param = c(1.0, 0.01)))
  ) +

  # (3) COUNTRY deviations from the region curve (one curve per country; strong shrinkage)
  f(week_dev_country,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    replicate = countryx,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.3, 0.01)))
  ) +

  # Slow across-year structure per country (optional; drop in CV if collinear with time_seq)
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +

  # Country intercepts
  f(countryx,
    model = "iid", constr = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
  )




# MONTHLY ------------------------------------#
inla_m_formula <- y ~ 1 +
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +
  f(month,
    model = "rw1", replicate = countryx, cyclic = TRUE,
    constr = TRUE, scale.model = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(2, 0.01)))
  ) +
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +
  f(countryx, model = "iid", constr = TRUE)

inla_m_shared_formula <-
  y ~ 1 +
  # serial correlation within each country
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +
  # (1) GLOBAL seasonal pattern shared by all countries
  f(month_shared,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(2, 0.01))) # modest smoothing/shrinkage
  ) +
  # (2) COUNTRY-SPECIFIC seasonal DEVIATIONS (shrunk hard toward 0)
  f(month_dev,
    model = "rw1", cyclic = TRUE, replicate = countryx,
    constr = TRUE, scale.model = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.2, 0.01))) # strong shrinkage of deviations
  ) +
  # slow across-year structure per country (keep if helpful; drop if collinear with time_seq)
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +
  # country intercepts (can also shrink a bit if you wish)
  f(countryx,
    model = "iid", constr = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
  )


inla_m_region_shared_formula <-
  y ~ 1 +
  # Serial correlation inside each country
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +
  # (1) REGION-LEVEL shared seasonal curve (one 12-month RW1 per region)
  f(month_shared,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    replicate = regionx, # <- key: shared within region
    hyper = list(prec = list(prior = "pc.prec", param = c(2, 0.01))) # moderate smoothing
  ) +
  # (2) COUNTRY-SPECIFIC seasonal DEVIATIONS (shrunk hard toward 0)
  f(month_dev,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    replicate = countryx,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.2, 0.01))) # strong shrinkage
  ) +
  # Slow across-year structure (keep if helpful)
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +
  # Country intercepts (slight shrinkage ok)
  f(countryx,
    model = "iid", constr = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
  )


inla_m_hier_shared_formula <-
  y ~ 1 +
  # within-country serial dependence
  f(time_seq,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(2, 0.01))
    )
  ) +
  # GLOBAL seasonal backbone (one 12-month RW1 shared by all)
  f(month_shared,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(2, 0.01)))
  ) +
  # REGION deviation from global (one curve per region, moderately shrunk)
  f(month_dev,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    replicate = regionx,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.7, 0.01)))
  ) +
  # COUNTRY deviation from region (strongly shrunk)
  f(month_dev_country,
    model = "rw1", cyclic = TRUE, constr = TRUE, scale.model = TRUE,
    replicate = countryx,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.25, 0.01)))
  ) +
  # slow across-year structure per country (optional if collinear with time_seq)
  f(yearx,
    model = "ar1", replicate = countryx, constr = TRUE,
    hyper = list(
      rho  = list(prior = "pc.cor1", param = c(0.8, 0.9)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
  ) +
  f(countryx,
    model = "iid", constr = TRUE,
    hyper = list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
  )



ctrl_fam <- list(
  family = "nbinomial",
  control.family = list(
    hyper = list(
      size = list(prior = "loggamma", param = c(5, 0.1))
    )
  )
)


# functions for model runs -------------------------------#


# run_job_inla <- function(model, mask_type, fold, seed = 20250811) {
#   start <- Sys.time()
#
#   # DO NOT set num.threads here anymore (we set it globally above)
#   # INLA::inla.setOption(num.threads = "1:1")  # <- removed
#
#   set.seed(seed) # or set once outside if you prefer
#
#   mask <- masks[[mask_type]]
#   formula <- formulas[[model]]
#
#   out <- fit_fold_inla(
#     data = data,
#     mask = mask,
#     formula = formula,
#     fold_id = fold,
#     ctrl_fam = ctrl_fam,
#     train_policy = "inclusive"
#   )
#
#   end <- Sys.time()
#   sp <- get_fold_split(mask, fold_id = fold)
#
#   data_id <- get_attr(mask, "data_id", NA_character_)
#   stride_val <- get_attr(mask, "stride", NA_integer_)
#   k_eval_val <- get_attr(mask, "k_eval", NA_integer_)
#   bmode_val <- get_attr(mask, "boundary_mode", NA_character_)
#   dir_val <- get_attr(mask, "direction", NA_character_)
#   cv_val <- get_attr(mask, "cv_kind", NA_character_)
#   gap_val <- get_attr(mask, "gap_len", NA_integer_)
#   edge_val <- get_attr(mask, "edge", NA_integer_)
#   test_fracval <- get_attr(mask, "test_frac", NA_real_)
#
#   dur_sec <- as.numeric(difftime(end, start, units = "secs"))
#
#   dplyr::mutate(
#     out,
#     model = model, mask_type = mask_type, fold = fold, data_id = data_id,
#     n_train = length(sp$train_idx), n_test = length(sp$test_idx),
#     cv_kind = cv_val, direction = dir_val,
#     gap_len = gap_val, edge = edge_val,
#     boundary_mode = bmode_val, k_eval = k_eval_val,
#     stride = stride_val, test_frac = test_fracval,
#     dur_sec = dur_sec
#   )
#
#   fn <- sprintf("logs/inla_%s_%s_fold%s.csv", model, mask_type, fold)
#   readr::write_csv(
#     dplyr::distinct(
#       out, model, mask_type, fold, data_id, n_train, n_test,
#       cv_kind, direction,
#       gap_len, edge,
#       boundary_mode, k_eval, stride, test_frac, dur_sec
#     ),
#     file = fn
#   )
# }


# for probabilistic model performance metrics
# run_job_inla_joint <- function(model, mask_type, fold,
#                                data, masks, formulas, ctrl_fam,
#                                train_policy = "inclusive",
#                                nsamp = 400,
#                                alphas = c(0.1, 0.2, 0.3, 0.4),
#                                seed = 20250811,
#                                out_dir = "runs") {
#   # ---- paths (resume-safe) ----
#   dir_point <- file.path(out_dir, "inla_point")
#   dir_prob <- file.path(out_dir, "inla_prob")
#   dir.create(dir_point, recursive = TRUE, showWarnings = FALSE)
#   dir.create(dir_prob, recursive = TRUE, showWarnings = FALSE)
#
#   fp_point <- file.path(dir_point, sprintf("%s_%s_fold%s.csv", model, mask_type, fold))
#   fp_prob <- file.path(dir_prob, sprintf("%s_%s_fold%s.csv", model, mask_type, fold))
#
#   # If both artifacts exist, just return them (resume)
#   if (file.exists(fp_point) && file.exists(fp_prob)) {
#     point <- readr::read_csv(fp_point, show_col_types = FALSE)
#     prob <- readr::read_csv(fp_prob, show_col_types = FALSE)
#     return(list(point = point, prob = prob))
#   }
#
#   set.seed(seed)
#   INLA::inla.setOption(num.threads = "1:1") # safe even in sequential runs
#
#   if (!"row_id" %in% names(data)) data <- dplyr::mutate(data, row_id = dplyr::row_number())
#   mask <- masks[[mask_type]]
#   formula <- formulas[[model]]
#
#   sp <- get_fold_split(mask, fold_id = fold, train_policy = train_policy)
#   if (length(sp$test_idx) == 0L) {
#     point <- tibble::tibble(model, mask_type, fold, row_id = integer(0), pred = numeric(0))
#     prob <- tibble::tibble(model, mask_type, fold, n_test = 0, cov80 = NA_real_, cov90 = NA_real_, WIS = NA_real_)
#     readr::write_csv(point, fp_point)
#     readr::write_csv(prob, fp_prob)
#     return(list(point = point, prob = prob))
#   }
#
#   train <- data[sp$train_idx, ] %>% dplyr::mutate(y = dengue_total)
#   test <- data[sp$test_idx, ] %>% dplyr::mutate(y = NA_real_)
#   df <- dplyr::bind_rows(train, test)
#
#   fit <- INLA::inla(
#     update(formula, y ~ .),
#     data = df,
#     family = ctrl_fam$family,
#     control.family = ctrl_fam$control.family,
#     control.predictor = list(compute = TRUE, link = 1),
#     control.compute = list(dic = FALSE, waic = FALSE, config = TRUE), # <- config=TRUE for sampling
#     control.inla = list(strategy = "simplified.laplace", int.strategy = "eb")
#   )
#
#   # ---- point predictions ----
#   hold_df_idx <- which(is.na(df$y)) # indices within 'df' for TEST rows
#   point <- tibble::tibble(
#     model = model,
#     mask_type = mask_type,
#     fold = fold,
#     row_id = df$row_id[hold_df_idx],
#     pred = fit$summary.fitted.values$mean[hold_df_idx]
#   )
#
#   # ---- predictive sampling (only Predictor nodes we need) ----
#   samp <- INLA::inla.posterior.sample(
#     nsamp, fit,
#     seed = seed,
#     selection = list(Predictor = hold_df_idx)
#   )
#
#   # Build matrix of predictive draws: eta -> mu -> Y (Poisson; use NB draws if NB family)
#   S <- length(samp)
#   N <- length(hold_df_idx)
#   Ys <- matrix(NA_real_, nrow = N, ncol = S)
#   for (s in seq_len(S)) {
#     # selection returns only Predictors; 'latent' is a matrix with the requested nodes
#     eta <- as.numeric(samp[[s]]$latent[, 1L]) # single column for Predictor
#     mu <- exp(eta)
#     Ys[, s] <- rpois(N, mu)
#   }
#
#   # Quantiles for coverage / WIS
#   q_med <- apply(Ys, 1, stats::quantile, probs = 0.5, na.rm = TRUE)
#   q80 <- apply(Ys, 1, stats::quantile, probs = c(0.10, 0.90), na.rm = TRUE)
#   q90 <- apply(Ys, 1, stats::quantile, probs = c(0.05, 0.95), na.rm = TRUE)
#   q_lo <- sapply(alphas, function(a) apply(Ys, 1, stats::quantile, probs = a / 2, na.rm = TRUE))
#   q_hi <- sapply(alphas, function(a) apply(Ys, 1, stats::quantile, probs = 1 - a / 2, na.rm = TRUE))
#
#   y_true <- data$dengue_total[df$row_id[hold_df_idx]]
#
#   prob <- tibble::tibble(
#     model = model,
#     mask_type = mask_type,
#     fold = fold,
#     n_test = length(y_true),
#     cov80 = coverage_rate(y_true, q80[1, ], q80[2, ]),
#     cov90 = coverage_rate(y_true, q90[1, ], q90[2, ]),
#     WIS = mean(wis_from_quantiles(y_true, q_med, q_lo, q_hi, alphas), na.rm = TRUE)
#   )
#
#   # ---- save artifacts (resume-safe) ----
#   readr::write_csv(point, fp_point)
#   readr::write_csv(prob, fp_prob)
#
#   list(point = point, prob = prob)
# }
