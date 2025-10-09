# pred_down <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_out.csv"))
#
#
# pred_down <- pred_down %>%
#   select(-imputed_weekly) %>%
#   mutate(id = paste0(adm_0_name, Year, month)) %>%
#   merge(., df, by = "id")
#
# summary(pred_down$imputed_monthly)
# summary(pred_down$imputed_weekly)
#
# names(pred_down)
#
# summary(is.na(pred_down$dengue_total))
# summary(pred_down$imputed_weekly)
# summary(pred_down$imputed_monthly)
# summary(pred_down$annual_total)
#
# pred_down <- pred_down %>%
#   group_by(adm_0_name, Year) %>%
#   mutate(dengue_total = case_when(
#     annual_total == 0 ~ 0,
#     TRUE ~ dengue_total
#   ))
#
# write.csv(pred_down, "runs/pred/pred_downscale_to_rescale.csv", row.names = F)

#
# pred_down <- pred_down %>%
#   group_by(adm_0_name, Year) %>%
#   mutate(pred_sum = sum(pred_mean, na.rm = T))%>%
#   ungroup()%>%
#   mutate(dengue_total_scaled = case_when(
#     is.na(dengue_total)~ as.integer(pred_mean*(annual_total/pred_sum)),
#     annual_total == 0 ~ 0,
#     TRUE ~ dengue_total
#     )
#   )
#
# summary(pred_down$dengue_total_scaled)

set.seed(123)
df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_out_V1.csv"))
# df <- pred_down
# fallback defaults if columns are missing for any reason
if (!"imputed_weekly" %in% names(df)) df$imputed_weekly <- FALSE
if (!"imputed_monthly" %in% names(df)) df$imputed_monthly <- FALSE


fit <- readRDS(file.path(getwd(), "runs/pred/pred_downscale_fit_V1.rds"))

# Safety check: if config info is missing, you can't sample the posterior
if (is.null(fit$misc$configs)) {
  stop("This 'fit' was saved without config=TRUE. Refit the final monthly model with config=TRUE and saveRDS again.")
}

# Read the data frame that matches the fit's row order
# df <- read.csv(file.path(getwd(), "runs/pred/pred_downscale_out.csv"))

# Ensure flags exist (conservative defaults = don't lock unless sure it's truly observed)
for (nm in c("imputed_weekly", "imputed_monthly")) {
  if (!nm %in% names(df)) df[[nm]] <- FALSE
}

# 1) Align df to the row order INLA used
# Align df to EXACT order INLA used
align_df_to_fit <- function(fit, df, key_cols = c("adm_0_name", "Year", "month")) {
  df_fit <- as.data.frame(fit$.args$data, stringsAsFactors = FALSE)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # coerce key types
  if ("Year" %in% key_cols) {
    df$Year <- as.integer(df$Year)
    df_fit$Year <- as.integer(df_fit$Year)
  }
  if ("month" %in% key_cols) {
    df$month <- as.integer(df$month)
    df_fit$month <- as.integer(df_fit$month)
  }

  stopifnot(all(key_cols %in% names(df_fit)), all(key_cols %in% names(df)))

  # (optional) check uniqueness of keys in df
  if (any(duplicated(df[key_cols]))) stop("Duplicate keys in df; add another key (e.g., time_seq).")

  key_fit <- do.call(paste, c(df_fit[key_cols], sep = "\r"))
  key_df <- do.call(paste, c(df[key_cols], sep = "\r"))
  ord <- match(key_fit, key_df)
  if (anyNA(ord)) {
    stop("Alignment failed: ", sum(is.na(ord)), " fit-rows not found in df. Check keys & types.")
  }
  out <- df[ord, , drop = FALSE]
  stopifnot(nrow(out) == nrow(df_fit))
  out
}
df <- align_df_to_fit(fit, df)



# -------------------------
# 1) Posterior sampling
# -------------------------
INLA::inla.setOption(num.threads = 8)
samples <- INLA::inla.posterior.sample(
  n = 300, result = fit
)


# Find the predictor rows once
pred_idx <- grep("^Predictor", rownames(samples[[1]]$latent))
N <- nrow(df)
stopifnot(length(pred_idx) == N)


## 0) Build what you really need from `samples`, then free it
n_pred <- nrow(fit$.args$data)
R <- 300
Ydraw <- matrix(0L, nrow = N, ncol = R)
pred_names <- paste0("Predictor:", 1:n_pred)

# η (latent predictor) → μ per draw, as a dense N×R matrix (usually ~100 MB, not 1+ GB)
eta_mat <- vapply(samples, function(s) as.numeric(s$latent[pred_names, 1]), numeric(n_pred))
mu_mat <- pmax(exp(eta_mat), 0)

# size per draw (Poisson fallback if absent)
size_vec <- vapply(samples, function(s) {
  hp <- s$hyperpar
  j <- grep("size.*nbinom|size.*nbinomial|size.*negative", names(hp), ignore.case = TRUE)
  if (length(j) > 0) as.numeric(hp[j[1]]) else Inf
}, numeric(1))

saveRDS(samples, "runs/pred/samples_V1.rds")
rm(samples)
gc() # <<< critical: don't export the 1.08 GB list to workers


# -------------------------
# 2) Simulate predictive counts with locking and raking
# -------------------------
lock_obs <- (!is.na(df$dengue_total)) & !df$imputed_weekly & !df$imputed_monthly
dengue_obs <- as.integer(df$dengue_total)

# group indices once
cy <- as.integer(as.factor(interaction(df$adm_0_name, df$Year, drop = TRUE)))
idxG <- split(seq_len(n_pred), cy)
annualG <- as.numeric(tapply(df$annual_total, cy, \(x) unique(x)[1]))

rake_one <- function(y, mu, idxG, annualG, lock_obs, dengue_obs) {
  y[lock_obs] <- dengue_obs[lock_obs]
  for (g in seq_along(idxG)) {
    ix <- idxG[[g]]
    at <- annualG[g]
    if (is.na(at)) next

    locked_g <- lock_obs[ix]
    free_ix <- ix[!locked_g]
    locked_sum <- sum(y[ix[locked_g]])

    budget <- at - locked_sum
    if (budget <= 0) {
      y[free_ix] <- 0
      next
    }

    s_free <- sum(y[free_ix])
    if (s_free == 0) {
      p <- mu[free_ix]
      if (sum(p) == 0) p[] <- 1 / length(free_ix) else p <- p / sum(p)
      y[free_ix] <- as.numeric(rmultinom(1, size = budget, prob = p))
    } else {
      k <- budget / s_free
      vals <- floor(y[free_ix] * k)
      rem <- budget - sum(vals)
      if (rem > 0) {
        resid <- y[free_ix] * k - vals
        addix <- order(resid, decreasing = TRUE)[seq_len(rem)]
        vals[addix] <- vals[addix] + 1
      }
      y[free_ix] <- vals
    }
  }
  y
}

R_actual <- ncol(mu_mat)
stopifnot(length(size_vec) == R_actual)

for (r in seq_len(R_actual)) {
  mu <- mu_mat[, r]
  sz <- size_vec[r]
  y <- if (is.finite(sz)) rnbinom(length(mu), mu = mu, size = sz) else rpois(length(mu), mu)
  Ydraw[, r] <- rake_one(y, mu, idxG, annualG, lock_obs, dengue_obs)
}

row_mean <- rowMeans(Ydraw)
row_lwr <- apply(Ydraw, 1, quantile, probs = 0.025, type = 8)
row_upr <- apply(Ydraw, 1, quantile, probs = 0.975, type = 8)



# numeric point estimates first
y_mean <- ifelse(lock_obs, dengue_obs, row_mean)

# init integer vector; lock observed months now
y_int <- numeric(length(y_mean))
y_int[lock_obs] <- dengue_obs[lock_obs]

# group indices (same 'cy' you use elsewhere)
cy <- interaction(df$adm_0_name, df$Year, drop = TRUE)
idxs <- split(seq_len(nrow(df)), cy)

for (ix in idxs) {
  at <- unique(df$annual_total[ix])
  if (length(at) != 1L || is.na(at)) next

  locked_g <- lock_obs[ix]
  free_ix <- ix[!locked_g]

  # budget for free months after fixing the locked ones
  budget <- at - sum(y_int[ix[locked_g]])
  if (budget <= 0 || length(free_ix) == 0L) {
    if (length(free_ix)) y_int[free_ix] <- 0
    next
  }

  # floor of means, then distribute the remainder by largest fractional parts
  vals <- floor(pmax(y_mean[free_ix], 0))
  rem <- budget - sum(vals)

  if (rem > 0) {
    frac <- pmax(y_mean[free_ix] - vals, 0)
    add <- order(frac, decreasing = TRUE)[seq_len(rem)]
    vals[add] <- vals[add] + 1
  }
  y_int[free_ix] <- vals
}

# save outputs
df$dengue_total_scaled <- y_int # integer point estimate, group sums == annual_total
df$dengue_lwr_scaled <- ifelse(lock_obs, dengue_obs, floor(row_lwr))
df$dengue_upr_scaled <- ifelse(lock_obs, dengue_obs, ceiling(row_upr))
df$disaggregated_yearly <- ifelse((!df$imputed_monthly & !df$imputed_weekly) & is.na(df$dengue_total), TRUE, FALSE)
# sanity: exact match (no tolerance needed)
by_cy <- split(seq_len(nrow(df)), interaction(df$adm_0_name, df$Year, drop = TRUE))

sum_point <- vapply(by_cy, function(ix) sum(df$dengue_total_scaled[ix]), numeric(1))
annual <- vapply(by_cy, function(ix) unique(df$annual_total[ix])[1], numeric(1))

sum_point <- unname(as.numeric(sum_point))
annual <- unname(as.numeric(annual))
stopifnot(identical(sum_point, annual))

names(df)
df_clean <- df %>%
  select(adm_0_name, ISO_A0, Year, month, time_seq, pop_est, lat_band, dengue_total_scaled:dengue_upr_scaled, imputed_weekly, imputed_monthly, disaggregated_yearly)
write.csv(df_clean, "runs/pred/pred_downscale_with_ci_V1.csv", row.names = F)
