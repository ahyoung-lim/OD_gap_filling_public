# ==============================================================================
# MI GROWTH-RATE POOLING — propagate multiple-imputation uncertainty into the
# main-text GROWTH-RATE and FOLD-INCREASE numbers. (Totals live in the pipeline's
# aggregate block: runs/mi_full/descriptive_summary/aggregate_totals_MI.csv — a single source, computed
# once from one draw matrix; this script does NOT recompute them. The fold
# increase is the ratio of two of those annual totals and is computed here so
# that it is reported next to the growth rates.)
# ==============================================================================
# Runs the growth-rate calculations — global and per-period rates, and the
# per-country rates — across the MI imputed datasets and pools them.
# Pooling rule is chosen PER METRIC, and for the model-based metrics BOTH intervals
# are reported so they can be compared.
#
# SCALE OF POOLING: every Rubin-pooled quantity is pooled on the scale on which
# the glm estimates it and its SE are approximately normal — the log-scale slope
# b (and, for the fitted curves, the log-scale linear predictor) — and only then
# transformed to %/yr via (exp(b) - 1) * 100 (or to counts via exp). Pooling the
# transformed %/yr values directly would assume a symmetric sampling distribution
# on a right-skewed scale; for large slopes (countries whose series start near
# zero) that gives symmetric intervals whose lower bound can fall below -100%.
# Pooling on the log scale keeps the interval asymmetric and bounded below by
# -100%. The percentile (between-imputation) intervals are quantiles and are
# unaffected by the choice of scale.
#
#   metric              pooling                                              m
#   ------------------  ---------------------------------------------------  ----
#   global_growth_pct   Rubin's rules (within+between) + percentile (btwn)    50
#   period_growth_pct   Rubin's rules (within+between) + percentile (btwn)    50
#   region_growth_pct   percentile only — the estimand is a MEDIAN of per-    50
#                       country rates, which has no clean per-dataset SE.
#                       NOT a manuscript-cited number: the fig3b/supp-fig
#                       median lines are recomputed from the plotted pooled
#                       dots, and this row is only their console cross-check.
#                       Its interval is the between-imputation spread only
#                       (percentile of the 50 per-dataset medians), not a
#                       full 95% CI.
#   region_total_growth_pct
#                       Rubin's rules — quasi-Poisson slope of each region's
#                       SUMMED yearly series (the fig3a panel metric; distinct
#                       from region_growth_pct, the median of per-country rates)
#   region_growth_since_emergence_pct
#                       percentile only — same estimand, model and inclusion
#                       rule as region_growth_pct, but each country's series
#                       starts at its first year with cases, so the leading
#                       zeros are excluded. Sensitivity: how much do the
#                       regional growth rates depend on the leading zeros?
#                       The full post-emergence period (to 2024) is used
#                       rather than an emergence-to-recent-peak window, to
#                       avoid the upward bias of a hand-picked peak endpoint.
#                       Console-check only, like region_growth_pct.
#   global_growth_since_emergence_pct, period_growth_since_emergence_pct,
#   region_total_growth_since_emergence_pct
#                       Rubin's rules — the summed-series slopes above, with
#                       each country's rows before its first year with cases
#                       dropped before summation (so neither its zero cases
#                       nor its population enter those years). Sensitivity:
#                       do the assumed pre-emergence zeros inflate the
#                       aggregate growth rates? Since dropped cases are zeros,
#                       only the population denominator differs.               50
#   global_growth_excl_assumed_zero_pct, period_growth_excl_assumed_zero_pct,
#   region_total_growth_excl_assumed_zero_pct
#                       Rubin's rules — as the since_emergence variant, but
#                       only pre-emergence years whose zero is ASSUMED
#                       (data_source Assumed_zero_cases or first_year in
#                       dt_heatmap_calibrated.csv) are dropped; pre-emergence
#                       years with a REPORTED zero stay in the sum. Isolates
#                       the assumed zeros specifically.                        50
#   fold_increase       percentile only — annual total in 2024 / annual total   5000
#                       in the start year (1990, 2000, 2010). Point = ratio of
#                       the additive annual totals in aggregate_totals_MI.csv,
#                       so it matches the totals table exactly; interval = 2.5
#                       and 97.5 percentiles of the per-draw ratio, numerator
#                       and denominator taken from the SAME draw so the
#                       year-to-year correlation is carried. All 5000 pooled
#                       draws are used (a quantile needs no independence).
#                       An uncertainty interval, not a CI: observed annual
#                       counts are held fixed across draws, so only the
#                       imputation/disaggregation uncertainty is propagated.
#
# TERMINOLOGY: the ensemble is 50 imputations (pipeline runs); each run carries 100
# conditional posterior draws of the final disaggregation, giving 5000 pooled draws
# (100 conditional x 50 imputations). Rubin assumes
# independent imputations, so the model-based metrics use one draw per run (m = 50).
#
# CANONICAL FILE for paper citation: all GROWTH and FOLD numbers come from
# growth_pooled.csv; all TOTAL numbers come from the pipeline's
# aggregate_totals_MI.csv. Do not mix.
#
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

source("script/03c_mi_datasets.R")

# ------------------------------------------------------------------------------
# Per-country growth rate
# ------------------------------------------------------------------------------
calculate_aroc_full_period_poisson <- function(df, grouping_var = "od_region", min_years = 3) {
  df %>%
    filter(!is.na(incidence_per_100k), !is.na(Year), !is.na(total_pop), !is.na(total_cases)) %>%
    group_by(adm_0_name, ISO_A0, !!sym(grouping_var)) %>%
    mutate(years_with_cases = sum(total_cases > 0)) %>%
    filter(years_with_cases >= min_years) %>%
    nest() %>%
    mutate(model_results = map(data, function(country_data) {
      tryCatch(
        {
          model <- glm(total_cases ~ Year + offset(log(total_pop)),
            data = country_data, family = quasipoisson(link = "log")
          )
          if (!model$converged) {
            return(NULL)
          }
          cs <- summary(model)$coefficients
          if (nrow(cs) < 2) {
            return(NULL)
          }
          # log-scale slope b and its SE are carried so the per-country
          # estimates can be Rubin-pooled on the log scale (see header);
          # annual_rate_of_change is the same slope expressed in %/yr
          data.frame(
            b = cs[2, 1], se_b = cs[2, 2],
            annual_rate_of_change = (exp(cs[2, 1]) - 1) * 100,
            p_value = cs[2, 4]
          )
        },
        error = function(e) NULL
      )
    })) %>%
    unnest(model_results) %>%
    select(-data) %>%
    ungroup() %>%
    filter(!is.na(annual_rate_of_change)) %>%
    rename(group = !!sym(grouping_var))
}

PERIODS <- list(
  "1990-2024" = c(1990, 2024),
  "2000-2024" = c(2000, 2024),
  "2010-2024" = c(2010, 2024)
)

# country-years whose annual total is an ASSUMED zero rather than a reported one:
# "Assumed_zero_cases" (assumed absence / outbreak-assumption records) and
# "first_year" (years before a country's documented first dengue year, set to
# zero in 01c). Reported zeros (OpenDengue or ad hoc sources) are not included.
# Used by the *_excl_assumed_zero_pct metrics below.
ASSUMED_ZERO_YEARS <- read.csv("data/processed_data/dt_heatmap_calibrated.csv",
                               stringsAsFactors = FALSE) %>%
  filter(data_source %in% c("Assumed_zero_cases", "first_year")) %>%
  transmute(adm_0_name = toupper(adm_0_name), Year = as.integer(Year)) %>%
  distinct()

# ------------------------------------------------------------------------------
# Pooling helpers
# ------------------------------------------------------------------------------
QN <- qnorm(0.975)
qv <- function(x, p) as.numeric(quantile(x[is.finite(x)], probs = p, names = FALSE, type = 8))

# Rubin's rules for a scalar estimand. Q = point estimates,
# U = within-imputation variances (SE^2 of Q on the scale it is pooled on).
rubin <- function(Q, U) {
  keep <- is.finite(Q) & is.finite(U)
  Q <- Q[keep]
  U <- U[keep]
  m <- length(Q)
  Qbar <- mean(Q)
  Ubar <- mean(U)
  B <- if (m > 1) stats::var(Q) else 0
  Tt <- Ubar + (1 + 1 / m) * B
  df <- if (B > 0) (m - 1) * (1 + Ubar / ((1 + 1 / m) * B))^2 else Inf
  tc <- if (is.finite(df)) stats::qt(0.975, df) else QN
  data.frame(
    m = m, point = Qbar, lwr = Qbar - tc * sqrt(Tt), upr = Qbar + tc * sqrt(Tt),
    within_sd = sqrt(Ubar), between_sd = sqrt(B)
  )
}

# log-scale slope -> %/yr; applied to the pooled point and interval bounds
to_pct <- function(b) (exp(b) - 1) * 100

# ------------------------------------------------------------------------------
# Per-dataset growth metrics (one draw per run, m = 50):
#   - aggregate global/period growth (fig3a) with the glm coefficient SE
#   - per-country -> regional median growth (fig3b)
# ------------------------------------------------------------------------------
one50 <- function(d) {
  gby_region <- d %>%
    group_by(Year, od_region) %>%
    summarise(
      total = sum(dengue_total_scaled, na.rm = TRUE),
      pop = sum(pop_est, na.rm = TRUE), .groups = "drop"
    )
  gby <- gby_region %>%
    group_by(Year) %>%
    summarise(total = sum(total), pop = sum(pop), .groups = "drop")

  fit_slope <- function(pd) {
    m <- glm(total ~ Year + offset(log(pop)), data = pd, family = quasipoisson(link = "log"))
    cs <- summary(m)$coefficients["Year", ]
    # log-scale slope and SE (pooled as such); pct is the %/yr point estimate,
    # kept for the percentile interval and the console
    data.frame(b = cs[1], se_b = cs[2], pct = to_pct(cs[1]), pval = cs[4])
  }
  glob <- cbind(metric = "global_growth_pct", scope = "global", fit_slope(gby))
  per <- do.call(rbind, lapply(names(PERIODS), function(pn) {
    yr <- PERIODS[[pn]]
    cbind(
      metric = "period_growth_pct", scope = pn,
      fit_slope(gby %>% filter(Year >= yr[1], Year <= yr[2]))
    )
  }))
  # each region's SUMMED yearly series, same slope model (the fig3a panel metric);
  # the fitted yearly curve is kept as well so the figure's trend line and band
  # can be pooled across imputations like the slope itself. The curve is kept on
  # the link (log) scale — linear predictor and its SE — so that it, too, is
  # Rubin-pooled where the estimate is approximately normal and only then
  # exponentiated to counts.
  reg_total_fit <- lapply(sort(unique(gby_region$od_region)), function(rr) {
    pd <- gby_region %>%
      filter(od_region == rr) %>%
      arrange(Year)
    m <- glm(total ~ Year + offset(log(pop)), data = pd, family = quasipoisson(link = "log"))
    cs <- summary(m)$coefficients["Year", ]
    pr <- predict(m, newdata = pd, type = "link", se.fit = TRUE)
    list(
      slope = data.frame(
        metric = "region_total_growth_pct", scope = rr,
        b = cs[1], se_b = cs[2], pct = to_pct(cs[1]), pval = cs[4]
      ),
      pred = data.frame(
        od_region = rr, Year = pd$Year,
        eta = as.numeric(pr$fit), se_eta = as.numeric(pr$se.fit)
      )
    )
  })
  reg_total <- do.call(rbind, lapply(reg_total_fit, `[[`, "slope"))
  reg_pred <- do.call(rbind, lapply(reg_total_fit, `[[`, "pred"))

  annual <- d %>%
    group_by(adm_0_name, ISO_A0, lat_band, od_region, Year) %>%
    summarise(
      total_cases = sum(dengue_total_scaled, na.rm = TRUE),
      total_pop = first(pop_est), .groups = "drop"
    ) %>%
    mutate(incidence_per_100k = (total_cases / total_pop) * 1e5)
  cty <- calculate_aroc_full_period_poisson(annual, "od_region", min_years = 6)
  reg <- cty %>%
    group_by(group) %>%
    summarise(med = median(annual_rate_of_change, na.rm = TRUE), .groups = "drop")

  # since-emergence variant: same regression on the same countries, but each
  # country's series is truncated to start at its first year with cases. The
  # emergence year is recomputed within each imputed dataset, so its uncertainty
  # propagates. Eligibility is unchanged (zero years are not active years).
  annual_emerg <- annual %>%
    group_by(adm_0_name) %>%
    filter(any(total_cases > 0), Year >= min(Year[total_cases > 0])) %>%
    ungroup()
  cty_emerg <- calculate_aroc_full_period_poisson(annual_emerg, "od_region", min_years = 6)
  reg_emerg <- cty_emerg %>%
    group_by(group) %>%
    summarise(med = median(annual_rate_of_change, na.rm = TRUE), .groups = "drop")
  # looser inclusion (>= 3 years with cases) for the supplementary figure that
  # shows recently emerging countries; the >= 6 set above matches fig3b
  cty_emerg3 <- calculate_aroc_full_period_poisson(annual_emerg, "od_region", min_years = 3)

  # since-emergence variant of the SUMMED-series slopes (global, period, region):
  # each country's rows before its first year with cases are dropped BEFORE the
  # yearly summation, so those years contribute neither their (zero) cases nor
  # their population. The summed case series is unchanged by this (the dropped
  # cases are zeros); the population denominator is what changes. The difference
  # from global_growth_pct / period_growth_pct / region_total_growth_pct is
  # therefore the effect of the assumed pre-emergence zeros on the aggregate
  # growth rates. Region-years in which no country has yet emerged are excluded
  # (no population to offset).
  gby_region_emerg <- annual_emerg %>%
    group_by(Year, od_region) %>%
    summarise(total = sum(total_cases), pop = sum(total_pop), .groups = "drop") %>%
    filter(pop > 0)
  gby_emerg <- gby_region_emerg %>%
    group_by(Year) %>%
    summarise(total = sum(total), pop = sum(pop), .groups = "drop")
  glob_emerg <- cbind(
    metric = "global_growth_since_emergence_pct", scope = "global",
    fit_slope(gby_emerg)
  )
  per_emerg <- do.call(rbind, lapply(names(PERIODS), function(pn) {
    yr <- PERIODS[[pn]]
    cbind(
      metric = "period_growth_since_emergence_pct", scope = pn,
      fit_slope(gby_emerg %>% filter(Year >= yr[1], Year <= yr[2]))
    )
  }))
  reg_total_emerg <- do.call(rbind, lapply(sort(unique(gby_region_emerg$od_region)), function(rr) {
    cbind(
      metric = "region_total_growth_since_emergence_pct", scope = rr,
      fit_slope(gby_region_emerg %>% filter(od_region == rr) %>% arrange(Year))
    )
  }))

  # narrower variant: drop only the pre-emergence years whose zero is ASSUMED
  # (ASSUMED_ZERO_YEARS); pre-emergence years with a reported zero stay in the
  # sum, as do any assumed-zero years after the first year with cases. Isolates
  # the effect of the assumed zeros specifically, as opposed to all leading zeros.
  annual_excl <- annual %>%
    group_by(adm_0_name) %>%
    mutate(first_case_year = if (any(total_cases > 0)) min(Year[total_cases > 0]) else NA_integer_) %>%
    ungroup() %>%
    left_join(ASSUMED_ZERO_YEARS %>% mutate(assumed = TRUE), by = c("adm_0_name", "Year")) %>%
    filter(!(coalesce(assumed, FALSE) & !is.na(first_case_year) & Year < first_case_year)) %>%
    select(-first_case_year, -assumed)
  gby_region_excl <- annual_excl %>%
    group_by(Year, od_region) %>%
    summarise(total = sum(total_cases), pop = sum(total_pop), .groups = "drop") %>%
    filter(pop > 0)
  gby_excl <- gby_region_excl %>%
    group_by(Year) %>%
    summarise(total = sum(total), pop = sum(pop), .groups = "drop")
  glob_excl <- cbind(
    metric = "global_growth_excl_assumed_zero_pct", scope = "global",
    fit_slope(gby_excl)
  )
  per_excl <- do.call(rbind, lapply(names(PERIODS), function(pn) {
    yr <- PERIODS[[pn]]
    cbind(
      metric = "period_growth_excl_assumed_zero_pct", scope = pn,
      fit_slope(gby_excl %>% filter(Year >= yr[1], Year <= yr[2]))
    )
  }))
  reg_total_excl <- do.call(rbind, lapply(sort(unique(gby_region_excl$od_region)), function(rr) {
    cbind(
      metric = "region_total_growth_excl_assumed_zero_pct", scope = rr,
      fit_slope(gby_region_excl %>% filter(od_region == rr) %>% arrange(Year))
    )
  }))
  n_dropped <- c(since_emergence = nrow(annual) - nrow(annual_emerg),
                 excl_assumed_zero = nrow(annual) - nrow(annual_excl))

  keep_cty <- function(d) d %>% select(adm_0_name, ISO_A0, group, b, se_b, annual_rate_of_change)
  list(
    model = rbind(glob, per, reg_total, glob_emerg, per_emerg, reg_total_emerg,
                  glob_excl, per_excl, reg_total_excl),
    n_dropped = n_dropped,
    region = reg, region_emerg = reg_emerg,
    country = keep_cty(cty), country_emerg = keep_cty(cty_emerg),
    country_emerg3 = keep_cty(cty_emerg3),
    region_pred = reg_pred
  )
}

# ------------------------------------------------------------------------------
# Run over the 50 imputations and pool
# ------------------------------------------------------------------------------
mi <- mi_load()
plan50 <- mi_sample_plan(mi, n_per_run = 1) # one draw per run
message(sprintf("growth pool: %d imputations (1 draw/run)", length(plan50)))

t0 <- Sys.time()
res <- lapply(seq_along(plan50), function(k) {
  if (k %% 10 == 0) {
    message(sprintf(
      "  dataset %d/%d (%.0fs)", k, length(plan50),
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
  }
  one50(get_imputed(mi, plan50, k))
})

nd <- do.call(rbind, lapply(res, `[[`, "n_dropped"))
message(sprintf(
  "country-years dropped before summation (range over imputations): since_emergence %d-%d, excl_assumed_zero %d-%d",
  min(nd[, "since_emergence"]), max(nd[, "since_emergence"]),
  min(nd[, "excl_assumed_zero"]), max(nd[, "excl_assumed_zero"])
))

# model-based: Rubin on the log-scale slope, transformed to %/yr; + percentile
# (between) on the %/yr values. within_sd_log / between_sd_log are on the
# log-slope scale.
model_all <- bind_rows(lapply(res, `[[`, "model"))
model_rows <- model_all %>%
  group_by(metric, scope) %>%
  group_modify(function(d, key) {
    r <- rubin(d$b, d$se_b^2)
    data.frame(
      m = r$m, method = "rubin",
      point = to_pct(r$point), lwr = to_pct(r$lwr), upr = to_pct(r$upr),
      between_lwr = qv(d$pct, 0.025), between_upr = qv(d$pct, 0.975),
      within_sd_log = r$within_sd, between_sd_log = r$between_sd
    )
  }) %>%
  ungroup()

# regional medians: percentile only. Not Rubin-pooled, so no log-scale SDs;
# between_sd here is the SD of the 50 per-dataset medians on the %/yr scale
# (the rubin rows carry within_sd_log / between_sd_log instead).
pool_region <- function(rows, metric_name) {
  rows %>%
    group_by(group) %>%
    summarise(
      m = sum(!is.na(med)), point = median(med, na.rm = TRUE),
      lwr = qv(med, 0.025), upr = qv(med, 0.975),
      between_sd = sd(med, na.rm = TRUE), .groups = "drop"
    ) %>%
    transmute(
      metric = metric_name, scope = group, m = m, method = "percentile",
      point = point, lwr = lwr, upr = upr, between_lwr = lwr, between_upr = upr,
      within_sd = NA_real_, between_sd = between_sd
    )
}
region_rows <- pool_region(bind_rows(lapply(res, `[[`, "region")), "region_growth_pct")
region_emerg_rows <- pool_region(
  bind_rows(lapply(res, `[[`, "region_emerg")),
  "region_growth_since_emergence_pct"
)

# fold increase per period: point from the additive annual totals written by the
# pipeline (aggregate_totals_MI.csv), interval from the per-draw ratio over all
# pooled draws (see header). Ratios are not floored/ceiled (not counts); NA if a
# start-year draw total is 0.
FOLD_PERIODS <- list(c(1990, 2024), c(2000, 2024), c(2010, 2024))
agg_annual <- read.csv("runs/mi_full/descriptive_summary/aggregate_totals_MI.csv") %>% filter(scope == "annual")
year_draw_rows <- function(y) {
  s <- mi$scaffold
  s$draw_row[s$Year == y & !is.na(s$draw_row)]
}
fold_rows <- do.call(rbind, lapply(FOLD_PERIODS, function(p) {
  s1 <- colSums(mi$draws[year_draw_rows(p[1]), , drop = FALSE])
  s2 <- colSums(mi$draws[year_draw_rows(p[2]), , drop = FALSE])
  fd <- ifelse(s1 > 0, s2 / s1, NA_real_)
  p1 <- agg_annual$dengue_total[agg_annual$Year == p[1]]
  p2 <- agg_annual$dengue_total[agg_annual$Year == p[2]]
  stopifnot(length(p1) == 1, length(p2) == 1)
  data.frame(
    metric = "fold_increase", scope = paste0(p[1], "-", p[2]),
    m = sum(is.finite(fd)), method = "percentile",
    point = if (p1 > 0) p2 / p1 else NA_real_,
    lwr = qv(fd, 0.025), upr = qv(fd, 0.975),
    between_lwr = NA_real_, between_upr = NA_real_,
    within_sd = NA_real_, between_sd = NA_real_
  )
}))

pooled <- bind_rows(model_rows, region_rows, region_emerg_rows, fold_rows)

# per-country growth rates, Rubin-pooled on the log-slope scale across the
# imputed datasets and transformed to %/yr — the country-level MI estimates the
# figure dots (fig3b / supp fig) are drawn from. within_sd_log / between_sd_log
# are on the log-slope scale.
pool_country <- function(rows, metric_name) {
  rows %>%
    group_by(adm_0_name, ISO_A0, group) %>%
    group_modify(function(d, key) rubin(d$b, d$se_b^2)) %>%
    ungroup() %>%
    transmute(
      metric = metric_name, adm_0_name, ISO_A0, od_region = group,
      m, point = to_pct(point), lwr = to_pct(lwr), upr = to_pct(upr),
      within_sd_log = within_sd, between_sd_log = between_sd
    )
}
country_pooled <- bind_rows(
  pool_country(bind_rows(lapply(res, `[[`, "country")), "country_growth_pct"),
  pool_country(bind_rows(lapply(res, `[[`, "country_emerg")), "country_growth_since_emergence_pct"),
  pool_country(bind_rows(lapply(res, `[[`, "country_emerg3")), "country_growth_since_emergence_min3_pct")
)
write.csv(country_pooled, "runs/mi_full/growth/country_growth_pooled.csv", row.names = FALSE)
message(sprintf(
  "wrote runs/mi_full/growth/country_growth_pooled.csv (%d countries x 2 metrics)",
  length(unique(country_pooled$adm_0_name))
))

# pooled fitted trend curve per region-year — the fig3a dashed line and band.
# Rubin on the per-dataset log-scale linear predictor and its SE, then
# exponentiated to counts; the band is therefore asymmetric and cannot go below
# zero.
region_pred_pooled <- bind_rows(lapply(res, `[[`, "region_pred")) %>%
  group_by(od_region, Year) %>%
  group_modify(function(d, key) {
    r <- rubin(d$eta, d$se_eta^2)
    data.frame(fit = exp(r$point), lwr = exp(r$lwr), upr = exp(r$upr))
  }) %>%
  ungroup()
write.csv(region_pred_pooled, "runs/mi_full/growth/region_trend_pred_MI.csv", row.names = FALSE)
message(sprintf(
  "wrote runs/mi_full/growth/region_trend_pred_MI.csv (%d region-years)",
  nrow(region_pred_pooled)
))
out_path <- "runs/mi_full/growth/growth_pooled.csv"
write.csv(pooled, out_path, row.names = FALSE)
message(sprintf(
  "wrote %s (%d rows). Totals are in aggregate_totals_MI.csv.",
  out_path, nrow(pooled)
))

# Sensitivity analysis sens02 (growth excluding the zero years): the
# since_emergence metrics drop every pre-emergence year, the excl_assumed_zero
# metrics drop only the years whose zero is assumed. They are computed above
# together with the main metrics; the rows are written once more here so the
# sensitivity results also live under output/tables/sensitivity/.
sens02_dir <- "output/tables/sensitivity"
dir.create(sens02_dir, recursive = TRUE, showWarnings = FALSE)
is_sens02 <- function(m) grepl("since_emergence|excl_assumed_zero", m)
write.csv(pooled %>% filter(is_sens02(metric)),
          file.path(sens02_dir, "sens02_growth_excl_zero_pooled.csv"), row.names = FALSE)
write.csv(country_pooled %>% filter(is_sens02(metric)),
          file.path(sens02_dir, "sens02_growth_excl_zero_country.csv"), row.names = FALSE)
message(sprintf("wrote %s/sens02_growth_excl_zero_{pooled,country}.csv", sens02_dir))

cat("\n=== POOLED GROWTH METRICS (%/yr) and FOLD INCREASE (x) ===\n")
cat("  growth: Rubin 95% CI [within+between]  (btwn = percentile-only); fold: 95% UI\n")
for (i in seq_len(nrow(pooled))) {
  r <- pooled[i, ]
  btw <- if (r$method == "rubin") sprintf("  (btwn %.2f, %.2f)", r$between_lwr, r$between_upr) else ""
  unit <- if (r$metric == "fold_increase") "x" else "%"
  cat(sprintf(
    "  %-18s %-32s %6.2f%s [%.2f, %.2f]%s\n",
    r$metric, r$scope, r$point, unit, r$lwr, r$upr, btw
  ))
}
