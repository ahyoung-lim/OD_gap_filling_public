# ==============================================================================
# SUB-ANNUAL SCALING SENSITIVITY — every figure in the summary, from one script
# ==============================================================================
# 283 country-years have both a complete sub-annual series and an annual total
# from a different source, the sub-annual sum falling short; the pipeline scales
# the sub-annual series up to match. A completeness cutoff T would scale only the
# series reaching T% of the annual total and discard the rest.
#   cutoff 0   = scale all 283  = current pipeline
#   cutoff 100 = scale none     = model predictions only
#
# METRIC. Total variation distance between two monthly profiles: normalise each
# country-year's twelve monthly estimates to sum to one under each setting and
# take half the sum of absolute differences. Reads as "X% of that country-year's
# cases would have to shift into a different month". Case-weighted across
# country-years throughout, except the peak-month and amplitude counts in §2,
# which are proportions OF COUNTRY-YEARS and are stated as such.
#
# Run k shares its seed across settings, so "cutoff A run k vs cutoff B run k" is
# one independent measurement; figures are median [min-max] over N_RUNS such
# pairs, which puts the Monte-Carlo variation inside the reported number.
# Distances are NOT additive — never obtain a comparison by subtracting two
# others; every figure below is measured directly.
#
# Requires, for each cutoff, N_RUNS completed MI runs (run_01.rds ...):
#   runs/mi_full                                   cutoff 0, the production run
#   runs/sensitivity/sens01_scaling_sensitivity/{thr25,thr50,thr75,thr90}     the intermediate cutoffs
#   runs/sensitivity/sens01_scaling_sensitivity/noscale                       cutoff 100
# Produced by script/sens01a_scaling_sensitivity_prep.R (model inputs, local) then
# 03b_run_pipeline_MI_full.R per cutoff (on WSL).
#
# Usage:  Rscript script/sens01b_scaling_sensitivity.R
#         N_RUNS=5 Rscript script/sens01b_scaling_sensitivity.R
# ==============================================================================

suppressMessages({library(dplyr); library(tidyr)})

N_RUNS <- suppressWarnings(as.numeric(Sys.getenv("N_RUNS", "2")))
if (is.na(N_RUNS)) N_RUNS <- 2

DIR <- c(`0` = "runs/mi_full",             `25` = "runs/sensitivity/sens01_scaling_sensitivity/thr25",
         `50` = "runs/sensitivity/sens01_scaling_sensitivity/thr50",  `75` = "runs/sensitivity/sens01_scaling_sensitivity/thr75",
         `90` = "runs/sensitivity/sens01_scaling_sensitivity/thr90",  `100` = "runs/sensitivity/sens01_scaling_sensitivity/noscale")
missing <- names(DIR)[!vapply(DIR, function(d)
  all(file.exists(sprintf("%s/mi50/disaggregation/run_%02d.rds", d, seq_len(N_RUNS)))), logical(1))]
if (length(missing)) stop("cutoff(s) without ", N_RUNS, " runs: ",
                          paste(missing, collapse = ", "), call. = FALSE)

# the 283 scaling country-years and their completeness
cy283 <- read.csv("data/processed_data/selection_outcome_V1_3.csv",
                  stringsAsFactors = FALSE) %>%
  filter(relationship_category == "Annual > sub-annual",
         subannual_is_complete == TRUE, T_res == "Year") %>%
  transmute(country_year, pct = pct_of_annual)

cells <- function(cut, k, tag) {
  r <- readRDS(sprintf("%s/mi50/disaggregation/run_%02d.rds", DIR[[as.character(cut)]], k))
  setNames(data.frame(key = r$keys, v = rowMeans(r$draws), stringsAsFactors = FALSE),
           c("key", tag))
}
split_key <- function(df)
  df %>% separate(key, into = c("adm_0_name", "Year", "month"), sep = "\\|",
                  convert = TRUE, extra = "drop") %>%
  mutate(country_year = paste0(adm_0_name, "_", Year))

d_half <- function(a, b) sum(abs(a / sum(a) - b / sum(b))) / 2
circ   <- function(x) pmin(abs(x) %% 12, 12 - (abs(x) %% 12))
top3   <- function(x) sum(sort(x / sum(x), decreasing = TRUE)[1:3])
fmt    <- function(x, dp = 2) sprintf(paste0("%.", dp, "f [%.", dp, "f-%.", dp, "f]"),
                                      median(x), min(x), max(x))

# per country-year comparison of two cutoffs, one matched run pair
pair <- function(cut_a, cut_b, k, keep = NULL) {
  d <- cells(cut_a, k, "a") %>%
    inner_join(cells(cut_b, k, "b"), by = "key") %>%
    split_key() %>%
    inner_join(cy283, by = "country_year")
  if (!is.null(keep)) d <- d %>% filter(country_year %in% keep)
  d %>% group_by(country_year) %>%
    filter(sum(a) > 0, sum(b) > 0) %>%
    summarise(tot = sum(a), dd = d_half(b, a),
              peak_same = as.integer(circ(month[which.max(b)] - month[which.max(a)]) == 0),
              flatter   = as.integer(top3(b) < top3(a)),
              .groups = "drop")
}
over_runs <- function(f) lapply(seq_len(N_RUNS), f) %>% bind_rows()

# ---- 0. what the 283 country-years are ---------------------------------------
rel <- over_runs(function(k) {
  r <- readRDS(sprintf("runs/mi_full/mi50/disaggregation/run_%02d.rds", k))
  data.frame(total = sum(rowMeans(r$draws)))
})
base <- over_runs(function(k) {
  x <- pair(0, 0, k) ; data.frame(cases = sum(x$tot))   # cutoff 0 values, the 283
})
n90 <- sum(cy283$pct >= 90)

cat("=================== SUB-ANNUAL SCALING SENSITIVITY ===================\n")
cat(sprintf("%d matched run pairs per cutoff\n\n", N_RUNS))
cat(sprintf("country-years affected by sub-annual scaling : %d\n", nrow(cy283)))
cat(sprintf("  their cases                                : %s\n", fmt(base$cases, 0)))
cat(sprintf("  share of all estimates 1990-2024           : %.1f%% of %s\n",
            100 * median(base$cases) / median(rel$total), fmt(rel$total, 0)))
cat(sprintf("  completeness >= 90%%                        : %d country-years\n", n90))
cat(sprintf("  completeness  < 90%%                        : %d country-years\n",
            nrow(cy283) - n90))

# ---- 1. cutoff 100 vs cutoff 0 -----------------------------------------------
s1 <- over_runs(function(k) {
  x <- pair(0, 100, k)
  data.frame(tvd = 100 * sum(x$dd * x$tot) / sum(x$tot))
})
cat("\n--- 1. cutoff 100 (model only) vs cutoff 0 (current) ---\n")
cat(sprintf("  %s%% of these country-years' cases shifted to a different month\n",
            fmt(s1$tvd)))

# ---- 2. the >=90% country-years, model against the observed benchmark --------
keep90 <- cy283$country_year[cy283$pct >= 90]
s2 <- over_runs(function(k) {
  x <- pair(0, 100, k, keep = keep90)          # a = observed (cutoff 0), b = model
  data.frame(tvd  = 100 * sum(x$dd * x$tot) / sum(x$tot),
             peak = 100 * mean(x$peak_same),
             flat = 100 * mean(x$flatter))
})
cat(sprintf("\n--- 2. the %d country-years at >=90%% completeness: model vs observed ---\n", n90))
cat("  (observed series = benchmark; excluded from the cutoff-100 fit, so held out)\n")
cat(sprintf("  model placed %s%% of their cases in a different month\n", fmt(s2$tvd)))
cat(sprintf("  correct peak month in %s%% of country-years\n", fmt(s2$peak, 0)))
cat(sprintf("  flatter curve than observed in %s%% of country-years\n", fmt(s2$flat, 0)))

# ---- 3. excluding the less complete series -----------------------------------
excl <- lapply(c(90, 75, 50, 25), function(T) {
  s <- over_runs(function(k) {
    x <- pair(0, T, k)
    data.frame(shifted = sum(x$dd * x$tot),
               tvd = 100 * sum(x$dd * x$tot) / sum(x$tot))
  })
  data.frame(cutoff = sprintf("<%g%%", T),
             `# cys excluded`  = sum(cy283$pct < T),
             `# cases shifted` = fmt(s$shifted, 0),
             `TVD weighted`    = paste0(fmt(s$tvd), "%"),
             check.names = FALSE)
}) %>% bind_rows()

cat("\n--- 3. excluding the country-years below each completeness cutoff ---\n")
cat("  (compared with the current pipeline, which uses all 283)\n\n")
print(excl, row.names = FALSE)

dir.create("output/tables/sensitivity", showWarnings = FALSE, recursive = TRUE)
write.csv(excl, "output/tables/sensitivity/sens01_scaling_sensitivity_exclusion.csv", row.names = FALSE)
cat("\nwrote output/tables/sensitivity/sens01_scaling_sensitivity_exclusion.csv\n")

# ---- summary table of the figures printed above (sections 0-2) ---------------
# One row per quantity: median [min-max] over the N_RUNS matched run pairs, as
# in the console output. Section 3 is the exclusion table written just above.
row1 <- function(section, quantity, x) data.frame(
  section = section, quantity = quantity,
  median = median(x), min = min(x), max = max(x), n_runs = N_RUNS)
summ <- bind_rows(
  row1(0, "country_years_affected_by_scaling", nrow(cy283)),
  row1(0, "cases_in_affected_country_years", base$cases),
  row1(0, "share_of_all_estimates_1990_2024_pct", 100 * median(base$cases) / median(rel$total)),
  row1(0, "all_estimates_1990_2024_cases", rel$total),
  row1(0, "country_years_completeness_ge90", n90),
  row1(0, "country_years_completeness_lt90", nrow(cy283) - n90),
  row1(1, "tvd_pct_cutoff100_vs_cutoff0", s1$tvd),
  row1(2, "tvd_pct_model_vs_observed_ge90", s2$tvd),
  row1(2, "correct_peak_month_pct_ge90", s2$peak),
  row1(2, "flatter_than_observed_pct_ge90", s2$flat)
)
write.csv(summ, "output/tables/sensitivity/sens01_scaling_sensitivity_summary.csv", row.names = FALSE)
cat("wrote output/tables/sensitivity/sens01_scaling_sensitivity_summary.csv\n")

# ==============================================================================
# OPTIONAL — not part of the reported summary
# ==============================================================================
# Two additional views of the same comparison. Uncomment either block to
# regenerate them.
#
# (a) TRANCHE TABLE — what each completeness band contributes on its own.
#     Adjacent cutoffs are compared directly, which swaps exactly one band
#     between observed and modelled and so isolates that band's contribution.
#     Reading it: the >=90% band supplies most of the seasonal signal and each
#     band below adds steadily less. Note the values do NOT sum to the endpoint
#     comparison in §1 — distances are not additive.
#
# RUNGS <- c(100, 90, 75, 50, 25, 0)
# tranche <- lapply(seq_len(length(RUNGS) - 1), function(i) {
#   hi <- RUNGS[i]; lo <- RUNGS[i + 1]
#   s <- over_runs(function(k) {
#     x <- pair(hi, lo, k)
#     data.frame(shifted = sum(x$dd * x$tot),
#                tvd = 100 * sum(x$dd * x$tot) / sum(x$tot))
#   })
#   data.frame(band = sprintf("%g-%g%%", lo, min(hi, 100)),
#              comparison = sprintf("cutoff %g -> %g", hi, lo),
#              n_cy = sum(cy283$pct >= lo & cy283$pct < hi | hi == 100 & cy283$pct >= lo),
#              `cases shifted` = fmt(s$shifted, 0),
#              `TVD weighted` = paste0(fmt(s$tvd), "%"), check.names = FALSE)
# }) %>% bind_rows()
# print(tranche, row.names = FALSE)
#
# (b) LADDER — distance from one reference cutoff to every other. Set REF to 0
#     for "how far would this cutoff move the current release" (sensitivity), or
#     to 100 for "how much does adding data move us from a model-only
#     reconstruction" (the framing used in the reviewer response).
#
# REF <- 100
# ladder <- lapply(setdiff(c(0, 25, 50, 75, 90, 100), REF), function(T) {
#   s <- over_runs(function(k) {
#     x <- pair(REF, T, k)
#     data.frame(tvd = 100 * sum(x$dd * x$tot) / sum(x$tot),
#                peak = 100 * mean(x$peak_same))
#   })
#   data.frame(cutoff = T, `TVD weighted` = paste0(fmt(s$tvd), "%"),
#              `peak month same %` = fmt(s$peak, 0), check.names = FALSE)
# }) %>% bind_rows() %>% arrange(abs(cutoff - REF))
# print(ladder, row.names = FALSE)
