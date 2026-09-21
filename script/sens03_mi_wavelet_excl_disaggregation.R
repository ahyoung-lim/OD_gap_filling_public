# ==============================================================================
# SENSITIVITY — wavelet power and synchrony trends when wavelet coefficients
# dominated by disaggregated months are excluded.
# ==============================================================================
# Question: are the MI-pooled power and synchrony trends (script 04c) driven by
# the monthly values that the gap-filling model distributed from annual totals?
#
# Disaggregated month = a month with no reported sub-annual value at the
# downscaling stage (dengue_total is NA in runs/mi_full/mi50/disaggregation/run_01_downscale.csv);
# its monthly value comes from the annual total. The positions are fixed by the
# reporting pattern, so they are identical in every imputed dataset. Weekly /
# monthly gap-filled months keep a reported sub-annual value and are NOT
# excluded.
#
# Exclusion rule (functions/fn_wavelet_mi.R): for every wavelet coefficient
# (scale x time) the share of disaggregated months inside its Morlet time
# envelope is computed; a coefficient is dropped when that share exceeds a
# threshold theta (20, 30, 50 %); a band-year power mean is kept only if at
# least half of its cells survive the cone of influence and the exclusion. For
# pairwise coherence and phase the share is the larger of the two countries'.
# The transform and coherence are computed once per dataset; band extraction
# and the trend models are repeated per threshold, plus an unmasked baseline
# from the same computation (it reproduces script 04c's results).
#
# Two country-set variants are reported:
#   per_threshold  countries / pairs and their years drop out as the exclusion
#                  removes their band-years (N shown per threshold);
#   fixed_support  every threshold, and the baseline, refitted on exactly the
#                  country-years and pair-windows that survive the strictest
#                  threshold, so only the values differ between thresholds.
#
# Pooling across the m imputed datasets (one draw per run) is the same Rubin's
# rules on the model scale as script 04c. In addition, the paired difference
# masked minus unmasked of the per-year slope (log power; logit synchrony) is
# formed within each dataset and pooled; its within-dataset variance is taken
# as the sum of the two fits' variances (conservative, the fits are positively
# correlated). Trend tests only (power and synchrony); the hemisphere analysis
# is not repeated.
#
# Per-dataset results are cached to runs/sensitivity/sens03_wavelet_excl_disaggregation/wavelet_coef_excl<thresholds>/
# ds_XXX.rds (threshold set in the folder name; caches with different sets are
# never mixed).
#
# Config (env vars):
#   MI_WAVE_EXCL        thresholds in %, default "20,30,50"
#   MI_M                cap on datasets (pilot)
#   MI_WAVE_CORES       parallel workers (default min(6, cores-2))
#   MI_WAVE_POOL_ONLY=1 skip computation, pool the cached ds files only
#
# Usage:
#   MI_M=2 Rscript script/sens03_mi_wavelet_excl_disaggregation.R
#   MI_WAVE_CORES=8 Rscript script/sens03_mi_wavelet_excl_disaggregation.R
#
# Outputs (output/tables/):
#   sens03_wavelet_excl_disaggregation_power_MI.csv     04c'spower table per threshold x variant
#   sens03_wavelet_excl_disaggregation_sync_MI.csv      04c'ssynchrony table per threshold x variant
#   sens03_wavelet_excl_disaggregation_paired_MI.csv    pooled masked - unmasked slope differences
#   sens03_wavelet_excl_disaggregation_coverage_MI.csv  band-years kept per country x threshold
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(parallel)
})
source("script/03c_mi_datasets.R")
source("functions/fn_wavelet_mi.R")

PROJ_DIR  <- getwd()
OUT_DIR   <- "output/tables/sensitivity"
POOL_ONLY <- Sys.getenv("MI_WAVE_POOL_ONLY", "0") %in% c("1", "true", "TRUE")
N_SIM     <- 5000
EXCL_PCT  <- suppressWarnings(as.numeric(strsplit(Sys.getenv("MI_WAVE_EXCL", "20,30,50"), ",")[[1]]))
EXCL_PCT  <- sort(unique(EXCL_PCT[is.finite(EXCL_PCT) & EXCL_PCT > 0 & EXCL_PCT < 100]))
if (length(EXCL_PCT) == 0) stop("MI_WAVE_EXCL must give at least one threshold in (0, 100)")
EXCL_FRAC <- EXCL_PCT / 100
EXCL_KEYS <- as.character(EXCL_FRAC)
excl_tag  <- paste(EXCL_PCT, collapse = "-")
WAVE_DIR  <- sprintf("runs/sensitivity/sens03_wavelet_excl_disaggregation/wavelet_coef_excl%s", excl_tag)
dir.create(WAVE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
ds_file <- function(k) file.path(WAVE_DIR, sprintf("ds_%03d.rds", k))
out_file <- function(what) file.path(OUT_DIR, sprintf("sens03_wavelet_excl_disaggregation_%s_MI.csv", what))
message(sprintf("exclusion thresholds: %s %% disaggregated; cache %s", paste(EXCL_PCT, collapse = "/"), WAVE_DIR))

# ==============================================================================
# PART 1 — compute (one cached ds file per imputed dataset)
# ==============================================================================
if (!POOL_ONLY) {
  # disaggregated-month flags, identical across runs (run 1's stage table)
  mask_tab <- read.csv("runs/mi_full/mi50/disaggregation/run_01_downscale.csv") %>%
    dplyr::transmute(adm_0_name, Year, month, mask_imp = as.integer(is.na(dengue_total)))
  stopifnot(!anyDuplicated(mask_tab[, c("adm_0_name", "Year", "month")]))
  message(sprintf("disaggregated months: %d of %d cells in the stage table", sum(mask_tab$mask_imp), nrow(mask_tab)))

  mi <- mi_load()
  plan <- mi_sample_plan(mi, n_per_run = 1)
  m_cap <- suppressWarnings(as.integer(Sys.getenv("MI_M", "")))
  if (!is.na(m_cap) && m_cap > 0 && m_cap < length(plan)) plan <- plan[seq_len(m_cap)]
  M <- length(plan)
  todo <- Filter(function(k) !file.exists(ds_file(k)), seq_len(M))
  message(sprintf("exclusion MI: m = %d datasets, %d cached, %d to compute", M, M - length(todo), length(todo)))
  items <- lapply(todo, function(k) {
    d <- get_imputed(mi, plan, k) %>%
      dplyr::left_join(mask_tab, by = c("adm_0_name", "Year", "month"))
    # cells absent from the stage table are the deterministic assumed-zero cells:
    # not disaggregated. Any other join failure would be a grid mismatch.
    stopifnot(all(d$dengue_total_scaled[is.na(d$mask_imp)] == 0))
    d$mask_imp[is.na(d$mask_imp)] <- 0L
    list(k = k, run = mi$run_of_col[plan[k]], col = plan[k], d = d)
  })
  rm(mi)

  worker <- function(item) {
    outf <- file.path(WAVE_DIR, sprintf("ds_%03d.rds", item$k))
    if (file.exists(outf)) return(outf)
    res <- tryCatch(wavelet_dataset(item$d, mask_max_imp = EXCL_FRAC),
                    error = function(e) list(error = conditionMessage(e)))
    saveRDS(c(list(k = item$k, run = item$run, col = item$col), res), outf)
    outf
  }

  if (length(items) > 0) {
    n_cores <- suppressWarnings(as.integer(Sys.getenv("MI_WAVE_CORES", "")))
    if (is.na(n_cores) || n_cores < 1) n_cores <- max(1L, min(6L, detectCores() - 2L))
    n_cores <- min(n_cores, length(items))
    t0 <- Sys.time()
    if (n_cores == 1L) {
      for (it in items) {
        worker(it)
        message(sprintf("  ds %d done (%.1f min elapsed)", it$k,
                        as.numeric(difftime(Sys.time(), t0, units = "mins"))))
      }
    } else {
      message(sprintf("launching %d parallel workers", n_cores))
      cl <- makePSOCKcluster(n_cores)
      clusterExport(cl, "PROJ_DIR", envir = environment())
      clusterEvalQ(cl, { setwd(PROJ_DIR); source("functions/fn_wavelet_mi.R") })
      clusterExport(cl, c("WAVE_DIR", "worker", "EXCL_FRAC"), envir = environment())
      invisible(parLapply(cl, items, worker))
      stopCluster(cl)
    }
    message(sprintf("compute done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  rm(items)
}

# ==============================================================================
# PART 2 — pooling per threshold and variant
# ==============================================================================
files <- list.files(WAVE_DIR, pattern = "^ds_[0-9]+[.]rds$", full.names = TRUE)
if (length(files) == 0) stop("No ds_*.rds found in ", WAVE_DIR)
per <- lapply(files, readRDS)
err <- vapply(per, function(x) !is.null(x$error), logical(1))
if (any(err)) message(sprintf("note: %d of %d datasets errored: %s", sum(err), length(per),
  paste(sprintf("ds %d (%s)", sapply(per[err], `[[`, "k"), sapply(per[err], `[[`, "error")), collapse = "; ")))
per <- per[!err]
if (length(per) < 2) stop("fewer than 2 usable datasets; cannot pool")
# every cached dataset must carry the requested threshold set
sets <- vapply(per, function(x) paste(names(x$excluded), collapse = ","), character(1))
if (length(unique(sets)) != 1 || !identical(sort(names(per[[1]]$excluded)), sort(EXCL_KEYS)))
  stop("cached threshold sets (", paste(unique(sets), collapse = " | "),
       ") differ from the requested set (", paste(EXCL_KEYS, collapse = ","), ")")
message(sprintf("pooling %d datasets", length(per)))

TH_LEVELS <- c("none", EXCL_KEYS)
th_label <- function(key) if (key == "none") "none" else sprintf("%g%%", 100 * as.numeric(key))

# per-dataset fit for (analysis, cycle, threshold, variant); NULL when that
# dataset's models failed at the threshold
get_fit <- function(x, analysis, cycle, key, variant) {
  node <- if (key == "none") x else x$excluded[[key]]
  if (is.null(node)) return(NULL)
  src <- if (variant == "per_threshold") node[[analysis]] else node$fixed[[analysis]]
  if (is.null(src)) NULL else src[[cycle]]
}
fits_for <- function(analysis, cycle, key, variant) {
  f <- lapply(per, get_fit, analysis = analysis, cycle = cycle, key = key, variant = variant)
  f[!vapply(f, is.null, logical(1))]
}

set.seed(123)
power_tabs <- list(); sync_tabs <- list(); paired <- list()
for (variant in c("per_threshold", "fixed_support")) {
  for (key in TH_LEVELS) {
    for (cyc in c("Annual", "Multiannual")) {
      fp <- fits_for("power", cyc, key, variant)
      if (length(fp) >= 2) {
        power_tabs[[length(power_tabs) + 1]] <- pool_power_cycle(fp, cyc, N_SIM)$table %>%
          mutate(threshold = th_label(key), variant = variant, .before = 1)
      } else message(sprintf("power %s %s %s: only %d datasets fitted, skipped", variant, th_label(key), cyc, length(fp)))
      fs <- fits_for("sync", cyc, key, variant)
      if (length(fs) >= 2) {
        sync_tabs[[length(sync_tabs) + 1]] <- pool_sync_cycle(fs, cyc, N_SIM)$table %>%
          mutate(threshold = th_label(key), variant = variant, .before = 1)
      } else message(sprintf("sync %s %s %s: only %d datasets fitted, skipped", variant, th_label(key), cyc, length(fs)))
    }
  }
}
power_all <- bind_rows(power_tabs)
sync_all <- bind_rows(sync_tabs)

# --- paired masked - unmasked slope differences, pooled ---------------------------
# per-year slope contrasts on the model scale (same as pool_power_cycle / pool_sync_cycle)
power_cvec <- list(
  Global = function(f) { w <- f$consts$nA / (f$consts$nA + f$consts$nAs)
    c(year_centered = 1 / f$consts$sd_yr, `year_centered:regionAsia` = (1 - w) / f$consts$sd_yr) },
  Americas = function(f) c(year_centered = 1 / f$consts$sd_yr),
  Asia = function(f) c(year_centered = 1 / f$consts$sd_yr, `year_centered:regionAsia` = 1 / f$consts$sd_yr))
yr <- "year_centered"; iAs <- "year_centered:pair_typeWithin Asia"; iC <- "year_centered:pair_typeAmericas-Asia"
sync_cvec <- list(
  Global = function(f) { w <- c(f$consts$n_pairs_A, f$consts$n_pairs_As, f$consts$n_pairs_C) / f$consts$n_pairs_tot
    setNames(c(1, w[2], w[3]) / f$consts$sd_year, c(yr, iAs, iC)) },
  `Within Americas` = function(f) setNames(1 / f$consts$sd_year, yr),
  `Within Asia` = function(f) setNames(c(1, 1) / f$consts$sd_year, c(yr, iAs)),
  `Americas-Asia` = function(f) setNames(c(1, 1) / f$consts$sd_year, c(yr, iC)))

pool_paired <- function(analysis, cycle, key, variant, cvecs) {
  base <- lapply(per, get_fit, analysis = analysis, cycle = cycle, key = "none", variant = variant)
  mask <- lapply(per, get_fit, analysis = analysis, cycle = cycle, key = key, variant = variant)
  ok <- !vapply(base, is.null, logical(1)) & !vapply(mask, is.null, logical(1))
  if (sum(ok) < 2) return(NULL)
  bind_rows(lapply(names(cvecs), function(rg) {
    qu <- t(sapply(which(ok), function(i) {
      b <- lincomb(base[[i]], cvecs[[rg]](base[[i]])); m <- lincomb(mask[[i]], cvecs[[rg]](mask[[i]]))
      c(Q = m[["Q"]] - b[["Q"]], U = m[["U"]] + b[["U"]], Qb = b[["Q"]], Qm = m[["Q"]])
    }))
    r <- rubin_scalar(qu[, "Q"], qu[, "U"])
    data.frame(analysis = analysis, cycle = cycle, region = rg, threshold = th_label(key), variant = variant,
               scale = if (analysis == "power") "log slope per year" else "logit slope per year",
               slope_unmasked = mean(qu[, "Qb"]), slope_masked = mean(qu[, "Qm"]),
               diff_point = r$point, diff_lwr = r$lwr, diff_upr = r$upr, diff_p = r$p, m = r$m,
               diff_pct_per_year = if (analysis == "power") (exp(r$point) - 1) * 100 else NA_real_)
  }))
}
for (variant in c("per_threshold", "fixed_support"))
  for (key in EXCL_KEYS)
    for (cyc in c("Annual", "Multiannual")) {
      paired[[length(paired) + 1]] <- pool_paired("power", cyc, key, variant, power_cvec)
      paired[[length(paired) + 1]] <- pool_paired("sync", cyc, key, variant, sync_cvec)
    }
paired_all <- bind_rows(paired)

# --- coverage: band-years kept per country x threshold, averaged over datasets ------
cov_rows <- list()
for (key in TH_LEVELS) {
  yt <- bind_rows(lapply(per, function(x) {
    y <- if (key == "none") x$yearly else x$excluded[[key]]$yearly
    if (is.null(y)) return(NULL)
    y %>% filter(country %in% x$countries$ann) %>% mutate(k = x$k)
  }))
  if (nrow(yt) == 0) next
  cov_rows[[length(cov_rows) + 1]] <- yt %>%
    group_by(country, k) %>%
    summarise(ann_years_kept = sum(!is.na(ann_power_yearly)), mlt_years_kept = sum(!is.na(mlt_power_yearly)),
              ann_mean_frac_valid = mean(ann_frac_valid, na.rm = TRUE),
              mlt_mean_frac_valid = mean(mlt_frac_valid, na.rm = TRUE), n_years = dplyr::n(), .groups = "drop") %>%
    group_by(country) %>%
    summarise(across(c(ann_years_kept, mlt_years_kept, ann_mean_frac_valid, mlt_mean_frac_valid, n_years), mean),
              .groups = "drop") %>%
    mutate(threshold = th_label(key), .before = 1)
}
coverage <- bind_rows(cov_rows)

# ==============================================================================
# PART 3 — write and print
# ==============================================================================
write.csv(power_all, out_file("power"), row.names = FALSE)
write.csv(sync_all, out_file("sync"), row.names = FALSE)
write.csv(paired_all, out_file("paired"), row.names = FALSE)
write.csv(coverage, out_file("coverage"), row.names = FALSE)

fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
th_order <- sapply(TH_LEVELS, th_label)

cat("\n=== POWER: %/yr (95% CI), p, N countries — by exclusion threshold ===\n")
power_all %>%
  mutate(threshold = factor(threshold, levels = th_order)) %>%
  arrange(variant, Cycle, Region, threshold) %>%
  transmute(variant, Cycle, Region, threshold, N,
            pct = sprintf("%6.2f (%6.2f, %6.2f)", Pct_Change, CI_Lower, CI_Upper), p = fmt_p(P_value),
            int_p = fmt_p(Interaction_P), FMI = round(FMI, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== SYNCHRONY: %/yr (95% CI), p, N pairs — by exclusion threshold ===\n")
sync_all %>%
  mutate(threshold = factor(threshold, levels = th_order)) %>%
  arrange(variant, Cycle, Pair_Type, threshold) %>%
  transmute(variant, Cycle, Pair_Type, threshold, N_pairs,
            pct = sprintf("%6.2f (%6.2f, %6.2f)", Pct_Change_Per_Year, CI_Lower, CI_Upper), p = fmt_p(Trend_P),
            int_p = fmt_p(Interaction_P), FMI = round(FMI, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== PAIRED masked - unmasked per-year slope (model scale), pooled ===\n")
paired_all %>%
  mutate(threshold = factor(threshold, levels = th_order)) %>%
  arrange(variant, analysis, cycle, region, threshold) %>%
  transmute(variant, analysis, cycle, region, threshold,
            unmasked = sprintf("%.4f", slope_unmasked), masked = sprintf("%.4f", slope_masked),
            diff = sprintf("%+.4f (%+.4f, %+.4f)", diff_point, diff_lwr, diff_upr), p = fmt_p(diff_p)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== COVERAGE: median band-years kept per country, by threshold ===\n")
coverage %>%
  mutate(threshold = factor(threshold, levels = th_order)) %>%
  group_by(threshold) %>%
  summarise(countries = dplyr::n(), ann_years_kept_med = median(ann_years_kept),
            mlt_years_kept_med = median(mlt_years_kept),
            ann_frac_valid_med = median(ann_mean_frac_valid, na.rm = TRUE),
            mlt_frac_valid_med = median(mlt_mean_frac_valid, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE, digits = 3)

cat("\nwrote:", out_file("{power,sync,paired,coverage}"), "\n")
