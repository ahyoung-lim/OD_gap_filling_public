# ==============================================================================
# MI WAVELET TRENDS — long-term trends in the annual and multiannual wavelet
# power of dengue incidence, in between-country synchrony, and in synchrony by
# hemisphere pair group, with multiple-imputation uncertainty propagated.
# ==============================================================================
# The wavelet analysis (functions/fn_wavelet_mi.R: eligibility, wavelet
# transform, yearly band power and its trend model, pairwise coherence / phase,
# 5-year-window synchrony and its trend model, hemisphere pair-group model) is
# run on each imputed monthly dataset — one posterior draw per MI run, m =
# number of runs — and only the fitted model coefficients and covariance
# matrices are kept per dataset. They are pooled across datasets with Rubin's
# rules on the model scale (log power slope; logit synchrony coefficients;
# logit hemisphere marginal means) and back-transformed to the reported
# quantities; see the header of fn_wavelet_mi.R for the definitions.
#
# Per-dataset results are cached to runs/mi_full/wavelet/wavelet_coef/ds_XXX.rds so an
# interrupted job resumes and pooling can be re-run without recomputation. Each
# cache file also records the country sets and the random-effect structure that
# converged (random slope, or random intercept when the random slope fails), so
# both can be checked for stability across datasets (diagnostics below), and the
# yearly power table and synchrony panel the models were fitted to.
#
# Config (env vars):
#   MI_M                cap on datasets (pilot), e.g. MI_M=4
#   MI_WAVE_CORES       parallel workers (default min(6, cores-2))
#   MI_WAVE_POOL_ONLY=1 skip computation, pool the cached ds files only
#
# Usage:
#   MI_M=4 Rscript script/04c_mi_wavelet_trends.R           # pilot
#   MI_WAVE_CORES=8 Rscript script/04c_mi_wavelet_trends.R  # full (m = n_runs)
#   MI_WAVE_POOL_ONLY=1 Rscript script/04c_mi_wavelet_trends.R
#
# Outputs (output/tables/):
#   wavelet_power_interaction_results_MI.csv
#   wavelet_sync_interaction_results_MI.csv
#   wavelet_hemi_diff_results_MI.csv
#   wavelet_hemi_full_results_MI.csv
# and (runs/mi_full/wavelet/):
#   wavelet_coef_pooled.csv       every pooled scalar with within/between SD, FMI
#   wavelet_coef_diagnostics.csv  model structure and country-set stability
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(parallel)
})
source("script/03c_mi_datasets.R")
source("functions/fn_wavelet_mi.R")

PROJ_DIR  <- getwd()
WAVE_DIR  <- "runs/mi_full/wavelet/wavelet_coef"
OUT_DIR   <- "output/tables"
POOL_ONLY <- Sys.getenv("MI_WAVE_POOL_ONLY", "0") %in% c("1", "true", "TRUE")
N_SIM     <- 5000     # draws from N(Q, T) for the non-linear quantities
dir.create(WAVE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
ds_file <- function(k) file.path(WAVE_DIR, sprintf("ds_%03d.rds", k))

# ==============================================================================
# PART 1 — compute (one cached ds file per imputed dataset)
# ==============================================================================
if (!POOL_ONLY) {
  mi <- mi_load()
  plan <- mi_sample_plan(mi, n_per_run = 1)     # one draw per run -> m = n_runs
  m_cap <- suppressWarnings(as.integer(Sys.getenv("MI_M", "")))
  if (!is.na(m_cap) && m_cap > 0 && m_cap < length(plan)) plan <- plan[seq_len(m_cap)]
  M <- length(plan)
  todo <- Filter(function(k) !file.exists(ds_file(k)), seq_len(M))
  message(sprintf("wavelet MI: m = %d datasets, %d cached, %d to compute", M, M - length(todo), length(todo)))
  items <- lapply(todo, function(k) list(k = k, run = mi$run_of_col[plan[k]], col = plan[k],
                                         d = get_imputed(mi, plan, k)))
  rm(mi)

  worker <- function(item) {
    outf <- file.path(WAVE_DIR, sprintf("ds_%03d.rds", item$k))
    if (file.exists(outf)) return(outf)
    res <- tryCatch(wavelet_dataset(item$d), error = function(e) list(error = conditionMessage(e)))
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
      clusterExport(cl, c("WAVE_DIR", "worker"), envir = environment())
      invisible(parLapply(cl, items, worker))
      stopCluster(cl)
    }
    message(sprintf("compute done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  rm(items)
}

# ==============================================================================
# PART 2 — pooling (Rubin's rules on the model scale)
# ==============================================================================
files <- list.files(WAVE_DIR, pattern = "^ds_[0-9]+[.]rds$", full.names = TRUE)
if (length(files) == 0) stop("No ds_*.rds found in ", WAVE_DIR)
per <- lapply(files, readRDS)
err <- vapply(per, function(x) !is.null(x$error), logical(1))
if (any(err)) message(sprintf("note: %d of %d datasets errored: %s", sum(err), length(per),
  paste(sprintf("ds %d (%s)", sapply(per[err], `[[`, "k"), sapply(per[err], `[[`, "error")), collapse = "; ")))
per <- per[!err]
M <- length(per)
if (M < 2) stop("fewer than 2 usable datasets; cannot pool")
message(sprintf("pooling %d datasets", M))

set.seed(123)
fits_of <- function(what, cycle) lapply(per, function(x) x[[what]][[cycle]])
pw_ann <- pool_power_cycle(fits_of("power", "Annual"), "Annual", N_SIM)
pw_mlt <- pool_power_cycle(fits_of("power", "Multiannual"), "Multiannual", N_SIM)
sy_ann <- pool_sync_cycle(fits_of("sync", "Annual"), "Annual", N_SIM)
sy_mlt <- pool_sync_cycle(fits_of("sync", "Multiannual"), "Multiannual", N_SIM)
hm_ann <- pool_hemi_cycle(fits_of("hemi", "Annual"), "Annual", N_SIM)
hm_mlt <- pool_hemi_cycle(fits_of("hemi", "Multiannual"), "Multiannual", N_SIM)

power_results <- bind_rows(pw_ann$table, pw_mlt$table)
sync_results <- bind_rows(sy_ann$table, sy_mlt$table)
hemi_results <- bind_rows(hm_ann$table, hm_mlt$table)
pooled_scalars <- bind_rows(pw_ann$scalars, pw_mlt$scalars, sy_ann$scalars, sy_mlt$scalars,
                            hm_ann$scalars, hm_mlt$scalars)

# pair-level mean synchrony averaged over datasets (distribution behind the hemisphere plot)
hemi_pairs <- bind_rows(lapply(per, function(x) bind_rows(
  x$hemi$Annual$pair_means %>% mutate(Cycle = "Annual"),
  x$hemi$Multiannual$pair_means %>% mutate(Cycle = "Multiannual")))) %>%
  group_by(Cycle, pair_id, ref_prov, other_prov, hemi_type) %>%
  summarise(mean_sync = mean(mean_sync), sd_across_datasets = sd(mean_sync), m = dplyr::n(), .groups = "drop")

# ==============================================================================
# PART 3 — diagnostics: model structure and country-set stability
# ==============================================================================
set_key <- function(v) paste(v, collapse = "|")
diag_rows <- list()
for (cyc in c("Annual", "Multiannual")) {
  for (an in c("power", "sync")) {
    re <- sapply(per, function(x) x[[an]][[cyc]]$re_structure)
    diag_rows[[length(diag_rows) + 1]] <- data.frame(
      check = "random_effect_structure", analysis = an, cycle = cyc,
      value = paste(sprintf("%s=%d", names(table(re)), as.integer(table(re))), collapse = "; "),
      datasets_with_fallback = paste(sapply(per, `[[`, "k")[re == "random_intercept"], collapse = ","))
  }
}
for (set in c("wavelet", "ann", "mlt")) {
  keys <- sapply(per, function(x) set_key(x$countries[[set]]))
  sizes <- sapply(per, function(x) length(x$countries[[set]]))
  all_c <- unique(unlist(lapply(per, function(x) x$countries[[set]])))
  unstable <- all_c[sapply(all_c, function(c) !all(sapply(per, function(x) c %in% x$countries[[set]])))]
  diag_rows[[length(diag_rows) + 1]] <- data.frame(
    check = "country_set", analysis = set, cycle = "",
    value = sprintf("%d distinct sets; size %d to %d", n_distinct(keys), min(sizes), max(sizes)),
    datasets_with_fallback = if (length(unstable) > 0) paste("not in all datasets:", paste(unstable, collapse = ",")) else "")
}
diagnostics <- bind_rows(diag_rows) %>% rename(detail = datasets_with_fallback)

# ==============================================================================
# PART 4 — write and print
# ==============================================================================
write.csv(power_results, file.path(OUT_DIR, "wavelet_power_interaction_results_MI.csv"), row.names = FALSE)
write.csv(sync_results, file.path(OUT_DIR, "wavelet_sync_interaction_results_MI.csv"), row.names = FALSE)
write.csv(hemi_results, file.path(OUT_DIR, "wavelet_hemi_diff_results_MI.csv"), row.names = FALSE)
write.csv(hemi_pairs, file.path(OUT_DIR, "wavelet_hemi_full_results_MI.csv"), row.names = FALSE)
write.csv(pooled_scalars, "runs/mi_full/wavelet/wavelet_coef_pooled.csv", row.names = FALSE)
write.csv(diagnostics, "runs/mi_full/wavelet/wavelet_coef_diagnostics.csv", row.names = FALSE)

fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))

cat(sprintf("\n=== POWER TRENDS (MI pooled, m = %d; %%/yr on power scale, Rubin 95%% CI) ===\n", M))
power_results %>%
  transmute(Cycle, Region, N,
            Pct_Change_yr = sprintf("%6.2f (%6.2f, %6.2f)", Pct_Change, CI_Lower, CI_Upper),
            p = fmt_p(P_value), Interaction_p = fmt_p(Interaction_P),
            Cum_1990_2024 = sprintf("%7.1f (%7.1f, %7.1f)", Pct_Change_Cum_1990_2024, Cum_CI_Lower_1990_2024, Cum_CI_Upper_1990_2024),
            Increasing = sprintf("%d/%d (%.0f%%)", N_Inc, N_Tot, Pct_Inc), FMI = round(FMI, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat(sprintf("\n=== SYNCHRONY TRENDS (MI pooled, m = %d; %%/yr on synchrony scale, 95%% CI from N(Q,T)) ===\n", M))
sync_results %>%
  transmute(Cycle, Pair_Type, N_pairs,
            Sync = sprintf("%.2f -> %.2f", Sync_Start, Sync_End),
            Pct_Change_yr = sprintf("%6.2f (%6.2f, %6.2f)", Pct_Change_Per_Year, CI_Lower, CI_Upper),
            p = fmt_p(Trend_P), Interaction_p = fmt_p(Interaction_P),
            Increasing = sprintf("%d/%d (%.0f%%)", N_Inc, N_Tot, Pct_Inc), FMI = round(FMI, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat(sprintf("\n=== HEMISPHERE (MI pooled, m = %d) ===\n", M))
hemi_results %>%
  transmute(cycle, group, n_pairs,
            mean = ifelse(is.na(emmean), "", sprintf("%.3f (%.3f, %.3f)", emmean, lo, hi)),
            diff = ifelse(is.na(diff_raw), "", sprintf("%.3f (%.3f, %.3f)", diff_raw, diff_lo, diff_hi)),
            OR = ifelse(is.na(OR), "", sprintf("%.2f (%.2f, %.2f)", OR, OR_lo, OR_hi)),
            p = fmt_p(p_value)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== DIAGNOSTICS ===\n")
print(as.data.frame(diagnostics), row.names = FALSE, right = FALSE)
cat("\nwrote:", file.path(OUT_DIR, "wavelet_{power_interaction,sync_interaction,hemi_diff,hemi_full}_results_MI.csv"),
    "\n       runs/mi_full/wavelet/wavelet_coef_pooled.csv, runs/mi_full/wavelet/wavelet_coef_diagnostics.csv\n")
