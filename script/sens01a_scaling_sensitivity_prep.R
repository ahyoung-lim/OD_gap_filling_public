# ==============================================================================
# SUB-ANNUAL SCALING SENSITIVITY — PREP for all variant settings
# ==============================================================================
# Runs 01b -> 01c -> 01d for each sub-annual scaling VARIANT and writes
# each to its own folder (never touching the 100% production files):
#   - noscale (0%)      EXCLUDE_SUBANNUAL_SCALING=true   -> data/sensitivity/sens01_scaling_sensitivity/noscale/
#   - cutoff 25%        SCALE_MIN_COMPLETENESS=25        -> data/sensitivity/sens01_scaling_sensitivity/thr25/
#   - cutoff 50%        SCALE_MIN_COMPLETENESS=50        -> data/sensitivity/sens01_scaling_sensitivity/thr50/
#   - cutoff 75%        SCALE_MIN_COMPLETENESS=75        -> data/sensitivity/sens01_scaling_sensitivity/thr75/
#   - cutoff 90%        SCALE_MIN_COMPLETENESS=90        -> data/sensitivity/sens01_scaling_sensitivity/thr90/
# (100% = the default production pipeline; use 03a_run_prep_production.R for that.)
#
# Needs the OpenDengue release files (OD_RELEASE_DIR, see 00_setup.R) and the
# EpiWeek package (v1.1), which 01d uses to build the weekly master calendar
# (see the header of 03a_run_prep_production.R).
#
# Gap filling per variant is a separate step, with the same flag as the prep:
#   SCALE_MIN_COMPLETENESS=50 MI_N_RUNS=1 Rscript script/03b_run_pipeline_MI_full.R   (etc.)
#
# Usage:   Rscript script/sens01a_scaling_sensitivity_prep.R                # all variants
#          SENS_SETTINGS=noscale,50 Rscript script/sens01a_scaling_sensitivity_prep.R   # a subset
# ==============================================================================

stopifnot("run from repo root" = file.exists("script/03b_run_pipeline_MI_full.R"))
check <- function(ok, msg) if (!isTRUE(ok)) stop("[sens] SAFEGUARD FAIL: ", msg, call. = FALSE) else
  message(">>> [sens] CHECK OK: ", msg)

# each variant: label, the env it sets, its output folder, and the expected scaled-record rule
VARIANTS <- list(
  noscale = list(env = c(EXCLUDE_SUBANNUAL_SCALING = "true",  SCALE_MIN_COMPLETENESS = "0"),
                 folder = "data/sensitivity/sens01_scaling_sensitivity/noscale", expect = "zero"),
  `25`    = list(env = c(EXCLUDE_SUBANNUAL_SCALING = "false", SCALE_MIN_COMPLETENESS = "25"),
                 folder = "data/sensitivity/sens01_scaling_sensitivity/thr25",   expect = "some"),
  `50`    = list(env = c(EXCLUDE_SUBANNUAL_SCALING = "false", SCALE_MIN_COMPLETENESS = "50"),
                 folder = "data/sensitivity/sens01_scaling_sensitivity/thr50",   expect = "some"),
  `75`    = list(env = c(EXCLUDE_SUBANNUAL_SCALING = "false", SCALE_MIN_COMPLETENESS = "75"),
                 folder = "data/sensitivity/sens01_scaling_sensitivity/thr75",   expect = "some"),
  `90`    = list(env = c(EXCLUDE_SUBANNUAL_SCALING = "false", SCALE_MIN_COMPLETENESS = "90"),
                 folder = "data/sensitivity/sens01_scaling_sensitivity/thr90",   expect = "some")
)

sel <- strsplit(Sys.getenv("SENS_SETTINGS", "noscale,25,50,75,90"), "\\s*,\\s*")[[1]]
bad <- setdiff(sel, names(VARIANTS))
if (length(bad)) stop("[sens] unknown setting(s): ", paste(bad, collapse = ", "),
  " (valid: ", paste(names(VARIANTS), collapse = ", "), ")", call. = FALSE)

run_step <- function(script) {
  message("\n>>> [sens] running ", script)
  if (system2("Rscript", script) != 0) stop("[sens] step FAILED: ", script, call. = FALSE)
}

for (name in sel) {
  v <- VARIANTS[[name]]
  message("\n==================== SETTING: ", name, "  ->  ", v$folder, " ====================")
  # clear both flags first, then set this variant's env (children inherit it)
  Sys.setenv(EXCLUDE_SUBANNUAL_SCALING = "false", SCALE_MIN_COMPLETENESS = "0")
  do.call(Sys.setenv, as.list(v$env))

  run_step("script/01b_select_best_record.R")
  bt <- file.path(v$folder, "Best_T_data_V1_3.csv")
  check(file.exists(bt), paste0("01b wrote ", bt))
  n_scaled <- sum(read.csv(bt)$scaled_to_annual, na.rm = TRUE)
  if (v$expect == "zero") {
    check(n_scaled == 0, paste0(name, ": 0 scaled records"))
  } else {
    check(n_scaled > 0, paste0(name, ": ", n_scaled, " scaled records (some kept, some dropped)"))
  }

  run_step("script/01c_annual_total_calibration.R")
  check(file.exists(file.path(v$folder, "dt_heatmap_calibrated.csv")),
        paste0("01c wrote ", v$folder, "/dt_heatmap_calibrated.csv"))

  run_step("script/01d_prep_data_model.R")
  check(file.exists(file.path(v$folder, "pred_data_disaggregate.csv")),
        paste0("01d wrote ", v$folder, "/pred_data_*.csv"))
}

message("\n>>> [sens] DONE. Variant prep is in data/sensitivity/sens01_scaling_sensitivity/<tag>/. Next step, the MI predictions per variant:")
for (name in sel) {
  v <- VARIANTS[[name]]
  flag <- if (name == "noscale") "EXCLUDE_SUBANNUAL_SCALING=true" else paste0("SCALE_MIN_COMPLETENESS=", name)
  message(sprintf(">>>   %s MI_N_RUNS=1 Rscript script/03b_run_pipeline_MI_full.R", flag))
}
