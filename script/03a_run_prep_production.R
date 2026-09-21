# ==============================================================================
# PREPROCESSING FOR THE PRODUCTION RUN  [01b -> 01c -> 01d]
# ==============================================================================
# Runs 01b -> 01c -> 01d in the default mode, with sub-annual scaling applied at
# the default completeness cutoff (100%), and checks after each step that the
# expected file was written. The scaling flag is set explicitly below so that a
# value left in the environment by the sens01 scripts cannot redirect the
# outputs into the sensitivity folders.
#
# Requires the EpiWeek package (v1.1): 01d builds the weekly master calendar with
# epiweekToDate(), which must return MMWR weeks starting on Sunday. If weeks start
# on another day, Sunday-aligned series (e.g. Taiwan) fall outside the calendar
# and the time_seq check in 01d stops the run.
#
# Usage:   Rscript script/03a_run_prep_production.R
# Output:  data/processed_data/*, data/model_input/*   (the inputs of 03b)
# Next:    MI_N_RUNS=50 Rscript script/03b_run_pipeline_MI_full.R
# ==============================================================================

stopifnot("run from repo root" = file.exists("script/03b_run_pipeline_MI_full.R"))
check <- function(ok, msg) if (!isTRUE(ok)) stop("[full] SAFEGUARD FAIL: ", msg, call. = FALSE) else
  message(">>> [full] CHECK OK: ", msg)

# make sure a leftover no-scale flag from an earlier command does NOT leak in and
# silently redirect the 100% run into data/sensitivity/sens01_scaling_sensitivity/noscale/
Sys.setenv(EXCLUDE_SUBANNUAL_SCALING = "false")

run_step <- function(script) {
  message("\n>>> [full] running ", script)
  if (system2("Rscript", script) != 0) stop("[full] step FAILED: ", script, call. = FALSE)
}

# --- 01b: select best record (sub-annual scaling applied) ---------------------
run_step("script/01b_select_best_record.R")
bt <- "data/processed_data/Best_T_data_V1_3.csv"
check(file.exists(bt), "01b wrote data/processed_data/Best_T_data_V1_3.csv")
check(sum(read.csv(bt)$scaled_to_annual, na.rm = TRUE) > 0,
  "01b output has scaled records (scaling IS applied)")

# --- 01c: annual total calibration --------------------------------------------
run_step("script/01c_annual_total_calibration.R")
check(file.exists("data/processed_data/dt_heatmap_calibrated.csv"),
  "01c wrote data/processed_data/dt_heatmap_calibrated.csv")

# --- 01d: prep model / prediction inputs --------------------------------------
run_step("script/01d_prep_data_model.R")
check(file.exists("data/model_input/pred_data_disaggregate.csv"),
  "01d wrote data/model_input/pred_data_*.csv")

message("\n>>> [full] DONE. Inputs are in data/processed_data/ and data/model_input/")
message(">>> Next step, gap filling with multiple imputation:")
message(">>>   MI_N_RUNS=50 Rscript script/03b_run_pipeline_MI_full.R")
message(">>> It writes the pooled prediction to runs/mi_full/mi50/mi_pooled_cells.csv")
