# ==============================================================================
# AD HOC IMPACT — run 01b twice (with / without ad hoc) then compare  [01b x2 -> 02b]
# ==============================================================================
# Quantifies what the ad hoc data adds by running the SAME 01b selection pipeline
# twice and comparing the two outputs:
#   1) default             -> data/processed_data/Best_T_data_V1_3.csv          (OD + ad hoc)
#   2) EXCLUDE_AD_HOC=true  -> data/processed_data/Best_T_data_V1_3_excl_ad_hoc.csv (OD only)
#
# Because both runs use the identical 01b code (only the ad hoc merge is toggled by
# the EXCLUDE_AD_HOC flag), the ONLY difference between the two files is ad hoc
# presence -- no stale hand-copied selection logic to drift out of sync.
#
# Runs in the DEFAULT (scaling-applied) mode; EXCLUDE_SUBANNUAL_SCALING is forced
# off so a leftover no-scale flag can't redirect these into data/sensitivity/sens01_scaling_sensitivity/noscale/.
#
# Usage:   Rscript script/02a_run_ad_hoc_impact.R
# ==============================================================================

stopifnot("run from repo root" = file.exists("script/01b_select_best_record.R"))
check <- function(ok, msg) {
  if (!isTRUE(ok)) {
    stop("[ad_hoc] SAFEGUARD FAIL: ", msg, call. = FALSE)
  } else {
    message(">>> [ad_hoc] CHECK OK: ", msg)
  }
}

# keep this comparison in the 100% (scaling-applied) space regardless of any leftover flag
Sys.setenv(EXCLUDE_SUBANNUAL_SCALING = "false")

run_step <- function(script) {
  message("\n>>> [ad_hoc] running ", script)
  if (system2("Rscript", script) != 0) stop("[ad_hoc] step FAILED: ", script, call. = FALSE)
}

# --- 01b run 1: WITH ad hoc (default) -----------------------------------------
Sys.setenv(EXCLUDE_AD_HOC = "false")
run_step("script/01b_select_best_record.R")
bt <- "data/processed_data/Best_T_data_V1_3.csv"
check(file.exists(bt), "01b (with ad hoc) wrote Best_T_data_V1_3.csv")
check("ad_hoc_data" %in% read.csv(bt)$cat, "with-ad-hoc output contains ad_hoc_data records")

# --- 01b run 2: WITHOUT ad hoc (OD only) --------------------------------------
Sys.setenv(EXCLUDE_AD_HOC = "true")
run_step("script/01b_select_best_record.R")
Sys.setenv(EXCLUDE_AD_HOC = "false") # reset so nothing downstream inherits it
nah <- "data/processed_data/Best_T_data_V1_3_excl_ad_hoc.csv"
check(file.exists(nah), "01b (no ad hoc) wrote Best_T_data_V1_3_excl_ad_hoc.csv (default untouched)")
check(!("ad_hoc_data" %in% read.csv(nah)$cat), "no-ad-hoc output has 0 ad_hoc_data records")

# --- 02b: compare the two -----------------------------------------------------
run_step("script/02b_ad_hoc_impact.R")

message("\n>>> [ad_hoc] DONE. Comparison written by 02b to data/processed_data/ad_hoc_comparison.csv")
