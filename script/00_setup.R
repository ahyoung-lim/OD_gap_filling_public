# ----------------------------------------------------------
# Load packages and source functions/other scripts
# ----------------------------------------------------------

# ----------------------------------------------------------
# External input locations (edit the defaults or set the environment variables)
# ----------------------------------------------------------
# OpenDengue temporal extract used in this analysis (as of 29 July 2026):
# download Temporal_extract_V1_3_2026_07_29.csv from figshare ([FIGSHARE LINK])
# into this folder. Read by 01a and 01b.
OD_RELEASE_DIR <- Sys.getenv("OD_RELEASE_DIR", "data/opendengue_release")
# OpenDengue-Dev filing database (internal, not distributable): archive/filingDB_allV_*.xlsx.
# Read only by 01a to fill source metadata and to check UUIDs against the previous
# release; the outputs of that step (data/processed_data/ad_hoc_*.csv) are included
# in the repository, so 01a does not need to be rerun to reproduce the analysis.
OD_DEV_DIR <- Sys.getenv("OD_DEV_DIR", "data/opendengue_dev")

# load packages
# fmt: skip
pacman::p_load(
  dplyr, tidyr, tidyverse, lubridate, EpiWeek, countrycode,
  stringi, zoo, data.table,
  ggplot2, patchwork, here,
  rnaturalearth, rnaturalearthdata, sf, spData, spdep,
  mapview, countrycode,
  INLA, mgcv,
  foreach, doParallel, tictoc,
  distill,
  future, future.apply, progressr, purrr
  # SuperLearner, xgboost, ranger, tuneRanger, nnls,
  # caret, caretEnsemble
)


conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("first", "data.table")
conflicted::conflict_prefer("month", "lubridate")
conflicted::conflict_prefer("year", "lubridate")
conflicted::conflict_prefer("union", "base")
conflicted::conflict_prefer("train", "caret")
conflicted::conflict_prefer("between", "dplyr")


# Clean up from previous code / runs
rm(list = ls(all = TRUE))


# global variables
# data period for analysis
min_year <- 1990
max_year <- 2024

# for parallel processing
ncl <- detectCores()

# for file names
today <- gsub("-", "_", Sys.Date())

# code structure ...........................................

# TYPICAL RUN -- from the OpenDengue release to the figures
#
#   01a  ad hoc data compilation ....... once per release; its outputs
#     |                                  (data/processed_data/ad_hoc_*.csv) are in
#     |                                  the repository, so it is rarely rerun
#   03a  runs 01b -> 01c -> 01d ........ production model inputs
#     |                                  (data/model_input/pred_data_*.csv)
#   03b  gap filling with multiple imputation, 50 runs
#     |                                  (runs/mi_full/mi50/)
#   03c  helper: one complete monthly dataset per posterior draw
#     |
#   04a 04b 04c 04d  downstream analyses ... (output/tables/)
#     |
#   fig*.R / sfig*.R  figures .......... (output/figures/)
#
#   Off the main line:
#     01e          gap characteristics of the processed data (descriptive only)
#     02a -> 02b   what the ad hoc data adds: 02a reruns 01b with and without the
#                  ad hoc records, 02b compares the two outputs. Not an input of
#                  03b, but sfig_sankey_table.R reads the no-ad-hoc file 02a writes
#                  (Best_T_data_V1_3_excl_ad_hoc.csv).
#     sens01-03    sensitivity analyses
#     CV/          cross-validation
#
# 01b, 01c and 01d do the processing; 02a, 03a and sens01a are runners that call
# them with different environment-variable settings (ad hoc on/off, sub-annual
# scaling), each writing to its own folder.
#
#
# STEP 1: PREPARE & PROCESS OD DATA (run once per OpenDengue release)
#   script/01a_ad_hoc_data_processing.R     compile the ad hoc data
#   script/01b_select_best_record.R         best record per country-year
#   script/01c_annual_total_calibration.R   annual-total calibration
#   script/01d_prep_data_model.R            model input datasets
#   script/01e_gap_assessment.R             gap characteristics
#   (script/03a_run_prep_production.R runs 01b -> 01c -> 01d in one go)
#
# STEP 2: AD HOC DATA IMPACT (not an input of STEP 3; used by sfig_sankey_table.R)
#   script/02a_run_ad_hoc_impact.R          runs 01b with / without ad hoc, then 02b
#   script/02b_ad_hoc_impact.R              comparison
#
# STEP 3: GAP FILLING WITH MULTIPLE IMPUTATION (requires INLA)
#   script/03b_run_pipeline_MI_full.R       weekly -> monthly -> downscale, 50 runs, pooling
#   script/03c_mi_datasets.R                helper: imputed datasets + released file
#
# STEP 4: DOWNSTREAM ANALYSES
#   script/04a_consistency_analysis.R       WHO consistency (sourced by fig1, fig2)
#   script/04b_mi_growth_pool.R             growth rates, MI-pooled
#   script/04c_mi_wavelet_trends.R          wavelet power / synchrony trends, MI-pooled
#
# SENSITIVITY ANALYSES:  script/sens01*, sens02*, sens03*
# FIGURES:               script/fig*.R, script/sfig*.R
# CROSS-VALIDATION:      script/CV/
#
# ----------------------------------------------------------
