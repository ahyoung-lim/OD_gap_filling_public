# ----------------------------------------------------------
# Load packages and source functions/other scripts
# ----------------------------------------------------------

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

# STEP 1: PREPARE & PROCESS OD DATA (run once)
# When new OpenDengue versions are available, the following
# scripts should be run only once to prepare the data
#
#   source("script/00_select_Best_T_res.R")
#   source("script/00_gap_assessment.R")  # Assessing missing data in OD Temporal Extract
#   source("script/01_prep_data.R")
#
# STEP 2: CREATE CROSS-VALIDATION FOLDS & FIT MODELS
# These scripts generate training/testing folds and fit models
# These steps should be rerun whenever models need to be updated
#
#   source("script/02_model_fitting_gam.R")    # Fits GAM models
#   source("script/02_model_fitting_inla.R")   # Fits INLA models
#
# STEP 3: EVALUATE & VISUALIZE MODEL PERFORMANCE
# assess models performance and visualise the results
#
#   source("script/03_model_eval.R")
#
# ----------------------------------------------------------
