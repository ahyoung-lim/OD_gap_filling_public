# OpenDengue Gap-Filling

Repository containing the data, analysis code, and key outputs supporting the findings reported in the associated publication (currently available as as [preprint](https://www.researchsquare.com/article/rs-8976788/v1)). This project performs temporal gap-filling and downscaling of the [OpenDengue](https://github.com/OpenDengue/master-repo) database (V1.3), producing complete national-level monthly dengue case estimates for 1990–2024. 

## Overview

This repository provides:

1. **Data processing** — cleaning, deduplication, and harmonisation of dengue case counts from OpenDengue and supplementary ad hoc sources
2. **Gap-filling models** — Bayesian spatio-temporal models (INLA) that impute missing weekly and monthly case counts, with posterior-predictive uncertainty
3. **Cross-validation** — repeated CV evaluation of INLA and PyMC benchmark models
4. **Downstream analyses** — consistency checks against WHO data, regional trend analysis, and wavelet synchrony analysis

## Repository structure

```
├── script/
│   ├── 00_setup.R                    # Environment setup and package loading
│   ├── 01a_ad_hoc_data_processing.R  # Compile and clean ad hoc dengue data
│   ├── 01b_select_best_record.R      # Select best record per country-year
│   ├── 01c_annual_total_calibration.R# Calibrate annual totals (IHME, nearest-neighbour)
│   ├── 01d_prep_data_model.R         # Prepare model input datasets
│   ├── 01e_gap_assessment.R          # Assess data gap characteristics
│   ├── 02a_pred_weekly.R             # INLA weekly prediction model
│   ├── 02b_pred_monthly.R            # INLA monthly prediction model
│   ├── 02c_pred_downscale.R          # Downscaling: fill remaining monthly gaps
│   ├── 02d_posterior_sampling.R      # Posterior predictive sampling with raking
│   ├── 03a_ad_hoc_impact.R           # Sensitivity analysis (ad hoc data impact)
│   ├── 03b_consistency_analysis.R    # Consistency checks vs WHO data sources
│   ├── 03c_wavelet_analysis.R        # Wavelet power and synchrony analysis
│   ├── fig1–fig4, sfig*.R            # Figure scripts
│   └── CV/                           # Cross-validation scripts (R + Python)
├── functions/                        # Reusable helper functions
├── data/
│   ├── raw_data/                     # External data (WHO, IHME)
│   ├── processed_data/               # Intermediate processed datasets
│   └── model_input/                  # Model-ready datasets
├── runs/                             # Model outputs and predictions (not tracked)
└── output/                           # Figures and tables
```

## Workflow

The numbered scripts are designed to be run sequentially:

1. **`01a`–`01e`**: Data processing pipeline (run once per OpenDengue release)
2. **`02a`–`02d`**: Prediction and imputation (weekly → monthly → downscale → posterior)
3. **`03a`–`03c`**: Downstream analyses (sensitivity, consistency, wavelets)
4. **`fig*` / `sfig*`**: Generate manuscript figures
5. **`CV/`**: Cross-validation evaluation (independent of main pipeline)


## Requirements
R Version: 4.4.2
Operating System: Windows 10 x64 (build 19045)

