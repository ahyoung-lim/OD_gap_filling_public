# OpenDengue Gap-Filling

Code and data for temporal gap-filling and downscaling of the [OpenDengue](https://github.com/OpenDengue/master-repo) database, producing complete national-level monthly dengue case estimates for 1990–2024.

## Overview

This repository provides:

1. **Data processing** — cleaning, deduplication, and harmonisation of dengue case counts from OpenDengue and supplementary ad hoc sources
2. **Gap-filling models** — Bayesian spatio-temporal models (INLA) that impute missing weekly and monthly case counts, with posterior-predictive uncertainty
3. **Cross-validation** — repeated CV evaluation of the INLA models against a Dirichlet-multinomial benchmark fitted in PyMC
4. **Downstream analyses** — consistency checks against WHO data, regional trend analysis, and wavelet synchrony analysis

## Repository structure

```
├── script/
│   ├── 00_setup.R                          # Environment setup and package loading
│   ├── 01a_ad_hoc_data_processing.R        # Compile and clean ad hoc dengue data
│   ├── 01b_select_best_record.R            # Select best record per country-year
│   ├── 01c_annual_total_calibration.R      # Calibrate annual totals (IHME, nearest-neighbour)
│   ├── 01d_prep_data_model.R               # Prepare model input datasets
│   ├── 01e_gap_assessment.R                # Assess data gap characteristics
│   ├── 02a_run_ad_hoc_impact.R             # Run 01b with / without ad hoc data, then 02b
│   ├── 02b_ad_hoc_impact.R                 # Ad hoc data impact comparison
│   ├── 03a_run_prep_production.R           # Run 01b -> 01c -> 01d for the production inputs
│   ├── 03b_run_pipeline_MI_full.R          # Gap filling with multiple imputation (weekly -> monthly -> downscale, 50 runs)
│   ├── 03c_mi_datasets.R                   # Helper: imputed datasets, released gap-filled file
│   ├── 04a_consistency_analysis.R          # Consistency checks vs WHO data sources
│   ├── 04b_mi_growth_pool.R                # Growth rates, pooled across imputations
│   ├── 04c_mi_wavelet_trends.R             # Wavelet power and synchrony trends, pooled across imputations
│   ├── 04d_mi_wavelet_power_by_emergence_group.R  # Wavelet power trends by when dengue became established (Supp Fig 10)
│   ├── sens01a_scaling_sensitivity_prep.R  # Sensitivity 1: sub-annual scaling cutoffs (inputs)
│   ├── sens01b_scaling_sensitivity.R       # Sensitivity 1: comparison across cutoffs
│   ├── sens02_growth_excl_zero.R           # Sensitivity 2: growth excluding pre-emergence / assumed-zero years (figure)
│   ├── sens03_mi_wavelet_excl_disaggregation.R  # Sensitivity 3: wavelet trends excluding disaggregated months
│   ├── fig1–fig4, sfig*.R                  # Figure scripts
│   ├── fig_source_data_xlsx.R              # Source data workbooks for the main-text figures
│   ├── CV/                                 # Cross-validation scripts (R + Python)
│   └── arx/                                # Archived scripts (single-run pipeline, earlier wavelet analyses)
├── functions/                              # Reusable helper functions
├── data/
│   ├── raw_data/                           # External data (WHO, IHME)
│   ├── ad_hoc/                             # Supplementary ad hoc dengue data
│   ├── processed_data/                     # Intermediate processed datasets
│   ├── model_input/                        # Model-ready datasets
│   └── sensitivity/sens01_scaling_sensitivity/   # Model inputs per scaling cutoff
├── runs/                                   # Model outputs (only the pooled results and released datasets are tracked)
│   ├── mi_full/                            # Production MI run: mi50/ (fits, draws, released file), descriptive_summary/, growth/, wavelet/
│   ├── sensitivity/                        # sens01 runs per cutoff, sens03 wavelet cache
│   └── CV/                                 # Cross-validation fits and metrics
└── output/
    ├── figures/, tables/                   # Main figures and tables
    ├── source_data/                        # Per-figure source data (CSV per panel, one xlsx per figure)
    └── figures/sensitivity/, tables/sensitivity/   # Sensitivity analysis outputs (sens01-03)
```

## Workflow

The numbered scripts are designed to be run sequentially:

1. **`01a`–`01e`**: Data processing pipeline (run once per OpenDengue release; `03a` chains 01b–01d)
2. **`02a`–`02b`**: Ad hoc data impact
3. **`03b`–`03c`**: Gap filling with multiple imputation (weekly → monthly → downscale → posterior draws, 50 runs) and the released dataset
4. **`04a`–`04d`**: Downstream analyses (WHO consistency, growth rates, wavelets)
5. **`sens01`–`sens03`**: Sensitivity analyses
6. **`fig*` / `sfig*`**: Generate manuscript figures
7. **`fig_source_data_xlsx.R`**: Collect the panel CSVs written by the Fig. 1–4 scripts into one workbook per figure
8. **`CV/`**: Cross-validation evaluation (independent of main pipeline)

## Requirements

- **R** ≥ 4.0 with INLA (see `00_setup.R` for full package list)
- **Python** ≥ 3.9 with PyMC ≥ 5.0 and NumPyro/JAX (for the Dirichlet-multinomial CV benchmark only)

## Data

External inputs are read from two folders whose locations are set at the top of
`script/00_setup.R` (defaults below; override with the environment variables
`OD_RELEASE_DIR` and `OD_DEV_DIR`). All paths in the scripts are relative to the
repository root; run the scripts from there.

| Input | Default location | How to obtain | Read by |
|---|---|---|---|
| OpenDengue temporal extract (`Temporal_extract_V1_3_2026_07_29.csv`) | `data/opendengue_release/` | The OpenDengue temporal extract used in this analysis, as of 29 July 2026. Download from figshare: [FIGSHARE LINK] | `01a`, `01b` |
| OpenDengue-Dev filing database (`archive/filingDB_allV_*.xlsx`) | `data/opendengue_dev/` | Internal, not distributable. Used only by `01a` to fill source metadata of the ad hoc records and to check record identifiers against the previous release. The outputs of `01a` (`data/processed_data/ad_hoc_*.csv`) are included, so `01a` does not need to be rerun. | `01a` |
| WHO data | `data/raw_data/` | Included in the repository | `04a` |
| IHME GBD 2021 dengue estimates (`IHME-GBD_2021_DATA.csv`) and the compiled ad hoc surveillance data | `data/ad_hoc/` | Included in the repository | `01a`, `01c`, `03b` |

Other sources: UN World Population Prospects (population denominators), IHME
Global Burden of Disease 2021 (annual-total calibration).

## Citation

[To be added upon publication]

## License

[To be specified]
