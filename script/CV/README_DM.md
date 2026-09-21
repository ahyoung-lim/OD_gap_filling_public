# Dirichlet-Multinomial (DM) disaggregation model: repeated cross-validation

How to reproduce the DM cross-validation run
(`script/CV/03_down_DM_full_repeatedCV_v2.py`) on a machine with an NVIDIA GPU.

## What the script does

Fits the DM_v3_pooled_jax_v3 model 27 times
(3 repetitions x 3 mask types x 3 folds) using the gap masks stored in
`runs/CV/20260126/masks/`, and scores each fit on the held-out country-years.

| Setting | Value |
|---|---|
| Sampler | NUTS via NumPyro/JAX, 4 chains (vectorized), 1000 draws, 1500 tuning steps, target_accept 0.99 |
| Posterior predictive draws for scoring | 500, evenly spaced across all chains |
| Seeds | `SEED_BASE + 10000*rep + 100*fold + mask_type_offset` (see `fit_score_one`) |
| Mask types | `interp` (interpolation), `extrap_past` (backcast), `extrap_future` (forecast) |

## Requirements

- Linux (tested on Ubuntu via WSL) with an NVIDIA GPU and CUDA 12 drivers.
  The script runs on CPU as well but is roughly 10x slower.
- Python 3.10 or later in a virtual environment. Package versions used for
  the reported results are listed in `script/CV/requirements_dm.txt`
  (generated with `pip freeze`) and `script/CV/python_version.txt`.

```bash
python -m venv pymc_env
source pymc_env/bin/activate
pip install -r script/CV/requirements_dm.txt
```

Check that JAX sees the GPU before a long run:

```bash
python -c "import jax; print(jax.devices())"
```

Expected output contains `CudaDevice` (or `gpu`). If it shows `CpuDevice`
only, reinstall JAX with CUDA support (`pip install "jax[cuda12]"`).

## Inputs

- `runs/CV/20260126/masks/downscaling_rep0{1,2,3}/inla_down_mask_*_gzip_v2.rds`
  The masks are committed to the repository. They were produced by
  `00_build_gap_mask_downscaling.R` (mask seeds 123, 456, 789) and converted
  for Python by `00_mask_conversion_pymc.R`.

No other data files are needed; each mask file carries the model input data.

## Run

From the repository root (the script uses paths relative to the working
directory):

```bash
source pymc_env/bin/activate
python script/CV/03_down_DM_full_repeatedCV_v2.py
```

Or from a Jupyter notebook:

```python
%cd /path/to/OD_gap_filling_public
%run script/CV/03_down_DM_full_repeatedCV_v2.py
```

Runtime is roughly 3 to 5 hours on a single consumer GPU. Progress is
printed per fit (`[rep 01] interp | fold 1 | ...`). The run is resumable:
finished fits are recorded per repetition, and re-running the same command
skips them.

## Outputs

All outputs go to `runs/CV/20260126/dm_rerun_4chains/`. The January 2026
results in `runs/CV/20260126/` are not modified.

Per repetition (`down_DM_full_CV_rep01/` etc.) and combined at the top level:

| File | Content |
|---|---|
| `pymc_metrics_monthly_fold.csv` / `pymc_metrics_monthly_repeatedCV_fold.csv` | Fold-level MAE, RMSE, COV80, CRPS on incidence per 100,000 |
| `pymc_metrics_monthly.csv` / `pymc_metrics_monthly_repeatedCV_overall.csv` | Metrics aggregated over folds, weighted by number of test rows |
| `pymc_row_predictions_fold.csv` | One row per test country-year-month: observed and predicted incidence (posterior predictive mean and median), 10% and 90% quantiles, CRPS. Input for the region-stratified tables. |
| `pymc_convergence_diagnostics_fold.csv` | Per fit: max R-hat, min ESS, divergences for the key hyperparameters |
| `pymc_convergence_by_variable_fold.csv` | Per fit and model variable: number of elements, max R-hat, count of elements with R-hat above 1.01 and 1.05, min bulk and tail ESS |
| `pymc_convergence_p_missing_fold.csv` | Per element of `p_missing` (predicted monthly proportions for held-out country-years): R-hat, bulk and tail ESS |
| `pymc_hyperparam_summaries_fold.csv` | Posterior median and 95% interval of the hyperparameters |
| `down_DM_full_prior_spec.csv` | Prior specification of the model |

## Reproducibility note

Seeds are fixed, so re-running on the same software and hardware gives the
same numbers. Different GPU models or JAX/NumPyro versions can change the
floating-point path of the sampler; results then agree within Monte Carlo
error but are not bit-identical.

## Downstream

`script/CV/CV_tab1_final_models_performance.R` produces the supplementary
disaggregation performance table from the fold-level metrics.
`script/CV/CV_tab3a_candidate_dm_diagnostics.R` produces the supplementary
convergence and hyperparameter tables from the diagnostics files.
`script/CV/CV_tab3b_metrics_by_region.R` reads `pymc_row_predictions_fold.csv`
and produces the region-stratified performance tables that
`script/CV/CV_tab3c_fig_metrics_by_region_heatmap.R` plots.
