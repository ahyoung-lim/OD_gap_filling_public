"""pymc_dm_v3_pooled_jax_v3_repeatedCV.py

Repeated 3-fold CV for the *final* PyMC model: DM_v3_pooled_jax_v3.

Key additions vs your previous single-pass CV script:
  1) Explicit, exportable prior specification (for paper/Supp).
  2) Convergence diagnostics per fit (R-hat/ESS/divergences) exported to CSV.
  3) Small posterior summaries of key hyperparameters exported to CSV.
  4) Metrics aligned to the INLA evaluation helpers (incidence-based MAE/RMSE,
     COV80 and CRPS from posterior predictive draws; COV80 in [0,1], not %).
  5) Repeated CV (3 repetitions) using masks saved under runs/CV/20260126/masks.

Outputs (by default):
  runs/CV/20260126/
    pymc_dm_v3_pooled_jax_v3_CV_monthly_rep01/
      pymc_metrics_monthly.csv
      pymc_metrics_monthly_fold.csv
      pymc_convergence_diagnostics_fold.csv
      pymc_hyperparam_summaries_fold.csv
    pymc_dm_v3_pooled_jax_v3_CV_monthly_rep02/
      ...
    pymc_dm_v3_pooled_jax_v3_CV_monthly_rep03/
      ...
    pymc_metrics_monthly_repeatedCV_overall.csv
    pymc_metrics_monthly_repeatedCV_fold.csv
    pymc_convergence_diagnostics_fold.csv
    pymc_hyperparam_summaries_fold.csv
    pymc_dm_v3_pooled_jax_v3_prior_spec.csv

Notes:
  - This keeps the model structure unchanged (your DM_v3_pooled_jax_v3).
  - The DM CV split remains year-level (country-year in/out) as in your existing
    PyMC workflow, but metrics are computed on the implied monthly rows to match
    the INLA metric definitions.
"""

from __future__ import annotations

import os
import re
import glob
import lzma
import tempfile
from dataclasses import dataclass
from datetime import date
from typing import Dict, List, Tuple, Optional

import numpy as np
import pandas as pd
import arviz as az
import pyreadr

import pytensor.tensor as pt
import pymc as pm
import pymc.sampling.jax as pmjax
import jax


# -----------------------------------------------------------------------------
# Global settings
# -----------------------------------------------------------------------------

jax.config.update("jax_enable_x64", True)

# ---- PILOT MODE ----
PILOT = False
PILOT_REP = 1
PILOT_MASK_TYPE = "interp"      # or "extrap_past" / "extrap_future"
PILOT_FOLD = 1
# --------------------

# Match your INLA incidence scaling
PER = 1e5
POP_FLOOR = 1.0

# Match INLA probabilistic metric safeguards
MIN_DRAWS = 20
MIN_PROP = 0.1

# Where INLA repeated-CV assets live (masks already generated elsewhere)
BASE_DIR = os.path.join("runs", "CV", "20260126")
MASK_ROOT = os.path.join(BASE_DIR, "masks")

# Output layout :
#   base_dir = runs/CV/20260126
#   per-rep folder: base_dir/pymc_dm_v3_pooled_jax_v3_CV_monthly_repXX/
#   consolidated repeated-CV tables saved directly under base_dir
MODEL_TAG = "down_DM_full"

def rep_out_dir(rep_i: int) -> str:
    return os.path.join(BASE_DIR, f"{MODEL_TAG}_CV_rep{rep_i:02d}")

# CV design
N_FOLDS = 3
FOLDS = list(range(1, N_FOLDS + 1))
MASK_SEEDS = [123, 456, 789]   # mirrors your R repeated-CV driver

# Keep your original small-total filtering (avoids degenerate all-zero years)
MIN_TOTAL_TRAIN = 1
MIN_TOTAL_TEST = 1

# Sampler settings (keep these modest for CV; increase for final paper fits)
CHAINS = 2
DRAWS = 500
TUNE = 1500
TARGET_ACCEPT = 0.99
SEED_BASE = 20250811

# Posterior predictive draws used for scoring
PPC_DRAWS = 300

# Optional: lightweight prior sensitivity check (kept off by default)
DO_PRIOR_SENSITIVITY = False
SENSITIVITY_WHICH = dict(rep_i=1, mask_type="interp", fold=1)  # tiny, defensible subset

RESUME = True  # set False to force re-run everything

# fold-level checkpointing helpers

def _job_key(mask_type: str, fold: int, prior_tag: str) -> str:
    return f"{mask_type}|{int(fold)}|{prior_tag}"

def load_done_keys(metrics_fp: str) -> set[str]:
    if not (RESUME and os.path.exists(metrics_fp)):
        return set()
    df = pd.read_csv(metrics_fp)
    if df.empty:
        return set()
    return set(_job_key(r["mask_type"], r["fold"], r["prior_tag"]) for _, r in df.iterrows())

def append_dict_csv(fp: str, row: dict) -> None:
    os.makedirs(os.path.dirname(fp), exist_ok=True)
    df = pd.DataFrame([row])
    df.to_csv(fp, mode="a", header=not os.path.exists(fp), index=False)

def append_list_csv(fp: str, rows: list[dict]) -> None:
    if not rows:
        return
    os.makedirs(os.path.dirname(fp), exist_ok=True)
    df = pd.DataFrame(rows)
    df.to_csv(fp, mode="a", header=not os.path.exists(fp), index=False)


# -----------------------------------------------------------------------------
# Prior specification (exportable)
# -----------------------------------------------------------------------------


@dataclass(frozen=True)
class PriorSpec:
    param_block: str
    param_name: str
    prior: str
    comment: str = ""


def prior_spec_dm_v3_pooled_jax_v3(fixed_rho: float = 0.7) -> List[PriorSpec]:
    """Explicit list of priors/hyperpriors for DM_v3_pooled_jax_v3."""
    return [
        PriorSpec("seasonality", "g_month", "Normal(0, 0.3)", "global seasonal wiggle; month dim"),
        PriorSpec("seasonality", "sigma_region_month", "HalfNormal(0.4)", "region×month SD for r_month"),
        PriorSpec("seasonality", "r_month", "Normal(g_month, sigma_region_month)", "region×month"),
        PriorSpec("seasonality", "tau_country_month", "HalfNormal(0.15)", "country SD around region pattern"),
        PriorSpec("seasonality", "c_month", "Normal(r_month[region_of_country], tau_country_month)", "country×month"),
        PriorSpec("year_effect", "tau_region_year", "HalfNormal(0.15)", "region SD for year effects"),
        PriorSpec("year_effect", "year_effect_raw", "Normal(0, 1)", "country×year"),
        PriorSpec("year_effect", "year_effect", f"MVN with AR(1) corr (rho={fixed_rho}) via Cholesky", "country×year"),
        PriorSpec("concentration", "log_concentration", "Normal(1.6, 0.35)", "country×year (concentration = exp(log_conc))"),
        PriorSpec("likelihood", "obs_dm", "DirichletMultinomial(n=annual_total, a=dir_means*concentration)", "training years only"),
    ]


# -----------------------------------------------------------------------------
# Mask discovery helpers
# -----------------------------------------------------------------------------


def infer_mask_type(path: str) -> str:
    name = os.path.basename(path).lower()
    if "extrap" in name and "future" in name:
        return "extrap_future"
    if "extrap" in name and "past" in name:
        return "extrap_past"
    if "dirfuture" in name:
        return "extrap_future"
    if "dirpast" in name:
        return "extrap_past"
    if "interp" in name:
        return "interp"
    return "unknown"


def parse_seed_from_path(path: str) -> Optional[int]:
    m = re.search(r"seed(\d+)", os.path.basename(path))
    return int(m.group(1)) if m else None


def read_rds_any(path: str) -> pd.DataFrame:
    """Read an .rds file into a pandas DataFrame.

    Works around masks saved with XZ compression (common when saveRDS(..., compress='xz')).
    If pyreadr cannot read the file directly and the file has an XZ header,
    we decompress to a temporary .rds and re-try.
    """
    try:
        r = pyreadr.read_r(path)
        return next(iter(r.values()))
    except Exception as e:
        # Detect XZ magic header: FD 37 7A 58 5A 00
        try:
            with open(path, 'rb') as f:
                head = f.read(6)
            if head == b'\xfd7zXZ\x00':
                with lzma.open(path, 'rb') as f:
                    data = f.read()
                with tempfile.NamedTemporaryFile(suffix='.rds', delete=False) as tmp:
                    tmp.write(data)
                    tmp_path = tmp.name
                try:
                    r2 = pyreadr.read_r(tmp_path)
                    return next(iter(r2.values()))
                finally:
                    try:
                        os.unlink(tmp_path)
                    except OSError:
                        pass
        except Exception:
            pass
        # Re-raise original error if not handled
        raise


def find_masks_for_rep(rep_i: int, mask_seed: int) -> Dict[str, str]:
    """Locate one mask file per mask_type for a repetition.

    We search recursively under MASK_ROOT, preferring paths containing 'repXX'.
    """
    rep_tag = f"rep{rep_i:02d}"
    all_rds = glob.glob(os.path.join(MASK_ROOT, "**", "*_gzip_v2.rds"), recursive=True)

    if not all_rds:
        raise FileNotFoundError(f"No .rds mask files found under: {MASK_ROOT}")

    rep_rds = [p for p in all_rds if rep_tag in p]
    if rep_rds:
        search_space = rep_rds
    else:
        # Common case for your masks: the seed is embedded in the filename.
        seed_tag = f"seed{int(mask_seed)}"
        seed_rds = [p for p in all_rds if seed_tag in os.path.basename(p)]
        search_space = seed_rds if seed_rds else all_rds

    # pick the first matching file per type
    out: Dict[str, str] = {}
    for p in sorted(search_space):
        mt = infer_mask_type(p)
        if mt in ("interp", "extrap_past", "extrap_future") and mt not in out:
            out[mt] = p
        if len(out) == 3:
            break

    missing = {"interp", "extrap_past", "extrap_future"} - set(out)
    if missing:
        # fall back to the full search space for missing types
        for p in sorted(all_rds):
            mt = infer_mask_type(p)
            if mt in missing:
                out[mt] = p
                missing.remove(mt)
            if not missing:
                break

    if missing:
        raise FileNotFoundError(
            f"Could not find masks for {sorted(missing)} (rep={rep_i}). "
            f"Searched under: {MASK_ROOT}"
        )
    return out


# -----------------------------------------------------------------------------
# Data prep for Dirichlet–Multinomial CV
# -----------------------------------------------------------------------------


def build_dm_arrays_from_mask(
    df_long: pd.DataFrame,
    fold_id: int,
    *,
    min_total_train: int = 5,
    min_total_test: int = 10,
) -> Dict[str, object]:
    """Build year-level train/test arrays from the month-level mask.

    Train = complete country-years with *no* months in fold_id.
    Test  = complete country-years with *any* month in fold_id.

    Notes:
      - Handles fold values that may be NA/NaN safely (treats them as not-test).
      - Optional strictness: raises if a (country, year) has multiple non-missing fold values.
    """
    df = df_long.copy()

    # Basic type hygiene
    df["month"] = pd.to_numeric(df["month"], errors="coerce").astype("Int64")
    df["Year"] = pd.to_numeric(df["Year"], errors="coerce").astype("Int64")

    if "region" not in df.columns:
        df["region"] = "UNKNOWN"

    need = {"adm_0_name", "Year", "month", "dengue_total", "fold", "annual_total", "pop_est"}
    miss = need - set(df.columns)
    if miss:
        raise ValueError(f"Mask df is missing columns: {sorted(miss)}")

    # Drop rows with unusable keys (can't form country-year-month otherwise)
    df = df.dropna(subset=["adm_0_name", "Year", "month"]).copy()
    df["month"] = df["month"].astype(int)
    df["Year"] = df["Year"].astype(int)

    # ---- FIX: fold can contain NaN, so never do astype(int) ----
    fold_num = pd.to_numeric(df["fold"], errors="coerce")  # NaN stays NaN
    df["is_test"] = (fold_num == int(fold_id))            # NaN -> False

    # Optional but recommended sanity check:
    # fold should be consistent within each (adm_0_name, Year) for non-missing values
    bad = (
        df.loc[~fold_num.isna(), ["adm_0_name", "Year"]]
        .assign(fold_num=fold_num.loc[~fold_num.isna()].values)
        .groupby(["adm_0_name", "Year"], observed=False)["fold_num"]
        .nunique()
    )
    bad = bad[bad > 1]
    if len(bad) > 0:
        example = bad.head(10).to_dict()
        raise ValueError(
            "Fold assignment is not consistent within some (adm_0_name, Year). "
            f"Examples (nunique folds > 1): {example}"
        )

    # Year-level summary
    yr = (
        df.groupby(["adm_0_name", "Year"], observed=False, as_index=False)
        .agg(
            n_months=("month", "nunique"),
            any_test=("is_test", "any"),
            annual_total=("annual_total", "first"),
        )
    )
    yr = yr.loc[yr["n_months"] == 12].copy()

    train_pairs = yr.loc[
        (~yr["any_test"]) & (yr["annual_total"] >= min_total_train),
        ["adm_0_name", "Year", "annual_total"],
    ]
    test_pairs = yr.loc[
        (yr["any_test"]) & (yr["annual_total"] >= min_total_test),
        ["adm_0_name", "Year", "annual_total"],
    ]

    # Pivot monthly counts (truth)
    pivot = (
        df.pivot_table(
            index=["adm_0_name", "Year"],
            columns="month",
            values="dengue_total",
            aggfunc="sum",
            observed=False,
        )
        .sort_index()
        .reindex(columns=range(1, 13))
        .fillna(0)
    )

    # Pivot monthly pop (needed for incidence metrics)
    pop_pivot = (
        df.pivot_table(
            index=["adm_0_name", "Year"],
            columns="month",
            values="pop_est",
            aggfunc="first",
            observed=False,
        )
        .sort_index()
        .reindex(columns=range(1, 13))
    )

    keep_idx = pd.MultiIndex.from_frame(
        pd.concat(
            [train_pairs[["adm_0_name", "Year"]], test_pairs[["adm_0_name", "Year"]]],
            ignore_index=True,
        )
    )
    pivot = pivot.loc[keep_idx]
    pop_pivot = pop_pivot.loc[keep_idx]

    countries = sorted(pivot.index.get_level_values(0).unique())
    years = sorted(pivot.index.get_level_values(1).unique())

    # region per country (mode)
    cty_region = (
        df[df["adm_0_name"].isin(countries)]
        .groupby("adm_0_name")["region"]
        .agg(lambda s: s.mode().iat[0] if len(s.mode()) else s.iloc[0])
    )
    regions = sorted(cty_region.unique())
    r_index = {r: i for i, r in enumerate(regions)}
    c_index = {c: i for i, c in enumerate(countries)}
    y_index = {y: i for i, y in enumerate(years)}
    region_idx_for_country = np.array([r_index[cty_region[c]] for c in countries], dtype="int64")

    # TRAIN arrays
    train_idx = [
        (r.adm_0_name, r.Year)
        for r in train_pairs.itertuples(index=False)
        if r.adm_0_name in countries and r.Year in years
    ]
    if len(train_idx) == 0:
        raise ValueError("No training country-years found after filtering.")

    fit_counts = np.stack([pivot.loc[(c, y)].values for (c, y) in train_idx]).astype(np.int64)
    fit_totals = fit_counts.sum(axis=1).astype(np.int64)
    fit_cidx = np.array([c_index[c] for (c, _) in train_idx], dtype="int64")
    fit_yidx = np.array([y_index[y] for (_, y) in train_idx], dtype="int64")

    # TEST arrays
    test_idx = [
        (r.adm_0_name, r.Year)
        for r in test_pairs.itertuples(index=False)
        if r.adm_0_name in countries and r.Year in years
    ]
    if len(test_idx) == 0:
        test_counts = np.zeros((0, 12), dtype=np.int64)
        test_totals = np.zeros((0,), dtype=np.int64)
        test_cidx = np.zeros((0,), dtype="int64")
        test_yidx = np.zeros((0,), dtype="int64")
        test_pop_month = np.zeros((0, 12), dtype=float)
    else:
        test_counts = np.stack([pivot.loc[(c, y)].values for (c, y) in test_idx]).astype(np.int64)
        test_totals = test_counts.sum(axis=1).astype(np.int64)
        test_cidx = np.array([c_index[c] for (c, _) in test_idx], dtype="int64")
        test_yidx = np.array([y_index[y] for (_, y) in test_idx], dtype="int64")
        test_pop_month = np.stack([pop_pivot.loc[(c, y)].values for (c, y) in test_idx]).astype(float)

    # Historical proportions (TRAIN only), on log scale
    props_df = pd.DataFrame(
        fit_counts,
        index=pd.MultiIndex.from_tuples(train_idx, names=["adm_0_name", "Year"]),
    )
    row_sums = props_df.sum(axis=1).replace(0, 1)
    props_df = props_df.div(row_sums, axis=0)
    props = (
        props_df.groupby(level="adm_0_name", observed=False)
        .mean()
        .reindex(countries)
        .fillna(1 / 12)
        .values
    )
    props_log = np.log(props + 1e-6)

    coords = {
        "country": countries,
        "year": years,
        "month": np.arange(12),
        "region": regions,
        "region_idx": region_idx_for_country,
    }

    return dict(
        coords=coords,
        props_log=props_log,
        fit_counts=fit_counts,
        fit_totals=fit_totals,
        fit_cidx=fit_cidx,
        fit_yidx=fit_yidx,
        test_counts=test_counts,
        test_totals=test_totals,
        test_cidx=test_cidx,
        test_yidx=test_yidx,
        test_pop_month=test_pop_month,
    )


# -----------------------------------------------------------------------------
# Model: DM_v3_pooled_jax_v3 (final)
# -----------------------------------------------------------------------------


def build_model_dm_v3_pooled_noscan_jax_v3(
    *,
    model_coords_fold: Dict[str, object],
    historical_props_log_fold: np.ndarray,  # (C, 12)
    fit_annual_totals_fold: np.ndarray,     # (N_fit,)
    fit_monthly_counts_fold: np.ndarray,    # (N_fit, 12)
    fit_country_idx_fold: np.ndarray,       # (N_fit,)
    fit_year_idx_fold: np.ndarray,          # (N_fit,)
    missing_annual_totals_fold: np.ndarray, # (N_test,)
    missing_country_idx_fold: np.ndarray,   # (N_test,)
    missing_year_idx_fold: np.ndarray,      # (N_test,)
    country_region_idx: np.ndarray,         # (C,)
    fixed_rho: float = 0.7,
    priors: Optional[Dict[str, float]] = None,
):
    """Your final DM model, with optional prior scaling for sensitivity checks."""
    priors = priors or {}

    # Default prior scales (baseline)
    g_month_sigma = float(priors.get("g_month_sigma", 0.3))
    sigma_region_month_sigma = float(priors.get("sigma_region_month_sigma", 0.4))
    tau_country_month_sigma = float(priors.get("tau_country_month_sigma", 0.15))
    tau_region_year_sigma = float(priors.get("tau_region_year_sigma", 0.15))
    log_conc_mu = float(priors.get("log_conc_mu", 1.6))
    log_conc_sigma = float(priors.get("log_conc_sigma", 0.35))

    coords = dict(model_coords_fold)
    C = len(coords["country"])
    Y = len(coords["year"])
    MONTHS = len(coords["month"])
    assert MONTHS == 12, "This builder assumes monthly data (12 months)."

    N_test = int(len(missing_annual_totals_fold))
    coords["obs_test"] = np.arange(N_test, dtype=int)

    r_idx = pt.as_tensor(np.asarray(country_region_idx, dtype=np.int64))
    fit_c = pt.as_tensor(np.asarray(fit_country_idx_fold, dtype=np.int64))
    fit_y = pt.as_tensor(np.asarray(fit_year_idx_fold, dtype=np.int64))
    mis_c = pt.as_tensor(np.asarray(missing_country_idx_fold, dtype=np.int64))
    mis_y = pt.as_tensor(np.asarray(missing_year_idx_fold, dtype=np.int64))

    with pm.Model(coords=coords) as model:
        hist = pt.as_tensor(np.asarray(historical_props_log_fold, dtype=float))

        # Region-specific month variance + simple global month baseline
        sigma_region_month = pm.HalfNormal(
            "sigma_region_month", sigma=sigma_region_month_sigma, dims=("region", "month")
        )
        g_month = pm.Normal("g_month", mu=0.0, sigma=g_month_sigma, dims=("month",))
        z_r_month = pm.Normal("z_r_month", mu=0.0, sigma=1.0, dims=("region", "month"))
        r_month = pm.Deterministic(
            "r_month",
            g_month + z_r_month * sigma_region_month,
            dims=("region", "month"),
        )

        tau_country_month = pm.HalfNormal("tau_country_month", sigma=tau_country_month_sigma)
        z_c_month = pm.Normal("z_c_month", mu=0.0, sigma=1.0, dims=("country", "month"))
        c_month = pm.Deterministic(
            "c_month",
            r_month[r_idx] + z_c_month * tau_country_month,
            dims=("country", "month"),
        )

        seasonal_logits = hist + c_month  # (C, 12)

        # Year effects with fixed AR(1) correlation across years (rho fixed)
        #tau_region_year = pm.HalfNormal("tau_region_year", sigma=tau_region_year_sigma, dims=("region",))
        #year_effect_raw = pm.Normal("year_effect_raw", mu=0.0, sigma=1.0, dims=("country", "year"))

        #corr = np.array([[fixed_rho ** abs(i - j) for j in range(Y)] for i in range(Y)], dtype=float)
        #L = np.linalg.cholesky(corr)
        #year_effect = pm.Deterministic(
        #    "year_effect",
        #    pt.dot(year_effect_raw, L.T) * tau_region_year[r_idx][:, None],
        #    dims=("country", "year"),
        #)

        #logits = seasonal_logits[:, None, :] + year_effect[:, :, None]  # (C, Y, 12)
        
        dir_means = pm.Deterministic(
            "dirichlet_means",
            pm.math.softmax(logits, axis=2),
            dims=("country", "year", "month"),
        )

        log_conc = pm.Normal(
            "log_concentration", mu=log_conc_mu, sigma=log_conc_sigma, dims=("country", "year")
        )
        total_conc = pm.Deterministic("total_concentration", pt.exp(log_conc))

        dir_stable = pt.maximum(dir_means, 1e-6)
        conc_stable = pt.maximum(total_conc, 1.0)
        a_param = dir_stable * conc_stable[:, :, None]  # (C, Y, 12)

        a_fit = a_param[(fit_c, fit_y)]
        pm.DirichletMultinomial(
            "obs_dm",
            n=pt.as_tensor(np.asarray(fit_annual_totals_fold, dtype=np.int64)),
            a=a_fit,
            observed=np.asarray(fit_monthly_counts_fold, dtype=np.int64),
        )

        a_missing = a_param[(mis_c, mis_y)]
        pm.Deterministic("a_missing", a_missing, dims=("obs_test", "month"))

        pm.Deterministic(
            "p_missing",
            a_missing / pt.sum(a_missing, axis=-1, keepdims=True),
            dims=("obs_test", "month"),
        )

    return model


# -----------------------------------------------------------------------------
# Posterior predictive draws (DM) using posterior a_missing
# -----------------------------------------------------------------------------


def _collapse_a_draws(trace: az.InferenceData, a_name: str = "a_missing", max_draws: int | None = None) -> np.ndarray:
    da = trace.posterior[a_name]
    da = da.stack(sample=("chain", "draw"))
    # Ensure explicit dims (sample, obs_test, month)
    if set(("obs_test", "month")) <= set(da.dims):
        da = da.transpose("sample", "obs_test", "month")
    else:
        # Fall back: assume the last two dims are obs, month
        other = [d for d in da.dims if d != "sample"]
        da = da.transpose("sample", other[0], other[1])

    a = da.values.astype(float)  # (S, N, 12)
    if max_draws is not None and a.shape[0] > max_draws:
        a = a[:max_draws]
    return a


def dm_expected_counts_from_a(
    trace: az.InferenceData,
    n_vec: np.ndarray,
    *,
    a_name: str = "a_missing",
    max_draws: int = 300,
) -> np.ndarray:
    """Posterior draws of the *expected* Dirichlet–Multinomial counts.

    This is the analogue of INLA's fitted-value summaries (mean/median) because it
    excludes the extra multinomial sampling noise.

    Returns expected counts with shape (S, N_obs, 12).
    """
    a = _collapse_a_draws(trace, a_name=a_name, max_draws=max_draws)  # (S, N, 12)
    S, N, M = a.shape
    n_vec = np.asarray(n_vec, dtype=float)
    if len(n_vec) != N:
        raise ValueError(f"test_totals length {len(n_vec)} != Nobs {N}")
    p = a / a.sum(axis=2, keepdims=True)
    mu = n_vec[None, :, None] * p
    return mu


def dm_ppc_from_alpha(
    alpha: np.ndarray,
    n_vec: np.ndarray,
    *,
    seed: int = 42,
) -> np.ndarray:
    """Posterior predictive Dirichlet–Multinomial draws given alpha.

    alpha: (S, N, 12) concentration parameters.
    Returns counts with shape (S, N, 12).
    """
    alpha = np.asarray(alpha, float)
    S, N, M = alpha.shape
    n_vec = np.asarray(n_vec, dtype=np.int64)
    if len(n_vec) != N:
        raise ValueError(f"test_totals length {len(n_vec)} != Nobs {N}")

    rng = np.random.default_rng(seed)
    out = np.empty((S, N, M), dtype=np.int64)

    # Per draw: sample all rows' Dirichlet via Gamma and then multinomial per row
    for s in range(S):
        g = rng.gamma(shape=np.clip(alpha[s], 1e-12, None), scale=1.0)  # (N, 12)
        p = g / g.sum(axis=1, keepdims=True)
        for i in range(N):
            out[s, i] = rng.multinomial(int(n_vec[i]), p[i])

    return out


def dm_ppc_from_a(
    trace: az.InferenceData,
    n_vec: np.ndarray,
    *,
    a_name: str = "a_missing",
    max_draws: int = 300,
    seed: int = 42,
) -> np.ndarray:
    """Generate Dirichlet–Multinomial posterior predictive draws.

    Returns counts with shape (S, N_obs, 12).
    """
    a = _collapse_a_draws(trace, a_name=a_name, max_draws=max_draws)  # (S, N, 12)
    return dm_ppc_from_alpha(a, n_vec, seed=seed)


# -----------------------------------------------------------------------------
# Metrics aligned to inla_eval_helpers.R
# -----------------------------------------------------------------------------


def mae_vec(p: np.ndarray, y: np.ndarray) -> float:
    return float(np.mean(np.abs(p - y)))


def rmse_vec(p: np.ndarray, y: np.ndarray) -> float:
    return float(np.sqrt(np.mean((p - y) ** 2)))


def crps_mc_safe_py(y: np.ndarray, S_mat: np.ndarray) -> np.ndarray:
    """Match crps_mc_safe() from inla_eval_helpers.R exactly.

    y: (N,)
    S_mat: (N, S) draws
    Returns per-row CRPS (N,)
    """
    y = np.asarray(y, float)
    S_mat = np.asarray(S_mat, float)

    # a = rowMeans(|S - y|)
    a = np.nanmean(np.abs(S_mat - y[:, None]), axis=1)

    # b computed per row from sorted samples
    N, S = S_mat.shape
    b = np.full(N, np.nan, dtype=float)
    for i in range(N):
        s = S_mat[i]
        s = s[np.isfinite(s)]
        if s.size < 2:
            continue
        s = np.sort(s)
        k = np.arange(1, s.size + 1, dtype=float)
        S0 = float(s.size)
        b[i] = (2.0 / (S0 ** 2)) * np.sum((2.0 * k - S0 - 1.0) * s)

    return a - 0.5 * b


def score_fold_incidence(
    *,
    predictive_samples_counts: np.ndarray,  # (S, N_years, 12) posterior predictive counts
    truth_counts: np.ndarray,               # (N_years, 12)
    pop_month: np.ndarray,                  # (N_years, 12)
) -> Dict[str, float]:
    """Compute incidence metrics per fold, aligned to the INLA pipeline.

    - MAE/RMSE are computed on incidence using point predictions derived from the
      posterior predictive distribution:
        * mean-based: predictive mean
        * median-based: predictive median ("typical" realised value)

    - COV80/CRPS are computed from the same posterior predictive draws.
    """
    pop_month = np.asarray(pop_month, float)
    truth_counts = np.asarray(truth_counts, float)

    # Filter like INLA: finite truth & pop >= POP_FLOOR
    ok = np.isfinite(truth_counts) & np.isfinite(pop_month) & (pop_month >= POP_FLOOR)
    if not np.any(ok):
        return dict(
            n_test=0,
            n_test_used=0,
            prop_used=np.nan,
            MAE_inc_mean=np.nan,
            RMSE_inc_mean=np.nan,
            MAE_inc_median=np.nan,
            RMSE_inc_median=np.nan,
            COV80=np.nan,
            CRPS=np.nan,
            CRPS80=np.nan,
        )

    # Convert to incidence
    inc_pred = PER * predictive_samples_counts / pop_month[None, :, :]
    inc_truth = PER * truth_counts / pop_month

    # Flatten to match INLA's "one row = one timepoint" logic
    y_vec = inc_truth[ok].reshape(-1)

    # Build draw matrix once: (N_rows, S)
    S_mat = inc_pred[:, ok].T
    S_mat = np.asarray(S_mat, float)
    S_mat[~np.isfinite(S_mat)] = np.nan

    # ---- Point metrics (posterior predictive mean/median) ----
    pred_mean_vec = np.nanmean(S_mat, axis=1)
    pred_median_vec = np.nanmedian(S_mat, axis=1)

    mae_mean = mae_vec(pred_mean_vec, y_vec)
    rmse_mean = rmse_vec(pred_mean_vec, y_vec)
    mae_median = mae_vec(pred_median_vec, y_vec)
    rmse_median = rmse_vec(pred_median_vec, y_vec)

    n_test = int(y_vec.size)

    # ---- Probabilistic metrics (predictive draws) ----
    S = int(S_mat.shape[1])
    thr = max(MIN_DRAWS, int(np.ceil(MIN_PROP * S)))
    keep = np.sum(np.isfinite(S_mat), axis=1) >= thr
    n_test_used = int(np.sum(keep))

    prop_used = (float(n_test_used) / float(n_test)) if n_test > 0 else np.nan


    if n_test_used == 0:
        cov80 = np.nan
        crps = np.nan
    else:
        S_keep = S_mat[keep, :]
        y_keep = y_vec[keep]

        q10 = np.nanquantile(S_keep, 0.10, axis=1)
        q90 = np.nanquantile(S_keep, 0.90, axis=1)
        cov80 = float(np.mean((y_keep >= q10) & (y_keep <= q90)))

        crps_rows = crps_mc_safe_py(y_keep, S_keep)
        crps = float(np.nanmean(crps_rows))

    return dict(
        n_test=n_test,
        n_test_used=n_test_used,
        prop_used=prop_used,
        MAE_inc_mean=mae_mean,
        RMSE_inc_mean=rmse_mean,
        MAE_inc_median=mae_median,
        RMSE_inc_median=rmse_median,
        COV80=cov80,
        CRPS=crps,
        CRPS80=crps,  # keep alias requested
    )


def safe_wmean(x: np.ndarray, w: np.ndarray) -> float:
    x = np.asarray(x, float)
    w = np.asarray(w, float)
    ok = np.isfinite(x) & np.isfinite(w) & (w > 0)
    if not np.any(ok):
        return float("nan")
    return float(np.average(x[ok], weights=w[ok]))


# -----------------------------------------------------------------------------
# Convergence + hyperparameter summaries
# -----------------------------------------------------------------------------


def convergence_diagnostics(trace: az.InferenceData, var_names: List[str]) -> Dict[str, float]:
    """Compact convergence summary (R-hat/ESS/divergences) over selected vars."""
    summ = az.summary(trace, var_names=var_names, round_to=None)

    rhat_max = float(np.nanmax(summ["r_hat"].values)) if "r_hat" in summ.columns else float("nan")
    ess_bulk_min = float(np.nanmin(summ["ess_bulk"].values)) if "ess_bulk" in summ.columns else float("nan")
    ess_tail_min = float(np.nanmin(summ["ess_tail"].values)) if "ess_tail" in summ.columns else float("nan")

    div = np.nan
    if hasattr(trace, "sample_stats") and "diverging" in trace.sample_stats:
        div = float(trace.sample_stats["diverging"].values.sum())

    return dict(rhat_max=rhat_max, ess_bulk_min=ess_bulk_min, ess_tail_min=ess_tail_min, divergences=div)


def scalar_hyperparam_summaries(trace: az.InferenceData) -> Dict[str, Tuple[float, float, float]]:
    """Small set of scalar summaries for paper/Supp table.

    This is robust to PyMC/xarray auto-dimension names (e.g. *_dim_0) by:
      1) reducing over preferred structural dims when they exist (e.g. country/year),
      2) otherwise reducing over all non-(chain, draw) dims.
    """
    post = trace.posterior

    def reduce_mean(da, prefer_dims: Tuple[str, ...]):
        dims = [d for d in prefer_dims if d in da.dims]
        if dims:
            return da.mean(dim=dims, skipna=True)
        other = [d for d in da.dims if d not in ("chain", "draw")]
        return da.mean(dim=other, skipna=True) if other else da

    def q_summary(da) -> Tuple[float, float, float]:
        if "chain" in da.dims and "draw" in da.dims:
            s = da.stack(sample=("chain", "draw")).values.astype(float)
        else:
            s = np.asarray(da.values, dtype=float).ravel()
        s = s[np.isfinite(s)]
        if s.size == 0:
            return (float("nan"), float("nan"), float("nan"))
        return (
            float(np.quantile(s, 0.50)),
            float(np.quantile(s, 0.025)),
            float(np.quantile(s, 0.975)),
        )

    out: Dict[str, Tuple[float, float, float]] = {}
    if "tau_country_month" in post:
        out["tau_country_month"] = q_summary(post["tau_country_month"])

    if "tau_region_year" in post:
        out["tau_region_year_mean"] = q_summary(reduce_mean(post["tau_region_year"], ("region",)))

    if "sigma_region_month" in post:
        out["sigma_region_month_mean"] = q_summary(reduce_mean(post["sigma_region_month"], ("region", "month")))

    if "log_concentration" in post:
        out["log_concentration_mean"] = q_summary(reduce_mean(post["log_concentration"], ("country", "year")))

    if "total_concentration" in post:
        out["total_concentration_mean"] = q_summary(reduce_mean(post["total_concentration"], ("country", "year")))

    return out

# ---- Key parameters to monitor ----
KEY_VARS = [
    "tau_country_month",
    "sigma_region_month",
    "tau_region_year",
    "log_concentration",
    "total_concentration",
]

def convergence_stats_key_params(trace, key_vars=None):
    """
    Compute convergence diagnostics (R-hat / ESS) restricted to key parameters.
    Compatible with older/newer ArviZ versions (no stat_focus/kind arguments).
    """
    import numpy as np
    import arviz as az

    if key_vars is None:
        key_vars = KEY_VARS

    present = [v for v in key_vars if v in trace.posterior.data_vars]
    if len(present) == 0:
        return {
            "key_vars_present": "",
            "rhat_max_key": np.nan,
            "ess_bulk_min_key": np.nan,
            "ess_tail_min_key": np.nan,
        }

    summ = az.summary(trace, var_names=present, round_to=None)

    cols = summ.columns
    return {
        "key_vars_present": ",".join(present),
        "rhat_max_key": float(np.nanmax(summ["r_hat"].values)) if "r_hat" in cols else np.nan,
        "ess_bulk_min_key": float(np.nanmin(summ["ess_bulk"].values)) if "ess_bulk" in cols else np.nan,
        "ess_tail_min_key": float(np.nanmin(summ["ess_tail"].values)) if "ess_tail" in cols else np.nan,
    }



# -----------------------------------------------------------------------------
# Main runner
# -----------------------------------------------------------------------------


def fit_score_one(
    *,
    df_long: pd.DataFrame,
    rep_i: int,
    mask_seed: int,
    mask_type: str,
    fold: int,
    model_name: str,
    fixed_rho: float = 0.7,
    prior_tag: str = "baseline",
    priors: Optional[Dict[str, float]] = None,
) -> Tuple[Dict[str, object], Dict[str, object], List[Dict[str, object]]]:
    """Fit the model for one (rep, mask_type, fold) and return:
      - metrics row
      - convergence row
      - hyperparam rows
    """
    A = build_dm_arrays_from_mask(
        df_long,
        fold_id=fold,
        min_total_train=MIN_TOTAL_TRAIN,
        min_total_test=MIN_TOTAL_TEST,
    )

    n_test_years = int(A["test_counts"].shape[0])
    if n_test_years == 0:
        metrics = dict(
            model=model_name,
            mask_type=mask_type,
            fold=fold,
            rep=rep_i,
            mask_seed=mask_seed,
            prior_tag=prior_tag,
            n_test=0,
            n_test_used=0,
            prop_used=np.nan,
            n_test_mean=0,
            n_test_median=0,
            MAE_inc_mean=np.nan,
            RMSE_inc_mean=np.nan,
            MAE_inc_median=np.nan,
            RMSE_inc_median=np.nan,
            COV80=np.nan,
            CRPS=np.nan,
            CRPS80=np.nan,
            n_test_years=0,
        )
        conv = dict(
            model=model_name,
            mask_type=mask_type,
            fold=fold,
            rep=rep_i,
            mask_seed=mask_seed,
            prior_tag=prior_tag,
            rhat_max=np.nan,
            ess_bulk_min=np.nan,
            ess_tail_min=np.nan,
            divergences=np.nan,
        )
        return metrics, conv, []

    coords = A["coords"]
    model = build_model_dm_v3_pooled_noscan_jax_v3(
        model_coords_fold=coords,
        historical_props_log_fold=A["props_log"],
        fit_annual_totals_fold=A["fit_totals"],
        fit_monthly_counts_fold=A["fit_counts"],
        fit_country_idx_fold=A["fit_cidx"],
        fit_year_idx_fold=A["fit_yidx"],
        missing_annual_totals_fold=A["test_totals"],
        missing_country_idx_fold=A["test_cidx"],
        missing_year_idx_fold=A["test_yidx"],
        country_region_idx=np.asarray(coords["region_idx"], dtype=np.int64),
        fixed_rho=fixed_rho,
        priors=priors,
    )

    # deterministic seed per job (mirrors your R pattern: base + rep + mask + fold)
    seed_job = int(SEED_BASE + 10_000 * rep_i + 100 * fold + (0 if mask_type == "interp" else (1 if mask_type == "extrap_future" else 2)))

    with model:
        trace = pmjax.sample_numpyro_nuts(
            draws=DRAWS,
            tune=TUNE,
            chains=CHAINS,
            target_accept=TARGET_ACCEPT,
            chain_method="vectorized",
            postprocessing_backend="cpu",
            compute_convergence_checks=True,
            random_seed=seed_job,
            
        )

    # Convergence diagnostics (monitor a small, relevant set)
    diag_vars = [
        "tau_country_month",
        "tau_region_year",
        "sigma_region_month",
        "log_concentration",
    ]
    conv_stats = convergence_diagnostics(trace, var_names=diag_vars)
    conv_stats.update(convergence_stats_key_params(trace))
    conv = dict(
        model=model_name,
        mask_type=mask_type,
        fold=fold,
        rep=rep_i,
        mask_seed=mask_seed,
        prior_tag=prior_tag,
        chains=CHAINS,
        draws=DRAWS,
        tune=TUNE,
        target_accept=TARGET_ACCEPT,
        **conv_stats,
    )

    # Hyperparameter summaries (small)
    hyp = scalar_hyperparam_summaries(trace)
    hyp_rows = []
    for k, (med, q025, q975) in hyp.items():
        hyp_rows.append(
            dict(
                model=model_name,
                mask_type=mask_type,
                fold=fold,
                rep=rep_i,
                mask_seed=mask_seed,
                prior_tag=prior_tag,
                param=k,
                median=med,
                q025=q025,
                q975=q975,
            )
        )

    # Posterior draws needed for posterior predictive scoring (counts)
    alpha_draws = _collapse_a_draws(trace, a_name="a_missing", max_draws=PPC_DRAWS)  # (S, N, 12)

    # Posterior predictive counts (for COV80/CRPS)
    predictive_counts = dm_ppc_from_alpha(alpha_draws, A["test_totals"], seed=seed_job)

    # Metrics (incidence) aligned to INLA helpers
    score = score_fold_incidence(
        predictive_samples_counts=predictive_counts,
        truth_counts=A["test_counts"],
        pop_month=A["test_pop_month"],
    )

    metrics = dict(
        model=model_name,
        mask_type=mask_type,
        fold=fold,
        rep=rep_i,
        mask_seed=mask_seed,
        prior_tag=prior_tag,
        n_test=score["n_test"],
        n_test_used=score["n_test_used"],
        n_test_mean=score["n_test"],
        n_test_median=score["n_test"],
        MAE_inc_mean=score["MAE_inc_mean"],
        RMSE_inc_mean=score["RMSE_inc_mean"],
        MAE_inc_median=score["MAE_inc_median"],
        RMSE_inc_median=score["RMSE_inc_median"],
        COV80=score["COV80"],
        CRPS=score["CRPS"],
        CRPS80=score["CRPS80"],
        n_test_years=n_test_years,
    )


    return metrics, conv, hyp_rows


def aggregate_over_folds(fold_df: pd.DataFrame) -> pd.DataFrame:
    """Per-rep overall by (model, mask_type, prior_tag) using INLA-style weighting."""
    gcols = ["model", "mask_type", "rep", "mask_seed", "prior_tag"]

    rows = []
    for keys, g in fold_df.groupby(gcols, dropna=False):
        w_point = g["n_test"].values.astype(float)
        w_prob = g["n_test_used"].values.astype(float) if "n_test_used" in g.columns else w_point
        row = dict(zip(gcols, keys))

        row["n_test"] = int(np.nansum(w_point))
        if "n_test_used" in g.columns:
            row["n_test_used"] = int(np.nansum(w_prob))
            row["prop_used"] = (float(row["n_test_used"]) / float(row["n_test"])) if row["n_test"] > 0 else np.nan
        row["n_test_mean"] = int(np.nansum(g["n_test_mean"].values.astype(float)))
        row["n_test_median"] = int(np.nansum(g["n_test_median"].values.astype(float)))

        # Point metrics (incidence) — mean-based and median-based
        row["MAE_inc_mean"] = safe_wmean(g["MAE_inc_mean"].values, w_point)
        row["RMSE_inc_mean"] = safe_wmean(g["RMSE_inc_mean"].values, w_point)
        row["MAE_inc_median"] = safe_wmean(g["MAE_inc_median"].values, w_point)
        row["RMSE_inc_median"] = safe_wmean(g["RMSE_inc_median"].values, w_point)

        # Probabilistic metrics
        row["COV80"] = safe_wmean(g["COV80"].values, w_prob)
        row["CRPS"] = safe_wmean(g["CRPS"].values, w_prob)
        row["CRPS80"] = safe_wmean(g["CRPS80"].values, w_prob)

        rows.append(row)

    return (
        pd.DataFrame(rows)
        .sort_values(["rep", "mask_type", "model", "prior_tag"])
        .reset_index(drop=True)
    )

def main() -> None:
    os.makedirs(BASE_DIR, exist_ok=True)

    # Export prior spec once (paper/Supp convenience)
    prior_rows = [ps.__dict__ for ps in prior_spec_dm_v3_pooled_jax_v3(fixed_rho=0.7)]
    pd.DataFrame(prior_rows).to_csv(
        os.path.join(BASE_DIR, f"{MODEL_TAG}_prior_spec.csv"),
        index=False,
    )

    model_name = "DM_v3_pooled_jax_v3"

    all_fold_metrics: List[Dict[str, object]] = []
    all_overall_metrics: List[pd.DataFrame] = []
    all_conv: List[Dict[str, object]] = []
    all_hyp: List[Dict[str, object]] = []

    for rep_i, mask_seed in enumerate(MASK_SEEDS, start=1):
        if PILOT and rep_i != PILOT_REP:
            continue

        rep_dir = rep_out_dir(rep_i)

        metrics_fp = os.path.join(rep_dir, "pymc_metrics_monthly_fold.csv")
        conv_fp    = os.path.join(rep_dir, "pymc_convergence_diagnostics_fold.csv")
        hyp_fp     = os.path.join(rep_dir, "pymc_hyperparam_summaries_fold.csv")

        done = load_done_keys(metrics_fp)
        if RESUME and done:
            print(f"[rep {rep_i:02d}] RESUME: found {len(done)} completed jobs in {metrics_fp}", flush=True)

        os.makedirs(rep_dir, exist_ok=True)

        masks = find_masks_for_rep(rep_i, mask_seed)

        # If piloting, keep only the chosen mask type (if present)
        if PILOT:
            masks = {k: v for k, v in masks.items() if k == PILOT_MASK_TYPE}
            if len(masks) == 0:
                raise ValueError(f"PILOT_MASK_TYPE='{PILOT_MASK_TYPE}' not found for rep {rep_i}")

        rep_fold_rows: List[Dict[str, object]] = []
        rep_conv_rows: List[Dict[str, object]] = []
        rep_hyp_rows: List[Dict[str, object]] = []

        for mask_type, mask_path in masks.items():
            # Load mask frame (handles XZ-compressed .rds)
            df_long = read_rds_any(mask_path)

            # If filename encodes a seed, use it (otherwise use rep seed)
            ms = parse_seed_from_path(mask_path)
            mask_seed_used = int(ms) if ms is not None else int(mask_seed)

            for fold in FOLDS:
                if PILOT and fold != PILOT_FOLD:
                    continue

                print(
                    f"[rep {rep_i:02d}] {mask_type} | fold {fold} | seed={mask_seed_used} | {os.path.basename(mask_path)}",
                    flush=True,
                )

                job = _job_key(mask_type, fold, "baseline")
                if RESUME and job in done:
                    print(f"[rep {rep_i:02d}] skip {mask_type} fold {fold} (baseline already done)", flush=True)
                    continue

                m, c, h = fit_score_one(
                    df_long=df_long,
                    rep_i=rep_i,
                    mask_seed=mask_seed_used,
                    mask_type=mask_type,
                    fold=fold,
                    model_name=model_name,
                    prior_tag="baseline",
                    priors=None,
                )
                append_dict_csv(metrics_fp, m)
                append_dict_csv(conv_fp, c)
                append_list_csv(hyp_fp, h)
                done.add(job)

                #rep_fold_rows.append(m)
                #rep_conv_rows.append(c)
                #rep_hyp_rows.extend(h)

                # Optional tiny prior sensitivity check
                if (
                    DO_PRIOR_SENSITIVITY
                    and rep_i == SENSITIVITY_WHICH["rep_i"]
                    and mask_type == SENSITIVITY_WHICH["mask_type"]
                    and fold == SENSITIVITY_WHICH["fold"]
                ):
                    alt_priors = dict(
                        g_month_sigma=0.5,
                        sigma_region_month_sigma=0.6,
                        tau_country_month_sigma=0.25,
                        tau_region_year_sigma=0.25,
                        log_conc_mu=1.6,
                        log_conc_sigma=0.7,
                    )
                    m2, c2, h2 = fit_score_one(
                        df_long=df_long,
                        rep_i=rep_i,
                        mask_seed=mask_seed_used,
                        mask_type=mask_type,
                        fold=fold,
                        model_name=model_name,
                        prior_tag="alt_priors",
                        priors=alt_priors,
                    )
                    rep_fold_rows.append(m2)
                    rep_conv_rows.append(c2)
                    rep_hyp_rows.extend(h2)

        # Save per-rep outputs (resume-safe: never overwrite fold CSVs here)
        rep_fold_df = pd.read_csv(metrics_fp) if os.path.exists(metrics_fp) else pd.DataFrame()
        rep_overall_df = aggregate_over_folds(rep_fold_df)
        rep_overall_df.to_csv(os.path.join(rep_dir, "pymc_metrics_monthly.csv"), index=False)

        # Accumulate across reps (resume-safe: read from disk)
        all_overall_metrics.append(rep_overall_df)

        if os.path.exists(metrics_fp):
            all_fold_metrics.append(pd.read_csv(metrics_fp))
        if os.path.exists(conv_fp):
            all_conv.append(pd.read_csv(conv_fp))
        if os.path.exists(hyp_fp):
            all_hyp.append(pd.read_csv(hyp_fp))


    # Save combined (across repetitions)
    fold_all = pd.concat(all_fold_metrics, ignore_index=True) if all_fold_metrics else pd.DataFrame()
    overall_all = pd.concat(all_overall_metrics, ignore_index=True) if all_overall_metrics else pd.DataFrame()
    conv_all = pd.concat(all_conv, ignore_index=True) if all_conv else pd.DataFrame()
    hyp_all = pd.concat(all_hyp, ignore_index=True) if all_hyp else pd.DataFrame()

    fold_all.to_csv(os.path.join(BASE_DIR, "pymc_metrics_monthly_repeatedCV_fold.csv"), index=False)
    overall_all.to_csv(os.path.join(BASE_DIR, "pymc_metrics_monthly_repeatedCV_overall.csv"), index=False)
    conv_all.to_csv(os.path.join(BASE_DIR, "pymc_convergence_diagnostics_fold.csv"), index=False)
    hyp_all.to_csv(os.path.join(BASE_DIR, "pymc_hyperparam_summaries_fold.csv"), index=False)



if __name__ == "__main__":
    main()
