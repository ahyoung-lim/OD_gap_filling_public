# ==============================================================================
# MI IMPUTED DATASETS — helper to expose each posterior draw as a complete
# monthly dataset for the downstream growth-rate and wavelet analyses.
# ==============================================================================
# The MI pipeline (03b_run_pipeline_MI_full.R) writes runs/mi_full/mi50/disaggregation/run_*.rds, each
# holding a cells x R=100 matrix of posterior draws. One column of that matrix,
# across all runs, is ONE complete 1990-2024 monthly dataset. Pooling all
# N_RUNS x R columns is the multiple-imputation ensemble.
#
# This helper does three things so the existing downstream scripts need only
# swap their read.csv() for get_imputed(k):
#   1. Loads every completed run_*.rds and stacks the draw columns, recording
#      which run each column came from (so a sample can be spread across runs to
#      capture between-run / cascade uncertainty).
#   2. Attaches the pipeline's own country metadata (pop_est, lat_band, ISO_A0,
#      od_region) from map_final (functions/fn_load_map_shp.R) -- the same source
#      the wavelet script uses -- cached to disk so the shapefile load happens
#      once, not every session.
#   3. Restores the deterministic-zero country-years that the pipeline skips
#      (data_source == "Assumed_zero_cases" in dt_heatmap_calibrated.csv:
#      1,158 country-years x 12 = 13,896 cells, all zero). These carry no
#      uncertainty but ARE part of the time series, giving the full 60,060-cell
#      coverage that matches the released gap-filled dataset.
#
# It does NOT modify any existing script.
#
# Usage:
#   source("script/03c_mi_datasets.R")
#   mi   <- mi_load()                         # load runs + build/attach metadata
#   plan <- mi_sample_plan(mi, n_per_run = 1) # e.g. 1 draw/run -> m = n_runs
#   d    <- get_imputed(mi, plan, 1)          # imputed dataset #1 (data.frame)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# ------------------------------------------------------------------------------
# Country metadata (pop_est, lat_band, ISO_A0, Latitude, Longitude, od_region),
# per adm_0_name x Year. Built once from map_final and cached to an rds so the
# heavy shapefile / population load is not repeated. Delete the cache to rebuild.
# ------------------------------------------------------------------------------
mi_build_country_meta <- function(cache_path = "runs/mi_full/mi50/country_meta.rds",
                                  rebuild = FALSE) {
  if (!rebuild && file.exists(cache_path)) return(readRDS(cache_path))
  # map_final: one row per (country, Year) with pop_est + centroid-derived lat_band
  source("functions/fn_load_map_shp.R")            # defines map_final, add_od_regions
  meta <- map_final %>%
    sf::st_drop_geometry() %>%
    transmute(adm_0_name = as.character(adm_0_name), ISO_A0 = iso_a3, Year,
              pop_est, lat_band = as.character(lat_band), Latitude, Longitude) %>%
    add_od_regions(iso_col = "ISO_A0") %>%
    distinct(adm_0_name, Year, .keep_all = TRUE)
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(meta, cache_path)
  meta
}

# ------------------------------------------------------------------------------
# Deterministic-zero cells: the Assumed_zero_cases country-years, expanded to
# 12 months, dengue = 0. Canonical source: the 01c EES rule (emerging country
# with a missing annual total -> assumed 0).
# ------------------------------------------------------------------------------
mi_zero_cells <- function(heatmap_csv = "data/processed_data/dt_heatmap_calibrated.csv") {
  h <- read.csv(heatmap_csv, stringsAsFactors = FALSE)
  z <- h[h$data_source == "Assumed_zero_cases", c("adm_0_name", "Year"), drop = FALSE]
  z$adm_0_name <- toupper(z$adm_0_name)
  tidyr::crossing(z, month = 1:12)
}

# ------------------------------------------------------------------------------
# Load all completed runs, stack draw columns, and build the static scaffold
# (all cells x all metadata) that get_imputed() fills in.
# ------------------------------------------------------------------------------
mi_load <- function(run_dir       = "runs/mi_full/mi50/disaggregation",
                    heatmap_csv   = "data/processed_data/dt_heatmap_calibrated.csv",
                    meta_cache    = "runs/mi_full/mi50/country_meta.rds",
                    check_fingerprint = TRUE) {
  run_files <- sort(list.files(run_dir, pattern = "^run_\\d+\\.rds$", full.names = TRUE))
  if (length(run_files) == 0) stop("No run_*.rds found in ", run_dir)
  runs  <- lapply(run_files, readRDS)
  keys0 <- runs[[1]]$keys
  for (r in runs) if (!identical(r$keys, keys0))
    stop("Run key mismatch: runs were built on different cell grids.")

  # every pooled run must share one input fingerprint (mirrors the pipeline guard)
  if (check_fingerprint) {
    fps <- vapply(runs, function(x) if (is.null(x$input_fp)) NA_character_ else x$input_fp,
                  character(1))
    if (any(is.na(fps)) || length(unique(fps)) != 1)
      stop("Runs carry different/absent input fingerprints; do not pool them.")
  }

  draws      <- do.call(cbind, lapply(runs, function(x) x$draws))  # cells x (n_run*R)
  R          <- ncol(runs[[1]]$draws)
  run_id     <- as.integer(sub("^run_0*", "", tools::file_path_sans_ext(basename(run_files))))
  run_of_col <- rep(run_id, each = R)                              # source run per column
  message(sprintf("mi_load: %d runs x %d draws = %d imputed datasets available (%d modelled cells)",
                  length(runs), R, ncol(draws), nrow(draws)))

  # modelled cells: split "adm_0_name|Year|month"
  km <- do.call(rbind, strsplit(keys0, "|", fixed = TRUE))
  modelled <- data.frame(adm_0_name = km[, 1],
                         Year  = as.integer(km[, 2]),
                         month = as.integer(km[, 3]),
                         draw_row = seq_along(keys0),      # row index into `draws`
                         stringsAsFactors = FALSE)

  # deterministic-zero cells (draw_row = NA -> filled with 0)
  zero <- mi_zero_cells(heatmap_csv)
  overlap <- dplyr::semi_join(zero, modelled, by = c("adm_0_name", "Year", "month"))
  if (nrow(overlap) > 0)
    stop(sprintf("%d zero cells overlap modelled cells; the zero source is wrong.",
                 nrow(overlap)))
  zero$draw_row <- NA_integer_

  cells <- dplyr::bind_rows(modelled, zero)

  # attach metadata; time_seq is the global month index (1 = Jan 1990)
  meta <- mi_build_country_meta(meta_cache)
  scaffold <- cells %>%
    dplyr::left_join(meta, by = c("adm_0_name", "Year")) %>%
    dplyr::mutate(time_seq = (Year - 1990L) * 12L + month) %>%
    dplyr::arrange(adm_0_name, time_seq)

  miss_pop <- sum(is.na(scaffold$pop_est))
  if (miss_pop > 0)
    warning(sprintf("%d cells have no pop_est after metadata join (check map_final coverage).",
                    miss_pop))

  list(draws = draws, run_of_col = run_of_col, R = R,
       n_runs = length(runs), run_id = run_id,
       scaffold = scaffold)
}

# ------------------------------------------------------------------------------
# Sampling plan: pick n_per_run draw-columns from EACH run so the m = n_runs *
# n_per_run imputed datasets are spread across runs (captures between-run
# cascade uncertainty). Returns the chosen column indices into mi$draws.
# ------------------------------------------------------------------------------
mi_sample_plan <- function(mi, n_per_run = 1, seed = 123) {
  set.seed(seed)
  cols <- unlist(lapply(mi$run_id, function(rr) {
    pool <- which(mi$run_of_col == rr)
    if (n_per_run > length(pool))
      stop(sprintf("n_per_run=%d exceeds the %d draws in run %d.", n_per_run, length(pool), rr))
    sample(pool, n_per_run)
  }))
  message(sprintf("mi_sample_plan: m = %d datasets (%d per run x %d runs)",
                  length(cols), n_per_run, mi$n_runs))
  cols
}

# ------------------------------------------------------------------------------
# mi_write_release(): build the RELEASED gap-filled dataset in one fixed layout,
# so every downstream consumer (figure scripts, external users) reads one file
# with the same columns:
#   adm_0_name, ISO_A0, Year, month, time_seq, pop_est, lat_band,
#   dengue_total_scaled, dengue_lwr_scaled, dengue_upr_scaled,
#   imputed_weekly, imputed_monthly, disaggregated_yearly, od_region
# Sources: point estimates + 95% intervals from the pipeline's pooled cell file
# (runs/mi_full/mi50/mi_pooled_cells.csv, modelled + assumed-zero cells); country
# metadata from mi_build_country_meta(); the provenance flags from the downscale
# stage table (their positions are determined by the observed-data pattern, so
# run 1's table is representative; disaggregated_yearly = no sub-annual value at
# the downscale stage). Assumed-zero cells carry FALSE flags (deterministic 0).
#
# Usage:  Rscript -e 'source("script/03c_mi_datasets.R"); mi_write_release()'
# ------------------------------------------------------------------------------
mi_write_release <- function(cells_csv = "runs/mi_full/mi50/mi_pooled_cells.csv",
                             stage_csv = "runs/mi_full/mi50/disaggregation/run_01_downscale.csv",
                             out_csv   = "runs/mi_full/mi50/opendengue_gap_filled_MI.csv") {
  cells <- read.csv(cells_csv, stringsAsFactors = FALSE)
  # tolerate a modelled-only cell file (pre zero-restore): append the zero cells
  zero <- mi_zero_cells()
  missing_zero <- dplyr::anti_join(zero, cells, by = c("adm_0_name", "Year", "month"))
  if (nrow(missing_zero) > 0) {
    cells <- dplyr::bind_rows(cells, missing_zero %>%
      dplyr::mutate(dengue_total_scaled = 0L, dengue_lwr_scaled = 0L, dengue_upr_scaled = 0L))
  }
  flags <- read.csv(stage_csv, stringsAsFactors = FALSE) %>%
    dplyr::transmute(adm_0_name, Year, month,
                     imputed_weekly, imputed_monthly,
                     disaggregated_yearly = is.na(dengue_total))
  meta <- mi_build_country_meta()
  rel <- cells %>%
    dplyr::left_join(flags, by = c("adm_0_name", "Year", "month")) %>%
    dplyr::mutate(imputed_weekly = dplyr::coalesce(imputed_weekly, FALSE),
                  imputed_monthly = dplyr::coalesce(imputed_monthly, FALSE),
                  disaggregated_yearly = dplyr::coalesce(disaggregated_yearly, FALSE)) %>%
    dplyr::left_join(meta %>% dplyr::select(adm_0_name, ISO_A0, Year, pop_est, lat_band, od_region),
                     by = c("adm_0_name", "Year")) %>%
    dplyr::mutate(time_seq = (Year - 1990L) * 12L + month) %>%
    dplyr::select(adm_0_name, ISO_A0, Year, month, time_seq, pop_est, lat_band,
                  dengue_total_scaled, dengue_lwr_scaled, dengue_upr_scaled,
                  imputed_weekly, imputed_monthly, disaggregated_yearly, od_region) %>%
    dplyr::arrange(adm_0_name, time_seq)
  stopifnot(nrow(rel) == 60060,
            !anyNA(rel$pop_est), !anyNA(rel$od_region),
            !anyNA(rel$ISO_A0), !anyNA(rel$lat_band))
  # provenance-flag consistency with the pooling's lock definition, where available
  if ("is_locked" %in% names(cells)) {
    chk <- cells %>% dplyr::left_join(flags, by = c("adm_0_name", "Year", "month")) %>%
      dplyr::mutate(dplyr::across(c(imputed_weekly, imputed_monthly, disaggregated_yearly),
                                  ~ dplyr::coalesce(.x, FALSE)))
    bad <- sum(chk$is_locked == (chk$imputed_weekly | chk$imputed_monthly | chk$disaggregated_yearly))
    if (bad > 0) warning(sprintf("%d cells have is_locked inconsistent with the provenance flags", bad))
  }
  write.csv(rel, out_csv, row.names = FALSE)
  message(sprintf("wrote %s (%d rows; global total %s)",
                  out_csv, nrow(rel), format(sum(rel$dengue_total_scaled), big.mark = ",")))
  invisible(rel)
}

# ------------------------------------------------------------------------------
# get_imputed(mi, plan, k): the k-th imputed dataset as a data.frame in the
# layout the downstream scripts expect. dengue_total_scaled = the draw for
# modelled cells, 0 for zero cells.
# lwr/upr are NA: a single imputed dataset is one realisation, not an interval.
# ------------------------------------------------------------------------------
get_imputed <- function(mi, plan, k) {
  if (k < 1 || k > length(plan)) stop("k out of range: 1..", length(plan))
  col <- plan[k]
  s <- mi$scaffold
  y <- integer(nrow(s))
  is_mod <- !is.na(s$draw_row)
  y[is_mod]  <- mi$draws[s$draw_row[is_mod], col]
  y[!is_mod] <- 0L
  data.frame(adm_0_name = s$adm_0_name, ISO_A0 = s$ISO_A0, Year = s$Year,
             month = s$month, time_seq = s$time_seq, pop_est = s$pop_est,
             lat_band = s$lat_band, od_region = s$od_region,
             Latitude = s$Latitude, Longitude = s$Longitude,
             dengue_total_scaled = y,
             dengue_lwr_scaled = NA_integer_, dengue_upr_scaled = NA_integer_,
             stringsAsFactors = FALSE)
}
