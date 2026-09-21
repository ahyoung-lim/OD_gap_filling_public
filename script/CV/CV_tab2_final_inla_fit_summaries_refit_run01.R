# ==============================================================================
# Supplementary tables: posterior summaries (fixed effects and hyperparameters)
# of the final INLA gap-filling models
# ==============================================================================
# Sources of the three summaries:
#   weekly          the weekly fit shared by every MI run, read from the cache
#                   runs/mi_full/mi50/weekly/pred_imp_weekly_fit.rds (no refit)
#   monthly         refit once on the model input of MI run 1
#                   (runs/mi_full/mi50/monthly/run_01_monthly.csv)
#   disaggregation  refit once on the model input of MI run 1
#                   (runs/mi_full/mi50/disaggregation/run_01_downscale.csv)
# The two refits use the same formula, priors and INLA options as
# script/03b_run_pipeline_MI_full.R. The pipeline does not keep the monthly and
# disaggregation fit objects (one pair per run). Run files generated after
# 2026-09-08 carry these summaries (param_monthly / param_downscale) and the
# pool step of 03b writes them pooled over runs; this script is the substitute
# while the run set predates that field.
#
# Run on the machine that runs the MI pipeline (same INLA version), from the
# repository root:
#     Rscript script/CV/CV_tab2_final_inla_fit_summaries_refit_run01.R
# Roughly 45 min (monthly) + 55 min (disaggregation) plus loading the 3.7 GB
# weekly fit. MI_INLA_THREADS sets the INLA thread count (default 8, as in 03b).
#
# Outputs:
#   output/tables/CV_tab2_inla_fit_summaries_{weekly,monthly,disaggregation}.csv
#       one table per model in the layout of the supplementary document
#       (Parameter, Type, Median, CI_0.025, CI_0.975; rounded to 2 decimals)
#   runs/mi_full/descriptive_summary/CV_tab2_inla_fit_summaries_refit_run01_full.csv
#       all three models, unrounded, with the posterior mean and sd
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})
source("script/CV/00_imp_model_inla_spec.R")   # inla_m_hier_shared_formula, ctrl_fam

INLA::inla.setOption(num.threads = as.integer(Sys.getenv("MI_INLA_THREADS", "8")), save.memory = FALSE)

mi_dir  <- file.path("runs", "mi_full", "mi50")
full_csv <- file.path("runs", "mi_full", "descriptive_summary", "CV_tab2_inla_fit_summaries_refit_run01_full.csv")
docx_dir <- file.path("output", "tables")
dir.create(dirname(full_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(docx_dir, recursive = TRUE, showWarnings = FALSE)

say <- function(...) cat(sprintf(...), "\n", sep = "")

# Same layout as inla_param_summary() in 03b_run_pipeline_MI_full.R
inla_param_summary <- function(fit, model) {
  pick <- function(s, type) data.frame(
    model = model, Parameter = rownames(s), Type = type,
    Median = s[, "0.5quant"], CI_0.025 = s[, "0.025quant"], CI_0.975 = s[, "0.975quant"],
    Mean = s[, "mean"], SD = s[, "sd"], row.names = NULL, check.names = FALSE)
  rbind(pick(as.data.frame(fit$summary.fixed), "Fixed effect"),
        pick(as.data.frame(fit$summary.hyperpar), "Hyperparameter"))
}

# Model input of run 1 as written by 03b right after the fit: the model
# variables (y, countryx, yearx, time_seq, month_*) are in the file; the region
# factor is rebuilt from lat_band exactly as 03b builds it.
prep_stage_input <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$region  <- factor(df$lat_band)
  df$regionx <- as.integer(df$region)
  stopifnot(identical(df$y, df$dengue_total))
  chk <- sapply(c("countryx", "yearx", "regionx", "time_seq", "month",
                  "month_shared", "month_dev", "month_dev_country"), function(v) sum(is.na(df[[v]])))
  if (any(chk > 0)) stop("NA in model index(es): ", paste(names(chk)[chk > 0], collapse = ", "))
  df %>% arrange(adm_0_name, time_seq)
}

# Identical call to the monthly / disaggregation fits in 03b
fit_stage <- function(df) {
  INLA::inla(update(inla_m_hier_shared_formula, y ~ .), data = df, family = ctrl_fam$family,
    control.family = ctrl_fam$control.family, control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = FALSE, waic = FALSE, config = TRUE))
}

# ---- weekly: cached shared fit ------------------------------------------------
t0 <- Sys.time()
fit_w <- readRDS(file.path(mi_dir, "weekly", "pred_imp_weekly_fit.rds"))
say("weekly fit loaded in %.0f s | mlik=%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs")), fit_w$mlik[1])
summ_w <- inla_param_summary(fit_w, "weekly")
rm(fit_w); invisible(gc())

# ---- monthly: refit on run 1 input -------------------------------------------
df_m <- prep_stage_input(file.path(mi_dir, "monthly", "run_01_monthly.csv"))
t0 <- Sys.time()
fit_m <- fit_stage(df_m)
say("monthly refit: %d rows, %.0f s | mlik=%.1f", nrow(df_m), as.numeric(difftime(Sys.time(), t0, units = "secs")), fit_m$mlik[1])
summ_m <- inla_param_summary(fit_m, "monthly")
rm(fit_m); invisible(gc())

# ---- disaggregation: refit on run 1 input ------------------------------------
df_d <- prep_stage_input(file.path(mi_dir, "disaggregation", "run_01_downscale.csv"))
t0 <- Sys.time()
fit_d <- fit_stage(df_d)
say("disaggregation refit: %d rows, %.0f s | mlik=%.1f", nrow(df_d), as.numeric(difftime(Sys.time(), t0, units = "secs")), fit_d$mlik[1])
summ_d <- inla_param_summary(fit_d, "disaggregation")
rm(fit_d); invisible(gc())

# ---- table ---------------------------------------------------------------------
tab <- bind_rows(summ_w, summ_m, summ_d)
write.csv(tab, full_csv, row.names = FALSE)
say("wrote %s (%d rows)", full_csv, nrow(tab))
for (m in c("weekly", "monthly", "disaggregation")) {
  docx <- tab %>% filter(model == m) %>%
    transmute(Parameter, Type, Median = round(Median, 2),
              CI_0.025 = round(CI_0.025, 2), CI_0.975 = round(CI_0.975, 2))
  fp <- file.path(docx_dir, sprintf("CV_tab2_inla_fit_summaries_%s.csv", m))
  write.csv(docx, fp, row.names = FALSE)
  cat(sprintf("\n=== %s (%s) ===\n", m, fp))
  print(as.data.frame(docx), row.names = FALSE)
}
