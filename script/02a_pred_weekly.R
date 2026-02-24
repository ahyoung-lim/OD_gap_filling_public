source(file.path(getwd(), "script/imp_model_inla_spec.R"))
source(file.path(getwd(), "script/eval_helpers.R"))


data <- read.csv(file.path(getwd(), "data/model_input/pred_data_weekly.csv"))
nrow(data)
df <- data %>%
  mutate(
    y = dengue_total,
    region = factor(lat_band),
    regionx = as.integer(region),
    week52 = ((as.integer(week) - 1L) %% 52L) + 1L, # 1..52
    week_shared = week52, # global backbone index
    week_dev = week52, # region deviations use same index, replicated by region
    week_dev_country = week52 # country deviations use same index, replicated by country
  ) %>%
  arrange(adm_0_name, time_seq)


mod <- inla_w_hier_shared_formula


seed <- 123
set.seed(seed)

Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) RhpcBLASctl::blas_set_num_threads(1)


# Sequential mode:
# Give INLA most physical cores for sequential mode
INLA::inla.setOption(num.threads = 8, save.memory = FALSE)


fit <- INLA::inla(
  formula = update(mod, y ~ .),
  data = df,
  family = ctrl_fam$family,
  control.family = ctrl_fam$control.family,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = FALSE, waic = FALSE, config = TRUE)
)


df$pred_mean <- fit$summary.fitted.values$mean
df$pred_lwr <- fit$summary.fitted.values$`0.025quant`
df$pred_upr <- fit$summary.fitted.values$`0.975quant`

saveRDS(fit, file.path(getwd(), "runs/pred/pred_imp_weekly_fit.rds"))


write.csv(df, file.path(getwd(), "runs/pred/pred_imp_weekly.csv"), row.names = FALSE)
