# Time series plot
# data includes observed and predicted cases for different countries
base_TS_plot <- function(mod_name) {
  data = get(mod_name)
  p <- ggplot(data = data) +
    geom_point(aes(x = ymd(calendar_start_date), y = pred), color = "blue") + # predicted data
    geom_line(aes(x = ymd(calendar_start_date), y = dengue_total)) + # observed data
    scale_x_date(labels = scales::date_format("%Y")) +
    ggtitle(paste0(mod_name)) +
    guides(color = guide_legend(title = "mod")) +
    xlab(NULL) +
    theme_bw() +
    facet_wrap(adm_0_name ~ ., scales = "free")

  return(p)
}


# scatter plot (obs vs. pred)
base_scatter_plot <- function(mod_name, log_scale = TRUE, by_country = TRUE) {
  data = get(mod_name)

  if (log_scale) {
    # log-transformed outcomes and predictions
    obs_var <- "log_dengue_total"
    pred_var <- "stack"
    xlab <- "Observed cases (log scale)"
    ylab <- "Predicted cases (log scale)"
  } else {
    # incidence per 10^5
    obs_var <- "obs_inc"
    pred_var <- "pred_inc"
    xlab <- "Observed incidence per 100000 population"
    ylab <- "Predicted incidence per 100000 population"
  }

  # obs <- data[[obs_var]]
  # preds <- data[[pred_var]]

  maxv = max(c(data[[obs_var]], data[[pred_var]]), na.rm = TRUE)

  p <- ggplot(data) +
    geom_point(
      aes(x = data[[obs_var]], y = data[[pred_var]]),
      color = "#00bfc4"
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linewidth = 0.5,
      color = "black",
      linetype = "dashed"
    ) +
    coord_cartesian(xlim = c(0, maxv), ylim = c(0, maxv)) +
    xlab(xlab) +
    ylab(ylab) +
    facet_wrap(adm_0_name ~ ., scales = "free")

  if (!by_country) {
    data = na.omit(data)
    maxv = max(c(data[[obs_var]], data[[pred_var]]), na.rm = TRUE)
    rmse <- sqrt(mean((data[[obs_var]] - data[[pred_var]])^2, na.rm = TRUE))

    p <- ggplot(data) +
      geom_point(
        aes(x = data[[obs_var]], y = data[[pred_var]]),
        color = "#00bfc4",
        size = 2
      ) +
      geom_abline(
        slope = 1,
        intercept = 0,
        linewidth = 0.5,
        color = "black",
        linetype = "dashed"
      ) +
      coord_cartesian(xlim = c(0, maxv), ylim = c(0, maxv)) +
      ggtitle(paste0(mod_titles[mod_name], " (RMSE: ", round(rmse, 3), ")")) +
      xlab(xlab) +
      ylab(ylab)
  }

  return(p)
}


summary.CV.caretStack <- function(mod_name, by_country = TRUE) {
  data = na.omit(data.frame(get(mod_name))) # filter to held-out predictions only

  # incidence per 10^5
  obs_var <- "log_dengue_total"
  pred_var <- "stack"
  base_var_suffix <- ""

  # get the name of base learners
  idx <- match("stack", names(data)) # column location of "stack"
  base_model_names <- names(data)[idx - seq_len(num_models - 1)]
  base_var_names <- paste0(base_model_names, base_var_suffix)

  n <- nrow(data)

  # stack prediction
  SL.predict <- data %>% select(all_of(c(obs_var, pred_var)))

  # base learner predictions
  library.predict <- data %>% select(all_of(c(base_var_names, obs_var)))

  # Risk estimate
  Risk.SL <- SL.predict %>%
    summarise(
      mean = sqrt(mean((!!sym(obs_var) - !!sym(pred_var))^2, na.rm = TRUE))
    ) %>%
    t()

  Risk.library <- library.predict %>%
    pivot_longer(
      -c(obs_var),
      names_to = "model",
      values_to = "pred"
    ) %>%
    mutate(squared_err = (!!sym(obs_var) - pred)^2) %>%
    group_by(model) %>%
    summarise(
      rmse = sqrt(mean(squared_err, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_wider(., names_from = model, values_from = rmse) %>%
    ungroup() %>%
    select(sort(base_var_names))

  Sd.library <- unlist(lapply(sort(base_var_names), function(base_var_name) {
    sd(sqrt((library.predict[[obs_var]] - library.predict[[base_var_name]])^2))
  }))

  se <- (1 / sqrt(n)) *
    c(sd(sqrt((SL.predict[[obs_var]] - SL.predict[[pred_var]])^2)), Sd.library)

  # base learners vs. meta learner (global)
  d <- data.frame(
    Y = unlist(c(Risk.SL["mean", ], Risk.library)), # Risk estimate
    X = c("stack", sort(base_model_names)), # Algorithm names
    se = se
  ) %>%
    mutate(
      Lower = Y - 1.96 * se, # 95% CIs
      Upper = Y + 1.96 * se,
      X = reorder(X, -Y)
    )

  # base learners vs. meta learner (by country)
  d_country <- data %>%
    group_by(adm_0_name) %>%
    summarise(across(
      all_of(c(pred_var, base_var_names)),
      ~ sqrt(mean((!!sym(obs_var) - .)^2, na.rm = TRUE)),
      .names = "{.col}"
    )) %>%
    ungroup() %>%
    suppressWarnings()

  p <- ggplot(d, aes(x = X, y = Y, ymin = Lower, ymax = Upper)) +
    geom_pointrange() +
    coord_flip() +
    ylab("V-fold CV Risk Estimate") +
    xlab("Method") +
    ggtitle(mod_name)

  if (by_country) {
    return(list(table = d, plot = p, table_country = d_country))
  } else {
    return(list(table = d, plot = p))
  }
}


# Function to check if any test data point has been used more than once
# Just to ensure that test points are used only once across different folds
check_oos <- function(mod_name) {
  data <- get(mod_name)

  dup <- data %>%
    group_by(adm_0_name, time_seq) %>%
    tally() %>%
    filter(n > 1)

  return(dup)
}


# ARCHIVE ====================================================================
# FOR GAM AND INLA MODELS:
# Function to compute Mean Absolute Error (MAE) for each country and globally

MAE_function <- function(mod_name) {
  data = get(mod_name)
  # Get the number of unique countries
  ncountry <- length(unique(data$adm_0_name))

  # Initialise dataframe to store MAE results
  table <- matrix(
    NA,
    nrow = ncountry,
    ncol = 1,
    dimnames = list(c(unique(data$adm_0_name)), c("mae"))
  )

  # Compute MAE for each country
  for (i in 1:ncountry) {
    country_filter <- data$countryx == i # Filter data for the current country

    # Calculate MAE for the new model
    table[i, "mse"] <- mean(
      data$pred_inc[country_filter], # new model prediction
      data$obs_inc[country_filter], # observed incidence
      na.rm = TRUE
    )
  }

  table_global <- matrix(
    nrow = 1,
    ncol = 1,
    dimnames = list(c("GLOBAL"), c("mse"))
  )

  table_global[, "mse"] <- hydroGOF::mae(
    data$pred_inc, # new model prediction
    data$obs_inc, # observed incidence
    na.rm = TRUE
  )
  table <- rbind(table, table_global)
  return(table)
}
