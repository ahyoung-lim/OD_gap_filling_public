## ----------------------------------------------------------------------
## Validate that each record’s start and end dates fall in the same epiweek
## ----------------------------------------------------------------------

check_epiweek_span <- function(data, env = .GlobalEnv) {
  ## ------------------------------------------------------------------
  ## Build start/end year + epi‑week numbers
  ## ------------------------------------------------------------------
  data$Year_start <- NA_integer_
  data$Year_end <- NA_integer_
  data$epiweek_start <- NA_integer_
  data$epiweek_end <- NA_integer_

  for (i in seq_len(nrow(data))) {
    data$Year_start[i] <- dateToEpiweek(data$calendar_start_date[i])$year
    data$Year_end[i] <- dateToEpiweek(data$calendar_end_date[i])$year
    data$epiweek_start[i] <- dateToEpiweek(data$calendar_start_date[i])$weekno
    data$epiweek_end[i] <- dateToEpiweek(data$calendar_end_date[i])$weekno
  }

  data$Year_epiweek_start <- paste0(data$Year_start, "_", data$epiweek_start)
  data$Year_epiweek_end <- paste0(data$Year_end, "_", data$epiweek_end)

  mismatches <- data %>%
    dplyr::filter(Year_epiweek_start != Year_epiweek_end)

  ## ------------------------------------------------------------------
  ## Side‑effect: create or remove 'mismatches' in the chosen environment
  ## ------------------------------------------------------------------
  if (nrow(mismatches) > 0) {
    assign("mismatches", mismatches, envir = env)
    message(sprintf(
      "⚠️  %d record(s) record(s) span two different epiweeks. Inspect the 'mismatches' object for details.",
      nrow(mismatches)
    ))
    return(data)
  } else {
    if (exists("mismatches", envir = env)) rm("mismatches", envir = env)
    message("✓ All records fall within a single epi‑week.")
  }

  invisible(mismatches)
}
