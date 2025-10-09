library(data.table)

# 1. Count CDC-style epiweeks in a year
count_epiweeks <- function(year) {
  start_date <- as.Date(paste0(year, "-01-01"))
  end_date <- as.Date(paste0(year + 1, "-01-01"))
  all_dates <- seq(start_date, end_date - 1, by = "day")
  epiweeks <- epiweek(all_dates)

  first_wk1_idx <- which(epiweeks == 1)[1]
  valid_epiweeks <- epiweeks[first_wk1_idx:length(epiweeks)]

  length(unique(valid_epiweeks))
}

# 2. Create epiweek lookup for each year
build_epiweek_lookup <- function(start_year, end_year) {
  tibble(
    Year = start_year:end_year,
    epiweeks = sapply(start_year:end_year, count_epiweeks)
  )
}

# 3. Generate CDC epiweek-aligned weekly calendar
generate_weekly_calendar <- function(start_year, end_year, epiweek_table) {
  rows <- lapply(start_year:end_year, function(y) {
    n_weeks <- epiweek_table$epiweeks[epiweek_table$Year == y]
    dates <- seq(
      EpiWeek::epiweekToDate(y, 1)$d0,
      EpiWeek::epiweekToDate(y, n_weeks)$d0,
      by = "7 days"
    )
    tibble(
      calendar_start_date = as.Date(dates),
      Year = y,
      week = 1:length(dates)
    )
  })
  bind_rows(rows) %>%
    mutate(time_seq = row_number())
}


# 4. Main Function: Complete weekly country-year data
make_week_complete_clean <- function(data, keep_vars = FALSE) {
  data$calendar_start_date <- as.Date(data$calendar_start_date)

  # Build epiweek table
  year_range <- range(data$Year, na.rm = TRUE)
  epiweek_table <- build_epiweek_lookup(start_year = year_range[1], end_year = year_range[2])

  # Create master weekly calendar
  weekly_calendar <- generate_weekly_calendar(year_range[1], year_range[2], epiweek_table)

  # Get unique country-year combinations
  unique_country_years <- data %>%
    distinct(adm_0_name, ISO_A0, Year)

  # Cross join country-year with calendar
  weekly_template <- merge(unique_country_years, weekly_calendar, by = "Year", all = TRUE)

  # Join observed data with complete week structure
  data_complete <- full_join(
    weekly_template,
    data,
    by = c("adm_0_name", "ISO_A0", "Year", "calendar_start_date")
  )

  # Fill data where missing
  data_complete <- data_complete %>%
    group_by(adm_0_name, ISO_A0, Year) %>%
    fill(adm_0_name, ISO_A0, Year, .direction = "downup") %>%
    ungroup()

  # Add global time sequence if not already present
  # data_complete <- left_join(
  #   data_complete,
  #   weekly_calendar %>% dplyr::select(calendar_start_date, time_seq),
  #   by = "calendar_start_date"
  # )

  # Return minimal set unless full columns requested
  if (!keep_vars) {
    data_complete <- data_complete %>%
      dplyr::select(adm_0_name, ISO_A0, Year, week, calendar_start_date, dengue_total, time_seq) %>%
      arrange(adm_0_name, time_seq)
  }

  return(as.data.table(data_complete))
}


make_month_complete_clean <- function(data, keep_vars = FALSE) {
  # Ensure date is in Date format
  data$calendar_start_date <- as.Date(data$calendar_start_date)

  # Extract unique year range from input data
  year_range <- unique(data$Year)

  # Build monthly lookup table (1st of each month)
  monthly_lookup <- lapply(year_range, function(y) {
    tibble(
      Year = y,
      calendar_start_date = seq.Date(as.Date(paste0(y, "-01-01")),
        as.Date(paste0(y, "-12-01")),
        by = "month"
      )
    )
  }) %>%
    bind_rows() %>%
    mutate(month = month(calendar_start_date))

  # Get unique country-year combinations
  unique_country_years <- data %>%
    distinct(adm_0_name, ISO_A0, Year)

  # Cross join with monthly lookup
  monthly_template <- merge(unique_country_years, monthly_lookup, by = "Year", all.y = TRUE)

  # Join with original data
  data_complete <- full_join(
    monthly_template,
    data,
    by = c("adm_0_name", "ISO_A0", "Year", "calendar_start_date")
  )

  # Fill metadata down/up per group
  data_complete <- data_complete %>%
    group_by(adm_0_name, ISO_A0, Year) %>%
    fill(adm_0_name, ISO_A0, Year, .direction = "downup") %>%
    ungroup()

  # Generate time sequence (global)
  time_seq <- data_complete %>%
    distinct(calendar_start_date) %>%
    arrange(calendar_start_date) %>%
    mutate(time_seq = row_number())

  data_complete <- left_join(data_complete, time_seq, by = "calendar_start_date")

  # Final column selection
  if (!keep_vars) {
    data_complete <- data_complete %>%
      select(adm_0_name, ISO_A0, Year, calendar_start_date, dengue_total, time_seq)
  }

  return(as.data.table(data_complete))
}


make_year_complete_clean <- function(data, keep_vars = FALSE) {
  # Create full sequence of years
  year_seq <- tibble(
    Year = seq(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
    time_seq = seq_along(Year)
  )

  # Get unique countries
  unique_countries <- unique(data$adm_0_name)

  # Cross join countries and years
  full_template <- expand.grid(
    adm_0_name = unique_countries,
    Year = year_seq$Year,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    left_join(year_seq, by = "Year")

  # Left join original data to full template to fill missing years per country
  data_complete <- full_template %>%
    left_join(data, by = c("adm_0_name", "Year"))

  # Select columns based on keep_vars
  if (!keep_vars) {
    data_complete <- data_complete %>%
      select(adm_0_name, Year, dengue_total, time_seq)
  }

  return(as.data.table(data_complete))
}



# library(lubridate)
#
# # Function to calculate the total number of epiweeks in a year
# EpiweekCounter <- function(year) {
#   start_date <- as.Date(paste(year, "01-01", sep = "-"))
#   end_date <- as.Date(paste(year + 1, "01-01", sep = "-"))
#
#   # Create a sequence of dates for the entire year
#   dates <- seq(start_date, end_date, by = "day")
#
#   # Calculate epiweeks for each date
#   epiweeks <- sapply(dates, epiweek)
#
#   # Delete week 53 of the previous years
#   idx <- which(epiweeks == 1)[[1]] # find the index of the first week 1
#   # Generate a sequence of indices for the vector
#   all_idx <- seq_len(length(epiweeks))
#
#   # Remove items with indices lower than index_of_first_one
#   epiweeks <- epiweeks[all_idx[idx:length(all_idx)]]
#
#   # Count unique epiweeks
#   unique_epiweeks <- length(unique(epiweeks))
#
#
#   return(unique_epiweeks)
# }
#
# len <- (max_year + 1) - 1923
# nweeks <- vector(mode = "numeric", length = len)
# for (i in 1:len) {
#   nweeks[i] <- EpiweekCounter(i + 1923)
# }
#
# epiw <- cbind(data.frame(Year = 1924:(max_year + 1)), nweeks)
#
#
#
# WeeklyDateSequence <- function(start_year, end_year) {
#   nweeks <- epiw$nweeks[epiw$Year == end_year]
#
#   start_date <- EpiWeek::epiweekToDate(start_year, 1)$d0
#   end_date <- EpiWeek::epiweekToDate(end_year, nweeks)$d1
#
#   all_dates <- data.frame(calendar_start_date = as.character(seq(as.Date(start_date),
#     as.Date(end_date),
#     by = 7
#   )))
#   all_dates$time_seq <- 1:nrow(all_dates)
#   all_dates$week <- epiweek(as.Date(all_dates$calendar_start_date))
#
#   return(all_dates)
# }
#
#
#
# # make time series sequence for weekly data
# make_week_complete <- function(data, keep_vars = FALSE) {
#   dt_subset <- data %>%
#     group_by(adm_0_name, Year) %>%
#     group_split()
#
#   # loop through each country-year and save into a list
#   dt_subset_new <- list()
#
#   for (i in 1:length(dt_subset)) { # loop through each year
#     dat <- dt_subset[[i]]
#     current_year <- unique(dat$Year)
#
#     # make a template weekly data series
#     nweeks <- epiw$nweeks[epiw$Year == current_year] # total number of weeks per year
#
#     start_date <- EpiWeek::epiweekToDate(current_year, 1)$d0
#     end_date <- EpiWeek::epiweekToDate(current_year, nweeks)$d1
#
#     all_dates <- data.frame(calendar_start_date = as.character(seq(as.Date(start_date),
#       as.Date(end_date),
#       by = 7
#     )))
#     # all_dates$time_seq <- 1:nrow(all_dates)
#     all_dates$week <- epiweek(as.Date(all_dates$calendar_start_date))
#
#     # merge with original data and save it into a list
#     dat$week <- epiweek(as.Date(dat$calendar_start_date))
#     dat <- merge(dat, all_dates, by = c("calendar_start_date", "week"), all.y = T)
#
#     # Handle missing values
#     cols_to_fill <- c("adm_0_name", "Year", "country_year", "ISO_A0")
#     for (col in cols_to_fill) {
#       dat[[col]][is.na(dat[[col]])] <- first(na.omit(dat[[col]]))
#     }
#
#     cat(paste("\r inner loop completed", i))
#
#     if (!keep_vars) {
#       dat <- dat %>%
#         select(
#           adm_0_name, ISO_A0,
#           Year, week, calendar_start_date, dengue_total
#         )
#     }
#
#     dt_subset_new[[i]] <- as.data.frame(dat)
#   }
#
#   # merge all country-years
#   data_new <- rbindlist(dt_subset_new)
#
#   time_seq <- WeeklyDateSequence(min(data_new$Year), max(data_new$Year))[, c("calendar_start_date", "time_seq")]
#
#   data_new <- merge(data_new, time_seq, by = "calendar_start_date", all.x = T) %>%
#     select(c(names(dt_subset_new[[1]]), "time_seq"))
#
#   return(data_new)
# }
#
# # Function to make time series sequence for monthly data
# make_month_complete <- function(data, keep_vars = FALSE) {
#   # Group the data by country and year
#   dt_subset <- data %>%
#     group_by(adm_0_name, Year) %>%
#     group_split()
#
#   # Check if any country-year combination has less than 12 months of data
#   num_rows <- sapply(dt_subset, function(df) nrow(df))
#
#   if (any(num_rows != 12)) {
#     dt_subset_new <- list()
#
#     for (i in 1:length(dt_subset)) { # Loop through each country-year combination
#       dat <- dt_subset[[i]]
#       current_year <- unique(dat$Year)
#
#       # If the data does not cover all 12 months, fill in the missing months
#       if (nrow(dat) != 12) {
#         # Generate monthly sequence of dates for the current year
#         all_dates <- data.frame(
#           calendar_start_date = as.character(seq(
#             from = as.Date(make_date(year = current_year, month = 1, day = 1)),
#             to = as.Date(make_date(year = current_year, month = 12, day = 31)),
#             by = "1 month"
#           )),
#           stringsAsFactors = FALSE
#         )
#
#         # Merge the data with the full monthly sequence
#         dat <- merge(dat, all_dates, by = c("calendar_start_date"), all.y = TRUE)
#
#         # Handle missing values by filling with the first non-NA value in the group
#         cols_to_fill <- c("adm_0_name", "Year", "country_year", "ISO_A0")
#         for (col in cols_to_fill) {
#           dat[[col]][is.na(dat[[col]])] <- first(na.omit(dat[[col]]))
#         }
#
#         # Optionally select specific columns
#         if (!keep_vars) {
#           dat <- dat %>%
#             select(adm_0_name, ISO_A0, Year, calendar_start_date, dengue_total)
#         }
#
#         dt_subset_new[[i]] <- as.data.frame(dat)
#       } else {
#         # If the data already covers all 12 months, just keep it
#         if (!keep_vars) {
#           dat <- dat %>%
#             select(adm_0_name, ISO_A0, Year, calendar_start_date, dengue_total)
#         }
#
#         dt_subset_new[[i]] <- as.data.frame(dat)
#       }
#     }
#
#     # Combine the modified country-year data back together
#     data_new <- rbindlist(dt_subset_new)
#   } else {
#     # If all countries have complete monthly sequence, just combine them as is
#     message("All countries have complete monthly sequence")
#     data_new <- rbindlist(dt_subset)
#   }
#
#   # Generate a complete sequence of months for the entire dataset
#   time_seq <- data.frame(
#     calendar_start_date = as.character(seq(
#       from = as.Date(make_date(year = min(data_new$Year, na.rm = TRUE), month = 1, day = 1)),
#       to = as.Date(make_date(year = max(data_new$Year, na.rm = TRUE), month = 12, day = 31)),
#       by = "1 month"
#     )),
#     stringsAsFactors = FALSE
#   )
#   time_seq$time_seq <- 1:nrow(time_seq)
#
#   # Merge the generated time sequence with the dataset
#   data_new <- merge(data_new, time_seq, by = "calendar_start_date", all.x = TRUE) %>%
#     # select(c(names(dt_subset[[1]]), "time_seq"))%>%
#     select(adm_0_name, ISO_A0, Year, calendar_start_date, dengue_total, time_seq)
#
#   return(data_new)
# }
#
#
# # make time series sequence for yearly data
# make_year_complete <- function(data, keep_vars = FALSE) {
#   # Generate yearly sequence
#   year_seq <- data.frame(Year = seq(min(data$Year), max(data$Year), by = 1), stringsAsFactors = FALSE)
#   year_seq$time_seq <- 1:nrow(year_seq) # Add a time sequence index to the yearly sequence
#
#   year_seq <- cbind(
#     year_seq[rep(1:nrow(year_seq),
#       times = length(unique(data$adm_0_name))
#     ), ],
#     adm_0_name = rep(unique(data$adm_0_name),
#       times = nrow(year_seq)
#     )
#   )
#
#   # Now, merge data with year_seq to fill in missing years
#   data_new <- merge(data, year_seq, by = c("adm_0_name", "Year"), all.y = T)
#
#   # Handle missing values
#   cols_to_fill <- c("adm_0_name")
#   for (col in cols_to_fill) {
#     data_new[[col]][is.na(data_new[[col]])] <- first(na.omit(data_new[[col]]))
#   }
#
#   if (!keep_vars) {
#     data_new <- data_new %>%
#       select(adm_0_name, Year, dengue_total)
#   }
#
#   return(data_new)
# }
#
# # # Function to make time series sequence for weekly, monthly, or yearly data
# # make_ts_complete <- function(data, T_res, keep_NA = T){
# #
# #   if (T_res == "Week") {
# #     all_dates = WeeklyDateSequence(min(data$Year), max(data$Year))
# #
# #     data$week <- epiweek(as.Date(data$calendar_start_date))
# #     data <- merge(data, all_dates, by=c("calendar_start_date", "week"), all.y=T)
# #
# #   } else if (T_res == "Month") {
# #     data$calendar_start_date <- as.character(as.Date(paste0(data$Year, "-", data$month, "-01")))
# #
# #     all_dates = data.frame(calendar_start_date = as.character(seq(from = as.Date(make_date(year = min(data$Year), month = 1, day = 1)),
# #                                                                   to = as.Date(make_date(year = max(data$Year), month = 12, day = 1)), by = "1 month")),
# #                            stringsAsFactors = FALSE)
# #
# #     all_dates$time_seq <- 1:nrow(all_dates)
# #
# #     data <- merge(data, all_dates, by=c("calendar_start_date"), all.y=T)
# #     # data$time_seq = 1:nrow(data)
# #
# #
# #   } else {
# #     data$calendar_start_date <- as.character(as.Date(paste0(data$Year, "-01-01")))
# #     all_dates = data.frame(calendar_start_date = as.character(seq(from = as.Date(make_date(year = min(data$Year), month = 1, day = 1)),
# #                                                                   to = as.Date(make_date(year = max(data$Year), month = 1, day = 1)), by = "1 year")),
# #                            stringsAsFactors = FALSE)
# #     all_dates$time_seq <- 1:nrow(all_dates)
# #     data <- merge(data, all_dates, by=c("calendar_start_date"), all.y=T)
# #     # data$time_seq = 1:nrow(data)
# #
# #     }
# #
# #   if (keep_NA) {
# #     data <- data
# #   } else {
# #     data <- data[!is.na(data$dengue_total),]
# #   }
# #   return(data)
# # }
# #
# # # calling make_ts_complete function with visualisation
# # tsComplete <- function(data, T_res, plot = T) {
# #   data_list <- data %>%
# #     group_by(adm_0_name)%>%
# #     group_split()
# #
# #   if (plot) {
# #     # make data complete and visualise
# #     ls_complete <- list()
# #     for (i in 1:length(data_list)){
# #       ls_complete[[i]] <- make_ts_complete(data_list[[i]], T_res) # for visualisation purposes
# #       cat(paste("\rcompleted", i))
# #     }
# #
# #     ts <- lapply(ls_complete, function(data) {
# #       country_name <- unique(data$adm_0_name)[!is.na(unique(data$adm_0_name))]
# #
# #       data %>%
# #         ggplot() +
# #         geom_point(aes(x = as.Date(calendar_start_date), y = dengue_total, group = 1), size=0.7) +
# #         geom_line(aes(x = as.Date(calendar_start_date), y = dengue_total, group = 1), linewidth = 0.3) +
# #         ggtitle(paste0(country_name))
# #     })%>% wrap_plots()+ plot_layout(axis_titles = "collect")
# #
# #     return = ts
# #
# #   } else {
# #
# #     return = make_ts_complete(data, T_res, keep_NA = TRUE)
# #
# #   }
# #
# #   return(return)
# # }
