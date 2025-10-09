# Enhanced function with modified coverage-based selection logic
select_best_record_multi_new <- function(df, discrepancy_threshold = 0.2, epiweek_tbl = NULL) {
  # Countries that should always prioritize sub-annual data
  always_subannual_countries <- c("HONG KONG", "JAPAN", "TAIWAN")

  # Initialize dataframes for selected and unselected records
  selected_records <- data.frame()
  unselected_records <- data.frame()

  # Initialize dataframe to track annual vs sub-annual relationships
  relationship_tracking <- data.frame()

  # Helper function to check if subannual data is complete
  check_subannual_complete <- function(t_res, count, year = NA, epiweek_lookup = NULL) {
    if (t_res == "Week") {
      # Use actual week count from lookup table if available
      if (!is.null(epiweek_lookup) && !is.na(year)) {
        expected_weeks <- epiweek_lookup$epiweeks[epiweek_lookup$Year == year]
        if (length(expected_weeks) > 0) {
          return(count >= expected_weeks)
        }
      }
      # Fallback to 52 if lookup not available
      return(count >= 52)
    }
    if (t_res == "Month" && count == 12) {
      return(TRUE)
    }
    return(FALSE)
  }

  # Process each country_year group
  country_years <- unique(df$country_year)

  for (cy in country_years) {
    # Get all records for this country_year
    cy_records <- df %>% filter(country_year == cy)

    # Extract country name and year from country_year
    country_name <- str_extract(cy, "^[^_]+")
    year <- as.numeric(str_extract(cy, "\\d{4}$"))

    # Get expected week count for this year
    expected_weeks <- 52 # Default
    if (!is.null(epiweek_tbl) && !is.na(year)) {
      weeks_lookup <- epiweek_tbl$epiweeks[epiweek_tbl$Year == year]
      if (length(weeks_lookup) > 0) {
        expected_weeks <- weeks_lookup[1]
      }
    }

    # Always create the extended columns for consistency
    cy_records <- cy_records %>%
      mutate(
        sum_details = pmap(
          list(a0_sum, a1_sum, a2_sum, a0_count, a1_count, a2_count),
          function(...) {
            sums <- c(..1, ..2, ..3)
            counts <- c(..4, ..5, ..6)
            cols <- c("a0", "a1", "a2")
            valid <- !is.na(sums)
            if (any(valid)) {
              data.frame(
                column = cols[valid],
                sum = sums[valid],
                count = counts[valid]
              )
            } else {
              data.frame(column = character(), sum = numeric(), count = numeric())
            }
          }
        ),
        max_count = pmax(a0_count, a1_count, a2_count, na.rm = TRUE),
        max_sum = pmap_dbl(
          list(a0_sum, a1_sum, a2_sum),
          ~ {
            vals <- c(..1, ..2, ..3)
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) max(vals) else NA_real_
          }
        )
      )

    # CASE 1: If only one record, select it
    if (nrow(cy_records) == 1) {
      record <- cy_records
      details <- record$sum_details[[1]]
      if (nrow(details) > 0) {
        record$selected_column <- details$column[which.max(details$sum)]
      } else {
        record$selected_column <- NA
      }
      # Add NA relationship columns for consistency
      record$relationship_category <- NA_character_
      record$pct_of_annual <- NA_real_
      record$annual_total <- record$max_sum

      # Check completeness using the selected column's count, not max_count
      if (record$T_res != "Year" && !is.na(record$selected_column)) {
        record_details <- record$sum_details[[1]]
        selected_col_count <- record_details$count[record_details$column == record$selected_column]
        record$subannual_is_complete <- check_subannual_complete(record$T_res, selected_col_count, year, epiweek_tbl)
      } else {
        record$subannual_is_complete <- NA
      }

      selected_records <- bind_rows(selected_records, record %>% select(-sum_details))
      next
    }

    # Separate annual and sub-annual records
    annual_records <- cy_records %>% filter(T_res == "Year")
    subannual_records <- cy_records %>% filter(T_res != "Year")

    # CASE 1: Only annual records (keep as is)
    if (nrow(subannual_records) == 0) {
      best_idx <- which.max(annual_records$max_sum)
      selected <- annual_records[best_idx, ]
      details <- selected$sum_details[[1]]
      selected$selected_column <- details$column[which.max(details$sum)]
      selected$relationship_category <- NA_character_
      selected$pct_of_annual <- NA_real_
      selected$annual_total <- selected$max_sum
      selected$subannual_is_complete <- NA

      selected_records <- bind_rows(selected_records, selected %>% select(-sum_details))

      if (nrow(annual_records) > 1) {
        unselected_records <- bind_rows(
          unselected_records,
          annual_records[-best_idx, ] %>%
            select(-sum_details) %>%
            mutate(
              reason_unselected = "Multiple annual records - lower sum",
              relationship_category = NA_character_
            )
        )
      }
      next
    }

    # CASE 2: Only sub-annual records (assess data coverage)
    if (nrow(annual_records) == 0) {
      # Add completeness proportion for tiebreaker
      subannual_records <- subannual_records %>%
        mutate(
          expected_count = case_when(
            T_res == "Week" ~ expected_weeks, # Use year-specific week count
            T_res == "Month" ~ 12,
            TRUE ~ NA_real_
          ),
          completeness_proportion = max_count / expected_count
        )

      # Select best sub-annual record
      if (country_name %in% always_subannual_countries) {
        # For special countries: prioritize count, then sum, then proportion
        selected <- subannual_records %>%
          arrange(desc(max_count), desc(max_sum), desc(completeness_proportion)) %>%
          slice(1)

        details <- selected$sum_details[[1]]
        details <- details %>% arrange(desc(count), desc(sum))
        selected$selected_column <- details$column[1]
      } else {
        # For normal countries: prioritize sum, then proportion
        selected <- subannual_records %>%
          arrange(desc(max_sum), desc(completeness_proportion)) %>%
          slice(1)

        details <- selected$sum_details[[1]]
        selected$selected_column <- details$column[which.max(details$sum)]
      }

      # Check data coverage
      # Get the count for the selected spatial column
      selected_details <- selected$sum_details[[1]]
      selected_column_count <- selected_details$count[selected_details$column == selected$selected_column]

      selected$subannual_is_complete <- check_subannual_complete(selected$T_res, selected_column_count, year, epiweek_tbl)
      selected$relationship_category <- if_else(
        selected$subannual_is_complete,
        "Complete sub-annual only",
        "Incomplete sub-annual only"
      )
      selected$pct_of_annual <- NA_real_
      selected$annual_total <- selected$max_sum

      selected_records <- bind_rows(selected_records, selected %>% select(-sum_details))

      if (nrow(subannual_records) > 1) {
        unselected_records <- bind_rows(
          unselected_records,
          subannual_records %>%
            filter(row_number() != which(subannual_records$max_count == selected$max_count &
              subannual_records$max_sum == selected$max_sum)[1]) %>%
            select(-sum_details) %>%
            mutate(
              reason_unselected = "Other sub-annual record",
              relationship_category = NA_character_
            )
        )
      }
      next
    }

    # CASE 3: Both annual and sub-annual records exist
    # Step 1: Select the best annual record (highest sum)
    best_annual_idx <- which.max(annual_records$max_sum)
    best_annual <- annual_records[best_annual_idx, ]
    annual_details <- best_annual$sum_details[[1]]
    best_annual_column <- annual_details$column[which.max(annual_details$sum)]
    best_annual_sum <- max(annual_details$sum)

    # Step 2: Select the best sub-annual record
    # Add completeness proportion for tiebreaker
    subannual_records <- subannual_records %>%
      mutate(
        expected_count = case_when(
          T_res == "Week" ~ expected_weeks, # Use year-specific week count
          T_res == "Month" ~ 12,
          TRUE ~ NA_real_
        ),
        completeness_proportion = max_count / expected_count
      )

    if (country_name %in% always_subannual_countries) {
      # For special countries: prioritize count, then sum, then proportion
      best_subannual <- subannual_records %>%
        arrange(desc(max_count), desc(max_sum), desc(completeness_proportion)) %>%
        slice(1)
    } else {
      # For normal countries: prioritize sum, then proportion
      best_subannual <- subannual_records %>%
        arrange(desc(max_sum), desc(completeness_proportion)) %>%
        slice(1)
    }

    subannual_details <- best_subannual$sum_details[[1]]
    best_subannual_column <- subannual_details$column[which.max(subannual_details$sum)]
    best_subannual_sum <- max(subannual_details$sum)

    # Get the count for the selected spatial column (not max_count!)
    best_subannual_count <- subannual_details$count[subannual_details$column == best_subannual_column]

    # Step 3: Check if subannual is complete
    # Consider incomplete if: (1) count is insufficient OR (2) sum is 0 when annual is non-zero (only for non-special countries)
    subannual_count_complete <- check_subannual_complete(best_subannual$T_res, best_subannual_count, year, epiweek_tbl)

    # For special countries, only check count completeness
    # For non-special countries, also check if data is non-zero when annual is non-zero
    if (country_name %in% always_subannual_countries) {
      subannual_is_complete <- subannual_count_complete
      subannual_has_data <- TRUE # Not relevant for special countries
    } else {
      subannual_has_data <- !(best_subannual_sum == 0 && best_annual_sum != 0)
      subannual_is_complete <- subannual_count_complete && subannual_has_data
    }

    # Step 4: Make selection based on coverage and comparison
    pct_of_annual <- if_else(best_annual_sum > 0,
      (best_subannual_sum / best_annual_sum) * 100,
      NA_real_
    )

    if (!subannual_is_complete) {
      # Subannual is incomplete -> compare annual vs sum of subannual
      if (best_annual_sum > best_subannual_sum) {
        # Annual > incomplete subannual -> select annual
        selected <- best_annual
        selected$selected_column <- best_annual_column

        # Determine reason for incompleteness
        if (!subannual_count_complete) {
          selected$relationship_category <- "Incomplete sub-annual - annual selected"
          unselected_reason <- paste0(
            "Sub-annual incomplete (", best_subannual$max_count,
            " ", tolower(best_subannual$T_res), "s) and annual (",
            round(best_annual_sum), ") > sub-annual (",
            round(best_subannual_sum), ")"
          )
        } else if (!subannual_has_data) {
          selected$relationship_category <- "Sub-annual sum zero - annual selected"
          unselected_reason <- paste0(
            "Sub-annual complete but sum is zero (annual = ",
            round(best_annual_sum), ")"
          )
        } else {
          selected$relationship_category <- "Incomplete sub-annual - annual selected"
          unselected_reason <- "Sub-annual incomplete and annual > sub-annual"
        }

        selected$annual_total <- best_annual_sum
      } else {
        # Annual <= incomplete subannual -> select subannual
        selected <- best_subannual
        selected$selected_column <- best_subannual_column
        selected$relationship_category <- "Incomplete sub-annual - sub-annual selected"

        # Use sub-annual sum since it's what we're selecting
        selected$annual_total <- best_subannual_sum

        unselected_reason <- paste0(
          "Sub-annual incomplete (", best_subannual$max_count,
          " ", tolower(best_subannual$T_res), "s) but sub-annual (",
          round(best_subannual_sum), ") >= annual (",
          round(best_annual_sum), ")"
        )
      }
    } else {
      # Subannual is complete and > 0 -> compare sums
      if (best_annual_sum > best_subannual_sum && !(country_name %in% always_subannual_countries)) {
        # Annual > subannual AND not special country -> select annual
        selected <- best_annual
        selected$selected_column <- best_annual_column
        selected$relationship_category <- "Annual > sub-annual"
        selected$annual_total <- best_annual_sum
        unselected_reason <- paste0(
          "Annual (", round(best_annual_sum),
          ") > complete sub-annual (", round(best_subannual_sum), ")"
        )
      } else {
        # Either: annual <= subannual OR (annual > subannual but special country) -> select subannual
        selected <- best_subannual
        selected$selected_column <- best_subannual_column
        if (country_name %in% always_subannual_countries) {
          selected$relationship_category <- "Special country - sub-annual selected"
        } else if (best_subannual_sum >= best_annual_sum) {
          selected$relationship_category <- "Sub-annual >= Annual"
        } else {
          selected$relationship_category <- "Sub-annual < Annual (special case)"
        }

        # Original annual_total behavior: use annual benchmark for scaling if 80-99%, otherwise use subannual sum
        selected$annual_total <- if_else(
          pct_of_annual >= 80 & pct_of_annual < 100,
          best_annual_sum,
          best_subannual_sum
        )

        unselected_reason <- paste0(
          "Complete sub-annual selected (",
          round(pct_of_annual, 1), "% of annual)"
        )
      }
    }

    selected$pct_of_annual <- pct_of_annual
    selected$subannual_is_complete <- subannual_is_complete

    # Track this relationship
    relationship_tracking <- bind_rows(relationship_tracking, data.frame(
      country_year = cy,
      country = country_name,
      annual_sum = best_annual_sum,
      annual_column = best_annual_column,
      subannual_sum = best_subannual_sum,
      subannual_column = best_subannual_column,
      subannual_type = best_subannual$T_res,
      subannual_count = best_subannual_count, # Use the count for selected column, not max_count
      subannual_is_complete = subannual_is_complete,
      pct_of_annual = pct_of_annual,
      relationship_category = selected$relationship_category,
      selected_type = selected$T_res,
      is_special_country = country_name %in% always_subannual_countries,
      stringsAsFactors = FALSE
    ))

    selected_records <- bind_rows(selected_records, selected %>% select(-sum_details))

    # Track unselected records
    if (identical(selected$T_res, "Year")) {
      unselected <- bind_rows(
        annual_records[-best_annual_idx, ] %>%
          select(-sum_details) %>%
          mutate(
            reason_unselected = "Other annual record",
            relationship_category = NA_character_
          ),
        subannual_records %>%
          select(-sum_details) %>%
          mutate(
            reason_unselected = unselected_reason,
            relationship_category = selected$relationship_category
          )
      )
    } else {
      unselected <- bind_rows(
        annual_records %>%
          select(-sum_details) %>%
          mutate(
            reason_unselected = unselected_reason,
            relationship_category = selected$relationship_category
          ),
        subannual_records %>%
          filter(row_number() != which(subannual_records$max_count == best_subannual$max_count &
            subannual_records$max_sum == best_subannual$max_sum)[1]) %>%
          select(-sum_details) %>%
          mutate(
            reason_unselected = "Other sub-annual record",
            relationship_category = NA_character_
          )
      )
    }

    unselected_records <- bind_rows(unselected_records, unselected)
  }

  # Clean up temporary columns
  cols_to_remove <- c("max_count", "max_sum", "expected_count", "completeness_proportion")
  if (any(cols_to_remove %in% names(selected_records))) {
    selected_records <- selected_records %>%
      select(-any_of(cols_to_remove))
  }
  if (any(cols_to_remove %in% names(unselected_records))) {
    unselected_records <- unselected_records %>%
      select(-any_of(cols_to_remove))
  }

  return(list(
    selected = selected_records,
    unselected = unselected_records,
    relationships = relationship_tracking
  ))
}

# Optional: Create a cleaned dataset with only the selected column data
create_clean_dataset <- function(selected_df) {
  selected_df %>%
    mutate(
      final_count = case_when(
        selected_column == "a0" ~ a0_count,
        selected_column == "a1" ~ a1_count,
        selected_column == "a2" ~ a2_count,
        TRUE ~ NA_real_
      ),
      final_sum = case_when(
        selected_column == "a0" ~ a0_sum,
        selected_column == "a1" ~ a1_sum,
        selected_column == "a2" ~ a2_sum,
        TRUE ~ NA_real_
      )
    ) %>%
    select(country_year, T_res, selected_column, final_count, final_sum, everything())
}
