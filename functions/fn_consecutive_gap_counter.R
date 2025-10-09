# consec_counter <- function(data, new_data = FALSE) {
#   if (new_data) {
#     # for histogram of lengths of consecutive gaps
#     consec <- data %>%
#       group_by(adm_0_name) %>%
#       reframe(
#         na_lengths = rle(is.na(dengue_total))$lengths,
#         na_values = rle(is.na(dengue_total))$values
#       )
#
#     # merge rle results back to original data
#     consec_full <- data %>%
#       reframe(
#         na_lengths = rle(is.na(dengue_total))$lengths,
#         na_values = rle(is.na(dengue_total))$values
#       )
#
#     consec_full <- as.data.frame(lapply(consec_full, rep, consec_full$na_lengths))
#     data_new <- cbind(data, consec_full)
#
#     list <- list(
#       consec = consec,
#       data = data_new
#     )
#   } else {
#     consec <- data %>%
#       group_by(adm_0_name) %>%
#       reframe(
#         na_lengths = rle(is.na(dengue_total))$lengths,
#         na_values = rle(is.na(dengue_total))$values
#       )
#
#     list <- consec
#   }
#
#   return(list)
# }

consec_counter_clean <- function(data) {
  data %>%
    arrange(adm_0_name, calendar_start_date) %>%
    group_by(adm_0_name) %>%
    group_modify(~ {
      rle_out <- rle(is.na(.x$dengue_total))
      tibble(
        na_lengths = rle_out$lengths,
        na_values = rle_out$values
      )
    }) %>%
    ungroup() %>%
    filter(na_values == TRUE)
}

calc_na_lengths_tidy <- function(data) {
  data %>%
    arrange(Year) %>%
    summarise(
      na_lengths = list(rle(is.na(dengue_total))$lengths),
      na_values = list(rle(is.na(dengue_total))$values)
    ) %>%
    tidyr::unnest(cols = c(na_lengths, na_values))
}


consec_counter_detailed <- function(df) {
  df %>%
    arrange(adm_0_name, calendar_start_date) %>%
    group_by(adm_0_name) %>%
    group_modify(~ {
      rle_na <- rle(is.na(.x$dengue_total))
      # Expand rle result to match row count
      expanded <- tibble(
        na_lengths = rep(rle_na$lengths, rle_na$lengths),
        na_values = rep(rle_na$values, rle_na$lengths),
        run_id = rep(seq_along(rle_na$lengths), rle_na$lengths)
      )
      # Only keep gap_id if it's a NA run
      expanded <- expanded %>%
        mutate(
          gap_id = ifelse(na_values, paste0(.y$adm_0_name, "_", .y$Year, "_", run_id), NA)
        )
      bind_cols(.x, expanded)
    }) %>%
    ungroup()
}
