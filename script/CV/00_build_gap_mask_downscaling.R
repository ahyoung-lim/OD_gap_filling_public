# ─────────────────────────────────────────────────────────────────────────────
# Year-wise INTERPOLATION (Jan–Dec blocks), NO ADJACENT TEST YEARS
# ─────────────────────────────────────────────────────────────────────────────
build_gap_mask_yearwise_interpolation <- function(
    data,
    country_col = "adm_0_name",
    year_col = "Year",
    month_col = "month",
    time_col = "time_seq",
    n_folds = 3,
    edge_years = 1, # keep this many full years at each edge for training
    test_frac_target = 0.10, # ~90:10 per fold overall
    skip_countries = NULL,
    min_gap_years = 1, # forbid adjacent test years (≥1 year gap)
    seed = 123) {
  set.seed(seed)
  csym <- rlang::ensym(country_col)
  ysym <- rlang::ensym(year_col)
  msym <- rlang::ensym(month_col)
  tsym <- rlang::ensym(time_col)

  df <- data %>%
    dplyr::rename(country = !!csym, year = !!ysym, month = !!msym, t = !!tsym) %>%
    dplyr::mutate(
      country = as.character(country),
      year = as.integer(as.character(year)),
      month = as.integer(as.character(month))
    ) %>%
    dplyr::arrange(country, year, month) %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      fold = NA_integer_
    )

  if (!is.null(skip_countries)) df <- dplyr::filter(df, !country %in% skip_countries)

  # Full calendar years only
  full_years <- df %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(nm = dplyr::n_distinct(month), .groups = "drop") %>%
    dplyr::filter(nm == 12L) %>%
    dplyr::select(-nm)

  # Interior years per country (exclude edge buffer)
  bounds <- full_years %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(ymin = min(year), ymax = max(year), .groups = "drop")

  interior <- full_years %>%
    dplyr::inner_join(bounds, by = "country") %>%
    dplyr::filter(year >= (ymin + edge_years), year <= (ymax - edge_years)) %>%
    dplyr::select(country, year)

  # Candidate blocks (each block is 12 monthly rows for a country-year)
  blocks <- df %>%
    dplyr::semi_join(interior, by = c("country", "year")) %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(rows = list(row_id), n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n >= 12L) %>% # safety
    dplyr::select(-n)

  if (nrow(blocks) == 0L) {
    warning("No eligible interior full years were found.")
    out <- df %>%
      dplyr::select(-row_id) %>%
      dplyr::rename(!!csym := country, !!ysym := year, !!msym := month, !!tsym := t) %>%
      dplyr::arrange(!!csym, !!tsym) %>%
      dplyr::mutate(
        !!ysym := factor(!!ysym, levels = sort(unique(!!ysym))) # restore as ordered factor
      )

    attr(out, "cv_kind") <- "interpolation"
    attr(out, "country_col") <- rlang::as_string(csym)
    attr(out, "time_col") <- rlang::as_string(tsym)
    attr(out, "min_gap_years") <- min_gap_years
    return(out)
  }

  # Balancing toward ~10% per fold
  N <- nrow(df)
  target_per_fold <- as.integer(round((test_frac_target * N) / n_folds))
  fold_counts <- integer(n_folds)

  # Track used test years per country to enforce spacing
  used_years <- rlang::env()
  assigned <- rep(FALSE, nrow(blocks))

  progressed <- TRUE
  while (any(fold_counts < target_per_fold) && progressed) {
    progressed <- FALSE

    for (i in sample.int(nrow(blocks))) {
      if (assigned[i]) next
      cty <- blocks$country[i]
      yr <- blocks$year[i]
      uy <- rlang::env_get(used_years, cty, default = integer(0))

      # Enforce no-adjacent rule
      if (length(uy) && any(abs(yr - uy) <= min_gap_years)) next

      # Pick a fold by deficit
      deficit <- max(fold_counts) - fold_counts
      weights <- pmax(deficit + 1, 0.1)
      f <- sample.int(n_folds, 1L, prob = weights)

      rows <- blocks$rows[[i]]
      if (any(!is.na(df$fold[rows]))) next

      df$fold[rows] <- f
      fold_counts[f] <- fold_counts[f] + length(rows)
      assigned[i] <- TRUE
      rlang::env_bind(used_years, !!!setNames(list(c(uy, yr)), cty))
      progressed <- TRUE

      if (all(fold_counts >= target_per_fold)) break
    }
  }

  out <- df %>%
    dplyr::select(-row_id) %>%
    dplyr::rename(!!csym := country, !!ysym := year, !!msym := month, !!tsym := t) %>%
    dplyr::arrange(!!csym, !!tsym)

  out <- out %>%
    dplyr::mutate(!!ysym := factor(!!ysym, levels = sort(unique(!!ysym))))

  attr(out, "cv_kind") <- "interpolation"
  attr(out, "country_col") <- rlang::as_string(csym)
  attr(out, "time_col") <- rlang::as_string(tsym)
  attr(out, "min_gap_years") <- min_gap_years
  out
}


# ─────────────────────────────────────────────────────────────────────────────
# Year-wise EXTRAPOLATION: take boundary full years (Jan–Dec) as tests.
# Rolling across folds from the edge inward; keeps your "rolling" attrs.
# ─────────────────────────────────────────────────────────────────────────────
build_extrapolation_mask_yearwise <- function(
    data,
    country_col = "adm_0_name",
    year_col = "Year",
    month_col = "month",
    time_col = "time_seq",
    n_folds = 3,
    direction = c("future", "past"), # future = right edge; past = left edge
    min_interior_years = 1, # keep at least this many full years on the train side
    test_frac_target = 0.10,
    skip_countries = NULL,
    seed = 123) {
  set.seed(seed)
  direction <- match.arg(direction)
  csym <- rlang::ensym(country_col)
  ysym <- rlang::ensym(year_col)
  msym <- rlang::ensym(month_col)
  tsym <- rlang::ensym(time_col)

  df <- data %>%
    dplyr::rename(country = !!csym, year = !!ysym, month = !!msym, t = !!tsym) %>%
    dplyr::mutate(
      country = as.character(country),
      year = as.integer(as.character(year)),
      month = as.integer(as.character(month))
    ) %>%
    dplyr::arrange(country, year, month) %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      fold = NA_integer_
    )

  if (!is.null(skip_countries)) df <- dplyr::filter(df, !country %in% skip_countries)

  # Full calendar years per country
  full_years <- df %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(nm = dplyr::n_distinct(month), .groups = "drop") %>%
    dplyr::filter(nm == 12) %>%
    dplyr::mutate(year = as.integer(year)) # <- enforce integer here (belt + braces)

  # Build ordered boundary-year lists per country (edge → interior)
  year_lists <- full_years %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      yrs = list(sort(unique(as.integer(year)))), # <- enforce integer in the list
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      usable = purrr::map_int(yrs, ~ max(length(.x) - min_interior_years, 0L)),
      boundary = purrr::map2(yrs, usable, function(v, u) {
        v <- as.integer(v) # <- enforce integer again
        if (u <= 0) {
          integer(0)
        } else {
          as.integer(if (identical(direction, "future")) rev(tail(v, u)) else head(v, u))
        }
      })
    ) %>%
    dplyr::select(country, boundary)

  cand <- tidyr::unnest(year_lists, boundary, keep_empty = TRUE) %>%
    dplyr::mutate(boundary = as.integer(boundary))



  # Map each (country, boundary_year) to its 12 monthly row_ids
  block_map <- df %>%
    dplyr::inner_join(cand, by = c("country", "year" = "boundary")) %>%
    dplyr::group_by(country, year) %>%
    dplyr::summarise(rows = list(row_id), n = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(country, if (direction == "future") dplyr::desc(year) else year)

  if (nrow(block_map) == 0L) {
    warning("No eligible boundary full years were found.")
    out <- df %>%
      dplyr::select(-row_id) %>%
      dplyr::rename(!!csym := country, !!ysym := year, !!msym := month, !!tsym := t) %>%
      dplyr::arrange(!!csym, !!tsym)
    attr(out, "cv_kind") <- "rolling"
    attr(out, "direction") <- direction
    attr(out, "min_interior") <- min_interior_years * 12L
    attr(out, "country_col") <- rlang::as_string(csym)
    attr(out, "time_col") <- rlang::as_string(tsym)
    return(out)
  }

  # Split blocks per country; set pointers (edge → inward)
  blocks_by_cty <- split(block_map, block_map$country)
  ptr <- setNames(integer(length(blocks_by_cty)), names(blocks_by_cty))

  # Aim for ~10% per fold
  N <- nrow(df)
  target_per_fold <- as.integer(round((test_frac_target * N) / n_folds))
  fold_counts <- integer(n_folds)

  for (f in seq_len(n_folds)) {
    progressed <- TRUE
    while (fold_counts[f] < target_per_fold && progressed) {
      progressed <- FALSE
      for (cty in sample(names(blocks_by_cty))) {
        blks <- blocks_by_cty[[cty]]
        k <- nrow(blks)
        if (ptr[cty] >= k) next
        rows <- blks$rows[[ptr[cty] + 1L]]
        if (any(!is.na(df$fold[rows]))) {
          ptr[cty] <- ptr[cty] + 1L
          next
        }
        df$fold[rows] <- f
        fold_counts[f] <- fold_counts[f] + length(rows)
        ptr[cty] <- ptr[cty] + 1L
        progressed <- TRUE
        if (fold_counts[f] >= target_per_fold) break
      }
    }
  }


  out <- df %>%
    dplyr::select(-row_id) %>%
    dplyr::rename(!!csym := country, !!ysym := year, !!msym := month, !!tsym := t) %>%
    dplyr::arrange(!!csym, !!tsym)

  out <- out %>%
    dplyr::mutate(!!ysym := factor(!!ysym, levels = sort(unique(!!ysym))))

  # Attributes: your cv_split_extrapolation_rolling() will pick these up
  attr(out, "cv_kind") <- "rolling"
  attr(out, "direction") <- direction
  attr(out, "min_interior") <- min_interior_years * 12L # months
  attr(out, "country_col") <- rlang::as_string(csym)
  attr(out, "time_col") <- rlang::as_string(tsym)
  out
}
