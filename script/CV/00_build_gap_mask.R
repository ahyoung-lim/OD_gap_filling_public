library(dplyr)
library(purrr)


build_gap_mask_balanced <- function(data,
                                    country_col = "adm_0_name",
                                    time_col = "time_seq",
                                    n_folds = 5,
                                    gap_len = 2,
                                    edge = 2,
                                    skip_countries = NULL,
                                    seed = 123,
                                    balance_iterations = 3) {
  set.seed(seed)

  # ---- 0. Setup (same as original) ----
  df <- data %>%
    rename(country = {{ country_col }}, t = {{ time_col }}) %>%
    arrange(country, t) %>%
    mutate(
      row_id = row_number(),
      country = as.character(country),
      fold = NA_integer_
    )

  real_width <- 2 * gap_len - 1

  # ---- 1. Enhanced planning ----
  plan <- df %>%
    group_by(country) %>%
    summarise(
      series_len = n(),
      n_int = series_len - 2 * edge,
      .groups = "drop"
    ) %>%
    filter(n_int >= gap_len) %>%
    mutate(
      n_gaps_possible = floor(n_int / real_width),
      # Force more countries into more folds
      m_c = pmin(n_folds, pmax(2, round(n_gaps_possible * 0.8))), # More aggressive
      g_c = pmax(1L, floor(n_gaps_possible / m_c))
    )

  if (!is.null(skip_countries)) {
    plan <- filter(plan, !country %in% skip_countries)
  }

  # ---- 2. Candidate starts ----
  starts_pool <- df %>%
    filter(country %in% plan$country) %>%
    arrange(country, t) %>%
    group_by(country) %>%
    summarise(
      starts = list(row_id[(edge + 1):(n() - edge - gap_len + 1)]),
      .groups = "drop"
    ) %>%
    {
      setNames(dplyr::pull(., starts), as.character(dplyr::pull(., country)))
    }


  # ---- 3. Multi-pass placement with active balancing ----
  fold_counter <- integer(n_folds)

  for (iteration in 1:balance_iterations) {
    for (row in sample(nrow(plan))) {
      cty <- plan$country[row]
      gpf <- plan$g_c[row]
      m_folds <- plan$m_c[row]

      # Dynamic fold selection based on current imbalance
      fold_deficit <- max(fold_counter) - fold_counter
      fold_weights <- pmax(fold_deficit + 1, 0.1) # Never zero weight

      # Select folds with probability proportional to their deficit
      targ_folds <- sample(1:n_folds,
        size = min(m_folds, n_folds),
        prob = fold_weights,
        replace = FALSE
      )

      # Get available starts (not already used)
      used_positions <- df %>%
        filter(!is.na(fold), country == cty) %>%
        pull(row_id)

      cty_key <- as.character(cty)
      available_starts <- starts_pool[[cty_key]] %||% integer(0)
      if (length(used_positions) > 0) {
        # Remove positions that would overlap with existing gaps
        for (used_pos in used_positions) {
          available_starts <- available_starts[
            abs(available_starts - used_pos) >= real_width
          ]
        }
      }

      pool <- sample(available_starts)
      if (!length(pool)) next

      # Select non-overlapping starts
      sel <- integer()
      for (s in pool) {
        if (all(abs(s - sel) >= real_width)) sel <- c(sel, s)
      }
      placed <- length(sel)
      if (placed == 0) next

      # Distribute across target folds more evenly
      gaps_to_place <- min(placed, length(targ_folds) * gpf)
      if (gaps_to_place > 0) {
        sel <- sel[1:gaps_to_place]

        # Round-robin assignment to target folds
        fold_vec <- rep(targ_folds, length.out = gaps_to_place)

        for (j in seq_along(sel)) {
          rows <- sel[j] + 0:(gap_len - 1L)
          f <- fold_vec[j]
          df$fold[rows] <- f
          fold_counter[f] <- fold_counter[f] + 1
        }
      }
    }
  }

  # ---- 4. Final rebalancing pass ----
  final_counts <- table(df$fold, useNA = "ifany")
  final_counts <- final_counts[!is.na(names(final_counts))]

  if (length(final_counts) > 0 && max(final_counts) - min(final_counts) > mean(final_counts) * 0.3) {
    # Identify moveable gaps (complete blocks only)
    moveable_gaps <- df %>%
      filter(!is.na(fold)) %>%
      group_by(country, fold) %>%
      arrange(row_id) %>%
      mutate(
        block_id = cumsum(c(TRUE, diff(row_id) != 1)),
        block_size = n()
      ) %>%
      filter(block_size >= gap_len) %>%
      group_by(country, fold, block_id) %>%
      slice_head(n = gap_len) %>%
      ungroup()

    # Move some complete gaps from overfull to underfull folds
    overfull_folds <- as.integer(names(final_counts)[final_counts == max(final_counts)])
    underfull_folds <- as.integer(names(final_counts)[final_counts == min(final_counts)])

    if (length(overfull_folds) > 0 && length(underfull_folds) > 0) {
      candidates <- moveable_gaps %>%
        filter(fold %in% overfull_folds) %>%
        group_by(country, fold, block_id) %>%
        slice_head(n = 1) %>% # One representative per block
        ungroup()

      moves_needed <- min(
        nrow(candidates),
        ceiling((max(final_counts) - min(final_counts)) / 2)
      )

      for (i in 1:moves_needed) {
        if (i > nrow(candidates)) break

        move_country <- candidates$country[i]
        move_block <- candidates$block_id[i]
        target_fold <- sample(underfull_folds, 1)

        # Move the entire block
        move_rows <- moveable_gaps %>%
          filter(country == move_country, block_id == move_block) %>%
          pull(row_id)

        df$fold[df$row_id %in% move_rows] <- target_fold
      }
    }
  }

  # ---- 5. Cleanup  ----
  if (gap_len > 1) {
    df <- df %>%
      group_by(country, fold) %>%
      arrange(row_id, .by_group = TRUE) %>%
      mutate(block_id = cumsum(c(TRUE, diff(row_id) != 1))) %>%
      ungroup()

    short_blocks <- df %>%
      filter(!is.na(fold)) %>%
      count(country, fold, block_id) %>%
      filter(n < gap_len)

    if (nrow(short_blocks)) {
      # df <- anti_join(df, short_blocks, by = c("country", "fold", "block_id"))
      df <- df %>%
        mutate(fold = ifelse(row_id %in%
          inner_join(df, short_blocks,
            by = c("country", "fold", "block_id")
          )$row_id,
        NA_integer_, fold
        ))
    }

    df <- select(df, -block_id)
  }

  df %>%
    select(-row_id) %>%
    rename(
      {{ country_col }} := country,
      {{ time_col }} := t
    ) %>%
    arrange(adm_0_name, time_seq)
}




# Works for BOTH interpolation masks and rolling extrapolation masks
get_fold_split <- function(mask, fold_id, train_policy = c("inclusive", "exclusive")) {
  train_policy <- match.arg(train_policy)
  kind <- attr(mask, "cv_kind", exact = TRUE)

  if (identical(kind, "rolling")) {
    # Use the leak-proof rolling helper you already have
    return(cv_split_extrapolation_rolling(mask, fold_id = fold_id, train_policy = train_policy))
  }

  # Default (non-rolling) behavior: interpolation-style masks
  test_idx <- which(mask$fold == fold_id)
  if (train_policy == "inclusive") {
    train_idx <- which(is.na(mask$fold) | mask$fold != fold_id)
  } else { # exclusive: never train on rows that are test in ANY fold
    train_idx <- which(is.na(mask$fold))
  }
  list(train_idx = train_idx, test_idx = test_idx)
}

# Per-fold: counts + nice "train:test" ratio (relative to used rows)
fold_train_test_ratio <- function(mask, data, train_policy = "inclusive", digits = 0) {
  n_folds <- max(mask$fold, na.rm = TRUE)
  purrr::map_dfr(seq_len(n_folds), function(f) {
    sp <- get_fold_split(mask, fold_id = f, train_policy = train_policy)
    n_train <- length(sp$train_idx)
    n_test <- length(sp$test_idx)
    used <- n_train + n_test
    train_pct <- if (used > 0) 100 * n_train / used else NA_real_
    test_pct <- if (used > 0) 100 - train_pct else NA_real_
    tibble::tibble(
      fold = f,
      n_train = n_train,
      n_test = n_test,
      train_pct = round(train_pct, digits),
      test_pct = round(test_pct, digits),
      ratio = if (is.na(train_pct)) {
        NA_character_
      } else {
        sprintf("%.*f:%.*f", digits, round(train_pct, digits), digits, round(test_pct, digits))
      }
    )
  })
}

# Optional: per fold × country (also works for both mask types)
fold_train_test_ratio_by_country <- function(
    mask, data, fold_id,
    country_col = "adm_0_name",
    train_policy = "inclusive",
    digits = 0,
    drop_unused = TRUE) {
  sp <- get_fold_split(mask, fold_id = fold_id, train_policy = train_policy)
  df <- dplyr::mutate(data, .row = dplyr::row_number())
  tr <- df[sp$train_idx, ] |> dplyr::count(.data[[country_col]], name = "n_train")
  te <- df[sp$test_idx, ] |> dplyr::count(.data[[country_col]], name = "n_test")
  out <- dplyr::full_join(tr, te, by = country_col) |>
    dplyr::mutate(
      fold = fold_id,
      n_train = tidyr::replace_na(n_train, 0L),
      n_test = tidyr::replace_na(n_test, 0L),
      used = n_train + n_test,
      train_pct = dplyr::if_else(used > 0, 100 * n_train / used, NA_real_),
      test_pct = dplyr::if_else(used > 0, 100 - train_pct, NA_real_),
      ratio = dplyr::if_else(
        is.na(train_pct),
        NA_character_,
        sprintf("%.*f:%.*f", digits, round(train_pct, digits), digits, round(test_pct, digits))
      )
    )
  if (drop_unused) out <- dplyr::filter(out, used > 0)
  dplyr::arrange(out, dplyr::desc(n_test), dplyr::desc(n_train))
}

# Plot ONLY train + test rows for one fold (works for any mask)
# - Detects rolling vs non-rolling masks automatically
# - Inclusive train policy by default
# - Optional: filter to specific countries and/or reorder by boundary time
plot_fold_train_test <- function(
    data, mask, fold_id,
    country_col = "adm_0_name",
    time_col = "time_seq",
    countries = NULL, # e.g., "AFGHANISTAN" or c("AFGHANISTAN","ANGOLA")
    only_countries_with_test = FALSE, # TRUE = show only countries that have test rows in this fold
    train_policy = c("inclusive", "exclusive"),
    reorder_countries = TRUE # order by first test time in this fold (then by min time)
    ) {
  train_policy <- match.arg(train_policy)

  # --------- split helper that works for both mask types ----------
  get_split <- function(mask, fold_id, train_policy) {
    kind <- attr(mask, "cv_kind", exact = TRUE)
    if (identical(kind, "rolling")) {
      # requires cv_split_extrapolation_rolling() to be in scope
      return(cv_split_extrapolation_rolling(mask, fold_id = fold_id, train_policy = train_policy))
    } else {
      test_idx <- which(mask$fold == fold_id)
      if (train_policy == "inclusive") {
        train_idx <- which(is.na(mask$fold) | mask$fold != fold_id)
      } else {
        train_idx <- which(is.na(mask$fold))
      }
      return(list(train_idx = train_idx, test_idx = test_idx))
    }
  }

  sp <- get_split(mask, fold_id, train_policy)

  # keep ONLY rows used in this fold
  df_used <- data |>
    dplyr::arrange(adm_0_name, time_seq) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::filter(.row %in% c(sp$train_idx, sp$test_idx)) %>%
    dplyr::mutate(cat = dplyr::if_else(.row %in% sp$test_idx, "test", "train"))

  # optional country filtering
  if (!is.null(countries)) {
    df_used <- dplyr::filter(df_used, .data[[country_col]] %in% countries)
  }
  if (isTRUE(only_countries_with_test)) {
    tested <- unique(mask[[country_col]][mask$fold == fold_id])
    df_used <- dplyr::filter(df_used, .data[[country_col]] %in% tested)
  }

  # handle empty plot gracefully
  if (nrow(df_used) == 0L) {
    message(sprintf("Fold %s: no train/test rows to plot after filtering.", fold_id))
    return(ggplot2::ggplot())
  }

  # optional: reorder countries (first by earliest test in this fold, then by min time)
  if (reorder_countries) {
    ord <- df_used |>
      dplyr::group_by(.data[[country_col]]) |>
      dplyr::summarise(
        t_test_min = suppressWarnings(min(.data[[time_col]][cat == "test"], na.rm = TRUE)),
        t_min = min(.data[[time_col]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        # if no test for this country in this fold, fall back to its min time
        key = dplyr::if_else(is.finite(t_test_min), t_test_min, t_min)
      ) |>
      dplyr::arrange(key) |>
      dplyr::pull(.data[[country_col]])
    df_used[[country_col]] <- factor(df_used[[country_col]], levels = rev(ord))
  }

  df_used %>%
    arrange(adm_0_name, time_seq) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(x = .data[[time_col]], y = .data[[country_col]], fill = cat),
      color = "white", linewidth = 0.1
    ) +
    ggplot2::scale_fill_manual(values = c(train = "#A6CEE3", test = "#1F78B4")) +
    ggplot2::labs(
      title = paste0("Fold ", fold_id, " — train vs test (", train_policy, ")"),
      x = time_col, y = country_col, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "right")
}



# --------------------------------------------------------------------
# Leak-proof train/test split for a given fold (INCLUSIVE by default)
#
# direction = "future": train strictly BEFORE earliest test time (per country)
# direction = "past"  : train strictly AFTER  latest  test time (per country)
#
# train_policy:
#   "inclusive" -> rows that are test in other folds CAN be used for training
#                  in this fold, as long as they’re on the allowed side.
#   "exclusive" -> NEVER use rows that are test in ANY fold for training.
#
# Requires: mask built by build_extrapolation_mask_rolling(...)
# --------------------------------------------------------------------
cv_split_extrapolation_rolling <- function(mask, fold_id,
                                           train_policy = c("inclusive", "exclusive")) {
  train_policy <- match.arg(train_policy)

  kind <- attr(mask, "cv_kind", exact = TRUE)
  if (!identical(kind, "rolling")) {
    stop("Mask 'cv_kind' must be 'rolling'.")
  }
  dir <- attr(mask, "direction", exact = TRUE)
  ccol <- attr(mask, "country_col", exact = TRUE)
  tcol <- attr(mask, "time_col", exact = TRUE)
  if (any(vapply(list(dir, ccol, tcol), function(x) is.null(x) || !length(x), TRUE))) {
    stop("Mask missing required attributes (direction/country_col/time_col).")
  }

  n <- nrow(mask)
  train_keep <- rep(TRUE, n)

  # TEST rows for this fold
  mask <- mask %>% arrange(adm_0_name, time_seq)
  test_idx <- which(mask$fold == fold_id)

  if (length(test_idx)) {
    # For each country that has any test in this fold, compute the cutoff
    tt <- mask[test_idx, , drop = FALSE]
    split_keys <- split(seq_len(nrow(tt)), tt[[ccol]])
    for (k in names(split_keys)) {
      rows <- split_keys[[k]]
      times <- tt[[tcol]][rows]
      sel <- mask[[ccol]] == k

      if (identical(dir, "future")) {
        cut <- min(times) # forecast: train strictly before earliest test
        train_keep[sel] <- train_keep[sel] & (mask[[tcol]][sel] < cut)
      } else {
        cut <- max(times) # backcast: train strictly after latest test
        train_keep[sel] <- train_keep[sel] & (mask[[tcol]][sel] > cut)
      }
    }
  }

  if (train_policy == "inclusive") {
    train_idx <- which(train_keep & (is.na(mask$fold) | mask$fold != fold_id))
  } else { # exclusive
    train_idx <- which(train_keep & is.na(mask$fold))
  }

  list(train_idx = train_idx, test_idx = test_idx)
}



# ------------------------------------------------------------------------------
# (Optional) Sanity check: verify extrapolation per country for a fold
# Returns TRUE if all countries satisfy the extrapolation inequality.
# ------------------------------------------------------------------------------

assert_extrapolation_rolling <- function(mask, fold_id) {
  dir <- attr(mask, "direction", exact = TRUE)
  ccol <- attr(mask, "country_col", exact = TRUE)
  tcol <- attr(mask, "time_col", exact = TRUE)

  sp <- cv_split_extrapolation_rolling(mask, fold_id) # inclusive by default
  df <- mask
  df$.is_train <- FALSE
  df$.is_train[sp$train_idx] <- TRUE
  df$.is_test <- FALSE
  df$.is_test[sp$test_idx] <- TRUE

  det <- df |>
    dplyr::group_by(.data[[ccol]]) |>
    dplyr::summarise(
      has_test = any(.is_test),
      has_train = any(.is_train),
      train_min = if (has_train) min(.data[[tcol]][.is_train]) else NA_integer_,
      train_max = if (has_train) max(.data[[tcol]][.is_train]) else NA_integer_,
      test_min = if (has_test) min(.data[[tcol]][.is_test]) else NA_integer_,
      test_max = if (has_test) max(.data[[tcol]][.is_test]) else NA_integer_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ok = dplyr::case_when(
        !has_test ~ NA, # not evaluated in this fold
        dir == "future" ~ train_max < test_min,
        dir == "past" ~ train_min > test_max,
        TRUE ~ NA
      )
    )

  list(all_ok = all(det$ok | is.na(det$ok)), details = det)
}


library(digest)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Add rich metadata to a mask (kept in attributes)
stamp_mask <- function(mask, data, name,
                       model = c("dm", "inla", "gam", "pilot"), # NEW
                       country_col = "adm_0_name", time_col = "time_seq",
                       params = list(), seed = NA_integer_) {
  model <- tolower(model[1])
  if (!model %in% c("dm", "inla", "gam", "pilot")) {
    warning(sprintf("Unknown model '%s'; using 'pilot'.", model))
    model <- "pilot"
  }

  # compact signature of the evaluation frame only (country + time)
  sig_df <- data[, c(country_col, time_col)]
  data_hash <- digest(sig_df, algo = "xxhash64")

  attr(mask, "mask_name") <- name
  attr(mask, "model") <- model # NEW
  attr(mask, "data_id") <- params$data_id %||% "monthly_full"
  attr(mask, "data_hash") <- data_hash
  attr(mask, "country_col") <- country_col
  attr(mask, "time_col") <- time_col
  attr(mask, "n_folds") <- max(mask$fold, na.rm = TRUE)
  attr(mask, "created_utc") <- format(Sys.time(), tz = "UTC")
  attr(mask, "seed") <- seed

  # store any builder params you care about
  for (nm in names(params)) attr(mask, nm) <- params[[nm]]
  mask
}

# Generate a stable filename from attributes
mask_filename <- function(mask, prefix = "mask") {
  nm <- attr(mask, "mask_name") %||% "unnamed"
  mdl <- attr(mask, "model") %||% "generic" # NEW
  kind <- attr(mask, "cv_kind") %||% "balanced"
  dirn <- attr(mask, "direction") %||% NA
  nf <- attr(mask, "n_folds") %||% NA
  gl <- attr(mask, "gap_len") %||% NA
  ke <- attr(mask, "k_eval") %||% NA
  tf <- attr(mask, "target_frac") %||% NA
  st <- attr(mask, "stride") %||% NA
  ed <- attr(mask, "edge") %||% NA
  dh <- substr(attr(mask, "data_hash") %||% "nohash", 1, 8)
  sd <- attr(mask, "seed") %||% NA

  parts <- c(
    prefix, nm, mdl, kind, # ← include model up-front
    if (!is.na(dirn)) paste0("dir", dirn) else NULL,
    paste0("nf", nf),
    if (!is.na(gl)) paste0("k", gl) else NULL,
    if (!is.na(ke)) paste0("ke", ke) else NULL,
    if (!is.na(tf)) paste0("tf", sprintf("%.2f", tf)) else NULL,
    if (!is.na(st)) paste0("st", st) else NULL,
    if (!is.na(ed)) paste0("edge", ed) else NULL,
    paste0("seed", sd),
    paste0("hash", dh)
  )
  paste0(paste(parts, collapse = "_"), ".rds")
}

save_mask <- function(mask, dir = "runs/CV/masks", filename = NULL) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  if (is.null(filename)) filename <- mask_filename(mask)
  path <- file.path(dir, filename)
  saveRDS(mask, path, compress = "xz")
  path
}


# Quick summary check
summarise_mask <- function(mask, country_col = attr(mask, "country_col")) {
  dplyr::tibble(
    n_rows   = nrow(mask),
    n_cty    = dplyr::n_distinct(mask[[country_col]]),
    n_folds  = max(mask$fold, na.rm = TRUE),
    n_test   = sum(!is.na(mask$fold)),
    test_pct = round(100 * mean(!is.na(mask$fold)), 2)
  )
}


get_attr <- function(x, nm, default = NA) {
  v <- attr(x, nm, exact = TRUE)
  if (is.null(v)) default else v
}
