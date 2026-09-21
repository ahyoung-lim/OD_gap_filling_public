# ------------------------------------------------------------------------------
# Rolling-origin cross-validation mask (with optional boundary-only pruning)
#
# WHAT IT PRODUCES
#   A data.frame equal to your input but with one extra column:
#     - fold : integer or NA. When not NA, the row is part of the TEST set
#              for that fold. (Training rows are chosen later by the split helper.)
#
# EXTRAPOLATION SEMANTICS
#   direction = "future"  → forecast/forward: training is restricted to rows
#                           strictly BEFORE the earliest test time (per country, per fold).
#   direction = "past"    → backcast/backward: training is restricted to rows
#                           strictly AFTER the latest test time (per country, per fold).
#
# HOW IT WORKS (high level)
#   1) Build *raw* rolling windows by sampling "cutoffs" inside each country’s series.
#      - Each cutoff yields a contiguous test window of length 'gap_len' on the test side.
#      - 'min_interior' ensures enough training rows remain on the training side.
#      - 'stride' enforces spacing between multiple cutoffs for the same country & fold.
#      - We keep sampling until each fold reaches roughly 'test_frac_target' of total rows.
#   2) Optionally prune to boundary-only scoring:
#      - boundary_mode = "k"           → keep exactly 'k_eval' boundary-adjacent rows
#                                        (per country, per fold), drop the rest.
#      - boundary_mode = "target_frac" → grow boundary-adjacent blocks round-robin
#                                        across countries until the fold totals ≈ 'target_frac'.
#      - boundary_mode = "none"        → keep all raw windows.
#
# NOTES
#   - With boundary_mode="k", the *final* test size is driven by k_eval and how many
#     countries got at least one cutoff; 'test_frac_target' mainly controls raw candidates.
#   - Set stride = 1e9 to enforce at most one cutoff per country per fold.
#   - The function stores attributes (column names, direction) used by the split helper.
#
# DEPENDENCIES: dplyr, rlang
# ------------------------------------------------------------------------------

# --------------------------------------------------------------------
# Rolling-origin CV mask with optional boundary-only pruning (FIXED)
# - Adds a 'fold' column: rows with fold == f are TEST for that fold.
# - Use cv_split_extrapolation_rolling(mask, f) to get leak-proof train/test.
#   (Training is trimmed per-country at the boundary.)
#
# boundary_mode:
#   "none"         -> keep all raw rolling windows
#   "k"            -> keep exactly k_eval boundary-adjacent rows per country×fold
#   "target_frac"  -> grow boundary-adjacent blocks round-robin to ~ target_frac
#
# Dependencies: dplyr, rlang
# --------------------------------------------------------------------
build_extrapolation_mask_rolling <- function(
    data,
    country_col = "adm_0_name",
    time_col = "time_seq",
    n_folds = 5,
    gap_len = 2, # size of each raw test window per cutoff
    min_interior = 12, # training buffer on the train side
    direction = c("future", "past"),
    test_frac_target = 0.10, # target when creating *raw* windows
    stride = 8, # min spacing between cutoffs within a country per fold
    seed = 123,
    boundary_mode = c("none", "k", "target_frac"),
    k_eval = NULL, # used if boundary_mode == "k"
    target_frac = NULL # used if boundary_mode == "target_frac"
    ) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  direction <- match.arg(direction)
  boundary_mode <- match.arg(boundary_mode)
  set.seed(seed)

  csym <- rlang::ensym(country_col)
  tsym <- rlang::ensym(time_col)

  df <- data |>
    dplyr::rename(country = !!csym, t = !!tsym) |>
    dplyr::arrange(country, t) |>
    dplyr::mutate(
      row_id = dplyr::row_number(),
      country = as.character(country),
      fold = NA_integer_
    )

  N <- nrow(df)
  target_per_fold <- as.integer(round((test_frac_target * N) / n_folds))

  by_cty <- df |>
    dplyr::group_by(country) |>
    dplyr::summarise(rows = list(row_id), times = list(t), n = dplyr::n(), .groups = "drop")

  fold_counts <- integer(n_folds)
  chosen_cutoffs <- vector("list", n_folds)
  names(chosen_cutoffs) <- as.character(seq_len(n_folds))
  for (f in seq_len(n_folds)) chosen_cutoffs[[f]] <- list()

  add_cut <- function(cty_row, cutoff_pos, f) {
    ids <- cty_row$rows[[1]]
    n <- length(ids)
    if (is.na(cutoff_pos) || cutoff_pos < 1L || cutoff_pos > n) {
      return(FALSE)
    }

    if (direction == "future") {
      start_idx <- cutoff_pos + 1L
      end_idx <- min(n, cutoff_pos + gap_len)
    } else {
      end_idx <- cutoff_pos
      start_idx <- max(1L, cutoff_pos - gap_len + 1L)
    }
    if (start_idx > end_idx) {
      return(FALSE)
    }

    test_rows <- ids[start_idx:end_idx]
    if (!length(test_rows)) {
      return(FALSE)
    }

    df$fold[test_rows] <<- f
    fold_counts[f] <<- fold_counts[f] + length(test_rows)
    TRUE
  }

  # ---------- Stage 1: make RAW rolling windows ----------
  f <- 1L
  progressed <- TRUE
  while (any(fold_counts < target_per_fold) && progressed) {
    progressed <- FALSE
    for (i in seq_len(nrow(by_cty))) {
      if (all(fold_counts >= target_per_fold)) break
      cty_row <- by_cty[i, ]
      n <- cty_row$n[[1]]
      if (n < (min_interior + gap_len + 1L)) next

      cand <- if (direction == "future") {
        seq.int(min_interior, n - gap_len)
      } else {
        seq.int(gap_len, n - min_interior)
      }
      cand <- cand[cand >= 1L & cand <= n]
      if (!length(cand)) next

      cty_key <- as.character(cty_row$country[[1]])
      allowed <- chosen_cutoffs[[f]][[cty_key]] %||% integer(0)

      if (length(allowed)) {
        keep <- vapply(cand, function(x) all(abs(x - allowed) >= stride), logical(1))
        cand <- cand[keep]
      }
      if (!length(cand)) next

      cut_pos <- sample(cand, 1L)
      if (add_cut(cty_row, cut_pos, f)) {
        chosen_cutoffs[[f]][[cty_key]] <- c(allowed, cut_pos)

        progressed <- TRUE
        f <- if (f == n_folds) 1L else f + 1L
      }
    }
  }

  out <- df |>
    dplyr::select(-row_id) |>
    dplyr::rename(!!csym := country, !!tsym := t)

  # ---------- Stage 2: boundary-only pruning (FIXED) ----------
  if (boundary_mode != "none") {
    ccol <- rlang::as_string(csym)
    tcol <- rlang::as_string(tsym)

    prune_fold_boundary <- function(fold_id, mode, k_eval, target_frac) {
      fold_rows <- which(out$fold == fold_id)
      if (!length(fold_rows)) {
        return(invisible(NULL))
      }

      tt <- out[fold_rows, , drop = FALSE]
      split_cty <- split(tt, tt[[ccol]])

      # boundary time per country in this fold
      boundaries <- lapply(split_cty, function(s) {
        ts <- s[[tcol]]
        if (!length(ts)) {
          return(NULL)
        }
        if (direction == "future") min(ts) else max(ts)
      })

      # candidate indices on the boundary side, ordered from boundary inward
      cand <- lapply(names(boundaries), function(cty) {
        b <- boundaries[[cty]]
        if (is.null(b)) {
          return(integer(0))
        }
        sel <- out[[ccol]] == cty
        idx <- which(sel)
        tvec <- out[[tcol]][sel]
        if (direction == "future") {
          idx2 <- idx[tvec >= b]
          idx2[order(tvec[tvec >= b])]
        } else {
          idx2 <- idx[tvec <= b]
          idx2[order(-tvec[tvec <= b])]
        }
      })
      names(cand) <- names(boundaries)

      if (mode == "k") {
        if (is.null(k_eval)) stop("Provide k_eval when boundary_mode='k'.")
        keep <- integer(0)
        for (cty in names(cand)) {
          rows <- cand[[cty]]
          if (!length(rows)) next
          keep <- c(keep, utils::head(rows, k_eval)) # boundary, boundary-1, ...
        }
        # FIX: reset fold then set exactly the boundary rows
        out$fold[out$fold == fold_id] <<- NA_integer_
        out$fold[keep] <<- fold_id
        return(invisible(NULL))
      }

      if (mode == "target_frac") {
        if (is.null(target_frac)) stop("Provide target_frac when boundary_mode='target_frac'.")
        target_rows <- as.integer(round(target_frac * N / n_folds))
        keep <- integer(0)
        L_by_cty <- setNames(integer(length(cand)), names(cand))

        repeat {
          progressed_local <- FALSE
          for (cty in names(cand)) {
            if (length(keep) >= target_rows) break
            rows <- cand[[cty]]
            if (!length(rows)) next
            L_by_cty[cty] <- L_by_cty[cty] + 1L
            pick <- utils::head(rows, L_by_cty[cty])
            new <- setdiff(pick, keep)
            if (length(new)) {
              keep <- c(keep, new)
              progressed_local <- TRUE
            }
          }
          if (length(keep) >= target_rows || !progressed_local) break
        }
        # FIX: reset fold then set exactly the boundary-grown rows
        out$fold[out$fold == fold_id] <<- NA_integer_
        out$fold[keep] <<- fold_id
        return(invisible(NULL))
      }
    }

    for (f in seq_len(n_folds)) prune_fold_boundary(f, boundary_mode, k_eval, target_frac)
  }

  # attrs for helpers (so they never guess)
  attr(out, "cv_kind") <- "rolling"
  attr(out, "direction") <- direction
  attr(out, "min_interior") <- min_interior
  attr(out, "country_col") <- rlang::as_string(csym)
  attr(out, "time_col") <- rlang::as_string(tsym)
  out
}


# Visualize ONLY train + test rows for one fold
# - inclusive policy by default
# - optionally limit to specific country/countries
# plot_fold_train_test <- function(
#     data, mask, fold_id,
#     country_col = "adm_0_name",
#     time_col = "time_seq",
#     countries = NULL, # e.g. "AFGHANISTAN" or c("AFGHANISTAN","ANGOLA")
#     only_countries_with_test = FALSE # TRUE = show only countries that have test rows in this fold
#     ) {
#   # leak-proof split (inclusive)
#   sp <- cv_split_extrapolation_rolling(mask, fold_id = fold_id, train_policy = "inclusive")
#
#   # keep ONLY rows used for this fold
#   df <- data |>
#     arrange(adm_0_name, time_seq) %>%
#     dplyr::mutate(.row = dplyr::row_number()) |>
#     dplyr::filter(.row %in% c(sp$train_idx, sp$test_idx)) |>
#     dplyr::mutate(cat = dplyr::if_else(.row %in% sp$test_idx, "test", "train"))
#
#   # optional: limit to selected countries
#   if (!is.null(countries)) {
#     df <- dplyr::filter(df, .data[[country_col]] %in% countries)
#   }
#
#   # optional: show only countries that actually have test rows for this fold
#   if (isTRUE(only_countries_with_test)) {
#     tested <- unique(mask[[country_col]][mask$fold == fold_id])
#     df <- dplyr::filter(df, .data[[country_col]] %in% tested)
#   }
#
#   ggplot2::ggplot(df) +
#     ggplot2::geom_tile(
#       ggplot2::aes(
#         x = .data[[time_col]],
#         y = .data[[country_col]],
#         fill = cat
#       ),
#       color = "white", linewidth = 0.1
#     ) +
#     ggplot2::scale_fill_manual(values = c(train = "#A6CEE3", test = "#1F78B4")) +
#     ggplot2::labs(
#       title = paste("Fold", fold_id, "— train vs test (inclusive)"),
#       x = time_col, y = country_col, fill = NULL
#     ) +
#     ggplot2::theme_minimal(base_size = 11) +
#     ggplot2::theme(legend.position = "right")
# }

# # Per FOLD — train:test ratio
# fold_train_test_ratio <- function(mask, data, train_policy = "inclusive", digits = 0) {
#   n_folds <- max(mask$fold, na.rm = TRUE)
#   purrr::map_dfr(seq_len(n_folds), function(f) {
#     sp <- cv_split_extrapolation_rolling(mask, fold_id = f, train_policy = train_policy)
#     n_train <- length(sp$train_idx)
#     n_test <- length(sp$test_idx)
#     used <- n_train + n_test
#     train_pct <- if (used > 0) 100 * n_train / used else NA_real_
#     test_pct <- if (used > 0) 100 - train_pct else NA_real_
#     tibble::tibble(
#       fold = f,
#       n_train = n_train,
#       n_test = n_test,
#       train_pct = round(train_pct, digits),
#       test_pct = round(test_pct, digits),
#       ratio = if (is.na(train_pct)) {
#         NA_character_
#       } else {
#         sprintf("%.*f:%.*f", digits, round(train_pct, digits), digits, round(test_pct, digits))
#       }
#     )
#   })
# }
#
# # Per FOLD × COUNTRY — train:test ratio like "80:20"
# fold_train_test_ratio_by_country <- function(
#     mask, data, fold_id,
#     country_col = "adm_0_name",
#     train_policy = "inclusive",
#     digits = 0,
#     drop_unused = TRUE # drop countries with zero train & test for this fold
#     ) {
#   sp <- cv_split_extrapolation_rolling(mask, fold_id = fold_id, train_policy = train_policy)
#   df <- dplyr::mutate(data, .row = dplyr::row_number())
#
#   tr <- df[sp$train_idx, ] |> dplyr::count(.data[[country_col]], name = "n_train")
#   te <- df[sp$test_idx, ] |> dplyr::count(.data[[country_col]], name = "n_test")
#
#   out <- dplyr::full_join(tr, te, by = country_col) |>
#     dplyr::mutate(
#       fold = fold_id,
#       n_train = tidyr::replace_na(n_train, 0L),
#       n_test = tidyr::replace_na(n_test, 0L),
#       used = n_train + n_test,
#       train_pct = dplyr::if_else(used > 0, 100 * n_train / used, NA_real_),
#       test_pct = dplyr::if_else(used > 0, 100 - train_pct, NA_real_),
#       ratio = dplyr::if_else(
#         is.na(train_pct),
#         NA_character_,
#         sprintf("%.*f:%.*f", digits, round(train_pct, digits), digits, round(test_pct, digits))
#       )
#     )
#   if (drop_unused) out <- dplyr::filter(out, used > 0)
#   dplyr::arrange(out, dplyr::desc(n_test), dplyr::desc(n_train))
# }
#
#
# # Minimal usage example
# # # Build mask (weekly, backcasting). Set boundary_mode="k" for length-matched comparability.
# data_m <- read.csv("data/model_input/model_data_monthly.csv")
# mask <- build_extrapolation_mask_rolling(
#   data_m,
#   country_col = "adm_0_name",
#   time_col = "time_seq",
#   n_folds = 5,
#   gap_len = 5,
#   min_interior = 3,
#   direction = "past", # backcast
#   test_frac_target = 0.50, # create enough raw candidates
#   stride = 4,
#   boundary_mode = "k", # keep only boundary-adjacent rows
#   k_eval = 5, # score exactly 2 weeks at the boundary
#   seed = 123
# )
#
# # Leak-proof split for fold 1
# sp <- cv_split_extrapolation_rolling(mask, fold_id = 2, train_policy = "inclusive") # or "exclusive"
# train <- data_w[sp$train_idx, ] # model trains on these
# test <- data_w[sp$test_idx, ] # evaluate on these (boundary-only, 2 weeks)
#
# # Sanity check (TRUE means extrapolation is enforced for all countries)
# chk <- assert_extrapolation_rolling(mask, fold_id = 2)
# chk$all_ok
# bad <- dplyr::filter(chk$details, ok == FALSE | is.na(ok))
# bad %>% dplyr::slice_head(n = 10)
#
#
# table(mask$fold, useNA = "ifany")

#
#
# # Whole panel, all countries that have either train or test in this fold
# plot_fold_train_test(data_m, mask, fold_id = 3)
#
# # Zoom to just Afghanistan
# plot_fold_train_test(data_w, mask, fold_id = 1, countries = "GUYANA")

#
#
# fold_train_test_ratio(mask, data_m, digits = 0)
# fold_train_test_ratio_by_country(mask, data_m, fold_id = 2, digits = 0) |> head()
