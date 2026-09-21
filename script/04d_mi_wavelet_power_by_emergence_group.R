# ==============================================================================
# MI WAVELET POWER — trends by when dengue became established, and the
# low-season incidence behind them (supplementary figure)
# ==============================================================================
# The pooled power trend of script 04c mixes two kinds of country: those with
# transmission already established at the start of the series, and those where
# dengue became established during the 1990s. In the second kind the early
# years have no cases and therefore no seasonal cycle (band power near zero),
# so their trend measures the appearance of a cycle. This script separates the
# two.
#
# Groups (countries of the 04c analysis; annual 46, multiannual 45): by the
# first year with more than 20 cases in the released point estimates
# (runs/mi_full/mi50/opendengue_gap_filled_MI.csv) — the threshold that defines
# an active year in the wavelet eligibility rules:
#   established transmission by 1990   more than 20 cases already in 1990
#                                      (left-censored: the true year is 1990 or earlier)
#   first reported cases in 1991-1999  first year with more than 20 cases in 1991-1999
#
# PART 2  per-year trend of band power per group and cycle: the 04c model
#         (log power ~ year * region + (year | country)) fitted to the group's
#         countries in each imputed dataset, all years with band power
#         (annual 1991-2023, multiannual 1992-2022), pooled with Rubin's rules.
#         Global = mean of the regional slopes weighted by number of countries.
# PART 3  figure: band power by calendar year per group (geometric mean over
#         countries within a dataset; line = mean over the 50 datasets, band =
#         2.5-97.5% across datasets), the pooled log-linear fit (dashed) and
#         its slope, 95% CI and p value as text in the group's colour in the
#         lower right of each panel. One panel per cycle, each with its own axes. Drawn for the transform of the
#         full 1990-2024 series (04c cache) and, when the cache of the
#         start-at-emergence computation is present (START_CACHE), for the
#         transform started at each country's first year with more than 20
#         cases.
# PART 4  low-season and peak-season incidence. Wavelet power is computed from
#         log(cases + 1), so it measures the ratio between the high and the low
#         season, not their difference. For every country-year of each imputed
#         dataset (the same draw per MI run as 04c): cases in the lowest and
#         in the highest month. Reported per group: (i) the per-year trend of
#         log(lowest-month cases + 1) and of log(highest-month cases + 1), same
#         model and pooling as PART 2; (ii) by decade, the median lowest- and
#         highest-month incidence per 100,000 and the share of country-years
#         with at least one month with zero cases. (ii) is repeated on reported
#         months only (years with 12 reported months in the run 1 stage table),
#         because most monthly values of the 1990s are distributed from annual
#         totals.
#
# Usage:
#   Rscript script/04d_mi_wavelet_power_by_emergence_group.R
#
# Outputs:
#   output/figures/sfig10.png
#   output/figures/sfig10_start_gt20.png  (if START_CACHE exists)
#   output/tables/sfig10_power_by_emergence_group.csv       slopes behind the figure text
#   output/tables/sfig10_low_season_by_emergence_group.csv  PART 4
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})
source("functions/fn_wavelet_mi.R")

WAVE_DIR    <- "runs/mi_full/wavelet/wavelet_coef"
START_CACHE <- "script/arx/rebuttal/wavelet/mi_full_outputs/wavelet_power_emergence"
RUN_DIR     <- "runs/mi_full/mi50/disaggregation"
RELEASE_CSV <- "runs/mi_full/mi50/opendengue_gap_filled_MI.csv"
STAGE_CSV   <- "runs/mi_full/mi50/disaggregation/run_01_downscale.csv"
FIG_DIR     <- "output/figures"
TAB_DIR     <- "output/tables"
N_SIM       <- 5000
GROUPS      <- c("Established transmission by 1990", "First reported cases in 1991-1999")
GROUP_COLS  <- c("Established transmission by 1990" = "steelblue4", "First reported cases in 1991-1999" = "firebrick")

# ==============================================================================
# PART 1 — groups
# ==============================================================================
release <- read.csv(RELEASE_CSV)
start_gt20 <- release %>%
  group_by(country = adm_0_name, Year) %>%
  summarise(cases = sum(dengue_total_scaled), .groups = "drop") %>%
  group_by(country) %>%
  summarise(start_gt20 = suppressWarnings(min(Year[cases > 20])), .groups = "drop")

per <- lapply(list.files(WAVE_DIR, pattern = "^ds_[0-9]+[.]rds$", full.names = TRUE), readRDS)
per <- per[vapply(per, function(x) is.null(x$error), logical(1))]
M <- length(per)
sets <- list(Annual = per[[1]]$power$Annual$countries, Multiannual = per[[1]]$power$Multiannual$countries)
groups <- start_gt20 %>%
  filter(country %in% sets$Annual) %>%
  mutate(group = factor(ifelse(start_gt20 == 1990, GROUPS[1], GROUPS[2]), levels = GROUPS))
stopifnot(all(is.finite(groups$start_gt20)), all(groups$start_gt20 <= 1999))
message(sprintf("m = %d datasets; groups (annual set): %s", M,
                paste(sprintf("%s = %d", GROUPS, as.integer(table(groups$group))), collapse = ", ")))

# ==============================================================================
# PART 2 — pooled per-year trend per group and cycle
# ==============================================================================
fmt_p <- function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))

group_slopes <- function(yearly_of, variant) {
  bind_rows(lapply(c("Annual", "Multiannual"), function(cyc) {
    bind_rows(lapply(GROUPS, function(g) {
      set <- intersect(groups$country[groups$group == g], sets[[cyc]])
      fits <- lapply(seq_along(yearly_of), function(i) power_fits(yearly_of[[i]], set, per[[i]]$region_map)[[cyc]])
      tb <- pool_power_cycle(fits, cyc, N_SIM)$table %>% filter(Region == "Global")
      data.frame(Variant = variant, Cycle = cyc, Group = g, N = tb$N,
                 Year_From = min(fits[[1]]$consts$min_yr), Year_To = max(fits[[1]]$consts$max_yr),
                 Fit_Start = tb$Power_Start, Fit_End = tb$Power_End,
                 Pct_per_yr = tb$Pct_Change, CI_Lower = tb$CI_Lower, CI_Upper = tb$CI_Upper,
                 P_value = tb$P_value, N_Inc = tb$N_Inc, N_Tot = tb$N_Tot, FMI = tb$FMI)
    }))
  }))
}

yearly_series <- function(yearly_of) {
  bind_rows(lapply(c("Annual", "Multiannual"), function(cyc) {
    col <- if (cyc == "Annual") "ann_power_yearly" else "mlt_power_yearly"
    bind_rows(lapply(seq_along(yearly_of), function(i) {
      yearly_of[[i]] %>%
        filter(country %in% sets[[cyc]], !is.na(.data[[col]])) %>%
        inner_join(groups %>% select(country, group), by = "country") %>%
        group_by(group, year) %>%
        summarise(gm = exp(mean(log(.data[[col]]))), n = dplyr::n(), .groups = "drop") %>%
        mutate(k = i)
    })) %>%
      group_by(group, year) %>%
      summarise(n_countries = first(n), power = mean(gm), lo = quantile(gm, 0.025), hi = quantile(gm, 0.975), .groups = "drop") %>%
      mutate(Cycle = cyc)
  }))
}

# ==============================================================================
# PART 3 — figure
# ==============================================================================
draw_figure <- function(series, slopes, outfile, min_countries = 5) {
  series <- series %>% filter(n_countries >= min_countries)
  panel <- function(cyc, tag) {
    sr <- series %>% filter(Cycle == cyc)
    sl <- slopes %>% filter(Cycle == cyc) %>% mutate(group = factor(Group, levels = GROUPS))
    fit <- bind_rows(lapply(seq_len(nrow(sl)), function(i) {
      data.frame(group = sl$group[i], year = c(sl$Year_From[i], sl$Year_To[i]), power = c(sl$Fit_Start[i], sl$Fit_End[i]))
    }))
    # text in the lower right, inside the range of the data (log scale)
    lo <- log(min(sr$lo)); hi <- log(max(sr$hi))
    lab <- sl %>%
      mutate(label = sprintf("slope: %+.2f%% per year (95%% CI %.2f, %.2f), %s, n = %d", Pct_per_yr, CI_Lower, CI_Upper, fmt_p(P_value), N),
             x = 2023.5, y = exp(lo + c(0.34, 0.26)[as.integer(group)] * (hi - lo)))
    ggplot(sr, aes(year, power, colour = group, fill = group)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.3) +
      geom_line(data = fit, linetype = "dashed", linewidth = 0.8) +
      geom_text(data = lab, aes(x = x, y = y, label = label), hjust = 1, size = 3.1, show.legend = FALSE) +
      scale_y_log10() +
      scale_colour_manual(values = GROUP_COLS) + scale_fill_manual(values = GROUP_COLS) +
      scale_x_continuous(breaks = seq(1990, 2025, 5), limits = c(1990.5, 2023.5)) +
      labs(x = "Year", y = sprintf("%s-band wavelet power (log scale)", cyc), colour = NULL, fill = NULL, tag = tag) +
      theme_classic(base_size = 12)
  }
  g <- panel("Annual", "a") / panel("Multiannual", "b") + plot_layout(guides = "collect") & theme(legend.position = "top")
  ggsave(outfile, g, width = 8.5, height = 9, dpi = 200)
}

set.seed(123)
yearly_full <- lapply(per, `[[`, "yearly")
slopes_full <- group_slopes(yearly_full, "full")
draw_figure(yearly_series(yearly_full), slopes_full, file.path(FIG_DIR, "sfig10.png"))
slopes_all <- slopes_full

start_files <- list.files(START_CACHE, pattern = "^ds_[0-9]+[.]rds$", full.names = TRUE)
if (length(start_files) == M) {
  yearly_start <- lapply(start_files, function(f) readRDS(f)$yearly$start_gt20)
  slopes_start <- group_slopes(yearly_start, "start_gt20")
  draw_figure(yearly_series(yearly_start), slopes_start, file.path(FIG_DIR, "sfig10_start_gt20.png"))
  slopes_all <- bind_rows(slopes_full, slopes_start)
} else {
  message("start-at-emergence cache not found or incomplete; second figure skipped: ", START_CACHE)
}
write.csv(slopes_all, file.path(TAB_DIR, "sfig10_power_by_emergence_group.csv"), row.names = FALSE)

# ==============================================================================
# PART 4 — low-season and peak-season incidence
# ==============================================================================
pop <- release %>% distinct(country = adm_0_name, Year, pop_est)
stopifnot(!anyDuplicated(pop[, c("country", "Year")]))
countries <- groups$country
grid <- expand.grid(country = countries, Year = 1991:2023, month = 1:12, stringsAsFactors = FALSE)
decade_of <- function(y) cut(y, c(1990, 2000, 2010, 2023), labels = c("1991-2000", "2001-2010", "2011-2023"))

run_cache <- list()
season_of_dataset <- function(x) {
  run_file <- file.path(RUN_DIR, sprintf("run_%02d.rds", x$run))
  r <- readRDS(run_file)
  col_in_run <- x$col - (x$run - 1L) * ncol(r$draws)
  stopifnot(col_in_run >= 1, col_in_run <= ncol(r$draws))
  km <- do.call(rbind, strsplit(r$keys, "|", fixed = TRUE))
  cells <- data.frame(country = km[, 1], Year = as.integer(km[, 2]), month = as.integer(km[, 3]), cases = r$draws[, col_in_run])
  grid %>%
    left_join(cells, by = c("country", "Year", "month")) %>%
    mutate(cases = ifelse(is.na(cases), 0, cases)) %>%       # cells absent from the run are deterministic zeros
    group_by(country, year = Year) %>%
    summarise(low = min(cases), high = max(cases), any_zero = any(cases == 0), .groups = "drop")
}
message("PART 4: reading the draw of each dataset from the MI run files")
season <- lapply(per, season_of_dataset)

# (i) per-year trend of log(lowest-month cases + 1) and log(highest-month cases + 1)
season_trend <- bind_rows(lapply(GROUPS, function(g) {
  set <- groups$country[groups$group == g]
  fits <- lapply(seq_along(season), function(i) {
    power_fits(season[[i]] %>% transmute(country, year, ann_power_yearly = low + 1, mlt_power_yearly = high + 1), set, per[[i]]$region_map)
  })
  # power_fits() returns the first column's fit in $Annual and the second's in $Multiannual
  bind_rows(lapply(c("Annual", "Multiannual"), function(slot) {
    tb <- pool_power_cycle(lapply(fits, `[[`, slot), slot, N_SIM)$table %>% filter(Region == "Global")
    data.frame(Group = g, Quantity = ifelse(slot == "Annual", "log(lowest-month cases + 1)", "log(highest-month cases + 1)"),
               N = tb$N, Pct_per_yr = tb$Pct_Change, CI_Lower = tb$CI_Lower, CI_Upper = tb$CI_Upper, P_value = tb$P_value)
  }))
}))

# (ii) by decade: median incidence in the lowest and highest month, share of country-years with a zero month
by_decade <- bind_rows(lapply(seq_along(season), function(i) {
  season[[i]] %>%
    left_join(pop, by = c("country", "year" = "Year")) %>%
    left_join(groups %>% select(country, group), by = "country") %>%
    mutate(decade = decade_of(year), low_i = low / pop_est * 1e5, high_i = high / pop_est * 1e5) %>%
    group_by(group, decade, country) %>%
    summarise(low_i = median(low_i), high_i = median(high_i), zero = mean(any_zero), .groups = "drop") %>%
    group_by(group, decade) %>%
    summarise(low_per100k = median(low_i), high_per100k = median(high_i), pct_years_with_zero_month = 100 * mean(zero), .groups = "drop") %>%
    mutate(k = i)
})) %>%
  group_by(Group = group, Decade = decade) %>%
  summarise(across(c(low_per100k, high_per100k, pct_years_with_zero_month),
                   list(mean = mean, lo = ~ quantile(.x, 0.025), hi = ~ quantile(.x, 0.975))), .groups = "drop")

# the same share on reported months only (years with 12 reported months; identical in every run)
reported <- read.csv(STAGE_CSV) %>%
  filter(adm_0_name %in% countries, Year >= 1991, Year <= 2023) %>%
  group_by(country = adm_0_name, year = Year) %>%
  summarise(n_reported = sum(!is.na(dengue_total)), any_zero = any(dengue_total == 0, na.rm = TRUE),
            low = suppressWarnings(min(dengue_total, na.rm = TRUE)), .groups = "drop") %>%
  filter(n_reported == 12) %>%
  left_join(groups %>% select(country, group), by = "country") %>%
  mutate(decade = decade_of(year)) %>%
  group_by(Group = group, Decade = decade) %>%
  summarise(n_countries = n_distinct(country), n_country_years = dplyr::n(),
            pct_years_with_zero_month_reported = 100 * mean(any_zero), .groups = "drop")

low_season <- by_decade %>% left_join(reported, by = c("Group", "Decade"))
write.csv(bind_rows(
  season_trend %>% mutate(Table = "trend", .before = 1),
  low_season %>% mutate(Table = "by_decade", .before = 1) %>% mutate(Group = as.character(Group), Decade = as.character(Decade))
), file.path(TAB_DIR, "sfig10_low_season_by_emergence_group.csv"), row.names = FALSE)

# ==============================================================================
# print
# ==============================================================================
cat("\n=== GROUPS ===\n")
for (g in GROUPS) cat(sprintf("%s (%d): %s\n", g, sum(groups$group == g),
                              paste(tools::toTitleCase(tolower(sort(groups$country[groups$group == g]))), collapse = ", ")))

cat(sprintf("\n=== BAND POWER, per-year trend per group (MI pooled, m = %d; %%/yr, Rubin 95%% CI) ===\n", M))
slopes_all %>%
  transmute(Variant, Cycle, Group, N, Years = sprintf("%d-%d", Year_From, Year_To),
            Pct_per_yr = sprintf("%6.2f (%6.2f, %6.2f)", Pct_per_yr, CI_Lower, CI_Upper), p = fmt_p(P_value),
            Increasing = sprintf("%d/%d", N_Inc, N_Tot), FMI = round(FMI, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== CASES IN THE LOWEST AND HIGHEST MONTH OF THE YEAR, per-year trend 1991-2023 (log(cases + 1) scale; %/yr) ===\n")
season_trend %>%
  transmute(Group, Quantity, N, Pct_per_yr = sprintf("%6.2f (%6.2f, %6.2f)", Pct_per_yr, CI_Lower, CI_Upper), p = fmt_p(P_value)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== BY DECADE (median over countries; mean and 2.5-97.5% over datasets) ===\n")
low_season %>%
  transmute(Group, Decade,
            Lowest_month_per100k = sprintf("%.2f (%.2f, %.2f)", low_per100k_mean, low_per100k_lo, low_per100k_hi),
            Highest_month_per100k = sprintf("%.1f (%.1f, %.1f)", high_per100k_mean, high_per100k_lo, high_per100k_hi),
            Pct_years_with_a_zero_month = sprintf("%.0f (%.0f, %.0f)", pct_years_with_zero_month_mean, pct_years_with_zero_month_lo, pct_years_with_zero_month_hi),
            Reported_only = sprintf("%.0f%% of %d country-years (%d countries)", pct_years_with_zero_month_reported, n_country_years, n_countries)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nwrote:", file.path(FIG_DIR, "sfig10{,_start_gt20}.png"),
    "\n      ", file.path(TAB_DIR, "sfig10_{power,low_season}_by_emergence_group.csv"), "\n")
