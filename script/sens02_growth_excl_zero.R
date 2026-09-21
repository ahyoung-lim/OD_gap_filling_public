# ==============================================================================
# SENSITIVITY sens02 (supplementary figure) — per-country dengue growth from first emergence to 2024
# ==============================================================================
# Growth rate of each country's series restricted to its post-emergence period:
# from the first year with reported cases (> 0) through 2024, using the same
# population-adjusted quasi-Poisson regression as the main fig3b, so comparison
# with fig3b isolates the effect of the leading zero years.
#
# DOTS: per-country estimates pooled with Rubin's rules across the 50 imputed
# datasets (country_growth_pooled.csv). BLACK LINES: the median of the plotted
# estimates per region (a visual summary of the dots). The MI-pooled regional
# medians (growth_pooled.csv) are read ONLY for the console cross-check where
# available — they are not plotted and not manuscript-cited numbers, and their
# interval covers the between-imputation spread only (percentile of the 50
# per-dataset medians), not a full 95% CI.
#
# Two inclusion thresholds are produced:
#   sfig_sens02_growth_excl_zero_MI.png       countries with >= 3 years with cases — the supplementary
#                      figure; includes recently emerging countries, whose
#                      estimates rest on very few points and are imprecise
#                      (state in the caption). The y-axis extends to cover them.
#                      Label interval bounds above 1000% are shown as ">1000"
#                      (see fmt_bound below; state in the caption).
#   sfig_sens02_growth_excl_zero_min6_MI.png  countries with >= 6 years with cases — sensitivity
#                      variant matching the main fig3b inclusion rule, on the
#                      same fixed y-axis as fig3b.
#
# Inputs:  runs/mi_full/growth/country_growth_pooled.csv, runs/mi_full/growth/growth_pooled.csv
# Outputs: output/figures/sensitivity/sfig_sens02_growth_excl_zero_MI.png, output/figures/sensitivity/sfig_sens02_growth_excl_zero_min6_MI.png,
#          output/figures/sensitivity/sfig_sens02_growth_excl_zero_vs_years_since_emergence_MI.png
#          (per-country growth against years since first reported cases, two
#          panels: 1990-2024 and since emergence; >= 6 countries as fig3b)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggrepel)

region_cols <- c(
  "South America" = "#F6D49B",
  "North & Central America" = "#E28E49",
  "Caribbean" = "#F2B06D",
  "East & Southeast Asia" = "#2A6F9E",
  "South Asia" = "#A57DB8",
  "Pacific Islands" = "#B58EA8",
  "Sub-Saharan Africa" = "#6AA84F",
  "Europe, Middle East & North Africa" = "#2A9D8F"
)
lv <- names(region_cols) # display order

country_pooled <- read.csv("runs/mi_full/growth/country_growth_pooled.csv")

# first year with reported cases per country, for the outlier labels — taken
# from the released point estimates (a descriptive label; the regressions
# themselves redetermine the emergence year within each imputed dataset)
emergence_year <- read.csv("runs/mi_full/mi50/opendengue_gap_filled_MI.csv") %>%
  group_by(adm_0_name, Year) %>%
  summarise(total = sum(dengue_total_scaled), .groups = "drop") %>%
  filter(total > 0) %>%
  group_by(adm_0_name) %>%
  summarise(first_case_year = min(Year), .groups = "drop")
pooled_medians <- read.csv("runs/mi_full/growth/growth_pooled.csv") %>%
  filter(metric == "region_growth_since_emergence_pct") %>%
  transmute(
    od_region = factor(scope, levels = lv), pooled = point,
    lwr = lwr, upr = upr
  ) %>%
  filter(!is.na(od_region))

# interval bounds in the outlier labels. A country with only a few case-years
# and long runs of zeros can have an essentially unbounded log-scale SE, so its
# upper bound after transformation to %/yr is astronomically large (and its lower
# bound is -100%). Such a bound carries no information beyond "unbounded", so
# anything above LABEL_BOUND_CAP is displayed as ">CAP" rather than as a
# many-digit number. State in the caption.
LABEL_BOUND_CAP <- 1000
fmt_bound <- function(x) {
  ifelse(x > LABEL_BOUND_CAP, sprintf(">%d", LABEL_BOUND_CAP), sprintf("%.0f", x))
}

draw_since_emergence <- function(metric_name, outfile, ylim = NULL) {
  cty <- country_pooled %>%
    filter(metric == metric_name) %>%
    mutate(od_region = factor(od_region, levels = lv)) %>%
    left_join(emergence_year, by = "adm_0_name")

  cat(sprintf("\n---- %s: %d countries ----\n", metric_name, nrow(cty)))
  cat("per-country estimates, highest first:\n")
  cty %>%
    arrange(desc(point)) %>%
    transmute(adm_0_name, od_region, first_case_year,
      point = round(point, 1), lwr = round(lwr, 1), upr = round(upr, 1)
    ) %>%
    as.data.frame() %>%
    print(row.names = FALSE, max = 50)
  region_medians <- cty %>%
    group_by(od_region) %>%
    summarise(median_rate = median(point), n = dplyr::n(), .groups = "drop") %>%
    mutate(xpos = as.numeric(od_region))
  chk <- region_medians %>% left_join(pooled_medians, by = "od_region")
  cat("regional medians: drawn (median of plotted points) | MI-pooled at >=6, console check only, not cited\n")
  for (i in seq_len(nrow(chk))) {
    cat(sprintf(
      "  %-36s drawn %6.2f | pooled(>=6) %6.2f [%.2f, %.2f] (n=%d)\n",
      chk$od_region[i], chk$median_rate[i], chk$pooled[i], chk$lwr[i], chk$upr[i], chk$n[i]
    ))
  }

  # y-axis: fixed limits when given (comparability with fig3b); otherwise sized
  # to the data so imprecise recently-emerging countries are not clipped
  if (is.null(ylim)) {
    ymax <- ceiling(max(cty$point) / 50) * 50
    ymin <- floor(min(c(cty$point, -50)) / 50) * 50
    ylim <- c(ymin - 10, ymax)
  }
  n_clip <- sum(cty$point < ylim[1] | cty$point > ylim[2])
  if (n_clip > 0) message(n_clip, " point(s) fall outside the y-limits and are not drawn")

  # outlier labelling favours recently emerging countries: within each region,
  # deviation from the regional median is compared against the regional SD, with
  # a looser cut for later emergence (recent: 1 SD; 2000-2014: 1 SD and only
  # above the median; pre-2000: 2 SD). Two countries are excluded manually to
  # reduce label clutter.
  outliers <- cty %>%
    mutate(emergence_period = case_when(
      first_case_year >= 2015 ~ "recent",
      first_case_year >= 2000 ~ "established",
      TRUE ~ "long_established"
    )) %>%
    group_by(od_region) %>%
    mutate(
      median_rate = median(point),
      dev = abs(point - median_rate),
      sd_rate = sd(point)
    ) %>%
    ungroup() %>%
    mutate(
      labels = case_when(
        emergence_period == "recent" ~ dev > 1 * sd_rate,
        emergence_period == "established" ~ dev > 1 * sd_rate & point > median_rate,
        TRUE ~ dev > 2 * sd_rate
      )
    ) %>%
    filter(labels) %>%
    filter(!grepl("BONAIRE|CAMEROON", adm_0_name))

  p <- ggplot(cty, aes(x = od_region, y = point)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_beeswarm(aes(color = od_region),
      size = 4.5, cex = 1,
      method = "swarm", priority = "density"
    ) +
    geom_segment(
      data = region_medians,
      aes(x = xpos - 0.1, xend = xpos + 0.1, y = median_rate, yend = median_rate),
      linewidth = 1, color = "black"
    ) +
    ggrepel::geom_text_repel(
      data = outliers,
      aes(label = sprintf(
        "%s\n(%d; %.0f [%s, %s])",
        adm_0_name, first_case_year, point,
        fmt_bound(lwr), fmt_bound(upr)
      )),
      size = 4, color = "black", fontface = "bold", bg.color = "white", bg.r = 0.1,
      box.padding = 0.3, point.padding = 0.3, segment.color = "grey50",
      segment.size = 0.3, max.overlaps = Inf
    ) +
    scale_color_manual(values = region_cols) +
    scale_y_continuous(breaks = seq(-50, ylim[2], by = 50), limits = ylim) +
    scale_x_discrete(labels = function(x) gsub(" & ", "\n& ", x)) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 18, face = "bold", vjust = 0),
      axis.title.y = element_text(size = 18, face = "plain", vjust = 2),
      legend.position = "bottom",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(30, 10, 20, 10)
    ) +
    labs(
      x = NULL,
      y = "Population-adjusted annual growth rate,\nfirst emergence to 2024 (%)"
    ) +
    guides(color = "none")

  print(p)
  ggsave(plot = p, outfile, bg = "white", width = 14, height = 8, dpi = 300)
  invisible(p)
}

# supplementary figure: >= 3 years with cases (recently emerging countries included)
draw_since_emergence(
  "country_growth_since_emergence_min3_pct",
  "output/figures/sensitivity/sfig_sens02_growth_excl_zero_MI.png"
)

# sensitivity variant: >= 6 years with cases, same rule and y-axis as fig3b
draw_since_emergence("country_growth_since_emergence_pct",
  "output/figures/sensitivity/sfig_sens02_growth_excl_zero_min6_MI.png",
  ylim = c(-60, 100)
)

country_pooled %>%
  filter(metric == "country_growth_since_emergence_min3_pct") %>%
  slice_max(order_by = point, n = 10) %>%
  select(adm_0_name, point:upr)

# ------------------------------------------------------------------------------
# Per-country growth rate against time since emergence, two panels on the same
# countries (>= 6 years with cases, as fig3b): the 1990-2024 growth rate that
# includes the years before first reported cases, and the growth rate since
# first reported cases. x = 2024 minus the first year with cases (from the
# release point estimates, as for the labels above). An ordinary least-squares
# line with its 95% confidence band is drawn per panel and the slope, its 95%
# CI and R^2 are printed. In the left panel y is computed over a series that
# starts with zeros for every country with x < 34, so a relation with x is
# built into the calculation; the right panel removes that. Countries with
# cases already in 1990 all sit at x = 34: their true time since emergence is
# longer (left-censored by the start of the data). The lines are descriptive;
# the per-country estimates differ widely in precision and the fits are not
# weighted.
# ------------------------------------------------------------------------------
yse_versions <- c(
  country_growth_pct = "1990-2024, including years before first reported cases",
  country_growth_since_emergence_pct = "Since first reported cases"
)
yse <- country_pooled %>%
  filter(metric %in% names(yse_versions)) %>%
  inner_join(emergence_year, by = "adm_0_name") %>%
  mutate(
    years_since = 2024L - first_case_year,
    od_region = factor(od_region, levels = lv),
    version = factor(yse_versions[metric], levels = yse_versions)
  )
yse_lab <- yse %>%
  group_by(version) %>%
  group_modify(function(d, key) {
    f <- lm(point ~ years_since, data = d)
    ci <- confint(f)["years_since", ]
    data.frame(label = sprintf(
      "OLS slope %.2f %%/yr per year\n(95%% CI %.2f, %.2f), R² = %.2f, n = %d",
      coef(f)["years_since"], ci[1], ci[2], summary(f)$r.squared, nrow(d)
    ))
  }) %>%
  ungroup()
cat("\n---- per-country growth vs years since first reported cases ----\n")
for (i in seq_len(nrow(yse_lab))) cat(sprintf("  [%s]\n  %s\n", yse_lab$version[i], gsub("\n", " ", yse_lab$label[i])))

p_yse <- ggplot(yse, aes(x = years_since, y = point)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey75", linewidth = 0.8) +
  geom_point(aes(colour = od_region), size = 2.8, alpha = 0.9) +
  geom_text(
    data = yse_lab, aes(label = label), x = 34, y = Inf,
    hjust = 1, vjust = 1.3, size = 3.8, lineheight = 0.9
  ) +
  # panels are lettered (a = 1990-2024, b = since first reported cases); the
  # panel definitions go in the figure caption
  facet_wrap(~version, labeller = as_labeller(setNames(c("a", "b"), yse_versions))) +
  scale_colour_manual(values = region_cols, name = NULL) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  labs(
    x = "Years since first reported cases (to 2024)",
    y = "Population-adjusted annual growth rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    strip.text = element_text(size = 18, face = "bold", hjust = 0),
    panel.spacing = unit(1.2, "lines")
  ) +
  guides(colour = guide_legend(nrow = 2))
print(p_yse)
ggsave(
  plot = p_yse, "output/figures/sensitivity/sfig_sens02_growth_excl_zero_vs_years_since_emergence_MI.png",
  bg = "white", width = 14, height = 7, dpi = 300
)
