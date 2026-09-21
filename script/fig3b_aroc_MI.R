# ==============================================================================
# FIG 3B (MI) — per-country population-adjusted growth rates, 1990-2024
# ==============================================================================
# Fully MI-based: the DOTS are the per-country Rubin-pooled MI estimates
# (country_growth_pooled.csv, metric country_growth_pct) and the black MEDIAN
# lines are the median of the plotted per-country estimates — a visual summary
# of the dots, so the figure is self-consistent. The MI-pooled regional median
# (growth_pooled.csv, region_growth_pct: the median computed within each imputed
# dataset, then pooled) is a different summary read here ONLY for the console
# cross-check — it is not drawn in the figure and not a manuscript-cited number.
# Its interval covers the between-imputation spread only (percentile of the 50
# per-dataset medians), so it is not a full 95% CI.
# The median segments' x-positions are taken from the display-order factor
# explicitly, so they sit on the same category positions as the axis.
# Inputs:  runs/mi_full/growth/country_growth_pooled.csv, runs/mi_full/growth/growth_pooled.csv
# Output:  output/figures/fig3b_MI.png
# ==============================================================================

library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggrepel)
# Under Rscript there is no screen device, so print() of a plot would write Rplots.pdf;
# send those calls to a null device. ggsave() opens its own device and is unaffected.
if (!interactive()) pdf(NULL)

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

cty <- read.csv("runs/mi_full/growth/country_growth_pooled.csv") %>%
  filter(metric == "country_growth_pct") %>%
  mutate(od_region = factor(od_region, levels = lv))
# direction of the pooled country-level growth rates
cat("\n=== country growth direction (pooled point estimates) ===\n")
cty %>%
  mutate(direction = case_when(
    point > 0 ~ "increasing",
    point < 0 ~ "decreasing",
    TRUE ~ NA_character_
  )) %>%
  count(direction) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
print(as.data.frame(cty), row.names = FALSE)

# drawn lines = median of the plotted points; the MI-pooled regional medians
# (growth_pooled.csv) are read ONLY for the console cross-check below — they are
# not plotted and not cited in the manuscript
region_medians <- cty %>%
  group_by(od_region) %>%
  summarise(median_rate = median(point), n = dplyr::n(), .groups = "drop") %>%
  mutate(xpos = as.numeric(od_region))
pooled_medians <- read.csv("runs/mi_full/growth/growth_pooled.csv") %>%
  filter(metric == "region_growth_pct") %>%
  transmute(
    od_region = factor(scope, levels = lv), pooled = point,
    lwr = lwr, upr = upr
  ) %>%
  filter(!is.na(od_region))

cat("\n=== regional medians: drawn (median of plotted points) vs MI-pooled (growth_pooled.csv) ===\n")
chk <- region_medians %>% left_join(pooled_medians, by = "od_region")
for (i in seq_len(nrow(chk))) {
  cat(sprintf(
    "  %-36s drawn %6.2f | pooled %6.2f [%.2f, %.2f] (n=%d)\n",
    chk$od_region[i], chk$median_rate[i], chk$pooled[i], chk$lwr[i], chk$upr[i], chk$n[i]
  ))
}

cty %>%
  arrange(desc(point)) %>%
  slice_max(order_by = point, n = 10)

# outlier labelling
regional_stats <- cty %>%
  group_by(od_region) %>%
  summarise(median_growth = median(point), sd_growth = sd(point), .groups = "drop")
outliers <- cty %>%
  left_join(regional_stats, by = "od_region") %>%
  mutate(
    is_outlier2 = abs(point - median_growth) > 1.5 * sd_growth,
    is_outlier3 = abs(point - median_growth) > 1 * sd_growth
  ) %>%
  mutate(
    labels = ifelse((od_region %in% c("South Asia", "Sub-Saharan Africa", "Europe, Middle East & North Africa") & is_outlier3), TRUE, FALSE),
    labels = ifelse((!od_region %in% c("South Asia", "Sub-Saharan Africa", "Europe, Middle East & North Africa") & is_outlier2), TRUE, labels)
  ) %>%
  filter(labels)

n_clip <- sum(cty$point < -60 | cty$point > 100)
if (n_clip > 0) message(n_clip, " point(s) fall outside the y-limits (-60, 100) and are not drawn")

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
    data = outliers, aes(label = adm_0_name),
    size = 5, color = "black", fontface = "bold", bg.color = "white", bg.r = 0.1,
    box.padding = 0.3, point.padding = 0.3, segment.color = "grey50",
    segment.size = 0.3, max.overlaps = Inf,
    seed = 123 # fixed label placement, so the figure is identical on every run
  ) +
  scale_color_manual(values = region_cols) +
  scale_y_continuous(breaks = seq(-50, 100, by = 50), limits = c(-60, 100)) +
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
  labs(x = NULL, y = "Population-adjusted annual growth rate (%)") +
  guides(color = "none")

print(p)
ggsave(
  plot = p, "output/figures/fig3b_MI.png", bg = "white",
  width = 14, height = 8, dpi = 300
)

# ---- Source data for Fig. 3b: country growth rates and the regional medians drawn as lines ----
# Assembled into one workbook per figure by script/fig_source_data_xlsx.R.
dir.create("output/source_data", recursive = TRUE, showWarnings = FALSE)
write.csv(cty %>% transmute(adm_0_name, ISO_A0, od_region, growth_pct_per_year = point, lwr, upr) %>%
    arrange(od_region, adm_0_name),
  "output/source_data/fig3b_source_data_countries.csv", row.names = FALSE)
write.csv(region_medians %>% transmute(od_region, median_growth_pct_per_year = median_rate, n_countries = n),
  "output/source_data/fig3b_source_data_region_median.csv", row.names = FALSE)
