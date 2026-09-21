# ==============================================================================
# FIG 4 — trends in wavelet power and synchrony, pooled across imputations
# ==============================================================================
# Panel (a): trend in annual- and multiannual-band wavelet power (% change per
# year) for the Global / Americas / Asia scopes. Panel (b): the same for
# synchrony (pair types Global / Within Americas / Within Asia; the
# Americas-Asia cross-pairs are tabulated but not plotted).
#
# Estimates are the MI results of script/04c_mi_wavelet_trends.R
# (output/tables/wavelet_power_interaction_results_MI.csv and
# wavelet_sync_interaction_results_MI.csv): the wavelet analysis run on each of
# 50 imputed datasets, the model coefficients pooled with Rubin's rules on the
# log / logit scale and back-transformed to % change per year with a 95% CI.
# An asterisk marks estimates whose 95% CI excludes zero.
#
# Panel (c): pair-mean synchrony by hemisphere pairing (Within = both countries
# in the same hemisphere, Between = cross-equatorial), by cycle type. Violin /
# box show the per-pair mean synchrony averaged across the 50 imputed datasets
# (wavelet_hemi_full_results_MI.csv); the bracket annotation gives the
# MI-pooled Within - Between difference from the mixed-effects model, with its
# 95% CI and p-value (wavelet_hemi_diff_results_MI.csv).
# Output: output/figures/fig4_MI.png
# ==============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
# Under Rscript there is no screen device, so print() of a plot would write Rplots.pdf;
# send those calls to a null device. ggsave() opens its own device and is unaffected.
if (!interactive()) pdf(NULL)

pow_tab <- read.csv("output/tables/wavelet_power_interaction_results_MI.csv")
syn_tab <- read.csv("output/tables/wavelet_sync_interaction_results_MI.csv")
pooled <- bind_rows(
  pow_tab %>% transmute(analysis = "power", cycle = Cycle, region = Region,
                        mag_point = Pct_Change, mag_lwr = CI_Lower, mag_upr = CI_Upper),
  syn_tab %>% transmute(analysis = "synchrony", cycle = Cycle, region = Pair_Type,
                        mag_point = Pct_Change_Per_Year, mag_lwr = CI_Lower, mag_upr = CI_Upper)
)
hemi_pairs <- read.csv("output/tables/wavelet_hemi_full_results_MI.csv")
hemi_pooled <- read.csv("output/tables/wavelet_hemi_diff_results_MI.csv")

scope_cols <- c("Global" = "#1B9E77", "Americas" = "#D95F02", "Asia" = "#7570B3")

prep <- function(d, scope_map) {
  d %>%
    mutate(
      Scope = factor(scope_map[region], levels = names(scope_cols)),
      Cycle = factor(cycle, levels = c("Annual", "Multiannual")),
      signif = ifelse(mag_lwr > 0 | mag_upr < 0, "*", "")
    ) %>%
    filter(!is.na(Scope))
}

panel <- function(d, ylab) {
  ggplot(d, aes(x = Cycle, y = mag_point, color = Scope)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(
      aes(ymin = mag_lwr, ymax = mag_upr),
      position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8
    ) +
    geom_text(
      aes(label = signif, y = mag_upr + 0.3),
      position = position_dodge(width = 0.5), size = 6, vjust = 0, show.legend = FALSE
    ) +
    scale_color_manual(name = "Geographic Scope", values = scope_cols) +
    labs(x = "Cycle Type", y = ylab) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "top",
      legend.title = element_text(size = 12)
    )
}

pow <- prep(
  pooled %>% filter(analysis == "power"),
  c("Global" = "Global", "Americas" = "Americas", "Asia" = "Asia")
)
syn <- prep(
  pooled %>% filter(analysis == "synchrony"),
  c("Global" = "Global", "Within Americas" = "Americas", "Within Asia" = "Asia")
)

cat("Power trends (%/yr, Rubin 95% CI; * = CI excludes 0):\n")
print(pow %>% transmute(Cycle, Scope,
  est = sprintf("%.2f [%.2f, %.2f]%s", mag_point, mag_lwr, mag_upr, signif)),
  row.names = FALSE)
cat("\nSynchrony trends (%/yr, Rubin 95% CI; * = CI excludes 0):\n")
print(syn %>% transmute(Cycle, Scope,
  est = sprintf("%.2f [%.2f, %.2f]%s", mag_point, mag_lwr, mag_upr, signif)),
  row.names = FALSE)

# ---- panel c: synchrony by hemisphere pairing ----
hemi_pairs <- hemi_pairs %>%
  mutate(
    Group = factor(hemi_type, levels = c("Between", "Within")),
    Cycle = factor(Cycle, levels = c("Annual", "Multiannual"))
  )
annotations <- hemi_pooled %>%
  filter(group == "Difference") %>%
  rename(Cycle = cycle, point = diff_raw, lwr = diff_lo, upr = diff_hi) %>%
  mutate(
    Cycle = factor(Cycle, levels = c("Annual", "Multiannual")),
    diff_label = sprintf("Diff. = %.2f [%.2f, %.2f]", point, lwr, upr),
    p_label = ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.2f", p_value)),
    signif = p_value < 0.05
  )

cat("\nHemisphere contrast, Within - Between (Rubin 95% CI):\n")
print(annotations %>% transmute(Cycle,
  est = sprintf("%.3f [%.3f, %.3f], %s", point, lwr, upr, p_label)),
  row.names = FALSE)

panel_c <- ggplot(hemi_pairs, aes(x = Group, y = mean_sync, fill = Group)) +
  geom_violin(alpha = 0.5, color = NA, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.9, outlier.size = 0.8, color = "grey20") +
  facet_wrap(~Cycle, ncol = 2) +
  # bracket connecting the two groups, colored by significance
  geom_segment(
    data = annotations,
    aes(x = 1, xend = 2, y = 0.97, yend = 0.97, color = signif),
    inherit.aes = FALSE, linewidth = 1
  ) +
  geom_segment(
    data = annotations,
    aes(x = 1, xend = 1, y = 0.95, yend = 0.97, color = signif),
    inherit.aes = FALSE, linewidth = 0.8
  ) +
  geom_segment(
    data = annotations,
    aes(x = 2, xend = 2, y = 0.95, yend = 0.97, color = signif),
    inherit.aes = FALSE, linewidth = 0.8
  ) +
  geom_text(
    data = annotations,
    aes(x = 1.5, y = 1.08, label = diff_label),
    inherit.aes = FALSE, size = 4, color = "grey20"
  ) +
  geom_text(
    data = annotations,
    aes(x = 1.5, y = 1.0, label = p_label, fontface = ifelse(signif, "bold", "plain")),
    inherit.aes = FALSE, size = 4,
    color = ifelse(annotations$signif, "#D55E00", "grey50")
  ) +
  scale_fill_manual(values = c("Between" = "#0072B2", "Within" = "#E69F00"), guide = "none") +
  scale_color_manual(values = c("TRUE" = "#D55E00", "FALSE" = "grey60"), guide = "none") +
  scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
  labs(x = "Hemisphere pairing", y = "Mean synchrony per pair") +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

fig4 <- (panel(pow, "Power (% change per year)") +
  panel(syn, "Synchrony (% change per year)")) /
  panel_c +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "top",
    plot.tag = element_text(size = 15, face = "bold"),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 5),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(10, 0, 10, 0)
  )

print(fig4)
ggsave("output/figures/fig4_MI.png", fig4,
  width = 12, height = 12, bg = "white", dpi = 300
)

# ---- Source data for Fig. 4: pooled trend estimates (a, b) and hemisphere pairing (c) ----
# Assembled into one workbook per figure by script/fig_source_data_xlsx.R.
dir.create("output/source_data", recursive = TRUE, showWarnings = FALSE)
sd_trend <- function(d) d %>%
  transmute(Cycle, Scope, pct_change_per_year = mag_point, ci_lower = mag_lwr, ci_upper = mag_upr) %>%
  arrange(Cycle, Scope)
write.csv(sd_trend(pow), "output/source_data/fig4_source_data_a.csv", row.names = FALSE)
write.csv(sd_trend(syn), "output/source_data/fig4_source_data_b.csv", row.names = FALSE)
write.csv(hemi_pairs %>% transmute(Cycle, hemisphere_pairing = hemi_type, pair_id, mean_sync) %>%
    arrange(Cycle, hemisphere_pairing, pair_id),
  "output/source_data/fig4_source_data_c_pairs.csv", row.names = FALSE)
write.csv(hemi_pooled %>% transmute(cycle, group, n_pairs, model_mean = emmean, mean_lower = lo, mean_upper = hi,
    difference = diff_raw, diff_lower = diff_lo, diff_upper = diff_hi, p_value),
  "output/source_data/fig4_source_data_c_diff.csv", row.names = FALSE)
