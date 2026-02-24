# ==============================================================================
# VISUALIZATION: Using Interaction Model Results
# ==============================================================================

power_int_results <- read.csv("output/tables/wavelet_power_interaction_results.csv")
sync_int_results <- read.csv("output/tables/wavelet_sync_interaction_results.csv")


# --- 1A: POWER % Change per Year (from interaction model) ---
power_int_for_plot <- power_int_results %>%
  filter(Region %in% c("Global", "Americas", "Asia")) %>%
  mutate(
    Scope = factor(Region, levels = c("Global", "Americas", "Asia")),
    Cycle = factor(Cycle, levels = c("Annual", "Multiannual")),
    signif = case_when(P_value < 0.01 ~ "**", P_value < 0.05 ~ "*", TRUE ~ "")
  )

cat("Power interaction model data for plot:\n")
print(power_int_for_plot %>% select(Cycle, Region, Pct_Change, CI_Lower, CI_Upper, P_value))

fig_power_pct_change_int <- ggplot(power_int_for_plot, aes(x = Cycle, y = Pct_Change, color = Scope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8
  ) +
  geom_text(
    aes(label = signif, y = CI_Upper + 0.5),
    position = position_dodge(width = 0.5), size = 6, vjust = 0, show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Geographic Scope",
    values = c("Global" = "#1B9E77", "Americas" = "#D95F02", "Asia" = "#7570B3")
  ) +
  labs(
    # title = "Power",
    # subtitle = "Endemic only; * indicates p < 0.05",
    x = "Cycle Type",
    y = "Power (% change per year)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "top",
    legend.title = element_text(size = 12)
  )

print(fig_power_pct_change_int)

# --- 1B: SYNCHRONY % Change per Year (from interaction model) ---
sync_int_for_plot <- sync_int_results %>%
  filter(Pair_Type %in% c("Global", "Within Americas", "Within Asia")) %>%
  mutate(
    Scope = case_when(
      Pair_Type == "Global" ~ "Global",
      Pair_Type == "Within Americas" ~ "Americas",
      Pair_Type == "Within Asia" ~ "Asia"
    ),
    Scope = factor(Scope, levels = c("Global", "Americas", "Asia")),
    Cycle = factor(Cycle, levels = c("Annual", "Multiannual")),
    signif = case_when(Trend_P < 0.01 ~ "**", Trend_P < 0.05 ~ "*", TRUE ~ "")
  )

cat("\nSynchrony interaction model data for plot:\n")
print(sync_int_for_plot %>% select(Cycle, Pair_Type, Pct_Change_Per_Year, CI_Lower, CI_Upper, Trend_P))

fig_sync_pct_change_int <- ggplot(sync_int_for_plot, aes(x = Cycle, y = Pct_Change_Per_Year, color = Scope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8
  ) +
  geom_text(
    aes(label = signif, y = CI_Upper + 0.3),
    position = position_dodge(width = 0.5), size = 6, vjust = 0, show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Geographic Scope",
    values = c("Global" = "#1B9E77", "Americas" = "#D95F02", "Asia" = "#7570B3")
  ) +
  labs(
    # title = "Synchrony",
    # subtitle = "Endemic only; * indicates p < 0.05",
    x = "Cycle Type",
    y = "Synchrony (% change per year)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "top",
    legend.title = element_text(size = 12)
  )

print(fig_sync_pct_change_int)

# --- Combined Plot: Power + Synchrony % Change from Interaction Models ---
library(patchwork)
fig_pct_change_combined_int <-
  (fig_power_pct_change_int + fig_sync_pct_change_int) +
    plot_layout(ncol = 2) +
    plot_annotation(
      tag_levels = "a"
    ) &
    theme(
      legend.position = "top",
      # plot.tag.position = c(0, 0.98),
      plot.tag = element_text(size = 15, face = "bold"),

      # ⬇ gives room for legend above
      plot.margin = margin(t = 10, r = 5, b = 10, l = 5),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(10, 0, 10, 0)
    )


print(fig_pct_change_combined_int)
ggsave("output/figures/wavelet_trends.png", fig_pct_change_combined_int,
  width = 12, height = 6, bg = "white", dpi = 300
)

# --- C: synchrony comparison between hemisphere pairing groups ---

paired_data_combined <- read.csv("output/tables/wavelet_hemi_full_results.csv")
annotations <- read.csv("output/tables/wavelet_hemi_diff_results.csv")


# Create violin + box + jitter plot
fig_violin <- ggplot(pair_data_combined, aes(x = Group, y = mean_sync, fill = Group)) +
  geom_violin(alpha = 0.5, color = NA, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.9, outlier.size = 0.8, color = "grey20") +
  facet_wrap(~Cycle, ncol = 2) +
  # Add significance annotation at top
  # Bracket connecting the two groups
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
  # Add significance annotation at top
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
  # Color scales
  scale_fill_manual(values = c("Between" = "#0072B2", "Within" = "#E69F00"), guide = "none") +
  scale_color_manual(
    values = c("TRUE" = "#D55E00", "FALSE" = "grey60"),
    guide = "none"
  ) +
  # Axis settings
  scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
  # Labels
  labs(
    # title = "Hemisphere Effect on Dengue Synchrony",
    # subtitle = "Distribution of pair-mean synchrony values",
    x = "Hemishpere paring",
    y = "Mean synchrony per pair"
  ) +
  # Theme
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40"),
    strip.text = element_text(face = "bold", size = 15),
    axis.text.x = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

print(fig_violin)

fig_violin_c <- fig_violin +
  labs(tag = "c") +
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    plot.tag.position = c(.01, 0.99)
  )

print(fig_violin_c)
ggsave("output/figures/wavelet_hemi.png", fig_violin_c,
  width = 12, height = 6, bg = "white", dpi = 300
)
