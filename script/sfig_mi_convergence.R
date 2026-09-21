# ==============================================================================
# Supplementary figure: multiple-imputation convergence diagnostic
# ==============================================================================
# Plots the cumulative convergence written by run_pipeline_MI_full.R: as
# imputations are pooled (1..N), the median width of the pooled 95% uncertainty
# interval is tracked separately for the estimated-annual cells (IHME /
# neighbour-median totals, the uncertainty-dominant group) and all other
# gap-filled cells. Flat curves before the final run indicate N is sufficient.
#
# Input : runs/mi_full/descriptive_summary/convergence_by_run.csv  (columns runs, draws, w_est, w_oth, ...)
# Output: output/figures/sfig_mi_convergence.png
# ==============================================================================

suppressMessages({library(ggplot2); library(dplyr); library(tidyr)})

d <- read.csv("runs/mi_full/descriptive_summary/convergence_by_run.csv")

long <- d %>%
  select(runs, w_est, w_oth) %>%
  pivot_longer(c(w_est, w_oth), names_to = "grp", values_to = "width") %>%
  mutate(grp = recode(grp,
    w_est = "Estimated-annual cells (GBD / neighbour-median)",
    w_oth = "Other gap-filled cells"))

p <- ggplot(long, aes(runs, width, colour = grp, linetype = grp)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.1) +
  scale_colour_manual(values = c("#2a78d6", "#888780")) +
  scale_linetype_manual(values = c("solid", "22")) +
  scale_x_continuous(breaks = seq(0, 50, 10), expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Number of imputations pooled",
       y = "Median 95% interval width (cases)",
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.5), legend.justification = c(1, 0.5),   # empty band between the two curves
        legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("output/figures/sfig_mi_convergence.png", p, width = 7, height = 4.2, dpi = 300, bg = "white")
cat("wrote output/figures/sfig_mi_convergence.png\n")
