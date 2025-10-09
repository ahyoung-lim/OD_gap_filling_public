today <- Sys.Date()

Heatmap <- function(data, region_name, complete = F) {
  p <- data %>%
    filter(region == region_name) %>%
    mutate(Year = as.character(Year)) %>%
    ggplot(aes(x = Year, y = adm_0_name, fill = factor(T_res))) +
    geom_tile(color = "white", lwd = 0.6, linetype = 1) +
    scale_y_discrete(limits = rev, expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0), breaks = seq(1990, 2024, by = 2)) +
    theme_bw() +
    xlab("Year") +
    ylab("Country") +
    ggtitle(paste0("Best temporal resolution available (as of ", today, ")")) +
    theme(
      axis.ticks = element_line(linewidth = 0.2),
      axis.ticks.length = unit(1.5, "pt"),
      legend.spacing.x = unit(0.2, "cm"),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0.2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    scale_fill_manual(
      name = "Temporal resolution",
      values = c("#479A5A", "#99D8CA", "#E5F5F9"),
      na.value = "grey70"
    )
  # facet_grid(subregion ~ ., switch = "y", scales = "free_y", space = "free_y")

  if (complete) {
    p <- data %>%
      mutate(
        T_res_n = ifelse(
          cat %in% c("Complete_data", "High_cov"),
          as.character(T_res),
          NA_character_
        ),
        T_res_n = factor(T_res_n, levels = levels(T_res)) # preserves order, includes NA
      ) %>%
      filter(region == region_name) %>%
      mutate(Year = as.character(Year)) %>%
      ggplot(aes(x = Year, y = adm_0_name, fill = factor(T_res_n))) +
      geom_tile(colour = "white", linewidth = 0.6, linetype = 1) +
      scale_y_discrete(limits = rev, expand = c(0, 0)) +
      scale_x_discrete(expand = c(0, 0), breaks = seq(1990, 2024, by = 2)) +
      theme_bw() +
      labs(
        x = "Year",
        y = "Country",
        title = paste0("Complete data (as of ", today, ")"),
        fill = "Temporal resolution"
      ) +
      theme(
        axis.ticks = element_line(linewidth = 0.2),
        axis.ticks.length = unit(1.5, "pt"),
        legend.spacing.x = unit(0.2, "cm"),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside"
      ) +
      scale_fill_manual(
        values = c("#479A5A", "#99D8CA", "#E5F5F9"),
        na.value = "grey70"
      )

    # facet_grid(
    #   subregion ~ .,
    #   switch = "y",
    #   scales = "free_y",
    #   space = "free_y"
    # )
  }
  return(p)
}
