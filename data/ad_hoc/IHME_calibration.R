library(dplyr)
library(countrycode)
library(ggplot2)
library(patchwork)


tab <- read.csv("data/processed_data/dt_heatmap_2025_07_16.csv")
tab$ISO_A0 <- countrycode(
  sourcevar = tab$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)
tab$ISO_A0[tab$adm_0_name == "SAINT MARTIN"] <- "MAF"

# IHME data
dt <- read.csv("data/pacific_data_ad_hoc/IHME-GBD_2021_DATA.csv")

head(dt)

dt <- dt %>%
  select(adm_0_name = location_name, Year = year, IHME_estimate = val, upper, lower)

unique(dt$location_name)
dt$ISO_A0 <- countrycode(
  sourcevar = dt$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)


OD_no_data_ctry <- unique(tab$ISO_A0[tab$cat_model == "No_data" & tab$Year < 2022])
IHME_ctry <- unique(dt$ISO_A0)

# countries available in OD but not in IHME
not_in_IHME <- OD_no_data_ctry[!OD_no_data_ctry %in% IHME_ctry]

unique(tab$adm_0_name[tab$ISO_A0 %in% not_in_IHME])
# [1] "ANGUILLA"                          "BONAIRE, SAINT EUSTATIUS AND SABA"
# [3] "CURACAO"                           "FRENCH POLYNESIA"
# [5] "NEW CALEDONIA"                     "PITCAIRN"
# [7] "SINT MAARTEN"                      "WALLIS AND FUTUNA"

# countries available in IHME
in_IHME <- OD_no_data_ctry[OD_no_data_ctry %in% IHME_ctry]
unique(tab$adm_0_name[tab$ISO_A0 %in% in_IHME])

tab_merge <- tab %>%
  left_join(., dt %>%
    filter(ISO_A0 %in% in_IHME) %>%
    select(ISO_A0, IHME_estimate, lower, upper, Year),
  by = c("ISO_A0", "Year")
  )

tab_merge <- tab_merge %>%
  # filter(ISO_A0 %in% in_IHME) %>%
  group_by(adm_0_name) %>%
  mutate(
    OD_avg = mean(annual_total, na.rm = T),
    IHME_avg = mean(IHME_estimate, na.rm = T),
    scalar = OD_avg / IHME_avg
  ) %>%
  mutate(IHME_calibrated = IHME_estimate * scalar)


tab_merge <- tab_merge %>%
  mutate(
    annual_total2 = ifelse(cat_model == "No_data" & is.na(annual_total) & !is.na(IHME_calibrated), IHME_calibrated, annual_total),
    est = ifelse(cat_model == "No_data" & is.na(annual_total) & !is.na(IHME_calibrated), "IHME_calibrated", "OD")
  )

tab_merge %>%
  filter(ISO_A0 %in% in_IHME) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = annual_total2)) +
  geom_point(aes(y = annual_total2, color = est)) +
  facet_wrap(adm_0_name ~ ., scales = "free")

tab_merge %>%
  group_by(est) %>%
  tally()

#   est                 n
#   <chr>           <int>
# 1 IHME_calibrated   160
# 2 OD               3585

tab2$cat_model <- factor(tab2$cat_model, levels = c(
  "No_data",
  "Disaggregation_annual",
  "Disaggregation_medium",
  "Imputation_medium",
  "Imputation_small",
  "No_modelling_required"
))

tab2 %>%
  group_by(cat_model, cat) %>%
  tally()

# write.csv(tab2, "data/processed_data/dt_heatmap_2025_07_15.csv", row.names = F)

tab_merge$cat_model <- factor(tab_merge$cat_model, levels = c(
  "No_data",
  "Disaggregation_annual",
  "Disaggregation_medium",
  "Imputation_medium",
  "Imputation_small",
  "No_modelling_required"
))

tab_merge$region_facet <- factor(tab_merge$region_facet,
  levels = c(
    "Asia | PICs",
    "Asia | SEARO",
    "Asia | WPRO",
    "Americas | Caribbean",
    "Americas | PAHO",
    "EMRO | EMRO",
    "EURO | EURO"
  )
)


library(ggpubfigs)
library(ggh4x)
pals <- friendly_pal(5, name = "zesty_four", type = "continuous")
pals <- c(pals, "#7F7F7F", "#C17193")


tab_merge %>%
  # filter(!is.na(annual_total2) & cat_model == "No_data")%>%
  mutate(
    cat_model = ifelse(
      !is.na(annual_total2) & cat_model == "No_data",
      "Disaggregation_annual",
      as.character(cat_model) # convert factor to character safely
    ),
    cat_model = factor(cat_model, levels = levels(tab_merge$cat_model))
  ) %>%
  filter(!region == "EURO") %>%
  ggplot() +
  geom_tile(aes(x = Year, y = adm_0_name, fill = cat_model)) +
  scale_fill_manual(values = pals) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  facet_wrap2(region_facet ~ ., scales = "free_y", axes = "all")


tab_merge %>%
  mutate(
    cat_model = ifelse(
      !is.na(annual_total2) & cat_model == "No_data",
      "Disaggregation_annual",
      as.character(cat_model) # convert factor to character safely
    ),
    cat_model = factor(cat_model, levels = levels(tab_merge$cat_model)),
    cat = ifelse(
      !is.na(annual_total2) & cat == "No_data",
      "Complete_data",
      as.character(cat)
    )
  ) %>%
  group_by(cat_model, cat) %>%
  tally()

tab_merge %>%
  mutate(
    cat_model = ifelse(
      !is.na(annual_total2) & cat_model == "No_data",
      "Disaggregation_annual",
      as.character(cat_model) # convert factor to character safely
    ),
    cat_model = factor(cat_model, levels = levels(tab_merge$cat_model)),
    cat = ifelse(
      !is.na(annual_total2) & cat == "No_data",
      "Complete_data",
      as.character(cat)
    )
  ) %>%
  region_class() %>%
  group_by(region.y) %>%
  tally() %>%
  print(n = 36)

# weekly high coverage -- see if gaps can be filled in
x <- tab_merge %>%
  filter(T_res == "Week" & cat == "High_coverage" & prop > 0.98)
unique(x$country_year)
#  [1] "AFGHANISTAN_2023"       "CAMBODIA_2014"          "CAMBODIA_2018"
#  [4] "CAMBODIA_2020"          "CAMBODIA_2022"          "FRENCH POLYNESIA_2014"
#  [7] "MALAYSIA_2014"          "MALAYSIA_2020"          "NEW CALEDONIA_2018"
# [10] "NEW CALEDONIA_2019"     "NICARAGUA_2008"         "PHILIPPINES_2014"
# [13] "PHILIPPINES_2018"       "SINGAPORE_2022"         "SRI LANKA_2010"
# [16] "SRI LANKA_2011"         "SRI LANKA_2012"         "SRI LANKA_2016"
# [19] "SRI LANKA_2018"         "WALLIS AND FUTUNA_2020" "YEMEN_2016"

# [1] "COLOMBIA_2002"           "DOMINICAN REPUBLIC_2009" "DOMINICAN REPUBLIC_2010" "DOMINICAN REPUBLIC_2012"
# [5] "DOMINICAN REPUBLIC_2013" "NEW CALEDONIA_2017"      "NEW CALEDONIA_2020"      "PHILIPPINES_2022"
# [9] "SRI LANKA_2013"          "SRI LANKA_2015"          "SRI LANKA_2017"          "SRI LANKA_2019"

t <- T_data %>%
  filter(country_year %in% x$country_year)



year_range <- range(t$Year, na.rm = TRUE)
epiweek_table <- build_epiweek_lookup(start_year = year_range[1], end_year = year_range[2])

for (i in 1:nrow(t)) {
  t$epiweek[i] <- EpiWeek::dateToEpiweek(t$calendar_start_date[i])$weekno
}










range_both <- range(c(tab_subset$annual_total, tab_subset$IHME_estimate), na.rm = TRUE)

tab_merge %>%
  ggplot(aes(x = annual_total, y = IHME_estimate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(alpha = .6) +
  ggtitle("No_data countries") +
  labs(x = "Annual total in OpenDengue", y = "IHME estimate") +
  coord_fixed(ratio = 1, xlim = range_both, ylim = range_both)

p1_list <- list()
for (i in 1:length(in_IHME)) {
  x <- tab_merge %>%
    filter(ISO_A0 %in% in_IHME[i])
  country_name <- unique(x$adm_0_name)

  range_both <- range(c(x$annual_total, x$IHME_estimate), na.rm = TRUE)

  p <- x %>%
    ggplot(aes(x = annual_total, y = IHME_estimate)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(alpha = .6) +
    ggtitle(paste0(country_name)) +
    labs(x = "Annual total in OpenDengue", y = "IHME estimate") +
    coord_fixed(ratio = 1, xlim = range_both, ylim = range_both)

  p1 <- x %>%
    ggplot(aes(x = Year)) +
    geom_line(aes(y = IHME_estimate)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
    geom_point(aes(y = annual_total), color = "red") +
    ggtitle(paste0(country_name))

  print(p1)
  p1_list[[i]] <- p1
}

wrap_plots(p1_list)


for (i in 1:length(in_IHME)) {
  x <- tab_merge %>%
    filter(ISO_A0 %in% in_IHME[i])
  country_name <- unique(x$adm_0_name)

  range_both <- range(c(x$annual_total, x$IHME_calibrated), na.rm = TRUE)

  p <- x %>%
    ggplot(aes(x = annual_total, y = IHME_calibrated)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(alpha = .6) +
    ggtitle(paste0(country_name)) +
    labs(x = "Annual total in OpenDengue", y = "IHME estimate") +
    coord_fixed(ratio = 1, xlim = range_both, ylim = range_both)

  p1 <- x %>%
    ggplot(aes(x = Year)) +
    geom_line(aes(y = IHME_calibrated)) +
    geom_point(aes(y = annual_total), color = "red") +
    ggtitle(paste0(country_name))

  print(p1)
  # p1_list[[i]] <- p1
}

tab_subset <- tab_merge %>%
  filter(ISO_A0 %in% in_IHME)

metrics <- tab_subset %>%
  summarise(
    MAE = mean(abs(IHME_calibrated - annual_total), na.rm = T),
    R2 = cor(IHME_calibrated, annual_total, use = "complete.obs")^2
  )

range_both <- range(c(tab_subset$annual_total, tab_subset$IHME_calibrated), na.rm = TRUE)


tab_merge %>%
  ggplot(aes(x = annual_total, y = IHME_calibrated)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(alpha = .6) +
  geom_text(
    data = metrics,
    aes(
      x = Inf, y = -Inf,
      label = sprintf("MAE = %.1f\nR²  = %.2f", MAE, R2)
    ),
    hjust = 1.05, vjust = -0.5, colour = "black", size = 3
  ) +
  ggtitle("No_data countries") +
  labs(x = "Annual total in OpenDengue", y = "IHME calibrated estimate") +
  coord_fixed(ratio = 1, xlim = range_both, ylim = range_both)


# ---- for "Complete_data" countries ---- #

OD_complete_ctry <- unique(tab$ISO_A0[tab$cat == "Complete_data"])
in_IHME <- OD_complete_ctry[OD_complete_ctry %in% IHME_ctry]
tab_merge <- tab %>%
  left_join(., dt %>%
    filter(ISO_A0 %in% in_IHME) %>%
    select(ISO_A0, IHME_estimate, lower, upper, Year),
  by = c("ISO_A0", "Year")
  )

for (i in 1:16) {
  x <- tab_merge %>%
    filter(ISO_A0 %in% in_IHME[i])
  country_name <- unique(x$adm_0_name)

  range_both <- range(c(x$annual_total, x$IHME_estimate), na.rm = TRUE)

  p <- x %>%
    ggplot(aes(x = annual_total, y = IHME_estimate)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(alpha = .6) +
    ggtitle(paste0(country_name)) +
    labs(x = "Annual total in OpenDengue", y = "IHME estimate") +
    coord_fixed(ratio = 1, xlim = range_both, ylim = range_both)

  p1 <- x %>%
    ggplot(aes(x = Year)) +
    geom_line(aes(y = IHME_estimate)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
    geom_point(aes(y = annual_total), color = "red") +
    ggtitle(paste0(country_name))

  print(p1)
}

tab_subset <- tab_merge %>%
  filter(ISO_A0 %in% in_IHME)

range_both <- range(c(tab_subset$annual_total, tab_subset$IHME_estimate), na.rm = TRUE)

tab_merge %>%
  ggplot(aes(x = annual_total, y = IHME_estimate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(alpha = .6) +
  ggtitle("Complete_data countries") +
  labs(x = "Annual total in OpenDengue", y = "IHME estimate") +
  coord_fixed(ratio = 1, xlim = range_both, ylim = range_both)
