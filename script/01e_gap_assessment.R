source("script/00_setup.R") # load libraries and functions
source("functions/fn_consecutive_gap_counter.R") # counting the number of consecutive gaps
source("functions/fn_make_week_complete.R") # make weekly data complete
source("functions/fn_Year_checker.R") # adjusting Year column
source("functions/fn_OD_region.R") # regional classification
git_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/master-repo-alim/master-repo/data/releases/V1.3/"

T_data <- read.csv("data/processed_data/Best_T_data_calibrated_V1_3.csv")
summary(is.na(T_data$ISO_A0))

T_data$ISO_A0 <- countrycode::countrycode(
  T_data$adm_0_name, "country.name", "iso3c"
)

T_data$ISO_A0 <- ifelse(T_data$adm_0_name == "SAINT MARTIN", "MAF", T_data$ISO_A0)

tab <- read.csv("data/processed_data/dt_heatmap_calibrated.csv") %>%
  add_od_regions()

cols <- c(
  "adm_0_name", "ISO_A0", "calendar_start_date", "calendar_end_date",
  "Year", "dengue_total", "T_res", "region", "country_year"
)

region_order <- c(
  # Row 1: Northern/Top (West to East)
  "North & Central America", # Northwest
  "Caribbean", # West-Center
  "South America", # Southwest

  "Europe, Middle East & North Africa", # North-Center
  "South Asia", # Center
  "Sub-Saharan Africa", # South-Center

  "East & Southeast Asia", # Northeast
  # Row 2: Middle latitude (West to East)
  "Pacific Islands" # East
  # Row 3: Southern (West to East)
)

# Calculate gap lengths and spans ------------------------
# 1) What are the lengths of these consecutive gaps?
# 2) How many spans of consecutive gaps are there?

# 1) lengths of consecutive gaps + histograms
# Weekly consecutive gaps
# Extract OD data based on temporal resolution available
Week <- T_data %>%
  filter(T_res == "Week") %>%
  select(all_of(cols))

# make weekly data complete and split into country-year
Week_detailed <- Week %>%
  make_week_complete_clean() %>%
  consec_counter_detailed() %>%
  mutate(gap_id = paste0(adm_0_name, "_", run_id))

gap_summary <- Week_detailed %>%
  filter(na_values) %>%
  group_by(adm_0_name, gap_id) %>%
  summarise(
    start = min(calendar_start_date),
    end = max(calendar_start_date),
    na_lengths = first(na_lengths, na.rm = T),
    .groups = "drop"
  ) %>%
  mutate(
    gap_size = case_when(
      na_lengths < 5 ~ "small",
      na_lengths < 52 ~ "medium",
      TRUE ~ "large"
    ),
    T_res = "Week"
  )

n_gap <- nrow(Week_detailed[is.na(Week_detailed$dengue_total), ])
n_total <- nrow(Week_detailed)

n_gap / n_total * 100 # 9%

med_week_size <- median(gap_summary$na_lengths) # median of weekly gap size is 2 weeks

median(gap_summary$na_lengths[gap_summary$gap_size == "small"]) # 1 week
median(gap_summary$na_lengths[gap_summary$gap_size == "medium"]) # 14 weeks

# histogram of weekly gaps
gap_summary$gap_size <- factor(gap_summary$gap_size,
  levels = c("small", "medium", "large")
)
p1 <- ggplot(data = gap_summary) +
  geom_histogram(aes(x = na_lengths, fill = gap_size), alpha = .4, binwidth = 1) +
  geom_vline(xintercept = median(gap_summary$na_lengths), color = "red") +
  scale_fill_manual(values = c("small" = "skyblue", "medium" = "tomato", "large" = "gold")) +
  ggtitle(
    paste0("Consecutive weekly gaps"),
    subtitle = paste0("Median:", med_week_size, " weeks")
  ) +
  scale_x_continuous(
    breaks = seq(0, max(gap_summary$na_lengths), by = 2),
    labels = seq(0, max(gap_summary$na_lengths), by = 2)
  )
p1


gap_summary %>%
  group_by(gap_size) %>%
  tally()
# gap_size     n
#   <chr>    <int>
# 1 large        8
# 2 medium     114
# 3 small      540

# Monthly consecutive gaps -------------------------------

Month <- T_data %>%
  filter(T_res == "Month") %>%
  select(all_of(cols))


# make monthly data complete and split into country-year
Month_clean <- Month %>%
  # rbind(., Week_to_Month) %>%
  make_month_complete_clean()

Month_clean %>%
  group_by(adm_0_name, calendar_start_date) %>%
  filter(n() > 1)

Month_detailed <- Month_clean %>%
  consec_counter_detailed() %>%
  mutate(gap_id = paste0(adm_0_name, "_", run_id))

gap_summary_month <- Month_detailed %>%
  filter(na_values) %>%
  group_by(adm_0_name, gap_id) %>%
  summarise(
    start = min(calendar_start_date),
    end = max(calendar_start_date),
    na_lengths = first(na_lengths, na.rm = T),
    .groups = "drop"
  ) %>%
  mutate(
    gap_size = case_when(
      na_lengths < 2 ~ "small",
      na_lengths >= 2 & na_lengths < 12 ~ "medium",
      na_lengths >= 12 ~ "large"
    ),
    T_res = "Month"
  )


med_month_size <- median(gap_summary_month$na_lengths) # 3.5 months

gap_summary_month$gap_size <- factor(gap_summary_month$gap_size,
  levels = c("small", "medium", "large")
)
p2 <- ggplot(data = gap_summary_month) +
  geom_histogram(aes(x = na_lengths, fill = gap_size), alpha = .4, binwidth = 1) +
  geom_vline(xintercept = median(gap_summary_month$na_lengths), color = "red") +
  ggtitle(
    paste0("Consecutive monthly gaps"),
    subtitle = paste0("Median:", med_month_size, " months")
  ) +
  scale_fill_manual(values = c("small" = "skyblue", "medium" = "tomato", "large" = "gold")) +
  scale_x_continuous(
    breaks = seq(0, max(gap_summary_month$na_lengths), by = 1),
    labels = seq(0, max(gap_summary_month$na_lengths), by = 1)
  )
p2

gap_summary_month %>%
  group_by(gap_size) %>%
  tally()

#   gap_size     n
#   <fct>    <int>
# 1 small        8
# 2 medium      34

# No more yearly consecutive gaps --
# large gap = cys with incomplete subannual case counts
# count the number of years that we have both subannual and annaul
# or count the number of years that we only know annual

# Usage per country
Year_clean <- tab %>%
  select(adm_0_name, Year, cat_model) %>%
  mutate(dengue_total = if_else(cat_model == "Sub_annual_imputation", "smaller_gaps",
    if_else(cat_model == "Annual_disaggregation", NA, "No_modelling_required")
  ))

gap_summary_year <- Year_clean %>%
  group_by(adm_0_name) %>%
  group_modify(~ calc_na_lengths_tidy(.x)) %>%
  ungroup() %>%
  filter(na_values)

Year_clean %>%
  filter(is.na(dengue_total)) %>%
  nrow() # 1483 (annual disaggregation + calibrated annual totals )

med_year_size <- median(gap_summary_year$na_lengths) # 2 years

# histogram of yearly gap lengths
p3 <- ggplot(data = gap_summary_year) +
  geom_histogram(aes(x = na_lengths), alpha = .4, binwidth = 1) +
  geom_vline(xintercept = med_year_size, color = "red") +
  ggtitle(
    paste0("Consecutive yearly gaps"),
    subtitle = paste0("Median:", med_year_size, " years")
  ) +
  scale_x_continuous(
    breaks = seq(0, max(gap_summary_year$na_lengths), by = 1),
    labels = seq(0, max(gap_summary_year$na_lengths), by = 1)
  )
p3


# merge all histograms
p1 + p2 + p3 + plot_layout(ncol = 1)

# 2) count the number of spans (chunks) of consecutive gaps -----

# Combine gaps by size categories
small_gap <- rbind(
  gap_summary %>% filter(gap_size == "small"),
  gap_summary_month %>% filter(gap_size == "small")
)

medium_gap <- rbind(
  gap_summary %>% filter(gap_size == "medium"),
  gap_summary_month %>% filter(gap_size == "medium")
)


large_gap <- rbind(
  gap_summary %>% filter(gap_size == "large") %>% select(adm_0_name, gap_size, T_res),
  gap_summary_month %>% filter(gap_size == "large") %>% select(adm_0_name, gap_size, T_res),
  gap_summary_year %>% mutate(gap_size = "large", T_res = "Year") %>% select(adm_0_name, gap_size, T_res)
)


# Classify by region
small_gap <- region_class(small_gap)
medium_gap <- region_class(medium_gap)
large_gap <- region_class(large_gap)

# Combine all gaps and add gap_size factor
small_gap_sub <- small_gap %>%
  select(gap_size, T_res, region)

medium_gap_sub <- medium_gap %>%
  select(gap_size, T_res, region)

large_gap_sub <- large_gap %>%
  select(gap_size, T_res, region)

all_gaps <- bind_rows(small_gap_sub, medium_gap_sub, large_gap_sub)

# Factor levels as before
all_gaps <- all_gaps %>%
  mutate(
    gap_size = factor(gap_size, levels = c("small", "medium", "large")),
    T_res = factor(T_res, levels = c("Week", "Month", "Year"))
    # region2 = case_when(
    #   region == "PAHO" ~ "Americas",
    #   region %in% c("SEARO", "WPRO") ~ "Asia",
    #   TRUE ~ as.character(region) # keep other values as is (if any)
    # ) %>% factor() # convert to factor if you want
  )


makeTable <- function(data, row_var, col_var) {
  row_var <- deparse(substitute(row_var))
  col_var <- deparse(substitute(col_var))

  # Create table and add margins
  tab <- table(data[[row_var]], data[[col_var]])
  tab_m <- addmargins(tab)

  total <- sum(tab)
  prop <- tab_m / total * 100
  prop_fmt <- sprintf("%.1f", round(prop, 1))

  # Combine count and percent
  combined <- matrix(
    ifelse(tab_m == 0, "0 (0.0%)", paste0(tab_m, " (", prop_fmt, "%)")),
    nrow = nrow(tab_m),
    ncol = ncol(tab_m),
    dimnames = dimnames(tab_m)
  )

  # Convert to data.frame and preserve row names
  df <- as.data.frame(combined, stringsAsFactors = FALSE)
  df <- cbind(rownames(df), df)
  colnames(df)[1] <- row_var # Rename first column to row_var

  return(df)
}


# Usage
makeTable(all_gaps, region, gap_size)
makeTable(all_gaps, T_res, gap_size)






week_gap_position <- Week_detailed %>%
  arrange(adm_0_name, time_seq) %>%
  group_by(adm_0_name) %>%
  mutate(
    is_obs = !is.na(dengue_total),

    # has any observation been seen so far (reading left→right)?
    any_seen_left = cummax(is_obs),

    # has any observation been seen when reading right→left?
    any_seen_rgt = rev(cummax(rev(is_obs))),
    gap_type = case_when(
      is_obs ~ "observed",
      !any_seen_left ~ "left_edge_gap", # before first non-missing
      !any_seen_rgt ~ "right_edge_gap", # after last  non-missing
      TRUE ~ "internal_gap" # between two non-missings
    )
  ) %>%
  ungroup() %>%
  mutate(
    gap_size = case_when(
      na_lengths < 5 ~ "small",
      na_lengths < 52 ~ "medium",
      TRUE ~ "large"
    )
  )

week_gap_position$gap_size <- factor(week_gap_position$gap_size,
  levels = c("small", "medium", "large")
)

week_gap_position %>%
  distinct(gap_id, gap_type) %>%
  group_by(gap_type) %>%
  tally()

#   gap_type           n
#   <chr>          <int>
# 1 internal_gap     632
# 2 left_edge_gap     17
# 3 observed         728
# 4 right_edge_gap    13

week_gap_position %>%
  distinct(gap_id, gap_type) %>%
  filter(gap_type != "observed") %>%
  nrow()

nrow(all_gaps[all_gaps$T_res == "Week", ])


# detailed view
week_gap_position %>%
  add_od_regions() %>%
  distinct(gap_id, gap_type, gap_size, od_region) %>%
  mutate(gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type)) %>%
  group_by(gap_size, gap_type, od_region) %>%
  tally() %>%
  ungroup() %>%
  complete(gap_size, gap_type, od_region, fill = list(n = 0)) %>%
  print(n = 55)

week_gap_position %>%
  add_od_regions() %>%
  distinct(gap_id, gap_type, gap_size, od_region) %>%
  mutate(
    gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type),
    size_type = paste(gap_size, gap_type, sep = "_")
  ) %>%
  filter(gap_type != "observed") %>%
  group_by(od_region, size_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = size_type,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(od_region)


month_gap_position <- Month_detailed %>%
  arrange(adm_0_name, time_seq) %>%
  group_by(adm_0_name) %>%
  mutate(
    is_obs = !is.na(dengue_total),

    # has any observation been seen so far (reading left→right)?
    any_seen_left = cummax(is_obs),

    # has any observation been seen when reading right→left?
    any_seen_rgt = rev(cummax(rev(is_obs))),
    gap_type = case_when(
      is_obs ~ "observed",
      !any_seen_left ~ "left_edge_gap", # before first non-missing
      !any_seen_rgt ~ "right_edge_gap", # after last  non-missing
      TRUE ~ "internal_gap" # between two non-missings
    )
  ) %>%
  ungroup() %>%
  mutate(
    gap_size = case_when(
      na_lengths < 2 ~ "small",
      na_lengths >= 2 & na_lengths < 12 ~ "medium",
      na_lengths >= 12 ~ "large"
    )
  )

month_gap_position$gap_size <- factor(month_gap_position$gap_size,
  levels = c("small", "medium", "large")
)

month_gap_position %>%
  add_od_regions() %>%
  distinct(gap_id, gap_type, gap_size, od_region) %>%
  mutate(gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type)) %>%
  group_by(gap_size, gap_type, od_region) %>%
  tally() %>%
  ungroup() %>%
  complete(gap_size, gap_type, od_region, fill = list(n = 0)) %>%
  print(n = 55)

month_gap_position %>%
  distinct(gap_id, gap_type) %>%
  group_by(gap_type) %>%
  tally()
# filter(gap_type !="observed")%>%

#   gap_type           n
#   <chr>          <int>
# 1 internal_gap       4
# 2 left_edge_gap      5
# 3 observed          79
# 4 right_edge_gap    19

month_gap_position %>%
  distinct(gap_id, gap_type) %>%
  filter(gap_type != "observed") %>%
  nrow()

nrow(all_gaps[all_gaps$T_res == "Month", ])

month_gap_position %>%
  add_od_regions() %>%
  mutate(gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type)) %>%
  group_by(gap_type, gap_size, od_region) %>%
  summarise(median = median(na_lengths, na.rm = T)) %>%
  print(n = 30)


Year_detailed <- Year_clean %>%
  arrange(adm_0_name, Year) %>% # make sure years are ordered
  group_by(adm_0_name) %>% # streaks are country-specific
  group_modify(~ {
    rle_na <- rle(is.na(.x$dengue_total))
    # Expand rle result to match row count
    expanded <- tibble(
      na_lengths = rep(rle_na$lengths, rle_na$lengths),
      na_values = rep(rle_na$values, rle_na$lengths),
      run_id = rep(seq_along(rle_na$lengths), rle_na$lengths)
    )
    # Only keep gap_id if it's a NA run
    expanded <- expanded %>%
      mutate(
        gap_id = paste0(.y$adm_0_name, "_", run_id)
      )
    bind_cols(.x, expanded)
  }) %>%
  ungroup()

year_gap_position <- Year_detailed %>%
  group_by(adm_0_name) %>%
  mutate(
    is_obs = !is.na(dengue_total),

    # has any observation been seen so far (reading left→right)?
    any_seen_left = cummax(is_obs),

    # has any observation been seen when reading right→left?
    any_seen_rgt = rev(cummax(rev(is_obs))),
    gap_type = case_when(
      is_obs ~ "observed",
      !any_seen_left ~ "left_edge_gap", # before first non-missing
      !any_seen_rgt ~ "right_edge_gap", # after last  non-missing
      TRUE ~ "internal_gap" # between two non-missings
    )
  ) %>%
  ungroup()

year_gap_position %>%
  distinct(gap_id, gap_type) %>%
  group_by(gap_type) %>%
  tally()

#   gap_type           n
#   <chr>          <int>
# 1 internal_gap     332
# 2 left_edge_gap     52
# 3 observed         475
# 4 right_edge_gap    14

year_gap_position %>%
  distinct(gap_id, gap_type) %>%
  filter(gap_type != "observed") %>%
  nrow()

nrow(all_gaps[all_gaps$T_res == "Year", ])

year_gap_position <- year_gap_position %>%
  mutate(ISO_A0 = countrycode::countrycode(adm_0_name, "country.name", "iso3c")) %>%
  mutate(ISO_A0 = case_when(
    adm_0_name == "SAINT MARTIN" ~ "MAF",
    TRUE ~ ISO_A0
  ))


year_gap_position %>%
  add_od_regions() %>%
  distinct(gap_id, gap_type, od_region) %>%
  mutate(gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type)) %>%
  group_by(gap_type, od_region) %>%
  tally() %>%
  ungroup() %>%
  complete(gap_type, od_region, fill = list(n = 0)) %>%
  print(n = 45)

year_gap_position %>%
  add_od_regions() %>%
  # mutate(gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type))%>%
  group_by(gap_type, od_region) %>%
  summarise(median = median(na_lengths, na.rm = T))


all_gap_position <- rbind(
  week_gap_position %>%
    filter(!is_obs) %>%
    add_od_regions() %>%
    distinct(gap_id, gap_type, gap_size, na_lengths, od_region) %>%
    mutate(T_res = "Week"),
  month_gap_position %>%
    filter(!is_obs) %>%
    add_od_regions() %>%
    distinct(gap_id, gap_type, gap_size, na_lengths, od_region) %>%
    mutate(T_res = "Month"),
  year_gap_position %>%
    filter(!is_obs) %>%
    add_od_regions() %>%
    distinct(gap_id, gap_type, na_lengths, od_region) %>%
    mutate(
      gap_size = "large",
      T_res = "Year"
    ) %>%
    select(gap_id, gap_type, gap_size, na_lengths, od_region, T_res)
)

tbl <- all_gap_position %>%
  mutate(na_lengths_std = case_when(
    T_res == "Week" ~ na_lengths / 4.3,
    T_res == "Year" ~ na_lengths * 12,
    TRUE ~ na_lengths
  )) %>%
  mutate(
    gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type),
    size_type = paste(gap_size, gap_type, sep = "_")
  ) %>%
  filter(gap_type != "observed") %>%
  group_by(od_region, size_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = size_type,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(od_region)

names(tbl)

tbl$od_region <- factor(tbl$od_region, levels = region_order)
tbl <- tbl %>%
  select(
    od_region,
    small_internal_gap, small_Edge,
    medium_internal_gap, medium_Edge,
    large_internal_gap, large_Edge
  ) %>%
  arrange(od_region) %>%
  ## row-wise totals
  mutate(region_total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  ## add grand-total row
  bind_rows(
    summarise(.,
      od_region = "Total",
      across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
    )
  )

tbl %>% print(n = Inf, width = Inf)

write.csv(tbl, "output/tables/table_number_of_consec_gaps.csv", row.names = F)

tbl2 <- all_gap_position %>%
  mutate(na_lengths_std = case_when(
    T_res == "Week" ~ na_lengths / 4.33,
    T_res == "Year" ~ na_lengths * 12,
    TRUE ~ na_lengths
  )) %>%
  mutate(
    gap_type = ifelse(grepl("edge", gap_type), "Edge", gap_type),
    size_type = paste(gap_size, gap_type, sep = "_")
  ) %>%
  filter(gap_type != "observed") %>%
  # filter(region == "SEARO" & size_type == "large_internal_gap")
  group_by(od_region, size_type) %>%
  summarise(median_lengths = round(median(na_lengths_std, na.rm = T), 1), .groups = "drop") %>%
  pivot_wider(
    names_from = size_type,
    values_from = median_lengths,
    values_fill = NA
  )

tbl2$od_region <- factor(tbl2$od_region, levels = region_order)
tbl2 <- tbl2 %>%
  select(
    od_region,
    small_internal_gap, small_Edge,
    medium_internal_gap, medium_Edge,
    large_internal_gap, large_Edge
  ) %>%
  arrange(od_region) %>%
  ## row-wise totals
  rowwise() %>%
  mutate(region_median = median(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  ## add grand-total row
  bind_rows(
    summarise(.,
      od_region = "Total",
      across(where(is.numeric), \(x) median(x, na.rm = TRUE))
    )
  )

tbl2 %>% print(n = Inf, width = Inf)

write.csv(tbl2, "output/tables/table_length_consec_gaps.csv", row.names = F)


p4 <- small_gap %>%
  group_by(adm_0_name) %>%
  tally() %>%
  ggplot() +
  geom_histogram(aes(x = n), fill = "skyblue", alpha = .4, binwidth = 1) +
  ggtitle(
    "Number of small gaps per country",
    subtitle = paste0(
      "Global number of small gaps (1-4 weeks): ",
      nrow(small_gap)
    )
  ) +
  xlab("Number of gaps") +
  scale_x_continuous(breaks = seq(0, 50, by = 2))
p4

p5 <- medium_gap %>%
  group_by(adm_0_name) %>%
  tally() %>%
  ggplot() +
  geom_histogram(aes(x = n), fill = "tomato", alpha = .4, binwidth = 1) +
  ggtitle(
    "Number of medium gaps per country",
    subtitle = paste0(
      "Global number of medium gaps (< 1 yr): ",
      nrow(medium_gap)
    )
  ) +
  xlab("Number of gaps") +
  scale_x_continuous(breaks = seq(0, 20, by = 1))
p5

p6 <- large_gap %>%
  group_by(adm_0_name) %>%
  tally() %>%
  ggplot() +
  geom_histogram(aes(x = n), fill = "gold", alpha = .4, binwidth = 1) +
  ggtitle(
    "Number of large gaps per country",
    subtitle = paste0(
      "Global number of large gaps (>= 1 yr): ",
      nrow(large_gap)
    )
  ) +
  xlab("Number of gaps") +
  scale_x_continuous(breaks = seq(0, 20, by = 1))
p6

p4 + p5 + p6 + plot_layout(ncol = 1)
