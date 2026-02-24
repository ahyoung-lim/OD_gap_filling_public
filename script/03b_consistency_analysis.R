# WHO DATA RECONCILATION --------------------------------
# 1. various WHO reports
who_data <- readxl::read_xlsx("data/raw_data/who_regional_summary.xlsx") %>%
  transmute(Year,
    who_region,
    cases = who_est,
    source = "WHO Reports"
  ) %>%
  filter(!(who_region == "AFRO" & Year == 2020))

# 2. Dengue explorer
de <- read.csv("data/raw_data/dengue_explorer_all_countries.csv") %>%
  transmute(
    adm_0_name = Country,
    Year,
    dengue_total = Cases,
    who_region = WHO_Region
  ) %>%
  filter(!is.na(dengue_total))

de <- de %>%
  filter(!adm_0_name %in% c("Netherlands Antilles", "Japan"))

# Add ISO codes
de$ISO_A0 <- countrycode::countrycode(
  sourcevar = de$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)

od_iso <- unique(df$ISO_A0)
de_iso <- unique(de$ISO_A0)
setdiff(de_iso, od_iso)
# [1] "NLD" "SVK" "IRL" "CAN" "GRC" "SVN" "MLT" "POL" "LUX" "HUN" "ROU" "NOR" "FIN" "PRK"
# [15] "SWE" "NZL" "ISL" "GBR" "DEU" "CZE" "BEL" "EST" "AUT" "LTU" "MNG" "LVA"

de <- de %>%
  filter(!ISO_A0 %in% setdiff(de_iso, od_iso))

unique(de$ISO_A0) # 89 countries only

de_region <- de %>%
  filter(who_region != "EURO") %>% # exclude for fair comparison
  group_by(Year, who_region) %>%
  summarise(cases = sum(dengue_total)) %>%
  transmute(Year,
    who_region,
    cases,
    source = "WHO Dengue Explorer"
  )

de_global <- de %>%
  filter(who_region != "EURO") %>% # exclude for fair comparison
  group_by(Year) %>%
  summarise(cases = sum(dengue_total)) %>%
  transmute(Year,
    who_region = "Global",
    cases,
    source = "WHO Dengue Explorer"
  )

de_summary <- bind_rows(de_region, de_global)

# 3. WHO global dashboard: https://worldhealthorg.shinyapps.io/dengue_global/
who_dash <- readxl::read_xlsx("data/raw_data/WHO-ALL-2025-Y01-00.xlsx")
# additional data extracted from WHO SEARO dashboard: https://worldhealthorg.shinyapps.io/searo-dengue-dashboard/
who_searo_ad_hoc <- read.csv("data/raw_data/SEARO_ad_hoc_2025_06_04.csv")

who_iso <- unique(who_dash$iso3)
setdiff(who_iso, od_iso)

who_dash %>%
  filter(iso3 %in% setdiff(who_iso, od_iso)) %>%
  group_by(country) %>%
  filter(sum(cases, na.rm = T) > 0)

who_dash <- who_dash %>%
  filter(!iso3 %in% setdiff(who_iso, od_iso)) %>% # remove countries with 0 cases (mostly continental europe)
  filter(!is.na(cases))

who_iso <- unique(who_dash$iso3) # 121 countries

who_dash_clean <- who_dash %>%
  # filter(who_region %in% c("SEAR", "WPR"))%>%
  transmute(
    Year = year(date),
    date = as.character(date),
    ISO_A0 = iso3,
    who_region,
    cases
  )

who_searo_clean <- who_searo_ad_hoc %>%
  transmute(
    Year,
    date = as.character(calendar_start_date),
    ISO_A0, who_region,
    cases = dengue_total
  ) %>%
  filter(!is.na(cases))

unique(who_searo_clean$ISO_A0)

who_db <- bind_rows(who_dash_clean, who_searo_clean) %>%
  group_by(Year, date, ISO_A0, who_region) %>%
  slice_max(cases, n = 1) %>%
  group_by(Year, ISO_A0, who_region) %>%
  summarise(cases = sum(cases))

who_db$cases[who_db$ISO_A0 == "LKA" & who_db$Year == 2024] <- 49877 # WHO dashboard data updated (May vs. Sep 2025)

who_region <- who_db %>%
  filter(Year < 2025) %>%
  mutate(who_region = case_when(
    who_region == "AMR" ~ "PAHO",
    who_region == "SEAR" ~ "SEARO",
    who_region == "AFR" ~ "AFRO",
    who_region == "EMR" ~ "EMRO",
    who_region == "EUR" ~ "EURO",
    who_region == "WPR" ~ "WPRO",
    TRUE ~ NA
  )) %>%
  group_by(Year, who_region) %>%
  summarise(cases = sum(cases, na.rm = T))

who_global <- who_db %>%
  filter(Year < 2025) %>%
  group_by(Year) %>%
  summarise(cases = sum(cases, na.rm = T)) %>%
  mutate(who_region = "Global")

who_db_combined <- bind_rows(who_region, who_global) %>%
  mutate(source = "WHO Dashboard")

# ALL WHO DATA SOURCES
who_combined <- bind_rows(who_data, de_summary, who_db_combined)

who_combined %>%
  filter(who_region == "Global") %>%
  ggplot(aes(x = Year, y = cases / 1e6, color = source, linetype = source)) +
  geom_line(size = 1.2) +
  geom_point(aes(color = source), size = 3)

who_combined %>%
  filter(who_region == "WPRO") %>%
  ggplot(aes(x = Year, y = cases / 1e6, color = source, linetype = source)) +
  geom_line(size = 1.2) +
  geom_point(aes(color = source), size = 3) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 2))

write.csv(who_combined, "data/processed_data/who_sources_combined.csv", row.names = F)
rm(
  who_dash, who_dash_clean, who_data, who_db_combined, who_global, who_region,
  who_searo_ad_hoc, who_searo_clean
)
