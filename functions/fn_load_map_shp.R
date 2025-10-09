library(data.table)
library(igraph)
library(tidyr)
library(dplyr)
library(spData)
library(sf)
library(spdep)
library(ggplot2)
library(rnaturalearth)
library(mapview)
library(countrycode)

# load shapefile
od_countries <- read.csv("data/processed_data/dt_heatmap_calibrated_2025_10_08.csv") %>%
  # filter(region == "PAHO") %>%
  distinct(adm_0_name)

od_countries$ISO_A0 <- countrycode(
  od_countries$adm_0_name,
  origin = "country.name",
  destination = "iso3c"
)
od_countries$ISO_A0[od_countries$adm_0_name == "SAINT MARTIN"] <- "MAF"

# geometry
map <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
  # filter(grepl("America", continent)) %>%
  select(iso_a3, brk_name) %>%
  filter(
    !brk_name %in%
      c(
        "Greenland",
        "Canada",
        "St. Pierre and Miquelon",
        "Falkland Is."
      )
  )

map$iso_a3[map$brk_name == "France"] <- "FRA"


# small French/Netherlands caribbean countries
geounit <- ne_countries(scale = 10, type = "map_units", returnclass = "sf") %>%
  filter(sovereignt %in% c("France", "Netherlands", "New Zealand")) %>%
  filter(
    !geounit %in%
      c(
        "Clipperton Island",
        "French Southern and Antarctic Lands",
        "Saint Pierre and Miquelon",
        # "Reunion",
        # "Mayotte",
        "France",
        "Netherlands"
        # "New Caledonia",
        # "French Polynesia"
      )
  ) %>%
  select(iso_a3, brk_name)

# geounit$iso_a3[geounit$brk_name == "Caribbean Netherlands"] <- "BES"
geounit$brk_name[geounit$brk_name == "Caribbean Netherlands"] <- "Bonaire, Sint Eustatius, and Saba"

# geounit$iso_a3[geounit$brk_name == "Caribbean Netherlands"] <- "BES"


map <- rbind(map, geounit[!geounit$iso_a3 %in% map$iso_a3, ])


# new_bbox <- st_bbox(
#   c(xmin = -124, ymin = -57, xmax = -30, ymax = 49),
#   crs = st_crs(map)
# )
# map <- st_crop(map, new_bbox)


mapview(map)

map[!map$brk_a3 %in% unique(od_countries$ISO_A0), ]

map <- map[map$iso_a3 %in% unique(od_countries$ISO_A0), ]

od_countries[!od_countries$ISO_A0 %in% unique(map$iso_a3), ]

# standardised country names
map <- merge(
  map %>% select(-brk_name),
  od_countries,
  by.x = "iso_a3", by.y = "ISO_A0"
)

map$adm_0_name <- factor(
  map$adm_0_name,
  levels = map$adm_0_name[order(map$adm_0_name)]
)

source("functions/fn_OD_region.R")
#
#
# map$brk_name[map$brk_name %in% americas_order == FALSE]
# map$brk_name[map$brk_name == "Saint-Martin"] <- "Saint Martin"
# map$brk_name[map$brk_name == "Dominican Rep."] <- "Dominican Republic"
# map$brk_name[map$brk_name == "Curaçao"] <- "Curacao"
# map$brk_name[map$brk_name == "Turks and Caicos Is."] <- "Turks and Caicos Islands"
# map$brk_name[ map$brk_name == "St. Vin. and Gren."] <- "Saint Vincent and the Grenadines"
# map$brk_name[map$brk_name == "Antigua and Barb."] <- "Antigua and Barbuda"
# map$brk_name[map$brk_name == "U.S. Virgin Is."] <- "Virgin Islands (US)"
# map$brk_name[map$brk_name == "British Virgin Is."] <- "Virgin Islands (UK)"
# map$brk_name[map$brk_name == "St-Barthélemy"] <- "Saint Barthelemy"
# map$brk_name[map$brk_name == "Cayman Is."] <- "Cayman Islands"
# map$brk_name[ map$brk_name == "Bonaire, Sint Eustatius, and Saba"] <- "Bonaire, Saint Eustatius and Saba"

# map$adm_0_name <- toupper(map$brk_name)

# 1) Repair, 2) compute centroid in a metric CRS, 3) transform centroid back to 4326,
# 4) extract coords, 5) bind to original data (which stays in 4326)
centroids_4326 <- map |>
  st_make_valid() |>
  st_transform(3857) |>
  st_centroid(of_largest_polygon = TRUE) |>
  st_transform(4326)

xy <- st_coordinates(centroids_4326)

map_with_centroid <- map |>
  mutate(
    Longitude = xy[, 1],
    Latitude = xy[, 2]
  )

map_with_centroid$lat_band <- cut(
  map_with_centroid$Latitude,
  breaks = c(-Inf, -15, -5, 5, 15, Inf),
  labels = c("< -15", "-15 to -5", "-5 to 5", "5 to 15", ">= 15"),
  right = FALSE
)

# update population estimates
und <- read.csv("data/un_world_pop_prospects.csv")
# https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used
# United Nations, Department of Economic and Social Affairs, Population Division (2024). World Population Prospects 2024, Online Edition. (accessed 2025-04-16)
# download "Compact xlsx" and used "Total Population, as of 1 July (thousands)"

names(und)[1] <- "Country"
names(und)[2] <- "iso3c"
names(und)[8] <- "pop_est"

und <- und %>%
  select(Country, iso3c, Year, pop_est) %>%
  filter(iso3c != "") %>%
  mutate(pop_est = as.numeric(gsub(" ", "", pop_est)) * 1000) %>%
  filter(Year > 1989)

# Duplicate 2023 data for 2024
und_2024 <- und %>%
  filter(Year == 2023) %>%
  mutate(Year = 2024)

# Combine original data with new 2024 rows
und <- bind_rows(und, und_2024)


map_final <- merge(map_with_centroid, und, by.x = "iso_a3", by.y = "iso3c", all.x = T)

rm(und, map, und_2024, map_with_centroid, xy, centroids_4326, geounit, od_countries)

#
# ggplot(map) +
#   geom_sf() +
#   geom_sf_text(aes(label = brk_name), check_overlap = TRUE) +
#   theme_bw()

# mapview(map) + mapview(map %>% st_centroid, color = "red")
