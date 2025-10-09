library(dplyr)
library(countrycode)
library(rnaturalearth)
library(sf)
library(ggplot2)

od <- read.csv("data/processed_data/Best_T_data_calibrated_V1_3_2025_10_08.csv")

od_iso <- od %>%
  filter(!is.na(ISO_A0)) %>%
  distinct(ISO_A0) %>%
  pull()



classify_regions <- function(iso3_vector) {
  # Define country groups manually
  south_america <- c(
    "ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUF",
    "GUY", "PRY", "PER", "SUR", "URY", "VEN"
  )

  central_america <- c("BLZ", "CRI", "SLV", "GTM", "HND", "MEX", "NIC", "PAN")

  caribbean <- c(
    "AIA", "ATG", "ABW", "BHS", "BRB", "BES", "BMU", "VGB",
    "CYM", "CUB", "CUW", "DMA", "DOM", "GRD", "GLP", "HTI",
    "JAM", "MTQ", "MSR", "PRI", "BLM", "KNA", "LCA", "MAF",
    "VCT", "SXM", "TTO", "TCA", "VIR"
  )

  se_asia <- c(
    "BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP",
    "THA", "TLS", "VNM"
  )

  df <- data.frame(iso3c = iso3_vector) %>%
    mutate(
      un_region = countrycode(iso3c, origin = "iso3c", destination = "un.region.name"),
      un_subregion = countrycode(iso3c, origin = "iso3c", destination = "un.regionsub.name"),
      continent = countrycode(iso3c, origin = "iso3c", destination = "continent"),
      country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
      consolidated_region = case_when(
        # Manual cases
        iso3c == "TWN" ~ "East & Southeast Asia",
        iso3c %in% c("MYT", "REU") ~ "Sub-Saharan Africa",
        iso3c == "WLF" ~ "Pacific Islands",

        # South America
        iso3c %in% south_america ~ "South America",

        # Central America & Mexico
        iso3c %in% central_america ~ "North & Central America",
        iso3c == "USA" ~ "North & Central America",

        # Caribbean
        iso3c %in% caribbean ~ "Caribbean",

        # East & Southeast Asia (note the hyphen in "South-eastern")
        un_subregion %in% c("Eastern Asia", "South-eastern Asia") ~ "East & Southeast Asia",
        iso3c %in% se_asia ~ "East & Southeast Asia",

        # South Asia
        un_subregion == "Southern Asia" ~ "South Asia",

        # Pacific Islands
        un_subregion %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands",
        iso3c %in% c("AUS", "NZL") ~ "Pacific Islands",

        # Sub-Saharan Africa
        un_subregion %in% c(
          "Sub-Saharan Africa", "Western Africa", "Eastern Africa",
          "Middle Africa", "Southern Africa"
        ) ~ "Sub-Saharan Africa",

        # Europe, Middle East & North Africa
        un_region == "Europe" ~ "Europe, Middle East & North Africa",
        un_subregion %in% c("Western Asia", "Northern Africa") ~ "Europe, Middle East & North Africa",
        TRUE ~ "Other"
      )
    )

  return(df)
}


# Test
result <- classify_regions(od_iso)

# Check what's in "Other"
result %>%
  filter(consolidated_region == "Other") %>%
  select(iso3c, country_name, un_region, un_subregion)

# Check distribution
table(result$consolidated_region)



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry)

# Keep only iso + band from your table
categories <- result %>%
  select(iso3c, consolidated_region) %>%
  rename(iso_a3 = iso3c)

world_region <- world %>%
  left_join(categories, by = "iso_a3")

mapview(world)
mapview(world_region, zcol = "consolidated_region")








# geometry
map <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
  # filter(grepl("America", continent)) %>%
  select(iso_a3, brk_name) %>%
  filter(
    !brk_name %in%
      c(
        "Greenland",
        "St. Pierre and Miquelon",
        "Falkland Is.",
        "Antarctica"
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

geounit$brk_name[geounit$brk_name == "Caribbean Netherlands"] <- "Bonaire, Sint Eustatius, and Saba"


map <- rbind(map, geounit[!geounit$iso_a3 %in% map$iso_a3, ])

# standardised country names
map <- merge(
  map %>% select(-brk_name),
  result,
  by.x = "iso_a3", by.y = "iso3c",
  all = T
)

mapview(map, zcol = "consolidated_region")
ggplot(map) +
  geom_sf(aes(fill = consolidated_region), color = "grey60", size = 0.1) +
  scale_fill_brewer(palette = "Spectral", name = "OD region", na.value = "grey90") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom")
