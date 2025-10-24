library(countrycode)

# WHO region ========================================
region_class <- function(data, upper = T) {
  who_regions <- list(
    PAHO = c(
      "Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "Bahamas",
      "Barbados", "Belize", "Bermuda", "Bolivia", "Bonaire, Saint Eustatius and Saba", "Brazil",
      "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominica",
      "Dominican Republic", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guadeloupe",
      "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique", "Mexico", "Montserrat",
      "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Saint Barthelemy",
      "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines",
      "Sint Maarten", "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands",
      "United States of America", "Uruguay", "Venezuela", "Virgin Islands (UK)", "Virgin Islands (US)"
    ),
    WPRO = c(
      "American Samoa", "Australia", "Brunei Darussalam", "Cambodia", "China", "Cook Islands", "Fiji",
      "French Polynesia", "Guam", "Hong Kong", "Japan", "Kiribati", "Lao People's Democratic Republic",
      "Macau", "Malaysia", "Marshall Islands", "Micronesia (Federated States of)", "Nauru",
      "New Caledonia", "Niue", "Northern Mariana Islands", "Palau",
      "Papua New Guinea", "Philippines", "Pitcairn", "Republic of Korea", "Samoa", "Singapore",
      "Solomon Islands", "Taiwan", "Tokelau", "Tonga", "Tuvalu",
      "Vanuatu", "Viet Nam", "Wallis and Futuna"
    ),
    SEARO = c(
      "Bangladesh", "Bhutan", "India", "Indonesia", "Maldives", "Myanmar",
      "Nepal", "Sri Lanka", "Thailand", "Timor-Leste"
    ),
    EMRO = c(
      "Afghanistan", "Pakistan", "Saudi Arabia", "Oman", "Yemen", "Sudan",
      "Djibouti", "Egypt", "Somalia", "Iran (Islamic Republic of)"
    ),
    AFRO = c(
      "Angola", "Benin", "Burkina Faso", "Cabo Verde", "Cameroon",
      "Central African Republic", "Chad", "Cote D'ivoire", "Eritrea", "Ethiopia",
      "Ghana", "Guinea", "Kenya", "Mali", "Mauritania", "Mauritius", "Mayotte",
      "Niger", "Reunion", "Sao Tome and Principe", "Senegal", "Seychelles",
      "Togo", "United Republic of Tanzania",
      "Comoros", "Democratic Republic of Congo", "Gabon", "Madagascar",
      "Mozambique", "Nigeria", "South Sudan",
      "Burundi", "Congo", "Equatorial Guinea", "Gambia",
      "Guinea-Bissau", "Liberia", "Malawi", "Namibia",
      "Rwanda", "Sierra Leone", "Uganda", "Zambia", "Zimbabwe"
    ),
    EURO = c(
      "France", "Italy", "Spain", "Croatia", "Portugal"
    )
  )

  # Create WHO region lookup
  who_region_lookup <- do.call(rbind, lapply(names(who_regions), function(region) {
    data.frame(
      adm_0_name = toupper(who_regions[[region]]),
      region = region,
      stringsAsFactors = FALSE
    )
  }))


  # Merge with main dataset
  data <- data %>%
    left_join(who_region_lookup, by = "adm_0_name")

  return(data)
}


# for spatial matrix
americas_order <- c(
  # North America
  "United States of America",
  # Central America
  "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama",

  # South America
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
  "Paraguay", "Peru", "Uruguay", "Venezuela",

  # Caribbean
  "Anguilla", "Antigua and Barbuda", "Aruba",
  "Bahamas", "Barbados", "Bermuda", "Bonaire, Saint Eustatius and Saba",
  "Cayman Islands", "Cuba", "Curacao",
  "Dominica", "Dominican Republic",
  "French Guiana",
  "Grenada", "Guadeloupe", "Guyana",
  "Haiti",
  "Jamaica",
  "Martinique", "Montserrat",
  "Puerto Rico",
  "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sint Maarten", "Suriname",
  "Trinidad and Tobago", "Turks and Caicos Islands",
  "Virgin Islands (UK)", "Virgin Islands (US)"
)


# NEW OD region ============================================

# Function 1: Returns a simple table of ISO3 codes and regions
get_od_regions <- function(iso3_vector) {
  # Ensure it's a plain vector and get unique values only
  iso3_vector <- unique(as.character(unname(iso3_vector)))

  # Define country groups
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

  df <- data.frame(ISO_A0 = iso3_vector) %>%
    mutate(
      un_region = suppressWarnings(
        countrycode(ISO_A0, origin = "iso3c", destination = "un.region.name")
      ),
      un_subregion = suppressWarnings(
        countrycode(ISO_A0, origin = "iso3c", destination = "un.regionsub.name")
      ),
      country_name = countrycode(ISO_A0, origin = "iso3c", destination = "country.name"),
      od_region = case_when(
        # Manual cases
        ISO_A0 == "TWN" ~ "East & Southeast Asia",
        ISO_A0 %in% c("MYT", "REU") ~ "Sub-Saharan Africa",
        ISO_A0 == "WLF" ~ "Pacific Islands",
        # South America
        ISO_A0 %in% south_america ~ "South America",
        # Central America & Mexico
        ISO_A0 %in% central_america ~ "North & Central America",
        ISO_A0 == "USA" ~ "North & Central America",
        # Caribbean
        ISO_A0 %in% caribbean ~ "Caribbean",
        # East & Southeast Asia
        un_subregion %in% c("Eastern Asia", "South-eastern Asia") ~ "East & Southeast Asia",
        ISO_A0 %in% se_asia ~ "East & Southeast Asia",
        # South Asia
        un_subregion == "Southern Asia" ~ "South Asia",
        # Pacific Islands
        un_subregion %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands",
        ISO_A0 %in% c("AUS", "NZL") ~ "Pacific Islands",
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
    ) %>%
    select(ISO_A0, country_name, od_region)

  return(df)
}

# Function 2: Adds region column to existing dataframe
add_od_regions <- function(df, iso_col = "ISO_A0") {
  # Get the ISO codes from the dataframe
  iso_codes <- df[[iso_col]]

  # Get region classifications
  regions <- get_od_regions(iso_codes)

  # Rename to match input column name for joining
  names(regions)[1] <- iso_col

  # Join back to original dataframe
  result <- df %>%
    left_join(regions, by = iso_col) %>%
    select(-country_name)

  return(result)
}


# library(dplyr)
# library(rnaturalearth)
# library(sf)
# library(ggplot2)
# library(mapview)
# library(patchwork)
# library(ggrepel)

# od <- read.csv("data/processed_data/Best_T_data_calibrated_V1_3_2025_10_08.csv")

# load in all countries included in OD gap-filled
# od_iso <- unique(od$ISO_A0[!is.na(od$ISO_A0)])

# region_table <- get_od_regions(od_iso)
# print(region_table)
#
#
# # Check what's in "Other"
# region_table %>%
#   filter(od_region == "Other")
#
# # Check distribution
# table(region_table$od_region)
#
# # example of function 2
# od %>%
#   slice_head(n = 50) %>%
#   add_od_regions()


# OD region system visualisations -------------------------
# geometry
# map <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
#   select(iso_a3, brk_name) %>%
#   filter(
#     !brk_name %in%
#       c(
#         "Greenland",
#         "St. Pierre and Miquelon",
#         "Falkland Is.",
#         "Antarctica"
#       )
#   )
#
# map$iso_a3[map$brk_name == "France"] <- "FRA"
#
#
# # small French/Netherlands caribbean countries
# geounit <- ne_countries(scale = 10, type = "map_units", returnclass = "sf") %>%
#   filter(sovereignt %in% c("France", "Netherlands", "New Zealand")) %>%
#   filter(
#     !geounit %in%
#       c(
#         "Clipperton Island",
#         "French Southern and Antarctic Lands",
#         "Saint Pierre and Miquelon",
#         "France",
#         "Netherlands"
#       )
#   ) %>%
#   select(iso_a3, brk_name)
#
# geounit$brk_name[geounit$brk_name == "Caribbean Netherlands"] <- "Bonaire, Sint Eustatius, and Saba"
#
# geounit <- geounit[!geounit$iso_a3 %in% map$iso_a3, ]
#
# map <- rbind(map, geounit[!geounit$iso_a3 %in% map$iso_a3, ])
#
# # standardised country names
# map <- merge(
#   map %>% select(-brk_name),
#   region_table,
#   by.x = "iso_a3", by.y = "ISO_A0",
#   all = T
# )
#
#
# map <- map %>%
#   mutate(
#     od_region = factor(od_region, levels = c(
#       # Americas (red → orange → yellow)
#       "South America",
#       "Central America & Mexico",
#       "Caribbean",
#       # Asia-Pacific (yellow-green → green → cyan → blue)
#       "East & Southeast Asia",
#       "South Asia",
#       "Pacific Islands",
#       # Africa-Europe (dark blue → purple)
#       "Sub-Saharan Africa",
#       "Europe, Middle East & North Africa"
#     ))
#   )
#
# # interactive view
# mapview(map, zcol = "od_region")
#
# # static world map
# world_plot <- ggplot(map) +
#   geom_sf(aes(fill = od_region), color = "grey60", size = 0.1) +
#   scale_fill_brewer(palette = "Spectral", name = "OD Region", na.value = "grey90") +
#   coord_sf(expand = FALSE) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
#
# ggsave("output/figures/OD_region_world_map.png", world_plot,
#        width = 12, height = 8, dpi = 300, bg = "white")

# # Define small island nations
#
# indian_ocean <- c("MUS", "SYC", "COM", "REU", "MYT")
# caribbean_islands <- region_table$ISO_A0[region_table$od_region == "Caribbean"]
#
#
#
# # Define island groups with bounding boxes
# island_groups <- list(
#   `Pacific West` = list(
#     countries = c(
#       # Melanesia
#       "PNG", "SLB", "VUT", "FJI", "NCL",
#       # Micronesia
#       "PLW", "FSM", "GUM", "MNP", "MHL", "NRU",
#       # Western Kiribati
#       "KIR",
#       # Australia (if you want it)
#       "AUS"
#     ),
#     xlim = c(130, 180),
#     ylim = c(-25, 20)
#   ),
#   `Pacific East` = list(
#     countries = c(
#       # Polynesia
#       "WSM", "TON", "TUV", "WLF", "NIU", "COK",
#       "PYF", "TKL", "ASM"
#       # Note: Kiribati spans both, but putting in West
#     ),
#     xlim = c(-180, -130),
#     ylim = c(-25, 5)
#   ),
#   `Indian Ocean` = list(
#     countries = indian_ocean,
#     xlim = c(40, 60),
#     ylim = c(-25, 0)
#   ),
#   `Caribbean` = list(
#     countries = caribbean_islands,
#     xlim = c(-85, -59),
#     ylim = c(10, 27)
#   )
# )
#
#
# # Function to create labeled map with OD region colors
# create_labeled_island_map <- function(group_name, group_info, world_data) {
#   # Filter islands in this group
#   islands <- world_data %>%
#     filter(iso_a3 %in% group_info$countries)
#
#   # Get centroids for labels
#   island_labels <- islands %>%
#     st_centroid() %>%
#     suppressWarnings() %>% # suppress warnings for lat/long centroids
#     st_coordinates() %>%
#     as.data.frame() %>%
#     bind_cols(islands %>% st_drop_geometry() %>% select(country_name, iso_a3, od_region))
#
#   # Create map
#   p <- ggplot() +
#     # Background countries in light gray
#     geom_sf(data = world_data, fill = "gray95", color = "white", size = 0.1) +
#     # Highlighted islands colored by OD region
#     geom_sf(data = islands, aes(fill = od_region), color = "black", size = 0.5) +
#     # Add labels with ggrepel for better placement
#     geom_text_repel(
#       data = island_labels,
#       aes(x = X, y = Y, label = country_name),
#       size = 2.5,
#       fontface = "bold",
#       box.padding = 0.5,
#       point.padding = 0.3,
#       segment.size = 0.1,
#       segment.alpha = 0.5,  # Make segments semi-transparent
#       max.overlaps = 20,
#       min.segment.length = 0
#     ) +
#     # Add points at centroids
#     geom_point(
#       data = island_labels,
#       aes(x = X, y = Y),
#       size = 1, color = "black"
#     ) +
#     scale_fill_brewer(
#       palette = "Spectral", name = "OD Region",
#       na.value = "grey90", drop = FALSE
#     ) +
#     coord_sf(xlim = group_info$xlim, ylim = group_info$ylim, expand = TRUE) +
#     labs(
#       title = group_name,
#       subtitle = paste(nrow(islands), "islands")
#     ) +
#     xlab(NULL)+ylab(NULL)+
#     theme_minimal() +
#     theme(
#       plot.title = element_text(size = 12, face = "bold"),
#       plot.subtitle = element_text(size = 9),
#       legend.position = "bottom",
#       legend.title = element_text(size = 9, face = "bold"),
#       legend.text = element_text(size = 8),
#       panel.grid = element_line(color = "gray90", size = 0.2),
#       axis.text = element_text(size = 7)
#     )
#
#   return(p)
# }
#
# # Create all maps
# maps <- lapply(names(island_groups), function(name) {
#   create_labeled_island_map(name, island_groups[[name]], map)
# })
#
# # maps[[1]]
#
# # Combine into grid
# combined_plot <- wrap_plots(maps, ncol = 2) &
#   theme(legend.position = "none")
#
# print(combined_plot)
#
# ggsave("output/figures/OD_region_island_nations.png", combined_plot,
#   width = 12, height = 8, dpi = 300, bg = "white"
# )
