# country order ========================================
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
