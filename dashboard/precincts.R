# google sheet of data manually made using:
# 1. https://www.gpsvisualizer.com/geocoder/
# 2. Data Description from Allegations Data
# 3. https://www1.nyc.gov/site/nypd/bureaus/patrol/precincts-landing.page

coordinates <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ8xw99f6gzPHNvINFt5THA4NrungmyCxGhKAUiRF57q7o1anelmhcgFu-1Dkm3wwqYv3kLi4hSF7Wl/pub?output=csv")

allegations <- read_csv(here("data", "allegations.csv"))

# subset allegations for officers at command in precincts and detective squads
allegations_pct_det <- allegations %>%
  filter(stringr::str_ends(command_now, "PCT") == TRUE | stringr::str_ends(command_now, "DET") == TRUE) %>%
  filter(year_received %in% c(2016, 2018))

# trim whitespaces
allegations_pct_det$command_now <- str_trim(allegations_pct_det$command_now, side = "both")
coordinates$command_now <- str_trim(coordinates$command_now, side = "both")

# join coordinate data with allegation data
map_data <- dplyr::inner_join(allegations_pct_det, coordinates, by = "command_now") %>% unique()

# convert command_now into factor
map_data <- map_data %>% mutate(command_now = as.factor(command_now))

# create counts for size of circle
radii <- map_data %>% count(command_now)
map_data <- map_data %>% left_join(radii, by = "command_now")

# colour palette for boroughs
boroughCol <- colorFactor(palette = "Paired", map_data$borough)

# pop up label
popup <- paste0(map_data$precinct.y, ", ", map_data$address, ", ", map_data$borough, "<br>", "Number of Allegations: ", map_data$n)

# yearly layers
map_data_2016 <- map_data %>% filter(year_received == 2016)
map_data_2018 <- map_data %>% filter(year_received == 2018)

# base leaflet
leaflet_year <- map_data %>%
  leaflet() %>%
  addTiles() %>%
  addTiles(group = "Open Street Maps") %>%
  addCircleMarkers(
    data = map_data_2016,
    radius = ~ map_data$n / 10,
    color = "pink",
    group = "2016",
    fillOpacity = 0.2,
    popup = popup
  ) %>%
  addCircleMarkers(
    data = map_data_2018,
    radius = ~ map_data$n / 10,
    color = "lavender",
    group = "2018",
    opacity = 0.2,
    popup = popup
  ) %>%
  addLayersControl(
    overlayGroups = map_data$year_received,
    options = layersControlOptions(collapsed = FALSE)
  )

# borough leaflet
leaflet_borough <- map_data %>%
  leaflet() %>%
  addTiles() %>%
  addTiles(group = "Open Street Maps") %>%
  addCircleMarkers(
    data = map_data,
    radius = ~ map_data$n / 10,
    color = boroughCol(map_data$borough),
    group = map_data$borough,
    fillOpacity = 0.8,
    popup = popup
  ) %>%
  addLayersControl(
    overlayGroups = map_data$borough,
    options = layersControlOptions(collapsed = FALSE)
  )

# # modules
# leafletUI <- function(id) {
#   ns <- NS(id)
#   tagList(selectInput(ns("year"), "Select Year", c(2016, 2018)))
#   leafletOutput(ns("leaflet"))
# }
#
# leafletModule <- function(input, output, session) {
#   output$leaflet <- renderLeaflet({"leaflet"})
# }
