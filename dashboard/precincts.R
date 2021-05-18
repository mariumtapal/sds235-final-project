# subset allegations for officers at command in precincts and detective squads
allegations_pct_det <- allegations %>%
  filter(stringr::str_ends(command_now, "PCT") == TRUE | stringr::str_ends(command_now, "DET") == TRUE) %>%
  filter(year_received %in% c(2016, 2018))

# 2016 and 2018 combined -------------------
# trim whitespace
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
boroughCol <- colorFactor(palette = "viridis", map_data$borough)
# pop up label
popup <- paste0(map_data$precinct.y, ", ", map_data$address, ", ", map_data$borough, "<br>", "Number of Allegations: ", map_data$n)

# leaflet
leaflet_all <- map_data %>%
  leaflet() %>%
  addTiles() %>%
  addTiles(group = "Open Street Maps") %>%
  addCircleMarkers(
    data = map_data,
    radius = ~ map_data$n / 10,
    color = boroughCol(map_data$borough),
    group = map_data$borough,
    fillOpacity = 0.8,
    label = lapply(popup, htmltools::HTML)
  ) %>%
  addLayersControl(
    overlayGroups = map_data$borough,
    options = layersControlOptions(collapsed = FALSE)
  )

# 2016 -------------------
map_data_2016 <- map_data %>% filter(year_received == 2016)
# create counts for size of circle
radii_2016 <- map_data_2016 %>% count(command_now)
map_data_2016 <- map_data_2016 %>% left_join(radii_2016, by = "command_now")
# colour palette for boroughs
boroughCol <- colorFactor(palette = "viridis", map_data_2016$borough)
# pop up label
popup_2016 <- paste0(map_data_2016$precinct.y, ", ", map_data_2016$address, ", ", map_data_2016$borough, "<br>", "Number of Allegations: ", map_data_2016$n.y)

# leaflet
leaflet_2016 <- map_data_2016 %>%
  leaflet() %>%
  addTiles() %>%
  addTiles(group = "Open Street Maps") %>%
  addCircleMarkers(
    data = map_data_2016,
    radius = ~ map_data_2016$n.y / 10,
    color = boroughCol(map_data_2016$borough),
    group = map_data_2016$borough,
    fillOpacity = 0.8,
    label = lapply(popup_2016, htmltools::HTML)
  ) %>%
  addLayersControl(
    overlayGroups = map_data_2016$borough,
    options = layersControlOptions(collapsed = FALSE)
  )

# 2018 -------------------
map_data_2018 <- map_data %>% filter(year_received == 2018)
# create counts for size of circle
radii_2018 <- map_data_2018 %>% count(command_now)
map_data_2018 <- map_data_2018 %>% left_join(radii_2018, by = "command_now")
# colour palette for boroughs
boroughCol <- colorFactor(palette = "viridis", map_data_2018$borough)
# pop up label
popup_2018 <- paste0(map_data_2018$precinct.y, ", ", map_data_2018$address, ", ", map_data_2018$borough, "<br>", "Number of Allegations: ", map_data_2018$n.y)

# leaflet
leaflet_2018 <- map_data_2018 %>%
  leaflet() %>%
  addTiles() %>%
  addTiles(group = "Open Street Maps") %>%
  addCircleMarkers(
    data = map_data_2018,
    radius = ~ map_data_2018$n.y / 10,
    color = boroughCol(map_data_2018$borough),
    group = map_data_2018$borough,
    fillOpacity = 0.8,
    label = lapply(popup_2018, htmltools::HTML)
  ) %>%
  addLayersControl(
    overlayGroups = map_data_2018$borough,
    options = layersControlOptions(collapsed = FALSE)
  )
