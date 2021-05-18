allegations <- read_csv(here("data", "allegations.csv"))
allegations_2016 <- allegations %>% filter(year_received == 2016)
allegations_2018 <- allegations %>% filter(year_received == 2018)

# barplot of rank of officers 2016
mos_officers_2016 <- allegations_2016 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(rank_incident) %>%
  count() %>%
  mutate(percent = paste(round(n / 744 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~rank_incident,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto",
    color = ~rank_incident
  ) %>%
  layout(
    title = "Ranks of the 744 Officers in 2016 Allegations",
    xaxis = list(title = "Rank of Officers"),
    yaxis = list(title = "Number of Complaints against officers")
  )


# barplot of rank of officers 2018
mos_officers_2018 <- allegations_2018 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(rank_incident) %>%
  count() %>%
  mutate(percent = paste(round(n / 744 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~rank_incident,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto",
    color = ~rank_incident
  ) %>%
  layout(
    title = "Ranks of the 744 Officers in 2018 Allegations",
    xaxis = list(title = "Rank of Officers"),
    yaxis = list(title = "Number of Complaints against officer")
  )



# barplot of types of allegations
mos_allegations_2016 <- allegations_2016 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(fado_type) %>%
  count() %>%
  mutate(percent = paste(round(n / 744 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~fado_type,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto",
    color = ~fado_type
  ) %>%
  layout(
    title = "Types of Allegations in 2016",
    xaxis = list(title = "Types of Allegations"),
    yaxis = list(title = "Number of Allegations")
  )



# barplot of types of allegations
mos_allegations_2018 <- allegations_2018 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(fado_type) %>%
  count() %>%
  mutate(percent = paste(round(n / 744 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~fado_type,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto",
    color = ~fado_type
  ) %>%
  layout(
    title = "Types of Allegations in 2018",
    xaxis = list(title = "Types of Allegations"),
    yaxis = list(title = "Number of Allegations")
  )


# Distribution of Complaints 2016
complaintresult2016 <- allegations_2016 %>%
  group_by(fado_type, board_disposition) %>%
  count() %>%
  plot_ly(
    x = ~fado_type,
    y = ~n,
    type = "bar",
    text = ~board_disposition,
    hovertemplate = "Status: %{text}<br>Type of Complaint: %{x}<br>%{y}",
    color = ~board_disposition,
    colors = brewer.pal(8, "Paired")
  ) %>%
  layout(
    title = "Distribution of Complaints by whether they are pursued or not for 2016",
    xaxis = list(title = "Type of Complaints"),
    yaxis = list(title = "Number of Complaints"),
    legend = list(title = "Type of Complainant")
  )

# Distribution of Complaints 2018
complaintresult2018 <- allegations_2018 %>%
  group_by(fado_type, board_disposition) %>%
  count() %>%
  plot_ly(
    x = ~fado_type,
    y = ~n,
    type = "bar",
    text = ~board_disposition,
    hovertemplate = "Status: %{text}<br>Type of Complaint: %{x}<br>%{y}",
    color = ~board_disposition,
    colors = brewer.pal(8, "Paired")
  ) %>%
  layout(
    title = "Distribution of Complaints by whether they are pursued or not for 2018",
    xaxis = list(title = "Type of Complaints"),
    yaxis = list(title = "Number of Complaints"),
    legend = list(title = "Type of Complainant")
  )


table <- allegations %>% filter(year_received == 2016 | year_received == 2018) %>% 
  group_by(last_name, first_name, year_received, 
           rank_now, mos_ethnicity, mos_gender) %>% count() %>% arrange(desc(n)) %>% 
  reactable(searchable = TRUE, filterable = TRUE, 
            columns = list(last_name = colDef(name = "Last Name"), 
                           first_name = colDef(name = "First Name"), 
                           year_received = colDef(name = "Year", align = "left"),
                           rank_now = colDef(name = "Rank"),
                           mos_ethnicity = colDef(name = "Ethnicity"),
                           mos_gender = colDef(name = "Gender"),
                           n = colDef(name = "Number of Complaints", align = "left")),
            theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "100%")))
