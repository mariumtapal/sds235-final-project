# deal with NAs
allegations_2016 <- allegations_2016 %>%
  replace_na(list(complainant_ethnicity = "Unknown", complainant_gender = "NA", complainant_age_incident = "NA"))

allegations_2018 <- allegations_2018 %>%
  replace_na(list(complainant_ethnicity = "Unknown", complainant_gender = "NA", complainant_age_incident = "NA"))

allegations <- allegations %>%
  replace_na(list(complainant_ethnicity = "Unknown", complainant_gender = "NA", complainant_age_incident = "NA"))

# Officer Plot
# barplot of races of officers 2016
mos_race_2016 <- allegations_2016 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(mos_ethnicity) %>%
  count() %>%
  mutate(percent = paste(round(n / 744 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~mos_ethnicity,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Racial Percentages of the 744 Officers in 2016 Allegations",
    xaxis = list(title = "Race of Officers"),
    yaxis = list(title = "Number of Complaints")
  )

# barplot of races of officers 2018
mos_race_2018 <- allegations_2018 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(mos_ethnicity) %>%
  count() %>%
  mutate(percent = paste(round(n / 726 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~mos_ethnicity,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Racial Percentages of the 726 Officers in 2018 Allegations",
    xaxis = list(title = "Race of Officers"),
    yaxis = list(title = "Number of Complaints")
  )

# barplot of races of all officers
allegations_officers <- allegations %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE)

mos_race <- allegations_officers %>%
  group_by(mos_ethnicity) %>%
  count() %>%
  mutate(percent = paste(round(n / nrow(allegations_officers) * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~mos_ethnicity,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Racial Percentages of Officers in Allegations",
    xaxis = list(title = "Race of Officers"),
    yaxis = list(title = "Number of Complaints")
  )

# gender of officers 2016
mos_gender_2016 <- allegations_2016 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(mos_gender) %>%
  count() %>%
  mutate(percent = paste(round(n / 744 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~mos_gender,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Gender Percentages of 744 Officers in 2016 Allegations",
    xaxis = list(title = "Gender of Officers"),
    yaxis = list(title = "Number of Complaints")
  )

# gender of officers 2018
mos_gender_2018 <- allegations_2018 %>%
  mutate(name = paste(first_name, last_name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(mos_gender) %>%
  count() %>%
  mutate(percent = paste(round(n / 726 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~mos_gender,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Gender Percentages of 726 Officers in 2018 Allegations",
    xaxis = list(title = "Gender of Officers"),
    yaxis = list(title = "Number of Complaints")
  )

# complainant plot
# barplot of races of complainants in 2016
comp_race_2016 <- allegations_2016 %>%
  group_by(complainant_ethnicity) %>%
  count() %>%
  mutate(percent = paste(round(n / 2345 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~complainant_ethnicity,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Racial Percentages of 2345 Complainants in 2016 Allegations",
    xaxis = list(title = "Race of Complainants"),
    yaxis = list(title = "Number of Complaints")
  )

# barplot of races of complainants in 2018
comp_race_2018 <- allegations_2018 %>%
  group_by(complainant_ethnicity) %>%
  count() %>%
  mutate(percent = paste(round(n / 2281 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~complainant_ethnicity,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Racial Percentages of 2281 Complainants in 2018 Allegations",
    xaxis = list(title = "Race of Complainants"),
    yaxis = list(title = "Number of Complaints")
  )

# gender of complainants 2016
comp_gender_2016 <- allegations_2016 %>%
  group_by(complainant_gender) %>%
  count() %>%
  mutate(percent = paste(round(n / 2345 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~complainant_gender,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Gender Breakdown of Complainants in 2016 Allegations",
    xaxis = list(title = "Gender of Complainants"),
    yaxis = list(title = "Number of Complaints")
  )

# gender of complainants 2018
comp_gender_2018 <- allegations_2018 %>%
  group_by(complainant_gender) %>%
  count() %>%
  mutate(percent = paste(round(n / 2281 * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~complainant_gender,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Gender Breakdown of Complainants in 2016 Allegations",
    xaxis = list(title = "Gender of Complainants"),
    yaxis = list(title = "Number of Complaints")
  )

complainant_race <- allegations %>%
  group_by(complainant_ethnicity) %>%
  count() %>%
  mutate(percent = paste(round(n / nrow(allegations) * 100, digits = 2), "%", sep = "")) %>%
  plot_ly(
    x = ~complainant_ethnicity,
    y = ~n,
    type = "bar",
    text = ~percent,
    textposition = "auto"
  ) %>%
  layout(
    title = "Racial Percentages of Complainants in Allegations",
    xaxis = list(title = "Race of Complainants"),
    yaxis = list(title = "Number of Complaints")
  )

# races plot
# barplot of races of officers and complainants
race_plot <- allegations %>%
  filter(year_received == 2016 | year_received == 2018) %>%
  group_by(mos_ethnicity, complainant_ethnicity, year_received) %>%
  count() %>%
  plot_ly(
    x = ~mos_ethnicity,
    y = ~n,
    type = "bar",
    text = ~complainant_ethnicity,
    hovertemplate = "Complainant Race: %{text}<br>Officer Race: %{x}<br>%{y}",
    color = ~complainant_ethnicity,
    colors = brewer.pal(8, "Paired")
  ) %>%
  layout(
    title = "Distribution of Complainant Race by Officer Race",
    xaxis = list(title = "Race of Officer"),
    yaxis = list(title = "Number of Complaints"),
    legend = list(title = "Race of Complainant")
  )

# 2016 race plot
race_plot_2016 <- allegations_2016 %>%
  group_by(mos_ethnicity, complainant_ethnicity) %>%
  count() %>%
  plot_ly(
    x = ~mos_ethnicity,
    y = ~n,
    type = "bar",
    text = ~complainant_ethnicity,
    hovertemplate = "Complainant Race: %{text}<br>Officer Race: %{x}<br>%{y}",
    color = ~complainant_ethnicity,
    colors = brewer.pal(8, "Paired")
  ) %>%
  layout(
    title = "Distribution of Complainant Race by Officer Race in 2016",
    xaxis = list(title = "Race of Officer"),
    yaxis = list(title = "Number of Complaints"),
    legend = list(title = "Race of Complainant")
  )

# 2018 race plot
race_plot_2018 <- allegations_2018 %>%
  group_by(mos_ethnicity, complainant_ethnicity) %>%
  count() %>%
  plot_ly(
    x = ~mos_ethnicity,
    y = ~n,
    type = "bar",
    text = ~complainant_ethnicity,
    hovertemplate = "Complainant Race: %{text}<br>Officer Race: %{x}<br>%{y}",
    color = ~complainant_ethnicity,
    colors = brewer.pal(8, "Paired")
  ) %>%
  layout(
    title = "Distribution of Complainant Race by Officer Race in 2018",
    xaxis = list(title = "Race of Officer"),
    yaxis = list(title = "Number of Complaints")
  )