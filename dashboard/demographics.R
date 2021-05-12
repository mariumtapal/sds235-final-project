library(tidyverse)
library(plotly)
library(here)
library(RColorBrewer)

#read in data
allegations <- read_csv(here("data", "allegations.csv")) 
allegations_2019 <- allegations %>%  filter(year_received == 2019)
allegations_2016 <- allegations %>%  filter(year_received == 2016)
allegations_2018 <- allegations %>%  filter(year_received == 2018)

#deal with NAs
allegations_2016 <- allegations_2016 %>% 
  replace_na(list(complainant_ethnicity = 'Unknown', complainant_gender = 'NA', complainant_age_incident = 'NA'))

allegations_2018 <- allegations_2018 %>% 
  replace_na(list(complainant_ethnicity = 'Unknown', complainant_gender = 'NA', complainant_age_incident = 'NA'))

allegations <- allegations %>% 
  replace_na(list(complainant_ethnicity = 'Unknown', complainant_gender = 'NA', complainant_age_incident = 'NA'))

#barplot of races of officers 2016
mos_race_2016 <- allegations %>%
  mutate(name = paste(first_name, last_name)) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  group_by(mos_ethnicity) %>% 
  count() %>% 
  mutate(percent = paste(round(n/744*100, digits = 2),'%', sep = "")) %>% 
  plot_ly(x = ~mos_ethnicity,
          y = ~n,
          type = 'bar',
          text = ~percent,
          textposition = 'auto') %>% 
  layout(title = "Racial Percentages of the 744 Officers in 2016 Allegations",
         xaxis = list(title = 'Race of Officers'),
         yaxis = list(title = 'Number of Complaints'))

#barplot of races of all officers
allegations_officers <- allegations %>% 
  mutate(name = paste(first_name, last_name)) %>% 
  distinct(name, .keep_all = TRUE)

mos_race <- allegations_officers %>%
  group_by(mos_ethnicity) %>% 
  count() %>% 
  mutate(percent = paste(round(n/nrow(allegations_officers)*100, digits = 2),'%', sep = "")) %>% 
  plot_ly(x = ~mos_ethnicity,
          y = ~n,
          type = 'bar',
          text = ~percent,
          textposition = 'auto') %>% 
  layout(title = "Racial Percentages of Officers in Allegations",
         xaxis = list(title = 'Race of Officers'),
         yaxis = list(title = 'Number of Complaints'))

#barplot of races of officers and complainants
race_plot <- allegations %>%
  filter(year_received == 2016 | year_received == 2018) %>% 
  group_by(mos_ethnicity, complainant_ethnicity, year_received) %>% 
  count() %>% 
  plot_ly(x = ~mos_ethnicity,
          y = ~n,
          type = 'bar',
          text = ~complainant_ethnicity,
          hovertemplate = 'Complainant Race: %{text}<br>Officer Race: %{x}<br>%{y}',
          color = ~complainant_ethnicity,
          colors = brewer.pal(8, "Paired")
  ) %>% 
  layout(title = "Distribution of Complainant Race by Officer Race",
         xaxis = list(title = "Race of Officer"),
         yaxis = list(title = "Number of Complaints"),
         legend = list(title = "Race of Complainant"))

