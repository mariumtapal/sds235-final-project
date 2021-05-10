#barplot of races of officers
mos_race_plot <- allegations_2019 %>% 
  distinct(last_name, .keep_all = TRUE) %>% 
  group_by(mos_ethnicity) %>% 
  count() %>% 
  mutate(percent = round(n/475*100, digits = 2)) %>% 
  plot_ly(x = ~mos_ethnicity,
          y = ~n,
          type = 'bar',
          text = ~percent,
          textposition = 'auto') %>% 
  layout(title = "Racial Breakdown of Officers in 2019 Allegations",
         xaxis = list(title = 'Race of Officers'),
         yaxis = list(title = 'Number of Complaints'))

mos_race_plot
