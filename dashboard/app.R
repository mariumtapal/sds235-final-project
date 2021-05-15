# load in required packages
library(shiny)
library(shinythemes)
library(leaflet)
library(reactable)
library(readxl)
library(janitor)
library(tidyverse)
library(readtext)
library(reactable)
library(plotly)
library(here)

# load in data
allegations <- read_csv(here("data", "allegations.csv"))
# 2019 till demographics file is updated
allegations_2016 <- allegations %>% filter(year_received == 2016)
allegations_2018 <- allegations %>% filter(year_received == 2018)

# load in source files
source("home.R")
source("allegations.R")
source("precincts.R")
source("demographics.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application theme
  theme = shinytheme("flatly"),

  # Application title
  titlePanel("NYC Civilian Complaints About PD"),

  # Create tabs
  tabsetPanel(
    tabPanel(
      "Home",
      p("")),
    tabPanel("Allegations"),
    tabPanel(
      "NYC Precincts",
      p(),
      p("This interactive map shows the number of allegations 
             against police officers in each NYC precinct in the years 2016 and 2018 
             (combined) by borough. You can toggle for each borough and click on the marker 
             to view this information. The size of the marks indicate the number of allegations."),
      fluidRow(
        column(
          3,
          selectInput("select_year_leaflet",
            label = "Precinct Allegations by Year",
            choices = list(
              "2016 and 2018 Combined", 2016,
              2018
            )
          ),
          h3("Limitations"),
          p("The allegations that were counted in the making of this map were only of the officers at
        command in a precinct or a detective squad as there was limited information about the
        physical location of other command stations."),
        ),
        column(
          9,
          textOutput("select_year_leaflet"),
          leafletOutput("leaflets")
        )
      ),
      p()
    ),
    tabPanel(
      "Race/Gender Demographics",
      p("The following graph illustrates the racial and gender breakdowns of all officers who appeared 
        in a formal complaint in 2016 or 2018. Using the dropdown menus on the side, you can select which
        demographic to plot and toggle between 2016 and 2018 to see how the numbers changed. Additionally,
        hovering over each bar reveals more information."),
      fluidRow(
        column(
          3, "Officer Demographics",
          selectInput("select_year1",
            label = "Year",
            choices = list(
              2016,
              2018
            )
          ),
          selectInput("select_variable",
            label = "Demographic",
            choices = list(
              "Race",
              "Gender"
            )
          )
        ),
        column(
          9,
          textOutput("output_year1"),
          textOutput("output_variable"),
          plotlyOutput("mos_race")
        )
      ),
      p(),
      p("Similar to the graph above, this interactive barplot shows the race and gender distributions of
        all complainants in the 2016 or 2018 allegations. The drop downs on the side allow you to select
        the demographic and year you would like to see."),
      fluidRow(
        column(3, "Complainant Demographics",
               selectInput("select_year2",
                           label = "Year",
                           choices = list(2016,
                                          2018
                                          )),
               selectInput("select_demographic",
                           label = "Demographic",
                           choices = list("Race",
                                          "Gender"))),
        column(9,
               textOutput("output_year_2"),
               textOutput("output_demographic"),
               plotlyOutput("complainant_race"))
       
      ),
      p(),
      p("Finally, the graph below breaks down the races of officers and the races of complainants for each
        of the two years. For each race of officer, the graph shows the number of associated complainants 
        in each racial category. For example, in 2016, 282 Hispanic complainants leveraged complaints against
        a white officer. You can switch between years using the menu on the side."),
      fluidRow(
        column(3, "Race of Officers and Complainants",
          selectInput("select_year",
            label = "Year",
            choices = list(
              2016,
              2018
            )
          )
        ),
        column(
          9,
          textOutput("output_year"),
          plotlyOutput("race_plot")
        )
      ),
      p(),
      )
    ),

  # Footer
  div(
    class = "footer",
    includeHTML("footer.html")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # put in output components here
  output$mos_race <- renderPlotly({
    off <- mos_race
    if (input$select_year1 == 2016 & input$select_variable == "Race") {
      off <- mos_race_2016
    }
    else if (input$select_year1 == 2016 & input$select_variable == "Gender") {
      off <- mos_gender_2016
    }
    else if (input$select_year1 == 2018 & input$select_variable == "Race") {
      off <- mos_race_2018
    }
    else if (input$select_year1 == 2018 & input$select_variable == "Gender") {
      off <- mos_gender_2018
    }
    off
  })
  
  output$complainant_race <- renderPlotly({
    comp <- complainant_race
    if (input$select_year2 == 2016 & input$select_demographic == "Race") {
      comp <- comp_race_2016
    }
    else if (input$select_year2 == 2016 & input$select_demographic == "Gender") {
      comp <- comp_gender_2016
    }
    else if (input$select_year2 == 2018 & input$select_demographic == "Race") {
      comp <- comp_race_2018
    }
    else if (input$select_year2 == 2018 & input$select_demographic == "Gender") {
      comp <- comp_gender_2018
    }
    comp
  })

  output$race_plot <- renderPlotly({
    res <- race_plot
    if (input$select_year == 2016) {
      res <- race_plot_2016
    }
    else if (input$select_year == 2018) {
      res <- race_plot_2018
    }
    res
  })

  output$leaflets <- renderLeaflet({
    off <- leaflet_all
    if (input$select_year_leaflet == 2016) {
      off <- leaflet_2016
    }
    else if (input$select_year_leaflet == 2018) {
      off <- leaflet_2018
    }
    off
  })
}

# Run the application
shinyApp(ui = ui, server = server)
