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
    tabPanel("Home"),
    tabPanel("Allegations"),
    tabPanel(
      "NYC Precincts",
      p("This interactive map shows the number of allegations 
             against police officers in each NYC precinct in the years 2016 and 2018 
             (combined) by borough. You can toggle for each borough and click on the marker 
             to view this information. The size of the marks indicate the number of allegations."),
      fluidRow(
        column(
          3,
          selectInput("select_year_leaflet",
            label = "Precinct Allegations by Borough",
            choices = list(
              "2016 and 2018 Combined", 2016,
              2018
            )
          ),
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
      fluidRow(
        column(
          3, "Race of Officers and Complainants",
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
      )
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
