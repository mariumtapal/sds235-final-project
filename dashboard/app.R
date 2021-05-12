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
allegations_2019 <- allegations %>% filter(year_received == 2019)
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
        tabPanel("NYC Precincts",
                 p("The following interactive map shows the number of allegations 
             against police officers in each NYC precinct in the years 2016 and 2018. 
             You can toggle each layer and click on the marker to this information."),
                 leafletOutput("leaflet_year"),
                 p(),
                 p("This interactive map shows the number of allegations 
             against police officers in each NYC precinct in the years 2016 and 2018 
             (combined) by borough. You can toggle each layer and click on the marker 
             to this information."),
                 leafletOutput("leaflet_borough"),
                 p()),
        tabPanel("Race/Gender Demographics", 
              sidebarLayout(
              sidebarPanel("Select Year",
              selectInput("select_year", 
                              label = "Year",
                              choices = list(2016, 
                                             2018)
                         )
                          ),
              mainPanel("Demographics of Officers in Allegations",
                  textOutput("output_year"),
                  plotlyOutput("race_plot")
                       )
                           )
        )),
        
    
    # Footer
    div(
        class = "footer",
        includeHTML("footer.html")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # put in output components here
  output$race_plot <- renderPlotly({
      subset(race_plot, year_received == input$select_year)
  })
  output$output_year <- renderText({
      paste("Year selected", input$select_year)
  })

  output$leaflet_year <- renderLeaflet({
    leaflet_year
  })

  output$leaflet_borough <- renderLeaflet({
    leaflet_borough
  })

}

# Run the application
shinyApp(ui = ui, server = server)
