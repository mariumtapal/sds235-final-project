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
allegations_2019 <- allegations %>%  filter(year_received == 2019)

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
        tabPanel("NYC Precincts"),
        tabPanel("Race/Gender Demographies")
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
}

# Run the application 
shinyApp(ui = ui, server = server)
