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
library(RColorBrewer)
library(kableExtra)
library(htmltools)

# load in data
allegations <- read_csv(here("data", "allegations.csv"))
allegations_2016 <- allegations %>% filter(year_received == 2016)
allegations_2018 <- allegations %>% filter(year_received == 2018)

# load in source files
source("home.R")
source("allegations.R")
source("precincts.R")
source("demographics.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # CSS for making the footer stick
  tags$body(
    tags$style(HTML("body {
                margin: 0;
                height: 100%;
                min-height: 100%;}"))
  ),


  # Application theme
  theme = shinytheme("flatly"),

  # Application title
  titlePanel("NYC Civilian Complaints About PD"),

  # Create tabs
  tabsetPanel(
    tabPanel(
      "Home",
      h3("General Overview"),
      p("In April 2017, the New York City Police Department initiated the use of 24,000 body cameras for its 
        patrol force and street units. The intent of body cams is to foster greater peace and trust 
        between civilians and police by recording their interactions. Specifically, the use of the cameras 
        is meant to dissuade and prevent police brutality, which is often racially motivated. In June of 2017, NYC 
        Mayor Bill de Blasio announced a policy that requires the NYPD to release all body cam footage of 
        incidents involving force which ended in injury or death."),
      tags$a(href = "https://apnews.com/article/2fea6f0179f8e95e332c2c4deeaa861a", "Source: AP News"),
      p("With all of this in mind, 
        we wanted to create a tool that allows city administrators, such as the District Attorney, to visualize
        the impact of body cameras on civilian complaints against the NYPD. As the body camera changes were 
        enacted in 2017, we chose to look at the allegations from 2016 and 2018."),
      h3("Data Source"),
      p("We draw on data from Propublica's dataset Civilian Complaints Against New York City Police Officers.
        This data comes from New York City's Civilian Complaint Review Board and 
        encompasses complaints from September 1985 to January 2020. Each observation in the data contains
        information on the officer and complainant involved, as well as information about the complaint itself.
        Our project centers on a selection of these variables for the years 2016 and 2018. Additionally, we used
        a GPS visualizer and data from the New York City government website to manually map precinct locations."),
      tags$a(
        href = "https://www.propublica.org/datastore/dataset/civilian-complaints-against-new-york-city-police-officers",
        "Propublica Data, "
      ),
      tags$a(href = "https://www.gpsvisualizer.com/geocoder/", "GPS Visualizer, "),
      tags$a(href = "https://www1.nyc.gov/site/nypd/bureaus/patrol/precincts-landing.page", "Precincts Data"),
      h3("Relevant Variables"),
      p("This project focuses on the types of allegations and whether they were substantiated, the distribution
        of allegations by precinct, and the race and gender demographics of both the officers and complainants.
        We use the following variables in our visualizations:"),
      tags$ul(
        tags$li("year_received: the year the complaint was filed"),
        tags$li("rank_incident: the rank of the officer at the time of the incident"),
        tags$li("mos_ethnicity: the race of the officer"),
        tags$li("mos_gender: the gender of the officer"),
        tags$li("complainant_ethnicity: the race of the complainant"),
        tags$li("complainant_gender: the gender of the complainant"),
        tags$li("fado_type: the category the allegation falls under"),
        tags$li("precinct: the precinct of the officer in the allegation"),
        tags$li("board_disposition: whether the allegation was substantiated and found to be in violation of the
        NYPD's rules")
      ),
      p("For further information on the variables in the dataset, see the corresponding data 
        dictionary."),
      tags$a(
        href = "https://github.com/mariumtapal/sds235-final-project/blob/main/data/data_descriptions.xlsx",
        "Data Dictionary"
      ),
      h3("Available Tabs"),
      p("Navigating the tabs above takes you to the different visualizations we have constructed. The
        Allegations tab illustrates the distribution of ranks among the officers in the allegations and the
        types of allegations, as well as whether they were substantiated. The NYC Precincts tab contains a 
        map of all precincts with the numbers of allegations by location. The Race/Gender Demographics tab 
        has a series of plots illustrating the demographics of officers and complainants, both individually
        and combined. Finally, every tab provides the ability to switch between the data for 2016 and 2018
        allegations."),
      p(),
    ),
    tabPanel("Allegations"),
    tabPanel(
      "NYC Precincts",
      h3("Map: Allegations by Year"),
      p("This interactive map shows the number of allegations 
             against police officers in each NYC precinct in the years 2016 and 2018, before
             and after the body cameras were introduced. The default view shows the combined
             allegations for 2016 and 2016. You can view a specfic year by clicking on the buttons
             below. Furthermore, you can select which boroughs you would like to look at by
             using the multi-select option on the upper-right corner of the map."),
      p(),
      p("Each circle marker represents a precinct. You can hover on these markers to 
        view the precinct name, address, and the number of allegations.
        The size of the marks reflects the number of allegations."),
      fluidRow(
        column(
          3,
          radioButtons("select_year_leaflet",
            label = "Select Year",
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
      p(),
      h3("Stats: Allegations by Precinct"),
      p("The table below shows the summary of allegations for each police 
      precinct in New York City by year. From the drop-down menu, select a
        precinct (this field is searchable) and select the year to view the table.
        Please refer to the home tab for details of each category."),
      fluidRow(
        column(
          3, selectizeInput("precinct",
            choices = sort(unique(map_data$precinct.y)),
            label = "Select Precinct", selected = "1st Precinct"
          ),
          radioButtons("year_precinct",
            label = "Select Year",
            choices = list(
              2016,
              2018
            )
          )
        ),
        column(9, htmlOutput("table_precinct"))
      ),
      h3("Limitation"),
      p("Due to limited information about the physical location of command stations other than 
      precincts or detective squads, a subset of the original data is used in the making of the 
        map and table on this page."),
    ),
    tabPanel(
      "Race/Gender Demographics",
      h3("Officer Demographics"),
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
      h4("A Note on Limitations"),
      p("These graphs only include the officers who appeared in a complaint. The data
         therefore does not take into account the racial and gender breakdown of the whole NYPD. It is possible
         that we see the percentages in the graph above because they are the percentages of the whole police force,
         and not because there is one group that disproportionately appears in the allegations. Moreover, because the data
         only considers two genders for officers, officers that identify outside the gender binary are not represented.
         While we were unable to obtain data on the NYPD in 2016 and 2018, as of 2021 the NYPD is 81% male and 19% female. 
         Additionally, 46% of officers are white, 30% are Hispanic, 15% are Black, and 9% are Asian. These numbers are not
         necessarily representative of the force in 2016 and 2018, but they can provide some context."),
      tags$a(
        href = "https://www1.nyc.gov/site/ccrb/policy/data-transparency-initiative-mos.page",
        "Current NYPD Demographics"
      ),
      h3("Complainant Demographics"),
      p("Similar to the graph above, this interactive barplot shows the race and gender distributions of
        all complainants in the 2016 or 2018 allegations. The drop downs on the side allow you to select
        the demographic and year you would like to see."),
      fluidRow(
        column(
          3, "Complainant Demographics",
          selectInput("select_year2",
            label = "Year",
            choices = list(
              2016,
              2018
            )
          ),
          selectInput("select_demographic",
            label = "Demographic",
            choices = list(
              "Race",
              "Gender"
            )
          )
        ),
        column(
          9,
          textOutput("output_year_2"),
          textOutput("output_demographic"),
          plotlyOutput("complainant_race")
        )
      ),
      p(),
      h3("Race of Complainants by Officer Race"),
      p("Finally, the graph below breaks down the races of officers and the races of complainants for each
        of the two years. For each race of officer, the graph shows the number of associated complainants 
        in each racial category. For example, in 2016, 282 Hispanic complainants leveraged complaints against
        a white officer. You can switch between years using the menu on the side."),
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
      ),
      p(),
      h4("A Final Note on Limitations"),
      p("Because this data relies on the reported civilian complaints, there is a risk of reporting bias. Thus,
        when looking at these numbers and demographics, we must also ask who may be underrepresented. It is possible
        there are particular groups who feel less safe reporting a complaint, or who don't believe it will 
        amount to anything. Such bias must be taken account when analyzing this potentially skewed data.")
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

  # summary table for precincts
  output$table_precinct <- reactive({
    req(input$precinct)
    req(input$year_precinct)
    map_data %>%
      filter(precinct.y == input$precinct & year_received == input$year_precinct) %>%
      group_by(fado_type, allegation, board_disposition) %>%
      count() %>%
      arrange(desc(n)) %>%
      knitr::kable("html", col.names = c("Top-level Category of Complaint",
                                         "Specific Category of Complaint",
                                         "Finding by the CCRB", "Count")) %>%
      kable_styling("striped", full_width = T)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
