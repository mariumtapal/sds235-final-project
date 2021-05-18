
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SDS 235: Visual Analytics - Final Project

## A Shiny App exploring NYC Civilian Complaints about the NYPD

Our app is deployed on shinyapps.io at this link
<add link after deploying>. You could also run it by cloning this
repository locally and running the `app.R` script from the `dashboard`
folder!

### Components of this repository

Our project includes the following files/folders:

-   the `dashboard` folder includes necessary components of our app:
    -   `app.R` containing the `server` and `ui` of our app
    -   the other `.R` files contain components that are sourced into
        `app.R`
    -   `footer.hmtl` contains HTML and CSS code for our footer
-   the `scratchpads` folder contains our data wrangling and
    visualizations before they go into the shiny app
-   the `data` folder contains the main data for our project. It is
    called **Civilian Complaints Against New York City Police
    Officers**. It was retrieved on April 29, 2021 from
    [ProPublica](www.propublica.org/datastore/dataset/civilian-complaints-against-new-york-city-police-officers).

### Required Packages

Our app requires the following R packages which are all available for
download from CRAN:

-   tidyverse
-   shiny
-   shinythemes
-   leaflet
-   plotly
-   here
-   RColorBrewer
-   kableExtra
-   htmltools
-   reactable

You can run the following code in your console to install all these
packages:

``` r
install.packages(c(
  "tidyverse", "shiny", "shinythemes", "leaflet", "plotly", "here", "RColorBrewer", "kableExtra", "htmltools", "reactable"
))
```

GitHub repo: <https://github.com/mariumtapal/sds235-final-project>

Group Members:

-   Marium Tapal
-   Eleni Partakki
-   Elisabeth Nesmith
