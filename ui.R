library(leaflet)

navbarPage("Fortinet connections analyzer", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Filter connections"),
        selectInput("countryFilterMAP", "Filter by country", c("All countries"="", sort(unique(inputData$srccountry))), multiple=TRUE),
        selectInput("serviceFilterMAP", "Filter by service", c("All services"="", sort(unique(inputData$service))), multiple=TRUE),
        dateInput("minDate", "Min date", "2017-12-08"),
        dateInput("maxDate", "Max date", "2017-12-11")
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        plotOutput("topcountries")
      ),
      column(6,
        plotOutput("cph")
      ),
      column(3,
        plotOutput("topservices")
      )
    ),
    fluidRow(
      column(3,
        selectInput("countryFilter", "Filter by country", c("All countries"="", sort(unique(inputData$srccountry))), multiple=TRUE)
      ),
      column(3,
             selectInput("serviceFilter", "Filter by service", c("All services"="", sort(unique(inputData$service))), multiple=TRUE)
      ),
      column(3,
             dateInput("minDate", "Min date", "2017-12-08")
      ),
      column(3,
             dateInput("maxDate", "Max date", "2017-12-11")
      )
    ),
    DT::dataTableOutput("data")
  ),

  conditionalPanel("false", icon("crosshair"))
)