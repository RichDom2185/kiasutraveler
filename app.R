library(shiny)
library(shinyjs)
library(shinyTime)
library(mapboxer)
library(bslib)
library(dplyr)
library(httr)
library(jsonlite)
library(ggplot2)
library(mapboxer)
library(sf)
library(wordcloud)

source("api/functions.R")
source("ui/components/bulma.R")

source("logic/utils/functions.R")

source("ui/pages/home.R")
source("ui/pages/rideHailing.R")
source("ui/pages/carSharing.R")
source("ui/pages/taxi.R")
source("ui/pages/publicTransport.R")
source("ui/pages/others.R")

options(shiny.autoreload = TRUE)

theme <- bs_theme(
  bg = "#0171bb", fg = "#FDf7f7", primary = "#FCC780",
  base_font = font_google("Lato")
)

ui <- navbarPage(
  theme = theme, title = div(img(src = "https://drive.google.com/file/d/1wHxSPAEbzlrN2s1Y-1Wn_HidsE9ixFAb/view?usp=sharing"), "Kiasu Traveler"),
  tabPanel("Ride Hailing", plotOutput("ridehailing")),
  tabPanel("Car Sharing", plotOutput("carsharing")),
  tabPanel(
    "Taxi",
    app(
      useShinyjs(),
      setDefaultTab("activeTab", "home"),
      stackElements(
        mapboxerOutput("map", height = "100vh"),
        column(
          box(
            style = "width: fit-content; position: absolute; top: 0; left: 0;",
            appTitle("Kiasutraveler")
          ),
          box(
            style = "width: fit-content; position: absolute; right: 0; top: 0",
            class = "py-3 px-3",
            tabs(
              "activeTab",
              tab("home", "Home"),
              tab("rideHailing", "Ride Hailing"),
              tab("carSharing", "Car-Sharing"),
              tab("taxi", "Taxi"),
              tab("publicTransport", "Public Transport"),
              tab("others", "Others")
            )
          ),
          box(
            # TODO: Remove max-height and overflow-y hotfix
            style = "width: fit-content; position: absolute; bottom: 0; right: 0; max-height: 30vh; overflow-y: auto",
            homeTabContent,
            rideHailingTabContent,
            carSharingTabContent,
            taxiTabContent,
            publicTransportTabContent,
            othersTabContent
          )
        )
      )
    )
  ),
  tabPanel("Public transport", plotOutput("pt")),
  navbarMenu(
    "Other insights",
    tabPanel(
      "Weather",
      tabsetPanel(
        tabPanel("2 hour forecast", plotOutput("2hourforecast")),
        tabPanel("Historical Rainfall Plot", plotOutput("historical_rainfall_plot")),
        tabPanel("Live Rainfall Map", mapboxerOutput("rainfall_map", height = "100vh"))
      )
    ),
    tabPanel(
      "Crowd",
      tabsetPanel(
        tabPanel("Bus Crowd", plotOutput("busVolume")),
        tabPanel("MRT Crowd", plotOutput("mrtVolume"))
      )
    ),
    tabPanel(
      "Traffic Incidents",
      tabsetPanel(
        tabPanel(
          "Wordcloud",
          fluidPage(
            sidebarLayout(
              position = "right",
              sidebarPanel(
                width = 7,
                h2("Traffic Incident Causes in Singapore", style = "font-weight:bold; font-size:20px;"), br(),
                p("The following wordcloud illustrates some of the most common traffic incident causes in Singapore from 2012 to 2018.
                                         The most common cause appears to be Diversion and Accidents"), br(),
                p("Incidents categorised under 'Diversion' include 'Turning / Changing Lanes without Due Care'."), br(),
                p("Incidents categorised under 'Accidents' include 'Disobeying Traffic Signals' or 'Failing to give way to a pedestrian'."), br(),
                p("Take account of these statistics when you are on the road!")
              ),
              mainPanel(width = 5, plotOutput("wordcloud"))
            )
          )
        ),
        tabPanel(
          "Live Incident Causes",
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                width = 6,
                h1("Live Traffic Incident Causes", style = "font-weight:bold; font-size:20px;"), br(),
                p("The bar graph illustrates live traffic incident causes right now. The data is updated every two minutes."),
                br(),
                p("Observe the various incidents happening now to get a better idea of the road congestion situation."),
                p("Click on the 'Live Traffic Incidents' tab to find out where these incidents occured!"), br(),
                p(paste(
                  "Getting traffic incident information on", format(Sys.Date(), "%A, %B %d, %Y"),
                  "at", format(Sys.time(), "%I:%M %p"), "..."
                ), style = "font-weight:bold;")
              ),
              mainPanel(width = 6, plotOutput("incidentbar"))
            )
          )
        ),
        tabPanel(
          "Live Traffic Incidents",
          stackElements(
            mapboxerOutput("incident_map", height = "100vh"),
            box(
              style = "width: fit-content; position: absolute; bottom: 0; right: 0",
              column(htmlOutput("trafficincident_description"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # Set up custom tab pages
  currentActiveTab <- reactiveVal()
  observeEvent(input$activeTab, {
    removeCssClass(id = currentActiveTab(), class = "is-active")
    currentActiveTab(input$activeTab)
    addCssClass(id = currentActiveTab(), class = "is-active")

    switch(input$activeTab,
      "home" = updateHomeTab(input, output),
      "rideHailing" = updateRideHailingTab(input, output),
      "carSharing" = updateCarSharingTab(input, output),
      "taxi" = updateTaxiTab(input, output),
      "publicTransport" = updatePublicTransportTab(input, output),
      "others" = updateOthersTab(input, output),
      # default
      warning("Invalid/no tab selected")
    )
  })
}

shinyApp(ui, server, options = list(
  port = 4000,
  host = "0.0.0.0"
  # launch.browser = FALSE
))
