library(shiny)
library(shinyjs)
library(shinyTime)
library(leaflet)
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

ui <- app(
    useShinyjs(),
    setDefaultTab("activeTab", "home"),
    stackElements(
        ##### FIXME: Start of hacky section #########
        # TODO: Combine to mapbox
        setDefaultTab("mapProvider", "mapbox"),
        conditionalPanel(
            condition = "input.mapProvider === 'mapbox'",
            mapboxerOutput("map", height = "100vh")
        ),
        conditionalPanel(
            condition = "input.mapProvider === 'leaflet'",
            leafletOutput("leafletMap", height = "100vh")
        ),
        ##### FIXME: End of hacky section #########
        column(
            box(
                style = "width: fit-content; position: absolute; top: 0; left: 0;",
                appTitle("kiasutraveler")
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
                style = "width: fit-content; position: absolute; bottom: 0; right: 0; max-height: 75%; overflow-y: auto; max-width: 50%",
                homeTabContent,
                rideHailingTabContent,
                carSharingTabContent,
                taxiTabContent,
                publicTransportTabContent,
                othersTabContent
            )
        ),
        ##### FIXME: Start of hacky section #########
        # TODO: Refactor
        tags$div(
            id = "modalOverlay",
            # FIXME: For some reason removing is-active prevents the modal from updating
            class = "modal is-active",
            tags$div(class = "modal-background"),
            tags$div(
                class = "modal-content pt-3 px-4",
                tableOutput("modalContentBody")
            ),
            tags$button(
                class = "modal-close is-large",
                onclick = "$('#modalOverlay').removeClass('is-active');"
            )
            ##### FIXME: End of hacky section #########
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
