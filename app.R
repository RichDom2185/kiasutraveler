library(shiny)
library(shinyjs)
library(shinyTime)
library(mapboxer)

source("api/functions.R")
source("ui/components/bulma.R")

source("logic/utils/functions.R")

source("ui/pages/rideHailing.R")
source("ui/pages/carSharing.R")
source("ui/pages/taxi.R")
source("ui/pages/publicTransport.R")
source("ui/pages/others.R")

options(shiny.autoreload = TRUE)

ui <- app(
    useShinyjs(),
    setDefaultTab("activeTab", "taxi"),
    stackElements(
        mapboxerOutput("map", height = "100vh"),
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
                    tab("rideHailing", "Ride Hailing"),
                    tab("carSharing", "Car-Sharing"),
                    tab("taxi", "Taxi"),
                    tab("publicTransport", "Public Transport"),
                    tab("others", "Others")
                )
            ),
            box(
                style = "width: fit-content; position: absolute; bottom: 0; right: 0",
                rideHailingTabContent,
                carSharingTabContent,
                taxiTabContent,
                publicTransportTabContent,
                othersTabContent
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
