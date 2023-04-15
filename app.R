library(shiny)
library(shinyjs)
library(shinyTime)
library(mapboxer)

source("api/functions.R")
source("ui/components/bulma.R")

source("ui/pages/taxi.R")

options(shiny.autoreload = TRUE)

ui <- app(
    useShinyjs(),
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
                    "transportModes",
                    # TODO: Fix IDs and add reactivity
                    tab("rideHailing", "Ride Hailing"),
                    tab("carSharing", "Car-Sharing"),
                    tab("taxi", "Taxi"),
                    tab("publicTransport", "Public Transport"),
                    tab("others", "Others")
                )
            ),
            box(
                style = "width: fit-content; position: absolute; bottom: 0; right: 0",
                taxiTabContent,
                conditionalPanel(
                    condition = "input.activeTab !== 'taxi'",
                    p("Coming soon!")
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
    })

    updateTaxiTab(input, output)
}

shinyApp(ui, server, options = list(
    port = 4000,
    host = "0.0.0.0"
    # launch.browser = FALSE
))
