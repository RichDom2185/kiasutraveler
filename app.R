library(shiny)
library(mapboxer)

source("ui/components.R")
source("logic/maps/maps.R")

ui <- app(
    appTitle("Taxi Availability Data in Singapore"),
    box(
        strong("Taxi Availability Data:"),
        mapboxerOutput("map")
    )
)

server <- function(input, output) {
    getSingaporeMap <- reactive(
        mapboxer(
            style = getBasemap(),
            center = COORDINATES_SINGAPORE,
            zoom = 10,
            pitch = 15,
            minZoom = 7
        )
    )

    output$map <- renderMapboxer(
        getSingaporeMap()
    )
}

shinyApp(ui, server, options = list(
    port = 4000
    # launch.browser = FALSE
))
