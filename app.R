library(hms)
library(jsonlite)
library(shiny)
library(shinyjs)
library(shinyTime)
library(mapboxer)

source("api/functions.R")
source("ui/components/bulma.R")
source("logic/maps/maps.R")

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
                    tab("grab", "Grab"),
                    tab("taxi", "Taxi"),
                    tab("getgo", "Getgo"),
                    tab("pt", "Public Transport"),
                )
            ),
            box(
                style = "width: fit-content; position: absolute; bottom: 0; right: 0",
                conditionalPanel(
                    condition = "input.activeTab == 'taxi'",
                    columns(
                        column(
                            dateInput(
                                inputId = "date",
                                label = "Select Date:",
                                format = "yyyy-mm-dd"
                            ),
                            timeInput(
                                inputId = "time",
                                label = "Select Time:",
                                value = as_hms(Sys.time())
                            )
                        ),
                        column(
                            radioButtons(
                                inputId = "layer",
                                label = "Select Map Type:",
                                choices = c("Point", "Heatmap")
                            ),
                            selectInput(
                                inputId = "mapType",
                                label = "Select Base Map Style:",
                                choices = basemap_types,
                                selected = "voyager"
                            )
                        )
                    ),
                    htmlOutput("description")
                ),
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

    data <- reactive({
        # TODO: Remove this in production
        print("Firing API call...")
        fromJSON(formatUrl(input, "date", "time"))
    })

    df <- reactive({
        df <- data()$features$geometry$coordinates %>% data.frame()

        colnames(df) <- c("lng", "lat")
        df$type <- "Taxi"

        return(df)
    })

    getSingaporeMap <- reactive(
        mapboxer(
            style = getBasemap(input$mapType),
            center = COORDINATES_SINGAPORE,
            zoom = 10,
            pitch = 15,
            minZoom = 7
        )
    )

    output$description <- renderText(paste(
        "Getting taxi availability on",
        strong(input$date),
        "at",
        strong(getTime(input, "time")),
        "as a",
        strong(input$layer),
        "..."
    ))

    output$map <- renderMapboxer({
        mapbox <- getSingaporeMap() %>% add_navigation_control(pos = "bottom-left")
        if (input$layer == "Heatmap") {
            mapbox <- mapbox %>%
                add_layer(
                    list(
                        id = "heatmap_layer",
                        type = "heatmap",
                        source = as_mapbox_source(df()),
                        paint = list(
                            "heatmap-opacity" = 0.3,
                            "heatmap-radius" = 10
                        )
                    )
                )
        } else if (input$layer == "Point") {
            mapbox <- mapbox %>%
                add_circle_layer(
                    id = "taxis_points",
                    source = as_mapbox_source(df()),
                    circle_color = "red",
                    circle_radius = 4,
                    circle_opacity = 0.2
                ) %>%
                add_tooltips("taxis_points", "{{type}}")
        }
    })
}

shinyApp(ui, server, options = list(
    port = 4000
    # launch.browser = FALSE
))
