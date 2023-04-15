library(jsonlite)
library(mapboxer)
library(shiny)

source("logic/maps/maps.R")
source("logic/utils/functions.R")

## FUNCTIONS

getBuildingNameFromPostalCode <- function(postalCode) {
    data <- getCoordinatesFromAddress(postalCode)[[1]]
    paste0(
        data$name, ", ",
        data$block, " ", data$roadName,
        " (", data$postalCode, ")"
    )
}

# TODO: Refactor out into utils
isValidPostalCode <- function(postalCode) {
    grepl("^[0-9]{6}$", postalCode)
}

getDirectionsBetweenPostalCodes <- function(fromPostalCode, toPostalCode) {
    fromLocation <- getCoordinatesFromAddress(fromPostalCode)[[1]]
    toLocation <- getCoordinatesFromAddress(toPostalCode)[[1]]

    result <- do.call(getDrivingDirections, list(
        fromLocation$lat,
        fromLocation$lng,
        toLocation$lat,
        toLocation$lng
    ))
    result
}

## UI AND LOGIC

homeTabContent <- conditionalPanel(
    condition = "input.activeTab === 'home'",
    p("Welcome to kiasutraveler! Where would you like to go today?"),
    br(),
    columns(
        column(input.text("pickUp", "Enter postal code...", label = "Pick-up from:")),
        column(input.text("dropOff", "Enter postal code...", label = "Destination to:"))
    ),
    actionButton("runButton", "Take me there!", class = "button is-light is-fullwidth"),
    br(),
    htmlOutput("homeDescription")
)

updateHomeTab <- function(input, output) {
    waypoints <- reactiveVal(data.frame())

    # TODO: Refactor
    output$map <- renderMapboxer({
        map <- mapboxer(
            # TODO: Remove hardcoding
            style = getBasemap("voyager"),
            center = COORDINATES_SINGAPORE,
            zoom = 10,
            pitch = 15,
            minZoom = 7
        ) %>% add_navigation_control(pos = "bottom-left")

        if (nrow(waypoints()) > 0) {
            map <- map %>%
                add_line_layer(
                    source = waypoints() %>%
                        points_to_lines() %>%
                        toJSON(auto_unbox = TRUE) %>%
                        as_mapbox_source(),
                    line_color = "red",
                    line_width = 5
                ) %>%
                add_marker(
                    head(waypoints(), 1L)[1],
                    head(waypoints(), 1L)[2],
                    popup = paste("Pick-up:", getCoordinatesFromAddress(input$pickUp)[[1]]$name)
                ) %>%
                add_marker(
                    tail(waypoints(), 1L)[1],
                    tail(waypoints(), 1L)[2],
                    popup = paste("Destination:", getCoordinatesFromAddress(input$dropOff)[[1]]$name)
                )
        } else {
            map
        }
    })

    output$homeDescription <- renderText({
        pickUp <- isolate(input$pickUp)
        dropOff <- isolate(input$dropOff)

        ifelse(
            input$runButton == 0,
            "Enter your pick-up and destination postal codes to get started!",
            ifelse(
                !isValidPostalCode(pickUp) || !isValidPostalCode(dropOff),
                "Please enter valid 6-digit postal codes!",
                {
                    # Logic for routing
                    # FIXME: This should be a separate function
                    withProgress(message = "Getting directions...", {
                        result <- getDirectionsBetweenPostalCodes(pickUp, dropOff)
                        waypoints(result$waypoints)
                    })
                    # Actual text
                    paste(
                        "Searching for directions from",
                        strong(getBuildingNameFromPostalCode(pickUp)),
                        "to",
                        strong(getBuildingNameFromPostalCode(dropOff))
                    )
                }
            )
        )
    })
}
