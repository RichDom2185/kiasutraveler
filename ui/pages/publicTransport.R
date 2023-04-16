library(dplyr)
library(hms)
library(mapboxer)
library(shiny)

## FUNCTIONS

getPublicTransportDirectionsBetweenPostalCodes <- function(fromPostalCode, toPostalCode) {
    fromLocation <- getCoordinatesFromAddress(fromPostalCode)$results[1, ]
    toLocation <- getCoordinatesFromAddress(toPostalCode)$results[1, ]

    result <- do.call(getPublicTransportDirections, list(
        fromLocation$lat,
        fromLocation$lng,
        toLocation$lat,
        toLocation$lng,
        format(Sys.time(), "%Y-%m-%d"),
        format(Sys.time(), "%H:%M:%S")
    ))

    result <- result$itineraries
    result$waypoints <- lapply(result$legs, function(legs_list) {
        do.call(rbind, legs_list$waypoints) %>%
            points_to_lines() %>%
            toJSON(auto_unbox = TRUE)
    })

    result
}

## UI AND LOGIC

publicTransportTabContent <- conditionalPanel(
    condition = "input.activeTab === 'publicTransport'",
    p("PublicTransport content coming soon!")
)

updatePublicTransportTab <- function(input, output) {
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

        pickUp <- isolate(input$pickUp)
        dropOff <- isolate(input$dropOff)
        data <- getPublicTransportDirectionsBetweenPostalCodes(pickUp, dropOff)
        # Create n discrete colors
        data$colors <- rainbow(nrow(data))

        for (i in seq_len(nrow(data))) {
            layer_id <- paste0("itinerary-", i)
            map <- map %>%
                add_line_layer(
                    id = layer_id,
                    source = data$waypoints[[i]] %>% as_mapbox_source(),
                    line_color = data$colors[[i]],
                    line_opacity = 0.4,
                    line_dasharray = c(3, i * 0.5),
                    line_width = 5
                ) %>%
                add_tooltips(layer_id, paste0(
                    strong(paste("Route Option", i)),
                    br(),
                    "Fare: $", data$fare[[i]],
                    br(),
                    "Duration: ", as_hms(data$duration[[i]])
                ))
        }

        map
    })
}
