library(jsonlite)
library(mapboxer)
library(shiny)

source("logic/maps/maps.R")
source("logic/utils/functions.R")

## FUNCTIONS

getBuildingNameFromPostalCode <- function(postalCode) {
    data <- getCoordinatesFromAddress(postalCode)$results[1, ]
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
    fromLocation <- getCoordinatesFromAddress(fromPostalCode)$results[1, ]
    toLocation <- getCoordinatesFromAddress(toPostalCode)$results[1, ]

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
    p(em(
        strong("Note:"),
        "If you encounter any errors in this app, please come
        back to the home page, key in known VALID postal codes,
        and press the 'Take me there!' button again."
    )),
    br(),
    columns(
        column(input.text("pickUp", "Enter postal code...", label = "Pick-up from:")),
        column(input.text("dropOff", "Enter postal code...", label = "Destination to:"))
    ),
    actionButton("runButton", "Take me there!", class = "button is-light is-fullwidth"),
    br(),
    htmlOutput("homeDescription"),
    uiOutput("homeComparisonTable")
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
                    popup = paste0(
                        strong("Start location:"),
                        br(),
                        getCoordinatesFromAddress(isolate(input$pickUp))$results[1, ]$name
                    )
                ) %>%
                add_marker(
                    tail(waypoints(), 1L)[1],
                    tail(waypoints(), 1L)[2],
                    popup = paste0(
                        strong("End location:"),
                        br(),
                        getCoordinatesFromAddress(isolate(input$dropOff))$results[1, ]$name
                    )
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
                        ##### FIXME: Start of hacky section #########
                        modalData$"Driving" <- result$waypoints %>%
                            geosphere::distGeo() %>%
                            sum(na.rm = TRUE) %>%
                            "/"(1000) %>%
                            round(2) %>%
                            paste("km")
                        modalData$"Public Transport" <- getPublicTransportDirectionsBetweenPostalCodes(pickUp, dropOff)$legs %>%
                            lapply(function(legs_list) {
                                do.call(rbind, legs_list$waypoints) %>%
                                    geosphere::distGeo() %>%
                                    sum(na.rm = TRUE)
                            }) %>%
                            range() %>%
                            "/"(1000) %>%
                            round(2) %>%
                            paste("km", collapse = "-")
                        ##### FIXME: End of hacky section #########
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
    ##### FIXME: Start of hacky section #########
    output$homeComparisonTable <- renderUI({
        pickUp <- isolate(input$pickUp)
        dropOff <- isolate(input$dropOff)

        # FIXME: For some reason, combining the conditions using && doesn't work
        if (input$runButton > 0) {
            if (isValidPostalCode(pickUp)) {
                if (isValidPostalCode(dropOff)) {
                    # TODO: Refactor
                    tags$button(
                        id = "showComparisonTableButton",
                        class = "button is-light is-fullwidth",
                        onclick = "$('#modalOverlay').addClass('is-active');",
                        "View summary comparison",
                        # TODO: Add nbsp
                        icon("arrow-right")
                    )
                }
            }
        }
    })

    modalData <- reactiveValues()

    observeEvent(input$runButton, {
        output$modalContentBody <- renderTable(
            width = "100%",
            {
                data <- modalData
                elements <- data %>%
                    reactiveValuesToList() %>%
                    isolate()

                # print(elements %>% data.frame() %>% t())
                df <- elements %>%
                    data.frame() %>%
                    t() %>%
                    data.frame()

                print(df)

                # FIXME: Remove hardcoding
                if (ncol(df) > 0) {
                    colnames(df)[1] <- "Distance"
                    df$"Service" <- row.names(df)
                    df[, c("Service", "Distance")]
                }
            }
        )
    })
    ##### FIXME: End of hacky section #########
}
