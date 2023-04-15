library(mapboxer)
library(shiny)

source("logic/maps/maps.R")

## FUNCTIONS

getBuildingNameFromPostalCode <- function(postalCode) {
    data <- getCoordinatesFromAddress(postalCode)[[1]]
    paste0(
        data$name, ", ",
        data$block, " ", data$roadName,
        " (", data$postalCode, ")"
    )
}

## UI AND LOGIC

homeTabContent <- conditionalPanel(
    condition = "input.activeTab === 'home'",
    p("Welcome to kiasutraveler! Where would you like to go today?"),
    HTML("<br>"),
    columns(
        column(input.text("pickUp", "Enter postal code...", label = "Pick-up from:")),
        column(input.text("dropOff", "Enter postal code...", label = "Destination to:"))
    ),
    actionButton("runButton", "Take me there!", class = "button is-light is-fullwidth"),
    HTML("<br>"),
    htmlOutput("homeDescription")
)

updateHomeTab <- function(input, output) {
    # TODO: Refactor
    output$map <- renderMapboxer({
        mapboxer(
            # TODO: Remove hardcoding
            style = getBasemap("voyager"),
            center = COORDINATES_SINGAPORE,
            zoom = 10,
            pitch = 15,
            minZoom = 7
        ) %>% add_navigation_control(pos = "bottom-left")
    })

    output$homeDescription <- renderText({
        pickUp <- isolate(input$pickUp)
        dropOff <- isolate(input$dropOff)

        # TODO: Refactor out into utils
        isValidPostalCode <- function(postalCode) {
            grepl("^[0-9]{6}$", postalCode)
        }

        ifelse(
            input$runButton == 0,
            "Enter your pick-up and destination postal codes to get started!",
            ifelse(
                !isValidPostalCode(pickUp) || !isValidPostalCode(dropOff),
                "Please enter valid 6-digit postal codes!",
                paste(
                    "Searching for directions from",
                    strong(getBuildingNameFromPostalCode(pickUp)),
                    "to",
                    strong(getBuildingNameFromPostalCode(dropOff))
                )
            )
        )
    })
}
