library(shiny)

## FUNCTIONS

getServicesBetweenPostalCodes <- function(fromPostalCode, toPostalCode) {
    fromLocation <- getCoordinatesFromAddress(fromPostalCode)$results[1, ]
    toLocation <- getCoordinatesFromAddress(toPostalCode)$results[1, ]

    result <- do.call(getGrabServicesAvailability, list(
        fromLocation$lat,
        fromLocation$lng,
        toLocation$lat,
        toLocation$lng
    ))
    result$services
}

## UI AND LOGIC

rideHailingTabContent <- conditionalPanel(
    condition = "input.activeTab === 'rideHailing'",
    p(
        "The following ride-hailing services from Grab are available
        between your selected start and end locations:"
    ),
    htmlOutput("rideHailingDescription"),
    tableOutput("rideHailingTable")
)

updateRideHailingTab <- function(input, output) {
    output$rideHailingTable <- renderTable({
        withProgress(message = "Checking available Grab services...", {
            pickUp <- isolate(input$pickUp)
            dropOff <- isolate(input$dropOff)

            getServicesBetweenPostalCodes(pickUp, dropOff)
        })
    })
}
