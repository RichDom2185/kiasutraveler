library(shiny)

## FUNCTIONS

getServicesBetweenPostalCodes <- function(fromPostalCode, toPostalCode) {
    fromLocation <- getCoordinatesFromAddress(fromPostalCode)[[1]]
    toLocation <- getCoordinatesFromAddress(toPostalCode)[[1]]

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
