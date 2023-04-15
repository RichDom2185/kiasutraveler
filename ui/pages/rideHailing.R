library(shiny)

rideHailingTabContent <- conditionalPanel(
    condition = "input.activeTab === 'rideHailing'",
    p("RideHailing content coming soon!")
)

updateRideHailingTab <- function(input, output) {
    # TODO: Fill up
}
