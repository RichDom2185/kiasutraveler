library(shiny)

publicTransportTabContent <- conditionalPanel(
    condition = "input.activeTab === 'publicTransport'",
    p("PublicTransport content coming soon!")
)

updatePublicTransportTab <- function(input, output) {
    # TODO: Fill up
}
