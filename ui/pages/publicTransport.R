library(shiny)

publicTransportTabContent <- conditionalPanel(
    condition = "input.activeTab === 'publicTransport'",
    p("PublicTransport content coming soon!")
)
