library(shiny)

carSharingTabContent <- conditionalPanel(
    condition = "input.activeTab === 'carSharing'",
    p("CarSharing content coming soon!")
)
