library(shiny)

crowdingTabContent <- conditionalPanel(
    condition = "input.othersTab === 'crowding'",
    p("Crowding tab data coming soon")
)

updateCrowdingTab <- function(input, output) {}
