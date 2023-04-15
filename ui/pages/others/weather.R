library(shiny)

weatherTabContent <- conditionalPanel(
    condition = "input.othersTab === 'weather'",
    p("Weather tab data coming soon")
)

updateWeatherTab <- function(input, output) {}
