library(shiny)

trafficIncidentsTabContent <- conditionalPanel(
    condition = "input.othersTab === 'trafficIncidents'",
    h1("Live Traffic Incident Causes", style = "font-weight:bold; font-size:20px;"), br(),
    p("The bar graph illustrates live traffic incident causes right now. The data is updated every two minutes."),
    br(),
    p("Observe the various incidents happening now to get a better idea of the road congestion situation."),
    p("Click on the 'Live Traffic Incidents' tab to find out where these incidents occured!"), br(),
    strong(paste(
        "Getting traffic incident information on", format(Sys.Date(), "%A, %B %d, %Y"),
        "at", format(Sys.time(), "%I:%M %p"), "..."
    ))
)

updateTrafficIncidentsTab <- function(input, output) {
}
