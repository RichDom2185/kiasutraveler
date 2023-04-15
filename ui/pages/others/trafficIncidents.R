library(shiny)

trafficIncidentsTabContent <- conditionalPanel(
    condition = "input.othersTab === 'trafficIncidents'",
    # Wordcloud
    h2("Traffic Incident Causes in Singapore", style = "font-weight:bold; font-size:20px;"), br(),
    p("The following wordcloud illustrates some of the most common traffic incident causes in Singapore from 2012 to 2018. The most common cause appears to be Diversion and Accidents"),
    br(),
    p("Incidents categorised under 'Diversion' include 'Turning / Changing Lanes without Due Care'."), br(),
    p("Incidents categorised under 'Accidents' include 'Disobeying Traffic Signals' or 'Failing to give way to a pedestrian'."), br(),
    p("Take account of these statistics when you are on the road!"),
    plotOutput("wordcloud"),
    # Live Incident Causes
    h1("Live Traffic Incident Causes", style = "font-weight:bold; font-size:20px;"), br(),
    p("The bar graph illustrates live traffic incident causes right now. The data is updated every two minutes."),
    br(),
    p("Observe the various incidents happening now to get a better idea of the road congestion situation."),
    p("Click on the 'Live Traffic Incidents' tab to find out where these incidents occured!"), br(),
    strong(paste(
        "Getting traffic incident information on", format(Sys.Date(), "%A, %B %d, %Y"),
        "at", format(Sys.time(), "%I:%M %p"), "..."
    )),
    plotOutput("incidentbar")
    # TODO: Live Traffic Incidents
)

updateTrafficIncidentsTab <- function(input, output) {
}
