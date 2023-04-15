library(shiny)

trafficIncidentsTabContent <- conditionalPanel(
    condition = "input.othersTab === 'trafficIncidents'",
    radioButtons(
        inputId = "incidentPlotType",
        label = "Select Map Type:",
        choices = c("Word Cloud", "Bar Chart")
    ),
    htmlOutput("incidentsDescription"),
    plotOutput("incidentsPlot")
    # TODO: Live Traffic Incidents
)

updateTrafficIncidentsTab <- function(input, output) {
    incidentsDescription <- reactiveVal()

    observeEvent(input$incidentPlotType, {
        # Update the description
        incidentsDescription(switch(input$incidentPlotType,
            # Wordcloud
            "Word Cloud" = {
                paste0(
                    h3(strong("Traffic Incident Causes in Singapore")),
                    br(),
                    p(
                        "The following word cloud illustrates some of the most common traffic
                        incident causes in Singapore from 2012 to 2018. The most common cause
                        appears to be Diversion and Accidents."
                    ),
                    br(),
                    p(
                        "Incidents categorised under 'Diversion' include 'Turning / Changing
                        Lanes without Due Care'."
                    ),
                    br(),
                    p(
                        "Incidents categorised under 'Accidents' include 'Disobeying Traffic
                        Signals' or 'Failing to give way to a pedestrian'."
                    ),
                    br(),
                    p(
                        "Take account of these statistics when you are on the road!"
                    )
                )
            },
            # Live Incident Causes
            "Bar Chart" = {
                paste0(
                    h3(strong("Live Traffic Incident Causes")),
                    br(),
                    p(
                        "The bar graph illustrates live traffic incident causes right now. The
                        data is updated every two minutes."
                    ),
                    br(),
                    p(
                        "Observe the various incidents happening now to get a better idea of
                        the road congestion situation."
                    ),
                    br(),
                    p(
                        "Click on the 'Live Traffic Incidents' tab to find out where these
                        incidents occured!"
                    ),
                    br(),
                    p(strong(paste(
                        "Getting traffic incident information on",
                        format(Sys.Date(), "%A, %B %d, %Y"),
                        "at", format(Sys.time(), "%I:%M %p"), "..."
                    )))
                )
            },
            # default
            ""
        ))
    })

    output$incidentsDescription <- renderText(incidentsDescription())
}
