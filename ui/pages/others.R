library(shiny)
library(shinyjs)

## FUNCTIONS

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

## UI AND LOGIC

othersTabContent <- list(
    setDefaultTab("othersTab", "trafficIncidents"),
    conditionalPanel(
        condition = "input.activeTab === 'others'",
        tabs(
            "othersTab",
            tab("trafficIncidents", "Traffic Incidents")
        ),
        trafficIncidentsTabContent
    )
)

updateOthersTab <- function(input, output) {
    currentOthersTab <- reactiveVal()
    observeEvent(input$othersTab, {
        removeCssClass(id = currentOthersTab(), class = "is-active")
        currentOthersTab(input$othersTab)
        addCssClass(id = currentOthersTab(), class = "is-active")

        switch(input$othersTab,
            "trafficIncidents" = updateTrafficIncidentsTab(input, output),
            # default
            warning("Invalid/no tab selected")
        )
    })
}
