library(shiny)
library(shinyjs)

source("ui/pages/others/weather.R")
source("ui/pages/others/crowding.R")
source("ui/pages/others/trafficIncidents.R")

othersTabContent <- list(
    setDefaultTab("othersTab", "weather"),
    conditionalPanel(
        condition = "input.activeTab === 'others'",
        tabs(
            "othersTab",
            tab("weather", "Weather"),
            tab("crowding", "Crowding"),
            tab("trafficIncidents", "Traffic Incidents")
        ),
        weatherTabContent,
        crowdingTabContent,
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
            "weather" = updateWeatherTab(input, output),
            "crowding" = updateCrowdingTab(input, output),
            "trafficIncidents" = updateTrafficIncidentsTab(input, output),
            # default
            warning("Invalid/no tab selected")
        )
    })
}
