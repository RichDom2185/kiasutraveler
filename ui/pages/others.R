library(shiny)
library(shinyjs)

source("ui/pages/others/trafficIncidents.R")

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
