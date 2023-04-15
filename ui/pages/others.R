library(shiny)

othersTabContent <- conditionalPanel(
    condition = "input.activeTab === 'others'",
    p("Others content coming soon!")
)

updateOthersTab <- function(input, output) {
    # TODO: Fill up
}
