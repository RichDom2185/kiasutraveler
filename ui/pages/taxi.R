taxiTabContent <- conditionalPanel(
    condition = "input.activeTab == 'taxi'",
    columns(
        column(
            dateInput(
                inputId = "date",
                label = "Select Date:",
                format = "yyyy-mm-dd"
            ),
            timeInput(
                inputId = "time",
                label = "Select Time:",
                value = as_hms(Sys.time())
            )
        ),
        column(
            radioButtons(
                inputId = "layer",
                label = "Select Map Type:",
                choices = c("Point", "Heatmap")
            ),
            selectInput(
                inputId = "mapType",
                label = "Select Base Map Style:",
                choices = basemap_types,
                selected = "voyager"
            )
        )
    ),
    htmlOutput("description")
)
