library(shiny)

carSharingTabContent <- conditionalPanel(
    condition = "input.activeTab === 'carSharing'",
    p("CarSharing content coming soon!")
)

updateCarSharingTab <- function(input, output) {
    # TODO: Refactor
    output$map <- renderMapboxer({
        map <- mapboxer(
            # TODO: Remove hardcoding
            style = getBasemap("voyager"),
            center = COORDINATES_SINGAPORE,
            zoom = 10,
            pitch = 15,
            minZoom = 7
        ) %>%
            add_navigation_control(pos = "bottom-left") %>%
            add_circle_layer(
                source = getAvailableGetgoVehicles()$data %>% as_mapbox_source(),
                popup = paste0(
                    strong("{{vehicleName}}"),
                    "<br>",
                    tags$em("{{vehicleNo}}"),
                    "<br>Capacity: {{capacity}}"
                ),
                circle_color = "red",
                circle_radius = 5
            )
    })
}
