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
                id = "getgo-layer",
                source = getAvailableGetgoVehicles()$data %>% as_mapbox_source(),
                circle_color = "red",
                circle_opacity = 0.3,
                circle_radius = 5
            ) %>%
            add_tooltips("getgo-layer", paste0(
                strong("{{vehicleName}}"),
                br(),
                tags$em("GetGo ({{vehicleNo}})"),
                br(),
                "Capacity: {{capacity}}"
            )) %>%
            add_circle_layer(
                id = "bluesg-layer",
                source = getAvailableBluesgStations()$stations %>%
                    filter(numCars > 0) %>%
                    mutate(circleRadius = sqrt(numCars) * 5) %>%
                    as_mapbox_source(),
                circle_color = "blue",
                circle_opacity = 0.3,
                circle_radius = c("get", "circleRadius")
            ) %>%
            add_tooltips("bluesg-layer", paste0(
                tags$em("BlueSG Station"),
                br(),
                strong("{{numCars}} available")
            ))
    })
}
