library(shiny)

carSharingTabContent <- conditionalPanel(
    condition = "input.activeTab === 'carSharing'",
    p(
        "Each red point on the map represents a GetGo vehicle that is
        available for booking, while each blue point represents a BlueSG
        station that has cars available for booking."
    ),
    br(),
    p(
        "Hover over the points to view more information about each of them."
    )
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
                # TODO: More information (e.g. price per km)
                strong("{{vehicleName}}"),
                br(),
                em("GetGo ({{vehicleNo}})"),
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
                em("BlueSG Station"),
                br(),
                strong("{{numCars}} available")
            ))
    })
}
