library(dplyr)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(sf)
library(shiny)

weatherTabContent <- conditionalPanel(
    condition = "input.othersTab === 'weather'",
    ##### FIXME: Start of hacky section #########
    # TODO: Remove hacky workaround and combine to mapbox
    # FIXME: Remove hardcoding
    # FIXME: Move state logic to server to follow Reactive principles.
    HTML(paste0(
        "<script defer>
        $(document).on('shiny:inputchanged', function(e) {
            switch (e.name) {
                case 'activeTab':
                    if (e.value !== 'others') {
                        Shiny.setInputValue('mapProvider', 'mapbox');
                    }
                    break;
                case 'othersTab':
                    if (e.value === 'weather') {
                        // Leave it to mapProvider to be set manually,
                        // as the selectInput is shown when this is the state.
                        return;
                    }
                    Shiny.setInputValue('mapProvider', 'mapbox');
                    break;
                default:
                    return;
            }
        });
        </script>"
    )),
    # TODO: The base map shows the live rainfall map
    selectInput("mapProvider",
        "Select Map Type:",
        choices = c(
            "Live Rainfall" = "mapbox",
            "Weather Forecast" = "leaflet"
        )
    ),
    ##### FIXME: End of hacky section #########
    plotOutput("weatherPlot")
)

updateWeatherTab <- function(input, output) {
    ##### FIXME: Hacky way to force a map reset when map provider is changed #########
    observeEvent(input$mapProvider, {
        output$map <- renderMapboxer({
            # TODO: Use own API
            url_rainfallLive <- "https://api.data.gov.sg/v1/environment/rainfall"
            data <- fromJSON(url_rainfallLive)
            coordinates_rainfallLive <- as.data.frame(data$metadata$stations$location)
            value_rainfallLive <- as.data.frame(data$items$readings[[1]]$value)
            data_rainfallLive <- cbind(coordinates_rainfallLive, value_rainfallLive)
            names(data_rainfallLive)[names(data_rainfallLive) == "data$items$readings[[1]]$value"] <- "rainfall"
            data_rainfallLive$rainfall <- data_rainfallLive$rainfall * 100

            COORDINATES_SINGAPORE <- c(103.8198, 1.3521)

            # Adapted from https://crazycapivara.github.io/mapboxer/articles/examples/showcase.html
            df_rain_sf <- sf::st_as_sf(
                data_rainfallLive,
                coords = c("longitude", "latitude"),
                crs = 4326
            )

            sf::sf_use_s2(TRUE)

            grid_sf <- sf::st_make_grid(df_rain_sf, square = TRUE)[df_rain_sf] %>%
                sf::st_sf()

            grid_sf %<>% dplyr::mutate(
                count = sapply(st_intersects(grid_sf, df_rain_sf), function(x) sum(df_rain_sf[x, ]$rainfall)),
                color = scales::col_numeric(palette = "Blues", count)(count)
            )

            rainfall_map <- as_mapbox_source(grid_sf) %>%
                mapboxer(
                    style = basemaps$Carto$dark_matter, center = COORDINATES_SINGAPORE, zoom = 10,
                    bounds = sf::st_bbox(grid_sf),
                    fitBoundsOptions = list(padding = 20)
                ) %>%
                add_navigation_control() %>%
                add_fill_layer(
                    fill_color = c("get", "color"),
                    fill_antialias = FALSE,
                    fill_opacity = 0.4,
                    popup = "Rainfall: {{count}}"
                )

            rainfall_map
        })


        output$leafletMap <- renderLeaflet({
            ### data source ###
            # TODO: Use own API
            url_forecast <- "https://api.data.gov.sg/v1/environment/2-hour-weather-forecast"
            data_rainfallForecast <- fromJSON(url_forecast)
            data_twohour <- cbind(
                as.data.frame(data_rainfallForecast$items$forecasts),
                as.data.frame(data_rainfallForecast$area_metadata$label_location)
            )

            #### visualisation ###
            data_twohour$forecast <- gsub("\\s*\\(.+", "", data_twohour$forecast)
            data_twohour$forecast <- as.factor(data_twohour$forecast)


            # TODO: check if we need grepl
            icons <- icons(
                iconUrl = sapply(data_twohour$forecast, function(v) {
                    switch(as.character(v),
                        "Sunny" = "https://cdn-icons-png.flaticon.com/128/4150/4150875.png",
                        "Cloudy" = "https://cdn-icons-png.flaticon.com/128/4150/4150884.png",
                        "Partly Cloudy" = "https://cdn-icons-png.flaticon.com/128/4150/4150891.png",
                        "Light Showers" = "https://cdn-icons-png.flaticon.com/128/4150/4150904.png",
                        "Light Rain" = "https://cdn-icons-png.flaticon.com/128/4150/4150904.png",
                        "Showers" = "https://cdn-icons-png.flaticon.com/128/4150/4150897.png",
                        "Moderate Rain" = "https://cdn-icons-png.flaticon.com/128/4150/4150897.png",
                        "Thundery Showers" = "https://cdn-icons-png.flaticon.com/128/4150/4150944.png"
                        # TODO: default fallback icon
                    )
                }),
                iconWidth = 38,
                iconHeight = 38
            )

            twohours <- leaflet(data_twohour) %>%
                setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
                addTiles() %>%
                addMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    popup = ~ paste(area, forecast, sep = ": "),
                    icon = icons
                )

            twohours
        })
    })

    output$weatherPlot <- renderPlot({
        rainfall_data <- read.csv("data/MonthlyRainfall.csv")
        monthly_avg <- rainfall_data %>%
            tidyr::separate(month, into = c("year", "month"), sep = "-") %>%
            mutate(year = as.numeric(year), month = as.factor(month)) %>%
            # filter the data for time period from 2013 to 2022
            filter(year >= 2013 & year <= 2022) %>%
            # calculate average number of rainy days for each month
            group_by(month) %>%
            summarise(ave_no_of_rainy_days = mean(no_of_rainy_days))
        levels(monthly_avg$month) <- base::month.abb

        # plot the monthly averages
        ggplot(monthly_avg, aes(x = month, y = ave_no_of_rainy_days, fill = ave_no_of_rainy_days)) +
            geom_bar(stat = "identity") +
            scale_fill_gradient(low = "#0171BB", high = "#E76963") +
            ggtitle("Average Monthly Rainy Days from 2013 to 2022") +
            ylab("Average Number of Rainy Days") +
            xlab("Month") +
            theme_classic() +
            scale_y_continuous(expand = c(0, 0))
    })
}
