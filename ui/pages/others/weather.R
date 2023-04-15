library(dplyr)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(shiny)

weatherTabContent <- conditionalPanel(
    condition = "input.othersTab === 'weather'",
    leafletOutput("weatherMap"),
    plotOutput("weatherPlot")
)

updateWeatherTab <- function(input, output) {
    output$weatherMap <- renderLeaflet({
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
