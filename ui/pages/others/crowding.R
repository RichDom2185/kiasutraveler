library(dplyr)
library(ggplot2)
library(shiny)

crowdingTabContent <- conditionalPanel(
    condition = "input.othersTab === 'crowding'",
    strong("Bus Crowding Plot"),
    plotOutput("busCrowdingPlot", width = "600px", height = "400px"),
    p("Buses are the most crowded from 7am to 8am and 5pm to 6pm on weekdays. 
    Buses are consistently crowded throughout the day on weekends especially 
    5pm to 7pm. However, you will definitely see less people at the platforms. 
    If possible, plan your travel time accordingly if you wish to avoid the crowd!"),
    br(),
    strong("MRT Crowding Plot"),
    plotOutput("mrtCrowdingPlot", width = "600px", height = "400px"),
    p("MRT is the most crowded from 7am to 8am and 5pm to 6pm on weekdays. 
    MRT is consistently crowded throughout the day on weekends but you will 
    definitely see less people at the platforms. If possible, plan your travel time accordingly 
    if you wish to avoid the crowd!")
)

updateCrowdingTab <- function(input, output) {
    output$busCrowdingPlot <- renderPlot({
        bus_psgvol <- read.csv("data/transport_node_bus_202302.csv")
        grouped_bus_psgvol <- bus_psgvol %>%
            group_by(TIME_PER_HOUR, DAY_TYPE) %>%
            summarize(TOTAL_TAP_IN_VOLUME = sum(TOTAL_TAP_IN_VOLUME))

        busPassengerVolPlot <- ggplot(grouped_bus_psgvol, aes(x = TIME_PER_HOUR, y = TOTAL_TAP_IN_VOLUME, fill = DAY_TYPE)) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~DAY_TYPE, nrow = 2) +
            labs(x = "Time in hour", y = "Total tap in volume") +
            scale_x_continuous(breaks = seq(0, 23, 1)) +
            scale_y_continuous(labels = scales::comma)

        busPassengerVolPlot
    })

    # Other Insights => Crowd => MRT Crowd
    output$mrtCrowdingPlot <- renderPlot({
        train_psgvol <- read.csv("data/transport_node_train_202302.csv")
        grouped_train_psgvol <- train_psgvol %>%
            group_by(TIME_PER_HOUR, DAY_TYPE) %>%
            summarize(TOTAL_TAP_IN_VOLUME = sum(TOTAL_TAP_IN_VOLUME))

        mrtPassengerVolPlot <- ggplot(grouped_train_psgvol, aes(x = TIME_PER_HOUR, y = TOTAL_TAP_IN_VOLUME, fill = DAY_TYPE)) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~DAY_TYPE, nrow = 2) +
            labs(x = "Time in hour", y = "Total tap in volume") +
            scale_x_continuous(breaks = seq(0, 23, 1)) +
            scale_y_continuous(labels = scales::comma)

        mrtPassengerVolPlot
    })
}
