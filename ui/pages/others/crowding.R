library(dplyr)
library(ggplot2)
library(shiny)

crowdingTabContent <- conditionalPanel(
    condition = "input.othersTab === 'crowding'",
    plotOutput("busCrowdingPlot", width = "600px", height = "400px"),
    plotOutput("mrtCrowdingPlot", width = "600px", height = "400px")
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
