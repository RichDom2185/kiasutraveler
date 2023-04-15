library(dplyr)
library(ggplot2)
library(shiny)

weatherTabContent <- conditionalPanel(
    condition = "input.othersTab === 'weather'",
    plotOutput("weatherPlot")
)

updateWeatherTab <- function(input, output) {
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
