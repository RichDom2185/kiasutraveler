library(ggplot2)
library(shiny)

weatherTabContent <- conditionalPanel(
    condition = "input.othersTab === 'weather'",
    p("Weather tab data coming soon"),
    plotOutput("weatherPlot")
)

updateWeatherTab <- function(input, output) {
    output$weatherPlot <- renderPlot({
        # read the CSV file
        rainfall <- read.csv("data/MonthlyRainfall.csv")

        # convert month to date format
        rainfall$month <- as.Date(paste(rainfall$month, "-01", sep = ""), format = "%Y-%m-%d")

        # filter the data for time period from 2013 to 2022
        rainfall <- rainfall[format(rainfall$month, "%Y") >= "2013" & format(rainfall$month, "%Y") <= "2022", ]

        # calculate average number of rainy days for each month
        monthly_avg <- aggregate(no_of_rainy_days ~ format(month, "%b"), data = rainfall, FUN = mean)

        # rename the column names
        colnames(monthly_avg) <- c("Month", "Average_Rainy_Days")

        # plot the monthly averages
        ggplot(monthly_avg, aes(x = Month, y = Average_Rainy_Days, fill = Average_Rainy_Days)) +
            geom_bar(stat = "identity") +
            scale_fill_gradient(low = "#0171BB", high = "#E76963") +
            ggtitle("Average Monthly Rainy Days from 2013 to 2022") +
            ylab("Average Number of Rainy Days") +
            xlab("Month") +
            theme_classic() +
            scale_y_continuous(expand = c(0, 0)) +
            scale_x_discrete(limits = month.abb)
    })
}
