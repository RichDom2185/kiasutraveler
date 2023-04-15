library(dplyr)
library(shiny)
library(stringr)
library(wordcloud)

trafficIncidentsTabContent <- conditionalPanel(
    condition = "input.othersTab === 'trafficIncidents'",
    radioButtons(
        inputId = "incidentPlotType",
        label = "Select Map Type:",
        choices = c("Word Cloud", "Bar Chart")
    ),
    htmlOutput("incidentsDescription"),
    plotOutput("incidentsPlot")
    # TODO: Live Traffic Incidents
)

updateTrafficIncidentsTab <- function(input, output) {
    incidentsDescription <- reactiveVal()

    observeEvent(input$incidentPlotType, {
        # Update the description
        incidentsDescription(switch(input$incidentPlotType,
            # Wordcloud
            "Word Cloud" = {
                paste0(
                    h3(strong("Traffic Incident Causes in Singapore")),
                    br(),
                    p(
                        "The following word cloud illustrates some of the most common traffic
                        incident causes in Singapore from 2012 to 2018. The most common cause
                        appears to be Diversion and Accidents."
                    ),
                    br(),
                    p(
                        "Incidents categorised under 'Diversion' include 'Turning / Changing
                        Lanes without Due Care'."
                    ),
                    br(),
                    p(
                        "Incidents categorised under 'Accidents' include 'Disobeying Traffic
                        Signals' or 'Failing to give way to a pedestrian'."
                    ),
                    br(),
                    p(
                        "Take account of these statistics when you are on the road!"
                    )
                )
            },
            # Live Incident Causes
            "Bar Chart" = {
                paste0(
                    h3(strong("Live Traffic Incident Causes")),
                    br(),
                    p(
                        "The bar graph illustrates live traffic incident causes right now. The
                        data is updated every two minutes."
                    ),
                    br(),
                    p(
                        "Observe the various incidents happening now to get a better idea of
                        the road congestion situation."
                    ),
                    br(),
                    p(
                        "Click on the 'Live Traffic Incidents' tab to find out where these
                        incidents occured!"
                    ),
                    br(),
                    p(strong(paste(
                        "Getting traffic incident information on",
                        format(Sys.Date(), "%A, %B %d, %Y"),
                        "at", format(Sys.time(), "%I:%M %p"), "..."
                    )))
                )
            },
            # default
            ""
        ))
    })

    output$incidentsDescription <- renderText(incidentsDescription())

    # Update the plot
    output$incidentsPlot <- renderPlot({
        switch(input$incidentPlotType,
            # Wordcloud
            "Word Cloud" = {
                traffic_data <- read.csv("data/causes-of-accidents-by-severity-of-injury-sustained.csv")

                # Manual Cleaning of Data for WordCloud Words appear simplified
                traffic_data$causes_of_accident <- traffic_data$causes_of_accident %>% str_replace_all(c(
                    "Carrying Out Work on the Road without Proper Attire or Sufficient Warning Signs" = "Roadwork",
                    "Causes Attributed to Road Conditions" = "Misc",
                    "Causes Attributed to Vehicles" = "Vehicle Breakdown",
                    "Changing Lane without Due Care" = "Diversion",
                    "Crossing Heedless of Traffic" = "Pedestrian",
                    "Crossing In Front or Behind a Vehicle which Obstructs View" = "Pedestrian",
                    "Crossing Within Pedestrian Crossing When Red Man Lighted" = "Pedestrian",
                    "Disobeying Traffic Light Signals Resulting in Accidents with Vehicle" = "Accident",
                    "Driving under the Influence of Alcohol" = "Alcohol",
                    "Failing to Give Way to Traffic with Right of Way" = "Accident",
                    "Failing to Keep a Proper Lookout" = "Diversion",
                    "Failing to Use Available Pedestrian Crossing" = "Pedestrian",
                    "Failing to Have Proper Control" = "Accident",
                    "Following Too Close to Vehicle In Front" = "Accident",
                    "Overtaking without Due Care" = "Diversion",
                    "Other causes attributed to drivers, riders or pedal cyclists" = "Accident",
                    "Other Causes of Accidents Attributed to Pedestrians" = "Pedestrians",
                    "Turning Vehicle & Failing to Give Way to Pedestrian During Green Man" = "Accident",
                    "Playing on The Road or Carpark" = "Pedestrian",
                    "Turning Without Due Care" = "Diversion",
                    "Under the Influence of Alcohol" = "Alcohol",
                    "Under the Influence of Drugs/Intoxicated Substance" = "Substance",
                    "Using PMD to Travel on Road" = "PMD",
                    "Other Causes" = "Misc"
                ))


                traffic_wordcloud <- traffic_data %>%
                    group_by(causes_of_accident) %>%
                    summarise(count = sum(number_of_accidents))

                set.seed(1000)
                wordcloud(
                    words = traffic_wordcloud$causes_of_accident,
                    freq = traffic_wordcloud$count,
                    min.freq = 1,
                    max.words = 200,
                    random.order = FALSE,
                    rot.per = 0.35, colors = brewer.pal(8, "RdBu")
                )
            },
            "Bar Chart" = {
                # TODO: Add
            },
            # default
            ""
        )
    })
}
