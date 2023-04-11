library(hms)
library(jsonlite)
library(shiny)
library(shinyjs)
library(shinyTime)
library(mapboxer)
library(bslib)
library(dplyr)
library(httr)
library(jsonlite)
library(ggplot2)
library(mapboxer)
library(sf)
library(wordcloud)

source("api/functions.R")
source("ui/components/bulma.R")
source("logic/maps/maps.R")

options(shiny.autoreload = TRUE)

theme <- bs_theme(
  bg = "#0171bb", fg = "#FDf7f7", primary = "#FCC780",
  base_font = font_google("Lato")
)

ui <- navbarPage(theme = theme,  title= div(img(src = "Kiasu Traveler", height = "40px", width = "40px"), "Kiasu Traveler"),
  tabPanel("Ride Hailing", plotOutput("ridehailing")),
  tabPanel("Car Sharing", plotOutput("carsharing")),
  tabPanel("Taxi",
  app(
    useShinyjs(),
    stackElements(
        mapboxerOutput("map", height = "100vh"),
        column(
            box(
                style = "width: fit-content; position: absolute; top: 0; left: 0;",
                appTitle("Kiasutraveler")
            ),
            box(
                style = "width: fit-content; position: absolute; right: 0; top: 0",
                class = "py-3 px-3",
                tabs(
                    "transportModes",
                    # TODO: Fix IDs and add reactivity
                    tab("ridehailing", "Ride Hailing"),
                    tab("carsharing", "Car-Sharing"),
                    tab("taxi", "Taxi"),
                    tab("pt", "Public Transport"),
                    tab("otherinsights", "Other Insights")
                    )
            ),
            box(
                style = "width: fit-content; position: absolute; bottom: 0; right: 0",
                conditionalPanel(
                    condition = "input.activeTab == 'taxi'",
                    columns(
                        column(
                            dateInput(
                                inputId = "date",
                                label = "Select Date:",
                                format = "yyyy-mm-dd"
                            ),
                            timeInput(
                                inputId = "time",
                                label = "Select Time:",
                                value = as_hms(Sys.time())
                            )
                        ),
                        column(
                            radioButtons(
                                inputId = "layer",
                                label = "Select Map Type:",
                                choices = c("Point", "Heatmap")
                            ),
                            selectInput(
                                inputId = "mapType",
                                label = "Select Base Map Style:",
                                choices = basemap_types,
                                selected = "voyager"
                            )
                        )
                    ),
                    htmlOutput("description")
                ),
                
                conditionalPanel(
                    condition = "input.activeTab == 'ridehailing'",
                    p("Coming soon!")
                ),
                conditionalPanel(
                  condition = "input.activeTab == 'carsharing'",
                  p("Coming soon!")
                ),
                conditionalPanel(
                  condition = "input.activeTab == 'pt'",
                  p("Coming soon!")
                ),
                
                conditionalPanel(
                  condition = "input.activeTab == 'otherinsights'",
                  p("Coming soon!")
                )
                
            )
        )
    )
)), 
    tabPanel("Public transport", plotOutput("pt")),
    navbarMenu("Other insights", 
               tabPanel("Weather",
                        tabsetPanel(
                          tabPanel("2 hour forecast", plotOutput("2hourforecast"))
                        )),
               tabPanel("Crowd",
                        tabsetPanel(
                          tabPanel("Bus Volume", plotOutput("busvolume")),
                          tabPanel("MRT Volume", plotOutput("MRT Volume"))
                        )),
               tabPanel("Traffic Incidents",
                        tabsetPanel(
                          tabPanel("Wordcloud",
                                   fluidPage(
                                   sidebarLayout(position = "right",
                                     sidebarPanel(width = 7,
                                       h2("Traffic Incident Causes in Singapore"), br(),
                                       p("The following wordcloud illustrates some of the most common traffic incident causes in Singapore from 2012 to 2018. 
                                         The most common cause appears to be Diversion and Accidents"), br(),
                                       p("Incidents categorised under 'Diversion' include 'Turning / Changing Lanes without Due Care'."), br(),
                                       p("Incidents categorised under 'Accidents' include 'Disobeying Traffic Signals' or 'Failing to give way to a pedestrian'."), br(),
                                       p("Take account of these statistics when you are on the road!")
                                     ),
                                     mainPanel(width = 5, plotOutput("wordcloud"))))),
                          tabPanel("Live Incident Causes", 
                                   fluidPage(
                                     sidebarLayout(
                                     sidebarPanel(width = 6,
                                               h1("Live Traffic Incident Causes"), br(),
                                               p("The bar graph illustrates live traffic incident causes right now. The data is updated every two minutes."),
                                               br(),
                                               p("Observe the various incidents happening now to get a better idea of the road congestion situation."),
                                               p("Click on the 'Live Traffic Incidents' tab to find out where these incidents occured!"), br(),
                                               p(paste(
                                                   "Getting traffic incident information on", format(Sys.Date(), "%A, %B %d, %Y"), 
                                                   "at",format(Sys.time(), "%I:%M %p"), "..."
                                                 ))),
                                     mainPanel(width = 6, plotOutput("incidentbar"))
                                   ))),
                          tabPanel("Live Traffic Incidents", 
                                   stackElements(mapboxerOutput("incident_map", height = "100vh"),
                                                 box(style = "width: fit-content; position: absolute; bottom: 0; right: 0",
                                                     column(htmlOutput("trafficincident_description")))))
               )))

)

server <- function(input, output) {
    # Set up custom tab pages
    currentActiveTab <- reactiveVal()
    observeEvent(input$activeTab, {
        removeCssClass(id = currentActiveTab(), class = "is-active")
        currentActiveTab(input$activeTab)
        addCssClass(id = currentActiveTab(), class = "is-active")
    })

    data <- reactive({
        # TODO: Remove this in production
        print("Firing API call...")
        fromJSON(formatUrl(input, "date", "time"))
    })

    df <- reactive({
        df <- data()$features$geometry$coordinates %>% data.frame()

        colnames(df) <- c("lng", "lat")
        df$type <- "Taxi"

        return(df)
    })

    getSingaporeMap <- reactive(
        mapboxer(
            style = getBasemap(input$mapType),
            center = COORDINATES_SINGAPORE,
            zoom = 10,
            pitch = 15,
            minZoom = 7
        )
    )

    output$description <- renderText(paste(
        "Getting taxi availability on",
        strong(input$date),
        "at",
        strong(getTime(input, "time")),
        "as a",
        strong(input$layer),
        "..."
    ))
    

    output$map <- renderMapboxer({
      mapbox <- getSingaporeMap() %>% add_navigation_control(pos = "bottom-left")
          if (input$layer == "Heatmap") {
            mapbox <- mapbox %>%
              add_layer(
                list(
                  id = "heatmap_layer",
                  type = "heatmap",
                  source = as_mapbox_source(df()),
                  paint = list(
                    "heatmap-opacity" = 0.3,
                    "heatmap-radius" = 10
                  )
                )
              )
          } else if (input$layer == "Point") {
            mapbox <- mapbox %>%
              add_circle_layer(
                id = "taxis_points",
                source = as_mapbox_source(df()),
                circle_color = "red",
                circle_radius = 4,
                circle_opacity = 0.2
              ) %>%
              add_tooltips("taxis_points", "{{type}}")
          }
        
    })
    
    # Other Insights => Traffic Incidents => Word Cloud
    output$wordcloud <- renderPlot({
      traffic_data <- read.csv("causes-of-accidents-by-severity-of-injury-sustained.csv")
      names(traffic_data)[names(traffic_data) == "causes_of_accident"] <- "C"
      
      #Manual Cleaning of Data for WordCloud Words appear simplified
      traffic_data$C <- gsub("Carrying Out Work on the Road without Proper Attire or Sufficient Warning Signs", "Roadwork", traffic_data$C)
      traffic_data$C <- gsub("Causes Attributed to Road Conditions", "Misc", traffic_data$C)
      traffic_data$C <- gsub("Causes Attributed to Vehicles", "Vehicle Breakdown", traffic_data$C)
      traffic_data$C <- gsub("Changing Lane without Due Care", "Diversion", traffic_data$C)
      traffic_data$C <- gsub("Crossing Heedless of Traffic", "Pedestrian", traffic_data$C)
      traffic_data$C <- gsub("Crossing In Front or Behind a Vehicle which Obstructs View", "Pedestrian", traffic_data$C)
      traffic_data$C <- gsub("Crossing Within Pedestrian Crossing When Red Man Lighted", "Pedestrian", traffic_data$C)
      traffic_data$C <- gsub("Disobeying Traffic Light Signals Resulting in Accidents with Vehicle", "Accident", traffic_data$C)
      traffic_data$C <- gsub("Driving under the Influence of Alcohol", "Alcohol", traffic_data$C)
      traffic_data$C <- gsub("Failing to Give Way to Traffic with Right of Way", "Accident", traffic_data$C)
      traffic_data$C <- gsub("Failing to Keep a Proper Lookout", "Diversion", traffic_data$C)
      traffic_data$C <- gsub("Failing to Use Available Pedestrian Crossing", "Pedestrian", traffic_data$C)
      traffic_data$C <- gsub("Failing to Have Proper Control", "Accident", traffic_data$C)
      traffic_data$C <- gsub("Following Too Close to Vehicle In Front", "Accident", traffic_data$C)
      traffic_data$C <- gsub("Overtaking without Due Care", "Diversion", traffic_data$C)
      traffic_data$C <- gsub("Other causes attributed to drivers, riders or pedal cyclists", "Accident", traffic_data$C)
      traffic_data$C <- gsub("Other Causes of Accidents Attributed to Pedestrians", "Pedestrians", traffic_data$C)
      traffic_data$C <- gsub("Turning Vehicle & Failing to Give Way to Pedestrian During Green Man", "Accident", traffic_data$C)
      traffic_data$C <- gsub("Playing on The Road or Carpark", "Pedestrian", traffic_data$C)
      traffic_data$C <- gsub("Turning Without Due Care", "Diversion", traffic_data$C)
      traffic_data$C <- gsub("Under the Influence of Alcohol", "Alcohol", traffic_data$C)
      traffic_data$C <- gsub("Under the Influence of Drugs/Intoxicated Substance", "Substance", traffic_data$C)
      traffic_data$C <- gsub("Using PMD to Travel on Road", "PMD", traffic_data$C)
      traffic_data$C <- gsub("Other Causes", "Misc", traffic_data$C)
      
      traffic_wordcloud <- traffic_data %>% group_by(C) %>% summarise(count = sum(number_of_accidents))
      names(traffic_wordcloud)[names(traffic_wordcloud) == "C"] <- "Causes"
      #traffic_wordcloud$Causes
      
      set.seed(1000)
      wordcloud(words = traffic_wordcloud$Causes, freq = traffic_wordcloud$count, min.freq = 1, max.words=200, 
                random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "RdBu"))
      })

    # Other Insights => Traffic Incidents => Bar graph
    output$incidentbar <- renderPlot({
      #Import API
      api_key <- "REDACTED"
      
      url_incidents <- "http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents"
      response <- GET(url_incidents, add_headers(AccountKey=api_key))
      data_incidents <- fromJSON(content(response, as = "text"))
      data_incidents <- as.data.frame(data_incidents)
      #Extract location
      df_incidents <- data.frame(lat = data_incidents$value.Latitude, long = data_incidents$value.Longitude)
      traffic_counted2 <- data_incidents %>% group_by(value.Type) %>% summarise(Count = n())
      
      #Account for Incident Types not occured on the day
      all_incidents <- c('Accident', 'Roadwork', 'Vehicle Breakdown', 'Weather', 'Obstacle', 'Road Block', 'Heavy Traffic', 'Misc', 'Diversion', 'Unattended Vehicle')
      not_today <- gsub(paste(traffic_counted2$value.Type, collapse = "|"), "", all_incidents)
      not_today <- not_today[nzchar(not_today)]
      not_today <- data.frame(value.Type = not_today, Count = 0)
      traffic_counted2 <- rbind(traffic_counted2, not_today)
      
      traffic_count_live <- ggplot(traffic_counted2) + geom_bar(aes(x = value.Type, y = Count), stat = 'identity', fill = "#0171bb") + geom_text(aes(label = Count, x = value.Type, y = Count), vjust = -0.2, colour = "black")
      
      traffic_count_live + theme_classic() + labs(title = "Live Traffic Incidents", x = "Type of Incident", y = "Number of Accidents") + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size= 20, face = "bold"))
    })
    
    
    
    # Other Insights => Traffic Incidents => Incident Map Plot
    output$incident_map <- renderMapboxer({
      #Import API
      api_key <- "REDACTED"
      
      url_incidents <- "http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents"
      response <- GET(url_incidents, add_headers(AccountKey=api_key))
      data_incidents <- fromJSON(content(response, as = "text"))
      data_incidents <- as.data.frame(data_incidents)
      df_incidents <- data.frame(lat = data_incidents$value.Latitude, long = data_incidents$value.Longitude)
      
      # Convert data to an sf object
      df_incident_sf <- st_as_sf(df_incidents, coords = c("long", "lat"), crs = 4326)
      df_incident_sf$message <- data_incidents$value.Message
      
      # Add a heatmap layer to the Mapbox map
      traffic_map <- as_mapbox_source(df_incident_sf) %>%
        mapboxer(style = getBasemap("voyager"), center = COORDINATES_SINGAPORE, zoom = 10) %>%
        add_navigation_control() %>%
        add_circle_layer(
          id = "Incidents",
          circle_color = "#0171bb",
          popup = "<p>{{message}}</p>"
        )
    })
    
    output$trafficincident_description <- renderText({
      date <- format(Sys.Date(), "%A, %B %d, %Y")  # current date
      time <- format(Sys.time(), "%I:%M %p")  # current time
      paste(
        "Getting traffic incident information on",
        "<b>", date, "</b>",
        "at",
        "<b>", time, "</b>",
        "..."
      )
    })
    
    
}

shinyApp(ui, server, options = list(
    port = 4000
    # launch.browser = FALSE
))
