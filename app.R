library(shiny)

source("ui/components.R")

ui <- app(
  div(class="box", "test"),
  box(
    input.text("pickUp", "Enter pick-up location...", label = "From:"),
    input.text("dropOff", "Enter destination location...", label = "To:")
  ),
  textInput("input_id", NULL, "value"),
  # textOutput("description"),
  htmlOutput("description"),
)

server <- function(input, output) {
 output$description <- renderText(paste(
   "Searching for directions from",
   strong(input$pickUp),
   "to",
   strong(input$dropOff)
 ))
}

shinyApp(ui, server, options = list(
  port = 4000
  # launch.browser = FALSE
))