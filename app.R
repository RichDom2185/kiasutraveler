library(shiny)

app <- function(...) fillPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "reset_bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bulma.min.css")
  ),
  div(class = "root", ...))

box <- function(...) div(class = "box", ...)

input.text <- function(id, placeholder = NULL, value = "", label = NULL) div(
  class = "field",
  if(is.null(label)) NULL else tags$label(class = "label", label),
  div(
    class = "control",
    tags$input(
      type = "text",
      class = "input",
      id = id,
      placeholder = placeholder,
      value
    )
  )
)

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