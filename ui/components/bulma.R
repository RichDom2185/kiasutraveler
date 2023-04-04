library(shiny)

app <- function(...) {
    fillPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "reset_bootstrap.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "bulma.min.css")
        ),
        div(class = "root py-5 px-4", ...)
    )
}


# Simple Bulma wrappers
appTitle <- function(...) p(class = "title", ...)
box <- function(...) div(class = "box", ...)
columns <- function(...) div(class = "columns", ...)
column <- function(...) div(class = "column", ...)


# Custom inputs
input.text <- function(id, placeholder = NULL, value = "", label = NULL) {
    div(
        class = "field",
        if (is.null(label)) NULL else tags$label(class = "label", label),
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
}
