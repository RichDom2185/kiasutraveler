library(shiny)
library(shinyjs)

app <- function(...) {
    fillPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "reset_bootstrap.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "bulma.min.css")
        ),
        div(class = "root", ...)
    )
}


# Simple Bulma wrappers
appTitle <- function(...) p(class = "title", ...)
box <- function(...) div(class = "box  my-5 mx-4", ...)
columns <- function(...) div(class = "columns", ...)
column <- function(...) div(class = "column", ...)


tabs <- function(id, ...) div(class = "tabs is-toggle is-centered", tags$ul(id = id, ...))
tab <- function(id, title, class = "", ...) {
    tags$li(id = id, class = class, a(
        onClick = paste0("Shiny.setInputValue('activeTab', '", id, "')"), title, ...
    ))
}

appTabs <- function(id, ..., selected = 1) {
    application_tabs <- list(...)
    list(
        tabs(id, lapply(seq_along(application_tabs), function(i) {
            application_tab <- application_tabs[[i]]
            list(
                tab(paste0("tab_", i), application_tab$title)
            )
        })),
        lapply(seq_along(application_tabs), function(i) {
            conditionalPanel(
                condition = paste0("input.activeTab == 'tab_", i, "'"),
                tabPanelBody(paste0("tab_", i), application_tabs[[i]]$content)
            )
        }),
        useShinyjs(),
        HTML("<script defer>
        $(document).on('shiny:connected', function() {
            Shiny.setInputValue('activeTab', 'tab_1');
        });
        </script>")
    )
}
appTab <- function(title, ...) {
    list(
        title = title,
        content = list(...)
    )
}

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
