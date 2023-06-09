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
appTitle <- function(..., class = "") {
    p(class = paste("title", class), ...)
}
box <- function(..., class = "") {
    div(class = paste("box my-5 mx-4", class), ...)
}
columns <- function(..., class = "") {
    div(class = paste("columns", class), ...)
}
column <- function(..., class = "") {
    div(class = paste("column", class), ...)
}


tabs <- function(selector, ..., class = "") {
    # Internal function in order to access parent environment
    tab <- function(id, title, class = "", ...) {
        tags$li(id = id, class = class, a(
            onClick = paste0("Shiny.setInputValue('", selector, "', '", id, "')"), title, ...
        ))
    }

    div(
        class = paste("tabs is-toggle is-centered", class),
        tags$ul(id = selector, lapply(list(...), function(args) do.call(tab, args)))
    )
}

tab <- function(id, title, class = "", ...) {
    list(id, title, class, ...)
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
        # TODO: Remove this old unused code
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

stackElements <- function(...) {
    elems <- list(...)
    list(
        tags$style(HTML("
        div.stack-container {
            display: grid;
        }
        div.stack-container > * {
            grid-area: 1/1;
        }
        ")),
        tags$div(class = "stack-container", lapply(seq_along(elems), function(i) {
            css <- paste0("height: min-content; z-index: ", i)
            tags$div(style = css, elems[[i]])
        }))
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
