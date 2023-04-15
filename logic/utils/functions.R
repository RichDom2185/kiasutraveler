library(shiny)
library(shinyjs)

setDefaultTab <- function(selector, tabId) {
    list(
        useShinyjs(),
        HTML(paste0(
            "<script defer>
            $(document).on('shiny:connected', function() {
                Shiny.setInputValue('", selector, "', '", tabId, "');
            });
            </script>"
        ))
    )
}
