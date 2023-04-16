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

# Generates a GeoJSON LineString representation of a list of points
points_to_lines <- function(points) {
    list(
        "type" = "FeatureCollection",
        "features" = list(
            list(
                "type" = "Feature",
                "geometry" = list(
                    "type" = "LineString",
                    "coordinates" = points
                )
            )
        )
    )
}
