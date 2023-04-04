library(mapboxer)

mapbox_basemap_types <- names(basemaps$Mapbox) # Requires API key
carto_basemap_types <- names(basemaps$Carto)

# FIXME: Hardcoded default name
getBasemap <- function(mapType = "voyager") {
    if (mapType %in% carto_basemap_types) {
        basemaps$Carto[[mapType]]
    } else if (mapType %in% mapbox_basemap_types) {
        basemaps$Mapbox[[mapType]]
    }
}

basemap_types <- c(
    # mapbox_basemap_types,
    carto_basemap_types
)

COORDINATES_SINGAPORE <- c(103.8198, 1.3521)
url <- "https://api.data.gov.sg/v1/transport/taxi-availability?date_time=2023-03-05T09%3A00%3A00"
