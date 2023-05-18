library(httr)
library(jsonlite)

source("api/routes.R")

# Ideally, we should use a parameter instead of a global flag,
# but this is done to simplify the function call signature.
setApiMode <- function(type) {
    types <- c("manual", "auto")
    if (!(type %in% types)) {
        warning("Invalid type value, defaulting to auto")
        type <- "auto"
    }
    if (type == "manual") {
        globals.api_handle_url <<- function(url) {
            httr::content(httr::GET(url))
        }
    } else if (type == "auto") {
        globals.api_handle_url <<- jsonlite::fromJSON
    }
}

globals.api_handle_url <- setApiMode("auto")

##########################################
# Using API_ENDPOINT_CARS_AVAILABILITY_URL
##########################################
getAvailableBluesgStations <- function() {
    url <- API_ENDPOINT_CARS_AVAILABILITY_URL
    params <- list(serviceType = "bluesg")
    globals.api_handle_url(modify_url(url, query = params))
}

getAvailableGetgoVehicles <- function() {
    url <- API_ENDPOINT_CARS_AVAILABILITY_URL
    params <- list(serviceType = "getgo")
    globals.api_handle_url(modify_url(url, query = params))
}


##########################################
# Using API_ENDPOINT_FORWARD_GEOCODE_URL
##########################################
getCoordinatesFromAddress <- function(address) {
    url <- API_ENDPOINT_FORWARD_GEOCODE_URL
    params <- list(address = address)
    globals.api_handle_url(modify_url(url, query = params))
}


##########################################
# Using API_ENDPOINT_REVERSE_GEOCODE_URL
##########################################
getAddressFromCoordinates <- function(lat, lng) {
    url <- API_ENDPOINT_REVERSE_GEOCODE_URL
    params <- list(lat = lat, lng = lng)
    globals.api_handle_url(modify_url(url, query = params))
}


##########################################
# Using API_ENDPOINT_RIDES_AVAILABILITY_UR
########################################## L
getGrabServicesAvailability <- function(startLat, startLng, endLat, endLng) {
    url <- API_ENDPOINT_RIDES_AVAILABILITY_URL
    params <- list(
        serviceType = "grab",
        startLat = startLat,
        startLng = startLng,
        endLat = endLat,
        endLng = endLng
    )
    globals.api_handle_url(modify_url(url, query = params))
}

getTaxiAvailability <- function() {
    url <- API_ENDPOINT_RIDES_AVAILABILITY_URL
    params <- list(
        serviceType = "taxi"
    )
    globals.api_handle_url(modify_url(url, query = params))
}


##########################################
# Using API_ENDPOINT_ROUTES_URL
##########################################
getCyclingDirections <- function(startLat, startLng, endLat, endLng) {
    url <- API_ENDPOINT_ROUTES_URL
    params <- list(
        startLat = startLat,
        startLng = startLng,
        endLat = endLat,
        endLng = endLng,
        mode = "cycle"
    )
    globals.api_handle_url(modify_url(url, query = params))
}

getDrivingDirections <- function(startLat, startLng, endLat, endLng) {
    url <- API_ENDPOINT_ROUTES_URL
    params <- list(
        startLat = startLat,
        startLng = startLng,
        endLat = endLat,
        endLng = endLng,
        mode = "drive"
    )
    globals.api_handle_url(modify_url(url, query = params))
}

getWalkingDirections <- function(startLat, startLng, endLat, endLng) {
    url <- API_ENDPOINT_ROUTES_URL
    params <- list(
        startLat = startLat,
        startLng = startLng,
        endLat = endLat,
        endLng = endLng,
        mode = "walk"
    )
    globals.api_handle_url(modify_url(url, query = params))
}

getPublicTransportDirections <- function(startLat, startLng, endLat, endLng, date, time) {
    url <- API_ENDPOINT_ROUTES_URL
    params <- list(
        startLat = startLat,
        startLng = startLng,
        endLat = endLat,
        endLng = endLng,
        mode = "pt",
        date = date,
        time = time,
        vehicleType = "TRANSIT"
    )
    globals.api_handle_url(modify_url(url, query = params))
}


##########################################
# Using API_ENDPOINT_TRAFFIC_INCIDENTS_URL
##########################################
getTrafficIncidents <- function() {
    url <- API_ENDPOINT_TRAFFIC_INCIDENTS_URL
    response <- GET(url)
    content(response)
}


##########################################
# Using API_ENDPOINT_TRAFFIC_DENSITY_URL
##########################################
getBusPassengerDensity <- function() {
    url <- API_ENDPOINT_TRAFFIC_DENSITY_URL
    params <- list(
        type = "bus"
    )
    globals.api_handle_url(modify_url(url, query = params))
}

# FIXME: `time` parameter is not used yet
getMrtPlatformDensity <- function(mrtLine, time) {
    url <- API_ENDPOINT_TRAFFIC_DENSITY_URL
    params <- list(
        type = "platform",
        mrt = mrtLine
    )
    globals.api_handle_url(modify_url(url, query = params))
}


##########################################
# Using API_ENDPOINT_RAINFALL_DATA_URL
##########################################
getRainfall <- function(date = NULL, time = NULL) {
    # Using default argument values to suport both
    # realtime data and historical data
    url <- API_ENDPOINT_RAINFALL_DATA_URL
    if (!is.null(date)) {
        url <- modify_url(url, query = list(date = date))
    }
    if (!is.null(time)) {
        url <- modify_url(url, query = list(time = time))
    }
    globals.api_handle_url(url)
}


##########################################
# Using API_ENDPOINT_WEATHER_FORECAST_URL
##########################################
getWeatherForecast <- function(date = NULL, time = NULL) {
    # Using default argument values to suport both
    # realtime data and historical data
    url <- API_ENDPOINT_WEATHER_FORECAST_URL
    if (!is.null(date)) {
        url <- modify_url(url, query = list(date = date))
    }
    if (!is.null(time)) {
        url <- modify_url(url, query = list(time = time))
    }
    globals.api_handle_url(url)
}
