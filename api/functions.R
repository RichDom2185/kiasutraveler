library(httr)
library(jsonlite)

source("api/routes.R")


##########################################
# Using API_ENDPOINT_FORWARD_GEOCODE_URL
##########################################
getCoordinatesFromAddress <- function(address) {
    url <- API_ENDPOINT_FORWARD_GEOCODE_URL
    params <- list(address = address)
    response <- GET(url, query = params)
    content(response)
}


##########################################
# Using API_ENDPOINT_REVERSE_GEOCODE_URL
##########################################
getAddressFromCoordinates <- function(lat, lng) {
    url <- API_ENDPOINT_REVERSE_GEOCODE_URL
    params <- list(lat = lat, lng = lng)
    response <- GET(url, query = params)
    content(response)
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
    response <- GET(url, query = params)
    content(response)
}

getTaxiAvailability <- function() {
    url <- API_ENDPOINT_RIDES_AVAILABILITY_URL
    params <- list(
        serviceType = "taxi"
    )
    response <- GET(url, query = params)
    content(response)
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
    response <- GET(url, query = params)
    content(response)
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
    response <- GET(url, query = params)
    content(response)
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
    response <- GET(url, query = params)
    content(response)
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
    response <- GET(url, query = params)
    content(response)
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
    response <- GET(url, query = params)
    content(response)
}

# FIXME: `time` parameter is not used yet
getMrtPlatformDensity <- function(mrtLine, time) {
    url <- API_ENDPOINT_TRAFFIC_DENSITY_URL
    params <- list(
        type = "platform",
        mrt = mrtLine
    )
    response <- GET(url, query = params)
    content(response)
}
