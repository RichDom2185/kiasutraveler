library(httr)

source("api/routes.R")


API_ENDPOINT_FORWARD_GEOCODE_URL
getAddressFromCoordinates <- function(lat, lng) {} # TODO:

API_ENDPOINT_REVERSE_GEOCODE_URL
getCoordinatesFromAddress <- function(address) {} # TODO:

API_ENDPOINT_RIDES_AVAILABILITY_URL
getGrabServicesAvailability <- function(startLat, startLng, endLat, endLng) {} # TODO:
getTaxiAvailability <- function(date, time) {} # TODO:

API_ENDPOINT_ROUTES_URL
getCyclingDirections <- function(startLat, startLng, endLat, endLng) {} # TODO:
getDrivingDirections <- function(startLat, startLng, endLat, endLng) {} # TODO:
getWalkingDirections <- function(startLat, startLng, endLat, endLng) {} # TODO:
getPublicTransportDirections <- function(startLat, startLng, endLat, endLng, date, time) {} # TODO:

API_ENDPOINT_TRAFFIC_INCIDENTS_URL
getTrafficIncidents <- function() {} # TODO:

API_ENDPOINT_TRAFFIC_DENSITY_URL
# FIXME: `time parameter is not used yet`
getBusPassengerDensity <- function() {} # TODO:
getMrtPlatformDensity <- function(mrtLine, time) {} # TODO:
