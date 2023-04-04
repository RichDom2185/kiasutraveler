library(httr)
library(mapboxer) # Required for pipe operator

getTime <- function(input, id) {
    input[[id]] %>% format("%T")
}

getDateTime <- function(input, date_id, time_id) {
    paste0(input[[date_id]], "T", getTime(input, time_id))
}

formatUrl <- function(input, date_id, time_id) {
    url %>% modify_url(query = list(
        date_time = getDateTime(input, date_id, time_id)
    ))
}
