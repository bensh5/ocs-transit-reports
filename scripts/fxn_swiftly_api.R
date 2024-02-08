
# SWIFTLY API CALL FUNCTIONS ----------------------------------------------

#' Test the user's Swiftly API key
#' 
#' @param api_key API key
#' @return HTTP status
test_swiftly <- function(api_key) {
  url <- "https://api.goswift.ly/test-key"
  
  response <- VERB("GET", url, add_headers(Authorization = swiftly_api_historical, ''), 
                   content_type("application/octet-stream"))
  
  return(response)
}

#' Get cumulative trips & path-length over route
#' 
#' @param routeKey single route
#' @param daysOfWeek default 1,2,3,4,5
#' @return table of cumulative stats
get_route_direction_stops <- function(api_key, agencyKey = "septa", routeKey, direction = "0",
                                      beginTime = time_int$PMpeak[1], endTime = time_int$PMpeak[2], 
                                      startDate = DATE_START, endDate = DATE_END, 
                                      daysOfWeek = DAYS_OF_WEEK, 
                                      excludeDates = "", format = "") {
  
  url = paste0("https://api.goswift.ly/run-times/", "septa", "/route/", routeKey, "/", "cumulative")
  res = GET(url, query = list(beginTime = beginTime, endTime = endTime, format = format,
                              startDate = startDate, endDate = endDate, 
                              direction = direction, daysOfWeek= daysOfWeek),
            add_headers(Authorization = swiftly_api_historical))
  
  x <- fromJSON(res$content |> rawToChar())
  
  return(stops)
}

#' Get runtimes by route per trip for given date-time duration
#' 
#' @param routeKey single route
#' @param tripKey single trip
#' @return list of route_ids and directions
get_runtime_by_trip <- function(api_key = swiftly_api_historical, agencyKey = "septa", 
                         routeKey = "1",
                         beginTime = time_int$PMpeak[1], endTime = time_int$PMpeak[2], 
                         startDate = DATE_START, endDate = DATE_END, 
                         daysOfWeek = DAYS_OF_WEEK, excludeDates = "") {
  
  url = paste0("https://api.goswift.ly/run-times/septa/route/", routeKey, "/", "by-trip")
  res = GET(url, query = list(beginTime = beginTime, endTime = endTime,
                              startDate = startDate, endDate = endDate, 
                              daysOfWeek= daysOfWeek),
            add_headers(Authorization = swiftly_api_historical))
  
  x <- fromJSON(res$content %>% rawToChar())$data
  
  code <- ifelse(res$status_code == "200", 
                 paste0("Route:", routeKey, " OK"),
                 paste("Error", res$status_code, "Route", routeId, "Trip", tripId,))
  
  print(code)
  
  return(x)
}

#' Get runtimes by trip per stop for given date-time duration
#' 
#' @param routeKey single route
#' @param tripKey single trip
#' @return table of route_ids and directions
get_runtime_by_stop <- function(api_key = swiftly_api_historical, agencyKey = "septa", 
                                routeKey = "1", tripKey,
                                beginTime = time_int$PMpeak[1], endTime = time_int$PMpeak[2], 
                                startDate = DATE_START, endDate = DATE_END, 
                                daysOfWeek = DAYS_OF_WEEK, excludeDates = "") {
  
  url = paste0("https://api.goswift.ly/run-times/septa/route/", routeKey, "/", "trip", "/", tripKey, "/by-stop")
  
  f <- function() {
    res <- GET(url, query = list(beginTime = beginTime, endTime = endTime,
                                 startDate = startDate, endDate = endDate,
                                 daysOfWeek = daysOfWeek),
               add_headers(Authorization = swiftly_api_historical))
    
    return(res)
  }
  
  dat <- f()
  
  x <- fromJSON(dat$content |> rawToChar())$data$byStopRuntimeData |> unnest(cols = c(observedRuntimes))
  
  code <- ifelse(dat$status_code == "200", 
                 paste0("Route:", routeKey, ", Trip:", tripKey, " OK"),
                 paste("Error", res$status_code, "Route", routeId, "Trip", tripId,))
  
  print(code)
  
  return(x)
}

#' Get speed map (low or high resolution)
#' 
#' @param routeKey single route
#' @param tripKey single trip
#' @return table of route_ids and directions
get_speedmap <- function(speed = "low", api_key = swiftly_api_historical,
                         agencyKey = "septa", route = "1", direction = "0",
                         beginTime = time_int$PMpeak[1], endTime = time_int$PMpeak[2], 
                         startDate = DATE_START, endDate = DATE_END, 
                         daysOfWeek = DAYS_OF_WEEK, excludeDates = "") {
  
  url = paste0("https://api.goswift.ly/speed-map/septa/", speed, "-resolution/")
  
  # api call function
  res <- GET(url, query = list(route = route, direction = direction, 
                               beginTime = beginTime, endTime = endTime,
                               startDate = startDate, endDate = endDate,
                               daysOfWeek = daysOfWeek, format = "geojson"),
             add_headers(Authorization = swiftly_api_historical))
  
  x <- st_read(res, quiet = T) |> mutate(route_id = route, direction = direction)
  
  code <- ifelse(res$status_code == "200", 
                 paste0("Route:", route, ", Direction:", direction, " OK"),
                 paste("Error", res$status_code, "Route", route, "Direction", direction,))
  
  print(code)
  
  return(x)
}

#' Get trip observations
#' 
#' @param routeKey single route
#' @param tripKey single trip
#' @param directionId 0 or 1
#' @return table of route_ids and directions
get_trip_observations <- function(api_key = swiftly_api_historical, agencyKey = "septa", 
                                  tripId, routes = "1", directionId = "0",
                                  beginTime = time_int$PMpeak[1], endTime = time_int$PMpeak[2], 
                                  startDate = DATE_START, endDate = DATE_END, 
                                  daysOfWeek = DAYS_OF_WEEK, 
                                  excludeDates = "", format = "") {
  
  url = paste0("https://api.goswift.ly/run-times/septa/trip-observations/")
  
  # api call function
  res <- GET(url, query = list(tripId = tripId, routes = routes, directionId = directionId,
                               beginTime = beginTime, endTime = endTime, format = format,
                               startDate = startDate, endDate = endDate, daysOfWeek = daysOfWeek),
             add_headers(Authorization = swiftly_api_historical))
  
  x <- fromJSON(res$content |> rawToChar())$data$byStopRuntimeDatas
  
  code <- ifelse(res$status_code == "200", 
                 paste0("Route:", routeId, ", Trip:", tripId, ", Direction:", directionId, " OK"),
                 paste("Error", res$status_code, "Route", routeId, "Trip", tripId,))
  
  print(code)
  
  return(x)
}
