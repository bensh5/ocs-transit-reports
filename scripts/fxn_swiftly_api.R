source("scripts/source.R")

# SWIFTLY API CALL FUNCTIONS ----------------------------------------------

#' Test the user's Swiftly API key
#' 
#' @param api_key API key
#' @returns HTTP status
test_swiftly <- function(api_key) {
  url <- "https://api.goswift.ly/test-key"
  
  response <- VERB("GET", url, add_headers(Authorization = api_key, ''), content_type("application/octet-stream"))
  
  return(response)
}

#' Build the route board
#' 
#' @param stop_list list of stops (formatted by corridor
#' @param filename dataset of stop and route GTFS
#' @returns table of route_ids and directions
build_route_board <- function(stop_list, filename) {
  stop_routes <- read.csv(filename) |> 
    mutate(route_id = fix_routes(route_id)) |> 
    select(-agency_id)
  
  x <- stop_routes |> 
    filter(stop_id %in% stop_list) |> 
    mutate(served = 1) |> 
    pivot_wider(names_from = stop_id, values_from = served, values_fill = 0)
  
  y <- x |> 
    rowwise() |> 
    mutate(y = sum(c_across(2:ncol(x)))) |> 
    #filter(y == (ncol(x) - 1)) |> 
    select(route_id) |> 
    mutate(direcion_0 = 0, direction_1 = 1) |> 
    gather(direction_0, direction, -route_id) |> 
    select(-direction_0) |> 
    filter(route_id != "WCS")
  
  return(y)
}

#' Call API for cumulative
#' 
#' @param stop_list list of stops (formatted by corridor
#' @param filename dataset of stop and route GTFS
#' @returns table of route_ids and directions
get_route_direction_stops <- function(api_key, agencyKey = "septa", routeKey, direction = "0",
                                      beginTime = "00:00", endTime = "23:59", 
                                      startDate = "04-01-2021", endDate = "04-10-2021", 
                                      daysOfWeek = "1,2,3,4,5", excludeDates = "", format = "") {
  
  url = paste("https://api.goswift.ly/run-times/", "septa", "/route/", routeKey, "/", "cumulative", sep = "")
  res = GET(url, query = list(beginTime = beginTime, endTime = endTime, format = format,
                              startDate = startDate, endDate = endDate, direction = direction, 
                              daysOfWeek= daysOfWeek),
            add_headers(Authorization = api_key))
  
  x <- fromJSON(res$content |> rawToChar())
  
  stops = ""
  
  if(res$status_code == "200") { 
    stops = x$data$filterData$tripPatterns$stops |> bind_rows() |> distinct(stopId, .keep_all = TRUE)
  } else {
    stops = ""
  }
  
  return(stops)
}

#' Call API by stop
#' 
#' @param stop_list list of stops (formatted by corridor)
#' @param filename dataset of stop and route GTFS
#' @returns table of route_ids and directions
get_runtime_by_stop <- function(agencyKey = "septa", routeKey = "1", tripKey, direction = "1", beginTime = "00:00", endTime = "23:59", startDate = "05-01-2021", 
                                endDate = "05-30-2021", daysOfWeek = "1,2,3,4,5", excludeDates = "", format = "") {
  
  url = paste("https://api.goswift.ly/run-times/", "septa", "/route/", routeKey, "/", "trip", "/", tripKey, "/by-stop", sep = "")
  
  # api call function
  f <- function() {
    
    res <- GET(url, query = list(beginTime = beginTime, endTime = endTime,
                                 format = format,
                                 startDate = startDate, endDate = endDate,
                                 direction = direction, daysOfWeek = daysOfWeek),
               add_headers(Authorization = api_key))
    
    return(res)
  }
  
  dat <- f()
  
  x <- fromJSON(dat$content |> rawToChar())$data$byStopRuntimeData |> unnest(cols = c(observedRuntimes))
  
  code <- ifelse(dat$status_code == "200", 
                 paste("Route:", routeKey, "Direction:", direction, "- Trip:", tripKey, "ran successfully"),
                 paste("Error", dat$status_code, "with trip", tripKey, "on route", routeKey))
  
  print(code)
  
  return(x)
}

# SANDBOX -----------------------------------------------------------------
api_key <- "7940c6ffb397e212179ffed1355bd9ef"


filename <- "data/stop_routes_2019.csv" 
stop_list <- c(10264, 10263, 14913, 3582)
routeKey_ <- '17'
tripKey_ <- "426241"
direction_ <- "0"
startDate <- "08-01-2023"
endDate <- "08-24-2023"

get_runtime_by_stop(routeKey=routeKey_, tripKey=tripKey_, direction=direction_)

