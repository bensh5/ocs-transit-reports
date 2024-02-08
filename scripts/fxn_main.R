#' Swiftly corridor runtimes function
#'
#' @param cor_routes corridor routes processed from build function
#' @param cor single corridor from cor_routes
#' @param yearmon year-month to process one month of runtime data
#' @return a tidy panel dataframe of stop runtimes for one month
#' @export
#'
get_cor_runtime_panel <- function(cor_routes, cor, yearmon) {
  dates <- c(format(ym(yearmon), "%m-%d-%Y"), 
             format(ceiling_date(ym(yearmon)+months(1)-days(1)), "%m-%d-%Y"))

  cor_routes <- cor_routes |> filter(corridor %in% cor)
  
  cor_stops <- cor_routes |> distinct(stop_id, .keep_all = T) |> 
    arrange(route_id, direction_id, stopOrderCor)
  
  # cor_routes <- map(split(cor_routes, cor_routes$route_id),
  #                  \(x) split(x, x[['direction_id']]))
  
  # nest to follow format for Swiftly API call
  cor_routes <- cor_routes |> group_by(corridor, route_id) |>
    nest(.key = "direction") |>
    mutate(direction = map(direction, ~.x |> 
                             group_by(direction_id, direction_compass) |> 
                             nest(.key = "stoplist"))) |>
    ungroup()
  
  cor_trips_runtimes <- 
    map(unique.default(sapply(cor_routes$route_id, unique)), 
        \(x) get_runtime_by_trip(routeKey = x,
                                 startDate = dates[1], endDate = dates[2])) |> 
    setNames(unique.default(sapply(cor_routes$route_id, unique)))
  
  cor_trips <- rrapply(cor_trips_runtimes, 
                       condition = \(x, .xname) .xname %in% c("tripId","scheduledDepartureSeconds"), 
                       how = "melt")
  cor_trips_a <- cor_trips |> filter(L3 == 'tripId') |> 
    rename(trip_id = value) |> select(-L3)
  cor_trips_b <- cor_trips |> filter(L3 == 'scheduledDepartureSeconds') |> 
    rename(depTime = value) |> select(-L3)
  cor_trips <- left_join(cor_trips_a, cor_trips_b, by = c("L1","L2")) |> 
    unnest(c(trip_id, depTime)) |> 
    rename(route_id = L1, direction = L2) |> 
    mutate(depTime = ymd_hms(paste('2021-01-01',hms::hms(depTime))))
  rm(cor_trips_a, cor_trips_b)
  
  cor_routestops_runtimes <- cor_trips |> 
    mutate(runtimes = map2(cor_trips$route_id, cor_trips$trip_id, 
                           \(x,y) get_runtime_by_stop(routeKey = x, tripKey = y,
                                                      startDate = dates[1],
                                                      endDate = dates[2])))
  
  cor_routestops_runtimes_panel <- cor_routestops_runtimes |> unnest(runtimes) |> 
    filter(stopId %in% cor_stops$stop_id) |> 
    left_join(select(cor_stops, subcorridor, stop_id, stopOrderCor), 
              by = c("stopId" = "stop_id"), relationship = "many-to-many") |> 
    relocate(stopOrderCor, .after = stopOrder)
  
  cor_routestops_runtimes_panel
}

#' Swiftly corridor speed function
#'
#' @param cor_routes corridor routes processed from build function
#' @param cor single corridor from cor_routes
#' @param yearmon year-month to process one month of runtime data
#' @return a tidy panel dataframe of stop runtimes for one month
#' @export
#'
get_cor_speed_panel <- function(cor_routes, cor, yearmon) {
  dates <- c(format(ym(yearmon), "%m-%d-%Y"), 
             format(ceiling_date(ym(yearmon)+months(1)-days(1)), "%m-%d-%Y"))
  
  cor_routes_distinct <- cor_routes |> distinct(route_id, direction_id)
  
  cor_routes_speeds <- map2(cor_routes_distinct$route_id,
                            str_sub(cor_routes_distinct$direction_id,-1), 
                            \(x,y) get_speedmap_low(route = x,
                                                     direction = y,
                                                     startDate = dates[1],
                                                     endDate = dates[2]))
  
  return(bind_rows(cor_routes_speeds))
}

#' Takes in complete corridor route-stops panel and computes descriptive stats
#' 
#' @return dataframe of descriptive stats for corridor
get_stats_descriptive <- function(panel) {
  stat_sum <- panel |> group_by(subcorridor, direction, stopOrderCor, stopId, stopName) |> 
    summarise_by_time(.date_var = depTime, .by = "10 minutes",
    tripCount = n(),
    m_dwellTime = mean(dwellTime),
    m_travelTime = mean(travelTime),
    m_runTime = mean(runTime),
    sd_dwellTime = sd(dwellTime),
    sd_travelTime = sd(travelTime),
    sd_runTime = sd(runTime),
    p10_travelTime = quant_num(travelTime, 0.1),
    p90_travelTime = quant_num(travelTime, 0.9),
    p95_travelTime = quant_num(travelTime, 0.95),
  ) |> 
  mutate(stopName = str_sub(str_extract(stopName, "&[:blank:].*"),3,-1),
         stopName = fct_reorder(stopName, stopOrderCor))
  
  stat_sum
}
