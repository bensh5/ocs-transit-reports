
#' Get latest SEPTA GTFS. Only run intermittently
#' 
#' @return SEPTA rail and bus GTFS
get_gtfs_septa <- function() {
  download.file("https://github.com/septadev/GTFS/releases/latest/download/gtfs_public.zip",
                destfile = zip.file)
  
  unzip(zip.file, exdir = "data")
  #unzip("data/google_bus.zip", exdir = "data/gtfs")
  
  file.remove(zip.file)
}


#' Build GTFS route-stop board
#'
#' @param f_gtfs 
#'
#' @return route-stop board associating each stop with its route
#' (or multiple routes)
build_route_stop <- function(f_gtfs) {
  f_gtfs$stops |> 
    left_join(f_gtfs$stop_times) |> 
    left_join(f_gtfs$trips) |> 
    distinct(stop_id, stop_sequence, .keep_all = TRUE) |> 
    stops_as_sf() |> 
    select(route_id, stop_id, stop_name, direction_id, stop_sequence) |> 
    arrange(route_id, direction_id, stop_sequence)
}

#' Build the analysis corridors by route according to supplied GTFS. 
#' Each corridor can contain the stops of multiple routes that traverse it, 
#' as well as subcorridors according to project phasing.
#' 
#' @param f_corridors corridors CSV, formatted as: 
#' corridor name, subcorridor name, compass direction, start stop, end stop
#' @param f_gtfs gtfs ZIP
#' @param f_filter should corridors CSV be filtered to single corridor of interest?
#' @param cor_name corridor names to filter to
#' @return stop-level dataset of corridor stops w/ direction, sequence
build_corridor <- function(f_corridors, f_gtfs, f_filter = F, cor_name) {
  # read corridors file
  corridors <- read_csv(f_corridors) |> 
    pivot_longer(cols = c(start, end), 
                 names_to = "marker", values_to = "stop_id") |> 
    mutate(stop_id = as.character(stop_id))
  
  #filter for specific corridor name
  corridors <- if(f_filter) filter(corridors, corridor %in% cor_name) else corridors
  
  route_stops <- build_route_stop(f_gtfs)
  
  # merge with GTFS intake
  corridors <- corridors |> 
    left_join(route_stops |> 
                select(stop_id, route_id, direction_id, stop_sequence) |> 
                st_drop_geometry(), by = "stop_id", relationship = "many-to-many") |> 
    group_by(subcorridor, route_id, direction_id) |> filter(n() > 1) |> ungroup()

  # With multiple routes on a corridor, direction_id can be arbitrarily 0 or 1
  # and is not automatically consistent with the corridor compass direction.
  # We define 0 as EB/NB and 1 as WB/SB, and have to switch direction_id if necessary.
  routes_dir_change <- corridors |> 
    mutate(dir_change = direction_compass %in% c('NB','EB') & direction_id == 1) |> 
    arrange(-dir_change) |> distinct(route_id, .keep_all = T) |> 
    select(route_id, dir_change)
  
  # and expand by full stop sequence per direction
  corridors <- corridors |> 
    # manually fix direction_id to compass direction
    mutate(direction_id = if_else(direction_compass %in% c('NB','EB'), 0, 1)) |> 
    arrange(corridor, subcorridor, route_id, direction_id, stop_sequence) |> 
    group_by(corridor, subcorridor, route_id, direction_id) |> 
    complete(stop_sequence = min(stop_sequence):max(stop_sequence),
             direction_compass = min(direction_compass)) |> 
    mutate(marker = if_else(is.na(marker), "middle", marker))
  
  # manually fix route_stops as well
  route_stops <- route_stops |> 
    right_join(routes_dir_change, by = "route_id") |> 
    mutate(direction_id = if_else(dir_change, if_else(direction_id == 0, 1, 0), direction_id)) |> 
    select(-dir_change)
  
  # inner join to get all corridor stop_id and geometry
  # direction_id follows Swiftly format 
  route_stops_cor <- route_stops |> 
    inner_join(select(corridors, -stop_id), by = c("route_id", "direction_id", "stop_sequence")) |> 
    relocate(geometry, .after = last_col()) |> 
    mutate(direction_id = paste0("direction-",direction_id)) |> 
    nest(.by = c(corridor, subcorridor, stop_id, stop_name, direction_id, direction_compass, marker)) |> 
    group_by(subcorridor, direction_id) |> mutate(stopOrderCor = row_number()) |> 
    unnest(data) |> arrange(corridor, subcorridor, direction_id, direction_compass, stopOrderCor) |> 
    relocate(stopOrderCor, .after = stop_sequence) |> ungroup()
  
  return(route_stops_cor)
}
