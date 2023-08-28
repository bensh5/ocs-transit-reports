library(httr)
library(jsonlite)
library(sf)
library(tidyverse)

TIME_BEGIN <- "00:00"
TIME_END <- "23:59"
DATE_START <- "04-15-2023"
DATE_END <- "04-17-2023"
DAYS_OF_WEEK <- "1,2,3,4,5"

DATE_EXCLUDE <- ""

#' fix routes of route list
#' 
#' @param route_list
#' @returns formatted route list
fix_routes <- function(route_list){
  
  char_vect <- c(`331` = "33S",
                 `101` = "10B",
                 `471` = "47M",
                 `701` = "BSO",
                 `702` = "C",
                 `703` = "G",
                 `704` = "HRS",
                 `705` = "HXH",
                 `706` = "J",
                 `707` = "K",
                 `708` = "KLS",
                 `709` = "KSL",
                 `710` = "L",
                 `711` = "MFO",
                 `712` = "R",
                 `713` = "13B",
                 `714` = "WCS",
                 `715` = "WPA",
                 `716` = "WPS",
                 `734` = "34B",
                 `801` = "H",
                 `802` = "XH",
                 `500` = "BLVDDIR")
  
  output <- recode(route_list, !!!char_vect)
  
  #output <- list_modify(route_list, `500` = "BLVDDIR")
  
  return(output)
}
