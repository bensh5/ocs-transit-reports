library(config)
library(ggh4x)
library(hms)
library(httr)
library(lubridate)
library(jsonlite)
library(rrapply)
library(sf)
library(tidytransit)
library(tidyverse)
library(timetk)
library(plotly)

# source main functions ---------------------------------------------------

fxn_source = list.files("scripts", 
                        pattern = "^fxn_.*.R$", full.names = T, ignore.case = T)
sapply(fxn_source, source, .GlobalEnv)

source("scripts/otis_aes.R")

# helper functions --------------------------------------------------------

pct_diff <- function(t1, t2) {
  t1 = as.numeric(t1)
  t2 = as.numeric(t2)
  
  return((t1 - t2) / ((t1 + t2) / 2))
}

pct_chg <- function(t1, t2) {
  t1 = as.numeric(t1)
  t2 = as.numeric(t2)
  
  return((t2 - t1) / abs(t1))
}

quant_num <- function(speed, level) {
  as.numeric(unlist(quantile(speed, probs=c(level), na.rm = TRUE)))
}

coord_y_datetime <- function(xlim = NULL, ylim = NULL, expand = TRUE) {
  if (!is.null(ylim)) {
    ylim <- lubridate::as_datetime(ylim)
  }
  ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = expand)
}

# default parameters, constant across all corridors -----------------------

swiftly_api_historical <- "7940c6ffb397e212179ffed1355bd9ef"
GTFS_FILE <- "data/google_bus.zip"
CORRIDOR_FILE <- "data/corridors.csv"

DATE_START_MIN <- "02-01-2020"
DATE_END_MAX <- "12-31-2024"
DATE_EXCLUDE <- "07-04-2019,07-04-2022"
DAYS_OF_WEEK <- "2,3,4"
INTERVAL_BIN <- "10 minutes"

time_int <- list(preAM = c("04:00","05:59"),
                 AMpeak = c("06:00","08:59"),
                 base = c("09:00","14:59"),
                 PMpeak = c("15:00","17:59"),
                 evening = c("18:00","21:59"),
                 latenight = c("22:00","23:59"),
                 owl = c("00:00","04:59"))

route_alias <- c(`331` = "33S",
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

route_exclude <- c("MFO", "BSO", "HRS", "HXH", "KLS", "KSL", "WCS", "WPA", "WPS")