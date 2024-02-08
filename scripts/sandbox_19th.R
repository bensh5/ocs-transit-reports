source("scripts/source.R")

# user inputs -------------------------------------------------------------

study_cor <- c("19th", "Market")
dates <- c("before"="2019-10", "after"="2023-10")

study_pop <- expand_grid(study_cor, dates)
study_pop$epoch <- names(dates[match(study_pop$dates, dates)])

# main function processing statements -------------------------------------

septa <- read_gtfs("data/google_bus.zip")
cor_routes <- build_corridor("data/corridors.csv", septa, T, study_cor)

# study_panel <- map2(study_pop[1], study_pop[2], get_cor_runtime_panel)

nineteenth_before <- get_cor_runtime_panel(cor_routes, study_cor[1], dates[1])
nineteenth_after <- get_cor_runtime_panel(cor_routes, study_cor[1], dates[2])
market_before <- get_cor_runtime_panel(cor_routes, study_cor[2], dates[1])
market_after <- get_cor_runtime_panel(cor_routes, study_cor[2], dates[2])

study_panel <- list("nineteenth_before" = nineteenth_before,
                    "nineteenth_after" = nineteenth_after,
                    "market_before" = market_before,
                    "market_after" = market_after)

# corridor stats statements -----------------------------------------------

study_panel_stats <- study_panel |> map(\(x) get_stats_descriptive(x))
mapply(write_csv, study_panel_stats, paste0("out/runtimes_",names(study_panel_stats),".csv"))

# stop spacing ------------------------------------------------------------

septa_route_stop <- build_route_stop(septa)

