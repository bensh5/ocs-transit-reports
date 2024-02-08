source("scripts/source.R")

# user inputs -------------------------------------------------------------

study_cor <- c("Chestnut", "Walnut")
dates <- c("before"="2019-10", "after"="2023-10")

study_pop <- expand_grid(study_cor, dates)
study_pop$epoch <- names(dates[match(study_pop$dates, dates)])

# main function processing statements -------------------------------------

septa <- read_gtfs("data/google_bus.zip")
cor_routes <- build_corridor("data/corridors.csv", septa, T, study_cor)

# study_panel <- map2(study_pop[1], study_pop[2], get_cor_runtime_panel)

chestnut_before <- get_cor_runtime_panel(cor_routes, study_cor[1], dates[1])
chestnut_after <- get_cor_runtime_panel(cor_routes, study_cor[1], dates[2])
walnut_before <- get_cor_runtime_panel(cor_routes, study_cor[2], dates[1])
walnut_after <- get_cor_runtime_panel(cor_routes, study_cor[2], dates[2])

study_panel <- list("chestnut_before" = chestnut_before,
                    "chestnut_after" = chestnut_after,
                    "walnut_before" = walnut_before,
                    "walnut_after" = walnut_after)

# corridor stats statements -----------------------------------------------

study_panel_stats <- study_panel |> map(\(x) get_stats_descriptive(x))
mapply(write_csv, study_panel_stats, paste0("out/runtimes_",names(study_panel_stats),".csv"))

# stop spacing ------------------------------------------------------------

septa_route_stop <- build_route_stop(septa)

