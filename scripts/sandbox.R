
# Set this environment variable according to your project config name in config.yml
Sys.setenv(R_CONFIG_ACTIVE = "19th")

source("scripts/source.R")

study_cor <- config::get("study_cor")
dates <- config::get("dates")

study_pop <- expand_grid(study_cor, dates)
study_pop$epoch <- names(dates[match(study_pop$dates, dates)])

gtfs <- read_gtfs(GTFS_FILE)
cor_routes <- build_corridor(CORRIDOR_FILE, gtfs, T, study_cor)


# study_panel <- map2(study_pop[1], study_pop[2], get_cor_runtime_panel,
#                     cor_routes = cor_routes)

cor1_before <- get_cor_runtime_panel(cor_routes, study_cor[1], dates[1])
cor1_after <- get_cor_runtime_panel(cor_routes, study_cor[1], dates[2])
cor2_before <- get_cor_runtime_panel(cor_routes, study_cor[2], dates[1])
cor2_after <- get_cor_runtime_panel(cor_routes, study_cor[2], dates[2])

study_panel <- list(paste0(study_cor[1], "_before") = cor1_before,
                    paste0(study_cor[1], "_after") = cor1_after,
                    paste0(study_cor[2], "_before") = cor2_before,
                    paste0(study_cor[2], "_after") = cor2_after)

# corridor stats statements -----------------------------------------------

study_panel_stats <- study_panel |> map(\(x) get_stats_descriptive(x, INTERVAL_BIN))
mapply(write_csv, study_panel_stats, paste0("out/runtimes_",names(study_panel_stats),".csv"))

# stop spacing ------------------------------------------------------------

septa_route_stop <- build_route_stop(septa)
