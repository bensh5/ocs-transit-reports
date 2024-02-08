# single corridor-year visualization --------------------------------------

plot_runtime_onecor <- function(cor, epoch) {
  runtimes <- read_csv(paste0("out/runtimes_",cor,"_",epoch,".csv"))
  
  ggplot(runtimes, aes(stopName, depTime, fill = p90_travelTime)) + 
    geom_tile() +
    coord_y_datetime(ylim = c(max(runtimes$depTime), 
                              min(runtimes$depTime))) +
    scale_fill_viridis_c() +
    facet_wrap(~subcorridor, scales = "free_x") +
    theme_otis() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = cor)
}

map2(study_panel_stats, names(study_panel_stats), \(x,y) plot_runtime_onecor(x, y))

# before/after comparision visualization ----------------------------------

before <- read_csv(paste0("out/runtimes_", "Chestnut", "_before.csv"))
after <- read_csv(paste0("out/runtimes_", "Chestnut", "_after.csv"))

comp <- left_join(before, after, by = c("subcorridor", "direction", "stopOrderCor",
                                        "stopId", "stopName", "depTime")) |> 
  group_by(subcorridor) |> 
  mutate(stopName = fct_reorder(stopName, stopOrderCor),
         pd_m_travelTime = pct_diff(m_travelTime.y, m_travelTime.x),
         pd_p90_travelTime = pct_diff(p90_travelTime.y, p90_travelTime.x),
         pc_m_travelTime = pct_chg(m_travelTime.x, m_travelTime.y),
         pc_p90_travelTime = pct_chg(p90_travelTime.x, p90_travelTime.y))

ggplot(comp, aes(stopName, depTime, fill = pd_p90_travelTime)) + 
  geom_tile() +
  coord_y_datetime(ylim = c(max(comp$depTime), 
                            min(comp$depTime))) +
  scale_fill_scico(palette = "bam", direction = -1, 
                   name = "% diff", labels = scales::percent) +
  facet_wrap(~subcorridor, scales = "free_x") +
  theme_otis() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Percent difference in travel time, Walnut, 2019-2023 (October)",
       subtitle = "Source: SEPTA, Swiftly")
ggsave("img/runtime_walnut_pctdiff_p90.png", height = 5, width = 12)

comp |> group_by(subcorridor) |> 
  summarise(y2019 = sum(m_travelTime.x, na.rm = T)/1080,
            y2022 = sum(m_travelTime.y, na.rm = T)/1080,
            pctchg = sprintf("%1.1f%%", pct_chg(y2019,y2022)*100)) |> 
  pivot_longer(cols = starts_with("y20"), names_to = "year") |> 
  mutate(pctchg = if_else(year == "y2022", NA, pctchg)) |> 
ggplot(aes(x = subcorridor, y = value)) +
  geom_col(aes(fill = year), position = "dodge") +
  geom_label(aes(label = pctchg), stat = "unique") +
  scale_fill_phl() +
  theme_otis() +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  labs(title = "Percent Change in travel time, Chestnut",
       y = "Average corridor runtime (minutes)")
ggsave("img/runtime_chestnut_comp.png", height = 5, width = 5)

right_turn <- c("23rd St", "21st St", "19th St", "17th St", "15th St", "Broad St",
                "12th St", "10th St", "8th St", "6th St", "4th St - MBNS", "2nd St")
comp$lane <- if_else(comp$stopName %in% right_turn, "Shared right", "Bus only")

comp |> group_by(subcorridor, lane) |> 
  summarise(y2019 = sum(m_travelTime.x, na.rm = T)/1080,
            y2022 = sum(m_travelTime.y, na.rm = T)/1080,
            pctchg = sprintf("%1.1f%%", pct_chg(y2019,y2022)*100)) |> 
  pivot_longer(cols = starts_with("y20"), names_to = "year") |> 
  mutate(pctchg = if_else(year == "y2022", NA, pctchg)) |> 
ggplot(aes(x = interaction(lane, subcorridor), y = value)) +
  geom_col(aes(fill = year), position = "dodge") +
  geom_label(aes(label = pctchg), stat = "unique") +
  scale_x_discrete(NULL, guide = "axis_nested") + 
  scale_fill_phl() +
  theme_otis() +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  labs(title = "Right-turn Lane Comparison, Chestnut",
       y = "Average corridor runtime (minutes)")
ggsave("img/runtime_chestnut_comp_lane.png", height = 4, width = 6)
