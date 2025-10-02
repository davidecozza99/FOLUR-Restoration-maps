##### Restoration database: simple graphs and analysis #####





# ### FIGURES AND ANALYSIS ###
# 
# # Tree cover
# treecover_top6 <- restoration_db %>%
#   filter(Variable == "Tree_cover") %>%
#   arrange(desc(Value)) %>%
#   slice_head(n = 6)
# 
# 
# labels_map <- c(
#   "VCT" = "Saint Vincent & Grenadines",
#   "MYS" = "Malaysia",
#   "SLB" = "Solomon Islands",
#   "GUY" = "Guyana",
#   "SUR" = "Suriname",
#   "GUF" = "French Guiana"
# )
# 
# # Plot with custom labels
# treecover_plot <- ggplot(treecover_top6, aes(x = reorder(iso3, -Value), y = Value, fill = iso3)) +
#   geom_col(show.legend = FALSE) +
#   theme_minimal() +
#   labs(
#     x = "",
#     y = "Tree cover (%)",
#     title = "Top 6 Countries with Highest Tree Cover"
#   ) +
#   scale_x_discrete(labels = labels_map)
# 
# 
# 
# 
# # Biomass_Carbon => aaboveandbelowground => carbon potential in 2010
# biomass_top6 <- restoration_db %>%
#   filter(Variable == "Biomass_Carbon") %>%
#   arrange(desc(Value)) %>%
#   slice_head(n = 6)
# 
# # Labels for biomass plot
# labels_map_biomass <- c(
#   "IDN" = "Indonesia",
#   "COD" = "DR Congo",
#   "CAN" = "Canada",
#   "USA" = "United States",
#   "RUS" = "Russia",
#   "BRA" = "Brazil"
# )
# 
# # Plot
# biomass_plot <- ggplot(biomass_top6, aes(x = reorder(iso3, -Value), y = Value / 1e9, fill = iso3)) +
#   geom_col(show.legend = FALSE) +
#   theme_minimal() +
#   labs(
#     x = "",
#     y = "Biomass Carbon (Pg C)", 
#     title = "Top 6 Countries with Highest Biomass Carbon"
#   ) +
#   scale_x_discrete(labels = labels_map_biomass)
# 
# 
# 
# 
# 
# # Biomass_Carbon => WALKER unrealized potential => potential - current
# unrealizedcarbon_top6 <- restoration_db %>%
#   filter(Variable == "Potential_carbonland_total") %>%
#   arrange(desc(Value)) %>%
#   slice_head(n = 6)
# 
# 
# labels_map <- c(
#   "IDN" = "Indonesia",
#   "COD" = "DR Congo",
#   "CHN" = "China",
#   "USA" = "United States",
#   "RUS" = "Russia",
#   "BRA" = "Brazil"
# )
# 
# # Plot
# unrealizedcarbon_plot <- ggplot(unrealizedcarbon_top6, aes(x = reorder(iso3, -Value), y = Value / 1e9, fill = iso3)) +
#   geom_col(show.legend = FALSE) +
#   theme_minimal() +
#   labs(
#     x = "",
#     y = "Biomass and Soil Organic Carbon (Pg C)", 
#     title = "Top 6 Countries with Highest Unrealized potential biomass and carbon"
#   ) +
#   scale_x_discrete(labels = labels_map)
# 
# 
# 
# 
# 
# 
# restoration_wide <- restoration_db %>%
#   filter(Variable %in% c("Biomass_Carbon_Density", "country_area")) %>%
#   select(iso3, Variable, Value) %>%  # drop source/unit to avoid duplicates
#   pivot_wider(names_from = Variable, values_from = Value)
# 
# mean_density <- restoration_wide %>%
#   summarise(weighted_mean = weighted.mean(Biomass_Carbon_Density, country_area, na.rm = TRUE)) %>%
#   pull(weighted_mean)
# 
# biomass_density_top6 <- restoration_wide %>%
#   arrange(desc(Biomass_Carbon_Density)) %>%
#   slice_head(n = 6)
# 
# biomass_density_plot <- ggplot(biomass_density_top6, 
#                                aes(x = reorder(iso3, Biomass_Carbon_Density), 
#                                    y = Biomass_Carbon_Density, fill = iso3)) +
#   geom_col(show.legend = FALSE) +
#   geom_hline(yintercept = mean_density, linetype = "dashed", color = "red") +
#   theme_minimal() +
#   labs(
#     x = "",
#     y = "Biomass Carbon Density (Mg/ha)",
#     title = "Top 6 Countries with Highest Biomass Carbon Density",
#     subtitle = paste0("Global weighted mean = ", round(mean_density, 2), " Mg/ha")
#   )

