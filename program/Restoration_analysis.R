
### IDEA OF ANALYSIS ###


restoration_db <- read_excel(here("data/output/restoration_db.xlsx"))



### Databases we have:  
# 1 Global tree cover (existing and potential) (% and ha)
# 2 Indian restoration areas (ha)
# 3 Carbon sequestration potential from reforestation (CO2e per year)
# 4 Net Climate Impact of tree cover restoration (carbon storage and albedo change)  (Mg CO2e)
# 5 Constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon combined density under baseline climate (Mg C, stock)
# 6 Potential for forest regeneration in tropical regions (ha)
# 7 Global map of forest landscape restoration opportunities (Wide or Mosaic Restoration)
# 8 Afforestation/Reforestation: technical potential (annual and cumulative) and area + cost-effective potential (annual and cumulative) and area


### 1) Comparison 1, 2, 6, 7 (India)


Indiaforest_db <- restoration_db %>% 
  filter(Variable %in% c("Excluded_areas", "Protection", "Wide_scale_Restoration", "Wide scale", 
                         "Mosaic_Restoration", "Mosaic", "Tree_cover_potential", "Potential_forestregeneration_total")) %>%
  filter(iso3 %in% c("IND")) %>% 
  select(iso3, Variable, Value) 

Indiaforest_stacked <- Indiaforest_db %>%
  mutate(Group = case_when(
    Variable %in% c("Excluded_areas", "Protection", "Wide_scale_Restoration", "Mosaic_Restoration") ~ "Chaturvedietal_2018",
    Variable %in% c("Mosaic", "Wide scale") ~ "Potapovetal",
    Variable == "Tree_cover_potential" ~ "Bastinetal_2019",
    Variable == "Potential_forestregeneration_total" ~ "Williamsal_2024",
    TRUE ~ NA_character_
  )) %>%
  mutate(Value = Value / 1e6) %>%
  arrange(Group) %>%
  mutate(
    Group_label = case_when(
      Group == "Chaturvedietal_2018" ~ "Chaturvedi et al. (2018)",
      Group == "Potapovetal" ~ "Potapov et al. (2020)",
      Group == "Bastinetal_2019" ~ "Bastin et al. (2019)",
      Group == "Williamsal_2024" ~ "Williams et al. (2024)",
      TRUE ~ Group
    ),
    Variable_label = case_when(
      Variable %in% c("Wide scale", "Wide_scale_Restoration") ~ "Wide scale Restoration",
      Variable %in% c("Mosaic", "Mosaic_Restoration") ~ "Mosaic Restoration",
      Variable == "Protection" ~ "Protection",
      Variable == "Tree_cover_potential" ~ "Tree cover restoration potential",
      Variable == "Potential_forestregeneration_total" ~ "Tree cover restoration potential",
      TRUE ~ gsub("_", " ", Variable)
    )
  ) %>% 
  filter(Variable_label != "Excluded areas")


India_plot <- ggplot(Indiaforest_stacked, aes(x = Group_label, y = Value, fill = Variable_label)) +
  geom_col(position = "stack") + 
  labs(
    x = "",
    y = "Area (Mha)",
    fill = "",
    title = "India Restoration Potential",
  ) +
  scale_fill_manual(
    values = c(
      "Protection" = "#d95f02",    
      "Wide scale Restoration" = "#7570b3",
      "Mosaic Restoration" = "#e7298a",     
      "Tree cover restoration potential" = "#a6761d",
      "Forest regeneration potential" = "black"
    ),
    guide = guide_legend(nrow = 4)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    legend.position = "bottom",
    legend.box = "horizontal",
  )




# Save the plot
figure_directory <- here("output", "figures")
filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "India_restoration"), ".png")
png(
  filename = here(figure_directory, filename),
  units = "in", height = 12, width = 20, res = 300
)
print(India_plot)
dev.off()





### 2) Comparison 1, 6, 7, 8 (World)

Worldforest_db <- restoration_db %>% 
  filter(Variable %in% c("Wide scale", "Mosaic", "Tree_cover_potential",
                         "ars_tech_ha_tot", "ars_feas_ha_tot" )) %>%
  group_by(Variable) %>% 
  mutate(Value  = sum(Value)) %>% 
  select(-iso3) %>% 
  mutate(iso3 = ("WRD")) %>% 
  select(iso3, Variable, Value) %>% 
  distinct ()


Worldforest_stacked <- Worldforest_db %>% 
  mutate(Value = Value / 1e6) %>%
  mutate(
    Group = case_when(
      Variable %in% c("Wide scale", "Mosaic", "Protection") ~ "Potapovetal",
      Variable == "Tree_cover_potential" ~ "Bastinetal_2019",
      Variable %in% c("ars_tech_ha_tot", "ars_feas_ha_tot") ~ "Roeetal",
      TRUE ~ NA_character_
    ),
    Group_label = case_when(
      Group == "Potapovetal" ~ "Potapov et al. (2020)",
      Group == "Bastinetal_2019" ~ "Bastin et al. (2019)",
      Group == "Roeetal" ~ "Roe et al. (2021)",
      TRUE ~ Group
    ),
    Variable_label = case_when(
      Variable %in% c("Wide scale") ~ "Wide scale restoration",
      Variable %in% c("Mosaic") ~ "Mosaic restoration",
      Variable == "Protection" ~ "Protection restorationion",
      Variable == "Tree_cover_potential" ~ "Tree cover restoration potential",
      Variable == "ars_tech_ha_tot" ~ "Technical afforestation/reforestation potential (2015-2050)",
      Variable == "ars_feas_ha_tot" ~ "Cost-effective afforestation/reforestation potential (2015-2050)"
    )
  ) %>% 
  mutate(
    Axis_group = case_when(
      Group_label == "Potapov et al. (2020)" ~ Group_label,  # all Potapov together
      Variable %in% c("ars_tech_ha_tot") ~ "Roe et al. (2021)", 
      Variable %in% c("ars_feas_ha_tot") ~ "Roe et al. (2021) ", 
      TRUE ~ Group_label
    )
  )


label_source <- c(
  "Wide scale" = "Potapov et al. (2020)",
  "Protection" = "Potapov et al. (2020)",
  "Mosaic" = "Potapov et al. (2020)",
  "Tree_cover_potential" = "Bastin et al. (2019)",
  "ars_tech_ha_tot" = "Roe et al. (2021)",
  "ars_feas_ha_tot" = "Roe et al. (2021)"
)

World_plot <- ggplot(Worldforest_stacked, aes(x = Axis_group, y = Value, fill = Variable_label)) +
  geom_col(position = "stack") +
  labs(
    x = "",
    y = "Mha",
    fill = "",
    title = "Global Restoration Potential"
  ) +
  scale_fill_manual(
    values = c(
      "Protection restorationion" = "#d95f02",    
      "Wide scale restoration" = "#7570b3",
      "Mosaic restoration" = "#e7298a",     
      "Tree cover restoration potential" = "#a6761d",
      "Technical afforestation/reforestation potential (2015-2050)" = "#1b9e77",
      "Cost-effective afforestation/reforestation potential (2015-2050)" = "#f781bf"
    ),
    guide = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    legend.position = "bottom",
    legend.box = "horizontal"
  )



# Save the plot
figure_directory <- here("output", "figures")
filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "World_restoration"), ".png")
png(
  filename = here(figure_directory, filename),
  units = "in", height = 12, width = 20, res = 300
)
print(World_plot)
dev.off()




### 3) Comparison 4 and 5 (World)

# 4 Net Climate Impact of tree cover restoration (carbon storage and albedo change)  (Mg CO2e)
# 5 Constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon combined density under baseline climate (Mg C, stock)


carbon_db <- restoration_db %>% 
  filter(Variable %in% c("ClimateImpact_treecoverrestoration_total", "Potential_carbonland_total")) %>%
  group_by(Variable) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Value = ifelse(Variable == "ClimateImpact_treecoverrestoration_total", Value / 3.664, Value)) %>%
  mutate(iso3 = "WRD") %>% 
  mutate(
    Group = case_when(
      Variable == "ClimateImpact_treecoverrestoration_total" ~ "Hasleretal_2024",
      Variable == "Potential_carbonland_total" ~ "Walkeral_2022",
      TRUE ~ NA_character_
    ),
    Group_label = case_when(
      Group == "Hasleretal_2024" ~ "Hasler et al (2024)",
      Group == "Walkeral_2022" ~ "Walker et al (2022)",
      TRUE ~ Group
    )
  )



label <- c(
  "ClimateImpact_treecoverrestoration_total" = "Net Climate Impact of tree cover restoration (carbon storage and albedo change)",
  "Potential_carbonland_total" = "Total constrained unrealized potential aboveground and belowground biomass, and soil organic carbon under baseline climate"
)



carbon_plot <- ggplot(carbon_db, aes(x = Group_label, y = Value / 1e9, fill = Variable)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "ClimateImpact_treecoverrestoration_total" = "#d95f02",
    "Potential_carbonland_total" = "#7570b3"
  ),
  labels = label,
  guide = guide_legend(nrow = 2)
  ) +
  labs(
    x = "",
    y = "Pg C",
    fill = "",
    title = "World Carbon Storage"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 26),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.position = "bottom",
    legend.box = "horizontal"
  )


# Save the plot
figure_directory <- here("output", "figures")
filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "World_carbon"), ".png")
png(
  filename = here(figure_directory, filename),
  units = "in", height = 12, width = 24, res = 300
)
print(carbon_plot)
dev.off()

### 4) Comparison 4 and 8 (World)


affor_refo_db <- restoration_db %>% 
  filter(Variable %in% c("ClimateImpact_treecoverrestoration_total", "ars_feascum", "ars_techcum")) %>%
  group_by(Variable) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(iso3 = "WRD") %>% 
  mutate(
    Value = case_when(
      Variable == "ClimateImpact_treecoverrestoration_total" ~ Value / 1e9,  # Mg => Gt
      Variable %in% c("ars_feascum", "ars_techcum") ~ Value / 1e3,                 
      TRUE ~ Value
    ),
    Group = case_when(
      Variable == "ClimateImpact_treecoverrestoration_total" ~ "Hasleretal_2024",
      Variable %in% c("ars_feascum", "ars_techcum") ~ "Roeetal",
      TRUE ~ NA_character_
    ),
    Group_label = case_when(
      Group == "Hasleretal_2024" ~ "Hasler et al (2024)",
      Group == "Roeetal" ~ "Roe et al (2021)",
      TRUE ~ Group
    )
  )


label <- c(
  "ClimateImpact_treecoverrestoration_total" = "Net Climate Impact of tree cover restoration (carbon storage and albedo change)",
  "ars_feascum" = "Potential cumulative Reforestation (tropics) (Cost-effective)",
  "ars_techcum" = "Potential cumulative Reforestation (tropics) (Technical)"
)


label_source <- c(
  "ClimateImpact_treecoverrestoration_total" = "Hasler et al (2024)",
  "ars_feascum" = "Roe et al (2021)",
  "ars_techcum" = "Roe et al (2021)"
)

affor_refo_plot <- ggplot(affor_refo_db, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.6) +  # each variable is its own column
  scale_fill_manual(
    values = c(
      "ClimateImpact_treecoverrestoration_total" = "#1b9e77",
      "ars_feascum" = "#7570b3",
      "ars_techcum" = "#e7298a"
    ),
    labels = label,
    guide = guide_legend(nrow = 3)
  ) +
  scale_x_discrete(labels = label_source) +
  labs(
    x = "",
    y = "Gt CO2",
    fill = "",
    title = "Global Potential and Net Climate Impact of Afforestation and Reforestation"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    legend.position = "bottom",
    legend.box = "horizontal",
  )


# # Save the plot
# figure_directory <- here("output", "figures")
# filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "World_climate"), ".png")
# png(
#   filename = here(figure_directory, filename),
#   units = "in", height = 12, width = 20, res = 300
# )
# print(affor_refo_plot)
# dev.off()







TropicalForest_db <- restoration_db %>% 
  filter(Variable %in% c("Restoration_Potential", "Potential_forestregeneration_total", "refor_tech_ha_tot", "refor_feas_ha_tot")) %>%
  group_by(Variable) %>% 
  mutate(Value  = sum(Value)) %>% 
  select(-iso3) %>% 
  mutate(iso3 = ("WRD")) %>% 
  select(iso3, Variable, Value) %>% 
  distinct () %>% 
  mutate(Value = Value / 1e6)

label_source <- c(
  "Restoration_Potential" = "Shyamsundar et al. (2022)",
  "Potential_forestregeneration_total" = "Williams et al. (2024)",
  "refor_tech_ha_tot" = "Roe et  al. (2020)" ,
  "refor_feas_ha_tot" = "Roe et  al. (2020)"
)

variable_labels <- c(
  "Restoration_Potential" = "Tree cover restoration in forests, wetlands, and agricultural lands",
  "Potential_forestregeneration_total" = "Potential for forest regeneration",
  "refor_tech_ha_tot" = "Total technical reforestation potential (2020-2050)",
  "refor_feas_ha_tot" = "Total cost-effective reforestation potential (2020-2050)"
)

TropicalForest_plot <- ggplot(TropicalForest_db, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.6) +
  scale_x_discrete(labels = label_source) +
  scale_fill_manual(
    values = c(
      "Restoration_Potential" = "#1b9e77",
      "Potential_forestregeneration_total" = "#d95f02",
      "refor_tech_ha_tot" = "#7570b3",
      "refor_feas_ha_tot" = "#e7298a"
    ),
    labels = variable_labels,
    guide = guide_legend(nrow = 4)
  ) +
  labs(
    x = "",
    y = "Mha",
    fill = "",
    title = "Forest regeneration potential in tropical regions"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 26),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.position = "bottom",
    legend.box = "horizontal"
  )



# # Save the plot
figure_directory <- here("output", "figures")
filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "Tropic_forest"), ".png")
png(
  filename = here(figure_directory, filename),
  units = "in", height = 12, width = 20, res = 300
)
print(TropicalForest_plot)
dev.off()




# 
# ### 2) Compare "Potential for forest regeneration in tropical regions" vs  Global tree cover (existing and potential)
# 
# 
# # Potential forests reg in tropical regions
# treecover_top6 <- restoration_db %>%
#   filter(Variable == "Potential_forestregeneration_total") %>%
#   arrange(desc(Value)) %>%
#   slice_head(n = 6)
# 
# 
# forestcover_db <- restoration_db %>% 
#   filter(Variable %in% c("Tree_cover_ha", "Potential_forestregeneration_total")) %>%
#   filter(iso3 %in% c("BRA", "IDN", "COL", "MEX", "COD", "CHN")) %>% 
#   select(iso3, Variable, Value) %>% 
#   pivot_wider(names_from = Variable, values_from = Value) %>% 
#   
#   select(iso3, Tree_cover_ha, Potential_forestregeneration_total) %>% 
#   pivot_longer(
#     cols = c(Tree_cover_ha, Potential_forestregeneration_total),
#     names_to = "Type",
#     values_to = "Value"
#   )
# 
# 
# labels_map <- c(
#   "IDN" = "Indonesia",
#   "COD" = "DR Congo",
#   "CHN" = "China",
#   "USA" = "United States",
#   "MEX" = "Mexico",
#   "BRA" = "Brazil",
#   "COL" = "Colombia"
# )
# 
# labels_map_type <- c(
#   "Tree_cover_ha" = "Current Tree Cover (Mha)",
#   "Potential_forestregeneration_total" = "Potential Forest Regeneration (Mha)"
# )
# 
# forestcover_plot <- ggplot(forestcover_db, aes(x = iso3, y = Value / 1e6, fill = Type)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     x = "",
#     y = "Forest cover & Potential (Mha)",
#     fill = "",
#     title = "Current Tree Cover vs Potential Forest Regeneration"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(labels = labels_map) +
#   scale_fill_manual(values = c("Tree_cover_ha" = "#1f77b4", 
#                                "Potential_forestregeneration_total" = "#ff7f0e"),
#                     labels = labels_map_type)
# 
# 


# ### 1) Compare "Aboveground and Belowground Biomass Carbon Density" and "Constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon combined density under baseline climate"
# # Aboveground and Belowground Biomass Carbon Density => Biomass_Carbon
# # Constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon combined density under baseline climate => Potential_carbonland_total
# # Current Carbon Stock (above and below biomass)" vs Unrealized Potential Carbon Stock (above and below biomass + SOC)
# 
# 
# biomass_db <- restoration_db %>% 
#   filter(Variable %in% c("Biomass_Carbon", "Potential_carbonland_total")) %>%
#   filter(iso3 %in% c("BRA", "RUS", "USA", "CAN", "COD", "IDN", "CHN")) %>% 
#   select(iso3, Variable, Value) %>% 
#   pivot_wider(names_from = Variable, values_from = Value) %>% 
# 
#   select(iso3, Biomass_Carbon, Potential_carbonland_total) %>% 
#   pivot_longer(
#     cols = c(Biomass_Carbon, Potential_carbonland_total),
#     names_to = "Type",
#     values_to = "Value"
#   )
# 
# labels_map <- c(
#   "IDN" = "Indonesia",
#   "COD" = "DR Congo",
#   "CHN" = "China",
#   "USA" = "United States",
#   "RUS" = "Russia",
#   "BRA" = "Brazil",
#   "CAN" = "Canada"
# )
# 
# labels_map_type <- c(
#   "Biomass_Carbon" = "Current Carbon Stock (above and below biomass)",
#   "Potential_carbonland_total" = "Unrealized Potential Carbon Stock (above and below biomass + SOC)"
# )
# 
# biomass_plot <- ggplot(biomass_db, aes(x = iso3, y = Value / 1e9, fill = Type)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     x = "",
#     y = "Carbon Stock (Pg C)",
#     fill = "",
#     title = "Current vs Unrealized Potential Carbon Stock by Country"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(labels = labels_map) +
#   scale_fill_manual(values = c("Biomass_Carbon" = "#1f77b4", 
#                                "Potential_carbonland_total" = "#ff7f0e"),
#                     labels = labels_map_type)
# 
