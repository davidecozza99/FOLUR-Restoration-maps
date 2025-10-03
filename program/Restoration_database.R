# Cleaning datasets and merging them into one, so that country-level comparison and analysis is possible

#Library
library(readxl)
library(tidyverse)
library(shinyBS)
library(shinyWidgets)
library(rintrojs)
library(latex2exp)
library(scales)
library(ggplot2)
library(gridExtra)
library(here)
library(openxlsx)
library(writexl)
library(gridExtra)
library(grid)
library(cowplot)
library(ggbreak)
library(ggprism)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(sf)
library(geojsonsf)

here()



# # Aboveground and Belowground Biomass Carbon Density for the year 2010
# 
# Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics <- read_csv(here("data/input/Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics.csv")) %>%
#   group_by(iso3) %>%
#   summarise(
#     country_area = sum(area, na.rm = TRUE),
#     Biomass_Carbon_Density = sum(`sum`, na.rm = TRUE) / country_area,
#     Biomass_Carbon = sum(`sum`, na.rm = TRUE)
#   ) %>%
#   pivot_longer(
#     cols = c(Biomass_Carbon_Density, country_area, Biomass_Carbon),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     unit = case_when(
#       Variable == "Biomass_Carbon_Density" ~ "Mg/ha",
#       Variable == "country_area" ~ "ha",
#       Variable == "Biomass_Carbon" ~ "Mg"
#     ),
#     source = "Aboveground_and_Belowground_Biomass_Carbon_Density",
#     description = case_when(
#       Variable == "Biomass_Carbon_Density" ~ "Aboveground and Belowground Biomass Carbon Density",
#       Variable == "Biomass_Carbon" ~ "Aboveground and Belowground Total Biomass Carbon",
#       Variable == "country_area" ~ "Area of the country"
#     )
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source)
# 
# 
# # Global tree cover (existing and potential)
# Bastin_Zonal_Statistics <- read_csv(here("data/input/Bastin_Zonal_Statistics.csv")) %>%
#   group_by(iso3) %>%
#   summarise(
#     country_area = sum(area, na.rm = TRUE),
#     Tree_cover = sum(`mean` * area, na.rm = TRUE) / sum(area, na.rm = TRUE),
#     Tree_cover_ha = sum(`mean` * area, na.rm = TRUE) / 100
#   ) %>%
#   pivot_longer(
#     cols = c(Tree_cover, Tree_cover_ha, country_area),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     unit = case_when(
#       Variable == "Tree_cover" ~ "%",
#       Variable == "Tree_cover_ha" ~ "ha",
#       Variable == "country_area" ~ "ha"
#     ),
#     source = "Bastinetal_2019",
#     description = case_when(
#       Variable == "Tree_cover" ~ "Global tree cover (existing and potential, % of land)",
#       Variable == "Tree_cover_ha" ~ "Global tree cover (existing and potential, hectares)",
#       Variable == "country_area" ~ "Area of the country (hectares)"
#     )
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source)
# 
# 
# 
# # Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics and Bastin_Zonal_Statistics have the same structure
# # => possible graph indicating corremation between tree coverage and carbon density
# 
# 
# 
# 
# # Indian restoration areas
# 
# Chaturvedi_ZonalStatistics <- read_csv(here("data/input/Chaturvedi _ZonalStatistics.csv")) %>%
#   mutate(
#     iso3 = "IND",
#     source = "Chaturvedietal_2018"
#   ) %>%
#   rename(
#     "Excluded_areas" = "0",
#     "Protection" = "1",
#     "Wide_scale_Restoration" = "2",
#     "Mosaic_Restoration" = "3"
#   ) %>%
#   summarise(
#     Excluded_areas = sum(Excluded_areas, na.rm = TRUE),
#     Protection = sum(Protection, na.rm = TRUE),
#     Wide_scale_Restoration = sum(Wide_scale_Restoration, na.rm = TRUE),
#     Mosaic_Restoration = sum(Mosaic_Restoration, na.rm = TRUE)
#   ) %>%
#   mutate(
#     iso3 = "IND",
#     unit = "ha",
#     source = "Chaturvedietal_2018"
#   ) %>%
#   pivot_longer(
#     cols = c(Excluded_areas, Protection, Wide_scale_Restoration, Mosaic_Restoration),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     description = case_when(
#       Variable == "Excluded_areas" ~ "Excluded areas",
#       Variable == "Protection" ~ "Protection",
#       Variable == "Wide_scale_Restoration" ~ "Wide-scale Restoration",
#       Variable == "Mosaic_Restoration" ~ "Mosaic Restoration"
#     )
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source)
# 
# 
# 
# # Carbon sequestration potential from reforestation in CO2e
# 
# Griscom_Zonal_Statistics <- read_csv(here("data/input/Griscom_Zonal_Statistics.csv")) %>%
#   group_by(iso3) %>%
#   summarise(
#     country_area = sum(area, na.rm = TRUE),
#     Reforestation_carbonseq_total = sum(`sum`, na.rm = TRUE),
#     Reforestation_carbonseq_density = sum(`sum`, na.rm = TRUE) / country_area
#   ) %>%
#   pivot_longer(
#     cols = c(Reforestation_carbonseq_density, Reforestation_carbonseq_total, country_area),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     unit = case_when(
#       Variable == "Reforestation_carbonseq_density" ~ "kgCO2e ha-1 yr-1",
#       Variable == "Reforestation_carbonseq_total" ~ "kgCO2e yr-1",
#       Variable == "country_area" ~ "ha"
#     ),
#     description = case_when(
#       Variable == "Reforestation_carbonseq_density" ~ "Density of carbon sequestration potential from reforestation",
#       Variable == "Reforestation_carbonseq_total" ~ "Total carbon sequestration potential from reforestation",
#       Variable == "country_area" ~ "Area of the country"
#     ),
#     source = "Griscometal_2017"
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source)
# 
# 
# 
# 
# 
# # Net Climate Impact of tree cover restoration (carbon storage and albedo change)
# 
# Hasler_Zonal_Statistics <- read_csv(here("data/input/Hasler_Zonal_Statistics.csv")) %>%
#   group_by(iso3) %>%
#   summarise(
#     country_area = sum(area, na.rm = TRUE),
#     ClimateImpact_treecoverrestoration_total = sum(`sum`, na.rm = TRUE),
#     ClimateImpact_treecoverrestoration_density = sum(`sum`, na.rm = TRUE) / country_area
#   ) %>%
#   pivot_longer(
#     cols = c(ClimateImpact_treecoverrestoration_total, ClimateImpact_treecoverrestoration_density, country_area),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     unit = case_when(
#       Variable == "ClimateImpact_treecoverrestoration_total" ~ "Mg CO2e",
#       Variable == "ClimateImpact_treecoverrestoration_density" ~ "Mg CO2e ha-1",
#       Variable == "country_area" ~ "ha"
#     ),
#     description = case_when(
#       Variable == "ClimateImpact_treecoverrestoration_total" ~ "Net Climate Impact of tree cover restoration (carbon storage and albedo change)",
#       Variable == "ClimateImpact_treecoverrestoration_density" ~ "Density of Net Climate Impact of tree cover restoration (carbon storage and albedo change) per hectare",
#       Variable == "country_area" ~ "Area of the country"
#     ),
#     source = "Hasleretal_2024"
#   ) %>%
#   select(iso3, Variable, Value, unit, source, description)
# 
# 
# 
# 
# # Constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon combined density under baseline climate
# 
# Walker_Zonal_Statistics <- read_csv(here("data/input/Walker_Zonal_Statistics.csv")) %>%
#   group_by(iso3) %>%
#   summarise(
#     country_area = sum(area, na.rm = TRUE),
#     Potential_carbonland_total = sum(`sum`, na.rm = TRUE),
#     Potential_carbonland_density = sum(`sum`, na.rm = TRUE) / country_area
#   ) %>%
#   pivot_longer(
#     cols = c(Potential_carbonland_total, Potential_carbonland_density, country_area),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     unit = case_when(
#       Variable == "Potential_carbonland_total" ~ "Mg C",
#       Variable == "Potential_carbonland_density" ~ "Mg C ha-1",
#       Variable == "country_area" ~ "ha"
#     ),
#     description = case_when(
#       Variable == "Potential_carbonland_total" ~ "Total constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon under baseline climate",
#       Variable == "Potential_carbonland_density" ~ "Density of constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon under baseline climate",
#       Variable == "country_area" ~ "Area of the country"
#     ),
#     source = "Walkeral_2022"
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source)
# 
# 
# 
# 
# # Potential for forest regeneration in tropical regions
# 
# Williams_Zonal_Statistics <- read_csv(here("data/input/Williams_Zonal_Statistics.csv")) %>%
#   group_by(iso3) %>%
#   summarise(
#     country_area = sum(area, na.rm = TRUE),
#     Potential_forestregeneration_total = sum(`sum`, na.rm = TRUE),
#     Potential_forestregeneration_percent = 100 * sum(`sum`, na.rm = TRUE) / country_area
#   ) %>%
#   pivot_longer(
#     cols = c(Potential_forestregeneration_total, Potential_forestregeneration_percent, country_area),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   mutate(
#     Value = ifelse(Variable == "Potential_forestregeneration_percent",
#                    round(Value, 2),
#                    Value),
#     unit = case_when(
#       Variable == "Potential_forestregeneration_total" ~ "ha",
#       Variable == "Potential_forestregeneration_percent" ~ "%",
#       Variable == "country_area" ~ "ha"
#     ),
#     description = case_when(
#       Variable == "Potential_forestregeneration_total" ~ "Total potential for forest regeneration in tropical regions",
#       Variable == "Potential_forestregeneration_percent" ~ "Share of country area with potential for forest regeneration (%)",
#       Variable == "country_area" ~ "Area of the country"
#     ),
#     source = "Williamsal_2024"
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source)
# 
# 
# 
# 
# 
# # Global map of forest landscape restoration opportunities (Wide or Mosaic Restoration)
# 
# Potapovetal <- read_excel(here("data/input/Potapovetal.xlsx")) %>%
#   pivot_longer(
#     cols = -iso3,
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   filter(grepl("1000 ha", Variable)) %>%
#   mutate(
#     Variable = gsub(" \\(1000 ha\\)", "", Variable),
#     Value = Value * 1000,
#     unit = "ha",
#     description = case_when(
#       Variable == "Wide-scale" ~ "Wide scale restoration",
#       Variable == "Mosaic" ~ "Mosaic restoration",
#       TRUE ~ Variable
#     ),
#     source = "Potapovetal"
#   )
# 
# 
# 
# 
# # 8 Afforestation/Reforestation: technical potential (annual and cumulative) and area + cost-effective potential (annual and cumulative) and area
# 
# # Read and pivot to long format
# RoeetAl <- read_excel(here("data/input/RoeetAl.xlsx")) %>%
#   pivot_longer(
#     cols = -iso3,
#     names_to = "Variable",
#     values_to = "Value"
#   )
# 
# # Create total area rows
# totals <- RoeetAl %>%
#   filter(Variable %in% c("ars_tech_ha", "ars_feas_ha")) %>%
#   mutate(
#     Variable = case_when(
#       Variable == "ars_tech_ha" ~ "ars_tech_ha_tot",
#       Variable == "ars_feas_ha" ~ "ars_feas_ha_tot"
#     ),
#     Value = Value * 35
#   )
# 
# # Combine original data with totals
# RoeetAl <- bind_rows(RoeetAl, totals) %>%
#   # Assign units and descriptions
#   mutate(
#     source = "Roeetal",
#     unit = case_when(
#       Variable %in% c("ars_tech", "ars_feas") ~ "MtCO2 yr-1",
#       Variable %in% c("ars_techcum", "ars_feascum", "refor_techcum", "refor_feascum") ~ "MtCO2",
#       Variable %in% c("ars_tech_ha", "ars_feas_ha") ~ "ha yr-1",
#       Variable %in% c("ars_tech_ha_tot", "ars_feas_ha_tot") ~ "ha (total over 35 yrs)",
#       TRUE ~ NA_character_
#     ),
#     description = case_when(
#       Variable == "ars_tech" ~ "Technical Potential annual for Afforestation and reforestation",
#       Variable == "ars_techcum" ~ "Technical Potential cumulative for Afforestation and reforestation",
#       Variable == "ars_tech_ha" ~ "Technical Area annual for Afforestation and reforestation",
#       Variable == "ars_feas" ~ "Cost-effective Potential annual for Afforestation and reforestation",
#       Variable == "ars_feascum" ~ "Cost-effective Potential cumulative for Afforestation and reforestation",
#       Variable == "ars_feas_ha" ~ "Cost-effective Area annual for Afforestation and reforestation",
#       Variable == "ars_tech_ha_tot" ~ "Technical Area total over 35 years for Afforestation and reforestation",
#       Variable == "ars_feas_ha_tot" ~ "Cost-effective Area total over 35 years for Afforestation and reforestation",
#       Variable == "refor_techcum" ~ "Technical Potential cumulative for reforestation",
#       Variable == "refor_feascum" ~ "Cost-effective Potential cumulative for reforestation",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   select(iso3, Variable, Value, unit, description, source) %>%
#   drop_na() %>%
#   distinct()
# 
# 
# Bastinetal_potential <- read_excel(here("data/input/Bastinetal_potential.xlsx"))
# Shyamsundaretal <- read_excel(here("data/input/Shyamsundaretal.xlsx"))
# 
# 
# 
# 
# 
# 
# restoration_db <- Bastin_Zonal_Statistics %>%
#   # rbind(Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics) %>%
#   rbind(Chaturvedi_ZonalStatistics) %>%
#   rbind(Griscom_Zonal_Statistics) %>%
#   rbind(Hasler_Zonal_Statistics) %>%
#   rbind(Walker_Zonal_Statistics) %>%
#   rbind(Williams_Zonal_Statistics) %>%
#   rbind(Potapovetal) %>%
#   rbind(RoeetAl) %>%
#   rbind(Bastinetal_potential) %>%
#   rbind(Shyamsundaretal) %>%
#   distinct(iso3, Variable, .keep_all = TRUE) %>%
#   drop_na()
# 
# write.xlsx(restoration_db,   file = here("data", "output", "restoration_db.xlsx"), row.names = FALSE)



restoration_db <- read_excel(here("data/output/restoration_db.xlsx"))



### IDEA OF ANALYSIS ###

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
    title = "India Restoration Potential"
  ) +
  scale_fill_manual(
    values = c(
      "Protection" = "#d95f02",    
      "Wide scale Restoration" = "#7570b3",
      "Mosaic Restoration" = "#e7298a",     
      "Tree cover restoration potential" = "#a6761d",
      "Forest regeneration potential" = "black"
    )
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
      Variable == "ars_tech_ha_tot" ~ "Afforestated/Reforestated (Technical)",
      Variable == "ars_feas_ha_tot" ~ "Afforestated/Reforestated (Cost-effective)"
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
      "Afforestated/Reforestated (Technical)" = "#1b9e77",
      "Afforestated/Reforestated (Cost-effective)" = "#f781bf"
    ),
    guide = guide_legend(ncol = 3, byrow = TRUE)
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
  "ars_feascum" = "Potential cumulative for afforestation and reforestation (Cost-effective)",
  "ars_techcum" = "Potential cumulative for afforestation and reforestation (Technical)"
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
  scale_x_discrete(labels = label_source) +  # show descriptive labels on x-axis
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
  filter(Variable %in% c("Restoration_Potential", "Potential_forestregeneration_total")) %>%
  group_by(Variable) %>% 
  mutate(Value  = sum(Value)) %>% 
  select(-iso3) %>% 
  mutate(iso3 = ("WRD")) %>% 
  select(iso3, Variable, Value) %>% 
  distinct () %>% 
  mutate(Value = Value / 1e6) 

label_source <- c(
  "Restoration_Potential" = "Shyamsundar et al. (2022)",
  "Potential_forestregeneration_total" = "Williams et al. (2024)"
)

variable_labels <- c(
  "Restoration_Potential" = "Tree cover restoration in forests, wetlands, and agricultural lands",
  "Potential_forestregeneration_total" = "Potential for forest regeneration in tropical regions"
)

TropicalForest_plot <- ggplot(TropicalForest_db, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.6) +   # two columns
  scale_x_discrete(labels = label_source) +  # x-axis shows authors
  scale_fill_manual(
    values = c(
      "Restoration_Potential" = "#7570b3",
      "Potential_forestregeneration_total" = "#d95f02"
    ),
    labels = variable_labels,  # legend shows variable descriptions
    guide = guide_legend(nrow = 2)
  ) +
  labs(
    x = "",
    y = "Mha",
    fill = "",  # legend title
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
