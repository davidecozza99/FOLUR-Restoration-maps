
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


restoration_db <- read_excel(here("data/output/restoration_db.xlsx"))


### 1) Global restoration potential

Worldforest_db <- restoration_db %>% 
  filter(Variable %in% c("Wide scale", "Mosaic", "Tree_cover_potential",
                         "ars_tech_ha_tot", "ars_feas_ha_tot",
                         "Potential_forestregeneration_total", "Restoration_Potential",
                         "MaxRefPotential")) %>%
  group_by(Variable) %>% 
  mutate(Value  = sum(Value)) %>% 
  select(-iso3) %>% 
  mutate(iso3 = ("WRD")) %>% 
  select(iso3, Variable, Value) %>% 
  distinct () %>% 
  ungroup() 
# %>% 
  # mutate(Value = case_when(
  #   Variable == "ars_tech_ha_tot" ~ 
  #       Value - Value[Variable == "ars_feas_ha_tot"],
  #     TRUE ~ Value)) #need to put the same source on one column, differentiating technical and cost-effective


Worldforest_stacked <- Worldforest_db %>% 
  mutate(Value = Value / 1e6) %>%
  mutate(
    Group = case_when(
      Variable %in% c("Wide scale", "Mosaic", "Protection") ~ "Potapovetal",
      Variable == "Tree_cover_potential" ~ "Bastinetal_2019",
      Variable %in% c("ars_tech_ha_tot", "ars_feas_ha_tot") ~ "Roeetal",
      Variable %in% c("Potential_forestregeneration_total") ~ "Williametal",
      Variable %in% c("Restoration_Potential") ~ "Shyamsundaretal_2022",
      Variable == "MaxRefPotential" ~ "fesenmyeretal",
      TRUE ~ NA_character_
    ),
    Group_label = case_when(
      Group == "Potapovetal" ~ "Potapov et al. (2020)",
      Group == "Bastinetal_2019" ~ "Bastin et al. (2019)",
      Group == "Roeetal" ~ "Roe et al. (2021)",
      Group == "Williametal" ~ "Williams et al. (2024)",
      Group == "Shyamsundaretal_2022" ~ "Shyamsundar et al. (2022)",
      Group == "fesenmyeretal" ~ "Fesenmyer et al. (2025)",
      TRUE ~ Group
    ),
    Variable_label = case_when(
      Variable %in% c("Wide scale") ~ "Wide scale restoration",
      Variable %in% c("Mosaic") ~ "Mosaic restoration",
      Variable == "Protection" ~ "Protection restorationion",
      Variable == "Tree_cover_potential" ~ "Tree cover restoration potential",
      Variable == "ars_tech_ha_tot" ~ "Technical restoration potential (2015-2050)",
      Variable == "ars_feas_ha_tot" ~ "Cost-effective restoration potential (2015-2050)",
      Variable ==  "Restoration_Potential" ~ "Tree cover restoration potential",
      Variable ==  "MaxRefPotential" ~ "Tree cover restoration potential",
      Variable ==  "Potential_forestregeneration_total" ~ "Tree cover restoration potential"
    )
  ) %>% 
  mutate(
    Axis_group = case_when(
      Group_label == "Potapov et al. (2020)" ~ Group_label,  # all Potapov together
      Group_label == "Roe et al. (2021)" ~ Group_label,  # all Roe together
      TRUE ~ Group_label
    )
  ) %>% 
  mutate(
    Variable_label = factor(
      Variable_label,
      levels = c(
        "Tree cover restoration potential",
        "Mosaic restoration",
        "Wide scale restoration",
        "Cost-effective restoration potential (2015-2050)",
        "Technical restoration potential (2015-2050)"
      )
    )
  )


label_source <- c(
  "Wide scale" = "Potapov et al. (2020)",
  "Protection" = "Potapov et al. (2020)",
  "Mosaic" = "Potapov et al. (2020)",
  "Tree_cover_potential" = "Bastin et al. (2019)",
  "ars_tech_ha_tot" = "Roe et al. (2021)",
  "ars_feas_ha_tot" = "Roe et al. (2021)",
  "Restoration_Potential" = "Williams et al. (2024)",
  "Potential_forestregeneration_total" = "Shyamsundar et al. (2022)",
  "MaxRefPotential" = "Fesenmyer et al. (2025)"
  
)

World_plot <- ggplot(Worldforest_stacked, aes(x = Axis_group, y = Value, fill = Variable_label)) +
  geom_col(position = "stack") +
  labs(
    x = "",
    y = "Mha",
    fill = "",
    title = ""
  ) +
  scale_fill_manual(
    values = c(
      "Wide scale restoration" = "#7570b3",
      "Mosaic restoration" = "lightblue",
      "Tree cover restoration potential" = "#a6761d",
      "Technical restoration potential (2015-2050)" = "#1b9e77",
      "Cost-effective restoration potential (2015-2050)" = "#f781bf"
    ),
    guide = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 19),
    legend.position = "top",
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




### 2) India restoration potential

Indiaforest_db <- restoration_db %>% 
  filter(Variable %in% c("Excluded_areas", "Protection", "Wide_scale_Restoration", "Wide scale",
                         "Mosaic_Restoration", "Mosaic", "Tree_cover_potential", "Potential_forestregeneration_total",
                         "Restoration_Potential",
                         "ars_tech_ha_tot", "ars_feas_ha_tot")) %>%
  filter(iso3 %in% c("IND")) %>% 
  select(iso3, Variable, Value) %>% 
  distinct () %>% 
  ungroup()
# %>% 
#   mutate(Value = case_when(
#     Variable == "ars_tech_ha_tot" ~ 
#       Value - Value[Variable == "ars_feas_ha_tot"],
#     TRUE ~ Value)) #need to put the same source on one column, differentiating technical and cost-effective



fesenmyer_india <- read_csv(here("data/input/fesenmyer_india.csv")) %>% 
  mutate(iso3 = "IND", unit = "ha") %>% 
  rename(Value = d0) %>% 
  mutate(Variable = "MaxRefPotential") %>% 
  select(iso3, Variable, Value) %>% 
  group_by(iso3) %>% 
  mutate(Value = sum(Value)) %>% 
  distinct()




Indiaforest_stacked <- Indiaforest_db %>%
  rbind(fesenmyer_india) %>% 
  mutate(Group = case_when(
    Variable %in% c("Excluded_areas", "Protection", "Wide_scale_Restoration", "Mosaic_Restoration") ~ "Chaturvedietal_2018",
    Variable %in% c("Mosaic", "Wide scale") ~ "Potapovetal",
    Variable == "Tree_cover_potential" ~ "Bastinetal_2019",
    Variable == "Potential_forestregeneration_total" ~ "Williamsal_2024",
    Variable == "Restoration_Potential" ~ "Shyamsundaretal_2022",
    Variable == "MaxRefPotential" ~ "Fesenmyer",
    Variable %in% c("ars_tech_ha_tot", "ars_feas_ha_tot") ~ "Roeetal",
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
      Group == "Shyamsundaretal_2022" ~ "Shyamsundar et al. (2022)",
      Group == "Roeetal" ~ "Roe et al. (2021)",
      Group == "Fesenmyer" ~ "Fesenmyer et al. (2025)",
      TRUE ~ Group
    ),
    Variable_label = case_when(
      Variable %in% c("Wide scale", "Wide_scale_Restoration") ~ "Wide scale restoration",
      Variable %in% c("Mosaic", "Mosaic_Restoration") ~ "Mosaic restoration",
      Variable == "Protection" ~ "Protected areas",
      Variable == "Tree_cover_potential" ~ "Tree cover restoration potential",
      Variable ==  "Restoration_Potential" ~ "Tree cover restoration potential",
      Variable ==  "Potential_forestregeneration_total" ~ "Tree cover restoration potential",
      Variable ==  "MaxRefPotential" ~ "Tree cover restoration potential",
      Variable == "ars_tech_ha_tot" ~ "Technical restoration potential (2015-2050)",
      Variable == "ars_feas_ha_tot" ~ "Cost-effective restoration potential (2015-2050)",
      TRUE ~ gsub("_", " ", Variable)
    )
  ) %>% 
  filter(Variable_label != "Excluded areas") %>% 
  mutate(
    Variable_label = factor(
      Variable_label,
      levels = c(
        "Tree cover restoration potential",
        "Mosaic restoration",
        "Protected areas",
        "Wide scale restoration",
        "Cost-effective restoration potential (2015-2050)",
        "Technical restoration potential (2015-2050)"
      )
    )
  )



India_plot <- ggplot(Indiaforest_stacked, aes(x = Group_label, y = Value, fill = Variable_label)) +
  geom_col(position = "stack") + 
  labs(
    x = "",
    y = "Area (Mha)",
    fill = "",
    title = "",
  ) +
  scale_fill_manual(
    values = c(
      "Wide scale restoration" = "#7570b3",
      "Mosaic restoration" = "lightblue",
      "Protected areas" = "#FFD54F",
      "Tree cover restoration potential" = "#a6761d",
      "Technical restoration potential (2015-2050)" = "#1b9e77",
      "Cost-effective restoration potential (2015-2050)" = "#f781bf"
    ),
    guide = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 24),
    legend.position = "top",
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






### 3) Brazil restoration potential

Brazilforest_db <- restoration_db %>% 
  filter(Variable %in% c("Excluded_areas", "Protection", "Wide_scale_Restoration", "Wide scale",
                         "Mosaic_Restoration", "Mosaic", "Tree_cover_potential", "Potential_forestregeneration_total",
                         "Restoration_Potential",
                         "ars_tech_ha_tot", "ars_feas_ha_tot")) %>%
  filter(iso3 %in% c("BRA")) %>% 
  select(iso3, Variable, Value) %>% 
  distinct () %>% 
  ungroup()



fesenmyer_brazil <- read_csv(here("data/input/fesenmyer_brazil.csv")) %>% 
  mutate(iso3 = "BRA", unit = "ha") %>% 
  rename(Value = d0) %>% 
  mutate(Variable = "MaxRefPotential") %>% 
  select(iso3, Variable, Value) %>% 
  group_by(iso3) %>% 
  mutate(Value = sum(Value)) %>% 
  distinct()



Brazilforest_stacked <- Brazilforest_db %>%
  rbind(fesenmyer_brazil) %>% 
  mutate(Group = case_when(
    Variable %in% c("Mosaic", "Wide scale") ~ "Potapovetal",
    Variable == "Tree_cover_potential" ~ "Bastinetal_2019",
    Variable == "Potential_forestregeneration_total" ~ "Williamsal_2024",
    Variable == "Restoration_Potential" ~ "Shyamsundaretal_2022",
    Variable == "MaxRefPotential" ~ "Fesenmyer",
    Variable %in% c("ars_tech_ha_tot", "ars_feas_ha_tot") ~ "Roeetal",
    TRUE ~ NA_character_
  )) %>%
  mutate(Value = Value / 1e6) %>%
  arrange(Group) %>%
  mutate(
    Group_label = case_when(
      Group == "Potapovetal" ~ "Potapov et al. (2020)",
      Group == "Bastinetal_2019" ~ "Bastin et al. (2019)",
      Group == "Williamsal_2024" ~ "Williams et al. (2024)",
      Group == "Shyamsundaretal_2022" ~ "Shyamsundar et al. (2022)",
      Group == "Roeetal" ~ "Roe et al. (2021)",
      Group == "Fesenmyer" ~ "Fesenmyer et al. (2025)",
      TRUE ~ Group
    ),
    Variable_label = case_when(
      Variable %in% c("Wide scale", "Wide_scale_Restoration") ~ "Wide scale restoration",
      Variable %in% c("Mosaic", "Mosaic_Restoration") ~ "Mosaic restoration",
      Variable == "Protection" ~ "Protected areas",
      Variable == "Tree_cover_potential" ~ "Tree cover restoration potential",
      Variable ==  "Restoration_Potential" ~ "Tree cover restoration potential",
      Variable ==  "Potential_forestregeneration_total" ~ "Tree cover restoration potential",
      Variable ==  "MaxRefPotential" ~ "Tree cover restoration potential",
      Variable == "ars_tech_ha_tot" ~ "Technical restoration potential (2015-2050)",
      Variable == "ars_feas_ha_tot" ~ "Cost-effective restoration potential (2015-2050)",
      TRUE ~ gsub("_", " ", Variable)
    )
  ) %>% 
  filter(Variable_label != "Excluded areas") %>% 
  mutate(
    Variable_label = factor(
      Variable_label,
      levels = c(
        "Tree cover restoration potential",
        "Mosaic restoration",
        "Protected areas",
        "Wide scale restoration",
        "Cost-effective restoration potential (2015-2050)",
        "Technical restoration potential (2015-2050)"
      )
    )
  )



Brazil_plot <- ggplot(Brazilforest_stacked, aes(x = Group_label, y = Value, fill = Variable_label)) +
  geom_col(position = "stack") + 
  labs(
    x = "",
    y = "Area (Mha)",
    fill = "",
    title = "",
  ) +
  scale_fill_manual(
    values = c(
      "Wide scale restoration" = "#7570b3",
      "Mosaic restoration" = "lightblue",
      "Protected areas" = "#FFD54F",
      "Tree cover restoration potential" = "#a6761d",
      "Technical restoration potential (2015-2050)" = "#1b9e77",
      "Cost-effective restoration potential (2015-2050)" = "#f781bf"
    ),
    guide = guide_legend(ncol = 2, byrow = TRUE)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
    legend.title = element_text(size = 24),
    axis.text.x = element_text(size = 19),
    axis.title.y = element_text(size = 24),
    legend.position = "top",
    legend.box = "horizontal",
  )



# Save the plot
figure_directory <- here("output", "figures")
filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "Brazil_restoration"), ".png")
png(
  filename = here(figure_directory, filename),
  units = "in", height = 12, width = 20, res = 300
)
print(Brazil_plot)
dev.off()





# 
# 
# ### 3) Comparison 4 and 5 (World)
# 
# # 4 Net Climate Impact of tree cover restoration (carbon storage and albedo change)  (Mg CO2e)
# # 5 Constrained unrealized potential aboveground biomass, belowground biomass, and soil organic carbon combined density under baseline climate (Mg C, stock)
# 
# 
# carbon_db <- restoration_db %>% 
#   filter(Variable %in% c("ClimateImpact_treecoverrestoration_total", "Potential_carbonland_total")) %>%
#   group_by(Variable) %>% 
#   summarise(Value = sum(Value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(Value = ifelse(Variable == "ClimateImpact_treecoverrestoration_total", Value / 3.664, Value)) %>%
#   mutate(iso3 = "WRD") %>% 
#   mutate(
#     Group = case_when(
#       Variable == "ClimateImpact_treecoverrestoration_total" ~ "Hasleretal_2024",
#       Variable == "Potential_carbonland_total" ~ "Walkeral_2022",
#       TRUE ~ NA_character_
#     ),
#     Group_label = case_when(
#       Group == "Hasleretal_2024" ~ "Hasler et al (2024)",
#       Group == "Walkeral_2022" ~ "Walker et al (2022)",
#       TRUE ~ Group
#     )
#   )
# 
# 
# 
# label <- c(
#   "ClimateImpact_treecoverrestoration_total" = "Net Climate Impact of tree cover restoration (carbon storage and albedo change)",
#   "Potential_carbonland_total" = "Total constrained unrealized potential aboveground and belowground biomass, and soil organic carbon under baseline climate"
# )
# 
# 
# 
# carbon_plot <- ggplot(carbon_db, aes(x = Group_label, y = Value / 1e9, fill = Variable)) +
#   geom_col(position = "stack") +
#   scale_fill_manual(values = c(
#     "ClimateImpact_treecoverrestoration_total" = "#d95f02",
#     "Potential_carbonland_total" = "#7570b3"
#   ),
#   labels = label,
#   guide = guide_legend(nrow = 2)
#   ) +
#   labs(
#     x = "",
#     y = "Pg C",
#     fill = "",
#     title = "World Carbon Storage"
#   ) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
#     legend.title = element_text(size = 24),
#     legend.text = element_text(size = 26),
#     axis.title.x = element_text(size = 30),
#     axis.title.y = element_text(size = 30),
#     legend.position = "bottom",
#     legend.box = "horizontal"
#   )
# 
# 
# # Save the plot
# figure_directory <- here("output", "figures")
# filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "World_carbon"), ".png")
# png(
#   filename = here(figure_directory, filename),
#   units = "in", height = 12, width = 24, res = 300
# )
# print(carbon_plot)
# dev.off()
# 
# ### 4) Comparison 4 and 8 (World)
# 
# 
# affor_refo_db <- restoration_db %>% 
#   filter(Variable %in% c("ClimateImpact_treecoverrestoration_total", "ars_feascum", "ars_techcum")) %>%
#   group_by(Variable) %>% 
#   summarise(Value = sum(Value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(iso3 = "WRD") %>% 
#   mutate(
#     Value = case_when(
#       Variable == "ClimateImpact_treecoverrestoration_total" ~ Value / 1e9,  # Mg => Gt
#       Variable %in% c("ars_feascum", "ars_techcum") ~ Value / 1e3,                 
#       TRUE ~ Value
#     ),
#     Group = case_when(
#       Variable == "ClimateImpact_treecoverrestoration_total" ~ "Hasleretal_2024",
#       Variable %in% c("ars_feascum", "ars_techcum") ~ "Roeetal",
#       TRUE ~ NA_character_
#     ),
#     Group_label = case_when(
#       Group == "Hasleretal_2024" ~ "Hasler et al (2024)",
#       Group == "Roeetal" ~ "Roe et al (2021)",
#       TRUE ~ Group
#     )
#   )
# 
# 
# label <- c(
#   "ClimateImpact_treecoverrestoration_total" = "Net Climate Impact of tree cover restoration (carbon storage and albedo change)",
#   "ars_feascum" = "Potential cumulative Reforestation (tropics) (Cost-effective)",
#   "ars_techcum" = "Potential cumulative Reforestation (tropics) (Technical)"
# )
# 
# 
# label_source <- c(
#   "ClimateImpact_treecoverrestoration_total" = "Hasler et al (2024)",
#   "ars_feascum" = "Roe et al (2021)",
#   "ars_techcum" = "Roe et al (2021)"
# )
# 
# affor_refo_plot <- ggplot(affor_refo_db, aes(x = Variable, y = Value, fill = Variable)) +
#   geom_col(width = 0.6) +  # each variable is its own column
#   scale_fill_manual(
#     values = c(
#       "ClimateImpact_treecoverrestoration_total" = "#1b9e77",
#       "ars_feascum" = "#7570b3",
#       "ars_techcum" = "#e7298a"
#     ),
#     labels = label,
#     guide = guide_legend(nrow = 3)
#   ) +
#   scale_x_discrete(labels = label_source) +
#   labs(
#     x = "",
#     y = "Gt CO2",
#     fill = "",
#     title = "Global Potential and Net Climate Impact of Afforestation and Reforestation"
#   ) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
#     legend.title = element_text(size = 24),
#     legend.text = element_text(size = 20),
#     axis.title.x = element_text(size = 24),
#     axis.title.y = element_text(size = 24),
#     legend.position = "bottom",
#     legend.box = "horizontal",
#   )
# 
# 
# # # Save the plot
# # figure_directory <- here("output", "figures")
# # filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "World_climate"), ".png")
# # png(
# #   filename = here(figure_directory, filename),
# #   units = "in", height = 12, width = 20, res = 300
# # )
# # print(affor_refo_plot)
# # dev.off()
# 
# 
# 
# 
# 
# 
# 
# TropicalForest_db <- restoration_db %>% 
#   filter(Variable %in% c("Restoration_Potential", "Potential_forestregeneration_total", "refor_tech_ha_tot", "refor_feas_ha_tot")) %>%
#   group_by(Variable) %>% 
#   mutate(Value  = sum(Value)) %>% 
#   select(-iso3) %>% 
#   mutate(iso3 = ("WRD")) %>% 
#   select(iso3, Variable, Value) %>% 
#   distinct () %>% 
#   mutate(Value = Value / 1e6)
# 
# label_source <- c(
#   "Restoration_Potential" = "Shyamsundar et al. (2022)",
#   "Potential_forestregeneration_total" = "Williams et al. (2024)",
#   "refor_tech_ha_tot" = "Roe et  al. (2020)" ,
#   "refor_feas_ha_tot" = "Roe et  al. (2020)"
# )
# 
# variable_labels <- c(
#   "Restoration_Potential" = "Tree cover restoration in forests, wetlands, and agricultural lands",
#   "Potential_forestregeneration_total" = "Potential for forest regeneration",
#   "refor_tech_ha_tot" = "Total technical reforestation potential (2020-2050)",
#   "refor_feas_ha_tot" = "Total cost-effective reforestation potential (2020-2050)"
# )
# 
# TropicalForest_plot <- ggplot(TropicalForest_db, aes(x = Variable, y = Value, fill = Variable)) +
#   geom_col(width = 0.6) +
#   scale_x_discrete(labels = label_source) +
#   scale_fill_manual(
#     values = c(
#       "Restoration_Potential" = "#1b9e77",
#       "Potential_forestregeneration_total" = "#d95f02",
#       "refor_tech_ha_tot" = "#7570b3",
#       "refor_feas_ha_tot" = "#e7298a"
#     ),
#     labels = variable_labels,
#     guide = guide_legend(nrow = 4)
#   ) +
#   labs(
#     x = "",
#     y = "Mha",
#     fill = "",
#     title = "Forest regeneration potential in tropical regions"
#   ) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "sans", color = "black", size = 30, face = "bold"),
#     legend.title = element_text(size = 24),
#     legend.text = element_text(size = 26),
#     axis.title.x = element_text(size = 30),
#     axis.title.y = element_text(size = 30),
#     legend.position = "bottom",
#     legend.box = "horizontal"
#   )
# 
# 
# 
# # # Save the plot
# figure_directory <- here("output", "figures")
# filename <- paste0(format(Sys.Date(),format = "%y%m%d"), "_", gsub(" ", "_", "Tropic_forest"), ".png")
# png(
#   filename = here(figure_directory, filename),
#   units = "in", height = 12, width = 20, res = 300
# )
# print(TropicalForest_plot)
# dev.off()
# 
# 
