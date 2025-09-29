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


### Cleaning each data set and need to aggregate at country level ###

# Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics

Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics <- 
  read_csv(here("data/input/Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics.csv")) %>% 
  group_by(iso3) %>%
  summarise(
    country_area   = sum(area, na.rm = TRUE),
    Carbon_Density = sum(`sum` * area, na.rm = TRUE) / sum(area, na.rm = TRUE)
  ) %>%
  mutate(
    unit   = "Mg/ha",
    source = "Aboveground_and_Belowground_Biomass_Carbon_Density"
  ) %>%
  select(iso3, Carbon_Density, unit, country_area, source)

  


# Bastin_Zonal_Statistics => Global tree cover (existing and potential)
Bastin_Zonal_Statistics <- read_csv(here("data/input/Bastin_Zonal_Statistics.csv")) %>% 
  group_by(iso3) %>%
  summarise(
    country_area   = sum(area, na.rm = TRUE),
    Tree_cover = sum(`mean` * area, na.rm = TRUE) / sum(area, na.rm = TRUE)
  ) %>%
  mutate(
    unit="%",
    source="Bastinetal_2019"
  ) %>%
  select(iso3, Tree_cover, unit, country_area, source)


# Aboveground_and_Belowground_Biomass_Carbon_Density_Zonal_Statistics and Bastin_Zonal_Statistics have the same structure 
# => possible graph indicating corremation between tree coverage and carbon density




# Chaturvedi_ZonalStatistics => India resotration areas
Chaturvedi_ZonalStatistics <- read_csv(here("data/input/Chaturvedi _ZonalStatistics.csv")) %>% 
  mutate(
    iso3 = "IND",
    source="Chaturvedietal_2018"
    ) %>% 
  rename("Excluded areas"="0",
         "Protection" ="1",
         "Wide-scale Restoration" = "2",
         "Mosaic Restoration" ="3") %>% 
  mutate(
    `Excluded areas`= sum(`Excluded areas`, na.rm = TRUE),
    Protection= sum(Protection, na.rm = TRUE),
    `Wide-scale Restoration` = sum(`Wide-scale Restoration`, na.rm = TRUE),
    `Mosaic Restoration`= sum(`Mosaic Restoration`, na.rm = TRUE)
  ) %>% 
  mutate(unit = "ha") %>% 
  select(iso3, `Excluded areas`, Protection, `Wide-scale Restoration`, `Mosaic Restoration`, unit,  source) %>% 
  distinct()


# Griscom_Zonal_Statistics => Reforestation potential in CO2e

Griscom_Zonal_Statistics <- read_csv(here("data/input/Griscom_Zonal_Statistics.csv")) 
# NEED TO CHECK THE VALUES? THEY SEEM TOO HIGH

%>% 
  group_by(iso3) %>%
  summarise(
    country_area = sum(area, na.rm = TRUE),
    Reforestation_potential = sum(`sum` * area, na.rm = TRUE) / sum(area, na.rm = TRUE)
  ) %>%
  mutate(
    unit= "kgCO2e ha-1 yr-1",
    source = "Griscometal_2017"
  ) %>%
  select(iso3, Reforestation_potential, unit, country_area, source)




Hasler_Zonal_Statistics <- read_csv(here("data/input/Hasler_Zonal_Statistics.csv"))







