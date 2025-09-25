library(sf)     
library(dplyr)  
library(readr)    
library(here)
library(sf)
library(ggplot2)
library(geojsonsf)



shapes <- st_read(here("data","input", "COPERNICUS_LandCover_2019_Zonal_Statistics.geojson"))

data <- st_read(here("data","input", "COPERNICUS_LandCover_2019_Zonal_Statistics.csv"))


p1 <- ggplot(shapes) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal() +
  ggtitle("Country Boundaries")

