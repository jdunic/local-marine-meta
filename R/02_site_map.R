library(raster)
library(dplyr)
library(ggplot2)
library(readr)

source('00_driver_extraction_functions.R')

sp_data <- read_sp_data('../master_data/SiteSpatialData.csv') %>% 
  mutate(study_site = paste(.$Study.ID, .$Site, sep = '_'))

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga') %>% 
  # Keller study 
  filter(Study.ID != '172') %>%
  # Study  136 - Enfermeria should have been classified as having an event - 
  # 'shrimp farming' and 'tidal restriction'
  filter(Site != 'Enfermeria')

vw_subset <- filter(fl_combined, Event != 'Yes') %>% 
  filter(!is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)) %>% 
  mutate(study_site = paste(.$Study.ID, .$Site, sep = '_'))

vw_spatial <- filter(sp_data, study_site %in% vw_subset$study_site)

## @knitr site-map
ggplot(data = vw_spatial, aes(x = Start_Long, y = Start_Lat)) + 
  theme_void() + 
  borders('world', colour = NA, fill = 'darkgrey', alpha = 0.5) + 
  geom_point(mapping = aes(colour = factor(study_site)), size = 2, alpha = 0.5) + 
  theme(legend.position = 'none') + 
  xlab("Longitude") +
  ylab("Latitude")

## @knitr site-map-effect size
#ggplot(data = vw_spatial, aes(x = Start_Long, y = Start_Lat)) + 
#  theme_void() + 
#  borders('world', colour = NA, fill = 'darkgrey', alpha = 0.5) + 
#  geom_point(mapping = aes(fill = yi_SppR_ROM), colour = 'grey60', shape = 21, size = 2) + 
#  theme(legend.position = 'none') + 
#  scale_fill_gradient2(low = 'blue', high = 'red') + 
#  xlab("Longitude") +
#  ylab("Latitude")

## @knitr cumulative-impacts-with-sites-map-2008
imp_map_b1 <- raster("../../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 1)
imp_map_b2 <- raster("../../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 2)
imp_map_b3 <- raster("../../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 3)
imp_stack <- stack(imp_map_b1, imp_map_b2, imp_map_b3)

plotRGB(imp_stack, colNA = 'black')
points(x = vw_spatial$Start_Long, vw_spatial$Start_Lat, pch = 19, col = 'blue')
points(x = vw_spatial$Start_Long, vw_spatial$Start_Lat, pch = 19, col = 'black', cex = 0.6)