library(rworldmap)
library(ggplot2)
library(gridExtra)
library(dplyr)

sites <- read.csv('master_data/SiteSpatialData.csv') %>% as_data_frame()

# Get world map from rworldmap
worldmap <- map_data(map = "world")
# For interest you can take a look at the data for the worldmap
str(worldmap)

# Plot entire world map with sites
sites <- filter(sites, !(Study.ID %in% c(172, 547, 581, 637)))

ggplot(data = worldmap, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group)) +
  theme_bw() +
  geom_point(data = sites, aes(x = Start_Long, y = Start_Lat), size = 1, colour = 'red', shape = 21) + 
  #geom_text(data = sites %>% distinct(Study.ID), aes(x = Start_Long - 20, y = Start_Lat, colour = Study.ID, label = unique(Study.ID)), size = 3) + 
  guides(colour=FALSE)
