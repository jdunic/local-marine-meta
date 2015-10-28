# Error recording script 

library(raster)
library(dplyr)
#library(hadsstR)
library(beepr)

library(googlesheets)
library(googleVis)


change_direction <- 
filter(data, Event. == 'Yes' & is.na(Expected.Change.Direction)) %>%
  select(Study.ID, Collector) %>% 
  mutate(problem = 'Need to include expected change direction in lookup table?')

event_col <- 
  filter(data, Event. != 'No' & Event. != 'Yes') %>%
  select(Study.ID, Collector) %>% 
  mutate(problem = 'Need to include yes or no in Event.')

zero_initial_rich <- 
  filter(data, SppR1 == 0) %>% 
  select(Study.ID, Collector) %>% 
  mutate(problem = 'Need to check if this zero SppR1 is real. If so, exclude in analysis?')

zero_final_rich <- 
  filter(data, SppR2 == 0) %>% 
  select(Study.ID, Collector) %>% 
  mutate(problem = 'Need to check if this zero SppR2 is real.')

### Missing spatial data (lat/lon) ###

# Load spatial data
master_data <- register_ss('Marine diversity master data ')
spatialData <- get_via_csv(ss = master_data, ws = 'SiteSpatialData')
Data <- get_via_csv(ss = master_data, ws = 'Data')

# Create unique study - site location lookups
Data$location_key <- paste(Data$Study.ID, Data$Site, sep = '_')
spatialData$location_key <- paste(spatialData$Study.ID, spatialData$Site, sep = '_')

# The coolest function ever! anti_join() !!!!!!!
missing_spatial_data <- 
anti_join(Data, spatialData, by = 'location_key') %>% 
  distinct(Study.ID) %>% 
  select(Study.ID, Collector, Lat, Long) %>%
  select(Study.ID, Collector) %>%
  mutate(problem = 'Need to enter spatial data into spatial data spreadsheet')


event_types <- register_ss("Event Types")
eventData <- get_via_csv(ss = event_types)

filter(Data, Event.type == 'No') %>%
  select(Study.ID, Collector) %>% 
  mutate(problem = 'There is no such thing as a "No" event type')

# Add additional event data
event_types <- register_ss("Event Types")
eventData <- get_via_csv(ss = event_types)


use crop to work on the map stuff
make panels of the under layers
- 95% confidence interval - overlapping zero (look at ipcc maps)
- generate a light gray mask that shows that the 95% confidence - 

- Ravana - ssh in - run R from the console, clone the repository - 


- screen - allows you to detach from a remote 


# Write google sheets into file

library(googlesheets)
library(googleVis)

master_data <- register_ss('Marine diversity master data ')

gs_ws_ls(master_data)

#gs_download(from = master_data, ws = 'Meta', to = 'master_data/Meta_data.csv')
#gs_download(from = master_data, ws = 'SiteSpatialMetaData', to = 'master_data/SiteSpatialMetaData.csv')
gs_download(from = master_data, ws = 'Data', to = 'master_data/Data.csv')
#gs_download(from = master_data, ws = 'SiteSpatialData', to = 'master_data/SiteSpatialData.csv')
#gs_download(from = master_data, ws = 'Papers2', to = 'master_data/Papers2.csv')
#gs_download(from = master_data, ws = 'Papers', to = 'master_data/Papers.csv')
#gs_download(from = master_data, ws = 'Missing Data', to = 'master_data/MissingData.csv')



library(ggmap)

-43.048281, 147.323644

map <- get_map(location = c(lon = 147.323644, lat = -43.048281), zoom = 13, maptype = 'satellite')
ggmap(map)

ggsave(filename = '/Users/jillian/Desktop/Meta-analysis_papers/_PK Papers/Barrett et al. 2007/tinderbox_map.png')