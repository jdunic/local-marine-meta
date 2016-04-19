library(dplyr)

fl <- read.csv('Data_outputs/firstLastData_v0.9-20160411.csv', stringsAsFactors = FALSE)

fl <- mutate(fl, site_id = paste(Study.ID, Site, sep = "_")) %>% 
        as_data_frame(.)

# ------------------------------------------------------------------------------
# Combine the extracted spatial data 
# ------------------------------------------------------------------------------
imps <- read.csv('Data_outputs/spatial_data_with_cumulative_impacts.csv', 
                 stringsAsFactors = FALSE)[, -1]
invs <- read.csv('Data_outputs/spatial_data_with_invasives.csv', 
                 stringsAsFactors = FALSE)[, -1]
nuts <- read.csv('Data_outputs/spatial_data_with_nutrients.csv', 
                 stringsAsFactors = FALSE)[, -1]
pest <- read.csv('Data_outputs/spatial_data_with_pesticides.csv', 
                 stringsAsFactors = FALSE)[, -1]
temp <- read.csv('Data_outputs/spatial_data_with_temp_data.csv', 
                 stringsAsFactors = FALSE)
single_temps <- read.csv('Data_outputs/spatial_data_with_single_temp_changes.csv', 
                          stringsAsFactors = FALSE)[, -1]

imps <- dplyr::select(imps, row, mean_imps) %>% 
          group_by()
invs <- dplyr::select(invs, row, mean_invs)
nuts <- dplyr::select(nuts, row, mean_nuts)
pest <- dplyr::select(pest, row, mean_pests)
temp <- dplyr::select(temp, row, mean_lin_change, mean_vocc)
sing_temp <- dplyr::select(single_temps, row, single_lin_change, single_vel)

spatial_data <- read.csv('master_data/SiteSpatialData.csv', stringsAsFactors = FALSE)
spatial_data <- 
  spatial_data %>% 
  mutate(Reference = trimws(.$Reference), 
         Site = trimws(.$Site), 
         Shape = trimws(.$Shape)) %>% 
  mutate(site_id = paste(.$Study.ID, .$Site, sep = "_"))

# Add rows so that I can create the spatial lines dataframe
spatial_data$row <- seq_along(spatial_data$Study.ID)

all_spatial_data <- 
left_join(spatial_data, imps, by = c('row' = 'row')) %>% 
  left_join(., invs, by = c('row' = 'row')) %>% 
  left_join(., nuts, by = c('row' = 'row')) %>% 
  left_join(., pest, by = c('row' = 'row')) %>% 
  left_join(., temp, by = c('row' = 'row')) %>% 
  left_join(., sing_temp, by = c('row' = 'row')) %>% 
  as_data_frame(.)

summarised_spatial <- 
  all_spatial_data %>% 
  dplyr::select(-Study.ID, -Reference, -Site, -Notes, -Source) %>% 
  full_join(., fl, by = c('site_id' = 'site_id')) %>%
  group_by(id) %>% 
  summarise(mean_imps = mean(mean_imps, na.rm = TRUE), 
            mean_invs = mean(mean_invs, na.rm = TRUE), 
            mean_nuts = mean(mean_nuts, na.rm = TRUE), 
            mean_pests = mean(mean_pests, na.rm = TRUE), 
            mean_lin_change = mean(mean_lin_change, na.rm = TRUE), 
            mean_vocc = mean(mean_vocc, na.rm = TRUE), 
            single_lin_change = mean(single_lin_change, na.rm = TRUE), 
            single_vel = mean(single_vel, na.rm = TRUE))

fl_combined <- 
  left_join(fl, summarised_spatial, by = c('id' = 'id'))

write.csv(fl_combined, 'Data_outputs/fl_combined.csv', row.names = FALSE)