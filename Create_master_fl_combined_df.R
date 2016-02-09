rm(list = setdiff(ls(), lsf.str()))
pkgs = names(sessionInfo()$otherPkgs)
sapply(pkgs, detach_package, character.only = TRUE)

library(raster)
library(dplyr)
library(purrr)
library(beepr)
devtools::install_github("jdunic/hadsstr")
library(hadsstr)

source('02_functions.R')

if (getwd() != '/Users/jillian/R_projects/Meta_analysis_ms') setwd('Meta_analysis_ms')

# Get spatial data (accurate lat longs for every site and where possible plots 
# contained within a site)
spatial_data <- read.csv('master_data/SiteSpatialData.csv', stringsAsFactors = FALSE)

spatial_data <- 
  spatial_data %>% 
  mutate(Reference = trimws(.$Reference), 
         Site = trimws(.$Site), 
         Shape = trimws(.$Shape)) %>% 
  mutate(site_id = paste(.$Study.ID, .$Site, sep = "_"))

# Add rows so that I can create the spatial lines dataframe
spatial_data$row <- seq_along(spatial_data$Study.ID)

sp_data_points <- filter(spatial_data, Shape == 'point')
sp_data_lines <- filter(spatial_data, Shape == 'line')


# Load and add site_id column to first last data to be able to combine the 
# spatial data with the master first last sheet. Also used for doing the 
# year-specific linear temperature change and climate velocity extractions.
fl <- read.csv('Data_outputs/firstLastData_v0.9-20160209.csv', stringsAsFactors = FALSE)

fl <- mutate(fl, site_id = paste(.$Study.ID, .$Site, sep = "_"))

# Create spatial objects
# ------------------------------------------------------------------------------
# Coerce points to spatial points object with explicit projection
coordinates(sp_data_points) <- ~Start_Long + Start_Lat
projection(sp_data_points) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Create spatial lines object for extract
# Create a list of the points that make up each line (just a start and end 
# point in this case)
lines_list <- 
  sp_data_lines %>% 
    split(.$row) %>%
    map(function(x) {
      start <- x[, c('Study.ID', 'Reference', 'Site', 'Start_Lat', 'Start_Long')]
      end <- x[, c('Study.ID', 'Reference', 'Site', 'End_Lat', 'End_Long')]
      names(start) <- names(end) <- c('Study.ID', 'Reference', 'Site', 'Lat', 'Lon')
      out <- rbind(start, end)
      coordinates(out) <- ~Lon + Lat
      projection(out) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      out <- Line(out)
      out <- Lines(out, ID = x$row)
      return(out)
      })

# Required for extract function input
spatial_lines_obj <- SpatialLines(lines_list)

# ------------------------------------------------------------------------------
# Get cumulative human impact values for all spatial data points
# ------------------------------------------------------------------------------
# Load human impacts map
impact_dir <- '/Users/jillian/R_projects/Human_Cumulative_Impacts/Data/CI_2013_OneTimePeriod'
imp_map <- raster(paste0(impact_dir, '/global_cumul_impact_2013_all_layers.tif'))


point_imps <- extract(imp_map, sp_data_points, buffer = 1000)
# Clean up the list of lists of impact values for both point imps and line imps
# get_mean_imp replaces zero values with NA because zero is land.
mean_point_imps <- unlist(lapply(point_imps, FUN = get_mean_imp))


line_imps <- extract(imp_map, spatial_lines_obj, along = TRUE)
mean_line_imps <- unlist(lapply(line_imps, FUN = get_mean_imp))


sp_data_points2 <- filter(spatial_data, Shape == 'point')
sp_data_lines2  <- filter(spatial_data, Shape == 'line')

sp_data_points2$mean_imps <- mean_point_imps
sp_data_lines2$mean_imps  <- mean_line_imps

combined_data <- rbind(sp_data_points2, sp_data_lines2)

# Save the data so I don't have to run all of this again + waste more time
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0(outdate,".csv")
write.csv(combined_data, 'Data_outputs/spatial_data_with_cumulative_impacts.csv')

# ------------------------------------------------------------------------------
# Get addtional impact values 
# ------------------------------------------------------------------------------
# Set human impacts data directory
extra_impact_dir <- '/Users/jillian/R_projects/Human_Cumulative_Impacts/Data'

invasives <- raster(paste0(extra_impact_dir, '/invasives_raw/invasives.tif'))

# Invasive potential #
# ----------------------------------------------------------------------- 
# Switch NA and zero values to be consistent with the other data layers and to 
# allow NA values to be omitted.

switch_NA_zero <- function(x) {
  x[is.na(x)] <- -99999
  x[x == 0] <- NA
  x[x == -99999] <- 0
  return(x)
}

point_invs <- extract(invasives, sp_data_points, buffer = 1000)
line_invs <- extract(invasives, spatial_lines_obj, along = TRUE)

point_invs_NA_switched <- lapply(point_invs, FUN = switch_NA_zero)
line_invs_NA_switched <- lapply(line_invs, FUN = switch_NA_zero)

mean_point_invs <- lapply(point_invs, FUN = mean, na.rm = TRUE)
mean_line_invs  <- lapply(line_invs, FUN = mean, na.rm = TRUE)

sp_data_points2 <- filter(spatial_data, Shape == 'point')
sp_data_lines2  <- filter(spatial_data, Shape == 'line')

sp_data_points2$mean_invs <- unlist(mean_point_invs)
sp_data_lines2$mean_invs  <- unlist(mean_line_invs)

invs_combined_data <- rbind(sp_data_points2, sp_data_lines2)

# Save the data so I don't have to run all of this again + waste more time
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0(outdate,".csv")
write.csv(invs_combined_data, 'Data_outputs/spatial_data_with_invasives.csv')



# Nutrients/Fertilizer layer # 
# -----------------------------------------------------------------------
nuts <- raster(paste0(extra_impact_dir, '/plumes_fertilizer_raw/plumes_fert.tif'))

point_nuts <- extract(nuts, sp_data_points, buffer = 1000)
mean_point_nuts <- lapply(point_nuts, FUN = mean, na.rm = TRUE)
line_nuts <- extract(nuts, spatial_lines_obj, along = TRUE)
mean_line_nuts <- lapply(line_nuts, FUN = mean, na.rm = TRUE)

sp_data_points2 <- filter(spatial_data, Shape == 'point')
sp_data_lines2  <- filter(spatial_data, Shape == 'line')

sp_data_points2$mean_nuts <- unlist(mean_point_nuts)
sp_data_lines2$mean_nuts  <- unlist(mean_line_nuts)

nuts_combined_data <- rbind(sp_data_points2, sp_data_lines2)

# Save the data so I don't have to run all of this again + waste more time
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0(outdate,".csv")
write.csv(nuts_combined_data, 'Data_outputs/spatial_data_with_nutrients.csv')


# Pesticide layer # 
# -----------------------------------------------------------------------
pesticides <- raster(paste0(extra_impact_dir, '/plumes_pesticide_raw/plumes_pest.tif'))

point_pests <- extract(pesticides, sp_data_points, buffer = 1000)
mean_point_pests <- lapply(point_pests, FUN = mean, na.rm = TRUE)
line_pests <- extract(pesticides, spatial_lines_obj, along = TRUE)
mean_line_pests <- lapply(line_pests, FUN = mean, na.rm = TRUE)

sp_data_points2 <- filter(spatial_data, Shape == 'point')
sp_data_lines2  <- filter(spatial_data, Shape == 'line')

sp_data_points2$mean_pests <- unlist(mean_point_pests)
sp_data_lines2$mean_pests  <- unlist(mean_line_pests)

pest_combined_data <- rbind(sp_data_points2, sp_data_lines2)

# Save the data so I don't have to run all of this again + waste more time
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0(outdate,".csv")
write.csv(pest_combined_data, 'Data_outputs/spatial_data_with_pesticides.csv')

################################################################################
#              Add raster velocity and linear change to full data              #
################################################################################

# Get single velocity and linear temp change raster for the total duration of 
# all of the studies together
yr_min <- min(fl$T1)
yr_max <- max(fl$T2)
print(c(yr_min, yr_max))

hadrast <- load_hadsst(file = 'master_data/HadISST_sst.nc')

all_rasters <- get_all_rasters(hadrast, years = yr_min:yr_max)

velocity <- select_raster(all_rasters, 'velocity_magnitude')
linear <- select_raster(all_rasters, 'linear_change')

beep()

# Get a velocity and linear temp change raster for each unique duration in the 
# dataset

durations <- unique(fl[, c('T1', 'T2')])
duration_text <- paste(durations$T1, durations$T2, sep = '_')

# Create a list of raster stacks that have the time spans as the z values so 
# that I can link them back up to the studies in the first last data
rast_set <- list()
for (i in seq_along(durations[[1]])) {
    all_rasters <- get_all_rasters(hadrast, years = durations$T1[i]:durations$T2[i])
    rast_stack <- stack(all_rasters)
    duration_text <- paste(durations$T1[i], durations$T2[i], sep = '_')
    # Remember that there are 5 data layers in each hadsst raster brick
    rast_set[[i]] <- setZ(rast_stack, rep(duration_text, each = 5), name = 'duration')
    names(rast_set[i]) <- duration_text
    # A counter so that I can have some idea of how far along things are
    print(i)
}

duration_text <- paste(durations$T1, durations$T2, sep = '_')
names(rast_set) <- duration_text

# Do the spatial points lookups and extraction for linear temperature change and 
# climate velocities. 

# Get a simplified data frame that has the necessary data needed to lookup 
# across tables. 
sp_data_points_lookup <- filter(spatial_data, Shape == 'point') %>% 
  select(site_id, Start_Lat, Start_Long, row) %>% 
  left_join(x = ., y = fl, by = c('site_id' = 'site_id')) %>% 
  mutate(timespans = paste(.$T1, .$T2, sep = "_")) %>%
  select(site_id, Study.ID, Site, Start_Lat, Start_Long, row, timespans) %>%
  distinct(row) %>%
  as_data_frame(.)

# Split this into a list so that I can work on it piece by piece to do the 
# cross referencing and select the correct raster dates
sp_data_points_list <- split(sp_data_points_lookup, seq(nrow(sp_data_points_lookup)))

linear_change_point_vals <- list()
vocc_point_vals <- list()

for (i in seq_along(sp_data_points_list)) {
    #browser()
#for (i in seq_along(sp_data_points_list)) {
  # raster::coordinates gets mad when you pass a tbl_df object
  sp_data_points_list[[i]] <- as.data.frame(sp_data_points_list[[i]], stringsAsFactors = FALSE)
#
  time_span_lookup <- sp_data_points_list[[i]]$timespans
#
  lin_change_rast <- raster::subset(rast_set[[time_span_lookup]], 'linear_change')
  vocc_rast <- raster::subset(rast_set[[time_span_lookup]], 'velocity_magnitude')
#
  coordinates(sp_data_points_list[[i]]) <- ~Start_Long + Start_Lat
  projection(sp_data_points_list[[i]]) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#
  linear_change_point_vals[[i]] <- extract(lin_change_rast, sp_data_points_list[[i]], buffer = 1000)
  vocc_point_vals[[i]] <- extract(vocc_rast, sp_data_points_list[[i]], buffer = 1000)
  names(linear_change_point_vals[[i]]) <- sp_data_points_list[[i]]$row
  names(vocc_point_vals[[i]]) <- sp_data_points_list[[i]]$row
}

beep()

# Create a simplified lookup for the spatial line data that joins the spatial 
# data and first last data so that I can get the right raster dates.
sp_data_lines <- 
  filter(spatial_data, Shape == 'line') %>% 
  select(-Study.ID, -Site) %>% 
  left_join(x = ., y = mutate(fl, timespans = paste(T1, T2, sep = "_")), by = c('site_id' = 'site_id')) %>%
  select(site_id, Study.ID, Site, Start_Lat, Start_Long, End_Lat, End_Long, row, timespans) %>%
    as_data_frame(.)

linear_change_line_vals <- list()
vocc_line_vals <- list()

# Fortunately the spatial lines object is already in list format and I named 
# each element as the row in the spatial data sheet so that we can lookup using 
# the row number and follow the relationship to the time span through 
# sp_data_lines.
for (i in seq_along(spatial_lines_obj)) {
  time_span_lookup <- 
  filter(sp_data_lines, row == names(spatial_lines_obj[i])) %>% 
  select(timespans) %>% 
  .[[1]]
#  
  lin_change_rast <- raster::subset(rast_set[[time_span_lookup]], 'linear_change')
  vocc_rast <- raster::subset(rast_set[[time_span_lookup]], 'velocity_magnitude')
#
  linear_change_line_vals[[i]] <- extract(lin_change_rast, spatial_lines_obj[i], along = TRUE)
  vocc_line_vals[[i]] <- extract(vocc_rast, spatial_lines_obj[i], along = TRUE)
  names(linear_change_line_vals[[i]]) <- names(spatial_lines_obj[i])
  names(vocc_line_vals[[i]]) <- names(spatial_lines_obj[i])
}


mean_lin_change <-
  lapply(c(linear_change_point_vals, linear_change_line_vals), function(x) {
    mean_lin <- mean(x[[1]], na.rm = TRUE)
    names(mean_lin) <- names(x)
    return(mean_lin)
})

mean_vocc <-
  lapply(c(vocc_point_vals, vocc_line_vals), function(x) {
    mean_vocc <- mean(x[[1]], na.rm = TRUE)
    names(mean_vocc) <- names(x)
    return(mean_vocc)
})

# The row orders are the same for linear change and vocc
# all.equal(lapply(mean_lin_change, names), lapply(mean_lin_change, names))
temp_data <- 
  data_frame(row = as.numeric(unlist(lapply(mean_lin_change, names))), 
             mean_lin_change = as.vector(unlist(mean_lin_change)), 
             mean_vocc = as.vector(unlist(mean_vocc)))

temp_data_combined <- 
  left_join(x = spatial_data, y = temp_data, by = c('row' = 'row'))

# Save the data so I don't have to run all of this again + waste more time
write.csv(temp_data_combined, "Data_outputs/spatial_data_with_temp_data.csv", row.names = FALSE)

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

imps <- dplyr::select(imps, row, mean_imps)
invs <- dplyr::select(invs, row, mean_invs)
nuts <- dplyr::select(nuts, row, mean_nuts)
pest <- dplyr::select(pest, row, mean_pests)
temp <- dplyr::select(temp, row, mean_lin_change, mean_vocc)

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
  as_data_frame(.)

summarised_spatial <- 
  full_join(all_spatial_data, fl, by = c('site_id' = 'site_id')) %>%
  group_by(id) %>% 
  summarise(mean_imps = mean(mean_imps, na.rm = TRUE), 
            mean_invs = mean(mean_invs, na.rm = TRUE), 
            mean_nuts = mean(mean_nuts, na.rm = TRUE), 
            mean_pests = mean(mean_pests, na.rm = TRUE), 
            mean_lin_change = mean(mean_lin_change, na.rm = TRUE), 
            mean_vocc = mean(mean_vocc, na.rm = TRUE))

fl_combined <- 
  left_join(fl, summarised_spatial, by = c('id' = 'id'))

write.csv(fl_combined, 'Data_outputs/fl_combined.csv', row.names = FALSE)