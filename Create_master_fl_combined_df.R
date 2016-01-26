#library(RCurl)
library(raster)
library(dplyr)
library(purrr)
#library(hadsstR)
library(beepr)

setwd('Meta_analysis_ms')

# Get spatial data (accurate lat longs for every site and where possible plots 
# contained within a site)
spatial_data <- read.csv('master_data/SiteSpatialData.csv', stringsAsFactors = FALSE)

# site_id can be used to join the spatial data with the 
spatial_data$site_id <- paste(spatial_data$Study.ID, spatial_data$Site, sep = "_")

# Add rows so that I can create the spatial lines dataframe
spatial_data$row <- seq_along(spatial_data$Study.ID)

sp_data_points <- filter(spatial_data, Shape == 'point')
sp_data_lines <- filter(spatial_data, Shape == 'line')


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

# Load data
fl_combined <- read.csv('Data_outputs/firstLastData_v0.9-20160115.csv')

fl_combined$id <- as.factor(1:length(fl_combined$Study.ID))

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
#                     Add temperature change to full data                      #
################################################################################
#sstData <- loadHadSST(directory="Data/", hadsstFilename="HadISST_sst.nc"); beep()

# Get climate change arrays for the period spanning our studies
#yr_min <- min(fl_combined$T1)
#yr_max <- max(fl_combined$T2)
#print(c(yr_min, yr_max))

#start <- Sys.time()
#cMats <- getClimateChange(sstData, years = yr_min:yr_max)
#total <- Sys.time() - start
#total
#beep()

# Get climate velocities at per decade rate.
#vel <- numeric(length(fl_combined2$id))
#for (i in seq_along(fl_combined2$id)) {
#    vel[i] <- getClimateLatLon(cMats, 
#                               lat = fl_combined2$Lat.y[i], 
#                               lon = fl_combined2$Long.y[i], 
#                               measure="velocity")
#}

#fl_combined2$vel_decade <- vel
#beep()


# Get average temperature change (per decade rate)
#lin_change <- numeric(length(fl_combined2$id))
#for (i in seq_along(fl_combined2$id)) {
#    lin_change[i] <- getClimateLatLon(cMats, 
#                                      lat = fl_combined2$Lat.y[i], 
#                                      lon = fl_combined2$Long.y[i], 
#                                      measure="linearChange")
#}

#fl_combined2$lin_change <- lin_change
#beep()


################################################################################
#              Add raster velocity and linear change to full data              #
################################################################################
source('../new_hadsstr/vocc_raster_functions.R')

yr_min <- min(fl_combined$T1)
yr_max <- max(fl_combined$T2)
print(c(yr_min, yr_max))


hadrast <- loadHadSST1('master_data/', hadsstFilename = "HadISST_sst.nc")
all_rasters <- getAllRasters(hadrast, years = yr_min:yr_max)
#velocity <- getVelocityMag_raster(hadsst_raster, years = yr_min:yr_max)
#linear <- getSSTLinChangeRaster(hadsst_raster, years = yr_min:yr_max)

velocity <- selectRaster(all_rasters, 'Velocity')
linear <- selectRaster(all_rasters, 'LinearChange')

beep()

######
# Extract velocity magnitudes
######
# Create spatial object
fl_combined_sp <- fl_combined
coordinates(fl_combined_sp) <- c('Long.y', 'Lat.y')
projection(fl_combined_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Get velocities and linear changes at each lat/lon
velocities <- extract(velocity, fl_combined_sp, small = T)
linears <- extract(linear, fl_combined_sp, small = T)

fl_combined_sp$raster_vel <- velocities
fl_combined_sp$raster_lin <- linears

fl_combined <- as.data.frame(fl_combined_sp)

# Retaining lat/lon values from spatial data.frame
# See SO answer here: http://gis.stackexchange.com/questions/142156/r-how-to-get-latitudes-and-longitudes-from-a-rasterlayer

fl_combined <- data.frame(fl_combined_sp@data, long = coordinates(fl_combined_sp))

# Clean up lat long column names

fl_combined <- rename(fl_combined, Long.y = long.Long.y, Lat.y = long.Lat.y)

################################################################################
# Saving data object with human impacts and climate velocity.
################################################################################
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0(outdate,".csv")
write.csv(fl_combined_sp, paste0("Data/full_data_with_impacts_and_velocity",trailer), row.names=F)

################################################################################
#                  Add additional impact values to full data                   #
################################################################################

fl_combined_sp <- fl_combined

# Set human impacts data directory
extra_impact_dir <- '/Users/jillian/R_projects/Human_Cumulative_Impacts/Data'

invasives <- raster(paste0(extra_impact_dir, '/invasives_raw/invasives.tif'))
fertilizers <- raster(paste0(extra_impact_dir, '/plumes_fertilizer_raw/plumes_fert.tif'))
pesticides <- raster(paste0(extra_impact_dir, '/plumes_pesticide_raw/plumes_pest.tif'))


coordinates(fl_combined_sp) <- c('Long.y', 'Lat.y')
projection(fl_combined_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Get mean impact value for each site (set 0.0 to NA - these points are land)
# FYI - this takes a long time :(

# Invasive potential #
# -----------------------------------------------------------------------
x <- as.array(invasives)

switch_NA_zero <- function(cell) {
  cell <- cell
  if (is.na(cell)) {
    cell <- 0
  } else if (cell == 0) {
    cell <- NA
  }
  return(cell)
} 

y <- apply(x, MARGIN = c(1, 2), FUN = switch_NA_zero)
beep()

z <- raster(x = y)

writeRaster((z, 'invs_zero_na_switched.tif', format = "GTiff"))


invs <- extract(invasives, fl_combined_sp, buffer = 1000, small = T)
beep()
mean_invs <- lapply(invs, FUN = mean, na.rm = TRUE)


# Fertilizer layer # 
# -----------------------------------------------------------------------
fert <- extract(fertilizers, fl_combined_sp, buffer = 1000, small = T)
beep()
mean_fert <- lapply(fert, FUN = mean, na.rm = TRUE)


# Pesticide layer # 
# -----------------------------------------------------------------------
pest <- extract(pesticides, fl_combined_sp, buffer = 1000, small = T)
beep()
mean_pest <- lapply(pest, FUN = mean, na.rm = TRUE)


# Raw data layer values for invaisves, fertilizers, and pesticides back into 
# master data.frame



fl_combined$mean_invs <- unlist(mean_invs)
fl_combined$mean_fert <- unlist(mean_fert)
fl_combined$mean_pest <- unlist(mean_pest)

################################################################################
# Saving data object with human impacts, raw impacts, and climate velocity.
################################################################################
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0(outdate,".csv")
write.csv(fl_combined, paste0("Data_outputs/full_data_with_impacts_velocity_invs_fert_pest",trailer), row.names=F)


################################################################################
# Additional study length climate velocity 
################################################################################
# Does it make sense for these to be averages?


fl_combined 
