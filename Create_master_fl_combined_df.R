#library(RCurl)
library(raster)
library(dplyr)
#library(hadsstR)
library(beepr)

library(googlesheets)
library(googleVis)

# Load data
fl_combined <- read.csv('Data_outputs/firstLastData_v0.4-20151101.csv')

fl_combined$id <- as.factor(1:length(fl_combined$Study.ID))

################################################################################
#               Add cumulative human impact values to full data                #
################################################################################

# Load human impacts map
impact_dir <- '/Users/jillian/R_projects/Human_Cumulative_Impacts/Data/CI_2013_OneTimePeriod'
imp_map <- raster(paste0(impact_dir, '/global_cumul_impact_2013_all_layers.tif'))

#plot(imp_map)

# Get spatial data (accurate lat longs for every site and where possible plots 
# contained within a site)
spatial_data <- read_csv('master_data/SiteSpatialData.csv')
spatial_data$'[EMPTY]' <- NULL

# Create the spatial dataframe with specific site data
fl_combined$location_key <- paste(fl_combined$Study.ID, fl_combined$Site, sep = '_')
spatial_data$location_key <- paste(spatial_data$Study.ID, spatial_data$Site, sep = '_')

fl_combined <- merge(fl_combined, spatial_data, by = 'location_key', all.x = TRUE)

# Clean up x's in colnames after the merge
names(fl_combined) <- sub("\\.x$", '', names(fl_combined))


# Work around for dirty lat longs while we're still missing that data
fl_combined$Lat.y[is.na(fl_combined$Lat.y)] <- fl_combined$Lat[is.na(fl_combined$Lat.y)]
fl_combined$Long.y[is.na(fl_combined$Long.y)] <- fl_combined$Long[is.na(fl_combined$Long.y)]

# Get studies that don't have matching location_key.
setdiff(spatial_data$location_key, fl_combined$location_key)


# Ignore rows that do not have an associated lat-long for now
# These need to be fixed.
fl_combined <- filter(fl_combined, !is.na(Lat.y) | !is.na(Long.y))


######
# Finally go on to get the cumulative impact values
######

# Temporary fix because it looks like some studies still don't have lat longs
fl_combined_sp <- fl_combined
coordinates(fl_combined_sp) <- c('Long.y', 'Lat.y')
projection(fl_combined_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Get mean impact value for each site (set 0.0 to NA - these points are land)
# FYI - this takes a long time :(
imps <- extract(imp_map, fl_combined_sp, buffer = 1000, small = T)
beep()

# get_mean_imp replaces zero values with NA because zero is land.
mean_imps <- lapply(imps, FUN = get_mean_imp)
fl_combined$mean_imps <- unlist(mean_imps)


# Let's save this data object just in case something happens
#outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
#trailer <- paste0(outdate,".csv")
#write.csv(fl_combined2, paste0("Data/full_data_with_impacts",trailer), row.names=F)

#fl_combined2 <- read.csv('Data/full_data_with_impacts20150330.csv')

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


hadrast <- loadHadSST1('Data/', hadsstFilename = "HadISST_sst.nc")
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
write.csv(fl_combined, paste0("Data/full_data_with_impacts_velocity_invs_fert_pest",trailer), row.names=F)


################################################################################
# Additional study length climate velocity 
################################################################################
# Does it make sense for these to be averages?
