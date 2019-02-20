
# Create spatial objects
# ----------------------------------------------------------------------------
read_sp_data <- function(filename) {
  spatial_data <- read_csv(file = filename, col_types = cols(Study.ID = col_character(), Reference = col_character(), Site = col_character()))
  spatial_data <- 
    mutate(spatial_data, site_id = paste(Study.ID, Site, sep = "_")) %>% 
    mutate(Start_Long = as.numeric(Start_Long), 
           Start_Lat = as.numeric(Start_Lat), 
           End_Long = as.numeric(End_Long), 
           End_Lat = as.numeric(End_Lat)) %>% 
    mutate(Shape = tolower(Shape))

  spatial_data$row <- seq_along(spatial_data$Study.ID)

  return(spatial_data)
}

create_sp_points <- function(spatial_data) {
  sp_data_points <- filter(spatial_data, Shape == 'point')
  sp_data_points <- as.data.frame(sp_data_points)

  # Coerce points to spatial points object with explicit projection
  coordinates(sp_data_points) <- ~Start_Long + Start_Lat
  projection(sp_data_points) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  return(sp_data_points)
}

# Create spatial lines object for extract
# Create a list of the points that make up each line (just a start and end 
# point in this case)
create_sp_lines <- function(spatial_data) {
  sp_data_lines <- filter(spatial_data, Shape == 'line')
  
  lines_list  <- 
    sp_data_lines %>% 
      split(.$row) %>%
      purrr:::map(function(x) {
        start <- x[, c('Study.ID', 'Reference', 'Site', 'Start_Lat', 'Start_Long')]
        end <- x[, c('Study.ID', 'Reference', 'Site', 'End_Lat', 'End_Long')]
        names(start) <- names(end) <- c('Study.ID', 'Reference', 'Site', 'Lat', 'Lon')
        out <- rbind(start, end)
        out <- as.data.frame(out)
        coordinates(out) <- ~Lon + Lat
        projection(out) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        out <- Line(out)
        out <- Lines(out, ID = x$row)
        return(out)
        })

  # Required for extract function input
  spatial_lines_obj <- SpatialLines(lines_list)

  return(spatial_lines_obj)
}

# ------------------------------------------------------------------------------
# Get cumulative human impact values (Halpern, 2013) for all spatial data points
# ------------------------------------------------------------------------------
extract_imp_data <- function(spatial_data, sp_points, sp_lines, in_file = 'master_data/Impact_Data/CI_2013_OneTimePeriod/global_cumul_impact_2013_all_layers.tif', 
  out_file = 'Data_outputs/spatial_data_with_cumulative_impacts.csv') {
  imp_map <- raster(in_file)

  point_imps <- raster::extract(imp_map, sp_points, buffer = 1000)
  # Clean up the list of lists of impact values for both point imps and line imps
  # get_mean_imp replaces zero values with NA because zero is land.
  mean_point_imps <- unlist(lapply(point_imps, FUN = get_mean_imp))

  line_imps <- raster::extract(imp_map, sp_lines, along = TRUE)
  mean_line_imps <- unlist(lapply(line_imps, FUN = get_mean_imp))

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')

  sp_data_points2$mean_imps <- mean_point_imps
  sp_data_lines2$mean_imps  <- mean_line_imps

  combined_data <- rbind(sp_data_points2, sp_data_lines2)

  # Save the data so I don't have to run all of this again + waste more time
  write_csv(combined_data, out_file)
  beepr::beep()
  return(combined_data)
}

# ------------------------------------------------------------------------------
# Get cumulative human impact values for 2008 Halpern et al. analysis for 
# all spatial data points
# ------------------------------------------------------------------------------
extract_imp_data_2008 <- function(spatial_data, sp_points, sp_lines) {
  imp_map <- raster('master_data/Impact_Data/CI_2013_OneTimePeriod/global_cumul_impact_2013_all_layers.tif')

  point_imps <- raster::extract(imp_map, sp_points, buffer = 1000)
  # Clean up the list of lists of impact values for both point imps and line imps
  # get_mean_imp replaces zero values with NA because zero is land.
  mean_point_imps <- unlist(lapply(point_imps, FUN = get_mean_imp))

  line_imps <- raster::extract(imp_map, sp_lines, along = TRUE)
  mean_line_imps <- unlist(lapply(line_imps, FUN = get_mean_imp))

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')

  sp_data_points2$mean_imps <- mean_point_imps
  sp_data_lines2$mean_imps  <- mean_line_imps

  combined_data <- rbind(sp_data_points2, sp_data_lines2)

  # Save the data so I don't have to run all of this again + waste more time
  write_csv(combined_data, 'Data_outputs/spatial_data_with_cumulative_impacts.csv')
  beepr::beep()
  return(combined_data)
}


# ------------------------------------------------------------------------------
# Get addtional impact values 
# ------------------------------------------------------------------------------

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

extract_invs_data <- function(spatial_data, sp_points, sp_lines) {
  invasives <- raster('master_data/Impact_Data/invasives_raw/invasives.tif')
  point_invs <- raster::extract(invasives, sp_points, buffer = 1000)
  line_invs <- raster::extract(invasives, sp_lines, along = TRUE)

  #point_invs_NA_switched <- lapply(point_invs, FUN = switch_NA_zero)
  #line_invs_NA_switched <- lapply(line_invs, FUN = switch_NA_zero)

  mean_point_invs <- lapply(point_invs, FUN = mean, na.rm = TRUE)
  mean_line_invs  <- lapply(line_invs, FUN = mean, na.rm = TRUE)

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')

  sp_data_points2$mean_invs <- unlist(mean_point_invs)
  sp_data_lines2$mean_invs  <- unlist(mean_line_invs)

  invs_combined_data <- rbind(sp_data_points2, sp_data_lines2)

  # Save the data so I don't have to run all of this again + waste more time
  write_csv(invs_combined_data, 'Data_outputs/spatial_data_with_invasives.csv')
  return(invs_combined_data)
}

# Nutrients/Fertilizer layer # 
# -----------------------------------------------------------------------
extract_nut_data <- function(spatial_data, sp_points, sp_lines) {
  nuts <- raster('master_data/Impact_Data/plumes_fertilizer_raw/plumes_fert.tif')
  point_nuts <- raster::extract(nuts, sp_points, buffer = 1000)
  mean_point_nuts <- lapply(point_nuts, FUN = mean, na.rm = TRUE)
  line_nuts <- raster::extract(nuts, sp_lines, along = TRUE)
  mean_line_nuts <- lapply(line_nuts, FUN = mean, na.rm = TRUE)

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')

  sp_data_points2$mean_nuts <- unlist(mean_point_nuts)
  sp_data_lines2$mean_nuts  <- unlist(mean_line_nuts)

  nuts_combined_data <- rbind(sp_data_points2, sp_data_lines2)

  # Save the data so I don't have to run all of this again + waste more time
  write_csv(nuts_combined_data, 'Data_outputs/spatial_data_with_nutrients.csv')

  return(nuts_combined_data)
}

# Pesticide layer # 
# -----------------------------------------------------------------------
extract_pest_data <- function(spatial_data, sp_points, sp_lines) {
  pesticides <- raster('master_data/Impact_Data/plumes_pesticide_raw/plumes_pest.tif')
  point_pests <- raster::extract(pesticides, sp_points, buffer = 1000)
  mean_point_pests <- lapply(point_pests, FUN = mean, na.rm = TRUE)
  line_pests <- raster::extract(pesticides, sp_lines, along = TRUE)
  mean_line_pests <- lapply(line_pests, FUN = mean, na.rm = TRUE)

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')

  sp_data_points2$mean_pests <- unlist(mean_point_pests)
  sp_data_lines2$mean_pests  <- unlist(mean_line_pests)

  pest_combined_data <- rbind(sp_data_points2, sp_data_lines2)

  write_csv(pest_combined_data, 'Data_outputs/spatial_data_with_pesticides.csv')

  return(pest_combined_data)
}


# HadSST data extraction # 
# -----------------------------------------------------------------------

get_total_duration_rasters <- function(fl_data, hadsst_file = 'master_data/HadISST_sst.nc') {
  yr_min <- min(fl_data$T1)
  yr_max <- max(fl_data$T2)

  hadrast <- load_hadsst(file = hadsst_file)

  all_rasters <- get_all_rasters(hadrast, years = yr_min:yr_max)
  return(all_rasters)
}

get_total_vel_raster <- function(all_rasters) {
  velocity <- select_raster(all_rasters, 'velocity_magnitude')
  return(velocity)
}

get_total_ltc_raster <- function(all_rasters) {
  linear <- select_raster(all_rasters, 'linear_change')
  return(linear)
}

get_total_ltc_data <- function(total_ltc_raster, spatial_data, sp_points, 
  sp_lines) {
  single_sst_points <- raster::extract(total_ltc_raster, sp_points, buffer = 1000)
  single_sst_lines <- raster::extract(total_ltc_raster, sp_lines, along = TRUE)
  mean_single_sst_points <- unlist(lapply(single_sst_points, FUN = get_mean_imp))
  mean_single_sst_lines <- unlist(lapply(single_sst_lines, FUN = get_mean_imp))

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')


  sp_data_points2 <- mutate(sp_data_points2, total_ltc = unlist(mean_single_sst_points))
  sp_data_lines2 <- mutate(sp_data_lines2, total_ltc = unlist(mean_single_sst_lines))

  single_temp_changes <- rbind(sp_data_points2, sp_data_lines2)

  write.csv(single_temp_changes, 'Data_outputs/spatial_data_with_total_ltc.csv')
  return(single_temp_changes)
}

get_total_vel_data <- function(total_vel_raster, spatial_data, sp_points, 
  sp_lines) {
  single_sst_points <- raster::extract(total_vel_raster, sp_points, buffer = 1000)
  single_sst_lines <- raster::extract(total_vel_raster, sp_lines, along = TRUE)
  mean_single_sst_points <- unlist(lapply(single_sst_points, FUN = get_mean_imp))
  mean_single_sst_lines <- unlist(lapply(single_sst_lines, FUN = get_mean_imp))

  sp_data_points2 <- filter(spatial_data, Shape == 'point')
  sp_data_lines2  <- filter(spatial_data, Shape == 'line')


  sp_data_points2 <- mutate(sp_data_points2, total_vocc = unlist(mean_single_sst_points))
  sp_data_lines2 <- mutate(sp_data_lines2, total_vocc = unlist(mean_single_sst_lines))

  single_temp_changes <- rbind(sp_data_points2, sp_data_lines2)

  write.csv(single_temp_changes, 'Data_outputs/spatial_data_with_total_vocc.csv')
  return(single_temp_changes)
}


# Get list of unique durations

get_study_dates <- function(fl_data) {
  durations <- unique(fl_data[, c('T1', 'T2')])
  return(durations)
}

get_start_end_dates <- function(durations) {
  duration_text <- paste(durations$T1, durations$T2, sep = '_')
  return(duration_text)
}

# Create a list of raster stacks that have the time spans as the z values so 
# that I can link them back up to the studies in the first last data

get_raster_stack_list <- function(durations, hadsst_file = 'master_data/HadISST_sst.nc', start_end_dates){
  rast_set <- list()
  hadrast <- load_hadsst(file = hadsst_file)

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

  names(rast_set) <- start_end_dates
  return(rast_set)
}

# Get a simplified data frame that has the necessary data needed to lookup 
# across tables. 
get_sp_points_lookup <- function(spatial_data, fl_data) {
  fl_data <- mutate(fl_data, site_id = paste(Study.ID, Site, sep = "_"))
  sp_data_points_lookup <- filter(spatial_data, Shape == 'point') %>% 
    dplyr::select(site_id, Start_Lat, Start_Long, row) %>% 
    left_join(x = ., y = fl_data, by = c('site_id' = 'site_id')) %>% 
    mutate(timespans = paste(.$T1, .$T2, sep = "_")) %>%
    dplyr::select(site_id, Study.ID, Site, Start_Lat, Start_Long, row, timespans) %>%
    distinct(row, .keep_all = TRUE) %>%
    as_data_frame(.)
#
  sp_data_points_list <- split(sp_data_points_lookup, seq(nrow(sp_data_points_lookup)))
#
  return(sp_data_points_list)
}

# Create a simplified lookup for the spatial line data that joins the spatial 
# data and first last data so that I can get the right raster dates.
get_sp_lines_lookup <- function(spatial_data, fl_data) {
  fl_data <- mutate(fl_data, site_id = paste(Study.ID, Site, sep = "_"))
  sp_data_lines <- 
    filter(spatial_data, Shape == 'line') %>% 
    dplyr::select(-Study.ID, -Site) %>% 
    left_join(x = ., y = mutate(fl_data, timespans = paste(T1, T2, sep = "_")), by = c('site_id' = 'site_id')) %>%
    dplyr::select(site_id, Study.ID, Site, Start_Lat, Start_Long, End_Lat, End_Long, row, timespans) %>%
      as_data_frame(.)
  return(sp_data_lines)
}

mk_ltc_point_vals_list <- function() {
  ltc_point_vals <- list()
  return(ltc_point_vals)
}

mk_vocc_point_vals_list <- function() {
  vocc_point_vals <- list()
  return(vocc_point_vals)
}

mk_ltc_line_vals_list <- function() {
  ltc_line_vals <- list()
  return(ltc_line_vals)
}

mk_vocc_line_vals_list <- function() {
  vocc_line_vals <- list()
  return(vocc_line_vals)
}

get_specific_ltc_points <- function(sp_data_points_list, ltc_point_vals, raster_stack) {
  for (i in seq_along(sp_data_points_list)) {
    sp_data_points_list[[i]] <- as.data.frame(sp_data_points_list[[i]], stringsAsFactors = FALSE)
#
    time_span_lookup <- sp_data_points_list[[i]]$timespans
#
    lin_change_rast <- raster::subset(raster_stack[[time_span_lookup]], 'linear_change')
#
    coordinates(sp_data_points_list[[i]]) <- ~Start_Long + Start_Lat
    projection(sp_data_points_list[[i]]) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#
    ltc_point_vals[[i]] <- raster::extract(lin_change_rast, sp_data_points_list[[i]], buffer = 1000)
#
    names(ltc_point_vals[[i]]) <- sp_data_points_list[[i]]$row
  }
  return(ltc_point_vals)
}

get_specific_vocc_points <- function(sp_data_points_list, vocc_point_vals, raster_stack) {
  for (i in seq_along(sp_data_points_list)) {
    sp_data_points_list[[i]] <- as.data.frame(sp_data_points_list[[i]], stringsAsFactors = FALSE)
#
    time_span_lookup <- sp_data_points_list[[i]]$timespans

    vocc_rast <- raster::subset(raster_stack[[time_span_lookup]], 'velocity_magnitude')
#
    coordinates(sp_data_points_list[[i]]) <- ~Start_Long + Start_Lat
    projection(sp_data_points_list[[i]]) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

    vocc_point_vals[[i]] <- raster::extract(vocc_rast, sp_data_points_list[[i]], buffer = 1000)

    names(vocc_point_vals[[i]]) <- sp_data_points_list[[i]]$row
  }
  return(vocc_point_vals)
}

# Fortunately the spatial lines object is already in list format and I named 
# each element as the row in the spatial data sheet so that we can lookup using 
# the row number and follow the relationship to the time span through 
# sp_lines

get_specific_ltc_lines <- function(sp_lines_lookup, sp_lines, ltc_line_vals, raster_stack)
for (i in seq_along(sp_lines_lookup)) {
  time_span_lookup <- 
  filter(sp_lines_lookup, row == names(sp_lines[i])) %>% 
  dplyr::select(timespans) %>% 
  .[[1]]
#  
  lin_change_rast <- raster::subset(raster_stack[[time_span_lookup]], 'linear_change')
#
  ltc_line_vals[[i]] <- raster::extract(lin_change_rast, sp_lines[i], along = TRUE)
  names(ltc_line_vals[[i]]) <- names(sp_lines[i])

  return(ltc_line_vals)
}

get_specific_vocc_lines <- function(sp_lines_lookup, sp_lines, vocc_line_vals, raster_stack)
for (i in seq_along(sp_lines_lookup)) {
  time_span_lookup <- 
  filter(sp_lines_lookup, row == names(sp_lines[i])) %>% 
  dplyr::select(timespans) %>% 
  .[[1]]
#  
  vocc_rast <- raster::subset(raster_stack[[time_span_lookup]], 'velocity_magnitude')
#
  vocc_line_vals[[i]] <- raster::extract(vocc_rast, sp_lines[i], along = TRUE)
  names(vocc_line_vals[[i]]) <- names(sp_lines[i])

  return(vocc_line_vals)
}

get_mean_specific_ltc <- function(specific_ltc_points, specific_ltc_lines) {
  mean_lin_change <-
    lapply(c(specific_ltc_points, specific_ltc_lines), function(x) {
      mean_lin <- mean(x[[1]], na.rm = TRUE)
      names(mean_lin) <- names(x)
      return(mean_lin)
  })
  return(mean_lin_change)
}

get_mean_specific_vocc <- function(specific_vocc_points, specific_vocc_lines) {
  mean_vocc <-
    lapply(c(specific_vocc_points, specific_vocc_lines), function(x) {
      mean_vocc <- mean(x[[1]], na.rm = TRUE)
      names(mean_vocc) <- names(x)
      return(mean_vocc)
  })
  return(mean_vocc)
}


combine_temp_data <- function(mean_lin_change, mean_vocc, spatial_data) {
  temp_data <- 
    data_frame(row = as.numeric(unlist(lapply(mean_lin_change, names))), 
               sliced_ltc = as.vector(unlist(mean_lin_change)), 
               sliced_vocc = as.vector(unlist(mean_vocc)))

  temp_data_combined <- 
    left_join(x = spatial_data, y = temp_data, by = c('row' = 'row'))

  # Save the data so I don't have to run all of this again + waste more time
  write.csv(temp_data_combined, "Data_outputs/spatial_data_with_temp_data.csv", row.names = FALSE)

  return(temp_data_combined)
}


# Add driver data to cleaned master data
combine_fl_data <- function(fl_data, spatial_data, imps, invs, nuts, pest, total_ltc, total_vocc, sliced_temp) {

  fl_data <- mutate(fl_data, site_id = paste(Study.ID, Site, sep = "_"))

  imps <- dplyr::select(imps, row, mean_imps)
  invs <- dplyr::select(invs, row, mean_invs)
  nuts <- dplyr::select(nuts, row, mean_nuts)
  pest <- dplyr::select(pest, row, mean_pests)
  total_ltc <- dplyr::select(total_ltc, row, total_ltc)
  total_vocc <- dplyr::select(total_vocc, row, total_vocc)
  single_temp <- dplyr::select(sliced_temp, row, sliced_ltc, sliced_vocc)

  spatial_data <- mutate(spatial_data, site_id = paste(Study.ID, Site, sep = "_"))

  # Add rows so that I can create the spatial lines dataframe
  spatial_data$row <- seq_along(spatial_data$Study.ID)

  all_spatial_data <- 
  left_join(spatial_data, imps, by = c('row' = 'row')) %>% 
    left_join(., invs, by = c('row' = 'row')) %>% 
    left_join(., nuts, by = c('row' = 'row')) %>% 
    left_join(., pest, by = c('row' = 'row')) %>% 
    left_join(., total_ltc, by = c('row' = 'row')) %>% 
    left_join(., total_vocc, by = c('row' = 'row')) %>% 
    left_join(., single_temp, by = c('row' = 'row'))

  summarised_spatial <- 
    all_spatial_data %>% 
    dplyr::select(-Study.ID, -Reference, -Site, -Notes, -Source) %>% 
    full_join(., fl_data, by = c('site_id' = 'site_id')) %>%
    group_by(id) %>% 
    summarise(mean_imps = mean(mean_imps, na.rm = TRUE), 
              mean_invs = mean(mean_invs, na.rm = TRUE), 
              mean_nuts = mean(mean_nuts, na.rm = TRUE), 
              mean_pests = mean(mean_pests, na.rm = TRUE), 
              total_ltc = mean(total_ltc, na.rm = TRUE), 
              total_vocc = mean(total_vocc, na.rm = TRUE), 
              sliced_ltc = mean(sliced_ltc, na.rm = TRUE), 
              sliced_vocc = mean(sliced_vocc, na.rm = TRUE))

  fl_combined <- 
    left_join(fl_data, summarised_spatial, by = c('id' = 'id'))

  write.csv(fl_combined, 'Data_outputs/fl_combined.csv', row.names = FALSE)

  return(fl_combined)
}