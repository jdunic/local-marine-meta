#fl_combined$date1 <- make_date_col(year = fl_combined$T1, month = fl_combined$T1m)
#fl_combined$date2 <- make_date_col(year = fl_combined$T2, month = fl_combined$T2m)


for each study/site/taxonomic grouping
get the start date and the end date

extract temperature values for all dates between those dates
start:end

calculate linear temperature change:

source('../new_hadsstr/vocc_raster_functions.R')

hadrast <- loadHadSST1('master_data/', hadsstFilename = "HadISST_sst.nc")

years <- c(1999, 2000, 2001)

test_coords <- fl_combined[1, c('Long.y', 'Lat.y')]


specific_lin_temp_change <- function(adf, hadsst_raster) {
  #browser()
  # get range of years for each study/site
  yr1 <- as.numeric(as.character(chron::years(adf$date1)))
  yr2 <- as.numeric(as.character(chron::years(adf$date2)))
  yrs <- yr1:yr2

  # lookup years in the hadsst raster - also grab the years to match with temps 
  # before taking the mean
  yearIDs  <- which(chron::years(hadrast@z$Date) %in% yrs)
  yrs <- as.numeric(as.character(chron::years(hadrast@z$Date)[yearIDs]))
  subset_x <- raster::subset(hadrast, yearIDs)

  # create a spatial object for raster extraction
  adf <- as.data.frame(adf)
  sp::coordinates(adf) <- c('Long.y', 'Lat.y')

  # extract temps and store as a plain vector
  temps <- as.vector(extract(subset_x, test_spdf))

  # get the annual mean temperatures before running the regression
  temps_df <- data.frame(year = yrs, temps = temps)
  mean_temp_df <- 
    temps_df %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_temp = mean(temps))

  time_ <- I(mean_temp_df$year - mean(mean_temp_df$year))
  slope <- lm(mean_temp_df$mean_temp ~ mean_temp_df$year)$coefficients[2]
  return(slope)
}

#fl_combined$row_key <- seq_along(fl_combined[, 1])

lin_change_slopes <- 
plyr::dlply(.data = fl_combined, .variables = 'row_key', function(x) specific_lin_temp_change(x, hadsst_raster = hadrast))
beep()

fl_combined$lin_change_specific <- as.vector(unlist(lin_change_slopes))

write.csv(fl_combined, 'Data_outputs/nov_5_fl_combined_for_wsn.csv')