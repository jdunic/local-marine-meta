process_master_data <- function(richData = rich_data, meta_data = meta_data, event_data = event_data, subset_list = subset_list, cbdata = cb_data) {

  # Event types should NOT be 'No'
  richData <- mutate(richData, Event.type = replace(Event.type, Event.type == 'No', NA))

  # Remove unnecessary columns to make my life easier later
  richData <- select(richData, -Lat, -Long, -SiSz, -SiSz..units.)

  ###########
  # Add event type categorisation.
  ###########
  # Write a list of the unique event types so that they can be classified
  #event_list <- unique(richData$Event.type)
  #write.csv(event_list[order(event_list)], 'master_data/event_list.csv')

  # Add additional event data
  event_data <- 
    event_data %>% 
    mutate(event_category = trimws(.$event_category), 
           event_desc = trimws(.$event_desc), 
           expected_change = trimws(.$expected_change), 
           event_notes = trimws(.$event_notes))

  richData <- left_join(richData, event_data, by = c('Event.type' = 'event_desc'))

  richData <- rename(richData, Event = Event.)
  # Check that the event data spreadsheet is up to date:
  extra_events <- setdiff(richData$Event.type, event_data$event_desc)
  extra_events

  if (sum(!is.na(extra_events)) > 1) {
    stop('There are listed in the master data sheet that are unaccounted for in the Event Types spreadsheet. Please update this before running this script.')
  }


  ###########
  ## Convert Error Types to SD
  ###########

  convertError <- function(x, errorType, n){
    errorType <- as.character(errorType)
    errorType <- gsub("\n", "", errorType)
    
    ret <- rep(NA, length(x))
    
    #first, the SD
    ret[grep("SD", errorType)] <- x[grep("SD", errorType)]
    ret[grep("sd", errorType)] <- x[grep("sd", errorType)]
    ret[grep("Sd", errorType)] <- x[grep("Sd", errorType)]
    
    #The CI
    ciIDX <- grep("95", errorType)
    x[ciIDX] <- x[ciIDX]/1.96 #assuming 1.96 for the CI
    
    #SE
    seIDX <- c(ciIDX, grep("SE", errorType))
    ret[seIDX] <- x[seIDX] * sqrt(n[seIDX])
    
    ret
  }

  getSD <- function(colname, dataset=richData){
    errName <- paste(colname, "Error", sep=".")
    errType <- paste(colname, "Error.Type", sep=".")
    n <- paste("n", gsub("(.*)(\\d)", "\\2", colname), sep="")
    convertError(dataset[[errName]], dataset[[errType]], dataset[[n]])  
  }

  errVec <- qw(SppR1, SppR2, Shan1, Shan2, Simps1, Simps2, Even1, Even2)

  # Do the SD calculation for all of the sites.
  for (err in errVec) {
    # Create new error column
    newCol <- paste(err, "SD", sep=".")
    error_correction <- tryCatch(getSD(err), 
                                   error = function(e) e, 
                                   warning = function(w) w
    )
    
    if (inherits(error_correction, 'error')) { 
      print(newCol)
      print(error_correction)
      next }
    richData[[newCol]] <- error_correction
  }

  avec <- paste(errVec, "Error.Type", sep=".")
  for(i in avec) {
    #print(i)
    #print(levels(richData[[i]]))
    cat("\n")
  }

  ###########
  ## Fix capitalization of Sys
  ###########
  richData$Sys <- as.character(richData$Sys)
  richData$Sys  <- tolower(richData$Sys)
  richData$Sys <- gsub("coarl", "coral", richData$Sys)
  richData$Sys <- gsub("rocky bottom", "rocky subtidal", richData$Sys)
  richData$Sys <- gsub("coastalshelf", "coastal shelf", richData$Sys)
  richData$Sys <- gsub("shelf", "coastal shelf", richData$Sys)
  richData$Sys <- gsub("coastal coastal shelf", "coastal shelf", richData$Sys)
  unique(richData$Sys)


  ######################################
  # Site and plot size unit conversions
  ######################################

  # Load modified datamart::uncov function set to allow use of '^' in unit names
  # and to silence datamart::mashup output
  source("R/datamart_UnitSetManager2_source.r")
  source("R/datamart_mashup_source.r")

  # Standardize unit names
  richData$PltSz..units. <- tolower(as.character(richData$PltSz..units.))

  unique(richData$PltSz..units.)


  richData$PltSz..units. <- gsub('acres', 'ac', richData$PltSz..units.)
  richData$PltSz..units. <- gsub('km2', 'km^2', richData$PltSz..units.)
  richData$PltSz..units. <- gsub('m2', 'm^2', richData$PltSz..units.)
  richData$PltSz..units. <- gsub('m3', 'm^3', richData$PltSz..units.)
  print('Double check that \'nm\' is nautical miles if you run this script')
  richData$PltSz..units. <- gsub('^nm[^i]', 'nmi', richData$PltSz..units.)
  richData$PltSz..units. <- gsub('liters', 'l', richData$PltSz..units.)


  convert_units <- function(value, unit) {
    if (is.na(unit)) {
      out_value <- NA
      out_unit <- NA
    } else if (unit %in% uconvlist()$Length) {
        out_value <- uconv(x = value, from = unit, to = 'm', uset = 'Length')
        out_unit <- 'm'
    } else if (unit %in% uconvlist()$Area) {
        out_value <- uconv(x = value, from = unit, to = 'm^2', uset = 'Area')
        out_unit <- 'm^2'
    } else if (unit %in% uconvlist()$Volume) {
        out_value <- uconv(x = value, from = unit, to = 'm^3', uset = 'Volume')
        out_unit <- 'm^3'
    } else if (!unit %in% c(uconvlist()$Length, uconvlist()$Area, uconvlist()$Volume)) {
        print(unit)
        stop('unit not in uconvlist of Length, Area, or Volume')
    }
      convert_df <- data.frame('new_val' = as.numeric(out_value), 'new_unit' = as.character(out_unit))
      #browser()
      return(convert_df)
  }

  # works!
  plot_size_conversion <- 
    richData %>% 
      rowwise() %>%
          do(convert_units(value = .$PltSz, unit = .$PltSz..units.)) 

  names(plot_size_conversion) <- c('PlotSize', 'PlotSizeUnits')
  richData <- tbl_df(bind_cols(richData, plot_size_conversion))

  # Is there a taxonomic type?
  types <- c("coral", "plant", "algae", 
             "fish", 'inverts', "mobile.inverts", "sessile.inverts", 
             "marine.mammals", "phytoplankton", "zooplankton")

  typeSum <- rowSums(select(richData, one_of(types)), na.rm = T)

  noTaxa <- richData[which(typeSum == 0), c('Study.ID', 'Reference', 
                                            'Collector')]
  noTaxa

  missing_data_df <- data.frame(Study.ID = NA, Reference = NA, 
                                Collector = NA, Note = NA)

  missing <- select(noTaxa, Study.ID, Reference, Collector)
  if (!is.data.frame(missing)) {
    missing$Note <- 'This sample is missing taxa'
  }

  missing_data_df <- rbind(missing_data_df, missing)

  sampling <- c("Vis", "Trwl", "Line", "Drdg", "Trp")

  sampleSum <- rowSums(select(richData, one_of(sampling)), na.rm=T)
  noSample <- richData[which(sampleSum == 0), ]
  noSample

  if (dim(noSample)[1] != 0) {
    missing <- select(noSample, Study.ID, Reference, Collector)
    if (is.data.frame(missing)) {
      missing$Note <- 'This sample is missing a sampling method'
    }
  }

  missing_data_df <- rbind(missing_data_df, missing)


  ####################
  # Add some Taxonomic Info
  ####################we
  # match taxa to their types

  getTaxa <- function(taxa_list) {
    #browser()
    taxa_list <- as.character(taxa_list)
    invert_check1 <- sum(c('mobile.inverts', 'sessile.inverts') %in% taxa_list)
    invert_check2 <- sum(c('inverts', 'mobile.inverts', 'sessile.inverts') %in% taxa_list)
    if (length(taxa_list) == 1) {
      taxa = taxa_list
    }
    if (length(taxa_list) == 2 & (invert_check1 == 2 | invert_check2 == 3)) {
      taxa <- 'Mixed inverts'
    } 
    if (length(taxa_list) > 1) {
      taxa <- 'Mixed'
    }
    return(as.data.frame(taxa))
  }

  richData$id <- 1:nrow(richData)

  taxa_values <- 
    richData %>% 
    select(id, Study.ID, Site, Sys, T1, T1m, T2, T2m, coral, plant, algae, 
           fish, inverts, mobile.inverts, sessile.inverts, marine.mammals, 
           phytoplankton, zooplankton) %>%
    group_by(id) %>%
    #group_by(Study.ID, Site, Sys, T1, T1m, T2, T2m) %>%
    gather(key, value, -id, -Study.ID, -Site, -Sys, -T1, -T1m, -T2, -T2m) %>%
    filter(value == 1) %>%
    group_by(id) %>%
    do(getTaxa(taxa_list = .$key)) %>%
    arrange(id)
  richData$taxa <- taxa_values$taxa

  if (nrow(taxa_values) != nrow(richData)) {
    print('Something went wrong converting taxonomic data to single taxa column')
  }

  # remove the now redundant taxa columns
  richData <- select(richData, -plant, -coral, -algae, -fish, -inverts, -mobile.inverts, 
                     -sessile.inverts, -marine.mammals, -phytoplankton, 
                     -zooplankton)

  # Get nice sampling method column
  getSampMethod <- function(samp_method_list) {
    #browser()
    samp_method_list <- as.character(samp_method_list)
    if (length(samp_method_list) == 1) {
      samp_method = samp_method_list
    }
    if (length(samp_method_list) > 1) {
      samp_method <- 'Multiple'
    }
    return(as.data.frame(samp_method))
  }

  samp_methods <- 
    richData %>% 
    select(id, Study.ID, Site, Sys, T1, T1m, T2, T2m, Vis:Trp) %>%
    group_by(id) %>%
    #group_by(Study.ID, Site, Sys, T1, T1m, T2, T2m) %>%
    gather(key, value, -id, -Study.ID, -Site, -Sys, -T1, -T1m, -T2, -T2m) %>%
    filter(value == 1) %>%
    group_by(id) %>%
    do(getSampMethod(samp_method_list = .$key)) %>%
    arrange(id)

  richData$samp_method <- samp_methods$samp_method

  # remove the now redundant sampling columns
  richData <- select(richData, -Vis, -Trwl, -Line, -Drdg, -Trp)


  ###########
  ## Make Months Consistent
  ###########

  fixMonths <- function(a){
    a <- as.character(a)
    mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    #take out the few that are unspecified
    a[agrep("Unspecified", a)] <- NA
    a[agrep("Winter", a)] <- 1 #midpoint of winter
    
    #Do it in order, such that studies with a range use the first month sampled
    for(i in 1:12){
      idx <- agrep(mon[i], a)
      if(length(idx)>0) a[idx] <- i
    }
    
    a
  }

  richData$T1m <- fixMonths(richData$T1m)
  richData$T2m <- fixMonths(richData$T2m)



  ###########
  ## Downsample the data to T1 and T2 - first/last
  ###########

  # What is going on here? - I don't even know what this is/was about
  # Where did these studies go? They are marked as being completed but they are 
  # not found in the master dataset.
  #xdf <- subset(richData, richData$Study.ID %in% c(3, 741, 134, 89))
  #xdf$Site <- factor(xdf$Site)
  #adf <- subset(xdf, xdf$Site==xdf$Site[1])

  # Get columns for all the first time points and all the second time points
  y1Cols <- grep("1", names(richData))
  y2Cols <- grep("2", names(richData))
  noYCols <- (1:length(names(richData)))[-c(y1Cols, y2Cols)]

  #until they clear the wall of shame
  #excludeStudy <- c(712, 702, 339, 691, 693, 3, 19, 741, 59, 134, 89, 705, 51)
  #excludeStudy <- c(339)

  # Revised function to get first-last values that have consistent sampling months
  make_date_col <- function(year, month) {
    dates <- structure(numeric(1), class="Date")
    for (i in seq_along(year)) {
      #browser()
      if (is.na(month[i]) | month[i] == '') {
        dates[i] <- as.Date(paste0(as.character(year[i]), '/01/01'), '%Y/%m/%d')
      } else {
        dates[i] <- as.Date(paste0(as.character(year[i]), '/', month[i], '/01'), '%Y/%m/%d')
      }
    }
    return(dates)
  }

  richData$date1 <- make_date_col(year = richData$T1, month = richData$T1m)
  richData$date2 <- make_date_col(year = richData$T2, month = richData$T2m)


  # Using the new date functions in 02_functions.R
  firstSampleFilteredData <- 
    richData %>% 
      group_by(Study.ID, Reference, Sys, taxa, samp_method, 
               Descriptor.of.Taxa.Sampled, Loc, Site) %>%
      do(get_first_last(.)) %>% 
      ungroup()

  str(firstSampleFilteredData)

  # Add duration to the data frame
  firstSampleFilteredData$Duration <- with(firstSampleFilteredData, T2-T1)

  ############
  #calculate a lot of effect sizes
  ############
  measurements <- c("SppR", "Shan", "Even")#, "Simps")
  effects <- c("MD", "SMD", "ROM", "SMDH")

  for(j in measurements){
    for(i in effects){
      cat(paste(i, j, sep=","))
      cat("\n")
      v1 = firstSampleFilteredData[[paste0(j, 1)]]
      v2 = firstSampleFilteredData[[paste0(j, 2)]]
      v1sd = firstSampleFilteredData[[paste(paste0(j, 1), "SD", sep=".")]]
      v2sd = firstSampleFilteredData[[paste(paste0(j, 2), "SD", sep=".")]]
      var.names = paste(c("yi", "vi"), j, i, sep=".")
      firstSampleFilteredData <- escalc(i, m1i=v2, sd1i = v2sd, n1i = n2, 
                        m2i=v1, sd2i = v1sd, n2i = n1, data=firstSampleFilteredData,
                        append=T, var.names=var.names)  
   }
  }

  # Remove more attributes that were magically added when doing the escalc
  attributes(firstSampleFilteredData)[c('digits', 'yi.names', 'vi.names')] <- NULL

  # Check that there are no notes
  str(firstSampleFilteredData)

  attributes(firstSampleFilteredData$yi.SppR.MD)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.SppR.SMD)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.SppR.ROM)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.SppR.SMDH)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Shan.MD)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Shan.SMD)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Shan.ROM)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Shan.SMDH)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Even.MD)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Even.SMD)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Even.ROM)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$yi.Even.SMDH)[c('measure', 'ni')] <- NULL
  attributes(firstSampleFilteredData$vi.Even.SMDH)[c('measure', 'ni')] <- NULL

  str(firstSampleFilteredData)

  ############
  # Add CB data
  ############

  cbdata <- 
    cbdata[, -1] %>% 
    filter(studyName %in% subset_list[[1]]) %>% 
    rename(Site = site)


  # get average number of replicates
  avg_reps <- function(plot_number) {
    if (is.na(as.numeric(plot_number)) == TRUE) {
      plot_counts <- as.numeric(strsplit(plot_number, '--')[[1]])
      reps <- mean(plot_counts)
      return(reps)
    }
    plot_counts <- as.numeric(plot_number)
    reps <- mean(plot_counts)
    return(reps)
  }

  cbdata <- 
    cbdata %>%
      mutate(temp_id = seq_along(cbdata[[1]])) %>%
      group_by(temp_id) %>%
      mutate(., n = avg_reps(PltN)) %>% 
      select(-temp_id, -PltN) %>% 
      ungroup()

  names(cbdata)

  # General housekeeping - stripping whitespace
  cbdata <- 
    cbdata %>% 
    mutate(Reference = trimws(.$Reference), 
           Collector = trimws(.$Collector), 
           Descriptor.of.Taxa.Sampled = trimws(.$Descriptor.of.Taxa.Sampled), 
           Sys = trimws(.$Sys), 
           Event_type = trimws(.$Event_type), 
           Year_of_Event = trimws(.$Year_of_Event), 
           Site = trimws(.$Site))

  # Add event data to CB data
  cbdata <- left_join(cbdata, event_data, by = c('Event_type' = 'event_desc'))

  # Check that the event data spreadsheet is up to date:
  extra_events <- setdiff(cbdata$Event_type, event_data$event_desc)
  extra_events

  if (sum(!is.na(extra_events)) > 1) {
    stop('There are listed in this data sheet that are unaccounted for in the Event Types spreadsheet. Please update this before running this script.')
  }

  # Units were already standardised by Robin. There is one set of values where 
  # the plot size units are NA - should double check what those should be. 

  getTaxa <- function(taxa_list) {
    #browser()
    taxa_list <- as.character(taxa_list)
    invert_check1 <- sum(c('mobile.inverts', 'sessile.inverts') %in% taxa_list)
    invert_check2 <- sum(c('inverts', 'mobile.inverts', 'sessile.inverts') %in% taxa_list)
    if (length(taxa_list) == 1) {
      taxa = taxa_list
    }
    if (length(taxa_list) == 2 & (invert_check1 == 2 | invert_check2 == 3)) {
      taxa <- 'Mixed inverts'
    } 
    if (length(taxa_list) > 1) {
      taxa <- 'Mixed'
    }
    return(as.data.frame(taxa))
  }

  cbdata$id <- 1:nrow(cbdata)

  taxa_values <- 
    cbdata %>% 
    select(id, protist, coral, plant, algae, 
           fish, inverts, mobile.inverts, sessile.inverts, marine.mammals, 
           phytoplankton, zooplankton) %>%
    group_by(id) %>%
    #group_by(Study.ID, Site, Sys, T1, T1m, T2, T2m) %>%
    gather(key, value, -id) %>%
    filter(value == 1) %>%
    group_by(id) %>%
    do(getTaxa(taxa_list = .$key)) %>%
    arrange(id)

  cbdata <- 
    cbdata %>% 
      select(-protist, -coral, -plant, -algae, -fish, -inverts, -mobile.inverts, 
             -mobile.inverts, -sessile.inverts, -marine.mammals, -phytoplankton, 
             -zooplankton)

  cbdata$taxa <- taxa_values$taxa

  if (nrow(taxa_values) != nrow(cbdata)) {
    print('Something went wrong converting taxonomic data to single taxa column')
  }

  # Get nice sampling method column
  getSampMethod <- function(samp_method_list) {
    #browser()
    samp_method_list <- as.character(samp_method_list)
    if (length(samp_method_list) == 1) {
      samp_method = samp_method_list
    }
    if (length(samp_method_list) > 1) {
      samp_method <- 'Multiple'
    }
    return(as.data.frame(samp_method))
  }

  samp_methods <- 
    cbdata %>% 
    select(id, Vis:Trp) %>%
    group_by(id) %>%
    gather(key, value, -id) %>%
    filter(value == 1) %>%
    group_by(id) %>%
    do(getSampMethod(samp_method_list = .$key)) %>%
    arrange(id)

  cbdata$samp_method <- samp_methods$samp_method

  # remove the now redundant sampling columns
  cbdata <- select(cbdata, -Vis, -Trwl, -Line, -Drdg, -Trp)

  # rearrange dates to match formatting in richData
  split_dates_list <- 
  cbdata %>% 
    split(.$subSiteID) %>% 
    map(function(grouping_df = .[]) {
         #browser()
         T1 <- grouping_df[1:(length(grouping_df[[1]]) - 1), ] %>% 
               rename(date1 = dateR, n1 = n, SppR1 = rich, I1 = abund, Shan1 = div)
         T2 <- grouping_df[2:length(grouping_df[[1]]), c('dateR', 'n', 'rich', 
               'abund', 'div')] %>% 
               rename(date2 = dateR, n2 = n, SppR2 = rich, I2 = abund, Shan2 = div)
         t1_t2_df <- cbind(T1, T2)
         return(t1_t2_df)
    })

  cbdata <- rbind_all(split_dates_list)

  # Using the new date functions in 02_functions.R
  cb_firstSampleFilteredData <- 
    cbdata %>% 
      group_by(subSiteID) %>%
      do(get_first_last(., dataset = 'cbdata')) %>% 
      ungroup()

  str(cb_firstSampleFilteredData)

  cb_firstSampleFilteredData <- 
    cb_firstSampleFilteredData %>% 
      mutate(T1 = year(date1), T1m = month(date1), T2 = year(date2), 
             T2m = month(date1)) %>% 
      mutate(Duration = T2 - T1)



  ############
  #calculate a lot of effect sizes
  ############
  measurements <- c("SppR", "Shan")#, "Simps")
  effects <- c("MD", "SMD", "ROM", "SMDH")

  for(j in measurements){
    for(i in effects){
      cat(paste(i, j, sep=","))
      cat("\n")
      v1 = cb_firstSampleFilteredData[[paste0(j, 1)]]
      v2 = cb_firstSampleFilteredData[[paste0(j, 2)]]
      v1sd = rep(NA, length(cb_firstSampleFilteredData[[1]]))
      v2sd = rep(NA, length(cb_firstSampleFilteredData[[1]]))
      var.names = paste(c("yi", "vi"), j, i, sep=".")
      cb_firstSampleFilteredData <- escalc(i, m1i=v2, sd1i = v2sd, n1i = n2, 
                        m2i=v1, sd2i = v1sd, n2i = n1, data=cb_firstSampleFilteredData,
                        append=T, var.names=var.names)  
   }
  }

  # Remove more attributes that were magically added when doing the escalc
  attributes(cb_firstSampleFilteredData)[c('digits', 'yi.names', 'vi.names')] <- NULL

  # Check that there are no notes
  str(cb_firstSampleFilteredData)

  attributes(cb_firstSampleFilteredData$yi.SppR.MD)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.SppR.SMD)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.SppR.ROM)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.SppR.SMDH)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.Shan.MD)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.Shan.SMD)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.Shan.ROM)[c('measure', 'ni')] <- NULL
  attributes(cb_firstSampleFilteredData$yi.Shan.SMDH)[c('measure', 'ni')] <- NULL

  str(cb_firstSampleFilteredData)


  # Make sure that some of the 
  cb_firstSampleFilteredData <- 
    cb_firstSampleFilteredData %>% 
      rename(Study.ID = studySub) %>% 
      select(-study_site, -id, -subSiteID, -date1, -date2)


  ## Combining class data with CB data

  # Months need to be same data type before rbinding
  fl_combined <- rbind_all(
    list(mutate(firstSampleFilteredData, T1m = as.numeric(T1m), 
                T2m = as.numeric(T2m)), 
         cb_firstSampleFilteredData))

  ############################################################
  #Write the data
  ############################################################

  #Data plan
  #0.1 - First time created
  #0.2 - Dump from data work party in April 2014
  # 0.2-1, data excluded that fails checks
  # 0.2-2, After addition of 18 new studies
  # 0.2-20140505, After addition of 8 new studies
  # 0.3 - data at end of class
  # 0.4 - improved cleaning after class
  #0.5 - All data entered
  #0.9 - All data quality controlled
  #1.0 - Final data for the paper

  fl_combined$id <- 1:nrow(fl_combined)

  # Include aggregation so that we can see if it affects effect size.
  fl_combined %>%
    mutate(sub1 = replace(sub1, is.na(sub1), 1), 
           sub2 = replace(sub2, is.na(sub2), 1)) %>%
    mutate(aggregation = ifelse(sub1 > 1 | sub2 > 1, 'aggregated', 'single plot')) %>% 
    mutate(PlotSize = ifelse(aggregation == 'aggregated', sub1 * PlotSize, PlotSize))

  return(firstLastData)

  write.csv(fl_combined, 'Data_outputs/firstLastData_1.0.csv', row.names = F)

}