########################################################################################
## Methods to Load & Prep Marine Meta-analytic data
## 
## Jarrett Byrnes
## 
## 
## Changelog
##    5/17/2014 - Added Event Sheet with its expected direction
##    5/17/2014 - Added 'mixed inverts' to taxa
##    5/11/2014 - Added event filtering and taxa
##    5/11/2014 - Fixed SE and CI conversions (had them inverted)
##    5/5/2014 - Added error checking code
##    4/20/2014 - Added code to filter to first/last
##    3/30/2014 - Added Log Ratio
##    2/03/2015 - Added conversions to standardize site and plot sizes
########################################################################################
#if (getwd() != '/Users/jillian/R_projects/Meta_analysis_ms') setwd('Meta_analysis_ms')

# libraries for Accessing Data from the web
# and meta-analysis
library(metafor)
library(lubridate)
library(dplyr)
library(tidyr)
library(datamart) # datamart MUST be loaded after dplyr because otherwise 'query'
#is masked by dplyr. I'm not sure that datamart properly uses namespace
# or calls specific functions using package::foo, but I know that dplyr does.
library(stringr)
library(beepr)

source('R/00_functions.R')

###########
## Data Loading and Cleaning
#v##########

# Load the master data!!!!
richData <- read.csv('master_data/Data.csv', stringsAsFactors=FALSE, 
                     na.strings = c('', 'NA', 'N/A', 'Na', 'na', ' '))

# Event types should NOT be 'No'
richData$Event.type[which(richData$Event.type == 'No')] <- NA

# Remove unnecessary columns to make my life easier later
richData <- select(richData, -Lat, -Long, -SiSz, -SiSz..units.)

# Strip trailing whitespace from columns where I think it is going to cause me 
# future pain
richData <- 
  richData %>% 
  mutate(Reference = trimws(.$Reference), 
         Collector = trimws(.$Collector), 
         Descriptor.of.Taxa.Sampled = trimws(.$Descriptor.of.Taxa.Sampled), 
         Sys = trimws(.$Sys), 
         Event.type = trimws(.$Event.type), 
         Year.of.Event = trimws(.$Year.of.Event), 
         Site = trimws(.$Site)) %>% 
  as_data_frame(.)

###########
# Coerce data to correct data types
###########
# First we need to get a lookup table of what all the proper data types should 
# 
meta_data <- read.csv('master_data/Meta_data.csv', stringsAsFactors=FALSE)

# Make meta_data$Column equal the colnames in the master datasheet.
meta_data$Column <- 
  str_replace_all(string = meta_data$Column, pattern = "[ ()?]", replacement = ".")

if (length(setdiff(names(richData), meta_data$Column)) > 0) { 
  stop('There are new columns listed in the master data sheet or meta data sheet thare are unaccounted for. Please update before running this script.')}

# Solution taken from 
# http://stackoverflow.com/questions/7680959/convert-type-of-multiple-columns-of-a-dataframe-at-once
# and http://stackoverflow.com/questions/11261399/function-for-converting-dataframe-column-type
convert.magic <- function(obj, types){
    for (i in 1:length(obj)){
      #browser()
        FUN <- switch(types[i], character = as.character, 
                                   numeric = as.numeric, 
                                   factor = as.factor)
        obj[[i]] <- FUN(obj[[i]])
    }
    return(obj)
}

re_ordered <- meta_data[match(names(richData), meta_data[['Column']]), ]

richData <- convert.magic(richData, types = re_ordered$DataType)

###########
# Add event type categorisation.
###########

# Write a list of the unique event types so that they can be classified
#event_list <- unique(richData$Event.type)
#write.csv(event_list[order(event_list)], 'master_data/event_list.csv')

# Add additional event data
event_data <- read.csv("master_data/Event_types.csv", stringsAsFactors = FALSE)

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

#to not use quotes
qw <- function(...) {
  sapply(match.call()[-1], deparse)
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

#Is there a taxonomic type?
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


# Using the new date functions in 00_functions.R
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

# Complete the creation of all the first and last data
source('R/cb_data_processing.R')

write.csv(richData, 'Data_outputs/cleaned_richData.csv')
