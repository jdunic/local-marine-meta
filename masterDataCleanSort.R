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

# libraries for Accessing Data from the web
# and meta-analysis
library(metafor)
#library(plyr)
library(reshape2)
library(lubridate)
library(dplyr)
library(tidyr)
library(datamart) # datamart MUST be loaded after dplyr because otherwise 'query' is masked by dplyr. I'm not sure that datamart properly uses namespace
# or calls specific functions using package::foo, but I know that dplyr does.
library(readr)
library(googleVis)
library(stringr)
library(beepr)
library(googlesheets)

setwd('Meta_analysis_ms')

###########
## Data Loading and Cleaning
#v##########

# Load the master data!!!!
richData <- read.csv('master_data/Data.csv', stringsAsFactors=FALSE)

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

re_ordered %>% select(Column, DataType)

richData <- convert.magic(richData, types = re_ordered$DataType)


###########
# Add event type categorisation.
###########

# Removing this for now to try and figure out what the fix that we're waiting 
# for actually is...
#richData <- richData[-which(richData$Study.ID==47),] #waiting on Pat's fix

# Add additional event data
event_types <- gs_title("Event Types")
eventData <- gs_read(ss = event_types)

# Random cleanup... it's not 'No'! It's NA! There is no value!
richData[which(richData$Event.type == 'No'), 'Event.type'] <- NA
richData[which(richData$Event.type == ''), 'Event.type'] <- NA

richData <- left_join(richData, eventData)

# Check that the event data spreadsheet is up to date:
extra_events <- setdiff(richData$Event.type, eventData$Event.type)
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
source("datamart_UnitSetManager2_source.r")
source("datamart_mashup_source.r")

# Standardize unit names
richData$SiSz..units. <- tolower(as.character(richData$SiSz..units.))
richData$PltSz..units. <- tolower(as.character(richData$PltSz..units.))

unique(richData$SiSz..units.)
unique(richData$PltSz..units.)

richData$SiSz..units. <- gsub('acres', 'ac', richData$SiSz..units.)
richData$PltSz..units. <- gsub('acres', 'ac', richData$PltSz..units.)
richData$SiSz..units. <- gsub('km2', 'km^2', richData$SiSz..units.)
richData$PltSz..units. <- gsub('km2', 'km^2', richData$PltSz..units.)
richData$SiSz..units. <- gsub('m2', 'm^2', richData$SiSz..units.)
richData$PltSz..units. <- gsub('m2', 'm^2', richData$PltSz..units.)
richData$SiSz..units. <- gsub('m3', 'm^3', richData$SiSz..units.)
richData$PltSz..units. <- gsub('m3', 'm^3', richData$PltSz..units.)
print('Double check that \'nm\' is nautical miles if you run this script')
richData$SiSz..units. <- gsub('^nm', 'nmi', richData$SiSz..units.)
richData$PltSz..units. <- gsub('^nm[^i]', 'nmi', richData$PltSz..units.)
richData$SiSz..units. <- gsub('liters', 'l', richData$SiSz..units.)
richData$PltSz..units. <- gsub('liters', 'l', richData$PltSz..units.)

# Replace with NA where appropriate
richData$SiSz..units.[which(richData$SiSz..units. == 'n/a')] <- NA
richData$SiSz..units.[which(richData$SiSz..units. == '')] <- NA
richData$PltSz..units.[which(richData$PltSz..units. == 'unspecified')] <- NA

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

#unique(richData$SiSz..units.)
site_size_conversion <- 
  richData %>% 
    rowwise() %>%
        do(convert_units(value = .$SiSz, unit = .$SiSz..units.))
names(site_size_conversion) <- c('SiteSize', 'SiteSizeUnits')
richData <- bind_cols(richData, site_size_conversion)

# works!
plot_size_conversion <- 
  richData %>% 
    rowwise() %>%
        do(convert_units(value = .$PltSz, unit = .$PltSz..units.)) 

names(plot_size_conversion) <- c('PlotSize', 'PlotSizeUnits')
richData <- tbl_df(bind_cols(richData, plot_size_conversion))


#colnames(richData)[colnames(richData)=="new_val"] <- "SiteSize"
#colnames(richData)[colnames(richData)=="new_unit"] <- "SiteSizeUnits"

#richData <- adply(richData, 1, function(x) convert_units(x$PltSz, x$PltSz..units.))
#colnames(richData)[colnames(richData)=="new_val"] <- "PlotSize"
#colnames(richData)[colnames(richData)=="new_unit"] <- "PlotSizeUnits"

#unique(richData[, c('SiteSizeUnits', 'PlotSizeUnits')], na.rm = TRUE)

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

missing <- select(noSample, Study.ID, Reference, Collector)
if (is.data.frame(missing)) {
  missing$Note <- 'This sample is missing a sampling method'
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

###########
## Error checking
###########

varToCheck <- c("n1", "n2", "T1", "T2", "PltSz", "Sys", 
                "Descriptor.of.Taxa.Sampled", 
                #"Event.", 
                #"A.priori.", 
                "Lat", "Long", "Loc", "Site", 
                "Vis", "Trwl", "Line", "Drdg", "Trp",
                "coral", "plant", "algae", 
           "fish", "inverts", "mobile.inverts", "sessile.inverts", "marine.mammals", 
           "phytoplankton", "zooplankton")

# Catch missing values
for(j in varToCheck){
  badidx <- which(is.na(richData[[j]]))
  print(j)
  if(length(badidx) > 0){
    print(
    richData[badidx, ] %>%
      select(one_of('Study.ID', 'Reference', 'Collector', j))
      )
    #print(ddply(richData[badidx, ], .(Study.ID, Reference, Collector, j), summarise, entries = length(n1)))
  } else {
      print("Clear!")
  }
  cat("\n")
}

# Need to figure out why this is NA. It isn't in the data sheet....
richData[which(richData$Study.ID == 446), 'Vis'] <- 0

# Check that if the lat/long are missing from the master data sheet, that there 
# is at least one lat long associated with the data in the SpatialData 
# spreadsheet.
spatial <- read.csv('master_data/SiteSpatialData.csv', stringsAsFactors=FALSE)

spatial_check <- 
  mutate(richData, spatial_data = ifelse(Study.ID %in% unique(spatial$Study.ID), 
                                         'yes', 'no'))

spatial_check %>%
    filter(spatial_data == 'no') %>%
      group_by(Study.ID) %>% 
      select(Study.ID, Collector, Site)


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

# What is going on here?
# Where did these studies go? They are marked as being completed but they are 
# not found in the master dataset.
xdf <- subset(richData, richData$Study.ID %in% c(691, 693, 3, 19, 741, 59, 134, 89, 705, 51))
xdf$Site <- factor(xdf$Site)
adf <- subset(xdf, xdf$Site==xdf$Site[1])

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

richData[which(richData$Study.ID == 453), 'T1m'] <- NA
richData[which(richData$Study.ID == 453), 'T2m'] <- NA

richData$date1 <- make_date_col(year = richData$T1, month = richData$T1m)
richData$date2 <- make_date_col(year = richData$T2, month = richData$T2m)


# Using the new date functions in 02_functions.R
firstSampleFilteredData <- 
  richData %>% 
    group_by(Study.ID, Reference, Sys, taxa, Vis, Trwl, Line, Drdg, Trp, 
             Descriptor.of.Taxa.Sampled, Loc, Site) %>%
    do(get_first_last(.))

# Add duration to the data frame
firstSampleFilteredData$Duration <- with(firstSampleFilteredData, T2-T1)


############
#calculate a lot of effect sizes
############
measurements <- c("SppR", "Shan", "Even")#, "Simps")
effects <- c("MD", "SMD", "ROM", "SMDH")

for(j in measurements){
  for(i in effects){
    cat(paste(i,j,sep=","))
    cat("\n")
    v1 = firstSampleFilteredData[[paste0(j,1)]]
    v2 = firstSampleFilteredData[[paste0(j,2)]]
    v1sd = firstSampleFilteredData[[paste(paste0(j,1),"SD",sep=".")]]
    v2sd = firstSampleFilteredData[[paste(paste0(j,2),"SD",sep=".")]]
    var.names = paste(c("yi", "vi"), j, i, sep=".")
    firstSampleFilteredData <- escalc(i, m1i=v2, sd1i = v2sd, n1i = n2, 
                      m2i=v1, sd2i = v1sd, n2i = n1, data=firstSampleFilteredData,
                      append=T, var.names=var.names)
  
 }
}

# Do the same for the full dataset
for(j in measurements){
  for(i in effects){
    cat(paste(i,j,sep=","))
    cat("\n")
    v1 = richData[[paste0(j,1)]]
    v2 = richData[[paste0(j,2)]]
    v1sd = richData[[paste(paste0(j,1),"SD",sep=".")]]
    v2sd = richData[[paste(paste0(j,2),"SD",sep=".")]]
    var.names = paste(c("yi", "vi"), j, i, sep=".")
    richData <- escalc(i, m1i=v2, sd1i = v2sd, n1i = n2, 
                      m2i=v1, sd2i = v1sd, n2i = n1, data=richData,
                      append=T, var.names=var.names)  
 }
}

# To make life easier when counting studies, create boolean field that flags:
# - richness ROMs (with and without variance)
# - richness ROMs (with variance) 
# - shannon ROMs (with and without variance)
# - shannon ROMs (with variance)
# - richness SMD
# - shannon SMD

filter(firstSampleFilteredData, yi.SppR1.SD == 0 | SppR2.SD == 0)

# Have no SMD calculated for them. I don't know why. I'm not sure what is different
# in these three studies than all the others...
no_SMD <- firstSampleFilteredData[which(firstSampleFilteredData$Study.ID %in% c(47, 150, 363)), ]

gvt <- gvisTable(firstSampleFilteredData[which(firstSampleFilteredData$Study.ID %in% c(47, 150, 363)), c('n1', 'n2', 'SppR1', 'SppR2', 'SppR1.SD', 'SppR2.SD', 'vi.SppR.ROM', 'vi.SppR.SMD') ])
plot(gvt)

firstSampleFilteredData$rich_ROM_uw <- as.numeric(!is.na(firstSampleFilteredData$yi.SppR.ROM))
firstSampleFilteredData$rich_ROM_w <- as.numeric(!is.na(firstSampleFilteredData$vi.SppR.ROM))

firstSampleFilteredData$shan_ROM_uw <- as.numeric(!is.na(firstSampleFilteredData$yi.Shan.ROM))
firstSampleFilteredData$shan_ROM_w <- as.numeric(!is.na(firstSampleFilteredData$vi.Shan.ROM))

firstSampleFilteredData$rich_SMD_uw <- as.numeric(!is.na(firstSampleFilteredData$yi.SppR.SMD))
firstSampleFilteredData$rich_SMD_w <- as.numeric(!is.na(firstSampleFilteredData$vi.SppR.SMD))

firstSampleFilteredData$shan_SMD_uw <- as.numeric(!is.na(firstSampleFilteredData$yi.Shan.SMD))
firstSampleFilteredData$shan_SMD_w <- as.numeric(!is.na(firstSampleFilteredData$vi.Shan.SMD))



# Need to figure out how to deal with 0 values for the log ratio...
rich_zero_samples <- 
firstSampleFilteredData[which(firstSampleFilteredData$SppR1 == 0 | 
                              firstSampleFilteredData$SppR2 == 0), ]
beep()

#####################
# Reformat for timeseries analysis
#####################
zdf <- subset(richData, richData$Study.ID=="423" & richData$Site=="Venados Island")

timeData <- ddply(richData, .(Study.ID, Reference, Sys, coral, plant, algae, fish, inverts, mobile.inverts,
                              sessile.inverts, marine.mammals, phytoplankton, zooplankton, 
                              Vis, Trwl, Line, Drdg, Trp,
                              Descriptor.of.Taxa.Sampled,
                              Loc, Site), function(adf){
                               # adf <- zdf
                                if(is.na(adf$T1m)[1]) adf$T1m <- 6 #July, for the heck of it
                                if(is.na(adf$T2m)[1]) adf$T2m <- 6 #July, for the heck of it
                                rowDates <- parse_date_time(paste(adf$T1m, "1", adf$T1, sep="-"), 
                                                            order="%m-%d-%y")
                                # print( adf$T1m )
                                # print( adf$T1 )
                                
                                # print( adf$Date )
                                #cat(adf$Study.ID[1])
                                #cat(", ")
                                #cat(as.character(adf$Site[1]))
                                
                                 #cat("\n")
                                #add a new row and move the final info to it
                                last <-  order(rowDates)[nrow(adf)]
                                adf <- rbind(adf, adf[last,])
                                adf$len <- nrow(adf)
                                
                                m <- c("SppR", "Shan", "Even")#, "Simps")
                                m1 <- which(names(adf) %in% paste0(m, 1))
                                m2 <- which(names(adf) %in% paste0(m, 2))
                                
                                s1 <- which(names(adf) %in% paste0(m, 1, ".SD"))
                                s2 <- which(names(adf) %in% paste0(m, 2, ".SD"))
                                adf[nrow(adf),c(m1, s1)] <- adf[last,c(m2, s2)]
                                
                                e1 <- which(names(adf) %in% paste0(m, 1, ".Error"))
                                e2 <- which(names(adf) %in% paste0(m, 2, ".Error"))
                                et1 <- which(names(adf) %in% paste0(m, 1, ".Error.Type"))
                                et2 <- which(names(adf) %in% paste0(m, 2, ".Error.Type"))
                                adf[nrow(adf),c(e1, et1)] <- adf[last,c(e2, et2)]
                                
                                adf$T1[nrow(adf)] <- adf$T2[last] 
                                adf$T1m[nrow(adf)] <- adf$T2m[last] 
                                
                                
                                names(adf)[m1] <- gsub("1", "", names(adf)[m1]) 
                                names(adf)[s1] <- gsub("1", "", names(adf)[s1]) 
                                adf <-  adf[,-c(m2, s2, e2, et2)]
                                adf <-  adf[,-which(names(adf) %in% c("T2", "T2m"))]
                                
                                adf$temporal_std_r <- (adf$SppR-mean(adf$SppR, na.rm=T))/sd(adf$SppR, na.rm=T)
                                
                                adf$Duration <- max(adf$T1) - min(adf$T1)
                                adf
                                
                              }
)

timeData <- timeData[,-(agrep("Simps", names(timeData)))]
#qplot(Date, SppR1, data=timeData, color=paste(Reference,Site), facets=~Taxa)
#qplot(Date, temporal_std_r, data=timeData, color=paste(Reference,Site), facets=~Taxa)



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
firstSampleFilteredData$Event. <- as.character(firstSampleFilteredData$Event.)
firstSampleFilteredData$Event. <- gsub("protection", "Yes", firstSampleFilteredData$Event.) #garumph
eventIDX <- which(firstSampleFilteredData$Event. == "Yes")
timeEventIDX <- which(timeData$Event. == "Yes")


ver <- 0.4
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0("_v",ver,"-",outdate,".csv")
write.csv(firstSampleFilteredData, paste0('Data/firstLastData', trailer), row.names = F)
write.csv(firstSampleFilteredData[-eventIDX,], paste0("Data/firstLastData_noevent",trailer), row.names=F)
write.csv(firstSampleFilteredData[eventIDX,], paste0("Data/firstLastData_event",trailer), row.names=F)
write.csv(timeData[-timeEventIDX,], paste0("Data/timeseriesData_noevent",trailer), row.names=F)
write.csv(timeData[timeEventIDX,], paste0("Data/timeseriesData_event",trailer), row.names=F)

write.csv(firstSampleFilteredData, paste0("Data/fullData",trailer), row.names=F)



############################################################
# Get standard deviation of time series residuals for each 
############################################################
# Using these values for weighting assumes that the change in species richness
# is a linear relationship.

names(richData)

names(richData)[which(names(richData) == 'Event.type.2')] <- 'EventType'

y1Cols <- grep("1", names(richData))
y2Cols <- grep("2", names(richData))

y1y2_names <- names(richData[, c(y1Cols, y2Cols)])

gen_Cols <- richData[, which(!names(richData) %in% y1y2_names)]

y1_data <- cbind(gen_Cols, richData[, y1Cols])
y2_data <- cbind(gen_Cols, richData[, y2Cols])
names(y2_data) <- names(y1_data)

# Get data frame where each row is a single observation of richness
ts_data <- rbind(y1_data, y2_data)

# Get rid of duplicate data
ts_data <- unique(ts_data)

# For each study-site-taxa-method-location get all the yearly points that have 
# the same month
test <- ddply(ts_data, .(Study.ID, Reference, Sys, 
                         coral, plant, algae, fish, inverts, mobile.inverts,
                         sessile.inverts, marine.mammals, phytoplankton, zooplankton, 
                         Vis, Trwl, Line, Drdg, Trp,
                         Descriptor.of.Taxa.Sampled,
                         Loc, Site), 
              function(x) {
                info <- paste('Study:', x$Study.ID, 'Site:', x$Site, 'Date:', x$date1, sep = ' ')
                print()
              }
)


############################################################
#for funsies
############################################################
library(ggplot2)
qplot(T1, SppR, data=timeData, color=paste(Study.ID, Site, sep=","), geom="line") +
  facet_wrap(~Taxa) + 
  theme_classic(base_size=18) +
  xlab("\nYear") + ylab("Species Richness\n")

hist(firstSampleFilteredData$Duration, main="Study Duration", xlab="Years")

qplot(Taxa, Duration, data=firstSampleFilteredData, geom="boxplot", fill=Taxa) +
  theme_bw(base_size=20)

##############
plot(SppR1~SppR2, data=firstSampleFilteredData[-eventIDX,])
abline(lm(SppR2~SppR1, data=firstSampleFilteredData), col="red", lty=2)

plot(SppR1~SppR2, data=firstSampleFilteredData[eventIDX,])
abline(lm(SppR2~SppR1, data=firstSampleFilteredData), col="red", lty=2)

plot(log(SppR2)-log(SppR1) ~ I(log(Duration)), data=firstSampleFilteredData)

zidx <- which(firstSampleFilteredData$SppR2==0 | firstSampleFilteredData$SppR1==0)
abline(lm(log(SppR2)-log(SppR1) ~ I(log(Duration)), data=firstSampleFilteredData[-zidx,]), col="red", lty=2)
summary(lm(log(SppR2)-log(SppR1) ~ I(log(Duration)), data=firstSampleFilteredData[-zidx,]), col="red", lty=2)
hist(firstSampleFilteredData$Duration, breaks=50)


########Just playing around a bit
newRich <-firstSampleFilteredData[-eventIDX,]
eventNewRich <-firstSampleFilteredData[eventIDX,]

rma(yi.SppR.ROM, vi.SppR.ROM, data=newRich)
rma(yi.SppR.ROM~Duration, vi.SppR.ROM, data=newRich)
rma(yi.SppR.SMD, vi.SppR.SMD, data=newRich)
rma(yi.SppR.SMD~I(log(Duration)), vi.SppR.SMD, data=newRich)
rma(yi.SppR.SMD~Duration, vi.SppR.SMD, data=eventNewRich)
plot(yi.SppR.SMD~Duration,  data=eventNewRich)

library(ggplot2)
ggplot(data=eventNewRich, mapping=aes(y=yi.SppR.SMD, x=Duration,
                                      ymin=yi.SppR.SMD-sqrt(yi.SppR.SMD),
                                      ymax=yi.SppR.SMD+sqrt(yi.SppR.SMD)))+
  geom_pointrange(mapping=aes(color=Expected.Change.Direction))

ggplot(data=eventNewRich, mapping=aes(y=yi.SppR.ROM, x=Event.type.2,
                                      ymin=yi.SppR.SMD-sqrt(yi.SppR.SMD),
                                      ymax=yi.SppR.SMD+sqrt(yi.SppR.SMD)))+
  geom_pointrange(mapping=aes(color=Expected.Change.Direction))


ggplot(data=eventNewRich, mapping=aes(y=yi.SppR.ROM, x=Duration))+
         geom_point(mapping=aes(color=Event.type.2, shape=Expected.Change.Direction)) +
         facet_wrap(~Expected.Change.Direction, scale="free_x") +
         stat_smooth(method="lm") +
  theme_bw(base_size=18)

#newRichM <- newRich[-which(is.na(newRich$Site)),]
rma.mv(yi.SMD~Duration, vi.SMD, random = list(~1|Site, ~1|Study.ID), data=newRich)
summary(lm(yi.MD ~ Duration, data=newRich))
summary(lm(yi.ROM ~ I(log(Duration)), data=newRich))
plot(yi.ROM ~ I(log(Duration)), data=newRich)
abline(lm(yi.ROM ~ I(log(Duration)), data=newRich), lty=2, col="red")

library(ggplot2)
qplot(log(Duration), yi.ROM, data=newRich, facets=~Sys) +
  stat_smooth(method="lm")

qplot(T1, SppR, data=full, color=I("grey"),  group=Study.ID, geom="point") + stat_smooth(method="lm", fill=NA, mapping=aes(color=factor(Study.ID)))
