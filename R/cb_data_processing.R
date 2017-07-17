library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

subset_list <- read_csv('master_data/CB_study_list_for_reformat.csv')

# removing -1 because of this bug: https://github.com/hadley/dplyr/issues/1576
cbdata <- read_csv('master_data/robin_bts_reformat.csv')[, -1]

cbdata <- 
  cbdata %>% 
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


# Include event data
event_data <- read.csv("master_data/Event_types.csv", stringsAsFactors = FALSE)

event_data <- 
  event_data %>% 
  mutate(event_category = trimws(.$event_category), 
         event_desc = trimws(.$event_desc), 
         expected_change = trimws(.$expected_change), 
         event_notes = trimws(.$event_notes))

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

# Using the new date functions in 00_functions.R
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

# Months need to be same data type before rbinding
fl_combined <- rbind_all(
  list(mutate(firstSampleFilteredData, T1m = as.numeric(T1m), 
              T2m = as.numeric(T2m)), 
       cb_firstSampleFilteredData))

# Get spatial data from cb data

cb_site_data <- select(cb_firstSampleFilteredData, Study.ID, Reference, Site, 
                       Lat, Long)

write.csv(cb_site_data, 'Data_outputs/cb_site_data.csv')

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

# Include aggregation so taht we can see if it affects effect size.
fl_combined %>%
  mutate(sub1 = replace(sub1, is.na(sub1), 1), 
         sub2 = replace(sub2, is.na(sub2), 1)) %>%
  mutate(aggregation = ifelse(sub1 > 1 | sub2 > 1, 'aggregated', 'single plot')) %>% 
  mutate(PlotSize = ifelse(aggregation == 'aggregated', sub1 * PlotSize, PlotSize))


ver <- 0.9
outdate <- as.character(format(Sys.Date(), format="%Y%m%d"))
trailer <- paste0("_v",ver,"-",outdate,".csv")
write.csv(fl_combined, paste0('Data_outputs/firstLastData', trailer), row.names = F)
