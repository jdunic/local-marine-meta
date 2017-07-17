library(dplyr)
library(tidyr)
library(ggplot2)

richData <- read.csv('Data_outputs/cleaned_richData.csv')

sites <- read.csv('master_data/SiteSpatialData.csv') %>% as_data_frame() %>% 
            mutate(Reference = trimws(.$Reference), 
                   Site = trimws(.$Site))

repeats <- read.csv('master_data/repeat_type.csv') %>% as_data_frame()

# Get the full time series of richData after being cleaned


head(richData)

SppR1_vals <-
  with(richData, data_frame(Database = 'Dunic et al'
                            CitationID = Reference, 
                            StudyID = paste(Study.ID, samp_method, sep = '_'), 
                            Site = Site, 
                            Latitude = NA,
                            Longitude = NA,
                            DepthElevation = NA, 
                            Day = NA, 
                            Month = T1m, 
                            Year = T1, 
                            Genus = NA, 
                            Species = NA, 
                            GenusSpecies = NA, 
                            Taxa = taxa, 
                            Organism = Descriptor.of.Taxa.Sampled, 
                            ObsEventID = NA, 
                            ObsID = NA, 
                            RepeatType = 'unknown', 
                            Treatment = 'control', 
                            ScaleValue = PlotSize, 
                            ScaleUnits = PlotSizeUnits, 
                            Driver = Event.type, 
                            BioType = 'marine', 
                            ValueType = 'richness', 
                            Value = SppR1
                            )
)
SppR2_vals <-
  with(richData, data_frame(Database = 'Dunic et al'
                            CitationID = Reference, 
                            StudyID = paste(Study.ID, samp_method, sep = '_'), 
                            Site = Site, 
                            Latitude = NA,
                            Longitude = NA,
                            DepthElevation = NA, 
                            Day = NA, 
                            Month = T2m, 
                            Year = T2, 
                            Genus = NA, 
                            Species = NA, 
                            GenusSpecies = NA, 
                            Taxa = taxa, 
                            Organism = Descriptor.of.Taxa.Sampled, 
                            ObsEventID = NA, 
                            ObsID = NA, 
                            RepeatType = 'unknown', 
                            Treatment = 'control', 
                            ScaleValue = PlotSize, 
                            ScaleUnits = PlotSizeUnits, 
                            Driver = Event.type, 
                            BioType = 'marine', 
                            ValueType = 'richness', 
                            Value = SppR2
                            )
  )
Shan1_vals <-
  with(richData, data_frame(Database = 'Dunic et al'
                            CitationID = Reference, 
                            StudyID = paste(Study.ID, samp_method, sep = '_'), 
                            Site = Site, 
                            Latitude = NA,
                            Longitude = NA,
                            DepthElevation = NA, 
                            Day = NA, 
                            Month = T1m, 
                            Year = T1, 
                            Genus = NA, 
                            Species = NA, 
                            GenusSpecies = NA, 
                            Taxa = taxa, 
                            Organism = Descriptor.of.Taxa.Sampled, 
                            ObsEventID = NA, 
                            ObsID = NA, 
                            RepeatType = 'unknown', 
                            Treatment = 'control', 
                            ScaleValue = PlotSize, 
                            ScaleUnits = PlotSizeUnits, 
                            Driver = Event.type, 
                            BioType = 'marine', 
                            ValueType = 'shannon', 
                            Value = Shan1
                            )
  )
Shan2_vals <-
  with(richData, data_frame(Database = 'Dunic et al'
                            CitationID = Reference, 
                            StudyID = paste(Study.ID, samp_method, sep = '_'), 
                            Site = Site, 
                            Latitude = NA,
                            Longitude = NA,
                            DepthElevation = NA, 
                            Day = NA, 
                            Month = T2m, 
                            Year = T2, 
                            Genus = NA, 
                            Species = NA, 
                            GenusSpecies = NA, 
                            Taxa = taxa, 
                            Organism = Descriptor.of.Taxa.Sampled, 
                            ObsEventID = NA, 
                            ObsID = NA, 
                            RepeatType = 'unknown', 
                            Treatment = 'control', 
                            ScaleValue = PlotSize, 
                            ScaleUnits = PlotSizeUnits, 
                            Driver = Event.type, 
                            BioType = 'marine', 
                            ValueType = 'shannon', 
                            Value= Shan2
                            )
  )

new_df <- 
  rbind_all(list(SppR1_vals, SppR2_vals, Shan1_vals, Shan2_vals)) %>% 
  distinct(StudyID, Site, Month, Year, ValueType) %>% 
  mutate(Study_ref = StudyID) %>% 
  separate(col = Study_ref, into = c('Study.ID', 'method')) %>% 
  mutate(site_id = paste(Study.ID, Site, sep = '_')) %>% 
  select(-Study.ID, -method)



mean_lat_lon <- function(start, end) {
  mean_coord <- mean(c(start, end), na.rm = TRUE)
  if (is.nan(mean_coord)) {
    mean_coord <- NA
  }
  return(mean_coord)
}

single_lat_lon <- 
  sites %>%
    mutate(site_id = paste(Study.ID, Site, sep = '_')) %>% 
    group_by(site_id) %>%
    filter(row_number()==1) %>%
    mutate(mean_lat = mean_lat_lon(Start_Lat, End_Lat), 
           mean_lon = mean_lat_lon(Start_Long, End_Long)) %>% 
    select(-Start_Lat, -Start_Long, -End_Lat, -End_Long, -Shape, -Source, 
           - Checked, -Notes, -Site)

sampling_count <- 
  sites %>% 
    mutate(site_id = paste(Study.ID, Site, sep = '_')) %>% 
    group_by(site_id) %>% 
    summarise(count = n())

single_lat_lon <- 
  left_join(single_lat_lon, sampling_count, by = 'site_id')


# Checking only a single site was selected
ggplot(data=single_lat_lon, aes(x=mean_lon, y=mean_lat))  +
  geom_point(mapping=aes(color=factor(Study.ID))) +
  borders("world", alpha=0.5) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() #+ 
  #geom_text(aes(label = Study.ID), size = 2)

schange_df <- 
  left_join(new_df, single_lat_lon, by = 'site_id') %>% 
    mutate(Latitude = mean_lat, Longitude = mean_lon) %>%
    select(-site_id, -Study.ID, -Reference, -mean_lat, -mean_lon) %>% 
    mutate(ObsEventID = paste(StudyID, Latitude, Longitude, Day, Month, Year, sep = '_')) %>%
    mutate(ObsID = paste(ObsEventID, Site, sep = '_')) %>% 
    mutate(SamplingCount = count) %>% 
    select(-count) %>% 
    filter(!is.na(SamplingCount) & !is.na(Value)) %>% 
    # Add repeat type
    mutate(Study_ref = StudyID) %>% 
    separate(col = Study_ref, into = c('Study.ID', 'method')) %>% 
    select(-method) %>%
    mutate(Study.ID = as.numeric(Study.ID)) %>% 
    left_join(repeats, by = 'Study.ID') %>% 
    mutate(RepeatType = plot.type) %>% 
    select(-Study.ID, -plot.type)

write.csv(schange_df, 'Data_outputs/Dunic_etal_biotime.csv', 
          row.names = FALSE)