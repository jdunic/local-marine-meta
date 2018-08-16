library(readxl)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Kilfoyle et al 2013
#-------------------------------------------------------------------------------
fish <- read_excel("~/Desktop/raw_species_papers/67_2013_Kilfoyle_etal/67_2013_Kilfoyle_etal_Table1.xlsx", 
    sheet = 'CIEE_data')

new_names <- vector('character', length = length(names(fish)))
new_names[1:2] <- c('common_name', 'scientific_name')
for (i in 3:length(names(fish))) {
  new_names[i] <- paste(names(fish)[i], fish[1, i], fish[2, i], sep = '.')
}

names(fish) <- new_names

raw_abund_N <- 
fish[-c(1, 2), ] %>%
  gather(key, value, -common_name, -scientific_name) %>%
  extract(key, c('year', 'site', 'measurement'), "(^\\d*)\\.(N|B)\\.(.*)") %>%
  spread(measurement, value) %>%
  filter(scientific_name != 'Actual total abundance' & 
         scientific_name != 'SIMPER  %difference'& 
         scientific_name != 'Total abundance' & 
         scientific_name != 'Total species') %>%
  separate(scientific_name, into = c('Genus', 'species'), sep = ' ') %>%
  filter(site == 'N') %>%
  arrange(year) %>%
  select(year, Genus, species, site, `T = abundance`)

write.csv(raw_abund_N, '~/Desktop/raw_species_papers/67_2013_Kilfoyle_etal/CIEE_data_natural_reef.csv')

# Artificial boulder reef)
raw_abund_B <- 
fish[-c(1, 2), ] %>%
  gather(key, value, -common_name, -scientific_name) %>%
  extract(key, c('year', 'site', 'measurement'), "(^\\d*)\\.(N|B)\\.(.*)") %>%
  spread(measurement, value) %>%
  filter(scientific_name != 'Actual total abundance' & 
         scientific_name != 'SIMPER  %difference'& 
         scientific_name != 'Total abundance' & 
         scientific_name != 'Total species') %>%
  separate(scientific_name, into = c('Genus', 'species'), sep = ' ') %>%
  filter(site == 'B') %>%
  arrange(year) %>%
  select(year, Genus, species, site, `T = abundance`)

write.csv(raw_abund_B, '~/Desktop/raw_species_papers/67_2013_Kilfoyle_etal/CIEE_data_artificial_boulder_reef.csv')

#-------------------------------------------------------------------------------
# Burrows et al 2012
#-------------------------------------------------------------------------------
whales <- read_excel("~/Desktop/raw_species_papers/193_Burrows_etal_2012/193_Burrows_etal_2012_Table1.xlsx", 
    sheet = 1)

new_names <- vector('character', length = length(names(whales)))
new_names[1:3] <- c('Genus', 'species', 'common_name')
for (i in 4:length(names(whales))) {
  new_names[i] <- paste(names(whales)[i], whales[1, i], sep = '.')
}

names(whales) <- new_names

whales_abund <- 
whales[-c(1, 24:39), ] %>%
  gather(key, value, -Genus, -species, -common_name) %>%
  extract(key, c('year', 'measurement'), "(^\\d*)\\.(.*)") %>%
  spread(measurement, value) %>%
  arrange(year) %>%
  select(year, Genus, species, Abundance)

write.csv(whales_abund, '~/Desktop/raw_species_papers/193_Burrows_etal_2012/CIEE_data_marine_mammals.csv')

#-------------------------------------------------------------------------------
# Yildiz and Feyzioglu 2014
#-------------------------------------------------------------------------------

plankton <- read_excel('~/Desktop/raw_species_papers/Yildiz and Feyzioglu (2014)/Yildiz and Feyzioglu (2014)-Raw Data.xlsx', 
    sheet = 'CIEE_data')

plank_abund <- 
plankton %>%
  gather(year, mean_abund_per_m2, -Phylum, -Order, -Species) %>%
  separate(Species, into = c('Genus', 'species'), sep = ' ') %>%
  select(year, Genus, species, mean_abund_per_m2)

write.csv(plank_abund, '~/Desktop/raw_species_papers/Yildiz and Feyzioglu (2014)/CIEE_data_plankton.csv')


  fish[-c(1, 2), ] %>%
  gather(key, value, -common_name, -scientific_name) %>%
  extract(key, c('year', 'site', 'measurement'), "(^\\d*)\\.(N|B)\\.(.*)") %>%
  spread(measurement, value) %>%
  filter(scientific_name != 'Actual total abundance' & 
         scientific_name != 'SIMPER  %difference'& 
         scientific_name != 'Total abundance' & 
         scientific_name != 'Total species') %>%
  separate(scientific_name, into = c('Genus', 'species'), sep = ' ') %>%
  filter(site == 'B') %>%
  arrange(year) %>%
  select(year, Genus, species, site, `T = abundance`)