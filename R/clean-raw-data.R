# Clean raw species data
#options(help_type="text")
#options(help_type="html")

library(tidyverse)

yildiz <- 
  read_csv('../master_data/raw_species_papers/6-Yildiz-and-Feyzioglu-2014.csv') %>% 
  gather(key = year, value = abundance, -Species) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

felix  <- 
  read_csv('../master_data/raw_species_papers/34_Felix-Hackradt_etal_2013.csv', skip = 2) %>% 
  filter(!str_detect(Species, c('abundance', 'richness'))) %>% 
  gather(key = key, value = abundance, -Species) %>% 
  mutate(abundance = replace_na(abundance, 0)) %>% 
  separate(key, into = c('group', 'year')) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


kilfoyle <- 
  read_csv('../master_data/raw_species_papers/67_2013_Kilfoyle_etal-tabula-Table1.csv') %>% 
  filter(!str_detect(`Scientific name`, c('abundance', 'species', 'SIMPER')) | 
         !str_detect(`Common Name`, 'Common Name')) %>% 
  gather(key = key, value = abundance, -`Common Name`, -`Scientific name`) %>%
  separate(abundance, into = c('abundance', 'occurrence'), sep = '/') %>% 
  select(-occurrence) %>% 
  mutate(abundance = replace_na(abundance, 0)) %>%
  mutate(abundance = as.numeric(abundance)) %>% 
  separate(key, into = c('year', 'group')) %>% 
  select(-`Common Name`) %>% 
  spread(key = `Scientific name`, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

diez <- 
  read_csv('../master_data/raw_species_papers/181_Diez_etal_2012-tabula-TableA2.csv', na = 'e') %>% 
  gather(key = year, value = 'percent_cover-frequency', -`Taxa list`) %>% 
  separate(year, into = c('year', 'measurement')) %>% 
  spread(key = measurement, value = `percent_cover-frequency`) %>% 
  mutate(cover = replace_na(C, 0)) %>% 
  # Did contain standard error
  select(-F, -SE, -C) %>% 
  extract(`Taxa list`, 'taxa', "([a-zA-Z]*\\s[a-zA-Z]*)") %>% 
  spread(key = taxa, value = cover) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

burrows <-   
  read_csv('../master_data/raw_species_papers/193_Burrows_etal_2012_Table1.csv') %>% 
  select(-X1) %>% 
  unite(col = taxa, Genus, species, sep = ' ') %>% 
  spread(key = taxa, value = Abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


ibarra_names <- 
  read_csv('../master_data/raw_species_papers/300_Ibarra_Obando_etal-cover-table.csv', col_names = FALSE) %>% 
  slice(1:2) %>% 
  mutate_all(funs(paste(.[[1]], .[[2]], sep = '-'))) %>% 
  slice(1) %>% 
  unlist()
#
ibarra <- 
  read_csv('../master_data/raw_species_papers/300_Ibarra_Obando_etal-cover-table.csv', col_names = ibarra_names) %>% slice(-(1:3)) %>% 
  rename(date = `Sampling date-NA`) %>% 
  gather(key = `species-group`, value = abundance, -date) %>% 
  separate(col = `species-group`, into = c('species', 'group'), sep = '-') %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  spread(key = species, value = abundance) %>% 
  extract(col = date, into = c('season', 'year'), regex = '(\\D)(\\d*)') %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  mutate(century = ifelse(year < 85, 2000, 1900)) %>% 
  mutate(year = century + year) %>% 
  select(-century) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


primo_names <- 
  read_csv('../master_data/raw_species_papers/360_Primo_etal_2009.csv', col_names = FALSE) %>% 
  slice(1:2) %>% 
  mutate_all(funs(paste(.[[1]], .[[2]], sep = '-'))) %>% 
  slice(1) %>% 
  unlist()
#
primo <- 
  read_csv('../master_data/raw_species_papers/360_Primo_etal_2009.csv', col_names = primo_names) %>% 
  slice(-(1:2)) %>% 
  rename(species = `Year-Season`) %>% 
  gather(key = `year-season`, value = abundance, -species) %>%
  mutate(abundance = as.numeric(abundance)) %>% 
  separate(col = `year-season`, into = c('year', 'season'), sep = '-') %>% 
  spread(key = species, value = abundance)  %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


james_large_seine <- 
  read_csv('../master_data/raw_species_papers/440_James_etal_2008-large-seine.csv') %>% 
  gather(key = year, value = abundance, -Species) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

james_small_seine <- 
  read_csv('../master_data/raw_species_papers/440_James_etal_2008-small-seine.csv') %>% 
  gather(key = year, value = abundance, -Species) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

james_gillnet <- 
  read_csv('../master_data/raw_species_papers/440_James_etal_2008-gillnet.csv') %>% 
  gather(key = year, value = abundance, -Species) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


ohgaki <- 
  read_csv('../master_data/raw_species_papers/551_Ohgaki-and_Kosuge-2004.csv') %>% 
  filter(!is.na(`1989`)) %>% 
  gather(key = year, value = abundance, -Species) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

methven <- 
  read_csv('../master_data/raw_species_papers/595_Methven_etal_2001.csv') %>% 
  filter(!is.na(`1982-Day`)) %>% 
  gather(key = `year-group`, value = abundance, -Species, -`Common name`) %>% 
  separate(col = `year-group`, into = c('year', 'group'), sep = '-') %>% 
  select(-`Common name`) %>% 
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

noe_names <- 
  read_csv('../master_data/raw_species_papers/599_Noe_and_Zedler_2001.csv', col_names = FALSE) %>% 
  slice(1:2) %>% 
  mutate_all(funs(paste(.[[1]], .[[2]], sep = '-'))) %>% 
  slice(1) %>% 
  unlist()
#
noe <-   
  read_csv('../master_data/raw_species_papers/599_Noe_and_Zedler_2001.csv', col_names = noe_names) %>% 
  rename(species = `Species-NA`) %>% 
  slice(-(1:2)) %>% 
  gather(key = site_time, value = abundance, -1) %>% 
  extract(abundance, into = c('abundance', 'se'), "(\\d*)\\s\\((\\d*)\\)") %>%
  mutate(abundance = replace_na(abundance, 0)) %>% 
  select(-se) %>% 
  spread(key = site_time, value = abundance) %>% 
  gather(key = `year-site`, value = abundance, -species) %>% 
  extract(col = `year-site`, into = c('year', 'site'), regex = '(\\d{4})-(.*)') %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  spread(key = species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


lazzari_names <- 
  read_csv('../master_data/raw_species_papers/620_Lazzari_etal_1999_Table1.csv', col_names = FALSE) %>% 
  slice(1:2) %>% 
  mutate_all(funs(paste(.[[1]], .[[2]], sep = '-'))) %>% 
  slice(1) %>% 
  unlist()
#
lazzari <- 
  read_csv('../master_data/raw_species_papers/620_Lazzari_etal_1999_Table1.csv', col_names = lazzari_names) %>% 
  rename(species = `NA-species`) %>% 
  slice(-c(1, 2, n())) %>% 
  gather(key = `site-year`, value = abundance, -species) %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  separate(col = `site-year`, into = c('site', 'year'), sep = '-') %>% 
  spread(key = species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)


williams_names <- 
  read_csv('../master_data/raw_species_papers/621_Williams_and_Zedler_1999.csv', col_names = FALSE) %>%
  slice(1:2) %>% 
  mutate_all(funs(paste(.[[1]], .[[2]], sep = '-'))) %>% 
  slice(1) %>% 
  unlist()
#
williams <- 
# Not sure what to do about <0.01 values. So far have replaced with 0.01
  read_csv('../master_data/raw_species_papers/621_Williams_and_Zedler_1999.csv', col_names = williams_names) %>%
  rename(species = `NA-Species`) %>% 
  slice(-(1:2)) %>%
  select(-`NA-Common Name`) %>% 
  gather(key = site_time, value = density, -species) %>% 
  mutate(density = replace_na(density, 0), 
         density = replace(density, density == '< 0.01', '0.01'), 
         density = str_replace(density, '\\s', '')) %>% 
  mutate(density = as.numeric(density)) %>% 
  spread(key = species, value = density) %>% 
  separate(site_time, into = c('site', 'year'), sep = '-') %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)



gibson_names <- 
  read_csv('../master_data/raw_species_papers/660_Gibson_etal_1993-trawl.csv', col_names = FALSE) %>% 
  slice(1:2) %>% 
  mutate_all(funs(paste(.[[1]], .[[2]], sep = '-'))) %>% 
  slice(1) %>% 
  unlist()

gibson_trawl <- 
  read_csv('../master_data/raw_species_papers/660_Gibson_etal_1993-trawl.csv', col_names = gibson_names, na = c('', '-')) %>% 
  slice(-(1:2)) %>% 
  rename(species = `Species-NA`) %>% 
  gather(key = year, value = abundance, -species) %>% 
  separate(col = `year`, into = c('year', 'metric'), sep = '-') %>% 
  filter(metric == 'N') %>% 
  select(-metric) %>% 
  mutate(abundance = as.numeric(abundance), 
         abundance = replace_na(abundance, 0)) %>% 
  filter(!species %in% c('Total species', 'Total numbers', 
                         'Total weight ( g )', 'Number of  samples')) %>% 
  spread(key = species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

gibson_seine <- 
  read_csv('../master_data/raw_species_papers/660_Gibson_etal_1993-seine.csv', col_names = gibson_names, na = c('', '-')) %>% 
  slice(-(1:2)) %>% 
  rename(species = `Species-NA`) %>% 
  gather(key = year, value = abundance, -species) %>% 
  separate(col = `year`, into = c('year', 'metric'), sep = '-') %>% 
  filter(metric == 'N') %>% 
  select(-metric) %>% 
  mutate(abundance = as.numeric(abundance), 
         abundance = replace_na(abundance, 0)) %>% 
  filter(!species %in% c('Total species', 'Total numbers', 
                         'Total weight ( g )', 'Number of  samples')) %>%
  spread(key = species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

gibson_trawl_crust <- 
  read_csv('../master_data/raw_species_papers/660_Gibson_etal_1993-crust-trawl.csv', na = c('', '-')) %>% 
  gather(key = year, value = abundance, -Species) %>% 
  mutate(abundance = replace_na(abundance, 0)) %>% 
  filter(!Species %in% c('Total species', 'Total numbers', 'Number of  samples')) %>%
  spread(key = Species, value = abundance) %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)

barlow <- 
  read_csv('../master_data/raw_species_papers/691_Barlow_and_Forney_2007_Table8.csv') %>% 
  gather(key = `year-metric`, value = abundance, -Species) %>% 
  extract(col = `year-metric`, into = 'year', regex = "(\\d{4})") %>% 
  spread(key = Species, value = abundance)  %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year)



