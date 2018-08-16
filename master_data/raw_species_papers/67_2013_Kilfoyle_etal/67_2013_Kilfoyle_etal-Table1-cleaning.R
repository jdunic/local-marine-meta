library(tidyverse)

kilf <- read_csv('/Users/jillian/Manuscripts/2018-Meta-analysis_ms/Meta-analysis_papers/raw_species_papers/67_2013_Kilfoyle_etal/67_2013_Kilfoyle_etal-tabula-Table1.csv') %>% 
  slice(-1) %>% 
  gather(key = year, value = count, -1, -2) %>% 
  separate(year, into = c('year', 'site'), sep = '-') %>% 
  separate(count, into = c('T', 'O'), sep = '/') %>% 
  select(-O) %>% 
  filter(!is.na(`Common Name`))

kilf %>% 
  filter(site == 'N') %>%
  spread(key = year, value = T) %>% 
  write_csv('/Users/jillian/Manuscripts/2018-Meta-analysis_ms/Meta-analysis_papers/raw_species_papers/67_2013_Kilfoyle_etal/67_2013_Kilfoyle_etal-Table1-siteN.csv')

kilf %>% 
  filter(site == 'B') %>%
  spread(key = year, value = T) %>% 
  write_csv('/Users/jillian/Manuscripts/2018-Meta-analysis_ms/Meta-analysis_papers/raw_species_papers/67_2013_Kilfoyle_etal/67_2013_Kilfoyle_etal-Table1-siteB.csv')

