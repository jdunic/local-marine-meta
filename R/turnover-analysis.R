# Get the no event studies that have raw species abundance matrices
studies_included <- no_event %>% distinct(Study.ID, .keep_all = TRUE) %>% 
  select(Study.ID, Reference, taxa, mean_imps, mean_invs, mean_nuts, sliced_ltc, vi_SppR_ROM)

raw_species_papers <- 
list.files('/Users/jillian/Manuscripts/2018-Meta-analysis_ms/Meta-analysis_papers/raw_species_papers') %>% 
  as_data_frame() %>% 
  separate(col = value, into = c('Study.ID', 'Ref')) %>% 
  slice(-19)

raw <- 
  left_join(raw_species_papers, studies_included, by = c('Study.ID')) %>% 
  filter(!is.na(mean_imps) & !is.na(mean_invs) & !is.na(mean_nuts) & !is.na(sliced_ltc))

table(raw$taxa)


# 11 studies have all driver data
# 13 studies have mean_imps
# Only 1 study has any variance/replication of species composition change
# Taxa: 2 Mixed, 1 algae, 6 fish, 1 marine mammals, 1 plant, 1 zooplankton
# 


