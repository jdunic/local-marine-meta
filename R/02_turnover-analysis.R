# Get the no event studies that have raw species abundance matrices
#studies_included <- no_event %>% distinct(Study.ID, .keep_all = TRUE) %>% 
#  select(Study.ID, Reference, taxa, mean_imps, mean_invs, mean_nuts, sliced_ltc, vi_SppR_ROM)

#raw_species_papers <- 
#list.files('/Users/jillian/Manuscripts/2018-Meta-analysis_ms/Meta-analysis_papers/raw_species_papers') %>% 
  #as_data_frame() %>% 
  #separate(col = value, into = c('Study.ID', 'Ref')) %>% 
  #slice(-19)

#raw <- 
#  left_join(raw_species_papers, studies_included, by = c('Study.ID')) %>% 
#  filter(!is.na(mean_imps) & !is.na(mean_invs) & !is.na(mean_nuts) & !is.na(sliced_ltc))

#table(raw$taxa)


# 11 studies have all driver data
# 13 studies have mean_imps
# Only 1 study has any variance/replication of species composition change
# Taxa: 2 Mixed, 1 algae, 6 fish, 1 marine mammals, 1 plant, 1 zooplankton


# 1) Get all of the turnover values from all years in time series compared to 
#    first year in time series
# 2) Identify study ID and site for each jaccard dataframe
# 3) Join jaccard dataframe with site coordinate data 
# Should Barlow and Fornkey even be included? Covers the coast from Washington - California

#SiteSpatialData.csv
#Study.ID,Reference,Site,Start_Lat,Start_Long,End_Lat,End_Long,Shape,Source,Checked,Notes
library(vegan)
library(raster)
library(tidyverse)

#source('clean-raw-data.R')
source('00_driver_extraction_functions.R')
source('00_functions.R')

load("../data_outputs/turnover_dfs.Rdata")

raw_refs <- read_csv('../master_data/raw_species_papers/raw-species-refs-list.csv')

#filter(spatial, Study.ID %in% raw_refs$ref_id) %>% 
#  write_csv('raw_ref_spatialdata.csv')

# Get spatial data and cumulative human impact layer
#-------------------------------------------------------------------------------
#raw_spatial <- read_csv('../master_data/raw_species_papers/raw_ref_spatialdata.csv')

#raw_points <- create_sp_points(raw_spatial)

#raw_lines <- create_sp_lines(raw_spatial)

#raw_chi_data <- 
#  extract_imp_data(raw_spatial, sp_points = raw_points, sp_lines = raw_lines, 
#                   in_file = '../master_data/Impact_Data/CI_2013_OneTimePeriod/global_cumul_impact_2013_all_layers.tif', 
#                   out_file = '../Data_outputs/raw_species_with_chi.csv') %>%
#  #group_by(Study.ID, Raw_Site) %>% 
  #summarise(mean_chi = mean(mean_imps, na.rm = TRUE))

#write_csv(raw_chi_data, '../Data_outputs/raw_species_with_chi.csv')

raw_chi_data <- read_csv('../Data_outputs/raw_species_with_chi.csv')

# Get Jaccard matrices for each dataset
#-------------------------------------------------------------------------------
# Copepods
yildiz_jac <- 
  yildiz %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(yildiz['year'], .) %>% 
  mutate(ref_id = 6, site = 'Trabzon', group = NA)

# Fish
felix_recruits_jac  <-
  felix %>% 
  filter(group == 'recruits') %>% 
  select(-group, -year) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(felix, group == 'recruits')['year'], .) %>% 
  mutate(ref_id = 34, site = 'Cape of Palos and Grosa Island', group = 'recruits')

# Fish
felix_settlers_jac  <-
  felix %>% 
  filter(group == 'settlers') %>% 
  select(-group, -year) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(felix, group == 'settlers')['year'], .) %>% 
  mutate(ref_id = 34, site = 'Cape of Palos and Grosa Island', group = 'settlers')

# Fish (N = hard bottom; B = mitigation boulders)
kilfoyle_jac <-
  kilfoyle %>% 
  filter(group == 'N') %>% 
  select(-group, -year) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(kilfoyle, group == 'N')['year'], .) %>% 
  mutate(ref_id = 67, site = 'N', group = NA)

# Algae
diez_jac <- 
  diez %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(diez['year'], .) %>% 
  mutate(ref_id = 181, site = 'all', group = NA)

# Marine mammals
burrows_jac <- 
  burrows %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(burrows['year'], .) %>% 
  mutate(ref_id = 193, site = 'all', group = NA)

# Marsh plants in tidal marsh (NT = non tidal marsh, T = tidal marsh)
ibarra_spring_jac <- 
  ibarra %>% 
  filter(group == 'T' & season == 'S') %>%  
  select(-season, -year, -group) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(ibarra, group == 'T' & season == 'S')['year'], .) %>% 
  mutate(ref_id = 300, site = 'Tidal', group = 'spring')


ibarra_fall_jac <- 
  ibarra %>% 
  filter(group == 'T' & season == 'F') %>%  
  select(-season, -year, -group) %>% 
  as.matrix() %>% 
  vegdist(., method="jaccard") %>%
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(ibarra, group == 'T' & season == 'F')['year'], .) %>% 
  mutate(ref_id = 300, site = 'Tidal', group = 'fall')

# Zooplankton
##### PRIMO NEEDS SPATIAL DATA
##### PRIMO had events (floods and droughts) and was excluded from analysis
primo_spring_jac <- 
  primo %>% 
  filter(season == 'S') %>% 
  select(-year, -season) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(primo, season == 'S')['year'], .) %>% 
  mutate(ref_id = 360, site = NA, group = 'spring')

primo_summer_jac <- 
  primo %>% 
  filter(season == 'SM') %>% 
  select(-year, -season) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(primo, season == 'SM')['year'], .) %>% 
  mutate(ref_id = 360, site = NA, group = 'summer')

primo_autumn_jac <- 
  primo %>% 
  filter(season == 'A') %>% 
  select(-year, -season) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(primo, season == 'A')['year'], .) %>% 
  mutate(ref_id = 360, site = NA, group = 'autumn')

primo_winter_jac <- 
  primo %>% 
  filter(season == 'W') %>% 
  select(-year, -season) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(primo, season == 'W')['year'], .) %>% 
  mutate(ref_id = 360, site = NA, group = 'winter')

# Fish
james_large_seine_jac <- 
  james_large_seine %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(james_large_seine['year'], .) %>% 
  mutate(ref_id = 440, site = 'all', group = 'large seine')

james_small_seine_jac <- 
  james_small_seine %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(james_small_seine['year'], .) %>% 
  mutate(ref_id = 440, site = 'all', group = 'small seine')

james_gillnet_jac <- 
  james_gillnet %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(james_gillnet['year'], .) %>% 
  mutate(ref_id = 440, site = 'all', group = 'gillnet')

# Mudflat gastropods
ohgaki_jac <- 
  ohgaki %>% 
  select(-year) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(ohgaki['year']) %>% 
  mutate(ref_id = 551, site = 'Nagura Estuary', group = NA)

# Fish
methven_day_jac <- 
  methven %>%
  filter(group == 'Day') %>% 
  select(-year, -group) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(methven, group == 'Day')['year']) %>% 
  mutate(ref_id = 595, site = 'estuary', group = 'day')

methven_night_jac <- 
  methven %>%
  filter(group == 'Night') %>% 
  select(-year, -group) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(methven, group == 'Night')['year']) %>% 
  mutate(ref_id = 595, site = 'estuary', group = 'night')

# Saltmarsh plants
# Noe et al LPL site was excluded - only one time point
noe_SW_C_jac <- 
  noe %>% 
  filter(site == 'SW-C') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(noe, site == 'SW-C')['year']) %>% 
  mutate(ref_id = 599, site = 'SWC', group = NA)

noe_SW_P_jac <- 
  noe %>% 
  filter(site == 'SW-P') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(noe, site == 'SW-P')['year']) %>% 
  mutate(ref_id = 599, site = 'SWP', group = NA)

noe_TE_C_jac <- 
  noe %>% 
  filter(site == 'TE-C') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(noe, site == 'TE-C')['year']) %>% 
  mutate(ref_id = 599, site = 'TEC', group = NA)

# Fish
lazzari_salt_pond_jac <- 
  lazzari %>% 
  filter(site == 'salt pond') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(lazzari, site == 'salt pond')['year']) %>% 
  mutate(ref_id = 620, site = 'salt pond', group = NA)

lazzari_beach_low_tide_jac <- 
  lazzari %>% 
  filter(site == 'beach low tide') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(lazzari, site == 'beach low tide')['year']) %>% 
  mutate(ref_id = 620, site = 'beach low tide', group = NA)

lazzari_beach_mid_flood_jac <- 
  lazzari %>% 
  filter(site == 'beach mid flood') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(lazzari, site == 'beach mid flood')['year']) %>% 
  mutate(ref_id = 620, site = 'beach mid flood', group = NA)

# Fish, only used natural marsh, not constructed marsh
williams_jac <- 
  williams %>% 
  filter(site == 'natural marsh') %>% 
  select(-year, -site) %>% 
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(williams, site == 'natural marsh')['year']) %>% 
  mutate(ref_id = 621, site = 'natural', group = NA)

# Fish
gibson_trawl_jac <- 
  gibson_trawl %>% 
  select(-year) %>%
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(gibson_trawl)['year']) %>% 
  mutate(ref_id = 660, site = 'Tralee Beach', group = 'fish trawl')

gibson_seine_jac <- 
  gibson_seine %>% 
  select(-year) %>%
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(gibson_seine)['year']) %>% 
  mutate(ref_id = 660, site = 'Tralee Beach', group = 'fish seine')

gibson_trawl_crust_jac <- 
  gibson_trawl_crust %>% 
  select(-year) %>%
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(gibson_trawl_crust)['year']) %>% 
  mutate(ref_id = 660, site = 'Tralee Beach', group = 'crustacean trawl')

# I think Barlow and Forney should be excluded because spatial scale is massive > 1000km
barlow_jac <- 
  barlow %>% 
  select(-year) %>%
  as.matrix() %>% 
  vegdist(., method = "jaccard") %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  .[1] %>% 
  bind_cols(filter(barlow)['year']) %>% 
  mutate(ref_id = 691, site = 'pacific coast', group = NA)

all_jac_mats <- 
  bind_rows(yildiz_jac, felix_recruits_jac, felix_settlers_jac, 
            kilfoyle_jac, diez_jac, burrows_jac, ibarra_spring_jac, 
            ibarra_fall_jac, james_large_seine_jac, james_small_seine_jac, 
            james_gillnet_jac, ohgaki_jac, methven_day_jac, 
            methven_night_jac, noe_SW_P_jac, noe_SW_C_jac, noe_TE_C_jac, 
            lazzari_salt_pond_jac, lazzari_beach_low_tide_jac, 
            lazzari_beach_mid_flood_jac, williams_jac, gibson_trawl_jac, 
            gibson_seine_jac, gibson_trawl_crust_jac) %>%
  left_join(., raw_chi_data, by = c('ref_id' = 'Study.ID', 'site' = 'Raw_Site')) %>% 
  rename(jaccard = `1`)
#
first_years <- 
  all_jac_mats %>% 
  group_by(ref_id, site, group) %>% 
  arrange(ref_id, site, group, year) %>%
  slice(1) %>% 
  ungroup() %>% 
  rename(first_year = year) %>% 
  select(ref_id, site, group, first_year)
#
all_jac_mats <- 
  left_join(all_jac_mats, first_years, by = c('ref_id', 'site', 'group')) %>% 
  mutate(duration = year - first_year)

# All jaccard matrices compared relative to initial time point
jacs_ts <- 
  all_jac_mats %>% 
  group_by(ref_id, site, group) %>% 
  arrange(ref_id, site, group, year) %>%
  slice(-1) %>% 
  ungroup()


# Jaccard ~ Duration * CHI
# When cumulative impacts were taken into account, there was still no observed 
# change in Jaccard over time.

library(lme4)
library(lmerTest)
library(broom)

more_than_1_year <- 
  jacs_ts %>% 
    group_by(ref_id, site, group) %>% 
    summarise(year_count = n()) %>% 
    filter(year_count > 1)

jacs_lms <- 
  jacs_ts %>%
    filter(ref_id %in% more_than_1_year[['ref_id']]) %>% 
    group_by(ref_id, site, group, mean_chi) %>% 
    do(fit = lm(jaccard ~ duration, data = .) %>% broom::tidy(.)) %>% 
    unnest(fit) %>% 
    filter(term == 'duration')

jacs_lmer <- 
  jacs_ts %>%
    filter(ref_id %in% more_than_1_year[['ref_id']]) %>% 
    mutate(ref_id = factor(ref_id)) %>% 
  lmer(jaccard ~ duration + (1 | ref_id), data = .)

jac_ts_mod <- 
  rma.mv(yi = estimate, V = std.error, data = jacs_lms, 
         random = ~ 1 | as.factor(ref_id))

jac_ts_chi_mod <- 
  rma.mv(yi = estimate, V = std.error, data = jacs_lms, 
         random = ~ 1 | as.factor(ref_id), 
         mods = ~ mean_chi)

# Jaccard data tables

jac_ts_mod_tab <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(jac_ts_mod$b), 
             estimate = round(as.vector(jac_ts_mod$b), digits = 3),
             se = round(jac_ts_mod$se, digits = 3), 
             pval = round(jac_ts_mod$pval, digits = 3), 
             ci.lb = round(jac_ts_mod$ci.lb, digits = 3), 
             ci.ub = round(jac_ts_mod$ci.ub, digits = 3), 
             k = jac_ts_mod$k, 
             n = jac_ts_mod$s.nlevels)

jac_ts_chi_mod_tab <- 
  data_frame(model = 'LR ~ D*CHI', 
             parameter = rownames(jac_ts_chi_mod$b), 
             estimate = round(as.vector(jac_ts_chi_mod$b), digits = 3), 
             se = round(jac_ts_chi_mod$se, digits = 3), 
             pval = round(jac_ts_chi_mod$pval, digits = 3), 
             ci.lb = round(jac_ts_chi_mod$ci.lb, digits = 3), 
             ci.ub = round(jac_ts_chi_mod$ci.ub, digits = 3), 
             k = jac_ts_chi_mod$k, 
             n = jac_ts_chi_mod$s.nlevels)


# Jaccard ~ Duration
# Jaccard did not change with increasing duration
dev.new(width = 8.2, height = 5)
jacs_ts %>%
  filter(ref_id %in% more_than_1_year[['ref_id']]) %>% 
  ggplot(data = .) + 
    geom_point(aes(x = duration, y = jaccard, colour = factor(ref_id))) + 
    stat_smooth(se = FALSE, method = 'lm', aes(x = duration, y = jaccard, colour = factor(ref_id))) + 
    #geom_abline(aes(slope = broom.mixed::tidy(jacs_lmer)[2, 'estimate'][[1]], 
    #                intercept = broom.mixed::tidy(jacs_lmer)[1, 'estimate'][[1]])) + 
    theme_minimal() + 
    theme(axis.text.y = element_text(hjust = 1, size = 13), 
          axis.text.x = element_text(size = 13), 
          axis.title = element_text(size = 13), 
          plot.background = element_blank()) + 
    ylab("Jaccard index\n") + 
    xlab("\nStudy duration (years)") + 
    guides(colour = FALSE)

#dev.copy2pdf(file = '../figures/jaccard-plot.pdf')
