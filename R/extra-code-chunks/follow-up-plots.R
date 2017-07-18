# Any effect of system?
# Doesn't look like it when I do the different systems with the most studies
no_event %>% 
  filter(!is.na(vi_SppR_ROM)) %>% 
  filter(!is.na(mean_nuts), !is.na(mean_invs), !(is.na(sliced_ltc))) %>% 
  mutate(study_site = paste(Study.ID, Site, sep = '_')) %>% 
  group_by(Sys) %>% 
  summarise(sites = length(unique(study_site)), 
            studies = length(unique(Study.ID)), 
            min_duration = min(Duration), 
            max_duration = max(Duration)) %>% 
  ungroup()

coral_sys <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Sys == 'coral reef'), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
coral_sys

coastal_sys <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Sys == 'coastal shelf'), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
coastal_sys

est_sys <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Sys == 'estuarine mudflat'), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
est_sys

sub_mud_sys <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Sys == 'subtidal mudflat'), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
sub_mud_sys

rocky_sys <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Sys == 'rocky subtidal'), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
rocky_sys


# Effect of sampling effort:
# There is an effect but I think it has to do with the exclusion of data rather 
# than being an effect of sampling effort: see sampling ES ~ sample effort plot
nsame <- no_event %>% mutate(rep_diff = n2 - n1) %>% filter(rep_diff == 0)
#nsame <- no_event %>% mutate(rep_diff = n2 - n1) %>% filter(rep_diff < 3)

mod1_nsame <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = nsame %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
mod1_nsame

impact_nsame <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = nsame %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_nsame

drivers_scaled_nsame <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = nsame %>% mutate(scaled_invs = mean_invs * 10^-3),
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled_nsame

# ES ~ sampling effort plot
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM)) %>% 
  mutate(rep_diff = n2 - n1) %>% 
  ggplot(data = ., aes(x = rep_diff, y = yi_SppR_ROM, colour = Study.ID)) + 
    geom_point() + 
    geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed')

# ES ~ sampling effort plot (absolute value of difference)
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM)) %>% 
  mutate(rep_diff = n2 - n1) %>% 
  ggplot(data = ., aes(x = abs(rep_diff), y = yi_SppR_ROM, colour = Study.ID)) + 
    geom_point() + 
    geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed')


# Does the short term cutoff affect our results?
# Unclear, results are no longer significant but the estimate effect sign is 
# is still positive
mod1_2years <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Duration > 2), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
mod1_2years

mod1_3years <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Duration > 3), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
mod1_3years

mod1_4years <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Duration > 4), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
mod1_4years

mod1_5years <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), Duration > 5), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration)
mod1_5years


# Spatial scale as a covariate
source('00_driver_extraction_functions.R')
spatial <- semi_join(read_sp_data('../master_data/SiteSpatialData.csv'), no_event, by = c('Study.ID', 'Site'))

# Get the min and max lat long values (so that I don't need all the pieces in between)
corners <- 
  spatial %>% 
    group_by(Study.ID, Site) %>% 
    slice(c(which.min(Start_Lat), which.min(Start_Long), 
            which.max(Start_Lat), which.max(Start_Long))) %>% 
    ungroup() %>% 
    mutate(study_site = paste0(Study.ID, '__', Site))

corners_list <- split(corners, corners$study_site)

study_site_sp_points <- lapply(corners_list, function(study_site_df) {
  coordinates(study_site_df) <- ~Start_Long + Start_Lat
  projection(study_site_df) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  return(study_site_df)
  #sp_polygon <- Polygon(study_site_df)
  #return(sp_polygon)
})

# Calculate and tabulate the maximum distances between replicates in studies.
max_dists <- lapply(study_site_sp_points, function(sp_points_df) {
  max_dist <- geosphere::distm(sp_points_df) %>% max(.)
  return(max_dist)
}) %>% 
  bind_rows(.) %>% 
  gather(key = study_site, value = max_dist) %>% 
  separate(study_site, into = c('Study.ID', 'Site'), sep = '__')

no_event <- left_join(no_event, max_dists)


ggplot(data = no_event, aes(x = max_dist, y = yi_SppR_ROM, colour = Study.ID)) + 
  geom_jitter() +
  geom_text(aes(label = Study.ID), colour = 'black', size = 2) 

mod1_1km <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), max_dist < 1000), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_1km

mod1_10km <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), max_dist < 10000 & max_dist > 1000), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_10km

mod1_100km <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), max_dist < 100000 & max_dist > 10000), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_100km

mod1_100km <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), max_dist < 100000), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_100km

# Scale as a covariate
mod1_sp <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * max_dist)
mod1_sp

impact_sp <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps + max_dist)
impact_sp

drivers_scaled_sp <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% mutate(rep_diff = n2 - n1) ,
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)) + max_dist)
drivers_scaled_sp


# Covariance between drivers globally
# Nutrients, Invasives, LTC


