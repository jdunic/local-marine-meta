# supplementary material code

library(readr)
library(dplyr)
library(metafor)

source('00_functions.R')

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID))

no_event <- filter(fl_combined, Event != 'Yes')

no_event %>% 
  group_by(taxa) %>% 
  summarise(taxa_count = n())

no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM)) %>% 
  group_by(taxa) %>% 
  summarise(taxa_count = n())



fl_combined <- readr::read_csv("Data_outputs/fl_combined.csv")

event <- filter(fl_combined, Event == 'Yes')
no_event <- filter(fl_combined, Event != 'Yes')


test <- 
fl_combined %>%
  mutate(sub1 = replace(sub1, is.na(sub1), 1), 
         sub2 = replace(sub2, is.na(sub2), 1)) %>%
  mutate(aggregation = ifelse(sub1 > 1 | sub2 > 1, 'aggregated', 'single plot')) %>% 
  mutate(PlotSize = ifelse(aggregation == 'aggregated', sub1 * PlotSize, PlotSize))
# ------------------------------------------------------------------------------
# Dealing with aggregation
# ------------------------------------------------------------------------------

# Interesting there does not appear to be a difference in effect size based on 
# whether samples were aggregated or single plots. I am kind of surprised by this.
ggplot(data = test, aes(x = n1, y = yi.SppR.ROM)) + 
  geom_point() + 
  xlim(c(0, 200)) +
  facet_wrap(~ aggregation)

ggplot(data = test, aes(x = sub1, y = yi.SppR.ROM)) + 
  geom_point() + 
  xlim(c(0, 200))


# ------------------------------------------------------------------------------
# Dealing with bounded area 
# ------------------------------------------------------------------------------

sites <- read.csv('master_data/SiteSpatialData.csv') %>% as_data_frame() %>% 
            mutate(Reference = trimws(.$Reference), 
                   Site = trimws(.$Site))

# Combining start and end lat/lons so that we can just look at all the points 
# the same when creating the polygons for bounded area.

sites <- 
rbind_all(list(sites %>% select(-End_Lat, -End_Long), 
               sites %>% select(-Start_Lat, -Start_Long))
)

test <- 
  sites %>%
  mutate(site_id = paste(Study.ID, Site, sep = '_')) %>% 
  split(., .$site_id)


# Model checks


fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga')

no_event <- filter(fl_combined, Event != 'Yes')


## @knitr collinearity-check-among-invs-nuts-vocc-ltc

# Check for collinearity among the 4 proposed drivers. Collinearity was low for 
# all variables except the decadal rate of linear temperature change and 
# climate velocity (covariance = 0.87). Therefore only linear temperature change 
# was used in the following analyses. 
cov(no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_vocc) & !is.na(sliced_ltc)) %>% 
    mutate(scaled_invs = scale(mean_invs * 10^-3), 
           scaled_nuts = scale(mean_nuts), 
           scaled_vocc = scale(sliced_vocc), 
           scaled_ltc  = scale(sliced_ltc)) %>% 
    select(scaled_invs, scaled_nuts, scaled_vocc, scaled_ltc)
)

## @knitr collinearity-check-among-invs-nuts-ltc
cov(no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_vocc) & !is.na(sliced_ltc)) %>% 
    mutate(scaled_invs = scale(mean_invs * 10^-3), 
           scaled_nuts = scale(mean_nuts), 
           scaled_ltc  = scale(sliced_ltc)) %>% 
    select(scaled_invs, scaled_nuts, scaled_ltc)
)