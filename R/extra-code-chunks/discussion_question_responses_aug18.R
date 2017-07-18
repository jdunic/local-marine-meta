# Answering discussion questions

# Sample size weighted analysis using the same subset of studies used in the 
# variance weighted analysis.

## @knitr duration-samp-size-weighted-model-output-var-weighted-data-subset
# Results are the same as the var-weighted
mod_vw_subset_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss
mod_vw_subset_ss_robust <- robust(mod_vw_subset_ss, cluster=1:mod_vw_subset_ss$k)
mod_vw_subset_ss_robust

## @knitr impacts-samp-size-weighted-model-output-var-weighted-data-subset
# Results are the same as the var-weighted (though not significant)
impact_vw_subset_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_vw_subset_ss
impact_vw_subset_ss_robust <- robust(impact_vw_subset_ss, cluster=1:impact_vw_subset_ss$k)
impact_vw_subset_ss_robust

## @knitr drivers-samp-size-weighted-model-output-scaled-var-weighted-data-subset
# Results are the same as the sample size weighted analysis on all the data
drivers_vw_subset_scaled_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(Duration >= 5) %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs) & !is.na(vi_SppR_ROM)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_vw_subset_scaled_ss
drivers_vw_subset_scaled_ss_robust <- robust(drivers_vw_subset_scaled_ss, cluster=1:drivers_vw_subset_scaled_ss$k)
drivers_vw_subset_scaled_ss_robust

## @knitr drivers-ss-weighted-intercept-scaled-par-est-plot-short-long-breakdown-fig10
model_driver_vector <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration*Invasives', 
    'Duration*LTC', 'Duration*Nutrients')
ordered_driver_vector <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration*Invasives', 
    'Duration*Nutrients', 'Duration*LTC')


# What are the differences between the datasets used in the var-weighted and 
# sample size weighted analyses?

no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM))


## @knitr var-weighted-vs-ss-weighted-impact-score-hist
no_event %>% 
  filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM)) %>% 
  ggplot(data = ., aes(x = mean_imps), fill = 'red', alpha = 0.5) + 
    geom_histogram() + 
    theme_bw() + 
    geom_histogram(data = no_event %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM) & !is.na(n1)), aes(x = mean_imps), fill = 'blue', alpha = 0.5) + 
    xlab('Cumulative impact score') + 
    ylab('Count')

## @knitr var-weighted-vs-ss-weighted-annual-temp-change-score-hist
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_invs)) %>%
  ggplot(data = ., aes(x = sliced_ltc/10), fill = 'red', alpha = 0.5) + 
    geom_histogram() + 
    theme_bw() + 
    geom_histogram(data = no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(n1) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_invs)), aes(x = sliced_ltc/10), fill = 'blue', alpha = 0.5)

## @knitr var-weighted-vs-ss-weighted-nutrients-score-hist
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_invs)) %>%
  ggplot(data = ., aes(x = mean_nuts), fill = 'red', alpha = 0.5) + 
    geom_histogram() + 
    theme_bw() + 
    geom_histogram(data = no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(n1) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_invs)), aes(x = mean_nuts), fill = 'blue', alpha = 0.5)


## @knitr var-weighted-vs-ss-weighted-invasives-score-hist
invs_hist_vw_ss_comparison <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_invs)) %>%
  ggplot(data = ., aes(x = mean_invs), fill = 'red', alpha = 0.5) + 
    geom_histogram() + 
    theme_bw() + 
    geom_histogram(data = no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(n1) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_invs)), aes(x = mean_invs), fill = 'blue', alpha = 0.5)

invs_plot1 <-
  invs_hist_vw_ss_comparison + 
    xlim(c(0, 1000)) +
    ylim(c(0, 100))

invs_plot2 <-
  invs_hist_vw_ss_comparison + 
    xlim(c(0, 10000)) +
    ylim(c(0, 100))

invs_plot3 <-
  invs_hist_vw_ss_comparison + 
    xlim(c(0, 100000)) +
    ylim(c(0, 100))

grid.arrange(invs_plot1, invs_plot2, invs_plot3, invs_hist_vw_ss_comparison, ncol = 2)


## @knitr sample-size-variance-relationship-scatterplot
no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM)) %>% 
  ggplot(data = ., aes(x = n1, y = vi_SppR_ROM, colour = Study.ID)) + 
    geom_point() + 
    theme_bw() + 
    xlim(c(0, 110))


# Analysis for different taxonomic groups:



# Get minimum impact value used in drivers:
invs_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$mean_invs %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

nut_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$mean_nuts %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

ltc_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$sliced_ltc %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

## @knitr drivers-predictions-driver-facets
# Low temps
low_ltc_low_invs_low_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`25%`, 
                         nuts = nut_quantiles$`25%`, 
                         temp = ltc_quantiles$`25%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 25', nuts = 'nutrients quantile: 25', ltc = 'ltc quantile: 25', 
         invs_val = invs_quantiles$`25%`, nuts_val = nut_quantiles$`25%`, 
         ltc_val = ltc_quantiles$`25%`)

low_ltc_high_invs_low_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`75%`, 
                         nuts = nut_quantiles$`25%`, 
                         temp = ltc_quantiles$`25%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 75', nuts = 'nutrients quantile: 25', ltc = 'ltc quantile: 25', 
         invs_val = invs_quantiles$`75%`, nuts_val = nut_quantiles$`25%`, 
         ltc_val = ltc_quantiles$`25%`)

low_ltc_low_invs_high_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`25%`, 
                         nuts = nut_quantiles$`75%`, 
                         temp = ltc_quantiles$`25%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 25', nuts = 'nutrients quantile: 75', ltc = 'ltc quantile: 25', 
         invs_val = invs_quantiles$`25%`, nuts_val = nut_quantiles$`75%`, 
         ltc_val = ltc_quantiles$`25%`)


low_ltc_high_invs_high_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`75%`, 
                         nuts = nut_quantiles$`75%`, 
                         temp = ltc_quantiles$`25%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 75', nuts = 'nutrients quantile: 75', ltc = 'ltc quantile: 25', 
         invs_val = invs_quantiles$`75%`, nuts_val = nut_quantiles$`75%`, 
         ltc_val = ltc_quantiles$`25%`)

# High temps
high_ltc_low_invs_low_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`25%`, 
                         nuts = nut_quantiles$`25%`, 
                         temp = ltc_quantiles$`75%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 25', nuts = 'nutrients quantile: 25', ltc = 'ltc quantile: 75', 
         invs_val = invs_quantiles$`25%`, nuts_val = nut_quantiles$`25%`, 
         ltc_val = ltc_quantiles$`75%`)

high_ltc_high_invs_low_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`75%`, 
                         nuts = nut_quantiles$`25%`, 
                         temp = ltc_quantiles$`75%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 75', nuts = 'nutrients quantile: 25', ltc = 'ltc quantile: 75', 
         invs_val = invs_quantiles$`75%`, nuts_val = nut_quantiles$`25%`, 
         ltc_val = ltc_quantiles$`75%`)

high_ltc_low_invs_high_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`25%`, 
                         nuts = nut_quantiles$`75%`, 
                         temp = ltc_quantiles$`75%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 25', nuts = 'nutrients quantile: 75', ltc = 'ltc quantile: 75', 
         invs_val = invs_quantiles$`25%`, nuts_val = nut_quantiles$`75%`, 
         ltc_val = ltc_quantiles$`75%`)


high_ltc_high_invs_high_nuts  <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = invs_quantiles$`75%`, 
                         nuts = nut_quantiles$`75%`, 
                         temp = ltc_quantiles$`75%`, 
                         duration = 15) %>% 
  mutate(invs = 'invasives quantile: 75', nuts = 'nutrients quantile: 75', ltc = 'ltc quantile: 75', 
         invs_val = invs_quantiles$`75%`, nuts_val = nut_quantiles$`75%`, 
         ltc_val = ltc_quantiles$`75%`)

combined_predictions <- 
  bind_rows(list(low_ltc_low_invs_low_nuts, low_ltc_high_invs_low_nuts, low_ltc_low_invs_high_nuts, low_ltc_high_invs_high_nuts, 
    high_ltc_low_invs_low_nuts, high_ltc_high_invs_low_nuts, high_ltc_low_invs_high_nuts, high_ltc_high_invs_high_nuts))
ggplot(data = combined_predictions) + 
  geom_line(aes(x = duration, y = pred, colour = invs)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = invs), alpha = 0.4) + 
  theme_minimal() + 
  xlab('\nDuration (years)') + 
  ylab('Predicted log ratio\n') + 
  facet_grid(ltc ~ nuts)


## @knitr stopping-above-chunk
# Event data
events <- filter(fl_combined, Event == 'Yes')

event_mod <-   

  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
    data = events %>% filter(!is.na(yi_SppR_ROM)),
    random = ~ 1 | factor(Study.ID), 
    mods = ~ Duration * factor(expected_change) - 1)


  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
    data = events %>% filter(!is.na(yi_SppR_ROM)),
    random = ~ 1 | factor(Study.ID), 
    mods = ~ Duration)


event_mod_ss <-   
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
    data = events %>% filter(!is.na(yi_SppR_ROM) & !is.na(expected_change)),
    random = ~ 1 | factor(Study.ID), 
    mods = ~ Duration * factor(expected_change) - 1)

event_mod_ss_robust <- robust(event_mod_ss, cluster=1:event_mod_ss$k)
event_mod_ss_robust



### AIC values for the 4 sample size weighted models
no_mods_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM), !is.na(n1)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ 1)
no_mods_ss
no_mods_ss_robust <- robust(no_mods_ss, cluster=1:no_mods_ss$k)
no_mods_ss_robust



AIC(no_mods_ss, mod1_ss, impact_ne_ss, drivers_scaled_ss)

                  df      AIC
no_mods_ss         2 763.1707
mod1_ss            3 762.2807
impact_ne_ss       5 640.2605
drivers_scaled_ss  9 501.9534


ggplot(data = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)), aes(x = 1 / n1, y = log(vi_SppR_ROM))) + 
  geom_point()


library(googleVis)
library(readr)

no_event %>% 
  #filter(!is.na(vi_SppR_ROM)) %>% 
  select(Study.ID, Site, Reference, Descriptor.of.Taxa.Sampled, taxa, n1, vi_SppR_ROM, PlotSize, PlotSizeUnits) %>% 
  distinct() %>% 
  arrange(n1) %>% 
  gvisTable(.) %>% 
  plot(.)


spatial <- semi_join(read_sp_data('../master_data/SiteSpatialData.csv'), no_event, by = c('Study.ID', 'Site'))

corners <- 
  spatial %>% 
    group_by(Study.ID, Site) %>% 
    slice(c(which.min(Start_Lat), which.min(Start_Long), 
            which.max(Start_Lat), which.max(Start_Long))) %>% 
    ungroup() %>% 
    mutate(study_site = paste0(Study.ID, '__', Site))

corners_list <- split(corners, corners$study_site)

test <- lapply(corners_list, function(study_site_df) {
  coordinates(study_site_df) <- ~Start_Long + Start_Lat
  projection(study_site_df) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  return(study_site_df)
  #sp_polygon <- Polygon(study_site_df)
  #return(sp_polygon)
})


max_dists <- lapply(test, function(sp_points_df) {
  max_dist <- geosphere::distm(sp_points_df) %>% max(.)
  return(max_dist)
}) %>% 
  bind_rows(.) %>% 
  gather(key = study_site, value = max_dist) %>% 
  separate(study_site, into = c('Study.ID', 'Site'), sep = '__')

no_event <- left_join(no_event, max_dists)

ggplot(data = no_event %>% filter(!is.na(vi_SppR_ROM), Study.ID != 172), aes(x = n1, y = max_dist, colour = taxa)) + 
  geom_jitter() +
  geom_text(aes(label = Study.ID), colour = 'black', size = 2) +
  xlim(c(0, 100))

z <- filter(spatial, Study.ID == 463, Site == 'Non-dredged')


# combining taxonomic groups:

taxa_combined <- 

no_event %>%
  mutate(taxa_combined = taxa) %>% 
  mutate(taxa_combined = gsub('phytoplankton|zooplankton', 'plankton', .$taxa_combined)) %>% 
  mutate(taxa_combined = gsub('mobile inverts|sessile inverts', 'inverts', .$taxa_combined)) %>%
  group_by(taxa_combined, n1) %>% 
  summarise(count = n()) %>% 
ggplot(data = ., aes(x = n1, y = count)) + 
  geom_bar(stat = 'identity') +
  xlim(c(0, 100)) +
  facet_wrap(~ taxa_combined, scales = 'free')

no_event %>%
  mutate(taxa_combined = taxa) %>% 
  mutate(taxa_combined = gsub('phytoplankton|zooplankton', 'plankton', .$taxa_combined)) %>% 
  mutate(taxa_combined = gsub('mobile inverts|sessile inverts', 'inverts', .$taxa_combined)) %>%
  #filter(taxa_combined != 'fish') %>% 
ggplot(data = ., aes(x = 1 / n1, y = vi_SppR_ROM, colour = taxa_combined)) + 
  geom_point() + 
  facet_wrap(~ taxa_combined)