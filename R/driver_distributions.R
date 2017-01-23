library(ggplot2)
library(gridExtra)
library(raster)
library(hadsstr)
library(dplyr)
library(beepr)
library(rgdal)

# Distribution of global drivers observed within our dataset
# ------------------------------------------------------------------------------

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga') %>% 
  # Keller study 
  filter(Study.ID != '172') %>%
  # Study  136 - Enfermeria should have been classified as having an event - 
  # 'shrimp farming' and 'tidal restriction'
  filter(Site != 'Enfermeria')

no_event <- filter(fl_combined, Event != 'Yes')

## @knitr Duration-frequency-no-event-data-vw-&-ss-weighted
ggplot() + 
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), aes(x = Duration), fill = 'black') +
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM)), aes(x = Duration), fill = 'blue', alpha = 0.6) + 
    theme_minimal() + 
    ylab('Frequency\n')


## @knitr Impacts-frequency-no-event-data-vw-&-ss-weighted
ggplot() + 
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), aes(x = mean_imps), fill = 'black') +
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM)), aes(x = mean_imps), fill = 'blue', alpha = 0.6) + 
    theme_minimal() + 
    ylab('Frequency\n')


## @knitr Invasives-frequency-no-event-data-vw-&-ss-weighted
ggplot() + 
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), aes(x = mean_invs), fill = 'black') +
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM)), aes(x = mean_invs), fill = 'blue', alpha = 0.6) + 
    theme_minimal() + 
    ylab('Frequency\n')

## @knitr Invasives-frequency-no-event-data-vw-&-ss-weighted
ggplot() + 
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), aes(x = mean_invs * 10^-3), fill = 'black') +
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM)), aes(x = mean_invs * 10^-3), fill = 'blue', alpha = 0.6) + 
    theme_minimal() + 
    ylab('Frequency\n')

## @knitr Nutrients-frequency-no-event-data-vw-&-ss-weighted
ggplot() + 
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), aes(x = mean_nuts), fill = 'black') +
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM)), aes(x = mean_nuts), fill = 'blue', alpha = 0.6) + 
    theme_minimal() + 
    ylab('Frequency\n')


## @knitr LTC-frequency-no-event-data-vw-&-ss-weighted
ggplot() + 
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), aes(x = sliced_ltc / 10), fill = 'black') +
    geom_histogram(data = filter(no_event, !is.na(yi_SppR_ROM)), aes(x = sliced_ltc / 10), fill = 'blue', alpha = 0.6) + 
    theme_minimal() + 
    ylab('Frequency\n')

## CUMULATIVE FREQUENCIES
## @knitr cumul-freq-and-hists-of-invs-nuts-ltc-in-data
driver_cum_freq <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  ggplot(data = ., aes(x = driver_value)) + 
    stat_ecdf() + 
    theme_minimal() + 
    xlab('\nDriver value') + 
    ylab('Cumulative frequency\n') + 
    facet_wrap( ~ driver, scales = 'free')

invs_cum_freq <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
    filter(driver == 'mean_invs') %>% 
  ggplot(data = ., aes(x = driver_value)) + 
    stat_ecdf() + 
    theme_minimal() + 
    xlab('') + 
    ylab('Cumulative frequency\n')

nuts_cum_freq <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  filter(driver == 'mean_nuts') %>% 
ggplot(data = ., aes(x = driver_value)) + 
  stat_ecdf() + 
  theme_minimal() + 
  xlab('') + 
  ylab('Cumulative frequency\n')

ltc_cum_freq <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  filter(driver == 'sliced_ltc') %>% 
ggplot(data = ., aes(x = driver_value)) + 
  stat_ecdf() + 
  theme_minimal() + 
  xlab('') +  
  ylab('Cumulative frequency\n')

driver_hist <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  ggplot(data = ., aes(x = driver_value)) + 
    geom_histogram() +
    #geom_histogram(binwidth = 10, colour = 'black', boundary = 0) + 
    theme_minimal() + 
    xlab('\nDriver value') + 
    ylab('Frequency\n') + 
    facet_wrap( ~ driver, scales = 'free')

invs_hist <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
    filter(driver == 'mean_invs') %>% 
  ggplot(data = ., aes(x = driver_value)) + 
    geom_histogram() + 
    theme_minimal() + 
    xlab('\nInvasion potential \n(shipping tonnage / km^2)') + 
    ylab('Frequency\n')

nuts_hist <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  filter(driver == 'mean_nuts') %>% 
ggplot(data = ., aes(x = driver_value)) + 
  geom_histogram() + 
  theme_minimal() + 
  xlab('\nNutrient addition \n(tonnes / km^2)') + 
  ylab('Frequency\n')

ltc_hist <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  filter(driver == 'sliced_ltc') %>% 
ggplot(data = ., aes(x = driver_value)) + 
  geom_histogram() + 
  theme_minimal() +  
  #xlab('\n Linear temperature change (˚C / decade)'
  xlab(expression(atop("Linear temperature change", "("*
           degree * "C / decade)"))) + 
  ylab('Frequency\n')

## @knitr driver-frequency-distributions
grid.arrange(driver_cum_freq, driver_hist, nrow = 2)


## @knitr full-drivers-model-var-weighted-invasives-predictions-with-dists-fig7
#grid.arrange(invs_predictions_0_25_75, invs_cum_freq, invs_hist, layout_matrix = cbind(c(1, 1), c(1, 1), c(2, 3)))

## @knitr full-drivers-model-var-weighted-nutrients-predictions-with-dists-fig8
#grid.arrange(nuts_predictions_0_25_75, nuts_cum_freq, nuts_hist, layout_matrix = cbind(c(1, 1), c(1, 1), c(2, 3)))

## @knitr full-drivers-model-var-weighted-ltc-predictions-with-dists-fig9
#grid.arrange(ltc_predictions_0_25_75, ltc_cum_freq, ltc_hist, layout_matrix = cbind(c(1, 1), c(1, 1), c(2, 3)))


## Distribution of data globally - random sampling of the Halpern data
#imps <- raster("../master_data/Impact_Data/CI_2013_OneTimePeriod/global_cumul_impact_2013_all_layers.tif")
#invs <- raster('../master_data/Impact_Data/invasives_raw/invasives.tif')
#nuts <- raster('../master_data/Impact_Data/plumes_fertilizer_raw/plumes_fert.tif')
#hadrast <- load_hadsst('../master_data/HadISST_sst.nc')

# Get cells to extract
#big_invs <- Which(invs > 0, cells = TRUE)
#big_nuts <- Which(nuts > 0, cells = TRUE)

# get cells and convert to coordinates for extraction of ltc data
#big_nuts_raster <- Which(nuts > 0, cells = TRUE)
#nuts_points <- rasterToPoints(big_nuts_raster, spatial=TRUE)
#proj4string(nuts_points)


meow <- readOGR("../master_data/MEOW-TNC", "meow_ecos")

meow2 <- spTransform(meow, projection(imps))
meow2 <- spTransform(meow[c(-68, -69, -70, -71), ], projection(imps))

fortify(meow2) %>% arrange(group) %>% filter(hole == TRUE)

#plot(spTransform(meow[c(-68, -69, -70, -71), ], projection(imps)))


map.df <- left_join(map.df, data, by=c('id'='ID'))

meow2@polygons[[1]]@Polygons[[1]]@hole %>% str

slot(meow2, 'polygons') %>% str

test <- lapply(meow2@polygons, function(polygon) {
  inner_hole_list = lapply(polygon@Polygons, function(inner_poly) {
    inner_hole = inner_poly@hole
    return(inner_hole)
  }
  inner_poly_sum <- sum(unlist(inner_hole_list))
  return(inner_poly_sum)
  )
  hole_TF = sum(unlist(inner_hole_list))
})

meow_nh <- fortify(meow2) %>% arrange(group) %>% mutate(group = as.numeric(as.character(group))) %>% filter(hole == TRUE) %>% 
  mutate(group = factor(group))

meow_list <- split(meow_nh, meow_nh$group)

lapply <- 

ggplot(test, mapping = aes(x = long, y = lat, group = group, fill = factor(group))) + geom_polygon()

68.1, 70.1, 70.2 

ggplot(meow2, mapping = aes(x = long, y = lat, group = group, fill = group)) +
 geom_polygon()

#invs_values <- extract(invs, big_invs)
#nuts_values <- extract(nuts, big_nuts)

# get average linear temperature change for variance weighted analysis time span
no_event %>% 
  filter(!is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM), vi_SppR_ROM > 0, 
         !is.na(mean_invs), !is.na(mean_nuts), !is.na(sliced_ltc)) %>%  
  summarise(first_year = min(T1), last_year = max(T2))

#vw_subset_ltc <- get_sst_linear_change(hadrast, years = 1969:2012)
#vw_subset_ltc_short <- get_sst_linear_change(hadrast, years = 1989:1990)

#meow_ltc_raster <- mask(vw_subset_ltc, meow)
#meow_ltc_raster_short <- mask(vw_subset_ltc_short, meow)

#ltc_values_all <- values(meow_ltc_raster)
#ltc_values <- ltc_values_all[!is.na(ltc_values_all)]

#ltc_values_all_short <- values(meow_ltc_raster_short)
#ltc_values_short <- ltc_values_all_short[!is.na(ltc_values_all_short)]

#saveRDS(ltc_values, '../Data_outputs/ltc_values_meow.rda')
#saveRDS(ltc_values_short, '../Data_outputs/ltc_values_short_meow.rda')

world_invs <- readRDS('../Data_outputs/invs_vals_above_zero.rda')
world_nuts <- readRDS('../Data_outputs/nuts_vals_above_zero.rda')
world_ltc  <- readRDS('../Data_outputs/ltc_values_meow.rda')
world_ltc_short  <- readRDS('../Data_outputs/ltc_values_short_meow.rda')

world_invs_df <- data_frame(invs = world_invs, dataset = 'global')
world_nuts_df <- data_frame(nuts = world_nuts, dataset = 'global')
world_ltc_df  <- data_frame(ltc = world_ltc, dataset = 'global')
world_ltc_short_df <- data_frame(ltc_short = world_ltc_short, dataset = 'global')

nuts_dist <- 
no_event %>% 
  filter(yi_SppR_ROM > 0, !is.na(vi_SppR_ROM), !is.na(mean_invs), !is.na(mean_nuts), !is.na(sliced_ltc)) %>% 
  select(mean_nuts) %>% 
  rename(nuts = mean_nuts) %>% 
  mutate(dataset = 'analysis') %>% 
  bind_rows(., world_nuts_df) %>% 
ggplot(data = ., aes(x = log(nuts + 0.1), colour = dataset, fill = dataset)) + 
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  guides(colour = FALSE, fill = FALSE) +
  ylab('Density') +
  xlab('\nlog(nutrient addtion + 0.1)')
invs_dist <- 
no_event %>% 
  filter(yi_SppR_ROM > 0, !is.na(vi_SppR_ROM), !is.na(mean_invs), !is.na(mean_nuts), !is.na(sliced_ltc)) %>% 
  select(mean_invs) %>% 
  rename(invs = mean_invs) %>% 
  mutate(dataset = 'analysis') %>% 
  bind_rows(., world_invs_df) %>% 
ggplot(data = ., aes(x = log(invs + 1), colour = dataset, fill = dataset)) + 
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  guides(colour = FALSE, fill = FALSE) + 
  ylab('') +
  xlab('\nlog(propagule pressure + 1)')
ltc_dist <- 
no_event %>% 
  filter(yi_SppR_ROM > 0, !is.na(vi_SppR_ROM), !is.na(mean_invs), !is.na(mean_nuts), !is.na(sliced_ltc)) %>% 
  select(sliced_ltc) %>% 
  rename(ltc = sliced_ltc) %>% 
  mutate(dataset = 'analysis') %>% 
  bind_rows(., world_ltc_df) %>% 
ggplot(data = ., aes(x = ltc / 10, colour = dataset, fill = dataset)) + 
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  ylab('') +
  xlab('\nLinear temperature change C / year') + 
  labs(fill = '', colour = '')

#grid.arrange(nuts_dist, invs_dist, ltc_dist, ncol = 3)


ltc_dist_short <- 
no_event %>% 
  filter(yi_SppR_ROM > 0, !is.na(vi_SppR_ROM), !is.na(mean_invs), !is.na(mean_nuts), !is.na(sliced_ltc)) %>% 
  select(sliced_ltc) %>% 
  rename(ltc_short = sliced_ltc) %>% 
  mutate(dataset = 'analysis') %>% 
  bind_rows(., world_ltc_short_df) %>% 
ggplot(data = ., aes(x = ltc_short / 10, colour = dataset, fill = dataset)) + 
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  ylab('') +
  xlab('\nLinear temperature change C / year') + 
  labs(fill = '', colour = '')

master_layout <- 
grid.layout(nrow = 1, ncol = 3, 
            widths = unit(c(0.8, 0.8, 1), "null"),
            heights = unit(c(1), "null"))
#dev.new(height = 3.5, width = 11)

## @knitr global-vs-analysis-data-coverage-drivers-plot
grid.newpage()
pushViewport(viewport(layout = master_layout))
print(nuts_dist, vp = set_vp(1, 1))
print(invs_dist, vp = set_vp(1, 2))
print(ltc_dist, vp = set_vp(1, 3))

#
grid.text(
    "a)", vp = viewport(layout.pos.row = 1, layout.pos.col = 1), 
    gp = gpar(fontsize = 11), vjust = -13, hjust = 12
    )
grid.text(
    "b)", vp = viewport(layout.pos.row = 1, layout.pos.col = 2), 
    gp = gpar(fontsize = 11), vjust = -13, hjust = 12
    )
grid.text(
    "c)", vp = viewport(layout.pos.row = 1, layout.pos.col = 3), 
    gp = gpar(fontsize = 11), vjust = -13, hjust = 16.5
    )