## Local marine diversity change meta-analysis model results with 
# Diez et al. 2011 removed

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(metafor)
library(lme4)
library(lmerTest)
library(gridExtra)
library(knitr)

source('00_functions.R')

fl_combined_no_diez <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga') %>% 
  # Keller study 
  filter(Study.ID != '172') %>%
  # Study  136 - Enfermeria should have been classified as having an event - 
  # 'shrimp farming' and 'tidal restriction'
  filter(Site != 'Enfermeria') %>%  
  filter(Reference != 'Diez et al. 2011')

no_event_no_diez <- filter(fl_combined_no_diez, Event != 'Yes')

## @knitr duration-var-weighted-intercept-nodiez
mod1_no_diez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event_no_diez, 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_no_diez

## @knitr no-event-data-plot-with-duration-var-weight-regression-intercept-nodiez
duration_w_no_diez <- 
filter(no_event_no_diez, mod1_no_diez$not.na) %>%
ggplot(data = .) +
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 1) + 
  ylab('Log Ratio') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') +
  ggtitle('Weighted Analysis') + 
  xlab('Duration (years)') +
  ylim(-2, 2) + 
  geom_line(data = data.frame(x = 1:41, y = predict(mod1_no_diez, 1:41)$pred), aes(x = x, y = y), colour = 'blue') +
  geom_ribbon(data = data.frame(x = 1:41, ymin = predict(mod1_no_diez, 1:41)$ci.lb, ymax = predict(mod1_no_diez, 1:41)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.1)
duration_w_no_diez

## @knitr impacts-var-weighted-intercept-nodiez
impact_ne_w_no_diez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = filter(no_event_no_diez, Reference != 'Diez et al. 2011'),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_w_no_diez

## @knitr impacts-var-weighted-intercept-profile-plot-nodiez
profile(impact_ne_w_no_diez)


## @knitr cumulative-impacts-freq-dist-for-var-weighted-dataset-nodiez
ggplot(data = no_event_no_diez %>% filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)), 
       aes(x = mean_imps)) + 
  geom_histogram(binwidth = 1, colour = 'black', boundary = 0) + 
  scale_x_continuous(breaks = 0:10) + 
  theme_minimal() + 
  xlab('\nCumulative human impact score') + 
  ylab('Frequency\n')


## @knitr drivers-var-weighted-intercept-nodiez
drivers_scaled_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled_nodiez

## @knitr drivers-var-weighted-intercept-not-scaled-nodiez
drivers_unscaled_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts))
drivers_unscaled_nodiez

## @knitr drivers-var-weighted-int-scaled-par-est-plot-short-long-breakdown-nodiez
driver_summary_plot_short_nodiez  <- 
  mk_rma_summary_df(drivers_scaled_nodiez) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(interaction = c('yes', 'no', 'no', 'no', 'yes', 'yes', 'yes')) %>%
    filter(interaction == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-6.4, 6.4)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 14), 
            axis.text.x = element_text(size = 14), 
            axis.title = element_text(size = 16)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short_nodiez

driver_summary_plot_long_nodiez  <- 
  mk_rma_summary_df(drivers_scaled_nodiez) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(interaction = c('yes', 'no', 'no', 'no', 'yes', 'yes', 'yes')) %>%
    filter(interaction == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-1.2, 1.2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 14), 
            axis.text.x = element_text(size = 14), 
            axis.title = element_text(size = 16)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long_nodiez

grid.arrange(driver_summary_plot_short_nodiez, driver_summary_plot_long_nodiez, ncol = 2)

## @knitr drivers-quantile-specifications-nodiez
# ------------------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------------------
# Get range of durations used in drivers:
no_event_no_diez %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  .$Duration %>% 
  range(.)

# Get minimum impact value used in drivers:
invs_quantiles_nodiez <- 
  no_event_no_diez %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$mean_invs %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

nut_quantiles_nodiez <- 
  no_event_no_diez %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$mean_nuts %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

ltc_quantiles_nodiez <- 
  no_event_no_diez %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$sliced_ltc %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

## @knitr drivers-predictions-based-on-quantiles-nodiez

invs_q0_nodiez  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`0%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '0', 
                   value = invs_quantiles_nodiez$`0%`)
invs_q25_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`25%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '25', 
                   value = invs_quantiles_nodiez$`25%`)
invs_q50_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '50', 
                   value = invs_quantiles_nodiez$`50%`)
invs_q75_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`75%`,
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '75', 
                   value = invs_quantiles_nodiez$`75%`)
invs_q100_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`100%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '100', 
                   value = invs_quantiles_nodiez$`100%`)
invs_predictions_nodiez <- 
  rbind_all(list(invs_q0_nodiez, invs_q25_nodiez, invs_q50_nodiez, invs_q75_nodiez, invs_q100_nodiez))

nuts_q0_nodiez  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = nut_quantiles_nodiez$`0%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '0', 
                   value = nut_quantiles_nodiez$`0%`)
nuts_q25_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = nut_quantiles_nodiez$`25%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '25', 
                   value = nut_quantiles_nodiez$`25%`)
nuts_q50_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '50', 
                   value = nut_quantiles_nodiez$`50%`)
nuts_q75_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = nut_quantiles_nodiez$`75%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '75', 
                   value = nut_quantiles_nodiez$`75%`)
nuts_q100_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = nut_quantiles_nodiez$`100%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '100', 
                   value = nut_quantiles_nodiez$`100%`)
nuts_predictions_nodiez <- 
  rbind_all(list(nuts_q0_nodiez, nuts_q25_nodiez, nuts_q50_nodiez, nuts_q75_nodiez, nuts_q100_nodiez))

ltc_q0_nodiez  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles_nodiez$`0%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '0', value = ltc_quantiles_nodiez$`0%`)
ltc_q25_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles_nodiez$`25%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '25', value = ltc_quantiles_nodiez$`25%`)
ltc_q50_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '50', value = ltc_quantiles_nodiez$`50%`)
ltc_q75_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles_nodiez$`75%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '75', value = ltc_quantiles_nodiez$`75%`)
ltc_q100_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles_nodiez$`100%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '100', value = ltc_quantiles_nodiez$`100%`)
ltc_predictions_nodiez <- 
  rbind_all(list(ltc_q0_nodiez, ltc_q25_nodiez, ltc_q50_nodiez, ltc_q75_nodiez, ltc_q100_nodiez))


## @knitr drivers-predictions-based-on-median-values-nodiez
##### Drivers held constant at median values
invs_q0_nodiez  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`0%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '0', 
                   value = invs_quantiles_nodiez$`0%`)
invs_q25_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`25%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '25', 
                   value = invs_quantiles_nodiez$`25%`)
invs_q50_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '50', 
                   value = invs_quantiles_nodiez$`50%`)
invs_q75_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`75%`,
                                   nuts = nut_quantiles_nodiez$`50%`, temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '75', 
                   value = invs_quantiles_nodiez$`75%`)
invs_q100_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`100%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '100', 
                   value = invs_quantiles_nodiez$`100%`)
invs_predictions <- 
  rbind_all(list(invs_q0, invs_q25, invs_q50, invs_q75, invs_q100))

nuts_q0  _nodiez<- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`0%`, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '0', 
                   value = nut_quantiles_nodiez$`0%`)
nuts_q25 _nodiez<- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`25%`, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '25', 
                   value = nut_quantiles_nodiez$`25%`)
nuts_q50 _nodiez<- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '50', 
                   value = nut_quantiles_nodiez$`50%`)
nuts_q75 _nodiez<- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`75%`, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '75', 
                   value = nut_quantiles_nodiez$`75%`)
nuts_q100_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`100%`, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '100', 
                   value = nut_quantiles_nodiez$`100%`)
nuts_predictions <- 
  rbind_all(list(nuts_q0, nuts_q25, nuts_q50, nuts_q75, nuts_q100))

_nodiezltc_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = ltc_quantiles_nodiez$`0%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '0', value = ltc_quantiles_nodiez$`0%`)
ltc_q25_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = ltc_quantiles_nodiez$`25%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '25', value = ltc_quantiles_nodiez$`25%`)
ltc_q50_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = ltc_quantiles_nodiez$`50%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '50', value = ltc_quantiles_nodiez$`50%`)
ltc_q75_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = ltc_quantiles_nodiez$`75%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '75', value = ltc_quantiles_nodiez$`75%`)
ltc_q100_nodiez <- get_driver_predictions(unscaled_rma_object = drivers_unscaled_nodiez, 
                                   invs = invs_quantiles_nodiez$`50%`, 
                                   nuts = nut_quantiles_nodiez$`50%`, 
                                   temp = ltc_quantiles_nodiez$`100%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '100', value = ltc_quantiles_nodiez$`100%`)
ltc_predictions_nodiez <- 
  rbind_all(list(ltc_q0_nodiez, ltc_q25_nodiez, ltc_q50_nodiez, ltc_q75_nodiez, ltc_q100_nodiez))


all_driver_predictions_nodiez <- 
  rbind_all(list(invs_predictions_nodiez, nuts_predictions_nodiez, ltc_predictions_nodiez)) %>% 
  mutate(quantile = factor(quantile, levels = c('0', '25', '50', '75', '100'))) %>% 
  mutate(value = signif(value, digits = 3)) %>% 
  mutate(value = round(value, digits = 2))

## @knitr full-drivers-model-var-weighted-predictions-driver-facet-nodiez
set.seed(5)
all_driver_predictions_nodiez %>% 
  filter(quantile %in% c('0', '25', '75')) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_text(data = filter(all_driver_predictions_nodiez, quantile %in% c('0', '25', '75')) %>% 
                   group_by(driver, quantile) %>% 
                   slice(which.max(duration)), 
            aes(x = duration + 1, y = pred, label = value, colour = quantile),
            vjust = 0.25, hjust = 0, position = position_jitter(width = 0, height = 0.06), 
            size = 3) +
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  theme_minimal() +
  xlim(c(0, 20)) + 
  facet_wrap(~ driver, scale = 'free_y')


## @knitr full-drivers-model-var-weighted-invasives-predictions-nodiez

quantiles <- c('0', '25', '75')
invs_predictions_0_25_75_nodiez <- 
all_driver_predictions_nodiez %>% 
  filter(driver == 'invasives') %>% 
  filter(quantile %in% quantiles) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  scale_fill_discrete(labels = paste0('q', quantiles, ': ', all_driver_predictions_nodiez %>% 
  filter(driver == 'invasives' & quantile %in% quantiles) %>% group_by(quantile) %>% 
  slice(1) %>% .$value)) +
  guides(colour = FALSE) +
  theme_minimal() + 
  xlab('\nDuration') + 
  ylab('Predicted log ratio\n') + 
  ggtitle('Invasion potential')
invs_predictions_0_25_75_nodiez

## @knitr full-drivers-model-var-weighted-nutrients-predictions-nodiez

quantiles <- c('0', '25', '75')
nuts_predictions_0_25_75_nodiez <- 
all_driver_predictions_nodiez %>% 
  filter(driver == 'nutrients') %>% 
  filter(quantile %in% quantiles) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  scale_fill_discrete(labels = paste0('q', quantiles, ': ', all_driver_predictions_nodiez %>% 
  filter(driver == 'nutrients' & quantile %in% quantiles) %>% group_by(quantile) %>% 
  slice(1) %>% .$value)) +
  guides(colour = FALSE) +
  theme_minimal() + 
  xlab('\nDuration') + 
  ylab('Predicted log ratio\n') + 
  ggtitle('Nutrient addition')
nuts_predictions_0_25_75_nodiez

## @knitr full-drivers-model-var-weighted-ltc-predictions-nodiez

quantiles <- c('0', '25', '75')
ltc_predictions_0_25_75 <- 
all_driver_predictions_nodiez %>% 
  filter(driver == 'ltc') %>% 
  filter(quantile %in% quantiles) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  scale_fill_discrete(labels = paste0('q', quantiles, ': ', all_driver_predictions_nodiez %>% 
  filter(driver == 'ltc' & quantile %in% quantiles) %>% group_by(quantile) %>% 
  slice(1) %>% .$value)) +
  guides(colour = FALSE) +
  theme_minimal() +
  xlab('\nDuration') + 
  ylab('Predicted log ratio\n') + 
  ggtitle('Linear temperature change')

## @knitr invasions-var-weighted-0-25-75-predictions-nodiez
invs_predictions_0_25_75_nodiez

## @knitr nutrients-var-weighted-0-25-75-predictions-nodiez
nuts_predictions_0_25_75_nodiez

## @knitr ltc-var-weighted-0-25-75-predictions-nodiez
ltc_predictions_0_25_75_nodiez


# Distribution of global drivers observed within our dataset
# ------------------------------------------------------------------------------

## @knitr cumul-freq-and-hists-of-invs-nuts-ltc-in-data-nodiez
driver_cum_freq_nodiez <- 
  no_event_no_diez %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    gather(key = driver, value = driver_value, mean_invs, mean_nuts, sliced_ltc) %>% 
  ggplot(data = ., aes(x = driver_value)) + 
    stat_ecdf() + 
    theme_minimal() + 
    xlab('\nDriver value') + 
    ylab('Cumulative frequency\n') + 
    facet_wrap( ~ driver, scales = 'free')


driver_hist_nodiez <- 
  no_event_no_diez %>% 
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

grid.arrange(driver_cum_freq_nodiez, driver_hist_nodiez, nrow = 2)


# ------------------------------------------------------------------------------
# Specific drivers and their interactions - variance weighted
# ------------------------------------------------------------------------------

## @knitr drivers-interactions-var-weighted

# Invasives * temperature (Sorte et al 2010, )
# Nutrients * temperature (Binzer et al 2012)
interactions_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))

# Likely low power, but at least the model does not appear to be 
# overparameterised. 
profile(interactions_nodiez)

interactions_nodiez

## @knitr drivers-interactions-samp-size-weighted
interactions_ss_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event_no_diez %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_nuts)))$n1, 
         data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))
interactions_ss_nodiez 

robust(interactions_ss_nodiez, cluster=1:interactions_ss_nodiez$k)

## @knitr drivers-interactions-samp-size-weighted
interactions_uw_nodiez <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + 
         scale(sliced_ltc)*scale(mean_nuts)) + (1 | Study.ID), 
       data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
       na.action = na.omit)
summary(interactions_uw_nodiez)


# ------------------------------------------------------------------------------
# Sample size weighted analysis:
# ------------------------------------------------------------------------------

## @knitr duration-samp-size-weighted-model-output-nodiez
mod1_ss_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event_no_diez %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)))$n1, 
         data = no_event_no_diez %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss_nodiez
mod1_ss_robust_nodiez <- robust(mod1_ss_nodiez, cluster=1:mod1_ss_nodiez$k)
mod1_ss_robust_nodiez

## @knitr impacts-samp-size-weighted-model-output-nodiez
impact_ne_ss_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event_no_diez %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)))$n1, 
         data = no_event_no_diez %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_ne_ss_nodiez
impact_ne_ss_robust_nodiez <- robust(impact_ne_ss_nodiez, cluster=1:impact_ne_ss_nodiez$k)
impact_ne_ss_robust_nodiez

## @knitr drivers-samp-size-weighted-model-output-nodiez
drivers_scaled_ss_nodiez <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event_no_diez %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_nuts)))$n1, 
         data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_scaled_ss_nodiez
drivers_scaled_ss_robust_nodiez <- robust(drivers_scaled_ss_nodiez, cluster=1:drivers_scaled_ss_nodiez$k)
drivers_scaled_ss_robust_nodiez


# ------------------------------------------------------------------------------
# Unweighted analysis - lmer
# ------------------------------------------------------------------------------
# Unweighted analysis that includes all of the log ratios that do not have 
# variance associated with them. 

## @knitr duration-unweighted-model-output-nodiez
lme1_nodiez <- lmer(yi_SppR_ROM ~ Duration + (1 | Study.ID), 
            data = no_event_no_diez, na.action = na.omit)
summary(lme1_nodiez)

## @knitr impact-unweighted-model-output-nodiez
impact_lme_ne_nodiez <- 
  lmer(yi_SppR_ROM ~ Duration * (mean_imps) + (1 | Study.ID), 
       data = no_event_no_diez, na.action = na.omit)
summary(impact_lme_ne_nodiez)

## @knitr drivers-unweighted-model-output-nodiez
drivers_lme_ne_nodiez <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + 
       scale(mean_nuts)) + (1 | Study.ID),
       data = no_event_no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
       na.action = na.omit)
summary(drivers_lme_ne_nodiez)

# ------------------------------------------------------------------------------
# Comparison of model output tables
# ------------------------------------------------------------------------------

# Variance weighted 
# ------------------------------------------------------------------------------

## @knitr duration-var-weighted-model-output-df-nodiez
mod1_output_df_nodiez <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_no_diez$b), 
             estimate = round(as.vector(mod1_no_diez$b), digits = 3), 
             se = round(mod1_no_diez$se, digits = 3), 
             pval = round(mod1_no_diez$pval, digits = 3), 
             ci.lb = round(mod1_no_diez$ci.lb, digits = 3), 
             ci.ub = round(mod1_no_diez$ci.ub, digits = 3), 
             k = mod1_no_diez$k, 
             n = mod1_no_diez$s.nlevels)
mod1_output_df_nodiez

## @knitr impact-var-weighted-model-output-df-nodiez
imp_mod_output_df_nodiez <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_w_no_diez$b), 
             estimate = round(as.vector(impact_ne_w_no_diez$b), digits = 3), 
             se = round(impact_ne_w_no_diez$se, digits = 3), 
             pval = round(impact_ne_w_no_diez$pval, digits = 3), 
             ci.lb = round(impact_ne_w_no_diez$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_w_no_diez$ci.ub, digits = 3), 
             k = impact_ne_w_no_diez$k, 
             n = impact_ne_w_no_diez$s.nlevels)
imp_mod_output_df_nodiez

## @knitr drivers-var-weighted-model-output-df-scaled-nodiez
drivers_mod_output_df_nodiez <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_nodiez$b)), 
             estimate = round(as.vector(drivers_scaled_nodiez$b), digits = 3), 
             se = round(drivers_scaled_nodiez$se, digits = 3), 
             pval = round(drivers_scaled_nodiez$pval, digits = 3), 
             ci.lb = round(drivers_scaled_nodiez$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_nodiez$ci.ub, digits = 3), 
             k = drivers_scaled_nodiez$k, 
             n = drivers_scaled_nodiez$s.nlevels)
drivers_mod_output_df_nodiez


# Sample size weighted 
# ------------------------------------------------------------------------------

## @knitr duration-var-weighted-model-output-df-nodiez
mod1_ss_output_df_nodiez <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_ss_robust_nodiez$b), 
             estimate = round(as.vector(mod1_ss_robust_nodiez$b), digits = 3), 
             se = round(mod1_ss_robust_nodiez$se, digits = 3), 
             pval = round(mod1_ss_robust_nodiez$pval, digits = 3), 
             ci.lb = round(mod1_ss_robust_nodiez$ci.lb, digits = 3), 
             ci.ub = round(mod1_ss_robust_nodiez$ci.ub, digits = 3), 
             k = mod1_ss_nodiez$k, 
             n = mod1_ss_nodiez$s.nlevels)
mod1_ss_output_df_nodiez

## @knitr impact-var-weighted-model-output-df-nodiez
imp_mod_ss_output_df_nodiez <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_ss_nodiez$b), 
             estimate = round(as.vector(impact_ne_ss_nodiez$b), digits = 3), 
             se = round(impact_ne_ss_nodiez$se, digits = 3), 
             pval = round(impact_ne_ss_nodiez$pval, digits = 3), 
             ci.lb = round(impact_ne_ss_nodiez$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_ss_nodiez$ci.ub, digits = 3), 
             k = impact_ne_ss_nodiez$k, 
             n = impact_ne_ss_nodiez$s.nlevels)
imp_mod_ss_output_df_nodiez

## @knitr drivers-var-weighted-model-output-df-scaled-nodiez
drivers_mod_ss_output_df_nodiez <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_ss$b)), 
             estimate = round(as.vector(drivers_scaled_ss$b), digits = 3), 
             se = round(drivers_scaled_ss$se, digits = 3), 
             pval = round(drivers_scaled_ss$pval, digits = 3), 
             ci.lb = round(drivers_scaled_ss$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_ss$ci.ub, digits = 3), 
             k = drivers_scaled_ss$k, 
             n = drivers_scaled_ss$s.nlevels)
drivers_mod_ss_output_df_nodiez

# Unweighted - standard lmer
# ------------------------------------------------------------------------------

## @knitr duration-unweighted-model-output-df-nodiez
lme1_output_df_nodiez <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(coef(summary(lme1_nodiez))), 
             estimate = as.vector(coef(summary(lme1_nodiez))[, 1]), 
             se = as.vector(coef(summary(lme1_nodiez))[, 2]),
             pval = as.vector(coef(summary(lme1_nodiez))[, 5]), 
             ci.lb = as.vector(confint(lme1_nodiez)[-(1:2), 1]), 
             ci.ub = as.vector(confint(lme1_nodiez)[-(1:2), 2]), 
             k = lme1_nodiez@pp$Zt@Dim[[2]], 
             n = lme1_nodiez@pp$Zt@Dim[[1]]
  )
lme1_output_df_nodiez

## @knitr impact-unweighted-model-output-df-nodiez
impact_lme_output_df_nodiez <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(coef(summary(impact_lme_ne_nodiez))), 
             estimate = as.vector(coef(summary(impact_lme_ne_nodiez))[, 1]), 
             se = as.vector(coef(summary(impact_lme_ne_nodiez))[, 2]),
             pval = as.vector(coef(summary(impact_lme_ne_nodiez))[, 5]), 
             ci.lb = as.vector(confint(impact_lme_ne_nodiez)[-(1:2), 1]), 
             ci.ub = as.vector(confint(impact_lme_ne_nodiez)[-(1:2), 2]), 
             k = impact_lme_ne_nodiez@pp$Zt@Dim[[2]], 
             n = impact_lme_ne_nodiez@pp$Zt@Dim[[1]]
  )
impact_lme_output_df_nodiez

## @knitr drivers-unweighted-model-output-df-nodiez
drivers_lme_output_df_nodiez <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(coef(summary(drivers_lme_ne_nodiez)))), 
             estimate = as.vector(coef(summary(drivers_lme_ne_nodiez))[, 1]), 
             se = as.vector(coef(summary(drivers_lme_ne_nodiez))[, 2]),
             pval = as.vector(coef(summary(drivers_lme_ne_nodiez))[, 5]), 
             ci.lb = as.vector(confint(drivers_lme_ne_nodiez)[-(1:2), 1]), 
             ci.ub = as.vector(confint(drivers_lme_ne_nodiez)[-(1:2), 2]), 
             k = drivers_lme_ne_nodiez@pp$Zt@Dim[[2]], 
             n = drivers_lme_ne_nodiez@pp$Zt@Dim[[1]]
             )
drivers_lme_output_df_nodiez


## @knitr sample-size-diffs-based-on-mod-weighting-nodiez
data_frame(model = c('LR ~ D', 'LR ~ D * Imp', 'LR ~ D * (I + N + LTC)'), 
           `var-w sites` = c(mod1_no_diez$k, impact_ne_w_no_diez$k, drivers_scaled_nodiez$k), 
           `var-w studies` = c(mod1_no_diez$s.nlevels, impact_ne_w_no_diez$s.nlevels, drivers_scaled_nodiez$s.nlevels), 
           `samp-w sites` = c(mod1_ss_nodiez$k, impact_ne_ss_nodiez$k, drivers_scaled_ss_nodiez$k), 
           `samp-w studies` = c(mod1_ss_nodiez$s.nlevels, impact_ne_ss_nodiez$s.nlevels, drivers_scaled_ss_nodiez$s.nlevels), 
           `un-w sites` = c(lme1_nodiez@pp$Zt@Dim[[2]], impact_lme_ne_nodiez@pp$Zt@Dim[[2]], drivers_lme_ne_nodiez@pp$Zt@Dim[[2]]), 
           `un-w studies` = c(lme1_nodiez@pp$Zt@Dim[[1]], impact_lme_ne_nodiez@pp$Zt@Dim[[1]], drivers_lme_ne_nodiez@pp$Zt@Dim[[1]]))

knitr::kable(., digits = 3, caption = '**Table 2.** Variance-weighted (VW) and unweighted (UW) meta-regression of the proportion of species richness change across a gradient of three drivers (raw coefficient estimates, except for invasives --> invs * 0.001) of global change over time including the estimated percent change per unit of the driver and the corresponding model p-value. The percent change has been back-calculated from the estimated log ratio of the proportion of change in species richness.')