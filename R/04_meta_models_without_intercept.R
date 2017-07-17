# TO DO: rewrite to remove intercepts.

## Local marine diversity change meta-analysis model results

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

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga')

no_event <- filter(fl_combined, Event != 'Yes')

## @knitr duration-var-weighted-no-int
mod1_no_int <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event, 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration - 1)
mod1_no_int

## @knitr no-event-data-plotted-with-duration-var-weighted-regression-no-int
duration_w_no_int <- 
filter(no_event, mod1_no_int$not.na) %>%
ggplot(data = .) +
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 1) + 
  ylab('Log Ratio') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') +
  ggtitle('Weighted Analysis') + 
  xlab('Duration (years)') +
  ylim(-2, 2) + 
  geom_line(data = data.frame(x = 1:41, y = predict(mod1_no_int, 1:41)$pred), aes(x = x, y = y), colour = 'blue') +
  geom_ribbon(data = data.frame(x = 1:41, ymin = predict(mod1_no_int, 1:41)$ci.lb, ymax = predict(mod1_no_int, 1:41)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.1)

duration_w_no_int


## @knitr impacts-var-weighted-no-int
impact_ne_w_no_int <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps - 1)
impact_ne_w_no_int

## @knitr impacts-var-weighted-intercept-profile-plot-no-int
profile(impact_ne_w_no_int)

## @knitr impacts-var-weighted-intercept-coefficient-estimates-no-int
mk_rma_summary_df(impact_ne_w_no_int) %>%
  mutate(driver = gsub("mean_imps", "Human Impact", driver)) %>%
  mutate(driver = factor(driver, levels = c('Duration:Human Impact', 'Duration', 'Human Impact'))) %>%
  ggplot(data = ., aes(x = estimate, y = driver)) + 
    theme_bw() +
    geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 0.8) + 
    geom_point(size = 3) + 
    geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
    xlim(c(-0.12, 0.12)) + 
    theme(axis.text.y = element_text(hjust = 1, size = 14), 
          axis.text.x = element_text(size = 14), 
          axis.title = element_text(size = 16)) + 
    ylab("") + 
    xlab('\nCoefficient estimate')


## @knitr impacts-var-weighted-predictions-no-int
imp_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)) %>% 
    .$mean_imps %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

q0_imps_no_int <- get_imp_predictions(rma_object = impact_ne_w_no_int, impact_value = imp_quantiles$`0`, duration = 15) %>% 
           mutate(driver = 'impact', quantile = '0', value = round(imp_quantiles$`0`, 2))
q25_imps_no_int <- get_imp_predictions(rma_object = impact_ne_w_no_int, impact_value = imp_quantiles$`25`, duration = 15) %>% 
            mutate(driver = 'impact', quantile = '25', value = round(imp_quantiles$`25`, 2))
q50_imps_no_int <- get_imp_predictions(rma_object = impact_ne_w_no_int, impact_value = imp_quantiles$`50`, duration = 15) %>% 
            mutate(driver = 'impact', quantile = '50', value = round(imp_quantiles$`50`, 2))
q75_imps_no_int <- get_imp_predictions(rma_object = impact_ne_w_no_int, impact_value = imp_quantiles$`75`, duration = 15) %>% 
            mutate(driver = 'impact', quantile = '75', value = round(imp_quantiles$`75`, 2))
q100_imps_no_int <- get_imp_predictions(rma_object = impact_ne_w_no_int, impact_value = imp_quantiles$`100`, duration = 15) %>% 
             mutate(driver = 'impact', quantile = '100', value = round(imp_quantiles$`100`, 2))

imp_predictions_no_int <- 
  rbind_all(list(q0_imps_no_int, q25_imps_no_int, q50_imps_no_int, q75_imps_no_int, q100_imps_no_int)) %>% 
  mutate(quantile = factor(quantile, levels = c('0', '25', '50', '75', '100')))

## @knitr impacts-var-weighted-predicted-log-ratio-no-int
impact_predicted_log_ratio_no_int <- 
  ggplot(data = imp_predictions_no_int, aes(x = duration, y = pred)) + 
    geom_line(aes(x = duration, y = pred, colour = quantile)) + 
    geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.3) + 
    geom_text(data = group_by(imp_predictions_no_int, quantile) %>% 
                     slice(which.max(duration)), 
              aes(x = duration + 1, y = pred, vjust = 0.4, colour = quantile, label = value)) +
    scale_colour_manual(values = c('blue', 'green', 'gold', 'orange', 'red')) + 
    scale_fill_manual(values = c('light blue', 'light green', 'light goldenrod', 
                                 'moccasin', 'light coral')) +
    theme_minimal() + 
    theme(legend.position = 'none') + 
    ylab('Predicted proportion of change') + 
    xlab('Duration (years)') + 
    geom_hline(yintercept = 0, colour = 'black', size = 0.15) + 
    geom_vline(xintercept = 0, colour = 'black', size = 0.15)

## @knitr impacts-var-weighted-observed-log-ratio-no-int
observed_log_ratio_no_int <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)) %>% 
    ggplot(data = .) + 
      geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = Study.ID)) + 
      theme_minimal() + 
      theme(legend.position = 'none') + 
      geom_hline(yintercept = 0, colour = 'black', size = 0.15) + 
      geom_vline(xintercept = 0, colour = 'black', size = 0.15) + 
      ylab('Log ratio') + 
      xlab('Duration (years)')

## @knitr impacts-var-weighted-predicted-percent-change-no-int
impact_predicted_percent_change_no_int <- 
  ggplot(data = imp_predictions_no_int, aes(x = duration, y = get_percent_change(pred))) + 
    geom_line(aes(x = duration, y = get_percent_change(pred), colour = quantile)) + 
    geom_ribbon(aes(x = duration, ymin = get_percent_change(ci.lb), 
                    ymax = get_percent_change(ci.ub), fill = quantile), 
                alpha = 0.3) + 
    geom_text(data = group_by(imp_predictions_no_int, quantile) %>% 
                     slice(which.max(duration)), 
              aes(x = duration + 1, y = get_percent_change(pred), vjust = 0.4, colour = quantile, label = value)) +
    scale_colour_manual(values = c('blue', 'green', 'gold', 'orange', 'red')) + 
    scale_fill_manual(values = c('light blue', 'light green', 'light goldenrod', 
                                 'moccasin', 'light coral')) +
    theme_minimal() + 
    theme(legend.position = 'none') + 
    ylab('Predicted percent change\n') + 
    xlab('\nDuration (years)') + 
    geom_hline(yintercept = 0, colour = 'black', size = 0.15) + 
    geom_vline(xintercept = 0, colour = 'black', size = 0.15) + 
    scale_x_continuous(breaks = seq(from = 0, to = 15, by = 5))

## @knitr impacts-var-weighted-observed-percent-change-no-int
observed_percent_change_no_int <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)) %>% 
    ggplot(data = .) + 
      geom_point(aes(x = Duration, y = get_percent_change(yi_SppR_ROM), colour = Study.ID)) + 
      theme_minimal() + 
      theme(legend.position = 'none') + 
      geom_hline(yintercept = 0, colour = 'black', size = 0.15) + 
      geom_vline(xintercept = 0, colour = 'black', size = 0.15) + 
      ylab('Percent change') + 
      xlab('Duration (years)') + 
      scale_x_continuous(breaks = seq(from = 0, to = 15, by = 5))

## @knitr impacts-var-weighted-prediction-plots-log-ratio-no-int
grid.arrange(observed_log_ratio, impact_predicted_log_ratio_no_int, ncol = 2)

## @knitr impacts-var-weighted-prediction-plots-log-ratio-no-int
grid.arrange(observed_percent_change_no_int, impact_predicted_percent_change_no_int, ncol = 2)

## @knitr drivers-var-weighted-no-int
drivers_scaled_no_int <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)) - 1)
drivers_scaled_no_int

## @knitr drivers-var-weighted-intercept-not-scaled
drivers_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts))
drivers_unscaled


## @knitr drivers-var-weighted-intercept-scaled-par-est-plot-no-facets
model_driver_vector <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration*Invasives', 
    'Duration*LTC', 'Duration*Nutrients')
ordered_driver_vector <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration*Invasives', 
    'Duration*Nutrients', 'Duration*LTC')

driver_summary_plot <- 
  mk_rma_summary_df(drivers_scaled_no_int) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-2, 2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 14), 
            axis.text.x = element_text(size = 14), 
            axis.title = element_text(size = 16)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')

driver_summary_plot

## @knitr drivers-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown
driver_summary_plot_short  <- 
  mk_rma_summary_df(drivers_scaled_no_int) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(interaction = c('no', 'no', 'no', 'no', 'yes', 'yes', 'yes')) %>%
    filter(interaction == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-2, 2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 14), 
            axis.text.x = element_text(size = 14), 
            axis.title = element_text(size = 16)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short

driver_summary_plot_long  <- 
  mk_rma_summary_df(drivers_scaled_no_int) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(interaction = c('no', 'no', 'no', 'no', 'yes', 'yes', 'yes')) %>%
    filter(interaction == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.2, 0.2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 14), 
            axis.text.x = element_text(size = 14), 
            axis.title = element_text(size = 16)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long

grid.arrange(driver_summary_plot_short, driver_summary_plot_long, ncol = 2)

## @knitr drivers-quantile-specifications
# ------------------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------------------
# Get range of durations used in drivers:
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  .$Duration %>% 
  range(.)

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

## @knitr drivers-predictions-based-on-quantiles

invs_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`0%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '0', 
                   value = invs_quantiles$`0%`)
invs_q25 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`25%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '25', 
                   value = invs_quantiles$`25%`)
invs_q50 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '50', 
                   value = invs_quantiles$`50%`)
invs_q75 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`75%`,
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '75', 
                   value = invs_quantiles$`75%`)
invs_q100 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`100%`, 
                                   nuts = 0, temp = 0, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '100', 
                   value = invs_quantiles$`100%`)
invs_predictions <- 
  rbind_all(list(invs_q0, invs_q25, invs_q50, invs_q75, invs_q100))

nuts_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`0%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '0', 
                   value = nut_quantiles$`0%`)
nuts_q25 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`25%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '25', 
                   value = nut_quantiles$`25%`)
nuts_q50 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`50%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '50', 
                   value = nut_quantiles$`50%`)
nuts_q75 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`75%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '75', 
                   value = nut_quantiles$`75%`)
nuts_q100 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`100%`, 
                                   temp = 0, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '100', 
                   value = nut_quantiles$`100%`)
nuts_predictions <- 
  rbind_all(list(nuts_q0, nuts_q25, nuts_q50, nuts_q75, nuts_q100))

ltc_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`0%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '0', value = ltc_quantiles$`0%`)
ltc_q25 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`25%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '25', value = ltc_quantiles$`25%`)
ltc_q50 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '50', value = ltc_quantiles$`50%`)
ltc_q75 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`75%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '75', value = ltc_quantiles$`75%`)
ltc_q100 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`100%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '100', value = ltc_quantiles$`100%`)
ltc_predictions <- 
  rbind_all(list(ltc_q0, ltc_q25, ltc_q50, ltc_q75, ltc_q100))


## @knitr drivers-predictions-based-on-median-values
##### Drivers held constant at median values
invs_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`0%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '0', 
                   value = invs_quantiles$`0%`)
invs_q25 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`25%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '25', 
                   value = invs_quantiles$`25%`)
invs_q50 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '50', 
                   value = invs_quantiles$`50%`)
invs_q75 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`75%`,
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '75', 
                   value = invs_quantiles$`75%`)
invs_q100 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`100%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'invasives', quantile = '100', 
                   value = invs_quantiles$`100%`)
invs_predictions <- 
  rbind_all(list(invs_q0, invs_q25, invs_q50, invs_q75, invs_q100))

nuts_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`0%`, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '0', 
                   value = nut_quantiles$`0%`)
nuts_q25 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`25%`, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '25', 
                   value = nut_quantiles$`25%`)
nuts_q50 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '50', 
                   value = nut_quantiles$`50%`)
nuts_q75 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`75%`, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '75', 
                   value = nut_quantiles$`75%`)
nuts_q100 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`100%`, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'nutrients', quantile = '100', 
                   value = nut_quantiles$`100%`)
nuts_predictions <- 
  rbind_all(list(nuts_q0, nuts_q25, nuts_q50, nuts_q75, nuts_q100))

ltc_q0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`0%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '0', value = ltc_quantiles$`0%`)
ltc_q25 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`25%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '25', value = ltc_quantiles$`25%`)
ltc_q50 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`50%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '50', value = ltc_quantiles$`50%`)
ltc_q75 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`75%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '75', value = ltc_quantiles$`75%`)
ltc_q100 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`100%`, duration = 15) %>%
            mutate(driver = 'ltc', quantile = '100', value = ltc_quantiles$`100%`)
ltc_predictions <- 
  rbind_all(list(ltc_q0, ltc_q25, ltc_q50, ltc_q75, ltc_q100))


all_driver_predictions <- 
  rbind_all(list(invs_predictions, nuts_predictions, ltc_predictions)) %>% 
  mutate(quantile = factor(quantile, levels = c('0', '25', '50', '75', '100'))) %>% 
  mutate(value = signif(value, digits = 3)) %>% 
  mutate(value = round(value, digits = 2))

## @knitr full-drivers-model-var-weighted-predictions-driver-facet
set.seed(5)
all_driver_predictions %>% 
  filter(quantile %in% c('0', '25', '75')) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_text(data = filter(all_driver_predictions, quantile %in% c('0', '25', '75')) %>% 
                   group_by(driver, quantile) %>% 
                   slice(which.max(duration)), 
            aes(x = duration + 1, y = pred, label = value, colour = quantile),
            vjust = 0.25, hjust = 0, position = position_jitter(width = 0, height = 0.06), 
            size = 3) +
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  theme_minimal() +
  xlim(c(0, 20)) + 
  facet_wrap(~ driver, scale = 'free_y')


## @knitr full-drivers-model-var-weighted-invasives-predictions

quantiles <- c('0', '25', '75')
invs_predictions_0_25_75 <- 
all_driver_predictions %>% 
  filter(driver == 'invasives') %>% 
  filter(quantile %in% quantiles) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  scale_fill_discrete(labels = paste0('q', quantiles, ': ', all_driver_predictions %>% 
  filter(driver == 'invasives' & quantile %in% quantiles) %>% group_by(quantile) %>% 
  slice(1) %>% .$value)) +
  guides(colour = FALSE) +
  theme_minimal() + 
  xlab('\nDuration') + 
  ylab('Predicted log ratio\n') + 
  ggtitle('Invasion potential')
invs_predictions_0_25_75

## @knitr full-drivers-model-var-weighted-nutrients-predictions

quantiles <- c('0', '25', '75')
nuts_predictions_0_25_75 <- 
all_driver_predictions %>% 
  filter(driver == 'nutrients') %>% 
  filter(quantile %in% quantiles) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  scale_fill_discrete(labels = paste0('q', quantiles, ': ', all_driver_predictions %>% 
  filter(driver == 'nutrients' & quantile %in% quantiles) %>% group_by(quantile) %>% 
  slice(1) %>% .$value)) +
  guides(colour = FALSE) +
  theme_minimal() + 
  xlab('\nDuration') + 
  ylab('Predicted log ratio\n') + 
  ggtitle('Nutrient addition')
nuts_predictions_0_25_75

## @knitr full-drivers-model-var-weighted-ltc-predictions

quantiles <- c('0', '25', '75')
ltc_predictions_0_25_75 <- 
all_driver_predictions %>% 
  filter(driver == 'ltc') %>% 
  filter(quantile %in% quantiles) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = pred, colour = quantile)) + 
  geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.4) + 
  scale_fill_discrete(labels = paste0('q', quantiles, ': ', all_driver_predictions %>% 
  filter(driver == 'ltc' & quantile %in% quantiles) %>% group_by(quantile) %>% 
  slice(1) %>% .$value)) +
  guides(colour = FALSE) +
  theme_minimal() +
  xlab('\nDuration') + 
  ylab('Predicted log ratio\n') + 
  ggtitle('Linear temperature change')

## @knitr invasions-var-weighted-0-25-75-predictions
invs_predictions_0_25_75

## @knitr nutrients-var-weighted-0-25-75-predictions
nuts_predictions_0_25_75

## @knitr ltc-var-weighted-0-25-75-predictions
ltc_predictions_0_25_75


# Distribution of global drivers observed within our dataset
# ------------------------------------------------------------------------------

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

grid.arrange(driver_cum_freq, driver_hist, nrow = 2)


# ------------------------------------------------------------------------------
# Specific drivers and their interactions - variance weighted
# ------------------------------------------------------------------------------

## @knitr drivers-interactions-var-weighted

# Invasives * temperature (Sorte et al 2010, )
# Nutrients * temperature (Binzer et al 2012)
interactions <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))

# Likely low power, but at least the model does not appear to be 
# overparameterised. 
profile(interactions)

interactions

## @knitr drivers-interactions-samp-size-weighted
interactions_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_nuts)))$n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))
interactions_ss 

robust(interactions_ss, cluster=1:interactions_ss$k)

## @knitr drivers-interactions-samp-size-weighted
interactions_uw <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + 
         scale(sliced_ltc)*scale(mean_nuts)) + (1 | Study.ID), 
       data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
       na.action = na.omit)
summary(interactions_uw)


# ------------------------------------------------------------------------------
# Sample size weighted analysis:
# ------------------------------------------------------------------------------

## @knitr duration-samp-size-weighted-model-output
mod1_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)))$n1, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss
mod1_ss_robust <- robust(mod1_ss, cluster=1:mod1_ss$k)
mod1_ss_robust

## @knitr impacts-samp-size-weighted-model-output
impact_ne_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)))$n1, 
         data = no_event %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_ne_ss
impact_ne_ss_robust <- robust(impact_ne_ss, cluster=1:impact_ne_ss$k)
impact_ne_ss_robust

## @knitr drivers-samp-size-weighted-model-output
drivers_scaled_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / (no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_nuts)))$n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_scaled_ss
drivers_scaled_ss_robust <- robust(drivers_scaled_ss, cluster=1:drivers_scaled_ss$k)
drivers_scaled_ss_robust


# ------------------------------------------------------------------------------
# Unweighted analysis - lmer
# ------------------------------------------------------------------------------
# Unweighted analysis that includes all of the log ratios that do not have 
# variance associated with them. 

## @knitr duration-unweighted-model-output
lme1 <- lmer(yi_SppR_ROM ~ Duration + (1 | Study.ID), 
            data = no_event, na.action = na.omit)
summary(lme1)

## @knitr impact-unweighted-model-output
impact_lme_ne <- 
  lmer(yi_SppR_ROM ~ Duration * (mean_imps) + (1 | Study.ID), 
       data = no_event, na.action = na.omit)
summary(impact_lme_ne)

## @knitr drivers-unweighted-model-output
drivers_lme_ne <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + 
       scale(mean_nuts)) + (1 | Study.ID),
       data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
       na.action = na.omit)
summary(drivers_lme_ne)

# ------------------------------------------------------------------------------
# Comparison of model output tables
# ------------------------------------------------------------------------------

# Variance weighted 
# ------------------------------------------------------------------------------

## @knitr duration-var-weighted-model-output-df
mod1_output_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_no_int$b), 
             estimate = round(as.vector(mod1_no_int$b), digits = 3), 
             se = round(mod1_no_int$se, digits = 3), 
             pval = round(mod1_no_int$pval, digits = 3), 
             ci.lb = round(mod1_no_int$ci.lb, digits = 3), 
             ci.ub = round(mod1_no_int$ci.ub, digits = 3), 
             k = mod1_no_int$k, 
             n = mod1_no_int$s.nlevels)
mod1_output_df

## @knitr impact-var-weighted-model-output-df
imp_mod_output_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_w_no_int$b), 
             estimate = round(as.vector(impact_ne_w_no_int$b), digits = 3), 
             se = round(impact_ne_w_no_int$se, digits = 3), 
             pval = round(impact_ne_w_no_int$pval, digits = 3), 
             ci.lb = round(impact_ne_w_no_int$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_w_no_int$ci.ub, digits = 3), 
             k = impact_ne_w_no_int$k, 
             n = impact_ne_w_no_int$s.nlevels)
imp_mod_output_df

## @knitr drivers-var-weighted-model-output-df-scaled
drivers_mod_output_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_no_int$b)), 
             estimate = round(as.vector(drivers_scaled_no_int$b), digits = 3), 
             se = round(drivers_scaled_no_int$se, digits = 3), 
             pval = round(drivers_scaled_no_int$pval, digits = 3), 
             ci.lb = round(drivers_scaled_no_int$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_no_int$ci.ub, digits = 3), 
             k = drivers_scaled_no_int$k, 
             n = drivers_scaled_no_int$s.nlevels)
drivers_mod_output_df


# Sample size weighted 
# ------------------------------------------------------------------------------

## @knitr duration-var-weighted-model-output-df
mod1_ss_output_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_ss_robust$b), 
             estimate = round(as.vector(mod1_ss_robust$b), digits = 3), 
             se = round(mod1_ss_robust$se, digits = 3), 
             pval = round(mod1_ss_robust$pval, digits = 3), 
             ci.lb = round(mod1_ss_robust$ci.lb, digits = 3), 
             ci.ub = round(mod1_ss_robust$ci.ub, digits = 3), 
             k = mod1_ss$k, 
             n = mod1_ss$s.nlevels)
mod1_ss_output_df

## @knitr impact-var-weighted-model-output-df
imp_mod_ss_output_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_ss_robust$b), 
             estimate = round(as.vector(impact_ne_ss_robust$b), digits = 3), 
             se = round(impact_ne_ss_robust$se, digits = 3), 
             pval = round(impact_ne_ss_robust$pval, digits = 3), 
             ci.lb = round(impact_ne_ss_robust$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_ss_robust$ci.ub, digits = 3), 
             k = impact_ne_ss$k, 
             n = impact_ne_ss$s.nlevels)
imp_mod_ss_output_df

## @knitr drivers-var-weighted-model-output-df-scaled
drivers_mod_ss_output_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_ss_robust$b)), 
             estimate = round(as.vector(drivers_scaled_ss_robust$b), digits = 3), 
             se = round(drivers_scaled_ss_robust$se, digits = 3), 
             pval = round(drivers_scaled_ss_robust$pval, digits = 3), 
             ci.lb = round(drivers_scaled_ss_robust$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_ss_robust$ci.ub, digits = 3), 
             k = drivers_scaled_ss$k, 
             n = drivers_scaled_ss$s.nlevels)
drivers_mod_ss_output_df

# Unweighted - standard lmer
# ------------------------------------------------------------------------------

## @knitr duration-unweighted-model-output-df
lme1_output_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(coef(summary(lme1))), 
             estimate = as.vector(coef(summary(lme1))[, 1]), 
             se = as.vector(coef(summary(lme1))[, 2]),
             pval = as.vector(coef(summary(lme1))[, 5]), 
             ci.lb = as.vector(confint(lme1)[-(1:2), 1]), 
             ci.ub = as.vector(confint(lme1)[-(1:2), 2]), 
             k = lme1@pp$Zt@Dim[[2]], 
             n = lme1@pp$Zt@Dim[[1]]
  )
lme1_output_df

## @knitr impact-unweighted-model-output-df
impact_lme_output_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(coef(summary(impact_lme_ne))), 
             estimate = as.vector(coef(summary(impact_lme_ne))[, 1]), 
             se = as.vector(coef(summary(impact_lme_ne))[, 2]),
             pval = as.vector(coef(summary(impact_lme_ne))[, 5]), 
             ci.lb = as.vector(confint(impact_lme_ne)[-(1:2), 1]), 
             ci.ub = as.vector(confint(impact_lme_ne)[-(1:2), 2]), 
             k = impact_lme_ne@pp$Zt@Dim[[2]], 
             n = impact_lme_ne@pp$Zt@Dim[[1]]
  )
impact_lme_output_df

## @knitr drivers-unweighted-model-output-df
drivers_lme_output_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(coef(summary(drivers_lme_ne)))), 
             estimate = as.vector(coef(summary(drivers_lme_ne))[, 1]), 
             se = as.vector(coef(summary(drivers_lme_ne))[, 2]),
             pval = as.vector(coef(summary(drivers_lme_ne))[, 5]), 
             ci.lb = as.vector(confint(drivers_lme_ne)[-(1:2), 1]), 
             ci.ub = as.vector(confint(drivers_lme_ne)[-(1:2), 2]), 
             k = drivers_lme_ne@pp$Zt@Dim[[2]], 
             n = drivers_lme_ne@pp$Zt@Dim[[1]]
             )
drivers_lme_output_df


## @knitr sample-size-diffs-based-on-mod-weighting
data_frame(model = c('LR ~ D', 'LR ~ D * Imp', 'LR ~ D * (I + N + LTC)'), 
           `var-w sites` = c(mod1_no_int$k, impact_ne_w_no_int$k, drivers_scaled_no_int$k), 
           `var-w studies` = c(mod1_no_int$s.nlevels, impact_ne_w_no_int$s.nlevels, drivers_scaled_no_int$s.nlevels), 
           `samp-w sites` = c(mod1_ss$k, impact_ne_ss$k, drivers_scaled_ss$k), 
           `samp-w studies` = c(mod1_ss$s.nlevels, impact_ne_ss$s.nlevels, drivers_scaled_ss$s.nlevels), 
           `un-w sites` = c(lme1@pp$Zt@Dim[[2]], impact_lme_ne@pp$Zt@Dim[[2]], drivers_lme_ne@pp$Zt@Dim[[2]]), 
           `un-w studies` = c(lme1@pp$Zt@Dim[[1]], impact_lme_ne@pp$Zt@Dim[[1]], drivers_lme_ne@pp$Zt@Dim[[1]]))

knitr::kable(., digits = 3, caption = '**Table 2.** Variance-weighted (VW) and unweighted (UW) meta-regression of the proportion of species richness change across a gradient of three drivers (raw coefficient estimates, except for invasives --> invs * 0.001) of global change over time including the estimated percent change per unit of the driver and the corresponding model p-value. The percent change has been back-calculated from the estimated log ratio of the proportion of change in species richness.')