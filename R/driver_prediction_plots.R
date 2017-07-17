# DRIVER PREDICTION PLOTS

## @knitr drivers-quantile-specifications
# ------------------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------------------
# GET QUANTILES
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
    quantile(., probs = seq(from = 0, to = 1, by = 0.05)) %>% 
    as.list(.) %>% 
    as_data_frame(.)

nut_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$mean_nuts %>% 
    quantile(., , probs = seq(from = 0, to = 1, by = 0.05)) %>% 
    as.list(.) %>% 
    as_data_frame(.)

ltc_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
    .$sliced_ltc %>% 
    quantile(., , probs = seq(from = 0, to = 1, by = 0.05)) %>% 
    as.list(.) %>% 
    as_data_frame(.)

# Mean values
mean_values <- 
no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
           !is.na(mean_nuts) & !is.na(sliced_ltc)) %>%
    summarise(mean_invs = mean(mean_invs), mean_nuts = mean(mean_nuts), 
              mean_temp = mean(sliced_ltc))

# DRIVERS ALL ELSE 0
## @knitr drivers-predictions-based-on-quantiles-all-else-0
all0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`0%`, 
                                   nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)

## invasives predictions
invs_q25_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`25%`, 
                                   nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '25', invs_value = invs_quantiles$`25%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
invs_q50_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
invs_q75_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`75%`,
                                   nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '75', invs_value = invs_quantiles$`75%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
invs_q100_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`100%`, 
                                   nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '100', invs_value = invs_quantiles$`100%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
invs_midpoint <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                    invs = invs_quantiles$`100%` / 2, 
                                    nuts = 0, temp = 0, duration = 20)
invs_predictions <- 
  bind_rows(list(all0, invs_q25_all0, invs_q50_all0, invs_q75_all0, invs_q100_all0))


nuts_q25_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`25%`, 
                                   temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '25', nuts_value = nut_quantiles$`25%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
nuts_q50_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`50%`, 
                                   temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
nuts_q75_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`75%`, 
                                   temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '75', nuts_value = nut_quantiles$`75%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
nuts_q100_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`100%`, 
                                   temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '100', nuts_value = nut_quantiles$`100%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
nuts_midpoint <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                    invs = 0, nuts = nut_quantiles$`100%` / 2, 
                                    temp = 0, duration = 20)

nuts_predictions <- 
  bind_rows(list(all0, nuts_q25_all0, nuts_q50_all0, nuts_q75_all0, nuts_q100_all0))


ltc_q25_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`25%`, duration = 20) %>%
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '25', temp_value = ltc_quantiles$`25%`)
ltc_q50_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`50%`, duration = 20) %>%
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
ltc_q75_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`75%`, duration = 20) %>%
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '75', temp_value = ltc_quantiles$`75%`)
ltc_q100_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`100%`, duration = 20) %>%
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '100', temp_value = ltc_quantiles$`100%`)
ltc_midpoint <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                    invs = 0, nuts = 0, 
                                    temp = ltc_quantiles$`100%` / 2, duration = 20)

ltc_predictions <- 
  bind_rows(list(all0, ltc_q25_all0, ltc_q50_all0, ltc_q75_all0, ltc_q100_all0))

# DRIVERS ALL ELSE MEDIAN
all_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)

## @knitr drivers-predictions-based-on-median-values
##### Drivers held constant at median values
invs_q0_median  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`0%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
invs_q25_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`25%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '25', invs_value = invs_quantiles$`25%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
invs_q75_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`75%`,
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '75', invs_value = invs_quantiles$`75%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
invs_q100_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`100%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '100', invs_value = invs_quantiles$`100%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
#invs_predictions <- 
#  bind_rows(list(all_median, invs_q25_median, invs_q50_median, invs_q75_median, invs_q100_median))

nuts_q0_median  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`0%`, 
                                   temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
nuts_q25_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`25%`, 
                                   temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '25', nuts_value = nut_quantiles$`25%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
nuts_q75_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`75%`, 
                                   temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '75', nuts_value = nut_quantiles$`75%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
nuts_q100_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`100%`, 
                                   temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '100', nuts_value = nut_quantiles$`100%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)
#nuts_predictions <- 
#  bind_rows(list(all_median, nuts_q25_median, nuts_q50_median, nuts_q75_median, nuts_q100_median))

ltc_q0_median  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`0%`, duration = 20) %>%
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
ltc_q25_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`25%`, duration = 20) %>%
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '25', temp_value = ltc_quantiles$`25%`)
ltc_q75_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`75%`, duration = 20) %>%
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '75', temp_value = ltc_quantiles$`75%`)
ltc_q100_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = ltc_quantiles$`100%`, duration = 20) %>%
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '100', temp_value = ltc_quantiles$`100%`)
#ltc_predictions <- 
#  bind_rows(list(all_median, ltc_q25_median, ltc_q50_median, ltc_q75_median, ltc_q100_median))


# DRIVERS ALL ELSE MEAN
all_mean <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = mean_values$mean_invs, 
                                   nuts = mean_values$mean_nuts, 
                                   temp = mean_values$mean_temp, duration = 20) %>% 
            mutate(invs_quantile = 'mean', invs_value = mean_values$mean_invs, 
                   nuts_quantile = 'mean', nuts_value = mean_values$mean_nuts, 
                   temp_quantile = 'mean', temp_value = mean_values$mean_temp)

## @knitr drivers-predictions-based-on-mean-values
##### Drivers held constant at mean values
invs_q0_mean  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, 
                                   nuts = mean_values$mean_nuts, 
                                   temp = mean_values$mean_temp, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = 0, 
                   nuts_quantile = 'mean', nuts_value = mean_values$mean_nuts, 
                   temp_quantile = 'mean', temp_value = mean_values$mean_temp)
invs_q100_mean <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`100%`, 
                                   nuts = mean_values$mean_nuts, 
                                   temp = mean_values$mean_temp, duration = 20) %>% 
            mutate(invs_quantile = '100', invs_value = invs_quantiles$`100%`, 
                   nuts_quantile = 'mean', nuts_value = mean_values$mean_nuts, 
                   temp_quantile = 'mean', temp_value = mean_values$mean_temp)

nuts_q0_mean  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = mean_values$mean_invs, 
                                   nuts = 0, 
                                   temp = mean_values$mean_temp, duration = 20) %>% 
            mutate(invs_quantile = 'mean', invs_value = mean_values$mean_invs, 
                   nuts_quantile = '0', nuts_value = 0, 
                   temp_quantile = 'mean', temp_value = mean_values$mean_temp)
nuts_q100_mean <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = mean_values$mean_invs, 
                                   nuts = nut_quantiles$`50%`, 
                                   temp = mean_values$mean_temp, duration = 20) %>% 
            mutate(invs_quantile = 'mean', invs_value = mean_values$mean_invs, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = 'mean', temp_value = mean_values$mean_temp)

ltc_q0_mean  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = mean_values$mean_invs, 
                                   nuts = mean_values$mean_nuts, 
                                   temp = ltc_quantiles$`100%`, duration = 20) %>% 
            mutate(invs_quantile = 'mean', invs_value = mean_values$mean_invs, 
                   nuts_quantile = 'mean', nuts_value = mean_values$mean_nuts, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`100%`)
ltc_q100_mean <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = mean_values$mean_invs, 
                                   nuts = mean_values$mean_nuts, 
                                   temp = ltc_quantiles$`100%`, duration = 20) %>% 
            mutate(invs_quantile = 'mean', invs_value = mean_values$mean_invs, 
                   nuts_quantile = 'mean', nuts_value = mean_values$mean_nuts, 
                   temp_quantile = '100', temp_value = ltc_quantiles$`100%`)


## @knitr driver-prediction-plots-0-max-in-dataset
driver_predictions <- bind_rows(all0, invs_q100_all0, nuts_q100_all0, ltc_q100_all0)
#
nuts_alone_predictions <- 
driver_predictions %>% 
  filter(invs_quantile == 0, temp_quantile == 0) %>% 
  mutate(nuts_quantile = factor(nuts_quantile, levels = c('0', '50', '100'))) %>% 
  mutate(driver = 'Nutrient addition') %>%
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = nuts_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = nuts_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 20)) + 
  ylim(c(-8, 8)) + 
  ylab('Predicted log response ratio') + 
  xlab('\nDuration (years)') + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 9), 
        strip.text = element_text(size = 9))
invs_alone_predictions <- 
driver_predictions %>% 
  filter(nuts_quantile == 0, temp_quantile == 0) %>% 
  mutate(invs_quantile = factor(invs_quantile, levels = c('0', '50', '75', '100'))) %>% 
  mutate(driver = 'Invasion potential') %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = invs_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = invs_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 20)) + 
  ylim(c(-8, 8)) + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 9), 
        strip.text = element_text(size = 9))
#
ltc_alone_predictions <- 
driver_predictions %>% 
  filter(nuts_quantile == 0, invs_quantile == 0) %>% 
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', '50', '100'))) %>% 
  mutate(driver = 'Linear temperature change') %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = temp_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = temp_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 20)) + 
  ylim(c(-8, 8)) + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 9), 
        strip.text = element_text(size = 9))
#
all_mean_predictions <- 
bind_rows(all0, all_mean) %>% 
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', 'mean', '100'))) %>%
  mutate(driver = 'All drivers') %>% 
ggplot(data = ., aes(group = temp_quantile)) + 
  geom_line(aes(x = duration, y = (pred), colour = temp_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = temp_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#F8766D')) + 
  scale_fill_manual(values = c('darkgrey', '#F8766D')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 20)) + 
  ylim(c(-8, 8)) + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 9), 
        strip.text = element_text(size = 9))
#

master_layout <- 
grid.layout(nrow = 2, ncol = 5, 
            widths = unit(c(0.1, 1, 0.9, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
#dev.new(height = 3.5, width = 11)

#grid.newpage()
pushViewport(viewport(layout = master_layout))
print(nuts_alone_predictions, vp = set_vp(1, 2))
print(invs_alone_predictions, vp = set_vp(1, 3))
print(ltc_alone_predictions, vp = set_vp(1, 4))
print(all_mean_predictions, vp = set_vp(1, 5))
#
grid.text(
    "a)", vp = viewport(layout.pos.row = 1, layout.pos.col = 2), 
    gp = gpar(fontsize = 11), vjust = -12.5, hjust = 10
    )
grid.text(
    "b)", vp = viewport(layout.pos.row = 1, layout.pos.col = 3), 
    gp = gpar(fontsize = 11), vjust = -12.5, hjust = 10
    )
grid.text(
    "c)", vp = viewport(layout.pos.row = 1, layout.pos.col = 4), 
    gp = gpar(fontsize = 11), vjust = -12.5, hjust = 10
    )
grid.text(
    "d)", vp = viewport(layout.pos.row = 1, layout.pos.col = 5), 
    gp = gpar(fontsize = 11), vjust = -12.5, hjust = 10
    )
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 11), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:5),
    gp = gpar(fontsize = 11), hjust = 0.25, vjust = 1
    )


#grid.arrange(nuts_alone_predictions, invs_alone_predictions, ltc_alone_predictions, all_mean_predictions, ncol = 4)



# "It's important to keep in mind... (for the really big - nutrients)"
# note the difference in scale

# DRIVER PREDICTION PLOTS

## @knitr drivers-world-values
# ------------------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------------------

# Max world values
world_invs <- readRDS('../Data_outputs/invs_vals_above_zero.rda')
world_nuts <- readRDS('../Data_outputs/nuts_vals_above_zero.rda')

## invasives predictions

invs_mean_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = mean(world_invs), 
                         nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = 'global mean', invs_value = mean(world_invs), 
                   nuts_quantile = '0', nuts_value = 0, 
                   temp_quantile = '0', temp_value = 0)
invs_1sd_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = sd(world_invs), 
                         nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '1 sd', invs_value = sd(world_invs), 
                   nuts_quantile = '0', nuts_value = 0, 
                   temp_quantile = '0', temp_value = 0)
invs_max_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = max(world_invs), 
                         nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = 'global maximum', invs_value = max(world_invs), 
                   nuts_quantile = '0', nuts_value = 0, 
                   temp_quantile = '0', temp_value = 0)

## nutrients predictions
nuts_mean_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = 0, 
                         nuts = mean(world_nuts), temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = 0, 
                   nuts_quantile = 'global mean', nuts_value = mean(world_nuts), 
                   temp_quantile = '0', temp_value = 0)
nuts_1sd_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = 0, 
                         nuts = sd(world_nuts), temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = 0, 
                   nuts_quantile = '1 sd', nuts_value = sd(world_nuts), 
                   temp_quantile = '0', temp_value = 0)
nuts_max_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = 0, 
                         nuts = max(world_nuts), temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = 0, 
                   nuts_quantile = 'global maximum', nuts_value = max(world_nuts), 
                   temp_quantile = '0', temp_value = 0)


# https://www.ncdc.noaa.gov/sotc/global/201513
# Overall, the global annual temperature has increased at an average rate of 0.07°C (0.13°F) per decade since 1880 and at an average rate of 0.17°C (0.31°F) per decade since 1970.

temp_0.07_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = 0, nuts = 0, temp = 0.07, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = 0, 
                   nuts_quantile = '1 sd', nuts_value = 0.07, 
                   temp_quantile = '0', temp_value = 0)
temp_0.17_world0 <- 
  get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                         invs = 0, nuts = 0, temp = 0.17, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = 0, 
                   nuts_quantile = 'global maximum', nuts_value = 0.17, 
                   temp_quantile = '0', temp_value = 0)

# DRIVERS ALL ELSE MEDIAN
all_median <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`50%`, 
                                   nuts = nut_quantiles$`50%`, temp = ltc_quantiles$`50%`, duration = 20) %>% 
            mutate(invs_quantile = '50', invs_value = invs_quantiles$`50%`, 
                   nuts_quantile = '50', nuts_value = nut_quantiles$`50%`, 
                   temp_quantile = '50', temp_value = ltc_quantiles$`50%`)

driver_predictions <- bind_rows(all0, invs_mean_world0, nuts_mean_world0, temp_0.07_world0, temp_0.17_world0)
#
nuts_alone_predictions <- 
driver_predictions %>% 
  filter(invs_quantile == 0, temp_quantile == 0) %>% 
  #mutate(nuts_quantile = factor(nuts_quantile, levels = c('0', '50', '100'))) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = nuts_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = nuts_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5)) + 
  ylab('Predicted log response ratio') + 
  xlab('\nDuration (years)')
#
invs_alone_predictions <- 
driver_predictions %>% 
  filter(nuts_quantile == 0, temp_quantile == 0) %>% 
  mutate(invs_quantile = factor(invs_quantile, levels = c('0', '50', '75', '100'))) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = invs_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = invs_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5))
#
ltc_alone_predictions <- 
driver_predictions %>% 
  filter(nuts_quantile == 0, invs_quantile == 0) %>% 
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', '50', '100'))) %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = temp_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = temp_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5))
#
all_mean_predictions <- 
bind_rows(all0, all_mean) %>% 
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', 'mean', '100'))) %>%
ggplot(data = ., aes(group = temp_quantile)) + 
  geom_line(aes(x = duration, y = (pred), colour = temp_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = temp_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#F8766D')) + 
  scale_fill_manual(values = c('darkgrey', '#F8766D')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5))
#
#grid.arrange(nuts_alone_predictions, invs_alone_predictions, ltc_alone_predictions, all_mean_predictions, ncol = 4)