library(dplyr)
library(ggplot2)

# Function to get predictions given invs, nuts, temp, duration values.
# Change or add predictors as desired.
get_driver_predictions <- function(unscaled_rma_object, invs, nuts, temp, duration) {
  mods = cbind(1:duration, 
               rep(invs, duration),  
               rep(temp, duration), 
               rep(nuts, duration),
               invs*(1:duration), 
               temp*(1:duration), 
               nuts*(1:duration))
  predictions <- predict(object = unscaled_rma_object, newmods = mods)
  class(predictions) <- 'list'
  predictions <- predictions[1:6]
  predictions <- as_data_frame(predictions)
  predictions$duration <- 1:duration
  return(predictions)
}


# Missing step is the creation of the invs_quantiles, nut_quantiles, ltc_quantiles dfs.

# Get the predicted values for different values. 
all0  <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`0%`, 
                                   nuts = 0, temp = 0, duration = 20) %>% 
            # To allow me to filter data subsets to plot for the final graphs, 
            # I label the quantiles, e.g., '0'
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)

invs_q100_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = invs_quantiles$`100%`, 
                                   nuts = 0, temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '100', invs_value = invs_quantiles$`100%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)

nuts_q100_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = nut_quantiles$`100%`, 
                                   temp = 0, duration = 20) %>% 
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '100', nuts_value = nut_quantiles$`100%`, 
                   temp_quantile = '0', temp_value = ltc_quantiles$`0%`)
ltc_q100_all0 <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                                   invs = 0, nuts = 0, 
                                   temp = ltc_quantiles$`100%`, duration = 20) %>%
            mutate(invs_quantile = '0', invs_value = invs_quantiles$`0%`, 
                   nuts_quantile = '0', nuts_value = nut_quantiles$`0%`, 
                   temp_quantile = '100', temp_value = ltc_quantiles$`100%`)


# Consolidate the predicted dataframes into one to make plotting easier:
driver_predictions <- bind_rows(all0, invs_q100_all0, nuts_q100_all0, ltc_q100_all0)


# I don't remember why I did all these plots separately... but I think there was 
# a reason. Apologies in advance for the redundancy of the ggplot formatting code
## @knitr driver-prediction-plots-0-max-in-dataset
nuts_alone_predictions <- 
driver_predictions %>% 
  filter(invs_quantile == 0, temp_quantile == 0) %>% 
  mutate(nuts_quantile = factor(nuts_quantile, levels = c('0', '100'))) %>% 
  mutate(driver = 'Nutrient addition') %>%
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = nuts_quantile)) + 
  # Make the ribbon for the confidence interval
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = nuts_quantile), alpha = 0.4) + 
  # Only formatting from here on down. I got the hex codes 
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
  mutate(invs_quantile = factor(invs_quantile, levels = c('0', '100'))) %>% 
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
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', '100'))) %>% 
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

# This plot seems sketchy because I use temp_quantile as the filter, but it's 
# because all the quantiles are set to the mean so I just chose to use 
# temp_quantile as the grouping variable (this note may be more for my own well-being)
all_mean_predictions <- 
  bind_rows(all0, all_mean) %>% 
    mutate(temp_quantile = factor(temp_quantile, levels = c('0', 'mean'))) %>%
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

# Laying out the plots. 
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