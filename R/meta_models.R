## Local marine diversity change meta-analysis model results

## @knitr cleaned-meta-models-data
library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(metafor)
library(lme4)
library(lmerTest)
library(gridExtra)
library(knitr)
library(beepr)

source('02_functions.R')

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

## @knitr duration-var-weighted-intercept
mod1 <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1

## @knitr impacts-var-weighted-intercept
impact_ne_w <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_w


## @knitr drivers-var-weighted-intercept
drivers_scaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3),
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled

## @knitr drivers-var-weighted-collinearity-table
drivers_cor_table <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% mutate(mean_invs = mean_invs * 10^-3) %>% select(mean_nuts, mean_invs, sliced_ltc, Duration) %>% cor()

## @knitr drivers-var-weighted-intercept-not-scaled
drivers_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event, #%>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (mean_invs + sliced_ltc + mean_nuts))
drivers_unscaled

## @knitr drivers-interactions-var-weighted-scaled
# Invasives * temperature (Sorte et al 2010, )
# Nutrients * temperature (Binzer et al 2012)
interactions_scaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc) * scale(mean_nuts)))

# Likely low power, but at least the model does not appear to be 
# overparameterised. 
#profile(interactions_scaled)
interactions_scaled

## @knitr drivers-interactions-var-weighted-unscaled
interactions_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs * sliced_ltc + sliced_ltc * mean_nuts))

# Likely low power, but at least the model does not appear to be 
# overparameterised. 
#profile(interactions_unscaled)
interactions_unscaled


# Variance weighted plots
# DURATION PLOT
## @knitr no-event-data-plotted-with-duration-var-weighted-fig1
duration_w <- 
filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)) %>%
ggplot(data = .) +
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 1) + 
  ylab('Log ratio') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') +
  #ggtitle('Weighted Analysis') + 
  xlab('Duration (years)') +
  ylim(-2, 2) + 
  geom_line(data = data.frame(x = 1:41, y = predict(mod1, 1:41)$pred), aes(x = x, y = y), colour = 'blue') +
  geom_ribbon(data = data.frame(x = 1:41, ymin = predict(mod1, 1:41)$ci.lb, ymax = predict(mod1, 1:41)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.1)
duration_w

# IMPACTS PLOTS
## @knitr impacts-var-weighted-intercept-profile-plot
profile(impact_ne_w)

## @knitr impacts-var-weighted-intercept-coefficient-estimates-fig2
mk_rma_summary_df(impact_ne_w) %>%
  mutate(driver = gsub("mean_imps", "Cumulative human impact", driver)) %>%
  filter(driver != 'intrcpt') %>% 
  mutate(driver = factor(driver, levels = c('Duration:Cumulative human impact', 'Duration', 'Cumulative human impact'))) %>%
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

## @knitr impacts-var-weighted-predictions
imp_quantiles <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)) %>% 
    .$mean_imps %>% 
    quantile(.) %>% 
    as.list(.) %>% 
    as_data_frame(.)

q0_imps <- get_imp_predictions(rma_object = impact_ne_w, impact_value = imp_quantiles$`0%`, duration = 15) %>% 
           mutate(driver = 'impact', quantile = '0', value = round(imp_quantiles$`0%`, 2))
q25_imps <- get_imp_predictions(rma_object = impact_ne_w, impact_value = imp_quantiles$`25%`, duration = 15) %>% 
            mutate(driver = 'impact', quantile = '25', value = round(imp_quantiles$`25%`, 2))
q50_imps <- get_imp_predictions(rma_object = impact_ne_w, impact_value = imp_quantiles$`50%`, duration = 15) %>% 
            mutate(driver = 'impact', quantile = '50', value = round(imp_quantiles$`50%`, 2))
q75_imps <- get_imp_predictions(rma_object = impact_ne_w, impact_value = imp_quantiles$`75%`, duration = 15) %>% 
            mutate(driver = 'impact', quantile = '75', value = round(imp_quantiles$`75%`, 2))
q100_imps <- get_imp_predictions(rma_object = impact_ne_w, impact_value = imp_quantiles$`100%`, duration = 15) %>% 
             mutate(driver = 'impact', quantile = '100', value = round(imp_quantiles$`100%`, 2))

imp_predictions <- 
  bind_rows(list(q0_imps, q25_imps, q50_imps, q75_imps, q100_imps)) %>% 
  mutate(quantile = factor(quantile, levels = c('0', '25', '50', '75', '100')))

## @knitr impacts-var-weighted-predicted-log-ratio
impact_predicted_log_ratio <- 
  ggplot(data = imp_predictions, aes(x = duration, y = pred)) + 
    geom_line(aes(x = duration, y = pred, colour = quantile)) + 
    geom_ribbon(aes(x = duration, ymin = ci.lb, ymax = ci.ub, fill = quantile), alpha = 0.3) + 
    geom_text(data = group_by(imp_predictions, quantile) %>% 
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

## @knitr impacts-var-weighted-observed-log-ratio
observed_log_ratio <- 
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

## @knitr impacts-var-weighted-predicted-percent-change
impact_predicted_percent_change <- 
  ggplot(data = imp_predictions, aes(x = duration, y = get_percent_change(pred))) + 
    geom_line(aes(x = duration, y = get_percent_change(pred), colour = quantile)) + 
    geom_ribbon(aes(x = duration, ymin = get_percent_change(ci.lb), 
                    ymax = get_percent_change(ci.ub), fill = quantile), 
                alpha = 0.3) + 
    geom_text(data = group_by(imp_predictions, quantile) %>% 
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

## @knitr impacts-var-weighted-observed-percent-change
observed_percent_change <- 
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

## @knitr impacts-var-weighted-prediction-plots-log-ratio-fig3
grid.arrange(observed_log_ratio, impact_predicted_log_ratio, ncol = 2)

## @knitr impacts-var-weighted-prediction-plots-percent-change-fig4
grid.arrange(observed_percent_change, impact_predicted_percent_change, ncol = 2)

## @knitr cumulative-impacts-freq-dist-for-var-weighted-dataset-fig5
ggplot(data = no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)), 
       aes(x = mean_imps)) + 
  geom_histogram(binwidth = 1, colour = 'black', boundary = 0) + 
  scale_x_continuous(breaks = 0:10) + 
  theme_minimal() + 
  xlab('\nCumulative human impact score') + 
  ylab('Frequency\n')


# DRIVERS PLOTS

## @knitr drivers-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown
model_driver_vector <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration:Invasives', 
    'Duration:LTC', 'Duration:Nutrients')
ordered_driver_vector <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration:Invasives', 
    'Duration:Nutrients', 'Duration:LTC')

driver_summary_plot_short  <- 
  mk_rma_summary_df(drivers_scaled) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-3.2, 3.2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short

driver_summary_plot_long  <- 
  mk_rma_summary_df(drivers_scaled) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.25, 0.25)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, driver_summary_plot_short, text_b, driver_summary_plot_long, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))

# DRIVERS INTERACTION PLOTS
## @knitr interactions-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown
model_interaction_vector <- 
  c('Duration', 
    'Invasives', 'LTC', 'Nutrients', 
    'Invasives:LTC', 'Nutrients:LTC', 
    'Duration:Invasives', 'Duration:LTC', 'Duration:Nutrients', 
    'Duration:Invasives:LTC', 'Duration:Nutrients:LTC')
ordered_interaction_vector <- 
  c('Duration', 
    'Invasives', 'Nutrients', 'LTC', 
    'Invasives:LTC', 'Nutrients:LTC', 
    'Duration:Invasives', 'Duration:Nutrients', 'Duration:LTC', 
    'Duration:Invasives:LTC', 'Duration:Nutrients:LTC')

interaction_summary_plot_short  <- 
  mk_rma_summary_df(interactions_scaled) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_interaction_vector, 
                             levels = rev(ordered_interaction_vector))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no'
      , 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-8.5, 8.5)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interaction_summary_plot_short

interaction_summary_plot_long  <- 
  mk_rma_summary_df(interactions_scaled) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_interaction_vector, 
                             levels = rev(ordered_interaction_vector))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no'
      , 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-1, 1)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interaction_summary_plot_long

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, interaction_summary_plot_short, text_b, interaction_summary_plot_long, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))



# ------------------------------------------------------------------------------
# Sample size weighted analysis:
# ------------------------------------------------------------------------------

## @knitr duration-samp-size-weighted-model-output
mod1_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss
mod1_ss_robust <- robust(mod1_ss, cluster=1:mod1_ss$k)
mod1_ss_robust

## @knitr impacts-samp-size-weighted-model-output
impact_ne_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_ne_ss
impact_ne_ss_robust <- robust(impact_ne_ss, cluster=1:impact_ne_ss$k)
impact_ne_ss_robust

## @knitr drivers-samp-size-weighted-model-output-scaled
drivers_scaled_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_scaled_ss
drivers_scaled_ss_robust <- robust(drivers_scaled_ss, cluster=1:drivers_scaled_ss$k)
drivers_scaled_ss_robust


## @knitr drivers-interactions-samp-size-weighted-scaled
interactions_ss_scaled <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))
interactions_ss_scaled 

interactions_ss_scaled_robust <- robust(interactions_ss_scaled, cluster=1:interactions_ss_scaled$k)
interactions_ss_scaled_robust



# @knitr drivers-interactions-samp-size-weighted-unscaled
interactions_ss_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs * sliced_ltc + sliced_ltc*mean_nuts))
interactions_ss_unscaled 

interactions_ss_unscaled_robust <- robust(interactions_ss_unscaled, cluster=1:interactions_ss_unscaled$k)



## @knitr drivers-ss-weighted-intercept-scaled-par-est-plot-short-long-breakdown
model_driver_vector <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration*Invasives', 
    'Duration*LTC', 'Duration*Nutrients')
ordered_driver_vector <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration*Invasives', 
    'Duration*Nutrients', 'Duration*LTC')

driver_summary_plot_short_ss  <- 
  mk_rma_summary_df(drivers_scaled_ss_robust) %>% 
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
      xlim(c(-0.2, 0.2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short_ss

driver_summary_plot_long_ss  <- 
  mk_rma_summary_df(drivers_scaled_ss_robust) %>% 
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
      xlim(c(-0.1, 0.1)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long_ss

grid.arrange(driver_summary_plot_short_ss, driver_summary_plot_long_ss, ncol = 2)


## @knitr drivers-samp-size-weighted-model-output-unscaled
drivers_unscaled_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts))
#drivers_unscaled_ss
drivers_unscaled_ss_robust <- robust(drivers_unscaled_ss, cluster=1:drivers_unscaled_ss$k)
drivers_unscaled_ss_robust



## @knitr drivers-ss-weighted-intercept-scaled-par-est-plot-short-long-breakdown-fig11
model_driver_vector <- 
  c('Dur', 'Invs', 'LTC', 'Nuts', 'Invs*LTC', 
    'Nuts*LTC', 'Dur*Invs', 'Dur*Nuts', 'Dur*LTC', 
    'Dur*Invs*LTC', 'Dur*Nuts*LTC')
ordered_driver_vector <- 
  c('Dur', 'Invs', 'LTC', 'Nuts', 'Invs*LTC', 
    'Nuts*LTC', 'Dur*Invs', 'Dur*Nuts', 'Dur*LTC',
    'Dur*Invs*LTC', 'Dur*Nuts*LTC')

interactions_summary_plot_short_ss  <- 
  mk_rma_summary_df(interactions_ss_scaled_robust) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(interaction = c('yes', 'no', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'yes')) %>%
    filter(interaction == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-2, 2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interactions_summary_plot_short_ss

interactions_summary_plot_long_ss  <- 
  mk_rma_summary_df(interactions_ss_scaled_robust) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(interaction = c('yes', 'no', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'yes')) %>%
    filter(interaction == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.25, 0.25)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13)) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interactions_summary_plot_long_ss

grid.arrange(interactions_summary_plot_short_ss, interactions_summary_plot_long_ss, ncol = 2)


# ------------------------------------------------------------------------------
# Comparison of model output tables
# ------------------------------------------------------------------------------

# Variance weighted 
# ------------------------------------------------------------------------------

## @knitr duration-var-weighted-model-output-table1
mod1_output_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1$b), 
             estimate = round(as.vector(mod1$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(mod1$b)), digits = 2), 
             se = round(mod1$se, digits = 3), 
             pval = round(mod1$pval, digits = 3), 
             ci.lb = round(mod1$ci.lb, digits = 3), 
             ci.ub = round(mod1$ci.ub, digits = 3), 
             k = mod1$k, 
             n = mod1$s.nlevels)

#mod1_output_df %>% 
#  knitr::kable(., caption = '**Table 1.** Parameter estimates from a variance-weighted meta-regression of the log ratio of the proportion of species richness change (LR) over study duration (D). For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr impact-var-weighted-model-output-table2
imp_mod_output_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_w$b), 
             estimate = round(as.vector(impact_ne_w$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(impact_ne_w$b)), digits = 2), 
             se = round(impact_ne_w$se, digits = 3), 
             pval = round(impact_ne_w$pval, digits = 3), 
             ci.lb = round(impact_ne_w$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_w$ci.ub, digits = 3), 
             k = impact_ne_w$k, 
             n = impact_ne_w$s.nlevels) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter)) %>% 
  mutate(per_lb = round(get_percent_change(impact_ne_w$ci.lb), digits = 3), 
         per_ub = round(get_percent_change(impact_ne_w$ci.ub), digits = 3))


#imp_mod_output_df %>% 
#  knitr::kable(., caption = '**Table 2.** Parameter estimates from a variance-weighted meta-regression of the relationship between the log ratio of the proportion of species richness change (LR), study duration (D), cumulative human impact value (Imp, Halpern et al. 2015), and the interaction of study duration and cumulative human impact value (D * Imp). For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the parameter estimates of the log ratio of the proportion of change in species richness. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr drivers-var-weighted-model-output-df-scaled-table3
drivers_mod_output_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled$b)), 
             estimate = round(as.vector(drivers_scaled$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(drivers_unscaled$b)), digits = 4), 
             se = round(drivers_scaled$se, digits = 3), 
             pval = round(drivers_scaled$pval, digits = 3), 
             ci.lb = round(drivers_scaled$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled$ci.ub, digits = 3), 
             k = drivers_scaled$k, 
             n = drivers_scaled$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter)) %>% 
  mutate(per_lb = round(get_percent_change(drivers_unscaled$ci.lb), digits = 5), 
         per_ub = round(get_percent_change(drivers_unscaled$ci.ub), digits = 3))

#drivers_mod_output_df %>% 
#  knitr::kable(., caption = '**Table 3.** Standardised coefficient estimates from a variance-weighted meta-regression of the relationship between the log ratio of the proportion of species richness change (LR), study duration (D), and three drivers of biodiversity change: invasion potential (I), nutrient addition (N), and linear temperature change (LTC). To test how these three drivers affect the rate of change of species richness we included interactions between study duration and these three drivers. For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness using raw (unscaled) predictor values. The units of this percent change are for each unit of driver impact level, except for the invasion potential, where the percent change is for each 10^3 increase in impact level. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr interactions-var-weighted-model-output
interactions_output_df <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(interactions_scaled$b)), 
             estimate = round(as.vector(interactions_scaled$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(interactions_unscaled$b)), digits = 2), 
             se = round(interactions_scaled$se, digits = 3), 
             pval = round(interactions_scaled$pval, digits = 3), 
             ci.lb = round(interactions_scaled$ci.lb, digits = 3), 
             ci.ub = round(interactions_scaled$ci.ub, digits = 3), 
             k = interactions_scaled$k, 
             n = interactions_scaled$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))

#interactions_output_df %>% 
#  knitr::kable(., caption = '**Table 4.** Standardised coefficient estimates from a variance-weighted meta-regression of the relationship between the log ratio of the proportion of species richness change (LR), study duration (D), and the interaction between drivers of biodiversity change: invasion potential (I), nutrient addition (N), and linear temperature change (LTC). To test how these three drivers affect the rate of change of species richness we included interactions between study duration and these three drivers. For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness using raw (unscaled) predictor values. The units of this percent change are for each unit of driver impact level, except for the invasion potential, where the percent change is for each 10^3 increase in impact level. Each study could contain multiple sites and therefore studies were modeled as random effects.')


# Sample size weighted 
# ------------------------------------------------------------------------------

## @knitr duration-ss-weighted-model-output-df
mod1_ss_output_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_ss_robust$b), 
             estimate = round(as.vector(mod1_ss_robust$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(mod1_ss_robust$b)), digits = 2),
             se = round(mod1_ss_robust$se, digits = 3), 
             pval = round(mod1_ss_robust$pval, digits = 3), 
             ci.lb = round(mod1_ss_robust$ci.lb, digits = 3), 
             ci.ub = round(mod1_ss_robust$ci.ub, digits = 3), 
             k = mod1_ss$k, 
             n = mod1_ss$s.nlevels)

#mod1_ss_output_df %>% 
#  knitr::kable(., caption = '**Table 6.** Parameter estimates from a *sample size-weighted* meta-regression of the log ratio of the proportion of species richness change (LR) over study duration (D). For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr impact-ss-weighted-model-output-df
imp_mod_ss_output_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_ss_robust$b), 
             estimate = round(as.vector(impact_ne_ss_robust$b), digits = 3),
            `% change` = round(get_percent_change(as.vector(impact_ne_ss_robust$b)), digits = 2), 
             se = round(impact_ne_ss_robust$se, digits = 3), 
             pval = round(impact_ne_ss_robust$pval, digits = 3), 
             ci.lb = round(impact_ne_ss_robust$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_ss_robust$ci.ub, digits = 3), 
             k = impact_ne_ss$k, 
             n = impact_ne_ss$s.nlevels) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))

#imp_mod_ss_output_df %>% 
#  knitr::kable(., caption = '**Table 7.** Parameter estimates from a *sample size-weighted* meta-regression of the relationship between the log ratio of the proportion of species richness change (LR), study duration (D), cumulative human impact value (Imp, Halpern et al. 2015), and the interaction of study duration and cumulative human impact value (D * Imp). For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the parameter estimates of the log ratio of the proportion of change in species richness. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr drivers-ss-weighted-model-output-df-scaled
drivers_mod_ss_output_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_ss_robust$b)), 
             estimate = round(as.vector(drivers_scaled_ss_robust$b), digits = 3), 
            `% change` = round(get_percent_change(as.vector(drivers_unscaled_ss_robust$b)), digits = 2),
             se = round(drivers_scaled_ss_robust$se, digits = 3), 
             pval = round(drivers_scaled_ss_robust$pval, digits = 3), 
             ci.lb = round(drivers_scaled_ss_robust$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_ss_robust$ci.ub, digits = 3), 
             k = drivers_scaled_ss$k, 
             n = drivers_scaled_ss$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))

#drivers_mod_ss_output_df %>% 
#  knitr::kable(., caption = '**Table 8.** Standardised coefficient estimates from a *sample size-weighted* meta-regression of the relationship between the log ratio of the proportion of species richness change (LR), study duration (D), and three drivers of biodiversity change: invasion potential (I), nutrient addition (N), and linear temperature change (LTC). To test how these three drivers affect the rate of change of species richness we included interactions between study duration and these three drivers. For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness using raw (unscaled) predictor values. The units of this percent change are for each unit of driver impact level, except for the invasion potential, where the percent change is for each 10^3 increase in impact level. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr drivers-interactions-ss-weighted-model-output-df-scaled
interactions_ss_output_df <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(interactions_ss_scaled_robust$b)), 
             estimate = round(as.vector(interactions_ss_scaled_robust$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(interactions_ss_unscaled_robust$b)), digits = 2), 
             se = round(interactions_ss_scaled_robust$se, digits = 3), 
             pval = round(interactions_ss_scaled_robust$pval, digits = 3), 
             ci.lb = round(interactions_ss_scaled_robust$ci.lb, digits = 3), 
             ci.ub = round(interactions_ss_scaled_robust$ci.ub, digits = 3), 
             k = interactions_ss_scaled$k, 
             n = interactions_ss_scaled$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))

#interactions_ss_output_df %>% 
#  knitr::kable(., caption = '**Table 9.** Standardised coefficient estimates from a *sample size-weighted* meta-regression of the relationship between the log ratio of the proportion of species richness change (LR), study duration (D), and the interaction between drivers of biodiversity change: invasion potential (I), nutrient addition (N), and linear temperature change (LTC). To test how these three drivers affect the rate of change of species richness we included interactions between study duration and these three drivers. For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness using raw (unscaled) predictor values. The units of this percent change are for each unit of driver impact level, except for the invasion potential, where the percent change is for each 10^3 increase in impact level. Each study could contain multiple sites and therefore studies were modeled as random effects.')


## @knitr sample-size-diffs-based-on-mod-weighting-comparison
sample_size_diffs_df <- 
  data_frame(model = c('LR ~ D', 'LR ~ D * Imp', 'LR ~ D * (I + N + LTC)', 
                       'LR ~ D * (I * LTC + N * LTC)'), 
           `var-w sites` = c(mod1$k, impact_ne_w$k, drivers_scaled$k, interactions_scaled$k), 
           `var-w studies` = c(mod1$s.nlevels, impact_ne_w$s.nlevels, drivers_scaled$s.nlevels, interactions_scaled$s.nlevels), 
           `samp-w sites` = c(mod1_ss$k, impact_ne_ss$k, drivers_scaled_ss$k, interactions_ss_scaled$k), 
           `samp-w studies` = c(mod1_ss$s.nlevels, impact_ne_ss$s.nlevels, drivers_scaled_ss$s.nlevels, interactions_ss_scaled$s.nlevels))




### VARIANCE WEIGHTED SUBSET - FOR SAMPLE SIZE WEIGHTED ANALYSIS
vw_subset <- filter(fl_combined, Event != 'Yes') %>% 
            filter(!is.na(vi_SppR_ROM))

## @knitr duration-samp-size-weighted-model-output-vw-subset
mod1_ss_vws <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = vw_subset %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss_vws
mod1_ss_robust_vws <- robust(mod1_ss_vws, cluster=1:mod1_ss_vws$k)
mod1_ss_robust_vws

## @knitr impacts-samp-size-weighted-model-output-vw-subset
impact_ne_ss_vws <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = vw_subset %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_ne_ss_vws
impact_ne_ss_robust_vws <- robust(impact_ne_ss_vws, cluster=1:impact_ne_ss_vws$k)
impact_ne_ss_robust_vws

## @knitr drivers-samp-size-weighted-model-output-scaled-vw-subset
drivers_scaled_ss_vws <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_scaled_ss_vws
drivers_scaled_ss_robust_vws <- robust(drivers_scaled_ss_vws, cluster=1:drivers_scaled_ss_vws$k)
drivers_scaled_ss_robust_vws

## @knitr drivers-samp-size-weighted-model-output-unscaled-vw-subset
drivers_unscaled_ss_vws <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * ((scaled_invs) + (sliced_ltc) + (mean_nuts)))
#drivers_unscaled_ss_vws
drivers_unscaled_ss_robust_vws <- robust(drivers_unscaled_ss_vws, cluster=1:drivers_unscaled_ss_vws$k)
drivers_unscaled_ss_robust_vws

## @knitr drivers-interactions-samp-size-weighted-scaled-vw-subset
interactions_ss_scaled_vws <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))
interactions_ss_scaled_vws 

interactions_ss_scaled_robust_vws <- robust(interactions_ss_scaled_vws, cluster=1:interactions_ss_scaled_vws$k)
interactions_ss_scaled_robust_vws


# @knitr drivers-interactions-samp-size-weighted-unscaled-vw-subset
interactions_ss_unscaled_vws <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs * sliced_ltc + sliced_ltc*mean_nuts))
interactions_ss_unscaled_vws 

interactions_ss_unscaled_robust_vws <- robust(interactions_ss_unscaled_vws, cluster=1:interactions_ss_unscaled_vws$k)


# Sample size weighted - VW SUBSET
## @knitr duration-ss-weighted-model-output-df-vw-subset
mod1_ss_output_df_vws <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_ss_robust_vws$b), 
             estimate = round(as.vector(mod1_ss_robust_vws$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(mod1_ss_robust_vws$b)), digits = 2),
             se = round(mod1_ss_robust_vws$se, digits = 3), 
             pval = round(mod1_ss_robust_vws$pval, digits = 3), 
             ci.lb = round(mod1_ss_robust_vws$ci.lb, digits = 3), 
             ci.ub = round(mod1_ss_robust_vws$ci.ub, digits = 3), 
             k = mod1_ss_vws$k, 
             n = mod1_ss_vws$s.nlevels)

## @knitr impact-ss-weighted-model-output-df-vw-subset
imp_mod_ss_output_df_vws <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_ss_robust_vws$b), 
             estimate = round(as.vector(impact_ne_ss_robust_vws$b), digits = 3),
            `% change` = round(get_percent_change(as.vector(impact_ne_ss_robust_vws$b)), digits = 2), 
             se = round(impact_ne_ss_robust_vws$se, digits = 3), 
             pval = round(impact_ne_ss_robust_vws$pval, digits = 3), 
             ci.lb = round(impact_ne_ss_robust_vws$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_ss_robust_vws$ci.ub, digits = 3), 
             k = impact_ne_ss_vws$k, 
             n = impact_ne_ss_vws$s.nlevels) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))


## @knitr drivers-ss-weighted-model-output-df-scaled-vw-subset
drivers_mod_ss_output_df_vws <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_ss_robust_vws$b)), 
             estimate = round(as.vector(drivers_scaled_ss_robust_vws$b), digits = 3), 
            `% change` = round(get_percent_change(as.vector(drivers_unscaled_ss_robust_vws$b)), digits = 2),
             se = round(drivers_scaled_ss_robust_vws$se, digits = 3), 
             pval = round(drivers_scaled_ss_robust_vws$pval, digits = 3), 
             ci.lb = round(drivers_scaled_ss_robust_vws$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_ss_robust_vws$ci.ub, digits = 3), 
             k = drivers_scaled_ss_vws$k, 
             n = drivers_scaled_ss_vws$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


## @knitr drivers-interactions-ss-weighted-model-output-df-scaled-vw-subset
interactions_ss_output_df_vws <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(interactions_ss_scaled_robust_vws$b)), 
             estimate = round(as.vector(interactions_ss_scaled_robust_vws$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(interactions_ss_unscaled_robust_vws$b)), digits = 2), 
             se = round(interactions_ss_scaled_robust_vws$se, digits = 3), 
             pval = round(interactions_ss_scaled_robust_vws$pval, digits = 3), 
             ci.lb = round(interactions_ss_scaled_robust_vws$ci.lb, digits = 3), 
             ci.ub = round(interactions_ss_scaled_robust_vws$ci.ub, digits = 3), 
             k = interactions_ss_scaled_vws$k, 
             n = interactions_ss_scaled_vws$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))