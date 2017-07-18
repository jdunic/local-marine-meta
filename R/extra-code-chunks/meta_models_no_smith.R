#source('01_meta_models.R')

no_smith <- filter(no_event, Reference != 'Smith et al 2006')


## SMITH HAD NO VARIANCES

# ------------------------------------------------------------------------------
# Sample size weighted analysis:
# ------------------------------------------------------------------------------

## @knitr duration-samp-size-weighted-model-output-no-smith
mod1_ss_ns <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_smith %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss_ns
mod1_ss_robust_ns <- robust(mod1_ss_ns, cluster=1:mod1_ss_ns$k)
mod1_ss_robust_ns

## @knitr impacts-samp-size-weighted-model-output-no-smith
impact_ne_ss_ns <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_smith %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_ne_ss_ns
impact_ne_ss_robust_ns <- robust(impact_ne_ss_ns, cluster=1:impact_ne_ss_ns$k)
impact_ne_ss_robust_ns

## @knitr drivers-samp-size-weighted-model-output-scaled-no-smith
drivers_scaled_ss_ns <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_smith %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_smith %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_scaled_ss_ns
drivers_scaled_ss_robust_ns <- robust(drivers_scaled_ss_ns, cluster=1:drivers_scaled_ss_ns$k)
drivers_scaled_ss_robust_ns


## @knitr drivers-interactions-samp-size-weighted-scaled-no-smith
interactions_ss_scaled_ns <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_smith %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_smith %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))
interactions_ss_scaled_ns 

interactions_ss_scaled_robust_ns <- robust(interactions_ss_scaled_ns, cluster=1:interactions_ss_scaled_ns$k)
interactions_ss_scaled_robust_ns


# @knitr drivers-interactions-samp-size-weighted-unscaled-no-smith
interactions_ss_unscaled_ns <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_smith %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_smith %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs * sliced_ltc + sliced_ltc*mean_nuts))
interactions_ss_unscaled_ns 

interactions_ss_unscaled_robust_ns <- robust(interactions_ss_unscaled_ns, cluster=1:interactions_ss_unscaled_ns$k)
interactions_ss_unscaled_robust_ns



# ------------------------------------------------------------------------------
# Comparison of model output tables
# ------------------------------------------------------------------------------

# Sample size weighted 
# ------------------------------------------------------------------------------

## @knitr duration-ss-weighted-model-output-df-no-smith
mod1_ss_output_df_ns <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_ss_robust_ns$b), 
             estimate = round(as.vector(mod1_ss_robust_ns$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(mod1_ss_robust_ns$b)), digits = 2),
             se = round(mod1_ss_robust_ns$se, digits = 3), 
             pval = round(mod1_ss_robust_ns$pval, digits = 3), 
             ci.lb = round(mod1_ss_robust_ns$ci.lb, digits = 3), 
             ci.ub = round(mod1_ss_robust_ns$ci.ub, digits = 3), 
             k = mod1_ss_ns$k, 
             n = mod1_ss_ns$s.nlevels)

## @knitr impact-ss-weighted-model-output-df-no-smith
imp_mod_ss_output_df_ns <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_ss_robust_ns$b), 
             estimate = round(as.vector(impact_ne_ss_robust_ns$b), digits = 3),
            `% change` = round(get_percent_change(as.vector(impact_ne_ss_robust_ns$b)), digits = 2), 
             se = round(impact_ne_ss_robust_ns$se, digits = 3), 
             pval = round(impact_ne_ss_robust_ns$pval, digits = 3), 
             ci.lb = round(impact_ne_ss_robust_ns$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_ss_robust_ns$ci.ub, digits = 3), 
             k = impact_ne_ss_ns$k, 
             n = impact_ne_ss_ns$s.nlevels) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))


## @knitr drivers-ss-weighted-model-output-df-scaled-no-smith
drivers_mod_ss_output_df_ns <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_ss_robust_ns$b)), 
             estimate = round(as.vector(drivers_scaled_ss_robust_ns$b), digits = 3), 
            `% change` = round(get_percent_change(as.vector(drivers_unscaled_ss_robust$b)), digits = 2),
             se = round(drivers_scaled_ss_robust_ns$se, digits = 3), 
             pval = round(drivers_scaled_ss_robust_ns$pval, digits = 3), 
             ci.lb = round(drivers_scaled_ss_robust_ns$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_ss_robust_ns$ci.ub, digits = 3), 
             k = drivers_scaled_ss_ns$k, 
             n = drivers_scaled_ss_ns$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


## @knitr drivers-interactions-ss-weighted-model-output-df-scaled-no-smith
interactions_ss_output_df_ns <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(interactions_ss_scaled_robust_ns$b)), 
             estimate = round(as.vector(interactions_ss_scaled_robust_ns$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(interactions_ss_unscaled_robust$b)), digits = 2), 
             se = round(interactions_ss_scaled_robust_ns$se, digits = 3), 
             pval = round(interactions_ss_scaled_robust_ns$pval, digits = 3), 
             ci.lb = round(interactions_ss_scaled_robust_ns$ci.lb, digits = 3), 
             ci.ub = round(interactions_ss_scaled_robust_ns$ci.ub, digits = 3), 
             k = interactions_ss_scaled_ns$k, 
             n = interactions_ss_scaled_ns$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


## PLOTS

# DURATION PLOT
## @knitr no-event-data-plotted-with-duration-var-weighted-no-smith
duration_ns <- 
filter(no_smith, !is.na(yi_SppR_ROM)) %>%
ggplot(data = .) +
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 1) + 
  ylab('Log ratio') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') +
  #ggtitle('Weighted Analysis') + 
  xlab('Duration (years)') +
  ylim(-2, 2) + 
  geom_line(data = data.frame(x = 1:55, y = predict(mod1_ss_ns, 1:55)$pred), aes(x = x, y = y), colour = 'blue') +
  geom_ribbon(data = data.frame(x = 1:55, ymin = predict(mod1_ss_ns, 1:55)$ci.lb, ymax = predict(mod1_ss_ns, 1:55)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.1)
duration_ns

# IMPACTS PLOTS
## @knitr impacts-ss-weighted-intercept-coefficient-estimates-no-smith  
mk_rma_summary_df(impact_ne_ss_ns) %>%
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


# DRIVERS PLOTS

## @knitr drivers-ss-weighted-int-scaled-par-est-plot-short-long-breakdown-no-smith
model_driver_vector_ns <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration:Invasives', 
    'Duration:LTC', 'Duration:Nutrients')
ordered_driver_vector_ns <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration:Invasives', 
    'Duration:Nutrients', 'Duration:LTC')

driver_summary_plot_short_ns <- 
  mk_rma_summary_df(drivers_scaled_ss_ns) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector_ns, 
                             levels = rev(ordered_driver_vector_ns))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.15, 0.15)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short

driver_summary_plot_long_ns <- 
  mk_rma_summary_df(drivers_scaled_ss_ns) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector_ns, 
                             levels = rev(ordered_driver_vector_ns))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.09, 0.09)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, driver_summary_plot_short_ns, text_b, driver_summary_plot_long_ns, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))

# DRIVERS INTERACTION PLOTS
## @knitr interactions-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown-no-smith
model_interaction_vector_ns <- 
  c('Duration', 
    'Invasives', 'LTC', 'Nutrients', 
    'Invasives:LTC', 'Nutrients:LTC', 
    'Duration:Invasives', 'Duration:LTC', 'Duration:Nutrients', 
    'Duration:Invasives:LTC', 'Duration:Nutrients:LTC')
ordered_interaction_vector_ns<- 
  c('Duration', 
    'Invasives', 'Nutrients', 'LTC', 
    'Invasives:LTC', 'Nutrients:LTC', 
    'Duration:Invasives', 'Duration:Nutrients', 'Duration:LTC', 
    'Duration:Invasives:LTC', 'Duration:Nutrients:LTC')

interaction_summary_plot_short_ns  <- 
  mk_rma_summary_df(interactions_ss_scaled_ns) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_interaction_vector_ns, 
                             levels = rev(ordered_interaction_vector_ns))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no'
      , 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-1.5, 1.5)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interaction_summary_plot_short

interaction_summary_plot_long_ns <- 
  mk_rma_summary_df(interactions_ss_scaled_ns) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_interaction_vector_ns, 
                             levels = rev(ordered_interaction_vector_ns))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no'
      , 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.3, 0.3)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interaction_summary_plot_long_ns

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, interaction_summary_plot_short_ns, text_b, interaction_summary_plot_long_ns, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))

