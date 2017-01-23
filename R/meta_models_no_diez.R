#source('meta_models.R')

no_diez <- filter(no_event, Reference != 'Diez et al. 2011')


## VARIANCE WEIGHTED MODELS
## @knitr duration-var-weighted-intercept-no-diez
mod1_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_diez %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_nd

## @knitr impacts-var-weighted-intercept-no-diez
impact_ne_w_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_diez,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_w_nd


## @knitr drivers-var-weighted-intercept-no-diez
drivers_scaled_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3),
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled_nd

## @knitr drivers-var-weighted-collinearity-table-no-diez
drivers_cor_table_nd <- 
no_diez %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% mutate(mean_invs = mean_invs * 10^-3) %>% select(mean_nuts, mean_invs, sliced_ltc, Duration) %>% cor()

## @knitr drivers-var-weighted-intercept-not-scaled-no-diez
drivers_unscaled_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_diez, #%>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (mean_invs + sliced_ltc + mean_nuts))
drivers_unscaled_nd

## @knitr drivers-interactions-var-weighted-scaled-no-diez
# Invasives * temperature (Sorte et al 2010, )
# Nutrients * temperature (Binzer et al 2012)
interactions_scaled_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc) * scale(mean_nuts)))

# Likely low power, but at least the model does not appear to be 
# overparameterised. 
#profile(interactions_scaled_nd)
interactions_scaled_nd

## @knitr drivers-interactions-var-weighted-unscaled-no-diez
interactions_unscaled_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs * sliced_ltc + sliced_ltc * mean_nuts))

# Likely low power, but at least the model does not appear to be 
# overparameterised. 
#profile(interactions_unscaled_nd)
interactions_unscaled_nd



# ------------------------------------------------------------------------------
# Sample size weighted analysis:
# ------------------------------------------------------------------------------

## @knitr duration-samp-size-weighted-model-output-no-diez
mod1_ss_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_diez %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
#mod1_ss_nd
mod1_ss_robust_nd <- robust(mod1_ss_nd, cluster=1:mod1_ss_nd$k)
mod1_ss_robust_nd

## @knitr impacts-samp-size-weighted-model-output-no-diez
impact_ne_ss_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_diez %>% filter(!is.na(mean_imps) & !is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
#impact_ne_ss_nd
impact_ne_ss_robust_nd <- robust(impact_ne_ss_nd, cluster=1:impact_ne_ss_nd$k)
impact_ne_ss_robust_nd

## @knitr drivers-samp-size-weighted-model-output-scaled-no-diez
drivers_scaled_ss_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
#drivers_scaled_ss_nd
drivers_scaled_ss_robust_nd <- robust(drivers_scaled_ss_nd, cluster=1:drivers_scaled_ss_nd$k)
drivers_scaled_ss_robust_nd


## @knitr drivers-interactions-samp-size-weighted-scaled-no-diez
interactions_ss_scaled_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))
interactions_ss_scaled_nd 

interactions_ss_scaled_robust_nd <- robust(interactions_ss_scaled_nd, cluster=1:interactions_ss_scaled_nd$k)
interactions_ss_scaled_robust_nd


# @knitr drivers-interactions-samp-size-weighted-unscaled-no-diez
interactions_ss_unscaled_nd <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
         #data = no_diez %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs * sliced_ltc + sliced_ltc*mean_nuts))
interactions_ss_unscaled_nd 

interactions_ss_unscaled_robust_nd <- robust(interactions_ss_unscaled_nd, cluster=1:interactions_ss_unscaled_nd$k)
interactions_ss_unscaled_robust_nd



# ------------------------------------------------------------------------------
# Comparison of model output tables
# ------------------------------------------------------------------------------

# Variance weighted 
# ------------------------------------------------------------------------------

## @knitr duration-var-weighted-model-output-no-diez
mod1_output_df_nd <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_nd$b), 
             estimate = round(as.vector(mod1_nd$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(mod1_nd$b)), digits = 2), 
             se = round(mod1_nd$se, digits = 3), 
             pval = round(mod1_nd$pval, digits = 3), 
             ci.lb = round(mod1_nd$ci.lb, digits = 3), 
             ci.ub = round(mod1_nd$ci.ub, digits = 3), 
             k = as.numeric(mod1_nd$k), 
             n = as.numeric(mod1_nd$s.nlevels))


## @knitr impact-var-weighted-model-output-no-diez
imp_mod_output_df_nd <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_w_nd$b), 
             estimate = round(as.vector(impact_ne_w_nd$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(impact_ne_w_nd$b)), digits = 2), 
             se = round(impact_ne_w_nd$se, digits = 3), 
             pval = round(impact_ne_w_nd$pval, digits = 3), 
             ci.lb = round(impact_ne_w_nd$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_w_nd$ci.ub, digits = 3), 
             k = as.numeric(impact_ne_w_nd$k), 
             n = as.numeric(impact_ne_w_nd$s.nlevels)) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))

## @knitr drivers-var-weighted-model-output-df-scaled-no-diez
drivers_mod_output_df_nd <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_nd$b)), 
             estimate = round(as.vector(drivers_scaled_nd$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(drivers_unscaled_nd$b)), digits = 2), 
             se = round(drivers_scaled_nd$se, digits = 3), 
             pval = round(drivers_scaled_nd$pval, digits = 3), 
             ci.lb = round(drivers_scaled_nd$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_nd$ci.ub, digits = 3), 
             k = as.numeric(drivers_scaled_nd$k), 
             n = as.numeric(drivers_scaled_nd$s.nlevels)) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


## @knitr interactions-var-weighted-model-output-no-diez
interactions_output_df_nd <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(interactions_scaled_nd$b)), 
             estimate = round(as.vector(interactions_scaled_nd$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(interactions_unscaled_nd$b)), digits = 2), 
             se = round(interactions_scaled_nd$se, digits = 3), 
             pval = round(interactions_scaled_nd$pval, digits = 3), 
             ci.lb = round(interactions_scaled_nd$ci.lb, digits = 3), 
             ci.ub = round(interactions_scaled_nd$ci.ub, digits = 3), 
             k = as.numeric(interactions_scaled_nd$k), 
             n = as.numeric(interactions_scaled_nd$s.nlevels)) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


# Sample size weighted 
# ------------------------------------------------------------------------------

## @knitr duration-ss-weighted-model-output-df-no-diez
mod1_ss_output_df_nd <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_ss_robust_nd$b), 
             estimate = round(as.vector(mod1_ss_robust_nd$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(mod1_ss_robust_nd$b)), digits = 2),
             se = round(mod1_ss_robust_nd$se, digits = 3), 
             pval = round(mod1_ss_robust_nd$pval, digits = 3), 
             ci.lb = round(mod1_ss_robust_nd$ci.lb, digits = 3), 
             ci.ub = round(mod1_ss_robust_nd$ci.ub, digits = 3), 
             k = mod1_ss_nd$k, 
             n = mod1_ss_nd$s.nlevels)

## @knitr impact-ss-weighted-model-output-df-no-diez
imp_mod_ss_output_df_nd <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_ss_robust_nd$b), 
             estimate = round(as.vector(impact_ne_ss_robust_nd$b), digits = 3),
            `% change` = round(get_percent_change(as.vector(impact_ne_ss_robust_nd$b)), digits = 2), 
             se = round(impact_ne_ss_robust_nd$se, digits = 3), 
             pval = round(impact_ne_ss_robust_nd$pval, digits = 3), 
             ci.lb = round(impact_ne_ss_robust_nd$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_ss_robust_nd$ci.ub, digits = 3), 
             k = impact_ne_ss_nd$k, 
             n = impact_ne_ss_nd$s.nlevels) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))


## @knitr drivers-ss-weighted-model-output-df-scaled-no-diez
drivers_mod_ss_output_df_nd <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_ss_robust_nd$b)), 
             estimate = round(as.vector(drivers_scaled_ss_robust_nd$b), digits = 3), 
            `% change` = round(get_percent_change(as.vector(drivers_unscaled_ss_robust$b)), digits = 2),
             se = round(drivers_scaled_ss_robust_nd$se, digits = 3), 
             pval = round(drivers_scaled_ss_robust_nd$pval, digits = 3), 
             ci.lb = round(drivers_scaled_ss_robust_nd$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_ss_robust_nd$ci.ub, digits = 3), 
             k = drivers_scaled_ss_nd$k, 
             n = drivers_scaled_ss_nd$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


## @knitr drivers-interactions-ss-weighted-model-output-df-scaled
interactions_ss_output_df <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(interactions_ss_scaled_robust_nd$b)), 
             estimate = round(as.vector(interactions_ss_scaled_robust_nd$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(interactions_ss_unscaled_robust$b)), digits = 2), 
             se = round(interactions_ss_scaled_robust_nd$se, digits = 3), 
             pval = round(interactions_ss_scaled_robust_nd$pval, digits = 3), 
             ci.lb = round(interactions_ss_scaled_robust_nd$ci.lb, digits = 3), 
             ci.ub = round(interactions_ss_scaled_robust_nd$ci.ub, digits = 3), 
             k = interactions_ss_scaled_nd$k, 
             n = interactions_ss_scaled_nd$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))


## PLOTS

# DURATION PLOT
## @knitr no-event-data-plotted-with-duration-var-weighted-no-diez
duration_w_nd <- 
filter(no_diez, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)) %>%
ggplot(data = .) +
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 1) + 
  ylab('Log ratio') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') +
  #ggtitle('Weighted Analysis') + 
  xlab('Duration (years)') +
  ylim(-2, 2) + 
  geom_line(data = data.frame(x = 1:41, y = predict(mod1_nd, 1:41)$pred), aes(x = x, y = y), colour = 'blue') +
  geom_ribbon(data = data.frame(x = 1:41, ymin = predict(mod1_nd, 1:41)$ci.lb, ymax = predict(mod1_nd, 1:41)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.1)
duration_w_nd

# IMPACTS PLOTS
## @knitr impacts-var-weighted-intercept-coefficient-estimates-no-diez  
mk_rma_summary_df(impact_ne_w_nd) %>%
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

## @knitr drivers-var-weighted-int-scaled-par-est-plot-short-long-breakdown-no-diez
model_driver_vector_nd <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration:Invasives', 
    'Duration:LTC', 'Duration:Nutrients')
ordered_driver_vector_nd <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration:Invasives', 
    'Duration:Nutrients', 'Duration:LTC')

driver_summary_plot_short_nd <- 
  mk_rma_summary_df(drivers_scaled_nd) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector_nd, 
                             levels = rev(ordered_driver_vector_nd))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-6.4, 6.4)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short

driver_summary_plot_long_nd <- 
  mk_rma_summary_df(drivers_scaled_nd) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector_nd, 
                             levels = rev(ordered_driver_vector_nd))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-1.4, 1.4)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, driver_summary_plot_short_nd, text_b, driver_summary_plot_long_nd, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))

# DRIVERS INTERACTION PLOTS
## @knitr interactions-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown-no-diez
model_interaction_vector_nd <- 
  c('Duration', 
    'Invasives', 'LTC', 'Nutrients', 
    'Invasives:LTC', 'Nutrients:LTC', 
    'Duration:Invasives', 'Duration:LTC', 'Duration:Nutrients', 
    'Duration:Invasives:LTC', 'Duration:Nutrients:LTC')
ordered_interaction_vector_nd<- 
  c('Duration', 
    'Invasives', 'Nutrients', 'LTC', 
    'Invasives:LTC', 'Nutrients:LTC', 
    'Duration:Invasives', 'Duration:Nutrients', 'Duration:LTC', 
    'Duration:Invasives:LTC', 'Duration:Nutrients:LTC')

interaction_summary_plot_short_nd  <- 
  mk_rma_summary_df(interactions_scaled_nd) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_interaction_vector_nd, 
                             levels = rev(ordered_interaction_vector_nd))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no'
      , 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-41, 41)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interaction_summary_plot_short

interaction_summary_plot_long_nd <- 
  mk_rma_summary_df(interactions_scaled_nd) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_interaction_vector_nd, 
                             levels = rev(ordered_interaction_vector_nd))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no'
      , 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-18, 18)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#interaction_summary_plot_long_nd

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, interaction_summary_plot_short_nd, text_b, interaction_summary_plot_long_nd, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))

