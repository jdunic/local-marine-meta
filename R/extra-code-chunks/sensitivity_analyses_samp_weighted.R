source('02_sensitivity_analyses.R')

#-------------------------------------------------------------------------------
# Sample size weighted leave-1-out analyses
#-------------------------------------------------------------------------------

## @knitr duration-samp-size-weighted-model-output
mod1_leave1out_studies_ss <- 
  leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM)), #max_dist / 1000 < 10), 
  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr impacts-samp-size-weighted-model-output
impacts_leave1out_studies_ss <- 
  leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(mean_imps)), #, max_dist / 1000 < 10), 
                   leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * mean_imps - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr drivers-samp-size-weighted-model-output
drivers_leave1out_studies_ss <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(mean_invs) & !is.na(sliced_ltc) & 
                 !is.na(mean_nuts)) %>% #, max_dist / 1000 < 10) %>% 
                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
                  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc) - 1, data = leave1out_df)', samp_size_weighted = TRUE)


## @knitr leave1out-interactions-samp-size-weighted-scaled
#interactions_leave1out_studies_ss_weighted <- 
#leave1out.rma.mv(model_input_df = no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(mean_nuts)) %>% mutate(scaled_invs = mean_invs * 10^-3), 
#                 leave1out_col = 'Study.ID', 
#                 model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(!is.na(yi_SppR_ROM) & !is.na(sliced_ltc) & !is.na(mean_nuts) & !is.na(scaled_invs)), 
#         random = ~ 1 | Study.ID, 
#         mods = ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc)*scale(mean_nuts)))')

## @knitr interactions-leave-1-out-study-ss-weighted-Duration
#leave1out_studies_plot(
#  single_driver_df = filter(mod1_leave1out_studies_ss, moderator == 'Duration') %>% 
#    left_join(., no_event %>% group_by(Study.ID) %>% slice(1) %>% ungroup(),
#              by = c('excluded_study' = 'Study.ID'))) + 
#  ggtitle('Duration')


# Sample size weighted leave-1-out plots
#-------------------------------------------------------------------------------

## @knitr mod1-leave-1-out-study-ss-weighted-plot
mod1_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(mod1_leave1out_studies_ss, moderator == 'Duration') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr impacts-leave-1-out-study-Duration-ss-weighted-plot
impacts_duration_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr impacts-leave-1-out-study-Impacts-ss-weighted-plot
impacts_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_ss, moderator == 'mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Impacts') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr impacts-leave-1-out-study-Duration-Impacts-ss-weighted-plot
duration_impacts_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_ss, moderator == 'Duration:mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Impacts') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-ss-weighted-plot
drivers_duration_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Invasives-ss-weighted-plot
drivers_invasives_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Nutrients-ss-weighted-plot
drivers_nutrients_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-LTC
drivers_LTC_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-Duration-Invasives
drivers_duration_invasives_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration:scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-Nutrients
drivers_duration_nutrients_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration:mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-LTC
drivers_duration_LTC_leave1out_ss_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration:sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


#-------------------------------------------------------------------------------
# Comparing var-weighted and ss-weighted leave-1-out parameter estimates
#-------------------------------------------------------------------------------

## @knitr Duration-leave-1-out-study-Duration-ss-weighted
grid.arrange(mod1_leave1out_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), mod1_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Duration-ss-weighted
grid.arrange(impacts_duration_leave1out_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), impacts_duration_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Impact-ss-weighted
grid.arrange(impacts_leave1out_plot + ggtitle('Var-Weighted\nImpacts') + theme(plot.title = element_text(size = 12)), impacts_leave1out_ss_plot + ggtitle('SampSize-Weighted\nImpacts') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Duration:Impact-ss-weighted
grid.arrange(duration_impacts_leave1out_plot + ggtitle('Var-Weighted\nDuration:Impacts') + theme(plot.title = element_text(size = 12)), duration_impacts_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration:Impacts') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration-ss-weighted
grid.arrange(drivers_duration_leave1out_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), drivers_duration_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Invasives-ss-weighted
grid.arrange(drivers_invasives_leave1out_plot + ggtitle('Var-Weighted\nInvasives') + theme(plot.title = element_text(size = 12)), drivers_invasives_leave1out_ss_plot + ggtitle('SampSize-Weighted\nInvasives') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Nutrients-ss-weighted
grid.arrange(drivers_nutrients_leave1out_plot + ggtitle('Var-Weighted\nNutrients') + theme(plot.title = element_text(size = 12)), drivers_nutrients_leave1out_ss_plot + ggtitle('SampSize-Weighted\nNutrients') + theme(plot.title = element_text(size = 12)), ncol = 2) 

## @knitr Drivers-leave-1-out-study-LTC-ss-weighted
grid.arrange(drivers_LTC_leave1out_plot + ggtitle('Var-Weighted\nLTC') + theme(plot.title = element_text(size = 12)), drivers_LTC_leave1out_ss_plot + ggtitle('SampSize-Weighted\nLTC') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:Invasives-ss-weighted
grid.arrange(drivers_duration_invasives_leave1out_plot + ggtitle('Var-Weighted\nDuration:Invasives') + theme(plot.title = element_text(size = 12)), drivers_duration_invasives_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration:Invasives') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:Nutrients-ss-weighted
grid.arrange(drivers_duration_nutrients_leave1out_plot + ggtitle('Var-Weighted\nDuration:Nutrients') + theme(plot.title = element_text(size = 12)), drivers_duration_nutrients_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration:Nutrients') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:LTC-ss-weighted
grid.arrange(drivers_duration_LTC_leave1out_plot + ggtitle('Var-Weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), drivers_duration_LTC_leave1out_ss_plot + ggtitle('SampSize-Weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), ncol = 2)

# Leave-1-out sample size weighted with variance-weighted data subset
## @knitr drivers-samp-size-weighted-model-output
drivers_leave1out_studies_ss_vw_subset <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & !is.na(sliced_ltc) & 
                 !is.na(mean_nuts)) %>% 
                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
                  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc) - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr drivers-leave-1-out-study-Duration-LTC-var-weighted-data-subset
drivers_duration_LTC_leave1out_ss_vw_subset_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss_vw_subset, moderator == 'Duration:sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('SampSize-Weighted\nDuration:LTC') + 
  theme(axis.title.y = element_blank()) + 
  theme(plot.title = element_text(size = 12))

drivers_duration_LTC_leave1out_ss_vw_subset_plot

## @knitr leave-1-out-compare-Duration:LTC-var-weighted-data-subset
grid.arrange(drivers_duration_LTC_leave1out_plot + ggtitle('Var-Weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), drivers_duration_LTC_leave1out_ss_vw_subset_plot, ncol = 2)


#-------------------------------------------------------------------------------
# samp size-weighted leave-1-out analyses
#-------------------------------------------------------------------------------
## @knitr duration-samp-size-weighted-model-output
mod1_leave1out_studies_ss <- 
  leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)), leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr impacts-samp-size-weighted-model-output
impacts_leave1out_studies_ss <- 
  leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM), !is.na(mean_imps)), 
                   leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * mean_imps - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr drivers-samp-size-weighted-model-output
drivers_leave1out_studies_ss <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM), !is.na(mean_invs), !is.na(sliced_ltc), !is.na(mean_nuts)) %>%) %>% 
                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
                  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc) - 1, data = leave1out_df)', samp_size_weighted = TRUE)

# PLOTS

mod1_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(mod1_leave1out_studies_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')


impacts_duration_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')

impacts_impacts_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_ss, moderator == 'mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Impacts')

impacts_duration_impacts_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_ss, moderator == 'Duration:mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration*Impacts')

drivers_duration_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Invasives-ss-weighted-plot
drivers_invasives_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Nutrients-ss-weighted-plot
drivers_nutrients_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-LTC
drivers_ltc_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-Duration-Invasives
drivers_duration_invasives_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration:scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-Nutrients
drivers_duration_nutrients_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration:mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-LTC
drivers_duration_ltc_leave1out_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_ss, moderator == 'Duration:sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


#-------------------------------------------------------------------------------
# samp size-weighted leave-1-out analyses with < 10km 
#-------------------------------------------------------------------------------
## @knitr duration-samp-size-weighted-model-output_10km
mod1_leave1out_studies_10km_ss <- 
  leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM), max_dist / 1000 <= 1), leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr impacts-samp-size-weighted-model-output_10km
impacts_leave1out_studies_10km_ss <- 
  leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM), !is.na(mean_imps), max_dist / 1000 <= 1), 
                   leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * mean_imps - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr drivers-samp-size-weighted-model-output_10km
drivers_leave1out_studies_10km_ss <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM), !is.na(mean_invs), !is.na(sliced_ltc), !is.na(mean_nuts), max_dist / 1000 <= 1) %>% 
                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
                  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = 1 / n1, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc) - 1, data = leave1out_df)', samp_size_weighted = TRUE)

## @knitr leave1out-ss-weighted-10km-plots
mod1_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(mod1_leave1out_studies_10km_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')


impacts_duration_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_10km_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')

impacts_impacts_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_10km_ss, moderator == 'mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Impacts')

impacts_duration_impacts_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_10km_ss, moderator == 'Duration:mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration*Impacts')

drivers_duration_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Invasives-ss-weighted-plot_10km
drivers_invasives_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Nutrients-ss-weighted-plot_10km
drivers_nutrients_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-LTC_10km
drivers_ltc_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-Duration-Invasives_10km
drivers_duration_invasives_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'Duration:scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-Nutrients_10km
drivers_duration_nutrients_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'Duration:mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-LTC_10km
drivers_duration_ltc_leave1out_10km_plot_ss <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km_ss, moderator == 'Duration:sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


#-------------------------------------------------------------------------------
# Comparing var-weighted and ss-weighted <=10 km leave-1-out parameter estimates
#-------------------------------------------------------------------------------

## @knitr Duration-leave-1-out-study-Duration-10km
grid.arrange(mod1_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), mod1_leave1out_10km_plot_ss + ggtitle('ss-weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Duration-10km
grid.arrange(impacts_duration_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), impacts_duration_leave1out_10km_plot_ss + ggtitle('SampSize-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Impact-10km
grid.arrange(impacts_impacts_leave1out_10km_plot + ggtitle('Var-Weighted\nImpacts') + theme(plot.title = element_text(size = 12)), impacts_impacts_leave1out_10km_plot_ss + ggtitle('ss-weighted\nImpacts') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Duration:Impact-10km
grid.arrange(impacts_duration_impacts_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration:Impacts') + theme(plot.title = element_text(size = 12)), impacts_duration_impacts_leave1out_10km_plot_ss + ggtitle('ss-weighted\nDuration:Impacts') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration-10km
grid.arrange(drivers_duration_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), drivers_duration_leave1out_10km_plot_ss + ggtitle('ss-weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Invasives-10km
grid.arrange(drivers_invasives_leave1out_10km_plot + ggtitle('Var-Weighted\nInvasives') + theme(plot.title = element_text(size = 12)), drivers_invasives_leave1out_10km_plot_ss + ggtitle('ss-weighted\nInvasives') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Nutrients-10km
grid.arrange(drivers_nutrients_leave1out_10km_plot + ggtitle('Var-Weighted\nNutrients') + theme(plot.title = element_text(size = 12)), drivers_nutrients_leave1out_10km_plot_ss + ggtitle('ss-weighted\nNutrients') + theme(plot.title = element_text(size = 12)), ncol = 2) 

## @knitr Drivers-leave-1-out-study-LTC-10km
grid.arrange(drivers_ltc_leave1out_10km_plot + ggtitle('Var-Weighted\nLTC') + theme(plot.title = element_text(size = 12)), drivers_ltc_leave1out_10km_plot_ss + ggtitle('ss-weighted\nLTC') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:Invasives-10km
grid.arrange(drivers_duration_invasives_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration:Invasives') + theme(plot.title = element_text(size = 12)), drivers_duration_invasives_leave1out_10km_plot_ss + ggtitle('ss-weighted\nDuration:Invasives') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:Nutrients-10km
grid.arrange(drivers_duration_nutrients_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration:Nutrients') + theme(plot.title = element_text(size = 12)), drivers_duration_nutrients_leave1out_10km_plot_ss + ggtitle('ss-weighted\nDuration:Nutrients') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:LTC-10km
grid.arrange(drivers_duration_ltc_leave1out_10km_plot + ggtitle('Var-Weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), drivers_duration_ltc_leave1out_10km_plot_ss + ggtitle('ss-weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), ncol = 2)

