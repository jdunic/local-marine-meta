## @knitr prep-sensitivity-analyses
#library(readr)
#library(dplyr)
#library(metafor)
#library(gridExtra)
#library(ggplot2)

source('00_functions.R')

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  mutate(Reference = replace(Reference, Reference == 'Bates, Barrett, Stuart-Smith, Holbrook, Thompson, and Edgar', 'Bates et al. 2013')) %>% 
  mutate(Reference = replace(Reference, Reference == 'GIBSON, RN; ANSELL, AD; ROBB, L', 'Lazzari et al. 1999')) %>% 
  mutate(Reference = replace(Reference, Reference == 'Bergmann, M.; Soltwedel, T.; Klages, M.', 'Bergmann et al. 2011')) %>% 
  filter(Study.ID != 'Shimanaga') %>% 
  filter(Event != 'Yes', Site != 'Enfermeria', Reference != 'Keller et al. 2012')

no_event <- filter(fl_combined, Event != 'Yes')
  
mod1_leave1out_studies <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
                 !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration - 1, data = leave1out_df)')

impacts_leave1out_studies <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
                 !is.na(vi_SppR_ROM) & !is.na(mean_imps) & vi_SppR_ROM > 0), 
                 leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration * mean_imps - 1, data = leave1out_df)')

drivers_leave1out_studies <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
                 !is.na(vi_SppR_ROM) & !is.na(mean_invs) & !is.na(sliced_ltc) & 
                 !is.na(mean_nuts) & vi_SppR_ROM > 0) %>% 
                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
                  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc) - 1, data = leave1out_df)')

#mod1_leave1out_sites <- 
#leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
#                 !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0), leave1out_col = 'id', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration, data = leave1out_df)')

#impacts_leave1out_sites <- 
#leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
#                 !is.na(vi_SppR_ROM) & !is.na(mean_imps) & vi_SppR_ROM > 0), 
#                 leave1out_col = 'id', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration * mean_imps, data = leave1out_df)')

#drivers_leave1out_sites <- 
#leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
#                 !is.na(vi_SppR_ROM) & !is.na(mean_invs) & !is.na(sliced_ltc) & 
#                 !is.na(mean_nuts) & vi_SppR_ROM > 0) %>% 
#                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
#                  leave1out_col = 'id', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc), data = leave1out_df)')

# Leave-1-out study var-weighted plots
#-------------------------------------------------------------------------------

## @knitr mod1-leave-1-out-study
mod1_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(mod1_leave1out_studies, moderator == 'Duration') %>% 
  left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE), by = c('excluded_study' = 'Study.ID')))
mod1_leave1out_plot

## @knitr impacts-leave-1-out-study-Duration
impacts_duration_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')
impacts_duration_leave1out_plot

## @knitr impacts-leave-1-out-study-Impacts
impacts_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies, moderator == 'mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Impacts')
impacts_leave1out_plot

## @knitr impacts-leave-1-out-study-Duration-Impacts
duration_impacts_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies, moderator == 'Duration:mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Impacts')
duration_impacts_leave1out_plot

## @knitr drivers-leave-1-out-study-Duration
drivers_duration_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')
drivers_duration_leave1out_plot

## @knitr drivers-leave-1-out-study-Invasives
drivers_invasives_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Invasives')

## @knitr drivers-leave-1-out-study-Nutrients
drivers_nutrients_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Nutrients')

## @knitr drivers-leave-1-out-study-LTC
drivers_LTC_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Linear temperature change')


## @knitr drivers-leave-1-out-study-Duration-Invasives
drivers_duration_invasives_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'Duration:scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Invasives')

## @knitr drivers-leave-1-out-study-Duration-Nutrients
drivers_duration_nutrients_leave1out_plot <-
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'Duration:mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Nutrients')

## @knitr drivers-leave-1-out-study-Duration-LTC
drivers_duration_LTC_leave1out_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies, moderator == 'Duration:sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Linear temperature change')

## @knitr impacts-leave-1-out-study-nutrients-3-panel
grid.arrange(impacts_duration_leave1out_plot, impacts_leave1out_plot, duration_impacts_leave1out_plot, ncol = 3)

## @knitr drivers-leave-1-out-study-nutrients-short-long-plot
grid.arrange(drivers_nutrients_leave1out_plot, drivers_duration_nutrients_leave1out_plot, ncol = 2)

## @knitr drivers-leave-1-out-study-invasives-short-long-plot
grid.arrange(drivers_invasives_leave1out_plot, drivers_duration_invasives_leave1out_plot, ncol = 2)

## @knitr drivers-leave-1-out-study-LTC-short-long-plot
grid.arrange(drivers_LTC_leave1out_plot, drivers_duration_LTC_leave1out_plot, ncol = 2)



## @knitr funnel-plots-and-extra
#par(mfrow = c(1, 3))
funnel(mod1, main="Funnel Plot for Duration Analysis")
funnel(impact_ne_w, main="Funnel Plot for Impacts Analysis")
funnel(drivers_scaled, main="Funnel Plot for Drivers Analysis")

#test = lm(rstandard(mod1)$z ~ diag(mod1$V))

#test <- data_frame(residuals = rstandard(mod1)$z, variance = diag(mod1$V), study = no_event$Study.ID[mod1$not.na])

#ggplot(data = test, aes(x = residuals, y = variance, colour = study)) + 
#  geom_point() + 
#  theme_minimal() +
#  theme(legend.position = 'none')


## @knitr likelihood-profile-plots-var-weighted-models

# By examining the profile likelihood plots of the variance and correlation 
# components, we can determine that we are in fact in trouble (or in this case, 
# that we're not actually in trouble because these look good)

profile(mod1)

profile(impact_ne_w)

profile(drivers)




#-------------------------------------------------------------------------------
# var-weighted leave-1-out analyses with < 10km 
#-------------------------------------------------------------------------------
mod1_leave1out_studies_10km <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
                 !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0 & max_dist / 1000 <= 10), leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration - 1, data = leave1out_df)')

mod1_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(mod1_leave1out_studies_10km, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')

impacts_leave1out_studies_10km <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
                 !is.na(vi_SppR_ROM) & !is.na(mean_imps) & vi_SppR_ROM > 0 & 
                 max_dist / 1000 <= 10), 
                 leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration * mean_imps - 1, data = leave1out_df)')

impacts_duration_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_10km, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration')

impacts_impacts_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_10km, moderator == 'mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Impacts')

impacts_duration_impacts_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_leave1out_studies_10km, moderator == 'Duration:mean_imps') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration*Impacts')

drivers_leave1out_studies_10km <- 
leave1out.rma.mv(model_input_df = filter(no_event, !is.na(yi_SppR_ROM) & 
                 !is.na(vi_SppR_ROM) & !is.na(mean_invs) & !is.na(sliced_ltc) & 
                 !is.na(mean_nuts) & vi_SppR_ROM > 0 & max_dist / 1000 <= 10) %>% 
                  mutate(scaled_invs = (mean_invs * 10 ^ -3)), 
                  leave1out_col = 'Study.ID', model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration * (scaled_invs + mean_nuts + sliced_ltc) - 1, data = leave1out_df)')

drivers_duration_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'Duration') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Invasives-ss-weighted-plot
drivers_invasives_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Nutrients-ss-weighted-plot
drivers_nutrients_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, Reference),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-LTC
drivers_ltc_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())


## @knitr drivers-leave-1-out-study-Duration-Invasives
drivers_duration_invasives_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'Duration:scaled_invs') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Invasives') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-Nutrients
drivers_duration_nutrients_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'Duration:mean_nuts') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Nutrients') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())

## @knitr drivers-leave-1-out-study-Duration-LTC
drivers_duration_ltc_leave1out_10km_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_leave1out_studies_10km, moderator == 'Duration:sliced_ltc') %>% 
    left_join(., no_event %>% distinct(Study.ID, .keep_all = TRUE),
              by = c('excluded_study' = 'Study.ID'))) + 
  ggtitle('Duration:Linear temperature change') + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())



#-------------------------------------------------------------------------------
# Comparing var-weighted and var-weighted <=10 km leave-1-out parameter estimates
#-------------------------------------------------------------------------------

## @knitr Duration-leave-1-out-study-Duration-10km
grid.arrange(mod1_leave1out_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), mod1_leave1out_10km_plot + ggtitle('10km var-weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Duration-10km
grid.arrange(impacts_duration_leave1out_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), impacts_duration_leave1out_10km_plot + ggtitle('SampSize-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Impact-10km
grid.arrange(impacts_leave1out_plot + ggtitle('Var-Weighted\nImpacts') + theme(plot.title = element_text(size = 12)), impacts_impacts_leave1out_10km_plot + ggtitle('10km var-weighted\nImpacts') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Impacts-leave-1-out-study-Duration:Impact-10km
grid.arrange(duration_impacts_leave1out_plot + ggtitle('Var-Weighted\nDuration:Impacts') + theme(plot.title = element_text(size = 12)), impacts_duration_impacts_leave1out_10km_plot + ggtitle('10km var-weighted\nDuration:Impacts') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration-10km
grid.arrange(drivers_duration_leave1out_plot + ggtitle('Var-Weighted\nDuration') + theme(plot.title = element_text(size = 12)), drivers_duration_leave1out_10km_plot + ggtitle('10km var-weighted\nDuration') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Invasives-10km
grid.arrange(drivers_invasives_leave1out_plot + ggtitle('Var-Weighted\nInvasives') + theme(plot.title = element_text(size = 12)), drivers_invasives_leave1out_10km_plot + ggtitle('10km var-weighted\nInvasives') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Nutrients-10km
grid.arrange(drivers_nutrients_leave1out_plot + ggtitle('Var-Weighted\nNutrients') + theme(plot.title = element_text(size = 12)), drivers_nutrients_leave1out_10km_plot + ggtitle('10km var-weighted\nNutrients') + theme(plot.title = element_text(size = 12)), ncol = 2) 

## @knitr Drivers-leave-1-out-study-LTC-10km
grid.arrange(drivers_LTC_leave1out_plot + ggtitle('Var-Weighted\nLTC') + theme(plot.title = element_text(size = 12)), drivers_ltc_leave1out_10km_plot + ggtitle('10km var-weighted\nLTC') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:Invasives-10km
grid.arrange(drivers_duration_invasives_leave1out_plot + ggtitle('Var-Weighted\nDuration:Invasives') + theme(plot.title = element_text(size = 12)), drivers_duration_invasives_leave1out_10km_plot + ggtitle('10km var-weighted\nDuration:Invasives') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:Nutrients-10km
grid.arrange(drivers_duration_nutrients_leave1out_plot + ggtitle('Var-Weighted\nDuration:Nutrients') + theme(plot.title = element_text(size = 12)), drivers_duration_nutrients_leave1out_10km_plot + ggtitle('10km var-weighted\nDuration:Nutrients') + theme(plot.title = element_text(size = 12)), ncol = 2)

## @knitr Drivers-leave-1-out-study-Duration:LTC-10km
grid.arrange(drivers_duration_LTC_leave1out_plot + ggtitle('Var-Weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), drivers_duration_ltc_leave1out_10km_plot + ggtitle('10km var-weighted\nDuration:LTC') + theme(plot.title = element_text(size = 12)), ncol = 2)


# Exclusion of Diez et al. 20
## @knitr drivers-var-weighted-intercept-no-diez
drivers_scaled_no_diez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(Reference != 'Diez et al. 2011'), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(total_ltc) + scale(mean_nuts)))
drivers_scaled_no_diez

## @knitr drivers-var-weighted-collinearity-table-no-diez
drivers_cor_table_no_diez <- 
no_event %>% 
  filter(Reference != 'Diez et al. 2011') %>%
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% mutate(mean_invs = mean_invs * 10^-3) %>% select(mean_nuts, mean_invs, sliced_ltc, Duration) %>% cor()

## @knitr drivers-var-weighted-intercept-not-scaled-no-diez
drivers_unscaled_no_diez <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3) %>% filter(Reference != 'Diez et al. 2011'), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (mean_invs + sliced_ltc + mean_nuts))
drivers_unscaled_no_diez

## @knitr drivers-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown-no-diez
model_driver_vector <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration:Invasives', 
    'Duration:LTC', 'Duration:Nutrients')
ordered_driver_vector <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration:Invasives', 
    'Duration:Nutrients', 'Duration:LTC')

driver_summary_plot_short_no_diez  <- 
  mk_rma_summary_df(drivers_scaled_no_diez) %>% 
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
      xlim(c(-7.5, 7.5)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short_no_diez

driver_summary_plot_long_no_diez  <- 
  mk_rma_summary_df(drivers_scaled_no_diez) %>% 
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
      xlim(c(-1.3, 1.3)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long_no_diez

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, driver_summary_plot_short_no_diez, text_b, driver_summary_plot_long_no_diez, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))


#-------------------------------------------------------------------------------
# LMER Leave-1-out analysis
#-------------------------------------------------------------------------------

#dur_lmer_l1o <- 
#leave1out.lmer(model_input_df = filter(no_event, !is.na(yi_SppR_ROM)), leave1out_col = 'Study.ID')

dur_lmer_l1o_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(dur_lmer_l1o, moderator == 'Duration') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
dur_lmer_l1o_plot

#impacts_lmer_l1o <- 
#leave1out.lmer(model_input_df = filter(no_event, !is.na(yi_SppR_ROM)), leave1out_col = 'Study.ID', model = 'lmer(yi_SppR_ROM ~ Duration * mean_imps + (1 | Study.ID), data = leave1out_df)')

impacts_dur_lmer_l1o_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_lmer_l1o, moderator == 'Duration') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
impacts_dur_lmer_l1o_plot 

impacts_imp_lmer_l1o_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_lmer_l1o, moderator == 'mean_imps') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
impacts_imp_lmer_l1o_plot

impacts_imp_dur_lmer_l1o_plot <- 
leave1out_studies_plot(
  single_driver_df = filter(impacts_lmer_l1o, moderator == 'Duration:mean_imps') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
impacts_imp_dur_lmer_l1o_plot

#drivers_lmer_l1o <- 
#leave1out.lmer(model_input_df = filter(no_event, !is.na(yi_SppR_ROM)) %>% mutate(scaled_invs = mean_invs * 10^-3), leave1out_col = 'Study.ID', model = 'lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)) + (1 | Study.ID), data = leave1out_df)')


drivers_dur_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'Duration') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_dur_lmer_l1o

drivers_invs_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'scale(scaled_invs)') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_invs_lmer_l1o

drivers_ltc_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'scale(sliced_ltc)') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_ltc_lmer_l1o

drivers_nuts_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'scale(mean_nuts)') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_nuts_lmer_l1o

drivers_dur_invs_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'Duration:scale(scaled_invs)') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_dur_invs_lmer_l1o

drivers_dur_ltc_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'Duration:scale(sliced_ltc)') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_dur_ltc_lmer_l1o

drivers_dur_nuts_lmer_l1o <- 
leave1out_studies_plot(
  single_driver_df = filter(drivers_lmer_l1o, moderator == 'Duration:scale(mean_nuts)') %>% 
  left_join(., no_event %>% distinct(Study.ID, Reference), by = c('excluded_study' = 'Study.ID'))) + 
  theme(axis.text.y = element_text(size = 7), 
        axis.title.y = element_blank())
drivers_dur_nuts_lmer_l1o