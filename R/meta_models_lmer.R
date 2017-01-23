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

## @knitr duration-lmer
dur_lmer <- 
  lmer(yi_SppR_ROM ~ Duration + (1 | Study.ID), 
       data = no_event %>% filter(!is.na(yi_SppR_ROM)))
summary(dur_lmer)

## @knitr impacts-lmer
impact_lmer <- 
  lmer(yi_SppR_ROM ~ Duration * mean_imps + (1 | Study.ID), 
       data = no_event %>% filter(!is.na(yi_SppR_ROM)))
summary(impact_lmer)


## @knitr drivers-scaled-lmer
drivers_scaled_lmer <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + 
      scale(mean_nuts)) + (1 | Study.ID), 
      data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(drivers_scaled_lmer)


## @knitr drivers-unscaled-lmer
drivers_unscaled_lmer <- 
  lmer(yi_SppR_ROM ~ Duration * (scaled_invs + sliced_ltc + 
      mean_nuts) + (1 | Study.ID), 
      data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(drivers_unscaled_lmer)

## @knitr drivers-interactions-scaled-lmer
interactions_scaled_lmer <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc) * scale(mean_nuts)) + (1 | Study.ID), 
      data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(interactions_scaled_lmer)

## @knitr drivers-interactions-unscaled-lmer
interactions_unscaled_lmer <- 
  lmer(yi_SppR_ROM ~ Duration * ((scaled_invs * sliced_ltc) + (sliced_ltc * mean_nuts)) + (1 | Study.ID), 
      data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(interactions_unscaled_lmer)


## @knitr duration-unweighted-table
dur_lmer_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(coefficients(summary(dur_lmer))), 
             estimate = round(coefficients(summary(dur_lmer))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(dur_lmer))[, 1]), digits = 2), 
             se = round(coefficients(summary(dur_lmer))[, 2], digits = 3), 
             pval = round(coefficients(summary(dur_lmer))[, 5], digits = 3), 
             ci.lb = round(confint(dur_lmer)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(dur_lmer)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(dur_lmer@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(dur_lmer@frame$Study.ID)))
#dur_lmer_df

## @knitr impacts-unweighted-table
impacts_lmer_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(coefficients(summary(impact_lmer))), 
             estimate = round(coefficients(summary(impact_lmer))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(impact_lmer))[, 1]), digits = 2), 
             se = round(coefficients(summary(impact_lmer))[, 2], digits = 3), 
             pval = round(coefficients(summary(impact_lmer))[, 5], digits = 3), 
             ci.lb = round(confint(impact_lmer)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(impact_lmer)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(impact_lmer@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(impact_lmer@frame$Study.ID))) %>% 
  mutate(parameter = gsub('scale\\(|\\)', replacement = '', parameter)) %>%
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))
#impacts_lmer_df

## @knitr drivers-unweighted-table
drivers_lmer_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = rownames(coefficients(summary(drivers_scaled_lmer))), 
             estimate = round(coefficients(summary(drivers_scaled_lmer))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(drivers_unscaled_lmer))[, 1]), digits = 2), 
             se = round(coefficients(summary(drivers_scaled_lmer))[, 2], digits = 3), 
             pval = round(coefficients(summary(drivers_scaled_lmer))[, 5], digits = 3), 
             ci.lb = round(confint(drivers_scaled_lmer)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(drivers_scaled_lmer)[-(1:2), 1], digits = 3), 
             k = as.numeric((length(drivers_scaled_lmer@frame$yi_SppR_ROM))), 
             n = as.numeric(as.numeric(nlevels(drivers_scaled_lmer@frame$Study.ID)))) %>%
  mutate(parameter = gsub('scale\\(|\\)', replacement = '', parameter)) %>%
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))
#drivers_lmer_df

## @knitr drivers-interactions-unweighted-table
interactions_lmer_df <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = rownames(coefficients(summary(interactions_scaled_lmer))), 
             estimate = round(coefficients(summary(interactions_scaled_lmer))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(interactions_unscaled_lmer))[, 1]), digits = 2), 
             se = round(coefficients(summary(interactions_scaled_lmer))[, 2], digits = 3), 
             pval = round(coefficients(summary(interactions_scaled_lmer))[, 5], digits = 3), 
             ci.lb = round(confint(interactions_scaled_lmer)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(interactions_scaled_lmer)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(interactions_scaled_lmer@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(interactions_scaled_lmer@frame$Study.ID))) %>%
  mutate(parameter = gsub('scale\\(|\\)', replacement = '', parameter)) %>%
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))
#interactions_lmer_df




### VARIANCE WEIGHTED SUBSET - FOR UNWEIGHTED ANALYSIS

vw_subset <- filter(fl_combined, Event != 'Yes') %>% 
            filter(!is.na(vi_SppR_ROM))

## @knitr duration-lmer-vw-subset
dur_lmer_vws <- 
  lmer(yi_SppR_ROM ~ Duration + (1 | Study.ID), 
       data = vw_subset %>% filter(!is.na(yi_SppR_ROM)))
summary(dur_lmer_vws)

## @knitr impacts-lmer-vw-subset
impact_lmer_vws <- 
  lmer(yi_SppR_ROM ~ Duration * mean_imps + (1 | Study.ID), 
       data = vw_subset %>% filter(!is.na(yi_SppR_ROM)))
summary(impact_lmer_vws)


## @knitr drivers-scaled-lmer-vw-subset
drivers_scaled_lmer_vws <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + 
      scale(mean_nuts)) + (1 | Study.ID), 
      data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(drivers_scaled_lmer_vws)


## @knitr drivers-unscaled-lmer-vw-subset
drivers_unscaled_lmer_vws <- 
  lmer(yi_SppR_ROM ~ Duration * (scaled_invs + sliced_ltc + 
      mean_nuts) + (1 | Study.ID), 
      data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(drivers_unscaled_lmer_vws)

## @knitr drivers-interactions-scaled-lmer-vw-subset
interactions_scaled_lmer_vws <- 
  lmer(yi_SppR_ROM ~ Duration * (scale(scaled_invs) * scale(sliced_ltc) + scale(sliced_ltc) * scale(mean_nuts)) + (1 | Study.ID), 
      data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(interactions_scaled_lmer_vws)


## @knitr drivers-interactions-unscaled-lmer-vw-subset
interactions_unscaled_lmer_vws <- 
  lmer(yi_SppR_ROM ~ Duration * ((scaled_invs * sliced_ltc) + (sliced_ltc * mean_nuts)) + (1 | Study.ID), 
    data = vw_subset %>% mutate(scaled_invs = mean_invs * 10^-3))
summary(interactions_unscaled_lmer_vws)


## @knitr duration-unweighted-table-vw-subset
dur_lmer_df_vws <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(coefficients(summary(dur_lmer_vws))), 
             estimate = round(coefficients(summary(dur_lmer_vws))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(dur_lmer_vws))[, 1]), digits = 2), 
             se = round(coefficients(summary(dur_lmer_vws))[, 2], digits = 3), 
             pval = round(coefficients(summary(dur_lmer_vws))[, 5], digits = 3), 
             ci.lb = round(confint(dur_lmer_vws)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(dur_lmer_vws)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(dur_lmer_vws@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(dur_lmer_vws@frame$Study.ID)))
#dur_lmer_df_vws

## @knitr impacts-unweighted-table-vw-subset
impacts_lmer_df_vws <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(coefficients(summary(impact_lmer_vws))), 
             estimate = round(coefficients(summary(impact_lmer_vws))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(impact_lmer_vws))[, 1]), digits = 2), 
             se = round(coefficients(summary(impact_lmer_vws))[, 2], digits = 3), 
             pval = round(coefficients(summary(impact_lmer_vws))[, 5], digits = 3), 
             ci.lb = round(confint(impact_lmer_vws)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(impact_lmer_vws)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(impact_lmer_vws@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(impact_lmer_vws@frame$Study.ID))) %>% 
  mutate(parameter = gsub('scale\\(|\\)', replacement = '', parameter)) %>%
  mutate(parameter = gsub('mean_imps', 'Impact', parameter))
#impacts_lmer_df_vws

## @knitr drivers-unweighted-table-vw-subset
drivers_lmer_df_vws <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = rownames(coefficients(summary(drivers_scaled_lmer_vws))), 
             estimate = round(coefficients(summary(drivers_scaled_lmer_vws))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(drivers_unscaled_lmer))[, 1]), digits = 2), 
             se = round(coefficients(summary(drivers_scaled_lmer_vws))[, 2], digits = 3), 
             pval = round(coefficients(summary(drivers_scaled_lmer_vws))[, 5], digits = 3), 
             ci.lb = round(confint(drivers_scaled_lmer_vws)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(drivers_scaled_lmer_vws)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(drivers_scaled_lmer_vws@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(drivers_scaled_lmer_vws@frame$Study.ID))) %>%
  mutate(parameter = gsub('scale\\(|\\)', replacement = '', parameter)) %>%
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))
#drivers_lmer_df_vws

## @knitr drivers-interactions-unweighted-table-vw-subset
interactions_lmer_df_vws <- 
  data_frame(model = 'LR ~ D * (I * LTC + N * LTC)', 
             parameter = rownames(coefficients(summary(interactions_scaled_lmer_vws))), 
             estimate = round(coefficients(summary(interactions_scaled_lmer_vws))[, 1], digits = 3),
             `% change` = round(get_percent_change(coefficients(summary(interactions_unscaled_lmer))[, 1]), digits = 2), 
             se = round(coefficients(summary(interactions_scaled_lmer_vws))[, 2], digits = 3), 
             pval = round(coefficients(summary(interactions_scaled_lmer_vws))[, 5], digits = 3), 
             ci.lb = round(confint(interactions_scaled_lmer_vws)[-(1:2), 1], digits = 3), 
             ci.ub = round(confint(interactions_scaled_lmer_vws)[-(1:2), 1], digits = 3), 
             k = as.numeric(length(interactions_scaled_lmer_vws@frame$yi_SppR_ROM)), 
             n = as.numeric(nlevels(interactions_scaled_lmer_vws@frame$Study.ID))) %>%
  mutate(parameter = gsub('scale\\(|\\)', replacement = '', parameter)) %>%
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter))
#interactions_lmer_df_vws


## @knitr unweighted-diffs-based-on-mod-weighting-comparison
uw_diffs_df <- 
  data_frame(model = c('LR ~ D', 'LR ~ D * Imp', 'LR ~ D * (I + N + LTC)'), 
           `var-w sites` = c(mod1$k, impact_ne_w$k, drivers_scaled$k), 
           `var-w studies` = c(mod1$s.nlevels, impact_ne_w$s.nlevels, 
                               drivers_scaled$s.nlevels), 
           `uw sites` = c(as.numeric(length(dur_lmer@frame$yi_SppR_ROM)), 
                          as.numeric(length(impact_lmer@frame$yi_SppR_ROM)), 
                          as.numeric(length(drivers_scaled_lmer@frame$yi_SppR_ROM))), 
           `uw studies` = c(as.numeric(nlevels(dur_lmer@frame$Study.ID)), 
                            as.numeric(nlevels(impact_lmer@frame$Study.ID)), 
                            as.numeric(nlevels(drivers_scaled_lmer@frame$Study.ID)))
           )