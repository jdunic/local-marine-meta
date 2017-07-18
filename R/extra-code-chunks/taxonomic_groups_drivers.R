#source('01_taxonomic_groups.R')

## @knitr taxa-counts-drivers
vw_taxa_counts_drivers <- 
  fixed_taxa %>% 
    filter(!is.na(vi_SppR_ROM)) %>% 
    filter(!is.na(mean_nuts), !is.na(mean_invs), !(is.na(sliced_ltc))) %>% 
    group_by(taxa) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()

uw_taxa_counts_drivers <- 
  fixed_taxa %>% 
    group_by(taxa) %>% 
    filter(!is.na(yi_SppR_ROM)) %>% 
    filter(!is.na(mean_nuts), !is.na(mean_invs), !(is.na(sliced_ltc))) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()

# Variance-weighted meta-regressions for taxonomic groups
mixed_vw_drivers_scaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(taxa == 'mixed') %>% 
         mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
mixed_vw_drivers_scaled
profile(mixed_vw_drivers_scaled)

algae_vw_drivers_scaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(taxa == 'algae') %>% 
         mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
algae_vw_drivers_scaled
profile(algae_vw_drivers_scaled)

fish_vw_drivers_scaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(taxa == 'fish') %>% 
         mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
fish_vw_drivers_scaled
profile(fish_vw_drivers_scaled)

# Variance-weighted UNSCALED
mixed_vw_drivers_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(taxa == 'mixed') %>% 
         mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts))
mixed_vw_drivers_unscaled

algae_vw_drivers_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(taxa == 'algae') %>% 
                mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts))
algae_vw_drivers_unscaled

fish_vw_drivers_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(taxa == 'fish') %>% 
         mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts))
fish_vw_drivers_unscaled
profile(fish_vw_drivers_unscaled)

#-------------------------------------------------------------------------------
# UNWEIGHTED meta-regressions for taxonomic groups
# No significant differences
#-------------------------------------------------------------------------------
mixed_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = no_event %>% filter(taxa == 'mixed', !is.na(yi_SppR_ROM)))
summary(mixed_uw)
# No change, n = 52

fish_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = no_event %>% filter(taxa == 'fish', !is.na(yi_SppR_ROM)))
summary(fish_uw)
# No change, n = 38

algae_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = no_event %>% filter(taxa == 'algae', !is.na(yi_SppR_ROM)))
summary(algae_uw)
# No change, n = 7

inverts_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'inverts', !is.na(yi_SppR_ROM)))
summary(inverts_uw)
# p = 0.0283, duration = 0.15, n = 3

coral_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = no_event %>% filter(taxa == 'coral', !is.na(yi_SppR_ROM)))
summary(coral_uw)
# 

zooplankton_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = no_event %>% filter(taxa == 'zooplankton', !is.na(yi_SppR_ROM)))
summary(zooplankton_uw)
# No change, n = 5

mixed_uw_df <- 
  mk_lme_summary_df_nk(mixed_uw, confint_method='Wald') %>% 
    mutate(taxa = 'mixed', analysis = 'uw')
fish_uw_df <- 
  mk_lme_summary_df_nk(fish_uw, confint_method = 'Wald') %>% 
    mutate(taxa = 'fish', analysis = 'uw')
algae_uw_df <- 
  mk_lme_summary_df_nk(algae_uw) %>% 
    mutate(taxa = 'algae', analysis = 'uw')
inverts_uw_df <- 
  mk_lme_summary_df_nk(inverts_uw) %>% 
    mutate(taxa = 'inverts', analysis = 'uw')
coral_uw_df <- 
  mk_lme_summary_df_nk(coral_uw) %>% 
    mutate(taxa = 'coral', analysis = 'uw')
zooplankton_uw_df <- 
  mk_lme_summary_df_nk(zooplankton_uw) %>% 
    mutate(taxa = 'zooplankton', analysis = 'uw')

## @knitr taxa-vw-ss-results-table
taxa_results <- 
  bind_rows(
    mk_rma_summary_df(mixed_vw) %>% 
      mutate(taxa = 'mixed', analysis = 'vw', k = mixed_vw$k, 
             n = mixed_vw$s.nlevels), 
    mk_rma_summary_df(fish_vw) %>% 
      mutate(taxa = 'fish', analysis = 'vw', k = fish_vw$k, 
             n = fish_vw$s.nlevels), 
    mk_rma_summary_df(algae_vw) %>% 
      mutate(taxa = 'algae', analysis = 'vw', k = fish_vw$k, 
             n = algae_vw$s.nlevels), 
    mk_rma_summary_df(mixed_ss_robust) %>% 
      mutate(taxa = 'mixed', analysis = 'ss', k = mixed_ss$k, 
             n = mixed_ss$s.nlevels), 
    mk_rma_summary_df(fish_ss_robust) %>% 
      mutate(taxa = 'fish', analysis = 'ss', k = fish_ss$k, 
             n = fish_ss$s.nlevels), 
    mk_rma_summary_df(algae_ss_robust) %>% 
      mutate(taxa = 'algae', analysis = 'ss', k = algae_ss$k, 
             n = algae_ss$s.nlevels), 
    mk_rma_summary_df(inverts_ss_robust) %>% 
      mutate(taxa = 'inverts', analysis = 'ss', k = inverts_ss$k, 
             n = 3), 
    mk_rma_summary_df(coral_ss_robust) %>% 
      mutate(taxa = 'coral', analysis = 'ss', k = coral_ss$k, 
             n = coral_ss$s.nlevels), 
    mk_rma_summary_df(zooplankton_ss_robust) %>% 
      mutate(taxa = 'zooplankton', analysis = 'ss', k = zooplankton_ss$k, 
             n = zooplankton_ss$s.nlevels)
  ) %>% 
  filter(driver != 'intrcpt') %>% 
  mutate(analysis = factor(analysis, levels = c('vw', 'ss')), 
         `percent change` = get_percent_change(estimate)) %>% 
  select(-driver)

taxa_results <- bind_rows(left_join(filter(taxa_results, analysis == 'vw'), 
  var_weighted_taxa_counts), left_join(filter(taxa_results, analysis == 'ss'), 
  ss_weighted_taxa_counts))

## @knitr rate-of-change-by-taxa-vw-ss-plot
taxa_results %>% 
  mutate(weighting = c(rep('variance weighted', 3), rep('sample size weighted', 6))) %>% 
  mutate(weighting = factor(weighting, levels = c('variance weighted', 'sample size weighted'))) %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'coral', 'inverts', 'zooplankton', 'fish'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_bw() +
  xlab('Taxa') + 
  ylab('Duration coefficient estimate') + 
  ylim(c(-0.15, 0.15)) + 
  facet_wrap(~ weighting, scales = 'free_x')

#-------------------------------------------------------------------------------
#### UNWEIGHTED + VAR WEIGHTED
#-------------------------------------------------------------------------------

## @knitr taxa-vw-uw-results-table
taxa_results <- 
  bind_rows(
    mk_rma_summary_df(mixed_vw) %>% 
      mutate(taxa = 'mixed', analysis = 'vw', k = mixed_vw$k, 
             n = mixed_vw$s.nlevels), 
    mk_rma_summary_df(fish_vw) %>% 
      mutate(taxa = 'fish', analysis = 'vw', k = fish_vw$k, 
             n = fish_vw$s.nlevels), 
    mk_rma_summary_df(algae_vw) %>% 
      mutate(taxa = 'algae', analysis = 'vw', k = algae_vw$k, 
             n = algae_vw$s.nlevels),
    mutate(mixed_uw_df, analysis = 'uw'), 
    mutate(fish_uw_df, analysis = 'uw'), 
    mutate(algae_uw_df, analysis = 'uw'), 
    mutate(inverts_uw_df, analysis = 'uw'), 
    mutate(coral_uw_df, analysis = 'uw'),
    mutate(zooplankton_uw_df, analysis = 'uw')) %>% 
  filter(!(driver %in% c('intrcpt', '(Intercept)'))) %>% 
  mutate(analysis = factor(analysis, levels = c('vw', 'uw')), 
         `percent change` = get_percent_change(estimate))
#
taxa_results <- 
bind_rows(
  left_join(filter(taxa_results, analysis == 'vw'), var_weighted_taxa_counts), 
  left_join(filter(taxa_results, analysis == 'uw'), unweighted_taxa_counts)) 

## @knitr rate-of-change-by-taxa-vw-uw-plot
taxa_results %>% 
  mutate(weighting = c(rep('variance weighted', 3), rep('unweighted', 6))) %>% 
  mutate(weighting = factor(weighting, levels = c('variance weighted', 'unweighted'))) %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'coral', 'inverts', 'zooplankton', 'fish'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_bw() +
  xlab('Taxa') + 
  ylab('Duration coefficient estimate') + 
  ylim(c(-0.23, 0.23)) + 
  facet_wrap(~ weighting, scales = 'free_x')


# Raw data for the variance weighted analysis broken down by taxonomic group
## @knitr taxa-mixed-vw-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
  filter(taxa == 'mixed') %>% 
  mutate(se = 2 * sqrt(vi_SppR_ROM)) %>%
  mk_cater_plot(., title = 'mixed', error_bar_se = .$se, weight = 1 / .$vi_SppR_ROM) %>% 
  draw_cater_plot(.)

## @knitr taxa-algae-vw-plot
algae_vw_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
  filter(taxa == 'algae') %>% 
  mutate(se = 2 * sqrt(vi_SppR_ROM)) %>%
  mk_cater_plot(., title = 'algae', error_bar_se = .$se, weight = 1 / .$vi_SppR_ROM) %>% 
  draw_cater_plot(.)
algae_vw_cat_plot

## @knitr taxa-fish-vw-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
  filter(taxa == 'fish') %>% 
  mutate(se = 2 * sqrt(vi_SppR_ROM)) %>%
  mk_cater_plot(., title = 'fish', error_bar_se = .$se, weight = 1 / .$vi_SppR_ROM) %>% 
  draw_cater_plot(.)

# Raw data for the ss weighted analysis broken down by taxonomic group
## @knitr taxa-mixed-ss-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'mixed') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'mixed', error_bar_se = .$se, weight = .$n1) %>% 
  draw_cater_plot(.)

## @knitr taxa-algae-ss-plot
algae_ss_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'algae') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'algae', error_bar_se = .$se, weight = .$n1) %>% 
  draw_cater_plot(.)
algae_ss_cat_plot

## @knitr taxa-inverts-ss-plot
inverts_ss_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'inverts') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'inverts', error_bar_se = .$se, weight = .$n1) %>% 
  draw_cater_plot(.)
inverts_ss_cat_plot

## @knitr taxa-coral-ss-plot
coral_ss_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'coral') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'coral', error_bar_se = .$se, weight = .$n1) %>% 
  draw_cater_plot(.)
coral_ss_cat_plot

## @knitr taxa-fish-ss-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'fish') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'fish', error_bar_se = .$se, weight = .$n1) %>% 
  draw_cater_plot(.)

