## @knitr taxonomic-data-cleanup
fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga')

# remove Site 'Enfermeria' - should have been classified as having an event. 
no_event <- filter(fl_combined, Event != 'Yes', Site != 'Enfermeria', 
                   Reference != 'Keller et al. 2012') %>% 
            mutate(taxa = replace(taxa, (Study.ID == 561 & taxa == 'Mixed'), 'zooplankton')) %>% 
            mutate(taxa = tolower(taxa))

taxa_levels <- c('plant', 'phytoplankton', 'zooplankton', 'algae', 'coral', 'inverts', 'fish', 'marine mammals', 'mixed')

fixed_taxa <- 
  no_event %>% 
  mutate(Descriptor.of.Taxa.Sampled = trimws(Descriptor.of.Taxa.Sampled)) %>% 
  mutate(Descriptor.of.Taxa.Sampled = replace(Descriptor.of.Taxa.Sampled, is.na(Descriptor.of.Taxa.Sampled), 'none')) %>% 
  mutate(taxa = case_when(.$Descriptor.of.Taxa.Sampled == 'Zooplankton' ~ 'zooplankton', 
                          .$Descriptor.of.Taxa.Sampled == 'all benthic inverts' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Foraminifera' ~ 'foraminifera', 
                          .$Descriptor.of.Taxa.Sampled == 'benthic infaunal invertebrates' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'small jellyfish' ~ 'zooplankton', 
                          .$Descriptor.of.Taxa.Sampled == 'soft-sediment infauna' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'crustaceans' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'All benthic inverts' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'mussels, crabs, polychaetes' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'benthic macrofauna, e.g., worms, molluscs' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'macrobenthic inverts (e.g. polychaetes, molluscs)' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Mostly Copepods, but all large zooplankton' ~ 'zooplankton', 
                          .$Descriptor.of.Taxa.Sampled == 'All types of beach inverts like Coleoptera, Diptera, Gastropoda, Isopoda, Polychaeta' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'bivalves' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Inverts' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Mollusks, polychaetes, crustaceans, echinoderms' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'mussel bed invertebrates' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'All inverts living on deep sea mussel mounds' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Gastropods, slugs and thangs FTW' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Copepods' ~ 'zooplankton', 
                          .$Descriptor.of.Taxa.Sampled == 'All macroscopic inverts' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Macrobenthos inver communities' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'Platyhelminthes' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'soft sediment macrobenthic invertebrates' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'benthic invertebrates' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'sessile invertebrates' ~ 'inverts', 
                          .$Descriptor.of.Taxa.Sampled == 'benthic invertebrate consumers' ~ 'inverts', 
                          TRUE ~ as.character(.$taxa)
                          )) %>% 
  mutate(study_site = paste(.$Study.ID, .$Site, sep = '__')) %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  mutate(taxa = case_when(.$taxa == 'mobile inverts' ~ "inverts",
                          .$taxa == 'sessile inverts' ~ "inverts",
                          .$taxa == 'mixed inverts' ~ "inverts", 
                          TRUE ~ as.character(.$taxa)
                          )) %>% 
  mutate(taxa = factor(taxa, levels = taxa_levels))

## @knitr taxa-counts
var_weighted_taxa_counts <- 
  fixed_taxa %>% 
    filter(!is.na(vi_SppR_ROM)) %>% 
    group_by(taxa) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()

ss_weighted_taxa_counts <- 
  fixed_taxa %>% 
    group_by(taxa) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()

unweighted_taxa_counts <- 
  fixed_taxa %>% 
    group_by(taxa) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()

## @knitr system-counts
var_weighted_system_counts <-
  fixed_taxa %>% 
    filter(!is.na(vi_SppR_ROM)) %>% 
    group_by(Sys) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()

ss_weighted_system_counts <-
  fixed_taxa %>% 
    group_by(Sys) %>% 
    summarise(sites = length(unique(study_site)), 
              studies = length(unique(Study.ID)), 
              min_duration = min(Duration), 
              max_duration = max(Duration)) %>% 
    ungroup()


# Table of possible groupings to analyse the data

## @knitr taxonomic-groups-by-study/site-var-weighted-data
var_weighted_taxa_counts %>% kable(., caption='Taxonomic group counts by study and site for the variance-weighted analysis')

## @knitr taxonomic-groups-by-study/site-ss-weighted-data
ss_weighted_taxa_counts %>% kable(., caption='Taxonomic group counts by study and site for the sample size-weighted analysis')

# Variance-weighted meta-regressions for taxonomic groups
mixed_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mixed_vw
# pval = 0.03, duration = 0.01, n = 19

fish_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
fish_vw
# No change, n = 12

algae_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'algae'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
algae_vw
# pval = 0.003, duration = 0.05, n = 4

zooplankton_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'zooplankton'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
zooplankton_vw

inverts_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'inverts'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
inverts_vw


# Sample size-weighted meta-regressions for taxonomic groups
# No significant differences
mixed_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(taxa == 'mixed', !is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mixed_ss
mixed_ss_robust <- robust(mixed_ss, cluster=1:mixed_ss$k)
mixed_ss_robust
# No change, n = 53

fish_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(taxa == 'fish', !is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
fish_ss
fish_ss_robust <- robust(fish_ss, cluster=1:fish_ss$k)
fish_ss_robust
# No change, n = 39

algae_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(taxa == 'algae', !is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
algae_ss
algae_ss_robust <- robust(algae_ss, cluster=1:algae_ss$k)
algae_ss_robust
# No change, n = 7

inverts_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = fixed_taxa %>% filter(taxa == 'inverts', !is.na(yi_SppR_ROM)), 
         #random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
inverts_ss
inverts_ss_robust <- robust(inverts_ss, cluster=1:inverts_ss$k)
inverts_ss_robust
# p < 0.0001, duration = 0.11, n = 3

coral_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(taxa == 'coral', !is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
coral_ss
coral_ss_robust <- robust(coral_ss, cluster=1:coral_ss$k)
coral_ss_robust
# p < 0.0001, duration - 0.037, n = 6

zooplankton_ss <- 
  rma.mv(yi = yi_SppR_ROM, V = 1 / n1, 
         data = no_event %>% filter(taxa == 'zooplankton'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
zooplankton_ss
zooplankton_ss_robust <- robust(zooplankton_ss, cluster=1:zooplankton_ss$k)
zooplankton_ss_robust
# No change, n = 5


#-------------------------------------------------------------------------------
# UNWEIGHTED meta-regressions for taxonomic groups
# No significant differences
#-------------------------------------------------------------------------------
mixed_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'mixed', !is.na(yi_SppR_ROM)))
summary(mixed_uw)
# No change, n = 52

fish_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'fish', !is.na(yi_SppR_ROM)))
summary(fish_uw)
# No change, n = 38

algae_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'algae', !is.na(yi_SppR_ROM)))
summary(algae_uw)
# No change, n = 7

inverts_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'inverts', !is.na(yi_SppR_ROM)))
summary(inverts_uw)
# p = 0.0283, duration = 0.15, n = 3

coral_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'coral', !is.na(yi_SppR_ROM)))
summary(coral_uw)
# 

zooplankton_uw <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'zooplankton', !is.na(yi_SppR_ROM)))
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
      mutate(taxa = 'algae', analysis = 'vw', k = algae_vw$k, 
             n = algae_vw$s.nlevels), 
    mk_rma_summary_df(zooplankton_vw) %>% 
      mutate(taxa = 'zooplankton', analysis = 'vw', k = zooplankton_vw$k, 
             n = zooplankton_vw$s.nlevels), 
    mk_rma_summary_df(inverts_vw) %>% 
      mutate(taxa = 'inverts', analysis = 'vw', k = inverts_vw$k, 
             n = inverts_vw$s.nlevels), 
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
  mutate(weighting = c(rep('variance weighted', 5), rep('sample size weighted', 6))) %>% 
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
    mk_rma_summary_df(zooplankton_vw) %>% 
      mutate(taxa = 'zooplankton', analysis = 'vw', k = zooplankton_vw$k, 
             n = zooplankton_vw$s.nlevels), 
    mk_rma_summary_df(inverts_vw) %>% 
      mutate(taxa = 'inverts', analysis = 'vw', k = inverts_vw$k, 
             n = inverts_vw$s.nlevels),
    mutate(mixed_uw_df, analysis = 'uw'), 
    mutate(fish_uw_df, analysis = 'uw'), 
    mutate(algae_uw_df, analysis = 'uw'), 
    mutate(inverts_uw_df, analysis = 'uw'), 
    mutate(coral_uw_df, analysis = 'uw'),
    mutate(zooplankton_uw_df, analysis = 'uw')) %>% 
  filter(!(driver %in% c('intrcpt', '(Intercept)'))) %>% 
  mutate(analysis = factor(analysis, levels = c('vw', 'uw')), 
         `percent change` = get_percent_change(estimate)) %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'coral', 'inverts', 'zooplankton', 'fish')))
#
taxa_results <- 
bind_rows(
  left_join(filter(taxa_results, analysis == 'vw'), var_weighted_taxa_counts), 
  left_join(filter(taxa_results, analysis == 'uw'), unweighted_taxa_counts)) %>% 
  select(-sites, -studies) %>% 
  rename(sites = k, studies = n)

## @knitr taxa-vw-results-table
taxa_results_vw <- 
  left_join(filter(taxa_results, analysis == 'vw'), var_weighted_taxa_counts)



## @knitr rate-of-change-by-taxa-vw-uw-plot
taxa_results %>% 
  mutate(weighting = c(rep('variance weighted', 5), rep('unweighted', 6))) %>% 
  mutate(weighting = factor(weighting, levels = c('variance weighted', 'unweighted'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_bw() +
  xlab('Taxa') + 
  ylab('Duration coefficient estimate') + 
  ylim(c(-0.23, 0.23)) + 
  facet_wrap(~ weighting, scales = 'free_x')


## @knitr rate-of-change-by-taxa-vw-plot
taxa_results %>% 
  mutate(weighting = c(rep('variance weighted', 5), rep('unweighted', 6))) %>% 
  filter(weighting == 'variance weighted') %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'coral', 'inverts', 'zooplankton', 'fish'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_bw() +
  xlab('Taxa') + 
  ylab('Duration coefficient estimate') + 
  ylim(c(-0.13, 0.13))


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

## @knitr taxa-inverts-vw-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
  filter(taxa == 'inverts') %>% 
  mutate(se = 2 * sqrt(vi_SppR_ROM)) %>%
  mk_cater_plot(., title = 'inverts', error_bar_se = .$se, weight = 1 / .$vi_SppR_ROM) %>% 
  draw_cater_plot(.)

## @knitr taxa-zooplankton-vw-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
  filter(taxa == 'zooplankton') %>% 
  mutate(se = 2 * sqrt(vi_SppR_ROM)) %>%
  mk_cater_plot(., title = 'zooplankton', error_bar_se = .$se, weight = 1 / .$vi_SppR_ROM) %>% 
  draw_cater_plot(.)



# Raw data for the uw weighted analysis broken down by taxonomic group
## @knitr taxa-mixed-uw-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'mixed') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'mixed', error_bar_se = .$se, weight = .$n1, error_bar_alpha = 0) %>% 
  draw_cater_plot(.)

## @knitr taxa-algae-uw-plot
algae_uw_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'algae') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'algae', error_bar_se = .$se, weight = .$n1, error_bar_alpha = 0) %>% 
  draw_cater_plot(.)
algae_uw_cat_plot

## @knitr taxa-inverts-uw-plot
inverts_uw_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'inverts') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'inverts', error_bar_se = .$se, weight = .$n1, error_bar_alpha = 0) %>% 
  draw_cater_plot(.)
inverts_uw_cat_plot

## @knitr taxa-coral-uw-plot
coral_uw_cat_plot <- 
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'coral') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'coral', error_bar_se = .$se, weight = .$n1, error_bar_alpha = 0) %>% 
  draw_cater_plot(.)
coral_uw_cat_plot

## @knitr taxa-fish-uw-plot
fixed_taxa %>% 
  filter(!is.na(yi_SppR_ROM)) %>% 
  filter(taxa == 'fish') %>% 
  mutate(se = 2 * sqrt(1 / n1)) %>%
  mk_cater_plot(., title = 'fish', error_bar_se = .$se, weight = .$n1, error_bar_alpha = 0) %>% 
  draw_cater_plot(.)



### UNWEIGHTED - VAR WEIGHTED SUBSET
mixed_uw_vws <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'mixed', !is.na(yi_SppR_ROM)) %>% 
              filter(!is.na(vi_SppR_ROM)))
summary(mixed_uw_vws)
# No change, n = 52

fish_uw_vws <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'fish', !is.na(yi_SppR_ROM)) %>% 
              filter(!is.na(vi_SppR_ROM)))
summary(fish_uw_vws)
# No change, n = 38

algae_uw_vws <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'algae', !is.na(yi_SppR_ROM)) %>% 
              filter(!is.na(vi_SppR_ROM)))
summary(algae_uw_vws)
# No change, n = 7

inverts_uw_vws <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'inverts', !is.na(yi_SppR_ROM)) %>% 
              filter(!is.na(vi_SppR_ROM)))
summary(inverts_uw_vws)
# No change, n = 7

zooplankton_uw_vws <- 
  lmer(yi_SppR_ROM ~ Duration  + (1 | Study.ID), 
       data = fixed_taxa %>% filter(taxa == 'zooplankton', !is.na(yi_SppR_ROM)) %>% 
              filter(!is.na(vi_SppR_ROM)))
summary(zooplankton_uw_vws)
# No change, n = 7


mixed_uw_df_vws <- 
  mk_lme_summary_df_nk(mixed_uw_vws, confint_method='Wald') %>% 
    mutate(taxa = 'mixed', analysis = 'uw')
fish_uw_df_vws <- 
  mk_lme_summary_df_nk(fish_uw_vws, confint_method = 'Wald') %>% 
    mutate(taxa = 'fish', analysis = 'uw')
algae_uw_df_vws <- 
  mk_lme_summary_df_nk(algae_uw_vws, confint_method = 'Wald') %>% 
    mutate(taxa = 'algae', analysis = 'uw')
inverts_uw_df_vws <- 
  mk_lme_summary_df_nk(inverts_uw_vws, confint_method = 'Wald') %>% 
    mutate(taxa = 'inverts', analysis = 'uw')
zooplankton_uw_df_vws <- 
  mk_lme_summary_df_nk(zooplankton_uw_vws, confint_method = 'Wald') %>% 
    mutate(taxa = 'zooplankton', analysis = 'uw')



## @knitr taxa-vw-uw-results-table-vw-subset
taxa_results_vws <- 
  bind_rows(
    mutate(mixed_uw_df_vws, analysis = 'uw'), 
    mutate(fish_uw_df_vws, analysis = 'uw'), 
    mutate(algae_uw_df_vws, analysis = 'uw'), 
    mutate(inverts_uw_df_vws, analysis = 'uw'), 
    mutate(zooplankton_uw_df_vws, analysis = 'uw')) %>%
  filter(!(driver %in% c('intrcpt', '(Intercept)'))) %>% 
  mutate(`percent change` = get_percent_change(estimate))

## @knitr rate-of-change-by-taxa-vw-uw-plot-vw-subset
taxa_results_vws %>% 
  #mutate(weighting = c(rep('unweighted', 5))) %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'fish', 'inverts', 'zooplankton'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_bw() +
  xlab('Taxa') + 
  ylab('Duration coefficient estimate') + 
  ylim(c(-0.15, 0.15)) #+ 
  #facet_wrap(~ weighting, scales = 'free_x')



## @knitr distribution-of-variance-across-taxa-plot
fixed_taxa %>%
  filter(!is.na(vi_SppR_ROM)) %>% 
ggplot(data = ., aes(x = vi_SppR_ROM)) + 
  geom_histogram() + 
  theme_bw() +
  guides(colour = FALSE) +
  xlab('variance') +
  facet_wrap(~ taxa)





imps_taxa_summary <- 
fixed_taxa %>% 
  filter(!is.na(vi_SppR_ROM), yi_SppR_ROM > 0, !is.na(mean_imps)) %>% 
  group_by(taxa) %>% 
  summarise(sites = length(unique(study_site)), 
            studies = length(unique(Study.ID)), 
            min_duration = min(Duration), 
            max_duration = max(Duration))

impacts_algae <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'algae'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impacts_algae

impacts_inverts <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'inverts'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impacts_inverts

impacts_fish <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impacts_fish

impacts_mixed <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impacts_mixed

# Too many parameters
#drivers_algae <- 
#  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
#         data = fixed_taxa %>% filter(taxa == 'algae'), 
#         random = ~ 1 | as.factor(Study.ID), 
#         mods = ~ Duration * (mean_nuts + mean_invs + sliced_ltc))

drivers_fish <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * (mean_nuts + mean_invs + sliced_ltc))
drivers_fish


## @knitr taxa-vw-impacts-results-table
taxa_results_imps <- 
  bind_rows(
    mk_rma_summary_df(impacts_algae) %>% 
      mutate(taxa = 'algae', analysis = 'vw', k = impacts_algae$k, 
             n = impacts_algae$s.nlevels), 
    mk_rma_summary_df(impacts_inverts) %>% 
      mutate(taxa = 'inverts', analysis = 'vw', k = impacts_inverts$k, 
             n = impacts_inverts$s.nlevels), 
    mk_rma_summary_df(impacts_fish) %>% 
      mutate(taxa = 'fish', analysis = 'vw', k = impacts_fish$k, 
             n = impacts_fish$s.nlevels),
    mk_rma_summary_df(impacts_mixed) %>% 
      mutate(taxa = 'mixed', analysis = 'vw', k = impacts_mixed$k, 
             n = impacts_mixed$s.nlevels)) %>% 
  filter(driver != 'intrcpt') %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'fish', 'inverts')), 
         `percent change` = get_percent_change(estimate)) %>% 
  rename(sites = k, studies = n) %>% 
  left_join(., select(imps_taxa_summary, taxa, max_duration), by = 'taxa') 

## @knitr taxa-vw-impacts-results-plot
taxa_results_imps %>% 
  mutate(driver = gsub('Duration', 'Duration', .$driver)) %>% 
  mutate(driver = gsub('mean_imps', 'CHI', .$driver)) %>% 
  mutate(driver = gsub('Duration:mean_imps', 'Duration:CHI', .$driver)) %>% 
  rename(parameter = driver) %>%
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'coral', 'inverts', 'zooplankton', 'fish')), 
         driver = factor(parameter, levels = c('CHI', 'Duration', 'Duration:CHI'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_bw() +
  xlab('Taxa') + 
  ylab('Duration coefficient estimate') + 
  #ylim(c(-0.23, 0.23)) + 
  facet_wrap(~ parameter, scales = 'free_x')


# Driver specific runs

mixed_vw_nuts <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_nuts)
mixed_vw_nuts

mixed_vw_invs <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_invs)
mixed_vw_invs

mixed_vw_ltc <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_ltc)
mixed_vw_ltc

fish_vw_nuts <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * nuts)
fish_vw_nuts

fish_vw_invs <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration * mean_nuts)
fish_vw_invs

fish_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
fish_vw


algae_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'algae'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
algae_vw
# pval = 0.003, duration = 0.05, n = 4

zooplankton_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'zooplankton'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
zooplankton_vw

inverts_vw <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'inverts'), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
inverts_vw