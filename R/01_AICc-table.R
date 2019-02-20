## @knitr models-for-AICc
mod1_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM), !is.na(mean_imps), !is.na(mean_invs), !is.na(sliced_ltc), !is.na(mean_nuts)), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration)
mod1_aic

impact_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM), !is.na(mean_imps), !is.na(mean_invs), !is.na(sliced_ltc), !is.na(mean_nuts)),
         random = ~ 1 | factor(Study.ID), method = 'ML', 
         mods = ~ Duration * mean_imps)
impact_aic

drivers_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM), !is.na(mean_imps), !is.na(mean_invs), !is.na(sliced_ltc), !is.na(mean_nuts)) %>% mutate(scaled_invs = mean_invs * 10^-3),
         random = ~ 1 | Study.ID, method = 'ML', 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_aic

drivers_with_chi_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM), !is.na(mean_imps), !is.na(mean_invs), !is.na(sliced_ltc), !is.na(mean_nuts)) %>% mutate(scaled_invs = mean_invs * 10^-3),
         random = ~ 1 | Study.ID, method = 'ML', 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts) + mean_imps))
drivers_with_chi_aic

## @knitr AICc-table
aic_tab <- 
  fitstats(mod1_aic, impact_aic, drivers_aic) %>% 
  mutate(metrics = rownames(.)) %>% 
  gather(key = Model, value = values, 1:3) %>% 
  spread(key = metrics, value = values) %>% 
  mutate(K = c(7, 3, 1)) %>%
  mutate(min_AICc = min(`AICc:`), `Delta AICc` = `AICc:` - min_AICc, 
         sum_AICc_weight = sum(exp(-0.5 * `Delta AICc`)), 
         `Akaike weight` = exp(-0.5 * `Delta AICc` / sum_AICc_weight)) %>% 
  mutate(`Akaike weight` = round(`Akaike weight`, digits = 3)) %>% 
  mutate(Model = c('LR ~ Dur * (Inv + Nut + LTC)', 'LR ~ Dur + CHI + Dur*CHI', 'LR ~ Dur')) %>% 
  select(Model, `logLik:`, K, `deviance:`, `AICc:`, `Delta AICc`, `Akaike weight`) %>% 
  rename(`Log-Likelihood` = `logLik:`, Deviance = `deviance:`, AICc = `AICc:`)
#aic_tab

## @knitr centered-predictors
mod1_centered <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_centered

impact_centered <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM)),
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * scale(mean_imps))
impact_centered

drivers_centered <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% filter(!is.na(yi_SppR_ROM)) %>% mutate(scaled_invs = mean_invs * 10^-3),
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_centered

fitstats(mod1_aic)
fitstats(impact_aic)
fitstats(drivers_aic)



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





# Taxonomic group AICc

mixed_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed', !is.na(mean_imps)), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration)
mixed_aic
# pval = 0.03, duration = 0.01, n = 19

fish_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish', !is.na(mean_imps)), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration)
fish_aic
# No change, n = 12

algae_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'algae', !is.na(mean_imps)), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration)
algae_aic
# pval = 0.003, duration = 0.05, n = 4

inverts_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'inverts', !is.na(mean_imps)), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration)
inverts_aic


impacts_mixed_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'mixed'), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration * mean_imps)
impacts_mixed_aic

impacts_fish_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'fish'), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration * mean_imps)
impacts_fish_aic

impacts_algae_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'algae'), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration * mean_imps)
impacts_algae_aic

impacts_inverts_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = fixed_taxa %>% filter(taxa == 'inverts'), 
         random = ~ 1 | as.factor(Study.ID), method = 'ML', 
         mods = ~ Duration * mean_imps)
impacts_inverts_aic



## @knitr AICc-table-mixed
aic_tab_mixed <- 
  fitstats(mixed_aic, impacts_mixed_aic) %>% 
  mutate(metrics = rownames(.)) %>% 
  gather(key = Model, value = values, 1:2) %>% 
  spread(key = metrics, value = values) %>% 
  mutate(K = c(3, 1)) %>%
  mutate(min_AICc = min(`AICc:`), `Delta AICc` = `AICc:` - min_AICc, 
         sum_AICc_weight = sum(exp(-0.5 * `Delta AICc`)), 
         `Akaike weight` = exp(-0.5 * `Delta AICc` / sum_AICc_weight)) %>% 
  mutate(`Akaike weight` = round(`Akaike weight`, digits = 3)) %>% 
  mutate(Model = c('LR ~ Dur + CHI + Dur*CHI', 'LR ~ Dur')) %>% 
  arrange(-`Akaike weight`) %>% 
  mutate(Taxa = c('mixed', '')) %>% 
  select(Taxa, Model, `logLik:`, K, `deviance:`, `AICc:`, `Delta AICc`, `Akaike weight`) %>% 
  rename(`Log-Likelihood` = `logLik:`, Deviance = `deviance:`, AICc = `AICc:`)
aic_tab_mixed

## @knitr AICc-table-fish
aic_tab_fish <- 
  fitstats(fish_aic, impacts_fish_aic) %>% 
  mutate(metrics = rownames(.)) %>% 
  gather(key = Model, value = values, 1:2) %>% 
  spread(key = metrics, value = values) %>% 
  mutate(K = c(3, 1)) %>%
  mutate(min_AICc = min(`AICc:`), `Delta AICc` = `AICc:` - min_AICc, 
         sum_AICc_weight = sum(exp(-0.5 * `Delta AICc`)), 
         `Akaike weight` = exp(-0.5 * `Delta AICc` / sum_AICc_weight)) %>% 
  mutate(`Akaike weight` = round(`Akaike weight`, digits = 3)) %>% 
  mutate(Model = c('LR ~ Dur', 'LR ~ Dur + CHI + Dur*CHI')) %>% 
  arrange(-`Akaike weight`) %>% 
  mutate(Taxa = c('fish', '')) %>% 
  select(Taxa, Model, `logLik:`, K, `deviance:`, `AICc:`, `Delta AICc`, `Akaike weight`) %>% 
  rename(`Log-Likelihood` = `logLik:`, Deviance = `deviance:`, AICc = `AICc:`)
aic_tab_fish

## @knitr AICc-table-algae
aic_tab_algae <- 
  fitstats(algae_aic, impacts_algae_aic) %>% 
  mutate(metrics = rownames(.)) %>% 
  gather(key = Model, value = values, 1:2) %>% 
  spread(key = metrics, value = values) %>% 
  mutate(K = c(3, 1)) %>%
  mutate(min_AICc = min(`AICc:`), `Delta AICc` = `AICc:` - min_AICc, 
         sum_AICc_weight = sum(exp(-0.5 * `Delta AICc`)), 
         `Akaike weight` = exp(-0.5 * `Delta AICc` / sum_AICc_weight)) %>% 
  mutate(`Akaike weight` = round(`Akaike weight`, digits = 3)) %>% 
  mutate(Model = c('LR ~ Dur', 'LR ~ Dur + CHI + Dur*CHI')) %>% 
  arrange(-`Akaike weight`) %>% 
  mutate(Taxa = c('algae', '')) %>% 
  select(Taxa, Model, `logLik:`, K, `deviance:`, `AICc:`, `Delta AICc`, `Akaike weight`) %>% 
  rename(`Log-Likelihood` = `logLik:`, Deviance = `deviance:`, AICc = `AICc:`)
aic_tab_algae

## @knitr AICc-table-inverts
aic_tab_inverts <- 
  fitstats(inverts_aic, impacts_inverts_aic) %>% 
  mutate(metrics = rownames(.)) %>% 
  gather(key = Model, value = values, 1:2) %>% 
  spread(key = metrics, value = values) %>% 
  mutate(K = c(3, 1)) %>%
  mutate(min_AICc = min(`AICc:`), `Delta AICc` = `AICc:` - min_AICc, 
         sum_AICc_weight = sum(exp(-0.5 * `Delta AICc`)), 
         `Akaike weight` = exp(-0.5 * `Delta AICc` / sum_AICc_weight)) %>% 
  mutate(`Akaike weight` = round(`Akaike weight`, digits = 3)) %>% 
  mutate(Model = c('LR ~ Dur + CHI + Dur*CHI', 'LR ~ Dur')) %>%  
  arrange(-`Akaike weight`) %>% 
  mutate(Taxa = c('inverts', '')) %>%
  select(Taxa, Model, `logLik:`, K, `deviance:`, `AICc:`, `Delta AICc`, `Akaike weight`) %>% 
  rename(`Log-Likelihood` = `logLik:`, Deviance = `deviance:`, AICc = `AICc:`)
aic_tab_inverts

## @knitr AICc-table-taxonomic-groups
aic_groups <- bind_rows(aic_tab_mixed, aic_tab_fish, aic_tab_algae, aic_tab_inverts)