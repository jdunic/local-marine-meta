library(tidyverse)
library(metafor)

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID))

event <- filter(fl_combined, Event == 'Yes')
no_event2 <- filter(fl_combined, Event != 'Yes')

# Increases in shannon diversity
mod1_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event2 %>% filter(!is.na(yi_Shan_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_shan

# Gains over time in shannon
# Gains with higher impacts
# Impacts over time decreases evenness
impact_ne_w_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event2,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_w_shan

# Short term LTC increases shannon
# Short term nutrient increases shannon
# Long term LTC decreases shannon
# Long term Nutrients decreases shannon
drivers_scaled_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event2 %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled_shan

drivers_unscaled_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event2 %>% filter(!is.na(yi_Shan_ROM), !is.na(vi_Shan_ROM)) %>% 
                filter(Study.ID != 47) %>% mutate(scaled_invs = mean_invs * 10^-3) %>%
                filter(!is.na(sliced_ltc), !is.na(mean_nuts)), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scaled_invs + sliced_ltc + mean_nuts),
         control=list(optimizer="bobyqa"))
drivers_unscaled_shan


# Pielou's
# Increases in evenness diversity
mod1_even <- 
  rma.mv(yi = yi_Even_ROM, V = vi_Even_ROM, 
         data = no_event2 %>% filter(!is.na(yi_Even_ROM), !is.na(vi_Even_ROM)) %>% 
                filter(Study.ID != 47), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_even

# Gains over time in evenness
# Gains with higher impacts
# Impacts over time decreases evenness
impact_ne_w_even <- 
  rma.mv(yi = yi_Even_ROM, V = vi_Even_ROM, 
         data = no_event2 %>% filter(!is.na(yi_Even_ROM), !is.na(vi_Even_ROM)) %>% 
                filter(Study.ID != 47), 
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_w_even

# Won't converge
drivers_scaled_even <- 
  rma.mv(yi = yi_Even_ROM, V = vi_Even_ROM, 
         data = no_event2 %>% filter(!is.na(yi_Even_ROM), !is.na(vi_Even_ROM)) %>% 
                filter(Study.ID != 47) %>% mutate(scaled_invs = mean_invs * 10^-3) %>%
                filter(!is.na(sliced_ltc), !is.na(mean_nuts)), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)),
         control=list(optimizer="bobyqa"))
drivers_scaled_even


test_scaled_aic <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2 %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, method = 'ML',
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts) + mean_imps))
test_scaled_aic

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

aic_tab

fitstats(mod1_aic, impact_aic, drivers_aic, test_scaled_aic) %>% 
  mutate(metrics = rownames(.)) %>% 
  gather(key = Model, value = values, 1:4) %>% 
  spread(key = metrics, value = values) %>%
  mutate(K = c(9, 7, 3, 1)) %>%
  mutate(min_AICc = min(`AICc:`), `Delta AICc` = `AICc:` - min_AICc, 
         sum_AICc_weight = sum(exp(-0.5 * `Delta AICc`)), 
         `Akaike weight` = exp(-0.5 * `Delta AICc` / sum_AICc_weight)) %>% 
  mutate(`Akaike weight` = round(`Akaike weight`, digits = 3)) %>% 
  mutate(Model = c('LR ~ Dur * (Inv + Nut + LTC + CHI)', 'LR ~ Dur * (Inv + Nut + LTC)', 'LR ~ Dur + CHI + Dur*CHI', 'LR ~ Dur')) %>% 
  select(Model, `logLik:`, K, `deviance:`, `AICc:`, `Delta AICc`, `Akaike weight`) %>% 
  rename(`Log-Likelihood` = `logLik:`, Deviance = `deviance:`, AICc = `AICc:`)

# I got ~8-9 better when I ran it with + impacts. I'm not sure that I really like that test as a way to answer reviewer 2, because it seems like it's almost double counting the impacts, and I still think that because the impact value is an aggregation of so many things it makes interpretation of this model with specific drivers + all impacts a challenge. On the other hand, I don't have a better solution to suggest. 

# I like figure 3, I like the addition of more years, and agree with Marc's comment about having + temps. I also really like the alpha to show magnitude of change. 



## @knitr duration-var-weighted-shannon-model-output-table1
mod1_shan_output_df <- 
  data_frame(model = 'LR ~ D', 
             parameter = rownames(mod1_shan$b), 
             estimate = round(as.vector(mod1_shan$b), digits = 3),
             `% change` = round(get_percent_change(as.vector(mod1_shan$b)), digits = 2), 
             se = round(mod1_shan$se, digits = 3), 
             pval = round(mod1_shan$pval, digits = 3), 
             ci.lb = round(mod1_shan$ci.lb, digits = 3), 
             ci.ub = round(mod1_shan$ci.ub, digits = 3), 
             k = mod1_shan$k, 
             n = mod1_shan$s.nlevels)

#mod1_output_df %>% 
#  knitr::kable(., caption = '**Table 1.** Parameter estimates from a variance-weighted meta-regression of the log ratio of the proportion of species richness change (LR) over study duration (D). For each parameter estimate the standard error (se), p-value (pval), lower (ci.lb) and upper (ci.ub) 95% confidence intervals, number of sites (k) and number of studies (n) is included. The percent change (% change) has been back-calculated from the estimated log ratio of the proportion of change in species richness. Each study could contain multiple sites and therefore studies were modeled as random effects.')

## @knitr impact-var-weighted-shannon-model-output-table2
imp_mod_shan_output_df <- 
  data_frame(model = 'LR ~ Imp + D + D*Imp', 
             parameter = rownames(impact_ne_w_shan$b), 
             estimate = round(as.vector(impact_ne_w_shan$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(impact_ne_w_shan$b)), digits = 2), 
             se = round(impact_ne_w_shan$se, digits = 3), 
             pval = round(impact_ne_w_shan$pval, digits = 3), 
             ci.lb = round(impact_ne_w_shan$ci.lb, digits = 3), 
             ci.ub = round(impact_ne_w_shan$ci.ub, digits = 3), 
             k = impact_ne_w_shan$k, 
             n = impact_ne_w_shan$s.nlevels) %>% 
  mutate(parameter = gsub('mean_imps', 'Impact', parameter)) %>% 
  mutate(per_lb = round(get_percent_change(impact_ne_w_shan$ci.lb), digits = 3), 
         per_ub = round(get_percent_change(impact_ne_w_shan$ci.ub), digits = 3))


## @knitr drivers-var-weighted-shannon-model-output-df-scaled-table3
drivers_mod_shan_output_df <- 
  data_frame(model = 'LR ~ D * (I + N + LTC)', 
             parameter = gsub('scale\\(|\\)', replacement = '', rownames(drivers_scaled_shan$b)), 
             estimate = round(as.vector(drivers_scaled_shan$b), digits = 3), 
             `% change` = round(get_percent_change(as.vector(drivers_unscaled$b)), digits = 4), 
             se = round(drivers_scaled_shan$se, digits = 3), 
             pval = round(drivers_scaled_shan$pval, digits = 3), 
             ci.lb = round(drivers_scaled_shan$ci.lb, digits = 3), 
             ci.ub = round(drivers_scaled_shan$ci.ub, digits = 3), 
             k = drivers_scaled_shan$k, 
             n = drivers_scaled_shan$s.nlevels) %>% 
  mutate(parameter = gsub('scaled_invs', 'Inv', parameter)) %>% 
  mutate(parameter = gsub('sliced_ltc', 'LTC', parameter)) %>% 
  mutate(parameter = gsub('mean_nuts', 'Nut', parameter)) %>% 
  mutate(per_lb = round(get_percent_change(drivers_unscaled_shan$ci.lb), digits = 5), 
         per_ub = round(get_percent_change(drivers_unscaled_shan$ci.ub), digits = 3))