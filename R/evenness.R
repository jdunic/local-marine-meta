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
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
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