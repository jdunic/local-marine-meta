


ggplot(data = no_event)


Prediction


nutrients: 0 - 50 - 100
temp: min - max by 1 degree
shipping: 0 - 100

predicted LR


5 years, 20 years


function(unscaled_rma_object, invs, nuts, temp, duration) {
  # invasive values were scaled by 0.001 to make invasives data variance similar
  # in magnitude to the other drivers 
  #invs <- invs * 10^-3
#
  mods = cbind(1:duration, 
               rep(invs, duration),  
               rep(temp, duration), 
               rep(nuts, duration),
               invs*(1:duration), 
               temp*(1:duration), 
               nuts*(1:duration))
  predictions <- predict(object = unscaled_rma_object, newmods = mods)
  class(predictions) <- 'list'
  predictions <- predictions[1:6]
  predictions <- as_data_frame(predictions)
  predictions$duration <- 1:duration
  return(predictions)
}




temp = -10:10
invs = seq(from = 0, to = 150000, by = 1000)
duration = as.vector(c(5, 20), mode = 'integer')
nuts = c(nut_quantiles$`0%`, nut_quantiles$`50%`, nut_quantiles$`100%`)


prediction_df <- 
  crossing(duration, invs, temp, nuts) %>% 
  mutate(invs_dur = invs*duration, temp_dur = temp*duration, nuts_dur = nuts*duration)
  
prediction_mat <- (as.matrix(prediction_df))
dimnames(prediction_mat) <- NULL

g_preds <- 
  bind_cols(prediction_df, 
  predict.rma(object = drivers_unscaled, newmods = prediction_mat) %>% as_data_frame()
  ) %>%
  mutate(nuts_factor = case_when(nuts == nut_quantiles$`0%` ~ 'low (0)', nuts == nut_quantiles$`50%` ~ 'median (0.46)', nuts == nut_quantiles$`100%` ~ 'max (185)')) %>%
  mutate(nuts_factor = forcats::fct_relevel(nuts_factor, c('low (0)', 'median (0.46)', 'max (185)')))

ggplot(data = g_preds, aes(x = invs, y = temp, fill = pred)) + 
  geom_tile() + 
  scale_fill_gradient2(low = '#998ec3', high = '#f1a340', mid = 'white') + 
  facet_grid(factor(duration) ~ factor(nuts_factor))


#f1a340
#f7f7f7
#998ec3

nut_quantiles$`0%`
nut_quantiles$`50%`
nut_quantiles$`100%`


invs_quantiles$`0%`
invs_quantiles$`100%`

ltc_quantiles$`0%`
ltc_quantiles$`100%`


temp = c(-1, 0, 1)
#invs = c(0.1, 1, 10, 100, 500, seq(from = 1000, to = 160000, by = 1000))
invs = c(seq(from = 0.1, to = 1, by = 0.05), seq(from = 1.1, to = 10, by = 0.5),
         seq(from = 10.1, to = 100, by = 5),  
         seq(from = 101, to = 10000, by = 50), seq(from = 10001, to = 160000, by = 1000))
#invs = seq(from = 0.1, to = 160000, by = 500)
#invs2 = log(seq(from = 0.1, to = 160000, by = 500))
#invs = pretty(x = invs, 100)
duration = as.vector(c(5, 20), mode = 'integer')
#nuts = (nut_quantiles$`0%`):nut_quantiles$`100%`
nuts = c(seq(from = 0.001, to = 0.01, by = 0.0005), 0.0012,
         seq(from = 0.011, to = 0.1, by = 0.005), 0.012,
         seq(from = 0.11, to = 1, by = 0.05), 0.12, 
         seq(from = 1.1, to = 10, by = 0.5), 1.2,
         seq(from = 11, to = 100, by = 5), 12, 
         seq(from = 101, to = 200, by = 50), 120)
#

prediction_df <- 
  crossing(duration, invs, temp, nuts) %>% 
  mutate(invs_dur = invs*duration, temp_dur = temp*duration, nuts_dur = nuts*duration)  
prediction_mat <- (as.matrix(prediction_df))
dimnames(prediction_mat) <- NULL
#
g_preds <- 
  bind_cols(prediction_df, 
  predict.rma(object = drivers_unscaled, newmods = prediction_mat) %>% as_data_frame()
  ) %>%
  mutate(nuts_factor = case_when(nuts == nut_quantiles$`0%` ~ 'low (0)', nuts == nut_quantiles$`50%` ~ 'median (0.46)', nuts == nut_quantiles$`100%` ~ 'max (185)'))
beep()

test <- 
g_preds %>% 
  mutate(change = case_when(ci.ub < 0 ~ 'loss', ci.lb > 0 ~ 'gain', ci.lb < 0 & ci.ub > 0 ~ 'no change')) %>% 
  mutate(change = factor(change, level = c('gain', 'no change', 'loss')))

ggplot() + 
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank()) + 
  geom_point(data = test, aes(x = invs, y = nuts, colour = change), shape = 15) + 
  #scale_colour_manual(values = c("#a1d76a", "grey90", "#e9a3c9")) + 
  facet_grid(factor(duration) ~ factor(temp)) + 
  scale_x_log10() + 
  scale_y_log10(labels=scales::trans_format('log10', scales::math_format(10^.x))) + 
  geom_point(
    data = no_event %>% 
           filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
                  !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
           mutate(temp = case_when(sliced_ltc <= -1 ~ -1, sliced_ltc > -1 | sliced_ltc < 1 ~ 0, sliced_ltc >= 1 ~ 1)) %>% 
           mutate(duration = case_when(Duration <= 10 ~ 5, Duration > 10 ~ 20)) %>% 
           mutate(change = case_when(yi_SppR_ROM + vi_SppR_ROM < 0 ~ 'loss', yi_SppR_ROM - vi_SppR_ROM > 0 ~ 'gain', yi_SppR_ROM - vi_SppR_ROM < 0 & yi_SppR_ROM + vi_SppR_ROM > 0 ~ 'no change')) %>% 
          mutate(change = factor(change, level = c('gain', 'no change', 'loss')))
  , 
    aes(x = mean_invs + 0.1, y = mean_nuts + 0.001, colour = change)) + 
  geom_point(data = no_event %>% filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
         !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% mutate(temp = case_when(sliced_ltc <= -1 ~ -1, sliced_ltc > -1 | sliced_ltc < 1 ~ 0, sliced_ltc >= 1 ~ 1)) %>% 
  mutate(duration = case_when(Duration <= 10 ~ 5, Duration > 10 ~ 20)), 
    aes(x = mean_invs + 0.1, y = mean_nuts + 0.001), shape = 21, colour = 'black') + 
  scale_colour_manual(values = c("#a1d76a", "grey90", "#e9a3c9"))
beep()



[-16.5,-7.17] (-7.17,2.16] (2.16,11.5]

no_event %>% mutate(temp = case_when(sliced_ltc <= -5 ~ -10, sliced_ltc > -5 | sliced_ltc < 5 ~ 0, sliced_ltc >= 5 ~ 10)) %>% 
  mutate(duration = case_when(Duration <= 10 ~ 5, Duration > 10 ~ 20)) %>% select(Duration)


(sum(mod1$sigma2) - sum(impact_ne_w$sigma2)) / sum(mod1$sigma2)
(sum(mod1$sigma2) - sum(drivers_scaled$sigma2)) / sum(mod1$sigma2)
(sum(impact_ne_w$sigma2) - sum(drivers_scaled$sigma2)) / sum(impact_ne_w$sigma2)





test <- no_event %>% distinct(Study.ID, .keep_all = TRUE) %>% 
  select(Study.ID, Reference, taxa, mean_imps, mean_invs, mean_nuts, sliced_ltc, vi_SppR_ROM)



test2 <- 
list.files('/Users/jillian/Manuscripts/2018-Meta-analysis_ms/Meta-analysis_papers/raw_species_papers') %>% 
  as_data_frame() %>% 
  separate(col = value, into = c('Study.ID', 'Ref')) %>% 
  slice(-20)

test3 <- 
left_join(test2, test, by = c('Study.ID')) %>% 
as.data.frame()

test3 %>% 
  filter(!is.na(mean_imps) & !is.na(mean_invs) & !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  .$taxa %>% 
  table()



# 12 studies have all driver data
# 14 studies have mean_imps
# Only 1 study has any variance/replication of species composition change
# Taxa: 2 Mixed, 1 algae, 6 fish, 1 marine mammals, 1 plant, 1 zooplankton
# 


