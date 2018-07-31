temp = c(-1, 0, 1)
invs = c(seq(from = 0.1, to = 1, by = 0.05), seq(from = 1.1, to = 10, by = 0.5),
         seq(from = 10.1, to = 100, by = 5),  
         seq(from = 101, to = 10000, by = 50), seq(from = 10001, to = 160000, by = 1000))
duration = as.vector(c(5, 20), mode = 'integer')
nuts = c(seq(from = 0.001, to = 0.01, by = 0.0005), 0.0012,
         seq(from = 0.011, to = 0.1, by = 0.005), 0.012,
         seq(from = 0.11, to = 1, by = 0.05), 0.12, 
         seq(from = 1.1, to = 10, by = 0.5), 1.2,
         seq(from = 11, to = 100, by = 5), 12, 
         seq(from = 101, to = 200, by = 50), 120)
#

temp = c(-1, 0, 1)
invs = seq(from = 0, to = 160000, by = 100)
duration = as.vector(c(5, 20), mode = 'integer')
nuts = seq(from = 0, to = 200, by = 1)

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
  mutate(nuts_factor = case_when(nuts == nut_quantiles$`0%` ~ 'low (0)', nuts == nut_quantiles$`50%` ~ 'median (0.46)', nuts == nut_quantiles$`100%` ~ 'max (185)')) %>% 
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
  geom_point(data = g_preds, aes(x = invs, y = nuts, colour = change), shape = 15) + 
  #scale_colour_manual(values = c("#a1d76a", "grey90", "#e9a3c9")) + 
  facet_grid(factor(duration) ~ factor(temp)) + 
  #scale_x_log10() + 
  #scale_y_log10(labels=scales::trans_format('log10', scales::math_format(10^.x))) + 
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


