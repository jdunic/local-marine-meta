temp = c(-0.5, -0.001, 0.001, 0.5)
invs = seq(from = 0, to = 160000, by = 1000)
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
  predict.rma(object = drivers_unscaled, newmods = prediction_mat) %>% as_data_frame()) %>%
  mutate(nuts_factor = case_when(nuts == nut_quantiles$`0%` ~ 'low (0)', 
                                 nuts == nut_quantiles$`50%` ~ 'median (0.46)', 
                                 nuts == nut_quantiles$`100%` ~ 'max (185)')) %>% 
  mutate(change = case_when(ci.ub < 0 ~ 'loss', ci.lb > 0 ~ 'gain', ci.lb < 0 & ci.ub > 0 ~ 'no change')) %>% 
  mutate(change = factor(change, level = c('gain', 'no change', 'loss'))) %>%
  mutate(temp = case_when(temp <=  -0.5 ~ '< -0.5', 
                          temp >   -0.5 & temp < 0 ~ '-0.5 to 0',
                          temp >    0 & temp < 0.5 ~ '0 to -0.5', 
                          temp >=   0.5 ~ '> 0.5')) %>% 
  mutate(temp = factor(temp, levels = c('< -0.5', 
                                        '-0.5 to 0', 
                                        '0 to -0.5',
                                        '> 0.5'))) %>% 
  mutate(duration = case_when(duration == 5 ~ '5 years', duration == 20 ~ '20 years')) %>% 
  mutate(duration = factor(duration, levels = c('5 years', '20 years')))

dev.new(width = 10, height = 5)
ggplot() + 
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),
        strip.text.y = element_text(angle = 0), 
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  geom_point(data = filter(g_preds), aes(x = invs, y = nuts, colour = change), shape = 15) + 
  scale_colour_manual(values = c('#0571b0', 'grey90', '#ca0020')) + 
  #scale_x_log10() + 
  #scale_y_log10(labels=scales::trans_format('log10', scales::math_format(10^.x))) + 
  #geom_point(
  #  data = no_event %>% 
  #         filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
  #                !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  #         mutate(temp = case_when(sliced_ltc <= -0.5 ~ '< -0.5', 
  #                                 sliced_ltc > -0.5 & sliced_ltc <= -0.001 ~ '-0.5 to 0',
  #                                 sliced_ltc >= 0.001 & sliced_ltc < 0.5 ~ '0 to -0.5', 
  #                                 sliced_ltc >= 0.5 ~ '> 0.5')) %>% 
  #         mutate(temp = factor(temp, levels = c('< -0.5', 
  #                                               '-0.5 to 0', 
  #                                               '0 to -0.5', 
  #                                               '> 0.5'))) %>% 
  #         mutate(duration = case_when(Duration <= 10 ~ '5 years', Duration > 10 ~ '20 years')) %>%
  #         mutate(duration = factor(duration, levels = c('5 years', '20 years'))) %>%  
  #         mutate(change = case_when(yi_SppR_ROM + vi_SppR_ROM < 0 ~ 'loss', yi_SppR_ROM - vi_SppR_ROM > 0 ~ 'gain', yi_SppR_ROM - vi_SppR_ROM < 0 & yi_SppR_ROM + vi_SppR_ROM > 0 ~ 'no change')), 
  #  aes(x = mean_invs + 0.1, y = mean_nuts + 0.001, colour = change)) + 
  #geom_point(
  #  data = no_event %>% 
  #         filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_invs) & 
  #                !is.na(mean_nuts) & !is.na(sliced_ltc)) %>% 
  #         mutate(temp = case_when(sliced_ltc <= -0.5 ~ '< -0.5', 
  #                                 sliced_ltc > -0.5 & sliced_ltc <= -0.001 ~ '-0.5 to 0',
  #                                 sliced_ltc >= 0.001 & sliced_ltc < 0.5 ~ '0 to -0.5', 
  #                                 sliced_ltc >= 0.5 ~ '> 0.5')) %>% 
  #         mutate(temp = factor(temp, levels = c('< -0.5', 
  #                                              '-0.5 to 0', 
  #                                              '0 to -0.5', 
  #                                              '> 0.5'))) %>% 
  #         mutate(duration = case_when(Duration <= 10 ~ '5 years', Duration > 10 ~ '20 years')) %>% 
  #         mutate(duration = factor(duration, levels = c('5 years', '20 years'))), 
  #  aes(x = mean_invs + 0.1, y = mean_nuts + 0.001), shape = 21, colour = 'black') + 
  facet_grid(duration ~ temp) + 
  #xlab('\nInvasion potential (metric tonnes of cargo shipped)') + 
  #ylab('Nutrient use (metric tonnes of nitrogen and phosphorous fertilizer used)\n') + 
  xlab('\n   Invasion potential') + 
  ylab('Nutrient use\n') + 
  labs(colour = 'LRR') + 
  guides(colour = guide_legend(override.aes = list(size = 3))) + 
  ggtitle(expression("   Temperature ("*degree*"C)"))
beep()

dev.copy2pdf(file = '../figures/combined-drivers.pdf')


