## world-map
ggplot(data = vw_spatial, aes(x = Start_Long, y = Start_Lat)) + 
  #theme_void() + 
  borders('world', colour = NA, fill = 'darkgrey', alpha = 0.8) + 
  theme(legend.position = 'none') +
  theme(panel.background = element_rect(fill = 'white'), 
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank()) + 
  xlab("Longitude") +
  ylab("Latitude")

## site-map
ggplot(data = vw_spatial, aes(x = Start_Long, y = Start_Lat)) + 
  #theme_void() + 
  borders('world', colour = NA, fill = 'darkgrey', alpha = 0.8) + 
  geom_jitter(mapping = aes(colour = factor(study_site)), width = 2, height = 2, size = 3, alpha = 0.8) + 
  theme(legend.position = 'none') +
  theme(panel.background = element_rect(fill = 'white'), 
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank()) + 
  xlab("Longitude") +
  ylab("Latitude")

#-------------------------------------------------------------------------------

## No covariates
filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)) %>%
ggplot(data = .) +
  theme_wb() + 
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 3) + 
  theme(legend.position = 'none', 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
  geom_hline(yintercept = -2, colour = 'white') +
  geom_vline(xintercept = 0, colour = 'white') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed', size = 1) +
  xlab('\nDuration (years)\n') +
  ylab('Log(species richness change)\n') +
  ylim(-2, 2) + 
  geom_ribbon(data = data.frame(x = 1:41, ymin = predict(mod1, 1:41)$ci.lb, ymax = predict(mod1, 1:41)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), fill = 'lightgrey', alpha = 0.5) + 
  geom_line(data = data.frame(x = 1:41, y = predict(mod1, 1:41)$pred), aes(x = x, y = y), colour = 'white', size = 2, alpha = 0.8)

#-------------------------------------------------------------------------------

## Impact coefficients
bare_impacts <- 
mk_rma_summary_df(impact_ne_w) %>%
  mutate(driver = gsub("mean_imps", "Cumulative human impact", driver)) %>%
  filter(driver != 'intrcpt') %>% 
  mutate(driver = factor(driver, levels = c('Cumulative human impact', 'Duration', 'Duration:Cumulative human impact'))) %>%
  mutate(plot_cats = factor(c('Duration', 'Short term\nCumulative Impact', 'Long term\nCumulative Impact'), levels = c('Duration', 'Short term\nCumulative Impact', 'Long term\nCumulative Impact'))) %>% 
  ggplot(data = ., aes(x = estimate, y = plot_cats, colour = plot_cats)) + 
    theme_wb() +
    geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.2) + 
    geom_point(size = 5) + 
    geom_vline(xintercept = 0, colour = 'yellow', linetype = 'dashed') + 
    xlim(c(-0.12, 0.12)) + 
    geom_hline(yintercept = 0.45, colour = 'white') +
    geom_vline(xintercept = -0.1, colour = 'white') +
    theme(legend.position = 'none', 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
    #ylab("\nPredictor\n") + 
    ylab('') + 
    xlab('Log(species richness change)\n') +
    coord_flip() + 
    scale_colour_manual(values = c('black', 'black', 'black')) + 
    scale_fill_manual(values = c('black', 'black', 'black'))

bare_impacts

bare_impacts + 
  scale_colour_manual(values = c('white', 'black', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

bare_impacts + 
  scale_colour_manual(values = c('white', 'white', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

bare_impacts + 
  scale_colour_manual(values = c('white', 'white', 'white')) + 
  geom_point(size = 5, shape = 21, colour = 'black', fill = NA)



#-------------------------------------------------------------------------------

## Impact prediction
## @knitr impacts-var-weighted-predictions
mean_imp_value <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & !is.na(mean_imps)) %>% summarise(mean(mean_imps))

no_imps <- get_imp_predictions(rma_object = impact_ne_w, impact_value = 0, duration = 20) %>% 
  mutate(impact_value = 0)

mean_imp_preds <- get_imp_predictions(rma_object = impact_ne_w, impact_value = mean_imp_value[[1]], duration = 20) %>% 
  mutate(impact_value = round(mean_imp_value[[1]], digits = 1))

# in our data = 8.89
# global from recent halpern = 11.1
max_imp_preds <- get_imp_predictions(rma_object = impact_ne_w, impact_value = 11.1, duration = 20) %>% 
  mutate(impact_value = round(mean_imp_value[[1]], digits = 1))

## Bare plot
bare_impact_predictions <- 
  ggplot() + 
    geom_hline(yintercept = 0, colour = 'white') +
    geom_vline(xintercept = 0, colour = 'white') +
    theme_wb() +
    theme(legend.position = 'none', 
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 26), 
          panel.border = element_blank(), 
          plot.background = element_rect(colour = 'black', fill = 'black')) +
    xlim(c(0, 20)) + 
    ylim(-1.3, 1.3) +
    #ylim(c(-0.5, 0.5)) + 
    ylab('Predicted log response ratio\n') + 
    xlab('\nDuration (years)\n')
bare_impact_predictions

no_impacts_plot <- 
  bare_impact_predictions + 
  geom_line(data = no_imps, aes(x = duration, y = (pred)), colour = '#00BFC4', size = 2) + 
  geom_ribbon(data = no_imps, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#00BFC4', alpha = 0.4)
no_impacts_plot

mean_impacts_plot <- 
  no_impacts_plot + 
  geom_line(data = mean_imp_preds, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  geom_ribbon(data = mean_imp_preds, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)
mean_impacts_plot

max_impacts_plot <- 
  mean_impacts_plot + 
  geom_line(data = max_imp_preds, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = max_imp_preds, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)
max_impacts_plot


 

#-------------------------------------------------------------------------------

#model_driver_vector <- 
#  c('Dur', 'Invasives', 'LTC', 'Nutrients', 'Dur:Invasives', 
#    'Dur:LTC', 'Duration:Nutrients')
#ordered_driver_vector <- 
#  c('Duration', 'Nutrients', 'Invasives', 'LTC', 'Duration:Invasives', 
#    'Duration:Nutrients', 'Duration:LTC')

dev.new(height = 7.5, width = 11.25)

model_driver_vector <- 
  c('Duration', 'Invasives', 'Rate of      \ntemp change', 'Nutrients', 'Invasives ', 
    'Rate of      \ntemp change ', 'Nutrients ')
ordered_driver_vector <- 
  c('Duration', 'Nutrients', 'Invasives', 'Rate of      \ntemp change', 
    'Nutrients ', 'Invasives ', 'Rate of      \ntemp change ')

drivers_bare_plot <- 
mk_rma_summary_df(drivers_scaled) %>% 
  filter(driver != 'intrcpt') %>% 
  mutate(grouping = as.character(driver)) %>% 
  mutate(grouping = factor(model_driver_vector, 
                           levels = (ordered_driver_vector))) %>% 
  #mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
  #filter(short_term == 'yes') %>%
  ggplot(data = ., aes(x = estimate, y = grouping, colour = grouping)) + 
    geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
    geom_point(size = 5) + 
    theme_wb() +
    xlim(c(-3.2, 3.2)) + 
    geom_hline(yintercept = 0.5, colour = 'white') +
    geom_vline(xintercept = 0, colour = 'yellow', linetype = 'dashed') +
    geom_vline(xintercept = -3.2, colour = 'white') +
    theme(legend.position = 'none', 
          axis.text = element_text(size = 20, angle = 45, hjust = 1), 
          axis.title = element_text(size = 26), 
          panel.border = element_blank(), 
          plot.background = element_rect(colour = 'black', fill = 'black')) +
    ylab("") + 
    xlab('Standardised coefficient estimate\n') + 
    coord_flip() + 
    scale_colour_manual(values = c('black', 'black', 'black', 'black', 'black', 'black', 'black'))
drivers_bare_plot

# Duration
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'black', 'black', 'black', 'black', 'black', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

# Short nuts
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'white', 'black', 'black', 'black', 'black', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

# Long nuts
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'white', 'black', 'black', 'white', 'black', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

# Short Invasives
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'white', 'white', 'black', 'white', 'black', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

# Long Invasives
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'white', 'white', 'black', 'white', 'white', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

# Short LTC
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'white', 'white', 'white', 'white', 'white', 'black')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

# Long LTC
drivers_bare_plot + 
  scale_colour_manual(values = c('white', 'white', 'white', 'white', 'white', 'white', 'white')) + 
  geom_point(size = 5, shape = 21, colour = 'black')

#-------------------------------------------------------------------------------
## Bare plot
bare_driver_predictions <- 
  ggplot() + 
    geom_hline(yintercept = 0, colour = 'white') +
    geom_vline(xintercept = 0, colour = 'white') +
    theme_wb() +
    theme(legend.position = 'none', 
          axis.text = element_text(size = 20), 
          axis.title = element_blank(), 
          panel.border = element_blank(), 
          plot.background = element_rect(colour = 'black', fill = 'black')) +
    xlim(c(0, 20)) + 
    ylim(c(-8, 5)) + 
    ylab('Predicted log response ratio\n') + 
    xlab('\nDuration (years)\n') + 
    geom_line(data = all0, aes(x = duration, y = (pred)), colour = '#00BFC4', size = 2, alpha = 0) + 
    geom_ribbon(data = all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#00BFC4', alpha = 0)
bare_driver_predictions

empty_nuts <- 
bare_driver_predictions + 
  ggtitle('Nutrients') + 
  theme(title = element_text(colour = 'white', size = 20))
empty_invs <- 
bare_driver_predictions + 
  ggtitle('Invasives') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank())
empty_ltc <- 
bare_driver_predictions + 
  ggtitle('Temperature') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank())

master_layout <- 
grid.layout(nrow = 2, ncol = 4, 
            widths = unit(c(0.1, 1, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
grid.newpage()
pushViewport(viewport(layout = master_layout))
grid.rect(gp=gpar(col='black', fill='black'), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:4))
print(empty_nuts, vp = set_vp(1, 2))
print(empty_invs, vp = set_vp(1, 3))
print(empty_ltc, vp = set_vp(1, 4))
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 20, col = 'white'), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:4),
    gp = gpar(fontsize = 20, col = 'white'), hjust = 0.25, vjust = 1
    )

# All drivers set to ZERO
no_drivers_plot <- 
  bare_driver_predictions + 
  geom_line(data = all0, aes(x = duration, y = (pred)), colour = '#00BFC4', size = 2) + 
  geom_ribbon(data = all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#00BFC4', alpha = 0.4)

no_drivers_nuts <- 
no_drivers_plot + 
  ggtitle('Nutrients') + 
  theme(title = element_text(colour = 'white', size = 20))
no_drivers_invs <- 
no_drivers_plot + 
  ggtitle('Invasives') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank())
no_drivers_ltc <- 
no_drivers_plot + 
  ggtitle('Temperature') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank())

master_layout <- 
grid.layout(nrow = 2, ncol = 4, 
            widths = unit(c(0.1, 1, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
grid.newpage()
pushViewport(viewport(layout = master_layout))
grid.rect(gp=gpar(col='black', fill='black'), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:4))
print(no_drivers_nuts, vp = set_vp(1, 2))
print(no_drivers_invs, vp = set_vp(1, 3))
print(no_drivers_ltc, vp = set_vp(1, 4))
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 20, col = 'white'), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:4),
    gp = gpar(fontsize = 20, col = 'white'), hjust = 0.25, vjust = 1
    )

# Single driver effects
only_nuts <- 
no_drivers_plot + 
  ggtitle('Nutrients') + 
  theme(title = element_text(colour = 'white', size = 20)) + 
  geom_line(data = nuts_midpoint, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  geom_ribbon(data = nuts_midpoint, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)
  #geom_line(data = nuts_q50_all0, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  #geom_ribbon(data = nuts_q50_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)
only_invs <- 
no_drivers_plot + 
  ggtitle('Invasives') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank()) +
  geom_line(data = invs_midpoint, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  geom_ribbon(data = invs_midpoint, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)
  #geom_line(data = invs_q50_all0, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  #geom_ribbon(data = invs_q50_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)
only_ltc <- 
no_drivers_plot + 
  ggtitle('Temperature') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank()) + 
  geom_line(data = ltc_midpoint, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  geom_ribbon(data = ltc_midpoint, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)
  #geom_line(data = ltc_q50_all0, aes(x = duration, y = (pred)), colour = 'yellow', size = 2) + 
  #geom_ribbon(data = ltc_q50_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.5)

master_layout <- 
grid.layout(nrow = 2, ncol = 4, 
            widths = unit(c(0.1, 1, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
grid.newpage()
pushViewport(viewport(layout = master_layout))
grid.rect(gp=gpar(col='black', fill='black'), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:4))
print(only_nuts, vp = set_vp(1, 2))
print(only_invs, vp = set_vp(1, 3))
print(only_ltc, vp = set_vp(1, 4))
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 20, col = 'white'), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:4),
    gp = gpar(fontsize = 20, col = 'white'), hjust = 0.25, vjust = 1
    )

# Single driver at max effects
max_only_nuts <- 
only_nuts + 
  ggtitle('Nutrients') + 
  theme(title = element_text(colour = 'white', size = 20)) + 
  geom_line(data = nuts_q100_all0, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = nuts_q100_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)
max_only_invs <- 
only_invs + 
  ggtitle('Invasives') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank()) +
  geom_line(data = invs_q100_all0, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = invs_q100_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)
max_only_ltc <- 
only_ltc + 
  ggtitle('Temperature') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank()) + 
  geom_line(data = ltc_q100_all0, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = ltc_q100_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)

master_layout <- 
grid.layout(nrow = 2, ncol = 4, 
            widths = unit(c(0.1, 1, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
grid.newpage()
pushViewport(viewport(layout = master_layout))
grid.rect(gp=gpar(col='black', fill='black'), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:4))
print(max_only_nuts, vp = set_vp(1, 2))
print(max_only_invs, vp = set_vp(1, 3))
print(max_only_ltc, vp = set_vp(1, 4))
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 20, col = 'white'), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:4),
    gp = gpar(fontsize = 20, col = 'white'), hjust = 0.25, vjust = 1
    )


# Single driver at max effects
max_only_nuts <- 
only_nuts + 
  ggtitle('Nutrients') + 
  theme(title = element_text(colour = 'white', size = 20)) + 
  geom_line(data = nuts_q100_all0, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = nuts_q100_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)
max_only_invs <- 
only_invs + 
  ggtitle('Invasives') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank()) +
  geom_line(data = invs_q100_all0, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = invs_q100_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)
max_only_ltc <- 
only_ltc + 
  ggtitle('Temperature') + 
  theme(title = element_text(colour = 'white', size = 20), 
        axis.text.y = element_blank()) + 
  geom_line(data = ltc_q100_all0, aes(x = duration, y = (pred)), colour = '#F8766D', size = 2) + 
  geom_ribbon(data = ltc_q100_all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#F8766D', alpha = 0.5)

master_layout <- 
grid.layout(nrow = 2, ncol = 4, 
            widths = unit(c(0.1, 1, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
grid.newpage()
pushViewport(viewport(layout = master_layout))
grid.rect(gp=gpar(col='black', fill='black'), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:4))
print(max_only_nuts, vp = set_vp(1, 2))
print(max_only_invs, vp = set_vp(1, 3))
print(max_only_ltc, vp = set_vp(1, 4))
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 20, col = 'white'), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:4),
    gp = gpar(fontsize = 20, col = 'white'), hjust = 0.25, vjust = 1
    )

# All drivers at mean
dev.new(height = 6.6, width = 9.88)

empty_all_mean_plot <- 
  ggplot() + 
  geom_hline(yintercept = 0, colour = 'white') +
  geom_vline(xintercept = 0, colour = 'white') +
  theme_wb() +
  theme(legend.position = 'none', 
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
  xlim(c(0, 20)) + 
  ylim(c(-1, 1)) + 
  ylab('Predicted log response ratio\n') + 
  xlab('\nDuration (years)\n') + 
  geom_line(data = all0, aes(x = duration, y = (pred)), colour = '#00BFC4', size = 2, alpha = 0) + 
  geom_ribbon(data = all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#00BFC4', alpha = 0)
empty_all_mean_plot


empty_all_mean_plot + 
  geom_line(data = all0, aes(x = duration, y = (pred)), colour = '#00BFC4', size = 2, alpha = 0.45) + 
  geom_ribbon(data = all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#00BFC4', alpha = 0.45)

empty_all_mean_plot + 
  geom_line(data = all0, aes(x = duration, y = (pred)), colour = '#00BFC4', size = 2, alpha = 0.45) + 
  geom_ribbon(data = all0, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = '#00BFC4', alpha = 0.45) + 
  geom_line(data = all_mean, aes(x = duration, y = (pred)), colour = 'yellow', size = 2, alpha = 0.45) + 
  geom_ribbon(data = all_mean, aes(x = duration, ymin = (ci.lb), ymax = (ci.ub)), fill = 'yellow', alpha = 0.45)

#-------------------------------------------------------------------------------
# Taxonomic results
taxa_results %>% 
  mutate(weighting = c(rep('variance weighted', 5), rep('unweighted', 6))) %>% 
  filter(weighting == 'variance weighted') %>% 
  mutate(taxa = factor(taxa, levels = c('mixed', 'algae', 'coral', 'inverts', 'zooplankton', 'fish'))) %>%
ggplot(data = ., aes(x = taxa, y = estimate)) + 
    theme_wb() +
    geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), width = 0, size = 1.2, colour = 'white') + 
    geom_point(size = 5, colour = 'white') + 
    geom_hline(yintercept = 0, colour = 'yellow', linetype = 'dashed') + 
    ylim(c(-0.12, 0.12)) + 
    geom_hline(yintercept = -0.12, colour = 'white') +
    geom_vline(xintercept = 0.5, colour = 'white') +
    theme(legend.position = 'none', 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
    #ylab("\nPredictor\n") + 
    xlab('') + 
    ylab('Log(species richness change)\n')




  ggplot(data = ., aes(x = estimate, y = plot_cats, colour = plot_cats)) + 
    theme_wb() +
    geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.2) + 
    geom_point(size = 5) + 
    geom_vline(xintercept = 0, colour = 'yellow', linetype = 'dashed') + 
    xlim(c(-0.12, 0.12)) + 
    geom_hline(yintercept = 0.45, colour = 'white') +
    geom_vline(xintercept = -0.1, colour = 'white') +
    theme(legend.position = 'none', 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        ) +
    #ylab("\nPredictor\n") + 
    ylab('') + 
    xlab('Log(species richness change)\n') +
    coord_flip() + 
    scale_colour_manual(values = c('black', 'black', 'black')) + 
    scale_fill_manual(values = c('black', 'black', 'black'))


# heat map plot

ltc_q100_mean <- get_driver_predictions(unscaled_rma_object = drivers_unscaled, 
                               invs = mean_values$mean_invs, 
                               nuts = mean_values$mean_nuts, 
                               temp = ltc_quantiles$`100%`, duration = 20) %>% 
        mutate(invs_quantile = 'mean', invs_value = mean_values$mean_invs, 
               nuts_quantile = 'mean', nuts_value = mean_values$mean_nuts, 
               temp_quantile = '100', temp_value = ltc_quantiles$`100%`)



#
bare_impacts <- 

# temp effects changing over time theory plot - bare
data_frame(driver = c('Short term\ntemperature increase', 'Long term\ntemperature increase'), estimate = c(0.25, -0.25)) %>% 
  mutate(driver = factor(driver, levels = driver)) %>% 
  ggplot(data = ., aes(x = estimate, y = driver, colour = driver)) + 
    theme_wb() +
    geom_point(size = 5) + 
    geom_vline(xintercept = 0, colour = 'yellow', linetype = 'dashed') + 
    geom_hline(yintercept = 0.45, colour = 'white') +
    geom_vline(xintercept = -0.4, colour = 'white') +
    xlim(c(-0.4, 0.4)) + 
    theme(legend.position = 'none', 
        axis.text = element_text(size = 20), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
    #ylab("\nPredictor\n") + 
    ylab('') + 
    xlab('Log(species richness change)\n') +
    coord_flip()

# temp effects changing over time theory plot - bare
data_frame(driver = c('Short term\ntemperature increase', 'Long term\ntemperature increase'), estimate = c(0, 1), years = c(1970, 2010)) %>% 
  mutate(driver = factor(driver, levels = driver)) %>% 
  ggplot(data = ., aes(y = estimate, x = years)) + 
    theme_wb() +
    geom_point(size = 5, colour = 'black') + 
    geom_hline(yintercept = 0, colour = 'white') +
    geom_vline(xintercept = 1970, colour = 'white') +
    #xlim(c(0, 2)) + 
    #ylim(c(1970, 2010)) + 
    theme(legend.position = 'none', 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
    xlab('\nTime (years)') + 
    ylab('Richness')



# bare no driver prediction plot
data_frame(driver = c(1, 2), estimate = c(0.25, -0.25)) %>% 
  ggplot(data = ., aes(x = estimate, y = driver, colour = driver)) + 
    theme_wb() +
    geom_point(colour = 'black') + 
    geom_abline(intercept = 0, slope = -0.1) + 
    geom_vline(xintercept = 0, colour = 'yellow', linetype = 'dashed') + 
    geom_hline(yintercept = 0.45, colour = 'white') +
    geom_vline(xintercept = -0.4, colour = 'white') +
    xlim(c(-0.4, 0.4)) + 
    theme(legend.position = 'none', 
        axis.text = element_blank(),
        #axis.text = element_text(size = 20), 
        axis.title = element_text(size = 26), 
        panel.border = element_blank(), 
        plot.background = element_rect(colour = 'black', fill = 'black')) +
    #ylab("\nPredictor\n") + 
    ylab('Duration (years)') + 
    xlab('Log(species richness change)\n') +
    coord_flip()
