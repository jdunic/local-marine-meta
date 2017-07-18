dev.new(height = 3.2, width = 6.5)
duration_w <- 
filter(no_event, !is.na(yi_SppR_ROM), !is.na(vi_SppR_ROM)) %>%
ggplot(data = .) +
  geom_point(aes(x = Duration, y = yi_SppR_ROM, colour = as.factor(Study.ID)), size = 1) + 
  ylab('Log ratio') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') +
  #ggtitle('Weighted Analysis') + 
  xlab('Duration (years)') +
  ylim(-2, 2) + 
  geom_line(data = data.frame(x = 1:41, y = predict(mod1, 1:41)$pred), aes(x = x, y = y), colour = 'blue') +
  geom_ribbon(data = data.frame(x = 1:41, ymin = predict(mod1, 1:41)$ci.lb, ymax = predict(mod1, 1:41)$ci.ub), aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.1)
duration_w

dev.copy2pdf(file = '../figures/figure_2.pdf')


mod1_output_df[2, c('model', 'k', 'n')] <- ''
imp_mod_output_df[2:4, c('model', 'k', 'n')] <- ''
drivers_mod_output_df[2:8, c('model', 'k', 'n')] <- ''
#interactions_output_df[2:12, c('model', 'k', 'n')] <- ''

#bind_rows(mod1_output_df, imp_mod_output_df, drivers_mod_output_df, interactions_output_df) %>% 
bind_rows(mod1_output_df, imp_mod_output_df, drivers_mod_output_df) %>% 
  mutate(parameter = gsub('intrcpt', 'Int', .$parameter)) %>% 
  mutate(parameter = gsub('Duration', 'Dur', .$parameter)) %>% 
  mutate(parameter = gsub('Impact', 'CHI', .$parameter)) %>% 
  mutate(parameter = gsub('total_ltc', 'LTC', .$parameter)) %>% 
  mutate(model = gsub('D', 'Dur', .$model)) %>%  
  mutate(model = gsub('Imp', 'CHI', .$model)) %>%  
  mutate(model = gsub('\\(I', '\\(Inv', .$model)) %>%  
  mutate(model = gsub('N', 'Nut', .$model)) %>%
  mutate(`% change` = format(.$`% change`, digits = 1)) %>% 
  mutate(`lower CI` = round(as.numeric(.$ci.lb), digits = 2),
         `upper CI` = round(as.numeric(.$ci.ub), digits = 2)) %>% 
  dplyr::select(model, n, k, parameter, estimate, `% change`, pval, `lower CI`, `upper CI`) %>% 
  readr::write_csv('../figures/table1.csv')

dev.new(height = 5, width = 7)
mk_rma_summary_df(impact_ne_w) %>%
  mutate(driver = gsub("mean_imps", "Cumulative human impact", driver)) %>%
  filter(driver != 'intrcpt') %>% 
  mutate(driver = factor(driver, levels = c('Duration:Cumulative human impact', 'Duration', 'Cumulative human impact'))) %>%
  ggplot(data = ., aes(x = estimate, y = driver)) + 
    theme_bw() +
    geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 0.8) + 
    geom_point(size = 3) + 
    geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
    xlim(c(-0.12, 0.12)) + 
    theme(axis.text.y = element_text(hjust = 1, size = 14), 
          axis.text.x = element_text(size = 14), 
          axis.title = element_text(size = 16)) + 
    ylab("") + 
    xlab('\nCoefficient estimate')
dev.copy2pdf(file = '../figures/figure_3.pdf')

dev.new(height = 6, width = 9)
model_driver_vector <- 
  c('Duration', 'Invasives', 'LTC', 'Nutrients', 'Duration:Invasives', 
    'Duration:LTC', 'Duration:Nutrients')
ordered_driver_vector <- 
  c('Duration', 'Invasives', 'Nutrients', 'LTC', 'Duration:Invasives', 
    'Duration:Nutrients', 'Duration:LTC')

driver_summary_plot_short  <- 
  mk_rma_summary_df(drivers_scaled) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'yes') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-3.2, 3.2)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_short

driver_summary_plot_long  <- 
  mk_rma_summary_df(drivers_scaled) %>% 
    filter(driver != 'intrcpt') %>% 
    mutate(grouping = as.character(driver)) %>% 
    mutate(grouping = factor(model_driver_vector, 
                             levels = rev(ordered_driver_vector))) %>% 
    mutate(short_term = c('no', 'yes', 'yes', 'yes', 'no', 'no', 'no')) %>%
    filter(short_term == 'no') %>%
    ggplot(data = ., aes(x = estimate, y = grouping)) + 
      theme_bw() +
      geom_errorbarh(aes(xmin = ci_lb, xmax = ci_ub), height = 0, size = 1.5) + 
      geom_point(size = 4) + 
      geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') + 
      xlim(c(-0.25, 0.25)) + 
      theme(axis.text.y = element_text(hjust = 1, size = 13), 
            axis.text.x = element_text(size = 13), 
            axis.title = element_text(size = 13), 
            plot.background = element_blank()) + 
      ylab("") + 
      xlab('\nStandardised coefficient estimate')
#driver_summary_plot_long

text_a <- textGrob('a)', vjust = -22, hjust = 0)
text_b <- textGrob('b)', vjust = -22, hjust = -2)
grid.arrange(text_a, driver_summary_plot_short, text_b, driver_summary_plot_long, ncol = 4, widths=unit(c(0.5, 10, 0.5, 12), 'cm'))
dev.copy2pdf(file = '../figures/figure_4.pdf')


dev.new(height = 4.5, width = 13)
driver_predictions <- bind_rows(all0, invs_q100_all0, nuts_q100_all0, ltc_q100_all0)
#
nuts_alone_predictions <- 
driver_predictions %>% 
  filter(invs_quantile == 0, temp_quantile == 0) %>% 
  mutate(nuts_quantile = factor(nuts_quantile, levels = c('0', '50', '100'))) %>% 
  mutate(driver = 'Nutrient addition') %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = nuts_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = nuts_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5)) + 
  ylab('Predicted log response ratio') + 
  xlab('\nDuration (years)') + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 16), 
        strip.text = element_text(size = 16))
invs_alone_predictions <- 
driver_predictions %>% 
  filter(nuts_quantile == 0, temp_quantile == 0) %>% 
  mutate(invs_quantile = factor(invs_quantile, levels = c('0', '50', '75', '100'))) %>% 
  mutate(driver = 'Propagule pressure') %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = invs_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = invs_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5)) + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 16), 
        strip.text = element_text(size = 16))
#
ltc_alone_predictions <- 
driver_predictions %>% 
  filter(nuts_quantile == 0, invs_quantile == 0) %>% 
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', '50', '100'))) %>% 
  mutate(driver = 'Linear temperature change') %>% 
ggplot(data = .) + 
  geom_line(aes(x = duration, y = (pred), colour = temp_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = temp_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5)) + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 16), 
        strip.text = element_text(size = 16))
#
all_mean_predictions <- 
bind_rows(all0, all_mean) %>% 
  mutate(temp_quantile = factor(temp_quantile, levels = c('0', 'mean', '100'))) %>%
  mutate(driver = 'All drivers') %>% 
ggplot(data = ., aes(group = temp_quantile)) + 
  geom_line(aes(x = duration, y = (pred), colour = temp_quantile)) + 
  geom_ribbon(aes(x = duration, ymin = (ci.lb), ymax = (ci.ub), fill = temp_quantile), alpha = 0.4) + 
  theme_minimal() +
  scale_colour_manual(values = c('darkgrey', '#619CFF')) + 
  scale_fill_manual(values = c('darkgrey', '#619CFF')) + 
  guides(colour = FALSE) +
  guides(fill = FALSE) +
  xlim(c(0, 15)) + 
  ylim(c(-5, 5)) + 
  facet_grid(. ~ driver) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 16), 
        strip.text = element_text(size = 16))
#

master_layout <- 
grid.layout(nrow = 2, ncol = 5, 
            widths = unit(c(0.1, 1, 0.9, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1), "null"))
#dev.new(height = 3.5, width = 11)

grid.newpage()
pushViewport(viewport(layout = master_layout))
print(nuts_alone_predictions, vp = set_vp(1, 2))
print(invs_alone_predictions, vp = set_vp(1, 3))
print(ltc_alone_predictions, vp = set_vp(1, 4))
print(all_mean_predictions, vp = set_vp(1, 5))
#
grid.text(
    "a)", vp = viewport(layout.pos.row = 1, layout.pos.col = 2), 
    gp = gpar(fontsize = 16), vjust = -15, hjust = 9
    )
grid.text(
    "b)", vp = viewport(layout.pos.row = 1, layout.pos.col = 3), 
    gp = gpar(fontsize = 16), vjust = -15, hjust = 10
    )
grid.text(
    "c)", vp = viewport(layout.pos.row = 1, layout.pos.col = 4), 
    gp = gpar(fontsize = 16), vjust = -15, hjust = 10
    )
grid.text(
    "d)", vp = viewport(layout.pos.row = 1, layout.pos.col = 5), 
    gp = gpar(fontsize = 16), vjust = -15, hjust = 10
    )
grid.text(
    expression('Predicted log response ratio'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 16), 
    vjust = 1, hjust = 0.38
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:5),
    gp = gpar(fontsize = 16), hjust = 0.25, vjust = 1
    )
dev.copy2pdf(file = '../figures/figure_5.pdf')

dev.new(height = 4, width = 8.5)
taxa_results %>% 
  mutate(weighting = c(rep('variance weighted', 5), rep('unweighted', 6))) %>% 
  filter(weighting == 'variance weighted') %>% 
  mutate(taxa = factor(c('Mixed', 'Fish', 'Algae', 'Zooplankton', 'Inverts'), levels = c('Mixed', 'Algae', 'Inverts', 'Zooplankton', 'Fish'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point(size = 3) + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0, size = 0.8) + 
  theme_bw() +
  xlab('\nTaxonomic group') + 
  ylab('Duration coefficient estimate\n') + 
  ylim(c(-0.13, 0.13)) + 
  theme(axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16), 
        axis.text.x = element_text(size = 16), 
        strip.text = element_text(size = 16))

dev.new(height = 4, width = 11.25)

taxa_results_imps %>% 
  mutate(driver = gsub('Duration', 'Duration', .$driver)) %>% 
  mutate(driver = gsub('mean_imps', 'CHI', .$driver)) %>% 
  mutate(driver = gsub('Duration:mean_imps', 'Duration:CHI', .$driver)) %>% 
  rename(parameter = driver) %>%
  mutate(taxa = factor(c('Algae', 'Algae', 'Algae', 'Inverts', 'Inverts', 'Inverts', 
                         'Fish', 'Fish', 'Fish', 'Mixed', 'Mixed', 'Mixed'), 
                        levels = c('Mixed', 'Algae', 'Inverts', 'Fish'))) %>% 
  mutate(driver = factor(parameter, levels = c('Duration', 'CHI', 'Duration:CHI'))) %>% 
ggplot(data = , aes(x = taxa, y = estimate)) + 
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed') + 
  geom_point() + 
  geom_errorbar(aes(x = taxa, ymin = ci_lb, ymax = ci_ub), width = 0) + 
  theme_minimal() +
  xlab('\nTaxa') + 
  ylab('Duration coefficient estimate\n') + 
  ylim(c(-1, 1)) + 
  facet_wrap(~ driver, scales = 'free_x') + 
  theme(axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16), 
        axis.text.x = element_text(size = 16), 
        strip.text = element_text(size = 16))



taxa_results_vw %>% 
  mutate(ci_lb_p = get_percent_change(ci_lb), 
         ci_ub_p = get_percent_change(ci_ub)) %>% 
  dplyr::select(taxa, studies, sites, max_duration, `percent change`, pval, ci_lb_p, ci_ub_p, sig_stars) %>% 
  rename(`lower ci` = ci_lb_p, `upper ci` = ci_ub_p, sig = sig_stars, `max duration` = max_duration) %>% 
  readr::write_csv('../figures/table2.csv')



taxa_results_imps2 <- taxa_results_imps

taxa_results_imps2[c(2, 3, 5, 6, 8, 9, 11, 12), c('taxa', 'studies', 'sites', 'max_duration')] <- '' 

taxa_results_imps2 %>% 
  mutate(ci_lb_p = get_percent_change(ci_lb), 
         ci_ub_p = get_percent_change(ci_ub)) %>% 
  mutate(driver = gsub('Duration', 'Dur', .$driver)) %>% 
  mutate(driver = gsub('mean_imps', 'CHI', .$driver)) %>% 
  mutate(driver = gsub('Duration:mean_imps', 'Dur:CHI', .$driver)) %>% 
  rename(parameter = driver) %>% 
  dplyr::select(taxa, studies, sites, max_duration, parameter, `percent change`, pval, ci_lb_p, ci_ub_p, sig_stars) %>% 
  rename(`lower ci` = ci_lb_p, `upper ci` = ci_ub_p, sig = sig_stars, `max duration` = max_duration) %>% 
  readr::write_csv('../figures/table3.csv')



papers <- readr::read_csv('../master_data/Papers.csv')

studies <- no_event %>% filter(!is.na(vi_SppR_ROM)) %>% distinct(Study.ID) %>% 
  mutate(Study.ID = as.numeric(as.character(Study.ID)))

filter(papers, `Study ID` %in% studies$Study.ID) %>% readr::write_csv('../figures/references.csv')


### SUPPLEMENTARY FIGURES

# IMPACTS JACKKNIFE RESULTS
dev.new(height = 10, width = 17)
# less cluttered plot
blank <- grid.rect(gp=gpar(col="white"))
grid.arrange(impacts_duration_leave1out_plot + 
               theme(axis.text.x = element_text(size = 14), 
                     axis.title.x = element_text(size = 20), 
                     axis.text.y = element_text(size = 16), 
                     axis.title.y = element_text(size = 20), 
                     plot.title = element_text(size = 16)), 
             blank,
             impacts_leave1out_plot + 
               theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
               theme(axis.text.x = element_text(size = 14), 
                     axis.title.x = element_text(size = 20), 
                     plot.title = element_text(size = 16)), 
             blank,
             duration_impacts_leave1out_plot + 
               theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
               theme(axis.text.x = element_text(size = 14), 
                     axis.title.x = element_text(size = 20), 
                     plot.title = element_text(size = 16)),
             blank, 
             ncol = 6, widths = c(10, 0.5, 5, 0.5, 5, 0.1))

dev.new(height = 9, width = 12)
# IMPACTS JACKKNIFE RESULTS
grid.arrange(
  drivers_nutrients_leave1out_plot + 
  theme(axis.text.x = element_text(size = 14), 
                     axis.title.x = element_text(size = 20), 
                     axis.text.y = element_text(size = 16), 
                     axis.title.y = element_text(size = 20), 
                     plot.title = element_text(size = 16)), 
  blank, 
  drivers_duration_nutrients_leave1out_plot + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  theme(axis.text.x = element_text(size = 14), 
         axis.title.x = element_text(size = 20), 
         plot.title = element_text(size = 16)),
  ncol = 3, widths = c(10, 0.5, 5))

## @knitr drivers-leave-1-out-study-invasives-short-long-plot
dev.new(height = 9, width = 12)
# IMPACTS JACKKNIFE RESULTS
grid.arrange(
  drivers_invasives_leave1out_plot + 
  theme(axis.text.x = element_text(size = 14), 
                     axis.title.x = element_text(size = 20), 
                     axis.text.y = element_text(size = 16), 
                     axis.title.y = element_text(size = 20), 
                     plot.title = element_text(size = 16)), 
  blank, 
  drivers_duration_invasives_leave1out_plot + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  theme(axis.text.x = element_text(size = 14), 
         axis.title.x = element_text(size = 20), 
         plot.title = element_text(size = 16)),
  ncol = 4, widths = c(10, 0.5, 5, 0.3))


## @knitr drivers-leave-1-out-study-LTC-short-long-plot
## @knitr drivers-leave-1-out-study-invasives-short-long-plot
dev.new(height = 9, width = 12)
# IMPACTS JACKKNIFE RESULTS
grid.arrange(
  drivers_LTC_leave1out_plot + 
  theme(axis.text.x = element_text(size = 14), 
                     axis.title.x = element_text(size = 20), 
                     axis.text.y = element_text(size = 16), 
                     axis.title.y = element_text(size = 20), 
                     plot.title = element_text(size = 16)), 
  blank, 
  drivers_duration_LTC_leave1out_plot + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  theme(axis.text.x = element_text(size = 14), 
         axis.title.x = element_text(size = 20), 
         plot.title = element_text(size = 16)),
  ncol = 4, widths = c(10, 0.5, 5, 0.3))

