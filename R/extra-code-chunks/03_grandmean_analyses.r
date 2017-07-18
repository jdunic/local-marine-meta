# Analysis Index
# 1) Weighted grand mean for the first - last data with an event #
# 2) Weighted estimates for the first - last data with an event using expected 
    # direction as a moderator #
# 3) 

# rma - fit a meta-analytic mixed-effects models with or without moderators via
# a linear mixed-effects model. 
# - the default for rma() is to fit a mixed-effects model with between-study 
# variance modelled as a random effect
# - if moderators are included, the residual variance between-studies is 
# modelled as a random effect

###################
# Weighted analysis
###################

# 1) Calculate grand mean for the first - last data with an event #

# Fitting grand mean model for event - incorporating site, nested within study 
# as random effects
gm_event <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_event, 
                   random = ~ factor(Site) | factor(Study.ID))

# Fitting grand mean model for no event - incorporating site, nested within
# study as random effects
gm_noevent <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_noevent, 
                   random = ~ factor(Site) | factor(Study.ID))

# Fitting grand mean model for combined event and no event - incorporating site, 
# nested within study as random effects
gm_combined <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, 
                   random = ~ factor(Site) | factor(Study.ID), 
                   data = rbind(fl_event, fl_noevent))

fl_noevent_clean_var <- fl_noevent[which(!is.na(fl_noevent$vi.SppR.ROM)), ]
gm_noevent_cl_var <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_noevent_clean_var, 
                   random = ~ factor(Site) | factor(Study.ID))

# rma object summary dataframes for event, no event, and combined data
gm_event_summ_df <- 
  mk_summary_df(fl_event, rma_object = gm_event, moderators = FALSE)
gm_noevent_summ_df <- 
  mk_summary_df(fl_noevent, rma_object = gm_noevent, moderators = FALSE)
gm_combined_summ_df <- 
  mk_summary_df(rbind(fl_event, fl_noevent), rma_object = gm_combined, moderators = FALSE)

gm_summ_df <- rbind(gm_event_summ_df, gm_noevent_summ_df, gm_combined_summ_df)
row.names(gm_summ_df) <- c('Event', 'No Event', 'All Data')

ggplot(data = gm_summ_df) +
  geom_point(aes(x = row.names(gm_summ_df), y = mean_estimate), size = 4) +
  geom_errorbar(aes(y = mean_estimate, ymin = lower_ci, ymax = upper_ci, 
                    x = row.names(gm_summ_df), width = 0)) +
  theme_bw() +
  xlab('Mean Effect Size') +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size =14), 
        axis.title.y = element_text(hjust = -0.5)) +
  geom_hline(x = 0, linetype = "dashed", colour = 'grey') +
  ylim(c(-0.3, 0.3)) +
  scale_x_discrete(labels = paste(row.names(gm_summ_df), "\nstudies = ", as.character(gm_summ_df$studies_per_mod), "\n sites = ", as.character(gm_summ_df$sites_per_mod)))

dev.copy2pdf(device = quartz, file = "Plots/effect_size_event_noevent_all.pdf")

# 2) Calculate grand mean for the first - last data with an event #
# Using expected direction as a moderator - weighted
# Note: study 365 is missing an event type
# effect size = event type + site | study

event_dir <- rma.mv(yi = yi.SppR.ROM ~ factor(Expected.Change.Direction) - 1, 
                    V = vi.SppR.ROM, 
                    data = fl_event[which(!is.na(fl_event$Expected.Change.Direction)), ], 
                    mods = ~ Expected.Change.Direction,
                    random = ~ factor(Site) | factor(Study.ID))

event_dir_summ_df <- 
  mk_summary_df(fl_event[which(!is.na(fl_event$Expected.Change.Direction)), ], 
                mods = "Expected.Change.Direction", 
                rma_object = event_dir, moderators = TRUE)
row.names(event_dir_summ_df) <- c('Negative', 'Positive', 'Uncertain')


ggplot(data = event_dir_summ_df) +
  geom_point(aes(x = row.names(event_dir_summ_df), y = mean_estimate), size = 4) +
  geom_errorbar(aes(y = mean_estimate, ymin = lower_ci, ymax = upper_ci, 
                    x = row.names(event_dir_summ_df), width = 0)) +
  theme_bw() +
  ylab('Mean Effect Size') +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(vjust = 0.8)) +
  geom_hline(x = 0, linetype = "dashed", colour = 'grey') +
  ylim(c(-0.7, 0.7)) +
  scale_x_discrete(labels = paste(row.names(event_dir_summ_df), "\nstudies = ", as.character(event_dir_summ_df$studies_per_mod), "\n sites = ", as.character(event_dir_summ_df$sites_per_mod)))

dev.copy2pdf(device = quartz, file = "Plots/effect_size_by_event.pdf")

sci_comm <- rbind(gm_summ_df[2, ], event_dir_summ_df[, 2:8])
sci_comm

ggplot(data = sci_comm) +
  geom_point(aes(x = row.names(sci_comm), y = mean_estimate), size = 4, colour = 'white') +
  geom_errorbar(aes(y = mean_estimate, ymin = lower_ci, ymax = upper_ci, 
                    x = row.names(sci_comm), width = 0), colour = 'white') +
  theme(panel.background = element_rect(fill = 'black'),
        plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "white")) +
  ylab('Mean Effect Size') +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 16, colour = 'white'), 
        axis.text.y = element_text(size = 16, colour = 'white'), 
        axis.title.y = element_text(vjust = 0.8, , colour = 'white', size = 16)) +
  geom_hline(x = 0, linetype = "dashed", colour = 'grey') +
  ylim(c(-0.7, 0.7)) +
  scale_x_discrete(labels = paste(row.names(sci_comm), "\nstudies = ", as.character(sci_comm$studies_per_mod), "\n sites = ", as.character(sci_comm$sites_per_mod)), aes(colour = 'white', size  = 16))

# Weighted analysis for 

# Effect of initial richness
init_rich_event_with_int <- rma.mv(yi = yi.SppR.ROM ~ SppR1, 
                          V = vi.SppR.ROM, 
                          data = fl_event, 
                          random = ~ factor(Study.ID)
                          )
init_rich_event_pred <- predict(init_rich_event_with_int)


# Need to remove studies that only have Shannon, and not richness
# effect size = species richness + study

rich_cleaned_fl_noevent <- fl_noevent[which(!is.na(fl_noevent$yi.SppR.SMD)), ]

init_rich_noevent_with_int <- rma.mv(yi = yi.SppR.ROM ~ SppR1, 
                            V = vi.SppR.ROM, 
                            data = rich_cleaned_fl_noevent, 
                            random = ~ factor(Study.ID)
                            )
init_rich_noevent_pred <- predict(init_rich_noevent_with_int)

# Plot predicted mixed model fit with confidence intervals
min_rich_index_event <- which(fl_event$SppR1 == min(fl_event$SppR1))
max_rich_index_event <- which(fl_event$SppR1 == max(fl_event$SppR1))
event_init_rich_plot <- 
    ggplot(data = fl_event, aes(x = SppR1, y = yi.SppR.ROM, 
                     colour = factor(Expected.Change.Direction))) +
      #geom_point(aes(x = SppR1, y = yi.SppR.ROM, colour = factor(Study.ID))) +
      geom_point() +
      theme_bw() +
      geom_hline(y = 0, colour = 'grey', linetype = 'dashed') + 
      ylab('Effect Size') +
      xlab('Initial Richness') +
      geom_ribbon(aes(x = SppR1, ymin = init_rich_event_pred[['ci.lb']], 
                      ymax = init_rich_event_pred[['ci.ub']]), 
                  alpha = 0.4, fill = 'grey', colour = NA
                  ) +
      geom_segment(aes(x = min(fl_event$SppR1), xend = max(fl_event$SppR1), 
                       y = init_rich_event_pred[['pred']][min_rich_index_event], 
                       yend = init_rich_event_pred[['pred']][max_rich_index_event]), 
                   colour = 'black') +
      #theme(legend.position="none") +
      title('Event')
event_init_rich_plot

dev.copy2pdf(device = quartz, file = "Plots/effect_size_by_initial_rich_event_direction_coloured.pdf")

min_rich_index_noevent <- which(rich_cleaned_fl_noevent$SppR1 == min(rich_cleaned_fl_noevent$SppR1))
max_rich_index_noevent <- which(rich_cleaned_fl_noevent$SppR1 == max(rich_cleaned_fl_noevent$SppR1))
noevent_init_rich_plot <- 
    ggplot(data = rich_cleaned_fl_noevent) +
      geom_point(aes(x = SppR1, y = yi.SppR.ROM, colour = factor(Study.ID))) +
      theme_bw() +
      geom_hline(y = 0, colour = 'grey', linetype = 'dashed') + 
      ylab('Effect Size') +
      xlab('Initial Richness') +
      geom_ribbon(aes(x = SppR1, ymin = init_rich_noevent_pred[['ci.lb']], 
                      ymax = init_rich_noevent_pred[['ci.ub']]), 
                  alpha = 0.4, fill = 'grey'
                  ) +
      geom_segment(aes(x = min(rich_cleaned_fl_noevent$SppR1), 
                       xend = max(rich_cleaned_fl_noevent$SppR1), 
                       y = init_rich_noevent_pred[['pred']][min_rich_index_noevent], 
                       yend = init_rich_noevent_pred[['pred']][max_rich_index_noevent]), 
                   colour = 'black') +
      theme(legend.position="none") +
      title('No Event')

grid.arrange(noevent_init_rich_plot, event_init_rich_plot, ncol = 2)

dev.copy2pdf(device = quartz, file = "Plots/effect_size_by_initial_rich.pdf")



# Weighted analysis
# Calculating the predicted values for positive, negative, uncertain event 
# directions in a meta-analytical framework, regressed against initial richness
event_pos <- rma.mv(yi = yi.SppR.ROM ~ SppR1 - 1, 
                    V = vi.SppR.ROM, 
                    data = fl_event[which(fl_event$Expected.Change.Direction == 'Positive'), ], 
                    random = ~ factor(Site) | factor(Study.ID))

event_neg <- rma.mv(yi = yi.SppR.ROM ~ SppR1 - 1, 
                    V = vi.SppR.ROM, 
                    data = fl_event[which(fl_event$Expected.Change.Direction == 'Negative'), ], 
                    random = ~ factor(Site) | factor(Study.ID))

event_uncer <- rma.mv(yi = yi.SppR.ROM ~ SppR1 - 1, 
                    V = vi.SppR.ROM, 
                    data = fl_event[which(fl_event$Expected.Change.Direction == 'Uncertain'), ], 
                    random = ~ factor(Site) | factor(Study.ID))


event_pos_summ <- 
    mk_summary_df(fl_event, rma_object = event_pos, moderators = FALSE)
event_neg_summ <- 
    mk_summary_df(fl_event, rma_object = event_neg, moderators = FALSE)
event_uncer_summ <- 
    mk_summary_df(fl_event, rma_object = event_uncer, moderators = FALSE)

event_dir_init_summ_df <- 
    rbind(event_pos_summ, event_neg_summ, event_uncer_summ)
row.names(event_dir_init_summ_df) <- c('Positive', 'Negative', 'Uncertain')


ggplot(data = event_dir_summ_df) +
  geom_point(aes(x = row.names(event_dir_summ_df), y = mean_estimate), size = 4) +
  geom_errorbar(aes(y = mean_estimate, ymin = lower_ci, ymax = upper_ci, 
                    x = row.names(event_dir_summ_df), width = 0)) +
  theme_bw() +
  ylab('Mean Effect Size') +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(vjust = 0.8)) +
  geom_hline(x = 0, linetype = "dashed", colour = 'grey') +
  ylim(c(-0.7, 0.7)) +
  scale_x_discrete(labels = paste(row.names(event_dir_summ_df), "\nstudies = ", as.character(event_dir_summ_df$studies_per_mod), "\n sites = ", as.character(event_dir_summ_df$sites_per_mod)))



# 3) Get the predicted values for lines fitted to studies based on expected
    # change direction
init_rich_event_by_dir <- rma.mv(yi = yi.SppR.ROM ~ SppR1, 
                    V = vi.SppR.ROM, 
                    data = fl_event[which(!is.na(fl_event$Expected.Change.Direction)), ], 
                    mods = ~ Expected.Change.Direction,
                    random = ~ factor(Site) | factor(Study.ID))



# Weighted analysis for 

# Effect of duration
init_dur_event_with_int <- rma.mv(yi = yi.SppR.ROM ~ Duration, 
                          V = vi.SppR.ROM, 
                          data = fl_event, 
                          random = ~ factor(Study.ID)
                          )
init_dur_event_pred <- predict(init_dur_event_with_int)


init_dur_noevent_with_int <- rma.mv(yi = yi.SppR.ROM ~ Duration, 
                            V = vi.SppR.ROM, 
                            data = fl_noevent, 
                            random = ~ factor(Study.ID)
                            )
init_dur_noevent_pred <- predict(init_dur_noevent_with_int)

# Plot predicted mixed model fit with confidence intervals
min_dur_index_event <- unique(which(fl_event$Duration == min(fl_event$Duration)))
max_dur_index_event <- unique(which(fl_event$Duration == max(fl_event$Duration)))
event_init_dur_plot <- 
    ggplot(data = fl_event) +
      geom_point(aes(x = Duration, y = yi.SppR.ROM, colour = factor(Study.ID))) +
      theme_bw() +
      geom_hline(y = 0, colour = 'grey', linetype = 'dashed') + 
      ylab('Effect Size') +
      xlab('Duration') +
      geom_ribbon(aes(x = Duration, ymin = init_dur_event_pred[['ci.lb']], 
                      ymax = init_dur_event_pred[['ci.ub']]), 
                  alpha = 0.4, fill = 'grey'
                  ) +
      geom_segment(aes(x = unique(min(fl_event$Duration)), 
                       xend = unique(max(fl_event$Duration)), 
                       y = unique(init_dur_event_pred[['pred']][min_dur_index_event]), 
                       yend = unique(init_dur_event_pred[['pred']][max_dur_index_event])),
                    colour = 'black' 
                   ) +
      theme(legend.position="none") +
      title('Event')

min_dur_index_noevent <- which(fl_noevent$Duration == min(fl_noevent$Duration))[1]
max_dur_index_noevent <- which(fl_noevent$Duration == max(fl_noevent$Duration))[1]
noevent_init_dur_plot <- 
    ggplot(data = fl_noevent) +
      geom_point(aes(x = Duration, y = yi.SppR.ROM, colour = factor(Study.ID))) +
      theme_bw() +
      geom_hline(y = 0, colour = 'grey', linetype = 'dashed') + 
      ylab('Effect Size') +
      xlab('Duration') +
      geom_ribbon(aes(x = Duration, ymin = init_dur_noevent_pred[['ci.lb']], 
                      ymax = init_dur_noevent_pred[['ci.ub']]), 
                  alpha = 0.4, fill = 'grey'
                  ) +
      geom_segment(aes(x = min(fl_noevent$Duration), 
                       xend = max(fl_noevent$Duration), 
                       y = unique(init_dur_noevent_pred[['pred']][min_dur_index_noevent]), 
                       yend = unique(init_dur_noevent_pred[['pred']][max_dur_index_noevent])), 
                   colour = 'black') +
      theme(legend.position="none") +
      title('No Event')

grid.arrange(noevent_init_dur_plot, event_init_dur_plot, ncol = 2)

dev.copy2pdf(device = quartz, file = "Plots/effect_size_by_duration.pdf")


fl_noevent[, c('PltSz', 'PltSz..units.')]






ggplot(data = fl_event) + 
  geom_point(aes(x = SppR1, y = test[['pred']])) +
  geom_abline(data = init_rich_event_summ, intercept = mean_estimate, )

test <- predict(init_rich_event)

ggplot() +
  geom_point()
  geom_abline(slope = init_rich_event_with_int$b[2, ], 
              intercept = init_rich_event_with_int$b[2, ])

  # Create a new data frame where columns are added for fitted values and limits of a 95% prediction interval
# Fitted values column name fit, limits are lwr and upr.

d = data.frame(RestaurantTips, predict(fit, interval="prediction"))
# Add another column for the residuals

d$residuals = residuals(fit)

# Simple plot of the data with a fitted regression line
p1 = ggplot(d,aes(x=Bill,y=Tip)) + geom_point() + geom_smooth(method="lm", se=FALSE)

# Fancier plot with a shaded 95% confidence interval

p2 = ggplot(d,aes(x=Bill,y=Tip)) + geom_point() + geom_smooth(method="lm")

# Even fancier plot with both confidence and prediction intervals

p3 = ggplot(d,aes(x=Bill,y=Tip)) +
    geom_ribbon(aes(ymin=lwr,ymax=upr,fill='prediction'),alpha=0.3) +
    geom_smooth(method="lm",aes(fill='confidence'),alpha=0.3) +
    geom_smooth(method="lm",se=FALSE,color='blue') +
    geom_point() +
    scale_fill_manual('Interval', values = c('green', 'yellow')) +
    ylab('Tip (dollars)') +
    xlab('Bill (dollars)')


# Effect of duration
me_dur_event <- rma.mv(yi = yi.SppR.ROM ~ Duration - 1, 
                    V = vi.SppR.ROM, 
                    data = fl_event, 
                    random = ~ factor(Site) | factor(Study.ID))

ggplot(data = fl_combined) +
  geom_point(aes(x = Duration, y = yi.SppR.ROM)) +
  theme_bw() +
  geom_hline(y = 0, colour = 'grey', linetype = 'dashed') + 
  ylab('Duration') +
  xlab('Effect Size') +
  facet_wrap(~ Event.) 


inf_gm_event <- influence(gm_event)
plot(inf_gm_event, plotdfb = TRUE)
# Study.ID == 3 has a high influence



R> res <- rma(yi, vi, mods = cbind(ablat, year), data = dat)
R> inf <- influence(res)
R> inf


influence((gm_event, digits=model$digits, ...))

leave1out(gm_event)



## Forest plots
# event forest - grand mean
forest(gm_event, cex = 0.75)
op <- par(cex = 0.75, font = 2)
text(-16, 15, "Site", pos = 4)
par(op)

forest(gm_noevent, cex = 0.75)
op <- par(cex = 0.75, font = 2)
text(-16, 15, "Study", pos = 4)
par(op)


forest(gm_combined, cex = 0.75)
op <- par(cex = 0.75, font = 2)
text(-16, 15, "Study", pos = 4)
par(op)


############ 
# EVENT data - moderator = event category: positive, negative, uncertain
############ 
gm_event_direction <- 
    rma(yi.SppR.ROM, vi.SppR.ROM, data = fl_event, method = "DL", 
        mods = ~ factor(Expected.Change.Direction))

gm_event_direction

leave1out(gm_event_direction)


forest(gm_event_direction, cex = 0.75)
op <- par(cex = 0.75, font = 2)
text(-16, 15, "Study", pos = 4)
par(op)


re_event <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, 
                   data = fl_event,
                   struct = "UN")



re_event <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, 
                   data = fl_event[!which(is.na(fl_event$vi.SppR.ROM)), ], 
                   struct = "UN")

re_noevent <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, 
                   data = fl_event[!which(is.na(fl_noevent$vi.SppR.ROM)), ], 
                   struct = "UN")


# Taxa with separated invertebrates - no events - weighted
LR_Taxa <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa) - 1, V = vi.SppR.ROM, 
                  data = taxa_NoEvent, random = ~ factor(Site) | factor(Study.ID))




forest(res, slab = paste(dat$author, dat$year, sep = ", "), 
  xlim = c(-16, 6), at = log(c(0.05, 0.25, 1, 4)), atransf = exp, 
  ilab = cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg), 
  ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.75)


fe_event <- rma(yi = yi.SppR.ROM, vi = vi.SppR.ROM, data = fl_event, 
                method = "DL", weighted = TRUE)

marine_mv <- rma.mv(LR ~ 1, VLR,
               data=marine,
               random= ~ 1 | Study, 
               struct="UN")




# Calculate grand mean for the first - last data with no event. 





str(fl_event)
length(unique(fl_event$Study.ID))
length(unique(fl_noevent$Study.ID))

length(unique(ts_event$Study.ID))
length(unique(ts_noevent$Study.ID))

# General data exploration:
# Non-independence: study level clustering:

ggplot(data = fl_event, aes(x = as.factor(Study.ID), y = yi.SppR.ROM, 
                            colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw() +
  geom_hline(y = 0)

ggplot(data = fl_noevent, aes(x = as.factor(Study.ID), y = yi.SppR.ROM, 
                            colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw() +
  geom_hline(y = 0)

colnames(ts_event)

dim(ts_event)

ggplot(data = ts_event, aes(x = T1, y = SppR, colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw()

ggplot(data = ts_noevent, aes(x = T1, y = SppR, colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw()