library(dplyr)
library(ggplot2)
library(metafor)

source('02_functions.R')

data <- read.csv("Data/full_data_with_impacts_velocity_invasives20150603.csv")
# No need for the different taxonomic 1/0 categories:
data <- data[, setdiff(names(data), c('coral', 'plant', 'algae', 'fish', 
                      'inverts', 'mobile.inverts', 'sessile.inverts', 
                      'marine.mammals', 'phytoplankton', 'zooplankton'))]


# More clean up... grrr - this should be removed by the time we're done
# Remove study 47
data <- data[-which(data$vi.SppR.ROM == 0), ]

# Remove duplicate data
data <- data[-which(duplicated(data$id)), ]

event <- data[which(data$Event. == 'Yes'), ]
# Need to add a few event classifications - make sure fixed after next master 
# cleaning script gets run
event[c(40, 41, 56, 59, 60), 'Expected.Change.Direction'] <- c('Uncertain', 'Uncertain', 'Uncertain', 'Positive', 'Positive')

no_event <- data[which(data$Event. == 'No' | is.na(data$Event.)), ]



# Weighted analysis
mod1 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, 
       data = filter(data, !is.na(vi.SppR.ROM) & vi.SppR.ROM > 0),
       random = ~ 1 | Study.ID, 
       mods = ~ Duration)

mod2 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + 1)

mod3 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration - 1)

# Overall mean shows no trend.
# Need to double check how many values are ultimately used.

local1 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration:factor(Expected.Change.Direction) - 1)

local2 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + factor(Expected.Change.Direction) - 1)

local3 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + factor(Expected.Change.Direction) - 1)

local4 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + factor(Expected.Change.Direction) + 1)

local5 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration*factor(Expected.Change.Direction) - 1)

local6 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration*factor(Expected.Change.Direction) + 1)

# Including an intercept changes the outcome of the results where there is a 
# decline on average over time, but there is no relationship between event 
# categories and log ratio.
# Including duration as an interaction with expected change direction has no 
# effect on the direction of the response.

global1 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_imps + 1)

global2 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + raster_vel + 1)

global3 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_imps + raster_vel + 1)

global4 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration:mean_imps + Duration:raster_vel + 1)

global5 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_imps + raster_lin + 1)

global6 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_invs + raster_lin + 1)

global7 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_invs + raster_vel + 1)




global8 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_imps + 1)

global9 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + raster_vel + 1)

global10 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_imps + raster_vel + 1)

global11 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration:mean_imps + Duration:raster_vel + 1)

global12 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_imps + raster_lin + 1)

global13 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + mean_invs + raster_lin + 1)

global14 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
       random = ~ 1 | factor(Study.ID),
       mods   = ~ Duration + Duration*mean_invs + raster_vel + 1)


# Unweighted
lme1 <- lme(yi.SppR.ROM ~ Duration + 1, random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=data, na.action = na.exclude)
summary(lme1)

lme2 <- lme(yi.SppR.ROM ~ Duration - 1, random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=data, na.action = na.exclude)
summary(lme2)


local_lme1 <-
  lme(yi.SppR.ROM ~ Duration - 1 , random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=data, na.action = na.exclude)
summary(lme1)


rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration:factor(Expected.Change.Direction) - 1)

local_lme2 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + factor(Expected.Change.Direction) - 1)

local_lme3 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + factor(Expected.Change.Direction) - 1)

local_lme4 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + factor(Expected.Change.Direction) + 1)





res.lme <- lme(yi.SppR.ROM ~ Duration + 1, random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=no_event, na.action = na.exclude)
summary(res.lme)

event.lme <- lme(yi.SppR.ROM ~ Duration + Expected.Change.Direction - 1, random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=event, na.action = na.exclude)
summary(event.lme)


mod1 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID))

mod2 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + 1)

mod3 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration - 1)





# use a weighting that is based on sample size

# power analysis and compare with and without variance weight

# linear model - plain - is unweighted
# linear model - assign weight argument (will make it weighted) - relative to the maximum weighting
# variance weighted meta-analysis is different - because it's operating from the fact that you know the variance surrounding each study around the 

# sample size weighted 


# Result #1 
# Let's do it like everyone else
# 1.2% decrease per year - it's negative
# Naive analysis:
mod1 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + 1)

ggplot(data = data, aes(x = Duration, y = yi.SppR.ROM, colour = as.factor(Study.ID))) +
  geom_point(size = 3) + 
  ylab('Log Ratio') +
  #theme_wb() +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), 
        legend.position = 'none') + 
  #geom_abline(intercept = -0.0112, slope = -0.0084, colour = 'white', lwd = 1.5) +
  geom_hline(y = 0, colour = 'red', linetype = 'dashed')

# Result #2
#- lets look at the ecological 
#- oh look, this matches our expectations
#- event types model: 
#- how did we get these, expected change directions

mod2 <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration:factor(Expected.Change.Direction) - 1)

mod2_summ_df <- 
  mk_summary_df(event, rma_object = mod2, moderators = FALSE, 
                mods = 'Expected.Change.Direction')
row.names(mod2_summ_df) <- c(' Duration*Negative', ' Duration*Positive', ' Duration*Uncertain')

event_counts <- ddply(event[which(event$rich_ROM_w == 1), ], .(Expected.Change.Direction), summarise, 'studies' = length(unique(Study.ID)), 'sites' = length(unique(id)))
rownames(event_counts) <- c('Duration*Negative', 'Duration*Positive', 'Duration*Uncertain')


mod3 <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = no_event,
               random = ~ 1 | factor(Study.ID),
               mods = ~ Duration - 1)

mod3_summ_df <- 
  mk_summary_df(no_event, rma_object = mod3, moderators = FALSE)
studies <- length(unique(no_event[which(no_event$rich_ROM_w == 1), ]$Study.ID))
sites <- length(no_event[which(no_event$rich_ROM_w == 1), ]$id)
rownames(mod3_summ_df) <- ' Duration'

noevent_counts <- data.frame('studies' = studies, 'sites' = sites)
noevent_counts

x <- rbind(mod2_summ_df, mod3_summ_df)
x$studies_per_mod[1:3] <- as.vector(unlist(event_counts['studies']))
x$studies_per_mod[4] <- as.vector(unlist(noevent_counts['studies']))
x$sites_per_mod[1:3] <- as.vector(unlist(event_counts['sites']))
x$sites_per_mod[4] <- as.vector(unlist(noevent_counts['sites']))

all <- x
all$predictors <- factor(rownames(all), levels = c(' Duration', ' Duration*Uncertain', ' Duration*Positive', ' Duration*Negative'), ordered = TRUE)
all$predictors <- factor(all$predictors, levels=rev(levels(all$predictors)))

# plot
ggplot(data = all) +
  geom_point(aes(y = factor(predictors), x = mean_estimate), size = 4, colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = factor(predictors), height = 0), size = 1, colour = 'white') +
  theme_wb() + 
  xlab('Change in log ratio/year\n') +
  ylab('\nEvent Type\n') + 
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18, hjust = 0), 
        axis.title.y = element_text(vjust = 1.6, size = 20),
        axis.title.x = element_text(vjust = -0.5, size = 20), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  xlim(c(-0.12, 0.12)) +
  scale_y_discrete(labels = paste(all$predictors, "\n studies = ", as.character(all$studies), "\n sites = ", as.character(all$sites)))


# Unweighted

res.lme <- lme(yi.SppR.ROM ~ Duration + 1, random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=no_event, na.action = na.exclude)
summary(res.lme)

event.lme <- lme(yi.SppR.ROM ~ Duration + Expected.Change.Direction - 1, random = ~ 1 | factor(Study.ID), weights = NULL, control=lmeControl(sigma = 1), data=event, na.action = na.exclude)
summary(event.lme)




# Result #3
# In this model I include climate velocity - which I do not think should 
# interact with duration. In a model with absolute temperature change alone, I
# would include the effect of duration.

global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ mean_imps + Duration:mean_imps + Duration + SppR1 + Duration:raster_vel + 1)

global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ mean_imps + Duration + SppR1 + raster_vel + 1)


global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ scale(mean_imps) + scale(Duration):scale(mean_imps) + scale(Duration) + scale(SppR1) + scale(Duration):scale(raster_vel) + 1)


global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration:mean_imps + Duration + SppR1 + raster_lin + Duration:raster_lin + 1)



global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration:mean_imps + Duration + SppR1 + Duration:vel_decade + 1)

# Why not include a model where duration is not being included
velocity_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + vel_decade + 1)

imps_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | factor(Study.ID),
       mods = ~ Duration + mean_imps + 1)





global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = data,
       random = ~ 1 | Study.ID,
       mods = ~ Duration:mean_imps + Duration + SppR1 + Duration*temp_changes+1)


global_summ_df <- 
  mk_summary_df(data, rma_object = global_model, moderators = FALSE, 
                mods = c('mean_imp_vals', 'Duration', 'vel_decade'))
row.names(global_summ_df) <- c('Intercept', 'Human Impact', 'Duration', 
                               'Initial Richness', 'Climate Velocity')
global_summ_df$predictors <- factor(row.names(global_summ_df), levels = c('Intercept', 'Duration', 'Initial Richness', 'Climate Velocity', 'Human Impact'))

global_summ_df <- global_summ_df[-which(global_summ_df$predictors == 'Intercept'), ]

ggplot(data = global_summ_df) +
  geom_point(aes(x = mean_estimate, y = predictors), 
             size = 4, colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = predictors, height = 0), colour = 'white', 
                    size = 1) + 
  theme_wb() + 
  xlab('Mean Effect Size\n') +
  ylab('\nPredictor') + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(vjust = 1.5, size = 16), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  xlim(c(-0.07, 0.07))


# HCI map, linear change, and climate velocities
library(rasterVis)

my_theme <- rasterTheme(region = colorRampPalette(c("blue","white", "red")),
#alpha=0.6,
panel.background = list(col = 'black')
)

rasterTheme

col_theme <- colorRampPalette(c("blue","white", "red"))
col_theme$panel.background$col = 'black' 

levelplot(linear, col.regions = pal(101), at = seq(-0.3, 0.3, length.out = 101))