library(raster)
library(ggplot2)
library(plyr)
library(metafor)

fl_event <- read.csv('Data/firstLastData_event_v0.4-20150302.csv')

fl_event[which(is.na(fl_event$Expected.Change.Direction) & 
                  fl_event$Event. == 'Yes'), 'Expected.Change.Direction'] <- 'Uncertain' 

fl_noevent <- read.csv('Data/firstLastData_noevent_v0.4-20150302.csv')
fl_noevent$Event. <- as.factor('No')

fl_combined <- rbind(fl_event, fl_noevent)

# Create a unique id for every study/site pair in the data. This allows us to 
# use this value to nest sites within studies in random effects models. 
fl_combined$id <- as.factor(1:length(fl_combined$Study.ID))
fl_combined$Duration = fl_combined$T2 - fl_combined$T1


#####################################################
#  Add cumulative human impact values to full data  #
#####################################################
imp_map_b1 <- raster("../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 1)
imp_map_b2 <- raster("../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 2)
imp_map_b3 <- raster("../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 3)

fl_combined2 <- fl_combined

coordinates(fl_combined2) <- c('Long', 'Lat')
projection(fl_combined2)

# Extracts the RGB values from the three data layers. 
# The 'land' test values return NA only. 
imp_stack <- stack(imp_map_b1, imp_map_b2, imp_map_b3)
impl <- extract(imp_stack, fl_combined2, buffer = 2000, small = T)

impl <- extract(imp_map, fl_combined2, buffer = 1000, small = T)

hex_imps <- llply(impl, function(x) get_imp_hex(x))

#991118 = red = 6 = very high
#E67D27 = dark orange = 5 = high
#F0B81F = dark yellow = 4 = medium high\
#F7EE3B = yellow = 3 = medium
#B2D698 = green = 2 = low
#8AB4EB = blue = 1 = very low

hexes <- c('#8AB4EB', '#B2D698', '#F7EE3B', '#F0B81F', '#E67D27', '#991118', NA)
imp_cat_val <- c(1, 2, 3, 4, 5, 6, NA)
imp_cat <- c('very low', 'low', 'medium', 'medium high', 'high', 'very high', NA)

imp_lookup_df <- data.frame('hex' = hexes, 'values' = imp_cat_val, 'category' = imp_cat)

imp_val_dfs <- llply(hex_imps, function(x) merge(x = x, y = imp_lookup_df, by.x = 1, by.y = 'hex'))

mean_imp_vals <- llply(imp_val_dfs, function(x) mean(x$values, na.rm = TRUE))

mean_imp_df <- data.frame('mean_imp_vals' = unlist(mean_imp_vals))

mean_imp_cats <- llply(imp_val_dfs, function(x) mean(x$))
mean_imp_cat_df <- data.frame('mean_imp_cats' = unlist(mean_imp_cats))

mean_imps <- 

fl_combined <- cbind(fl_combined, mean_imp_df)

#####################################################
#  Add temperature change values to full data  #
#####################################################

# I can't do this right now. Something is wrong...

climateChangeMats_1960_2011 <- getClimateChange(sstData, years = 1960:2011)

temp_changes <- 
ddply(fl_combined, .(id), function(x) 
      if (x$Lat == 53 & x$Long == 3 | 
          x$Lat == 60.1667 & x$Long == 6 |
          x$Lat == 35 & x$Long == 139 |
          x$Lat == 79 & x$Long == 12) {temp_change <- NA
      } else {
      temp_change <- getClimateLatLon(climateChangeMats_1935_2011, 
                                      lat = x$Lat, lon = x$Long, 'linearChange')
      return(temp_change)
  }
      )


###################################
#  Weighted analysis - by dataset #
###################################

event <- fl_combined[which(fl_combined$Event. == 'Yes'), ]
noevent <- fl_combined[which(fl_combined$Event. == 'No'), ]

event_dir <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | Study.ID,
       mods = ~ Duration:factor(Expected.Change.Direction) - 1)

rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined,
         random = ~ 1 | Study.ID,
         mods = ~ Duration:mean_imp_vals  + Duration + Duration:temp_changes)

rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined,
          random = ~ 1 | Study.ID,
          mods = ~ Duration:mean_imp_vals + Duration + SppR1 + Duration*temp_changes + 1)


# if temperature change and impact values are zero, the rate of change of species richness

# temperature change coefficient - if a temp change over a very short duration, 
# - in the presence of a 

# if you experience a temp change of 2 degrees, 

# temperature effect - short sharp shocks can have a negative effect - 
# when you include the interaction - range shift signal and longer tem processes that can compensate for 

# side note, year-velocity - see variation from year to year

# Here's the problem
# caused some debate
# the implications of no local scale loss is bad
# Dornelas - turnover --> ecological and human processes underlying all of this

# flaws - flaws in the datasets, ignored some biological covariates, biased

# Leads to hypotheses


# Result #1 
# Let's do it like everyone else
# 1.2% decrease per year - it's negative
# Naive analysis:
naive <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined,
       random = ~ 1 | Study.ID,
       mods = ~ Duration + 1)

ggplot(data = fl_combined, aes(x = Duration, y = yi.SppR.ROM, colour = as.factor(Study.ID))) +
  geom_point(size = 3) + 
  theme_wb() +
  ylab('Log Ratio') +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), 
        legend.position = 'none') + 
  #geom_abline(intercept = -0.0112, slope = -0.0084, colour = 'white', lwd = 1.5) +
  geom_hline(y = 0, colour = 'red', linetype = 'dashed')

ggplot(data = fl_combined, aes(x = Duration, y = yi.SppR.ROM)) +
  geom_point(size = 3) + 
  ylab('Log Ratio') +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18)) + 
  geom_abline(intercept = 0.1585, slope = -0.0136, lwd = 1.5) + 
  geom_hline(y = 0, colour = 'red')


All Data   -0.01152037

rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | Study.ID,
       mods = ~ Duration + 1)


ggplot(data = fl_event, aes(x = as.factor(Study.ID), y = yi.SppR.ROM, 
                            colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw() +
  geom_hline(y = 0) 


# plot x axis = duration, y axis = log ratio

# Result #2
#- lets look at the ecological 
#- oh look, this matches our expectations
#- event types model: 
#- how did we get these, expected change directions

event_dir <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event,
       random = ~ 1 | Study.ID,
       mods = ~ Duration:factor(Expected.Change.Direction)  - 1)


event_dir_summ_df <- 
  mk_summary_df(event, rma_object = event_dir, moderators = FALSE, 
                mods = 'Expected.Change.Direction')
row.names(event_dir_summ_df) <- c(' Duration*Negative', ' Duration*Positive', ' Duration*Uncertain')

ggplot(data = event_dir_summ_df) +
  geom_point(aes(y = row.names(event_dir_summ_df), x = mean_estimate), size = 4, 
             colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = row.names(event_dir_summ_df), height = 0), colour = 'white', 
                    size = 1) +
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
  xlim(c(-0.15, 0.15)) +
  scale_y_discrete(labels = paste(row.names(event_dir_summ_df), "\n studies = ", as.character(event_dir_summ_df$studies_per_mod), "\n sites = ", as.character(event_dir_summ_df$sites_per_mod)))


# Result #3
global_model <- 
rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined,
       random = ~ 1 | Study.ID,
       mods = ~ Duration:mean_imp_vals + Duration + SppR1 + Duration*temp_changes+1)

global_summ_df <- 
  mk_summary_df(fl_combined, rma_object = global_model, moderators = FALSE, 
                mods = c('mean_imp_vals', 'Duration', 'temp_changes'))
row.names(global_summ_df) <- c('Intercept', 'Duration', 'Initial Richness', 
                               'Temperature Change', 'Duration*Human Impact', 
                               'Duration*Temperature Change')
global_summ_df$predictors <- factor(row.names(global_summ_df), levels = c('Intercept', 'Duration', 'Initial Richness', 'Duration*Temperature Change', 'Temperature Change', 'Duration*Human Impact'))

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
  xlim(c(-8, 8))


ggplot(data = fl_combined) + 
  geom_point(aes(x = temp_changes, y = yi.SppR.ROM))



# Get grand mean for all data, no event data, and event data
gm_combined <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined, 
                     random = ~ 1 | factor(Study.ID), 
                     mods = ~ Duration - 1)

gm_noevent <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = noevent, 
                     random = ~ 1 | factor(Study.ID), 
                     mods = ~ Duration - 1)

gm_event <- rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event, 
                   random = ~ 1 | factor(Study.ID), 
                   mods = ~ Duration - 1)

# Get summary statistics for all data, no event data, and event data grand means
gm_event_summ_df <- 
  mk_summary_df(event, rma_object = gm_event, moderators = FALSE)
row.names(gm_event_summ_df) <- 'Event'
gm_noevent_summ_df <- 
  mk_summary_df(noevent, rma_object = gm_noevent, moderators = FALSE)
row.names(gm_noevent_summ_df) <- 'No Event'
gm_combined_summ_df <- 
  mk_summary_df(fl_combined, rma_object = gm_combined, moderators = FALSE)
row.names(gm_combined_summ_df) <- 'All Data'

gm_summ_df <- rbind(gm_event_summ_df, gm_noevent_summ_df, gm_combined_summ_df)
gm_summ_df$dataset <- factor(row.names(gm_summ_df), levels = c("Event", "No Event", "All Data"))

ggplot(data = gm_summ_df, aes(y = dataset, x = mean_estimate)) +
  geom_point(size = 4, colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = dataset, height = 0), colour = 'white', 
                    size = 1) +
  theme_wb() + 
  xlab('Mean Effect Size') +
  ylab('Dataset') + 
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14, hjust = 0), 
        axis.title.y = element_text(vjust = 1.7, size = 16),
        axis.title.x = element_text(vjust = -0.5, size = 16), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  xlim(c(-0.05, 0.05)) +
  scale_y_discrete(labels = paste(gm_summ_df$dataset, "\nstudies = ", as.character(gm_summ_df$studies_per_mod), "\nsites = ", as.character(gm_summ_df$sites_per_mod)))
  
dev.copy2pdf(device = quartz, file = "Plots/grand_mean_estimates.pdf")

#############################################################
# Event direction only model: ES = Expected.Change.Direction #
#############################################################
event_dir <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event, 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)), 
           mods = ~ factor(Expected.Change.Direction):Duration - 1)
event_dir

event_dir_summ_df <- 
  mk_summary_df(event, rma_object = event_dir, moderators = TRUE, mods = 'Expected.Change.Direction')
row.names(event_dir_summ_df) <- c('Negative', 'Positive', 'Uncertain')

ggplot(data = event_dir_summ_df) +
  geom_point(aes(y = row.names(event_dir_summ_df), x = mean_estimate), size = 4, 
             colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = row.names(event_dir_summ_df), height = 0), colour = 'white', 
                    size = 1) +
  theme_wb() + 
  xlab('Mean Effect Size') +
  ylab('Event Type') + 
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14, hjust = 0), 
        axis.title.y = element_text(vjust = 1.8, size = 16),
        axis.title.x = element_text(vjust = -0.5, size = 16), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  xlim(c(-0.11, 0.11)) +
  scale_y_discrete(labels = paste(row.names(event_dir_summ_df), "\nstudies = ", as.character(event_dir_summ_df$studies_per_mod), "\nsites = ", as.character(event_dir_summ_df$sites_per_mod)))
  
dev.copy2pdf(device = quartz, file = "Plots/event_dir_mean_estimates.pdf")

###########################################################################
# Full Model: ES = impacts + duration + temp_change + initial richness    #
#                  (+ expected.change.direction)                          #
###########################################################################

# Get grand mean for all data, no event data, and event data

fl_combined$temp_changes <- as.vector(temp_changes$V1)


rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined,
           random = ~ 1 | factor(Study.ID),
           mods = ~ Duration + mean_imp_vals:Duration + SppR1:Duration +
           factor(Expected.Change.Direction):Duration + temp_changes:Duration - 1)

combined_full <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined, 
           random = list(~ 1 | factor(Study.ID)), 
           mods = ~ mean_imp_vals:Duration + SppR1:Duration + 
           factor(Expected.Change.Direction):Duration + temp_changes:Duration + Duration)

event_full <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event, 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)),
           mods = ~ mean_imp_vals:Duration + SppR1:Duration + 
           factor(Expected.Change.Direction):Duration + temp_changes:Duration)

noevent_full <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = noevent, 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)),
           mods = ~ mean_imp_vals:Duration + SppR1:Duration + temp_changes:Duration - 1)


combined_full_uni <- 
    rma(yi = yi.SppR.ROM, vi = vi.SppR.ROM, data = fl_combined, 
        mods = ~ mean_imp_vals + Duration + SppR1 + Event. + 
        factor(Expected.Change.Direction) - 1)

# Get summary statistics for all data, no event data, and event data grand means
full_combined_summ_df <- 
  mk_summary_df(fl_combined, rma_object = combined_full, moderators = FALSE, 
                mods = c('SppR1', 'mean_imp_vals', 'Expected.Change.Direction', 'temp_changes'))

full_event_summ_df <- 
  mk_summary_df(event, rma_object = event_full, moderators = FALSE, 
                mods = c('mean_imp_vals', 'temp_changes', 'Duration', 'SppR1', 'Expected.Change.Direction'))

full_noevent_summ_df <- 
  mk_summary_df(noevent, rma_object = noevent_full, moderators = FALSE, 
                mods = c('mean_imp_vals', 'temp_changes', 'Duration', 'SppR1'))

#full_combined_uni_summ_df <- 
#  mk_summary_df(fl_combined, rma_object = combined_full_uni, moderators = FALSE, 
#                mods = c('mean_imp_vals', 'Duration', 'SppR1', 'Expected.Change.Direction'))

gm_simple_event <- rma(yi = yi.SppR.ROM, vi = vi.SppR.ROM, data = event)
inf <- influence(gm_simple_event)
plot(inf)


ggplot(data = full_combined_summ_df) +
  geom_point(aes(x = mean_estimate, y = row.names(full_combined_summ_df)), 
             size = 4, colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = row.names(full_combined_summ_df), height = 0), colour = 'white', 
                    size = 1) + 
  theme_wb() + 
  xlab('Mean Effect Size') +
  ylab('Predictor') + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(vjust = 1.5, size = 16), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  scale_y_discrete(labels = c("mean_imp_vals:Duration" = "Duration*Human impact",
                              "intrcpt" = "Duration*Negative event",
                              "Duration:factor(Expected.Change.Direction)Uncertain" = "Duration*Uncertain event",
                              "Duration:factor(Expected.Change.Direction)Positive" = "Duration*Positive event", 
                              "Duration:SppR1" = 'Duration*Initial richness', 
                              "Duration:temp_changes" = 'Duration*Temperature change'
                              )
  ) + 
  xlim(c(-4.5, 4.5))

dev.copy2pdf(device = quartz, file = "Plots/combined_full_model_mean_estimates.pdf")

ggplot(data = full_event_summ_df) +
  geom_point(aes(x = mean_estimate, y = row.names(full_event_summ_df)), 
             size = 4, colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = row.names(full_event_summ_df), height = 0), colour = 'white', 
                    size = 1) + 
  theme_wb() + 
  xlab('Mean Effect Size') +
  ylab('Predictor') + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(vjust = 1.5, size = 16), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  scale_y_discrete(labels = c("mean_imp_vals:Duration" = "Duration*Human impact",
                              "intrcpt" = "Duration*Negative event",
                              "Duration:factor(Expected.Change.Direction)Uncertain" = "Duration*Uncertain event",
                              "Duration:factor(Expected.Change.Direction)Positive" = "Duration*Positive event", 
                              "Duration:SppR1" = 'Duration*Initial richness', 
                              "Duration:temp_changes" = 'Duration*Temperature change'
                              )
  ) + 
  xlim(c(-4, 4))

dev.copy2pdf(device = quartz, file = "Plots/event_full_model_mean_estimates.pdf")

ggplot(data = full_noevent_summ_df) +
  geom_point(aes(x = mean_estimate, y = row.names(full_noevent_summ_df)), 
             size = 4, colour = 'white') +
  geom_errorbarh(aes(x = mean_estimate, xmin = lower_ci, xmax = upper_ci, 
                    y = row.names(full_noevent_summ_df), height = 0), colour = 'white', 
                    size = 1) + 
  theme_wb() + 
  xlab('Mean Effect Size') +
  ylab('Predictor') + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(vjust = 1.5, size = 16), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_vline(y = 0, linetype = "dashed", colour = 'yellow') +
  scale_y_discrete(labels = c("mean_imp_vals:Duration" = "Duration*Human impact",
                              "Duration:SppR1" = 'Duration*Initial richness', 
                              "Duration:temp_changes" = 'Duration*Temperature change'
                              )
  ) + 
  xlim(c(-0.7, 0.7))

dev.copy2pdf(device = quartz, file = "Plots/noevent_full_model_mean_estimates.pdf")


###########################################################################
                    # What about taxonomic groups? #
###########################################################################

combined_taxa <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined, 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)),
           mods = ~ factor(Taxa):Duration - 1)

event_taxa <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = event, 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)),
           mods = ~ factor(Taxa):Duration - 1)

noevent_taxa <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = noevent, 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)),
           mods = ~ factor(Taxa):Duration - 1)

taxa_list <- c('Coral', 'Fish', 'Mixed', 'Mixed inverts', 'Phytoplankton', 'Plants', 
               'Zooplankton'
               )

# Let's just present the combined taxa because as of right now (Mar 3, 2015) 
# there is no significant effect of taxa.
taxa_combined_summ_df <- 
  mk_summary_df(fl_combined, rma_object = combined_taxa, moderators = FALSE, 
                mods = 'Taxa')
row.names(taxa_combined_summ_df) <- taxa_list

taxa_list <- c('Coral', 'Fish', 'Mixed', 'Mixed inverts', 'Phytoplankton', 'Plants', 
               'Zooplankton'
               )

studies_per_mod <- 
  ddply(fl_combined[which(fl_combined$Taxa %in% taxa_list), ], .(Taxa), 
        summarise, studies = length(unique(Study.ID))
        )
sites_per_mod <- 
  ddply(fl_combined[which(fl_combined$Taxa %in% taxa_list), ], .(Taxa), 
        summarise, studies = length(id)
        )
taxa_combined_summ_df$studies_per_mod <- studies_per_mod[, 2]
taxa_combined_summ_df$sites_per_mod <- sites_per_mod[, 2]


ggplot(data = taxa_combined_summ_df) +
  geom_point(aes(x = row.names(taxa_combined_summ_df), y = mean_estimate, colour = row.names(taxa_combined_summ_df)), size = 4) +
  geom_errorbar(aes(y = mean_estimate, ymin = lower_ci, ymax = upper_ci, 
                    x = row.names(taxa_combined_summ_df), width = 0, colour = row.names(taxa_combined_summ_df)), 
                    size = 1) +
  theme_wb() + 
  ylab('Mean Effect Size') +
  xlab('Taxa') + 
  guides(colour = FALSE) +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16, hjust = 0), 
        axis.title.y = element_text(vjust = 1.7, size = 18),
        axis.title.x = element_text(vjust = -0.5, size = 18), 
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank()) +
  geom_hline(y = 0, linetype = "dashed", colour = 'white') +
  ylim(c(-0.1, 0.1)) +
  scale_x_discrete(labels = paste(row.names(taxa_combined_summ_df), "\nk = ", as.character(taxa_combined_summ_df$studies_per_mod), "\nn = ", as.character(taxa_combined_summ_df$sites_per_mod)))

dev.copy2pdf(device = quartz, file = "Plots/combined_taxa_model_mean_estimates.pdf")


#####
# Linearity of cumulative impact factors
#####
cat1 <- 
    rma.mv(yi = yi.SppR.ROM, V = vi.SppR.ROM, data = fl_combined[which(fl_combined$mean), 
           random = list(~ 1 | factor(Study.ID), ~ factor(Study.ID) | factor(id)),
           mods = ~ factor(Taxa):Duration - 1)



library(lattice)
pal <- colorRampPalette(c("blue","white", "red"))
latLonGrid <- expand.grid(lon = climateChangeMats_1935_2011$lon, lat = climateChangeMats_1935_2011$lat)

levelplot(climateChangeMats_1935_2011$linearChangeMat ~ lon * lat, 
  data = latLonGrid, col.regions = pal(101), at=seq(-0.5,0.5,length.out=101), 
  panel=function(...) {
              grid.rect(gp=gpar(col=NA, fill="black"))
              panel.grid(h = 0, v = 0)
              panel.levelplot(...)
           })