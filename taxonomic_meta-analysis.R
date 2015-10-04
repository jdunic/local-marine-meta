# Meta-analysis
setwd('Meta_analysis_2014')

# Analysis
library(metafor)
library(plyr)
library(nlme)

# Plotting
library(ggplot2)
library(gridExtra)

library(plyr)

# Loading data
event <- read.csv('Data/firstLastData_event_v0.3-20140518.csv')
no_event <- read.csv('Data/firstLastData_noevent_v0.3-20140518.csv')
time <- read.csv('Data/timeseriesData_noevent_v0.3-20140518.csv')

# Data cleanup
event[which(event$Event.type == "Multiple events, natural and artificial "), ]$Expected.Change.Direction <- "Negative"

Taxa2 <- NA
for (i in 1:length(event$Taxa)) {
    if (event$Taxa[i] %in% c("Mobile Inverts", "Sessile Inverts", "Mixed inverts")) {
        Taxa2[i] <- "Invertebrates"
    } else {
        Taxa2[i] <- as.character(event$Taxa[i])
    }
}
event$Taxa2 <- Taxa2

Taxa2 <- NA
for (i in 1:length(no_event$Taxa)) {
    if (no_event$Taxa[i] %in% c("Mobile Inverts", "Sessile Inverts", "Mixed inverts")) {
        Taxa2[i] <- "Invertebrates"
    } else {
        Taxa2[i] <- as.character(no_event$Taxa[i])
    }
}
no_event$Taxa2 <- Taxa2


event$line <- 1:length(event$Study.ID)
no_event$line <- 1:length(no_event$Study.ID)

# remove zero value
event <- event[-which(event$SppR1 == 0), ]
event <- event[-which(event$SppR2 == 0), ]

# remove zero value
no_event[which(is.na(no_event$yi.SppR.ROM)), ]
no_event <- no_event[-which(no_event$SppR2 == 0), ]
no_event <- no_event[-which(is.na(no_event$SppR1)), ]

# find how many studies we have with weights
length(no_event[-which(is.na(no_event$vi.SppR.ROM)), ])
length(event[-which(is.na(event$vi.SppR.ROM)), ])

# find how many studies we have without weights
length(no_event$Study.ID)
length(event$Study.ID)


# Fitting grand mean model for no_event
gm_mod_NoEvent <- rma(yi.SppR.ROM, vi.SppR.ROM, data = no_event)

# Fitting grand mean model for event
gm_mod_Event <- rma(yi.SppR.ROM, vi.SppR.ROM, data = event)


# Weighted regression of log ratios


baseplot <- ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_bw() +
  xlab("Taxa") +
  ylab("Log Ratio") +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 9), 
        axis.title.x = element_text(size = 10, vjust = -0.3), 
        axis.title.y = element_text(size = 10, vjust = 0.3),
        axis.text.y = element_text(size = 9))

#############
# No Events #
#############
ddply(no_event, .(Taxa), summarise, length(Taxa))
ddply(no_event, .(Taxa2), summarise, length(Taxa2))

# Dropping Mixed, Plants, and Zooplankton because there are two few values
taxa_NoEvent <- no_event[-which(no_event$Taxa %in% c('Mixed', 'Plants', 'Zooplankton')), ]
taxa2_NoEvent <- no_event[-which(no_event$Taxa2 %in% c('Mixed', 'Plants', 'Zooplankton')), ]

#------------------------------------------------------------------------------#
# Weighted analysis - no event
#------------------------------------------------------------------------------#

# Data frame with only points that will be used in the weighted analysis
taxa_NoEvent_weighted <- taxa_NoEvent[-which(is.na(taxa_NoEvent$vi.SppR.ROM)), ]
taxa2_NoEvent_weighted <- taxa2_NoEvent[-which(is.na(taxa2_NoEvent$vi.SppR.ROM)), ]

ddply(taxa_NoEvent_weighted, .(Taxa), summarise, length(Taxa))


# Taxa with separated invertebrates - no events - weighted
LR_Taxa <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa) - 1, V = vi.SppR.ROM, 
                  data = taxa_NoEvent, random = ~ factor(Site) | factor(Study.ID))
LR_Taxa_df <- mk_summary_df(taxa_NoEvent_weighted, taxa_NoEvent_weighted$Taxa, LR_Taxa)
LR_Taxa_df$moderator <- gsub("factor\\(Taxa\\)", "", as.character(LR_Taxa_df$moderator))
plot(residuals(LR_Taxa), main = "Weighted No Event")
abline(h = 0, col = "red")

LR_weighted <- baseplot + 
  geom_pointrange(data = LR_Taxa_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa_df$moderator), "\nk =", as.character(LR_Taxa_df$studies_per_mod), ", n =", as.character(LR_Taxa_df$sites_per_mod), sep = " ")) +
  labs(title = "Weighted") +
  theme(title = element_text(size = 10))

# Taxa with collapsed invertebrates - no events - weighted
# effect size = taxa + site | study
LR_Taxa2 <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa2) - 1, V = vi.SppR.ROM, 
                  data = taxa2_NoEvent, random = ~ factor(Site) | factor(Study.ID))
LR_Taxa2_df <- mk_summary_df(taxa2_NoEvent_weighted, taxa2_NoEvent_weighted$Taxa2, LR_Taxa2)
LR_Taxa2_df$moderator <- gsub("factor\\(Taxa2\\)", "", as.character(LR_Taxa2_df$moderator))
plot(residuals(LR_Taxa2), main = "Weighted No Event Combined Inverts")
abline(h = 0, col = "red")


LR_weighted2 <- baseplot + 
  geom_pointrange(data = LR_Taxa2_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa2_df$moderator), "\nk =", as.character(LR_Taxa2_df$studies_per_mod), ", n =", as.character(LR_Taxa2_df$sites_per_mod), sep = " ")) +
  labs(title = "Weighted") +
  theme(title = element_text(size = 10))

#------------------------------------------------------------------------------#
# Unweighted analysis - no event
#------------------------------------------------------------------------------#

LR_Taxa_UW_NoEvent <- lme(yi.SppR.ROM ~ Taxa - 1, random = ~ 1 | Study.ID/Site, data=taxa_NoEvent)
LR_Taxa_UW_df <- mk_lme_summary_df(taxa_NoEvent, taxa_NoEvent$Taxa, LR_Taxa_UW_NoEvent)
LR_Taxa_UW_df$moderator <- gsub("Taxa", "", as.character(LR_Taxa_UW_df$moderator))

plot(residuals(LR_Taxa_UW_NoEvent), main = "Unweighted No Event")
abline(h = 0, col = "red")

LR_unweighted <- baseplot + 
  geom_pointrange(data = LR_Taxa_UW_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa_UW_df$moderator), "\nk =", as.character(LR_Taxa_UW_df$studies_per_mod), ", n =", as.character(LR_Taxa_UW_df$sites_per_mod), sep = " ")) +
  labs(title = "Unweighted") +
  theme(title = element_text(size = 10))

LR_Taxa2_UW_NoEvent <- lme(yi.SppR.ROM ~ Taxa2 - 1, random = ~ 1 | Study.ID/Site, data=taxa2_NoEvent)
LR_Taxa2_UW_df <- mk_lme_summary_df(taxa2_NoEvent, taxa2_NoEvent$Taxa2, LR_Taxa2_UW_NoEvent)
LR_Taxa2_UW_df$moderator <- gsub("Taxa2", "", as.character(LR_Taxa2_UW_df$moderator))

plot(residuals(LR_Taxa2_UW_NoEvent), main = "Unweighted No Event")
abline(h = 0, col = "red")


LR_unweighted2 <- baseplot + 
  geom_pointrange(data = LR_Taxa2_UW_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa2_UW_df$moderator), "\nk =", as.character(LR_Taxa2_UW_df$studies_per_mod), ", n =", as.character(LR_Taxa2_UW_df$sites_per_mod), sep = " ")) +
  labs(title = "Unweighted") +
  theme(title = element_text(size = 10))

#------------------------------------------------------------------------------#
# No event taxa graphs
#------------------------------------------------------------------------------#
grid.arrange(LR_weighted, LR_unweighted, nrow = 2)

grid.arrange(LR_weighted2, LR_unweighted2, nrow = 2)

# Residuals
par(mfrow = c(2, 2))
plot(residuals(LR_Taxa), main = "Weighted No Event")
abline(h = 0, col = "red")
plot(residuals(LR_Taxa2), main = "Weighted No Event Combined Inverts")
abline(h = 0, col = "red")
plot(residuals(LR_Taxa_UW_NoEvent), main = "Unweighted No Event")
abline(h = 0, col = "red")
plot(residuals(LR_Taxa2_UW_NoEvent), main = "Unweighted No Event")
abline(h = 0, col = "red")


#------------------------------------------------------------------------------#
# Significance of moderators - unweighted - event
#------------------------------------------------------------------------------#
anova(LR_Taxa_UW_NoEvent)
     numDF denDF   F-value p-value
Taxa     6    25 0.2953747  0.9334

anova(LR_Taxa2_UW_NoEvent)
      numDF denDF    F-value p-value
Taxa2     4    27 0.09618056  0.9828

#------------------------------------------------------------------------------#
# Potentially shannon if time
#------------------------------------------------------------------------------#
ddply(no_event[which(is.na(no_event$vi.Shan.ROM) == FALSE), ], .(Taxa), summarise, length(Taxa))

LR_TaxaShan <- rma.mv(yi = yi.Shan.ROM ~ factor(Taxa) - 1, V = vi.Shan.ROM, 
                  data = taxa_NoEvent, random = ~ factor(Site) | factor(Study.ID))

LR_Taxa2Shan <- rma.mv(yi = yi.Shan.ROM ~ factor(Taxa2) - 1, V = vi.Shan.ROM, 
                  data = taxa_NoEvent, random = ~ factor(Site) | factor(Study.ID))


rma(yi.SppR.ROM~Lat, vi.SppR.ROM, data=event)


#############
#  Events  #
#############
ddply(event, .(Taxa), summarise, length(Taxa))
ddply(event, .(Taxa2), summarise, length(Taxa2))
# Dropping Mixed, Plants, and Zooplankton because there are two few values
taxa_event <- event[-which(event$Taxa %in% c("Coral", "Plants", "Sessile Inverts", "Algae")), ]
taxa2_event <- event[-which(event$Taxa2 %in% c("Coral", "Plants", "Algae")), ]

ddply(taxa_event, .(Taxa), summarise, length(Taxa))

# Data frame with only points that will be used in the weighted analysis
taxa_event_weighted <- taxa_event[-which(is.na(taxa_event$vi.SppR.ROM)), ]
taxa2_event_weighted <- taxa2_NoEvent[-which(is.na(taxa2_NoEvent$vi.SppR.ROM)), ]

# Taxa with separated invertebrates - no events - weighted
LR_Taxa_Event <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa) - 1, V = vi.SppR.ROM, 
                  data = taxa_event, random = ~ factor(Site) | factor(Study.ID))
LR_Taxa_Event_df <- mk_summary_df(taxa_event_weighted, taxa_event$Taxa, LR_Taxa_Event)
LR_Taxa_Event_df$moderator <- gsub("factor\\(Taxa\\)", "", as.character(LR_Taxa_Event_df$moderator))

LR_weighted_event <- baseplot + 
  geom_pointrange(data = LR_Taxa_Event_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa_Event_df$moderator), "\nk =", as.character(LR_Taxa_Event_df$studies_per_mod), ", n =", as.character(LR_Taxa_Event_df$sites_per_mod), sep = " ")) +
  labs(title = "Weighted") +
  theme(title = element_text(size = 10))

# Taxa with collapsed invertebrates - no events - weighted
LR_Taxa2_Event <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa2) - 1, V = vi.SppR.ROM, 
                  data = taxa2_event, random = ~ factor(Site) | factor(Study.ID))
LR_Taxa2_Event_df <- mk_summary_df(taxa2_event_weighted, taxa2_event$Taxa2, LR_Taxa2_Event)
LR_Taxa2_Event_df$moderator <- gsub("factor\\(Taxa2\\)", "", as.character(LR_Taxa2_Event_df$moderator))

LR_weighted2_event <- baseplot + 
  geom_pointrange(data = LR_Taxa2_Event_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa2_Event_df$moderator), "\nk =", as.character(LR_Taxa2_Event_df$studies_per_mod), ", n =", as.character(LR_Taxa2_Event_df$sites_per_mod), sep = " ")) +
  labs(title = "Weighted") +
  theme(title = element_text(size = 10))

#------------------------------------------------------------------------------#
# Weighted analysis - event - subset by expected direction
#------------------------------------------------------------------------------#
ddply(taxa_event, .(Taxa, Expected.Change.Direction), summarise, length(Expected.Change.Direction))

taxa_event_fewer <- taxa_event[-which(taxa_event$Taxa == 'Fish' & taxa_event$Expected.Change.Direction == 'Uncertain')]

# Taxa with separated invertebrates - no events - weighted
LR_Taxa_Event_Uncertain <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa) - 1, V = vi.SppR.ROM, 
                  data = taxa_event_fewer, random = ~ factor(Site) | factor(Study.ID), 
                  subset = Expected.Change.Direction == 'Uncertain')
LR_Taxa_Event_df <- mk_summary_df(taxa_event_weighted, taxa_event$Taxa, LR_Taxa_Event)
LR_Taxa_Event_df$moderator <- gsub("factor\\(Taxa\\)", "", as.character(LR_Taxa_Event_df$moderator))

LR_weighted_event <- baseplot + 
  geom_pointrange(data = LR_Taxa_Event_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa_Event_df$moderator), "\nk =", as.character(LR_Taxa_Event_df$studies_per_mod), ", n =", as.character(LR_Taxa_Event_df$sites_per_mod), sep = " ")) +
  labs(title = "Weighted") +
  theme(title = element_text(size = 10))

# Taxa with collapsed invertebrates - no events - weighted
LR_Taxa2_Event <- rma.mv(yi = yi.SppR.ROM ~ factor(Taxa2) - 1, V = vi.SppR.ROM, 
                  data = taxa2_event, random = ~ factor(Site) | factor(Study.ID))
LR_Taxa2_Event_df <- mk_summary_df(taxa2_event_weighted, taxa2_event$Taxa2, LR_Taxa2_Event)
LR_Taxa2_Event_df$moderator <- gsub("factor\\(Taxa2\\)", "", as.character(LR_Taxa2_Event_df$moderator))

LR_weighted2_event <- baseplot + 
  geom_pointrange(data = LR_Taxa2_Event_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa2_Event_df$moderator), "\nk =", as.character(LR_Taxa2_Event_df$studies_per_mod), ", n =", as.character(LR_Taxa2_Event_df$sites_per_mod), sep = " ")) +
  labs(title = "Weighted") +
  theme(title = element_text(size = 10))
#------------------------------------------------------------------------------#
# Unweighted analysis - event
#------------------------------------------------------------------------------#

LR_Taxa_UW_Event <- lme(yi.SppR.ROM ~ Taxa - 1, random = ~ 1 | Study.ID/Site, data=taxa_event)
LR_Taxa_UW_event_df <- mk_lme_summary_df(taxa_event, taxa_event$Taxa, LR_Taxa_UW_Event)
LR_Taxa_UW_event_df$moderator <- gsub("Taxa", "", as.character(LR_Taxa_UW_event_df$moderator))

LR_unweighted_event <- baseplot + 
  geom_pointrange(data = LR_Taxa_UW_event_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa_UW_event_df$moderator), "\nk =", as.character(LR_Taxa_UW_event_df$studies_per_mod), ", n =", as.character(LR_Taxa_UW_event_df$sites_per_mod), sep = " ")) +
  labs(title = "Unweighted") +
  theme(title = element_text(size = 10))

LR_Taxa2_UW_Event <- lme(yi.SppR.ROM ~ Taxa2 - 1, random = ~ 1 | Study.ID/Site, data=taxa2_event)
LR_Taxa2_UW_event_df <- mk_lme_summary_df(taxa2_event, taxa2_event$Taxa2, LR_Taxa2_UW_Event)
LR_Taxa2_UW_event_df$moderator <- gsub("Taxa2", "", as.character(LR_Taxa2_UW_event_df$moderator))

LR_unweighted2_event <- baseplot + 
  geom_pointrange(data = LR_Taxa2_UW_event_df, aes(x = moderator, y = mean_estimate, 
                                         ymin = lower_ci, ymax = upper_ci, 
                                         colour = moderator)) +
  scale_x_discrete(labels = paste(as.character(LR_Taxa2_UW_event_df$moderator), "\nk =", as.character(LR_Taxa2_UW_event_df$studies_per_mod), ", n =", as.character(LR_Taxa2_UW_event_df$sites_per_mod), sep = " ")) +
  labs(title = "Unweighted") +
  theme(title = element_text(size = 10))

#------------------------------------------------------------------------------#
# Event taxa graphs
#------------------------------------------------------------------------------#
grid.arrange(LR_weighted_event, LR_unweighted_event, nrow = 2)

grid.arrange(LR_weighted2_event, LR_unweighted2_event, nrow = 2)

# Residuals
par(mfrow = c(2, 2))
plot(residuals(LR_Taxa_Event), main = "Weighted Event")
abline(h = 0, col = "red")
plot(residuals(LR_Taxa2_Event), main = "Weighted Event Combined Inverts")
abline(h = 0, col = "red")
plot(residuals(LR_Taxa_UW_Event), main = "Unweighted Event")
abline(h = 0, col = "red")
plot(residuals(LR_Taxa2_UW_Event), main = "Unweighted Event")
abline(h = 0, col = "red")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Significance of moderators - unweighted - event
#------------------------------------------------------------------------------#
anova(LR_Taxa_UW_Event)
     numDF denDF  F-value p-value
Taxa     3     8 1.077795  0.4117

anova(LR_Taxa2_UW_Event)
      numDF denDF  F-value p-value
Taxa2     2    14 1.130962  0.3505


