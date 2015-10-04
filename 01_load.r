# Meta-analysis
# Open graphics device


# libraries
library(ggplot2)
library(metafor)
library(plyr)
#library(RCurl)
#library(grid)
#library(gridExtra)

setwd('Meta_analysis_ms')

# Data structure assumptions
# Study will be modeled as a random effect - each study is assumed to have a 
# different true mean. 
# Effect sizes/directions will differ depending on the type of event. 


fl_combined <- read.csv('Data/fullData_v0.4-20150602.csv')

# Create a unique id for every study/site pair in the data. This allows us to 
# use this value to nest sites within studies in random effects models. 
fl_combined$id <- as.factor(1:length(fl_combined$Study.ID))

# Additional data cleaning:

# First-last EVENT data:
# something is funky with study 365 - Hudson et al 2008. There is an event but 
# it does not say what that event was.

#fl_event <- fl_event[-which(fl_event$Event. == "Yes" & is.na(fl_event$Event.type)), ]

