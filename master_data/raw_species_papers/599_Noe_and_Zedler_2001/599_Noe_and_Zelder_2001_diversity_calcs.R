library(tidyr)
library(dplyr)
library(vegan)


data <- read.csv('/Users/jillian/Desktop/Meta-analysis_papers/Papers to QC/599_Noe_and_Zelder_2001_Table1.csv', stringsAsFactors = FALSE, na.strings = '')

data <- select(data, -Species.classification)

data[is.na(data)] <- "9999 (9999)"


mean_sd_df <- lapply(names(data[, -1]), FUN = function(name = x) {
    #browser()
    mean_sd <- 
    data %>% 
      extract_(col = name, 
               into = c(paste(name, 'mean', sep = '_'), paste(name, 'sd', sep = '_')), 
               regex = "(\\d*)\\ \\((\\d*)\\)", convert = TRUE) %>% 
      select_(paste(name, 'mean', sep = '_'), paste(name, 'sd', sep = '_'))
    return(mean_sd)
})

mean_sd_df <- as.data.frame(mean_sd_df)

mean_sd_df[mean_sd_df == 9999] <- 0

mean_df <- select(mean_sd_df, contains('mean'))

# correct for density - go back to the number of individuals they actually 
# counted in their four 0.04 m^2 sub plots
mean_df <- mean_df / (1 / 0.16)

# abundance
as.data.frame(colSums(mean_df))

# richness
as.data.frame(specnumber(mean_df, MARGIN = 2))

# shannon
as.data.frame(diversity(mean_df, index = "shannon", MARGIN = 2))

# simpson
as.data.frame(diversity(mean_df, index = "simpson", MARGIN = 2))

# Pielou
as.data.frame(diversity(mean_df, index = "shannon", MARGIN = 2)/log(specnumber(mean_df, MARGIN = 2)))