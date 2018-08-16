library(vegan)
library(dplyr)
library(tidyr)

# California - Table 7
data <- read.csv('~/Desktop/Meta-analysis_papers/Finished Papers/691_Barlow_and_Fornkey_2007/691_Barlow_and_Fornkey_2007_Table7.csv', 
    row.names = 1)

str(data)

# richness
colSums(data > 0)
# OR
specnumber(data, MARGIN = 2)

# shannon
diversity(data, index = "shannon", MARGIN = 2)

# simpson
diversity(data, index = "simpson", MARGIN = 2)

# Pielou
diversity(data, index = "shannon", MARGIN = 2)/log(specnumber(data, MARGIN = 2))

#-------------------------------------------------------------------------------
# Washington/Oregon - Table 8
data <- read.csv('~/Desktop/Meta-analysis_papers/Finished Papers/691_Barlow_and_Fornkey_2007/691_Barlow_and_Fornkey_2007_Table8.csv', 
    row.names = 1)

str(data)

# richness
colSums(data > 0)
# OR
specnumber(data, MARGIN = 2)

# shannon
diversity(data, index = "shannon", MARGIN = 2)

# simpson
diversity(data, index = "simpson", MARGIN = 2)

# Pielou
diversity(data, index = "shannon", MARGIN = 2)/log(specnumber(data, MARGIN = 2))