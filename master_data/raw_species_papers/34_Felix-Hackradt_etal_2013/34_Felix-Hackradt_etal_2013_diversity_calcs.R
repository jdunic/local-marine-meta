library(ggmap)
library(vegan)

tab1 <- read.csv('/Users/jillian/Desktop/Meta-analysis_papers/Papers to QC/34_Felix-Hackradt_etal_2013_Table1.csv', header = TRUE, stringsAsFactors = FALSE, 
    row.names = 1)

tab1[is.na(tab1)] <- 0

# abundance
as.data.frame(colSums(tab1))

# richness
as.data.frame(specnumber(tab1, MARGIN = 2))

# shannon
as.data.frame(diversity(tab1, index = "shannon", MARGIN = 2))

# simpson
as.data.frame(diversity(tab1, index = "simpson", MARGIN = 2))

# Pielou
as.data.frame(diversity(tab1, index = "shannon", MARGIN = 2)/log(specnumber(tab1, MARGIN = 2)))

library(ggmap)

map <- get_map(location = c(lon = -0.666564, lat = 37.646310), 
               zoom = 14, maptype = 'satellite')

ggmap(map)