library(vegan)
library(dplyr)

# Table 1
tab1 <- read.csv('/Users/jillian/Desktop/Meta-analysis_papers/Papers\ to\ extract/620_Lazzari_etal_1999/620_Lazzari_etal_1999_Table1.csv', row.names = 1, 
    stringsAsFactors = FALSE)

str(tab1)

abundance <- tab1[nrow(tab1), ]
tab1 <- tab1[-28, ]

# back calculate original species counts
counts <- data.frame(mapply(`*`, tab1/100, abundance, SIMPLIFY=FALSE))


# abundance
counts
as.data.frame(colSums(counts))

# richness
as.data.frame(specnumber(counts, MARGIN = 2))

# shannon
as.data.frame(diversity(counts, index = "shannon", MARGIN = 2))

# simpson
as.data.frame(diversity(counts, index = "simpson", MARGIN = 2))

# Pielou
as.data.frame(diversity(counts, index = "shannon", MARGIN = 2)/log(specnumber(counts, MARGIN = 2)))
