library(vegan)
library(dplyr)

# Table 1 - trawl fishes
tab1 <- read.csv('../Dropbox (Byrnes Lab)/Honig.Meta.Papers/Gibson et al./660_Gibson_etal_1993_Table1.csv', row.names = 1)

str(tab1)

# richness
specnumber(tab1, MARGIN = 2)

# shannon
diversity(tab1, index = "shannon", MARGIN = 2)

# simpson
diversity(tab1, index = "simpson", MARGIN = 2)

# Pielou
diversity(tab1, index = "shannon", MARGIN = 2)/log(specnumber(tab1, MARGIN = 2))


# Table 2 - seine fishes
tab2 <- read.csv('../Dropbox (Byrnes Lab)/Honig.Meta.Papers/Gibson et al./660_Gibson_etal_1993_Table2.csv', row.names = 1)

str(tab2)

# richness
specnumber(tab2, MARGIN = 2)

# shannon
diversity(tab2, index = "shannon", MARGIN = 2)

# simpson
diversity(tab2, index = "simpson", MARGIN = 2)

# Pielou
diversity(tab2, index = "shannon", MARGIN = 2)/log(specnumber(tab2, MARGIN = 2))

# Table 5 - trawl crustaceans
tab5 <- read.csv('../Dropbox (Byrnes Lab)/Honig.Meta.Papers/Gibson et al./660_Gibson_etal_1993_Table5.csv', row.names = 1)

str(tab5)

# richness
specnumber(tab5, MARGIN = 2)

# shannon
diversity(tab5, index = "shannon", MARGIN = 2)

# simpson
diversity(tab5, index = "simpson", MARGIN = 2)

# Pielou
diversity(tab5, index = "shannon", MARGIN = 2)/log(specnumber(tab5, MARGIN = 2))