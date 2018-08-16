library(tidyr)
library(dplyr)

library(ggmap)
library(vegan)

tab1 <- read.csv('/Users/jillian/Desktop/Meta-analysis_papers/Papers to QC/595_Methven_etal_2001_Table1.csv', header = TRUE, stringsAsFactors = FALSE, 
    encoding = "UTF-8")

# remove total fish and total species counts
tab1 <- tab1[-(36:37), ]

# remove family rows by finding and removing common names that do not have values
tab1 <- 
  tab1 %>% 
  filter(tab1$Common.name != '')

# fill blanks with '(0)' to be able to use extract
tab1$Total_1982.1983...b_1982.1983[tab1$Total_1982.1983...b_1982.1983 == '0'] <- '(0)'

total_1982 <- extract(tab1, col = Total_1982.1983...b_1982.1983, 
                      into = c('counts_1982'), 
                      regex = '\\((\\d*)\\)', convert = TRUE) %>% 
              select(Species, counts_1982)

total_1989 <- 
  select(tab1, Species, Total_1989.1990..b_1989.1990) %>%
  filter(Total_1989.1990..b_1989.1990 != 0) %>%
  extract(col = Total_1989.1990..b_1989.1990, into = c('counts_1989'), 
          regex = '(\\d*\\s?\\d*)\\s\\d*á?\\d*') %>% 
  left_join(select(tab1, Species), ., fill = 0) %>% 
  mutate(counts_1989 = as.numeric(.$counts_1989)) %>% 
  replace(is.na(.), 0)

totals <- 
  left_join(total_1982, total_1989) %>% 
  as.data.frame(., row.names = .$Species) %>% 
  select(-Species)

# abundance
as.data.frame(colSums(totals))

# richness
as.data.frame(specnumber(totals, MARGIN = 2))

# shannon
as.data.frame(diversity(totals, index = "shannon", MARGIN = 2))

# simpson
as.data.frame(diversity(totals, index = "simpson", MARGIN = 2))

# Pielou
as.data.frame(diversity(totals, index = "shannon", MARGIN = 2)/log(specnumber(totals, MARGIN = 2)))

library(ggmap)

map <- get_map(location = c(lon = -0.666564, lat = 37.646310), 
               zoom = 14, maptype = 'satellite')

ggmap(map)