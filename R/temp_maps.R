library(dplyr)
library(ggplot2)

meta_points <- read.csv('./master_data/SiteSpatialData.csv', stringsAsFactors = FALSE)

meta_points_summary <- meta_points %>%
  group_by(Study.ID, Start_Lat, Start_Long) %>%
  dplyr::summarise(Sites = length(Site))



ggplot(data=meta_points_summary, aes(x=Start_Long, y=Start_Lat))  +
  geom_point(mapping=aes(color=factor(Study.ID), size=Sites)) +
  borders("world", alpha=0.5) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() +
  scale_size(range=c(3,8)) +
  ggtitle(paste0(sum(meta_points_summary$Sites), (" Sites with Marine Diversity Change Data")))




ggplot(data=meta_points, aes(x=Start_Long, y=Start_Lat))  +
  geom_point(mapping=aes(color=Study.ID), size=1) +
  borders("world", alpha=0.5) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() +
  ggtitle(paste0(sum(meta_points_summary$Sites), (" Sites with Marine Diversity Change Data")))