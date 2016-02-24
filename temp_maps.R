library(dplyr)
library(ggplot2)

meta_points <- read.csv("Data_outputs/cb_site_data.csv")

meta_points_summary <- meta_points %>%
  group_by(Study.ID, Lat, Long) %>%
  dplyr::summarise(Sites = length(Site))



ggplot(data=meta_points_summary, aes(x=Long, y=Lat))  +
  geom_point(mapping=aes(color=Study.ID, size=Sites)) +
  borders("world", alpha=0.5) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() +
  scale_size(range=c(3,8)) +
  ggtitle(paste0(sum(meta_points_summary$Sites), (" Sites with Marine Diversity Change Data")))







ggplot(data=meta_points, aes(x=Long, y=Lat))  +
  geom_point(mapping=aes(color=Study.ID), size=3) +
  borders("world", alpha=0.5) +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal() +
  ggtitle(paste0(sum(meta_points_summary$Sites), (" Sites with Marine Diversity Change Data")))