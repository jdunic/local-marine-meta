#######################
#
# Map the studies from the meta-analysis
#
# Changelog
#
# 5/12/2014 - JEKB added code to turn maps into pdfs, and some geom_text for better labeling
# 5/12/2014 - JEKB fixed effect size names
#######################

# Meta-analysis
#setwd('Meta_analysis_2014')

library(rworldmap)
library(ggplot2)
library(gridExtra)
library(directlabels)


fl_event$Lat <- as.numeric(as.character(fl_event$Lat))
fl_event$Long <- as.numeric(as.character(fl_event$Long))

fl_noevent$Lat <- as.numeric(as.character(fl_noevent$Lat))
fl_noevent$Long <- as.numeric(as.character(fl_noevent$Long))


# Make dataframe with all of the data (event and no_events)
all_data <- rbind(fl_combined)
all_data$SiteIndex <- 1:length(all_data$Study.ID)

# Get world map from rworldmap
worldmap <- map_data(map = "world")
# For interest you can take a look at the data for the worldmap
str(worldmap)

# Plot entire world map with sites
sitemap <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "darkgrey", fill = "darkgrey") +
  theme_wb() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, colour = Event.), 
             size = 3, alpha = 0.7) +
  scale_colour_manual(values = c('red', 'black')) +
  theme(legend.position = c(0.07, 0.7), 
        legend.background = element_rect(fill=c('white', alpha = 0.7)),
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        panel.background = element_rect(fill = 'azure2')
        ) +
  xlab("Longitude") +
  ylab("Latitude")
sitemap

#see the study number for each point
pdf("maps/sites_and_studies.pdf", width=14)
sitemap +
  #geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) 
  geom_text(data = all_data, aes(x = Long+2, y =Lat+2, label = Study.ID, group = SiteIndex), size=2) 
dev.off()


#see who is associated with each point
pdf("maps/sites_and_people.pdf", width=14)
sitemap +
#  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Collector, group = SiteIndex), list("top.bumptwice", cex = 0.5)) 
  geom_text(data = all_data, aes(x = Long+2, y =Lat+2, label = Collector, group = SiteIndex), size=2) 
dev.off()


#see where each point should be
pdf("maps/sites_and_location.pdf", width=14)
sitemap +
#  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumptwice", cex = 0.5)) 
  geom_text(data = all_data, aes(x = Long+2, y =Lat+2, label = Loc, group = SiteIndex), size=2) 
dev.off()
  
  
  
# Making smaller maps to more easily read labels that you chose to use
# geom_dl() is used to help space labels out a bit better - if people have better 
# solutions that would be great!!!
# Should make this a function at some point!

# Americas
latlimits <- c(90, -90) 
longlimits <- c(-180, -30) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumpup", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)

# North Atlantic
latlimits <- c(0, 90) 
longlimits <- c(-30, 5) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)

# South Atlantic/Antarctica
latlimits <- c(0, -90) 
longlimits <- c(-30, 5) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)

# Continental Europe and North Africa
latlimits <- c(0, 90) 
longlimits <- c(0, 25) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)

# South Africa
latlimits <- c(-90, 0) 
longlimits <- c(0, 25) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)

# Middle East/Asia/Indian Ocean
latlimits <- c(90, -90) 
longlimits <- c(20, 120) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumpup", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"), 
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)

# East Asia/IndoPacific/Australia
latlimits <- c(90, -90) 
longlimits <- c(110, 180) 
p1 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), position = 'jitter', 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Study.ID, group = SiteIndex), list("top.bumptwice", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        )
p2 <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), 
                               size = 3) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  geom_dl(data = all_data, aes(x = Long, y =Lat, label = Loc, group = SiteIndex), list("top.bumpup", cex = 0.5)) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        axis.title.y = element_blank()
        )
grid.arrange(p1, p2, ncol = 2)


# Ugly way to get Collector names given bad lat/long
bad_latlongs <- c(561, 395, 565, 172, 6, 448, 51, 705, 524, 11, 67, 347, 538)


df <- data.frame("Collector" = NA, "StudyID" = NA, "Lat" = NA, "Long" = NA)
for (i in 1:length(bad_latlongs)) {
    #browser()
    Collector <- as.character(all_data$Collector[which(all_data$Study.ID == bad_latlongs[i])])
    StudyID <- bad_latlongs[i]
    Lat <- all_data$Lat[which(all_data$Study.ID == bad_latlongs[i])]
    Long <- all_data$Long[which(all_data$Study.ID == bad_latlongs[i])]
    row <- data.frame("Collector" = Collector, "StudyID" = StudyID, "Lat" = Lat, "Long" = Long)
    str(row)
    df <- rbind(df, row)
  }
unique(df)

######################
# Plotting study sites on HCI map
######################
# Study data
event <- read.csv("Data/firstLastData_event_v0.4-20150204.csv")
noevent <- read.csv("Data/firstLastData_noevent_v0.4-20150204.csv")


library(raster)

# The global human cumulative impact map (full model layer)

# Read in the tif file as an object with the formal class 'RasterLayer'
#imp_map <- raster("model_class_wgs84_lzw.tif")

# Alternatively we can read in the geotiff with each band as an individual 
# raster. This allows us to extract the RGB data from each 'band'
# See an iterative solution here: http://stackoverflow.com/questions/15270107/r-get-a-specific-band-of-a-rasterlayer


imp_map_b1 <- raster("../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 1)
imp_map_b2 <- raster("../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 2)
imp_map_b3 <- raster("../Human_Cumulative_Impacts/model_class_wgs84_lzw.tif", band = 3)

# We can subsequently combine these into a stack which allows us to plot using 
# the RGB colour scheme.
imp_stack <- stack(imp_map_b1, imp_map_b2, imp_map_b3)

both <- rbind(event, noevent)
both2 <- both
coordinates(both2) <- c('Long', 'Lat')
site_rgbs <- extract(imp_stack, both2, buffer = 1000, small = T, fun = mean)
site_rgbs_df <- as.data.frame(site_rgbs)
names(site_rgbs_df) <- c('red', 'green', 'blue')

both <- cbind(both, site_rgbs_df)
both$SiteIndex <- 1:length(both$Study.ID)

# Plot entire world map with sites
sitemap <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  theme_bw() +
  geom_point(data = both[which(!is.na(both$red)), ], 
             aes(x = Long, y = Lat, 
                 colour = rgb(red = red, green = green, blue = blue, maxColorValue = 255)), 
                 size = 3, alpha = 0.5) +
  #scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        ) +
  xlab("Longitude") +
  ylab("Latitude")
sitemap




# Get world map from rworldmap
worldmap <- map_data(map = "world")
# For interest you can take a look at the data for the worldmap
str(worldmap)

# Plot entire world map with sites
sitemap <- ggplot(data = worldmap, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "azure2", fill = "azure2") +
  theme_bw() +
  geom_point(data = all_data, aes(x = Long, y = Lat, group = SiteIndex, 
                               colour = yi.SppR.ROM), size = 3, alpha = 0.5) +
  scale_colour_gradient2(limits=c(-1.5, 2.5), low="red", mid=("grey")) +
  theme(legend.position="none",
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "cm")
        ) +
  xlab("Longitude") +
  ylab("Latitude")
sitemap