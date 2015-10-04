# Explorations and tinkering

str(fl_event)
length(unique(fl_event$Study.ID))
length(unique(fl_noevent$Study.ID))

length(unique(ts_event$Study.ID))
length(unique(ts_noevent$Study.ID))

# General data exploration:
# Non-independence: study level clustering:

ggplot(data = fl_event, aes(x = as.factor(Study.ID), y = yi.SppR.ROM, 
                            colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw() +
  geom_hline(y = 0)

ggplot(data = fl_noevent, aes(x = as.factor(Study.ID), y = yi.SppR.ROM, 
                            co.lour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw() +
  geom_hline(y = 0)

colnames(ts_event)

dim(ts_event)

ggplot(data = ts_event, aes(x = T1, y = SppR, colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw()

ggplot(data = ts_noevent, aes(x = T1, y = SppR, colour = as.factor(Study.ID))) +
  geom_point() +
  theme_bw()



# Do all the study start and end sample sizes match up?

event <- read.csv("Data/firstLastData_event_v0.4-20150204.csv")
noevent <- read.csv("Data/firstLastData_noevent_v0.4-20150204.csv")

both <- rbind(event, noevent)

gvt <- gvisTable(both[which(both$n1 != both$n2), 
                      c('Study.ID', 'Reference', 'Collector', 'Event.', 'Event.type',  
                        'SiteSize', 'SiteSizeUnits', 'PlotSize', 'PlotSizeUnits',
                        'T1', 'T1m', 'T2', 'T2m', 'n1', 'n2', 'sub1', 'sub2', 'SppR1', 'SppR2')
                     ]
                )
plot(gvt)

both[which(both$sub1 != both$sub2), ]

gvt <- gvisTable(both[which(both$sub1 != both$sub2), 
                      c('Study.ID', 'Reference', 'Collector', 'Event.', 'Event.type',  
                        'SiteSize', 'SiteSizeUnits', 'PlotSize', 'PlotSizeUnits',
                        'T1', 'T1m', 'T2', 'T2m', 'n1', 'n2', 'sub1', 'sub2', 'SppR1', 'SppR2')
                     ]
                )
plot(gvt)