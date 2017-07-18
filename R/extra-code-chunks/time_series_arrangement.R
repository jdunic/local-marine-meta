#####################
# Reformat for timeseries analysis
#####################
zdf <- subset(richData, richData$Study.ID=="423" & richData$Site=="Venados Island")

timeData <- ddply(richData, .(Study.ID, Reference, Sys, coral, plant, algae, fish, inverts, mobile.inverts,
                              sessile.inverts, marine.mammals, phytoplankton, zooplankton, 
                              Vis, Trwl, Line, Drdg, Trp,
                              Descriptor.of.Taxa.Sampled,
                              Loc, Site), function(adf){
                               # adf <- zdf
                                if(is.na(adf$T1m)[1]) adf$T1m <- 6 #July, for the heck of it
                                if(is.na(adf$T2m)[1]) adf$T2m <- 6 #July, for the heck of it
                                rowDates <- parse_date_time(paste(adf$T1m, "1", adf$T1, sep="-"), 
                                                            order="%m-%d-%y")
                                # print( adf$T1m )
                                # print( adf$T1 )
                                
                                # print( adf$Date )
                                #cat(adf$Study.ID[1])
                                #cat(", ")
                                #cat(as.character(adf$Site[1]))
                                
                                 #cat("\n")
                                #add a new row and move the final info to it
                                last <-  order(rowDates)[nrow(adf)]
                                adf <- rbind(adf, adf[last,])
                                adf$len <- nrow(adf)
                                
                                m <- c("SppR", "Shan", "Even")#, "Simps")
                                m1 <- which(names(adf) %in% paste0(m, 1))
                                m2 <- which(names(adf) %in% paste0(m, 2))
                                
                                s1 <- which(names(adf) %in% paste0(m, 1, ".SD"))
                                s2 <- which(names(adf) %in% paste0(m, 2, ".SD"))
                                adf[nrow(adf),c(m1, s1)] <- adf[last,c(m2, s2)]
                                
                                e1 <- which(names(adf) %in% paste0(m, 1, ".Error"))
                                e2 <- which(names(adf) %in% paste0(m, 2, ".Error"))
                                et1 <- which(names(adf) %in% paste0(m, 1, ".Error.Type"))
                                et2 <- which(names(adf) %in% paste0(m, 2, ".Error.Type"))
                                adf[nrow(adf),c(e1, et1)] <- adf[last,c(e2, et2)]
                                
                                adf$T1[nrow(adf)] <- adf$T2[last] 
                                adf$T1m[nrow(adf)] <- adf$T2m[last] 
                                
                                
                                names(adf)[m1] <- gsub("1", "", names(adf)[m1]) 
                                names(adf)[s1] <- gsub("1", "", names(adf)[s1]) 
                                adf <-  adf[,-c(m2, s2, e2, et2)]
                                adf <-  adf[,-which(names(adf) %in% c("T2", "T2m"))]
                                
                                adf$temporal_std_r <- (adf$SppR-mean(adf$SppR, na.rm=T))/sd(adf$SppR, na.rm=T)
                                
                                adf$Duration <- max(adf$T1) - min(adf$T1)
                                adf
                                
                              }
)

timeData <- timeData[,-(agrep("Simps", names(timeData)))]
#qplot(Date, SppR1, data=timeData, color=paste(Reference,Site), facets=~Taxa)
#qplot(Date, temporal_std_r, data=timeData, color=paste(Reference,Site), facets=~Taxa)


#write.csv(firstSampleFilteredData[-eventIDX,], paste0("Data/firstLastData_noevent",trailer), row.names=F)
#write.csv(firstSampleFilteredData[eventIDX,], paste0("Data/firstLastData_event",trailer), row.names=F)
#write.csv(timeData[-timeEventIDX,], paste0("Data/timeseriesData_noevent",trailer), row.names=F)
#write.csv(timeData[timeEventIDX,], paste0("Data/timeseriesData_event",trailer), row.names=F)

#write.csv(firstSampleFilteredData, paste0("Data/fullData",trailer), row.names=F)



############################################################
# Get standard deviation of time series residuals for each 
############################################################
# Using these values for weighting assumes that the change in species richness
# is a linear relationship.

names(richData)

names(richData)[which(names(richData) == 'Event.type.2')] <- 'EventType'

y1Cols <- grep("1", names(richData))
y2Cols <- grep("2", names(richData))

y1y2_names <- names(richData[, c(y1Cols, y2Cols)])

gen_Cols <- richData[, which(!names(richData) %in% y1y2_names)]

y1_data <- cbind(gen_Cols, richData[, y1Cols])
y2_data <- cbind(gen_Cols, richData[, y2Cols])
names(y2_data) <- names(y1_data)

# Get data frame where each row is a single observation of richness
ts_data <- rbind(y1_data, y2_data)

# Get rid of duplicate data
ts_data <- unique(ts_data)

# For each study-site-taxa-method-location get all the yearly points that have 
# the same month
test <- ddply(ts_data, .(Study.ID, Reference, Sys, 
                         coral, plant, algae, fish, inverts, mobile.inverts,
                         sessile.inverts, marine.mammals, phytoplankton, zooplankton, 
                         Vis, Trwl, Line, Drdg, Trp,
                         Descriptor.of.Taxa.Sampled,
                         Loc, Site), 
              function(x) {
                info <- paste('Study:', x$Study.ID, 'Site:', x$Site, 'Date:', x$date1, sep = ' ')
                print()
              }
)

