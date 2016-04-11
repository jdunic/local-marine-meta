#install.packages("ncdf")
#install.packages("chron")

#library(devtools)
#install_github("jebyrnes/hadsstR")

library(hadsstR)
library(lattice)
library(beepr)

sstData <- loadHadSST(directory="Data/", hadsstFilename="HadISST_sst.nc") 

Rprof("profile1.out", interval = 0.5, line.profiling=TRUE)
eval(climateChangeMats_1935_2011 <- getClimateChange(sstData, years=1935:2011))
Rprof(NULL)
beep()

summaryRprof("profile1.out", lines = "show")


# Indexing the SSTAnnualArray:
# rows = longitude ([1] = -179.5, [180] = 0, [360] = 179.5)
# columns = latitude ([1] = -89.5, [90] = 0, [180] = 89.5)
# 3rd dimension = annual average sst

# Get temperature difference between year 2 and year 1 for each site.
getPointTempChange <- function(SSTAnnualArray_T1, SSTAnnualArray_T2, latitude, 
                               longitude, return_point = FALSE) {
    if (latitude < 0) lat_ind <- round(latitude - 0.5) + 91
    lat_ind <- round(latitude - 0.5) 

    if (longitude < 0) lon_ind <- round(longitude - 0.5) + 181
    lon_ind <- round(longitude - 0.5) 

    pointTempChange <- SSTAnnualArray_T2[lon_ind, lat_ind, 1] - SSTAnnualArray_T1[lon_ind, lat_ind, 1]
    if(return_point == TRUE) {
      adf <- data.frame('lat' = latitude, 'lon' = longitude, 
                        'pointTempChange' = pointTempChange)
      return(adf)    
    }
    adf <- data.frame('pointTempChange' = pointTempChange)
    return(adf)
}

# Let's start with looking at decadal rates of climate velocities over a 
# timespan that covers the period of our studies
climateChangeMats_1935_2011 <- getClimateChange(sstData, years=1935:2011); beep() 

climateChangeMats_1960_2011 <- getClimateChange(sstData, years=1960:2011)

start <- proc.time()
site_SSTarrays <- 
dlply(.data = fl_combined[1:10, ], .(Study.ID, Site), 
      function(x) {
        SSTAnnualArray_T1 <- getSSTAnnualArray(sstData, years = x$T1[1])
        SSTAnnualArray_T2 <- getSSTAnnualArray(sstData, years = x$T2[1])
        pointTemp <- getPointTempChange(SSTAnnualArray_T1, SSTAnnualArray_T2, x$Lat[1], x$Long[1])
        return(pointTemp)
      }
     )
proc.time() - start
beep()


getPointTempChange(T1_SSTAnnualArray, T2_SSTAnnualArray, latitude = fl_combined[1, 'Lat'], 
                   longitude = fl_combined[1, 'Long'])



T1_SSTAnnualArray <- getSSTAnnualArray(sstData, years = 2008)
T2_SSTAnnualArray <- getSSTAnnualArray(sstData, years = 2009)


min(fl_combined$T1)
max(fl_combined$T2)


# get list of arrays for the time period of each study
start <- proc.time()
site_clim_change_arrays <- 
dlply(.data = fl_combined, .(Study.ID, Site), 
      function(x) getClimateChange(sstData, years = c(min(x$T1), max(x$T2)))
     )
#total <- proc.time() - start
total
beep(8)

getPointCV <- function(climateChangeArray, latitude, longitude, return_point = FALSE) {
    lat_ind <- which(climateChangeArray$lat == round(latitude) - 0.5)
    lon_ind <- which(climateChangeArray$lon == round(longitude) - 0.5)
    pointCV <- climateChangeArray$velocityMat[lon_ind, lat_ind]
    if(return_point == TRUE) {
      adf <- data.frame('lat' = latitude, 'lon' = longitude, 'CV' = pointCV)
      return(pointCV)    
    }
    return(data.frame('climate_vel' = pointCV))
}



getPointCV(site_clim_change_arrays[[1]], latitude = fl_combined[1, ]$Lat, 
           longitude = fl_combined[1, ]$Long)

test <- ldply(site_clim_change_arrays, 
      function(x) getPointCV(x, latitude = fl_combined[1, ]$Lat,
                             longitude = fl_combined[1, ]$Long)
      )

test <- merge(fl_combined, test, by = c('Study.ID', 'Site'))

merge(x, y, by=c("k1","k2")) # NA's match





test_clim_array1 <- getClimateChange(sstData, years = c(2008, 2009))
test_clim_array2 <- getClimateChange(sstData, years = c(2007, 2009))
test_clim_array3 <- getClimateChange(sstData, years = c(2007, 2008))
test_clim_array4 <- getClimateChange(sstData, years = c(2006, 2009))
test_clim_array5 <- getClimateChange(sstData, years = c(2006, 2007))

test_study <- fl_combined[1, ]

lat_ind <- which(test_clim_array1$lat == round(test_study$Lat) - 0.5)
lon_ind <- which(test_clim_array1$lon == round(test_study$Long) - 0.5)
getPointCV(test_clim_array1, latitude = fl_combined[1, ]$Lat, 
           longitude = fl_combined[1, ]$Long)
lat_ind <- which(test_clim_array2$lat == round(test_study$Lat) - 0.5)
lon_ind <- which(test_clim_array2$lon == round(test_study$Long) - 0.5)
getPointCV(test_clim_array2, latitude = fl_combined[1, ]$Lat, 
           longitude = fl_combined[1, ]$Long)
lat_ind <- which(test_clim_array3$lat == round(test_study$Lat) - 0.5)
lon_ind <- which(test_clim_array3$lon == round(test_study$Long) - 0.5)
getPointCV(test_clim_array3, latitude = fl_combined[1, ]$Lat, 
           longitude = fl_combined[1, ]$Long)
lat_ind <- which(test_clim_array4$lat == round(test_study$Lat) - 0.5)
lon_ind <- which(test_clim_array4$lon == round(test_study$Long) - 0.5)
getPointCV(test_clim_array4, latitude = fl_combined[1, ]$Lat, 
           longitude = fl_combined[1, ]$Long)
lat_ind <- which(test_clim_array5$lat == round(test_study$Lat) - 0.5)
lon_ind <- which(test_clim_array5$lon == round(test_study$Long) - 0.5)
getPointCV(test_clim_array5, latitude = fl_combined[1, ]$Lat, 
           longitude = fl_combined[1, ]$Long)




getSSTLinearChangeMatFromArray<- function(sstAnnualArray, years=1969:2009){
  changeMat <- apply(sstAnnualArray, c(1,2), function(x){
    #check if this is land
    if(sum(is.na(x))<length(x)) return(10*coef(lm(x~I(years-mean(years))))[2])
    return(NA)
  })
  
  changeMat
  
}


test_clim_array$velocityMat[lon_ind, lat_ind]

which(as.numeric(test_clim_array$lon) == round(test_study$Long), arr.ind = TRUE)

which(test_clim_array$lon == -180, arr.ind = TRUE)



climateChangeMats <- getClimateChange(sstData, years=1935:2011); beep()
climateChangeMats <- getClimateChange(sstData, years=2000:2009); beep()


climateChangeMats_1955 <- getClimateChange(sstData, years=1955:1956); beep()
climateChangeMats_2000 <- getClimateChange(sstData, years=2000:2001); beep()
climateChangeMats_2008 <- getClimateChange(sstData, years=2008:2009); beep()

climateChangeMats_1935_2011 <- getClimateChange(sstData, years=1935:20011); beep()

pal <- colorRampPalette(c("blue","white", "red"))
with(climateChangeMats_1955, image(lon, lat, averageMat, col = pal(80)))
with(climateChangeMats_2000, image(lon, lat, averageMat, col = pal(80)))
with(climateChangeMats_2008, image(lon, lat, averageMat, col = pal(80)))

with(climateChangeMats_1955_2009, image(lon, lat, averageMat, col = pal(80)))


pal2 <- colorRampPalette(c("darkblue", "blue", "green", "white", "yellow", "orange", "red"))
with(climateChangeMats, image(lon, lat, spatialGradMat, col=pal2(101)))


latLonGrid <- expand.grid(lon = climateChangeMats$lon, lat = climateChangeMats$lat)

velMatTruncated <- climateChangeMats_1935_2011$velocityMat
velMatTruncated[velMatTruncated >200] <- 200
velMatTruncated[velMatTruncated < -200] <- -200

levelplot(velMatTruncated ~ lon * lat, data = latLonGrid, #at = cutpts, 
           pretty = T, 
          col.regions = pal(100),
           at=seq(-200,200,length.out=100))
beep('coin')


latLonGrid <- expand.grid(lon = climateChangeMats_1935_2011$lon, lat = climateChangeMats_135_2011$lat)
velMatTruncated <- climateChangeMats_19435_2011$velocityMat
velMatTruncated[velMatTruncated >200] <- 200
velMatTruncated[velMatTruncated < -200] <- -200
levelplot(velMatTruncated ~ lon * lat, data = latLonGrid, #at = cutpts, 
           pretty = T, 
           col.regions = pal(100),
           at=seq(-200,200,length.out=100), 
           panel=function(...) {
              grid.rect(gp=gpar(col=NA, fill="black"))
              panel.grid(alpha = 0)
              panel.levelplot(...)
           }
           )
beep('coin')


linearChange <- getSSTLinearChangeMat(sstData, years = 1955:2009)


climateChangeMats <- getClimateChange(sstData, years=1960:2009)



getSSTLinearChangeMat <- function (sstObj, years = 1969:2009) 
{
    sstAnnualArray <- getSSTAnnualArray(sstObj, years)
    getSSTAnnualArray(sstAnnualArray, years)
}



# Test spatial gradient averaging

spatial_mat <- matrix(NA, nrow = 360, ncol = 180)
angle_mat <- matrix(NA, nrow = 360, ncol = 180)

for (i in 1:360) {
    for (j in 1:180) {
        spatial 
    }
}


# get spatially averaged gradient using the neighbourhood method



g, h, i, 
j, k, l, 
m, n, o



# Get test matrix with just 9 cells
x <- sstData$sstArray[1:3, 13:15, 1000]


# For each row in the matrix get the neighbouring values from top to bottom and
ns_diff <- matrix(NA, nrow = nrow(x) - 1, ncol = ncol(x))
for (j in 1:ncol(x)) {
  ns_diff[ ] <- diff(x, differences = 1)
}


we_diff <- matrix(NA, nrow = nrow(t(x)) - 1, ncol = ncol(t(x)))
for (i in 1:ncol(t(x))) {
  we_diff[ ] <- diff(t(x), differences = 1, drop = FALSE)
}



#function to get the spatially averaged gradient
getSpatialGrad <- function(NSmat, WEmat, i,j){

  li <- ncol(NSmat)
  lj <- nrow(WEmat)
  # print(c(i,j))
  #get bounding box indices
  id <- i+1
  iu <- i-1
  jl <- j-1
  jr <- j+1
  if(jr>li) jr<-1 #wrap
  if(jl==0) jl<-li #wrap
  if(id>lj) return(c(NA, NA)) #meh, it's ice
  if(iu==0) return(c(NA, NA)) #meh, it's ice
  
  
  yGrad <- weighted.mean(c(NSmat[i, j], NSmat[iu, j], 
                           NSmat[iu, jl], NSmat[iu, jr], NSmat[id, jl], NSmat[id, jr]),
                         c(2, 2, rep(1, 4)), na.rm=T)
  #oops - did this the wrong direction, so, multiplying by -1 to correct
  xGrad <- weighted.mean(c(WEmat[i,j],WEmat[i,jl], 
                           WEmat[iu,jl], WEmat[iu,jr], WEmat[id,jl], WEmat[id,jr]),
                         c(2,2,rep(1,4)), na.rm=T)
  
  #some convrsion to radial coordinates
  vecSum <- sqrt(xGrad^2+yGrad^2)
  vecAngle <- NA
  if(!is.na(vecSum)){
    vecAngle <- 90-atan2(yGrad, xGrad)*180/pi
    if(vecAngle<0) vecAngle <- 360+vecAngle
  }
  
  return(c(vecSum, vecAngle))
  
}



getSpatialGradMatsFromMats <- function(NSmat, WEmat){
  #greate matrices for spatial gradients and velocity
  spatialMat <- matrix(NA, nrow=nrow(WEmat), ncol=ncol(WEmat))
  angleMat <- matrix(NA, nrow=nrow(WEmat), ncol=ncol(WEmat))
  
  for(i in 1:nrow(spatialMat)){
    for(j in 1:ncol(spatialMat)){
      spatialGrad <- getSpatialGrad(NSmat, WEmat, i,j)
      spatialMat[i,j] <- spatialGrad[1]
      angleMat[i,j] <- spatialGrad[2]
    }
  }
  
  return(list(spatialGradMat = spatialMat, angleMat = angleMat))
}

