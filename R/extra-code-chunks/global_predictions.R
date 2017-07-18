Frequency distributions of impact predictions in coastal areas:
1) crop cumulative human impact values to MEOW boundaries
2) get frequency distributions of impacts 
3) run predict model over this distribution of impact values for 5, 10, 15 years
4) plot distribution of impacts, plot distribution of predicted values
5) plot distribution of impacts within the data, plot distribution of predicted values based on the distribution of impacts within our data
6) rinse and repeat for the other impact layers for 5, 10, 15 years


http://stackoverflow.com/questions/19321573/how-to-crop-a-raster-with-a-different-projection
This might not be the most elegant way but might help. I have created two example rasters loosely based on your examples. They have the same projection and extent.

library(raster)
r1 <- raster(nrows=500, ncols=500, 
         ext=extent(c(-20, 60.00893, -40.00893, 40)),
         crs='+proj=longlat +datum=WGS84')
r1[] <- rnorm(500*500,0,1)

r2 <- raster(nrows=50, ncols=50, 
         ext=extent(c(355500, 428500, 2879500, 2952500)),
         crs='+proj=utm +zone=36 +datum=WGS84 +units=m')
r2[] <- rnorm(50*50,0,1)
To be able to crop raster r1 using the extent of raster r2, I am first creating a spatialPolygon from the extent of raster r2, second assigning it the good projection, and third transforming the polygon to the projection of raster r1.

library(rgdal) 
# Make a SpatialPolygon from the extent of r2
r2extent <- as(extent(r2), 'SpatialPolygons')
# Assign this SpatialPolygon the good projection
proj4string(r2extent) <- proj4string(r2)
# Transform the projection to that of r1
r2extr1proj <- spTransform(r2extent, CRS(proj4string(r1)))
Finally, you can crop raster r1 using the polygon r2extr1proj which represents the extent of r2 in the projection of r1. Then plot the two rasters.

r1crop <- crop(r1, r2extr1proj)
layout(matrix(c(1:2), nrow=1))
plot(r1crop)
plot(r2)

http://www.marineregions.org/downloads.php

MEOW colour coding predictions:
