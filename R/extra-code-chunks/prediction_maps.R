# Create cropped raster

#(xmin, xmax, ymin, ymax)

# smaller extent
e <- as(extent(-71.705531, -67.944214, 41.377132, 43.180299), 
       'SpatialPolygons')

# boston harbour extent
e <- as(extent(-71.085674, -70.581677, 42.196429, 42.544938), 'SpatialPolygons')

# east coast
e <- as(extent(-71.254492, -69.784015, 40.963025, 43.861433), 'SpatialPolygons') 

# bigger extent I can look at
e <- as(extent(-76.998293, -36.612552, 31.445744, 52.414631), 
       'SpatialPolygons')

proj4string(e) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

e <- spTransform(e, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

crop_inv <- crop(invasives, e)
crop_nut <- crop(nuts, e)
#crop_pest <- crop(pesticides, e)

par(mfrow = c(1, 2))
plot(crop_inv)
plot(crop_nut)
#plot(crop_pest)


e <- spTransform(e, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
crop_vel <- crop(velocity, e)
crop_lin <- crop(linear, e)

crop_vel <- projectRaster(from = crop_vel, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crop_lin <- projectRaster(from = crop_lin, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

crop_vel <- resample(crop_vel, crop_inv)
crop_lin <- resample(crop_lin, crop_inv)
crop_fert <- resample(crop_fert, crop_inv)
crop_pest <- resample(crop_pest, crop_inv)

test <- crop_inv

x <- as.array(crop_inv)

switch_NA_zero <- function(cell) {
  cell <- cell
  if (is.na(cell)) {
    cell <- 0
  } else if (cell == 0) {
    cell <- NA
  }
  return(cell)
} 

y <- apply(x, MARGIN = c(1, 2), FUN = switch_NA_zero)

z <- raster(x = y, template = crop_inv)
plot(z)

plot(z, xlim = c(-5931008, -5576475), ylim = c(4883911, 5173388)) 

crop_inv2 <- z


for (i in list(crop_inv2, crop_fert, crop_pest, crop_vel, crop_lin)) {
  print(names(i))
  print(extent(i))
  print(origin(i))
}

par(mfrow = c(1, 2))
plot(crop_vel)
plot(crop_lin)

beep()

# Create raster layers for different durations 
time_map1 <- !is.na(crop_vel)

names(time_map1) <- 'Duration'
time_map10 <- time_map1 * 10
time_map20 <- time_map1 * 20
time_map50 <- time_map1 * 50


# Create a raster stack with the predictor layers
# Duration1
# Cumulative Impacts

# make sure the names of the raster layers match up with the names used in the model

names(global2_lme@frame)

names(crop_vel) <- 'mean_vocc'
names(crop_lin) <- 'raster_lin'
names(crop_inv2) <- 'mean_invs'
names(crop_fert) <- 'mean_fert'
names(crop_pest) <- 'mean_pest'


multi_preds <- stack(time_map1, crop_vel, crop_lin, crop_inv2, crop_fert, crop_pest)

# re.form: formula for random effects to condition on.  If 'NULL', include all 
# random effects; if 'NA' or '~0', include no random effects
predict1 <- predict(multi_preds, global2_lme, fun = lme4:::predict.merMod, re.form=~0)
beep()

predict1_percent <- (exp(predict1) - 1) * 100

plot(predict1, col = heat.colors(100), colNA = 'black')

plot(predict1_percent, col = heat.colors(100), colNA = 'black')

plot(predict1_percent, col = rainbow(100))



multi_preds <- stack(time_map10, crop_vel, crop_lin, crop_inv2, crop_fert, crop_pest)

# re.form: formula for random effects to condition on.  If 'NULL', include all 
# random effects; if 'NA' or '~0', include no random effects
predict1 <- predict(multi_preds, global2_lme, fun = lme4:::predict.merMod, re.form=~0)
beep()

predict1_percent <- (exp(predict1) - 1) * 100

plot(predict1, col = rainbow(100))

plot(predict1, col = topo.colors(100))

plot(predict1_percent, col = heat.colors(3))

plot(predict1_percent, col = rainbow(255))



predict1_percent <- exp(predict1) - 1


# Create raster layers for different durations 
time_map1 <- imp_map > 0
names(time_map1) <- 'Duration'
beep()

time_map10 <- time_map1 * 10
time_map20 <- time_map1 * 20
time_map50 <- time_map1 * 50


# Create a raster stack with the predictor layers
# Duration1
# Cumulative Impacts

# make sure the names of the raster layers match up with the names used in the model

names(global_lme@frame)

names(imp_map) <- 'mean_imps'
imp_preds <- stack(time_map1, imp_map)

# re.form: formula for random effects to condition on.  If 'NULL', include all 
# random effects; if 'NA' or '~0', include no random effects
predict1 <- predict(imp_preds, global_lme, fun = lme4:::predict.merMod, re.form=~0)

plot(predict1)

