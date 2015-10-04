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

fl_combined2 <- fl_combined

coordinates(fl_combined2) <- c('Long', 'Lat')

projection(fl_combined2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

impl <- extract(imp_map, fl_combined2, buffer = 1000, small = T)

# Extracts the RGB values from the three data layers. 
# The 'land' test values return NA only. 
imp_stack <- stack(imp_map_b1, imp_map_b2, imp_map_b3)
impl <- extract(imp_stack, fl_combined2, buffer = 2000, small = T)

hex_imps <- llply(impl, function(x) get_imp_hex(x))

#991118 = red = 6 = very high
#E67D27 = dark orange = 5 = high
#F0B81F = dark yellow = 4 = medium high
#F7EE3B = yellow = 3 = medium
#B2D698 = green = 2 = low
#8AB4EB = blue = 1 = very low

hexes <- c('#8AB4EB', '#B2D698', '#F7EE3B', '#F0B81F', '#E67D27', '#991118', NA)
imp_cat_val <- c(1, 2, 3, 4, 5, 6, NA)
imp_mean_cat_val <- c((0+1.4)/2, (1.4 + 4.59)/2, (4.95 + 8.47)/2, (8.74 + 12)/2, 
                      (12 + 15.52)/2, (15.52 + 20)/2, NA)
imp_cat <- c('very low', 'low', 'medium', 'medium high', 'high', 'very high', NA)

imp_lookup_df <- data.frame('hex' = hexes, 'values' = imp_cat_val, 
                            'category' = imp_cat, 'imp_mean_val' = imp_mean_cat_val)

imp_val_dfs <- llply(hex_imps, function(x) merge(x = x, y = imp_lookup_df, by.x = 1, by.y = 'hex'))

mean_imp_vals <- llply(imp_val_dfs, function(x) mean(x$imp_mean_val))

mean_imp_df <- data.frame('mean_imp_vals' = unlist(mean_imp_vals))

fl_combined <- cbind(fl_combined, mean_imp_df)

ggplot(data = fl_combined, aes(x = mean_imp_vals, y = yi.SppR.ROM)) +
  geom_point(aes(colour = mean_imp_vals), size = 3) +
  scale_colour_gradientn(colours=c('#8AB4EB', '#B2D698', '#F7EE3B', '#F0B81F', 
                                 '#E67D27', '#991118'), 
                       name = "Impact",
                       na.value=NA) +
  #geom_text(aes(label = Study.ID)) +
  stat_smooth(method = 'lm') +
  facet_wrap(~ Event.)
