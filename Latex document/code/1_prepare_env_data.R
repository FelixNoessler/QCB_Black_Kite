
##########################################################
# First script
# Prepare the environmental data
#   - Land cover data
#   - Bioclimatic variables
#   - Distance to closest landfill and river or lake
#
# save everything as a raster stack in the target 
# spatial resolution
#
# Prepare the change in annual temperature and 
# in annual precipiytion for the prediction of
# of the futre climate condtions
#
##########################################################



# Install required packages -----------------------------------------------
packages <- c("ggplot2", "gridExtra", "dplyr", 
              "tidyr", "purrr", "HH", "psych", "MuMIn",
              "rnaturalearth", "rmapshaper",
              "auk", "unmarked", "AICcmodavg",
              "raster", "rgeos", "sp", "sf")

install.packages(setdiff(packages, rownames(installed.packages())))


# Loading packages --------------------------------------------------------
library(ggplot2)
library(dplyr)


# Loading geometries for the mainland of Spain ----------------------------
spain <- rnaturalearth::ne_countries(country = 'spain',
                                     scale = 'medium',
                                     returnclass = 'sf')
# spain %>%
#   ggplot()+
#   geom_sf(fill='black')


spain_crop <- rmapshaper::ms_filter_islands(spain, 
                                            min_area = 100000000000, 
                                            drop_null_geometries=T)
# plot(sf::st_geometry(spain_crop))


# Prepare climate data ----------------------------------------------------
### Climate data

if (!file.exists('data/environmental_data/clim.grd')) {
  clim <- raster::getData('worldclim', 
                          var = 'bio',
                          res = 2.5, 
                          download = F, 
                          path = 'data/environmental_data')
  
  raster::xres(clim) * 111.19
  
  clim <- raster::crop(clim, spain_crop)
  clim <- raster::mask(clim, spain_crop)
  
  raster::writeRaster(clim, 
                      'data/environmental_data/clim.grd',
                      format = 'raster', 
                      options = 'INTERLEAVE=BAND',
                      overwrite = TRUE)
} else {
  clim <- raster::brick('data/environmental_data/clim.grd')
}


# Prepare land cover data -------------------------------------------------

### Tree cover

if (!file.exists('data/environmental_data/lc_tree.grd')) {
  lc_tree1 <- raster::raster('data/environmental_data/tree_cover1.tif')
  lc_tree1 <- raster::crop(lc_tree1, spain_crop)
  lc_tree1 <- raster::mask(lc_tree1, spain_crop)
  
  lc_tree2 <- raster::raster('data/environmental_data/tree_cover2.tif')
  lc_tree2 <- raster::crop(lc_tree2, spain_crop)
  lc_tree2 <- raster::mask(lc_tree2, spain_crop)
  
  lc_tree3 <- raster::raster('data/environmental_data/tree_cover3.tif')
  lc_tree3 <- raster::crop(lc_tree3, spain_crop)
  lc_tree3 <- raster::mask(lc_tree3, spain_crop)
  
  lc_tree4 <- raster::raster('data/environmental_data/tree_cover4.tif')
  lc_tree4 <- raster::crop(lc_tree4, spain_crop)
  lc_tree4 <- raster::mask(lc_tree4, spain_crop)
  
  lc_tree <- raster::merge(lc_tree1, lc_tree2, lc_tree3, lc_tree4)
  raster::plot(lc_tree)
  
  lc_tree_cover <- raster::resample(lc_tree, clim, method = 'bilinear')
  names(lc_tree_cover) <- 'tree_cover'
  
  raster::writeRaster(lc_tree_cover, 
                      'data/environmental_data/lc_tree.grd',
                      format = 'raster', 
                      options = 'INTERLEAVE=BAND',
                      overwrite = TRUE)
  rm(lc_tree1, lc_tree2, lc_tree3, lc_tree4, lc_tree)
} else {
  
  lc_tree_cover <- raster::raster('data/environmental_data/lc_tree.grd')
}


### Herbaceous vegetation

if (!file.exists('data/environmental_data/lc_herbs.grd')) {
  
  lc_herbs1 <- raster::raster(
    'data/environmental_data/herbaceous_vegetation1.tif')
  lc_herbs1 <- raster::crop(lc_herbs1, spain_crop)
  lc_herbs1 <- raster::mask(lc_herbs1, spain_crop)
  
  lc_herbs2 <- raster::raster(
    'data/environmental_data/herbaceous_vegetation2.tif')
  lc_herbs2 <- raster::crop(lc_herbs2, spain_crop)
  lc_herbs2 <- raster::mask(lc_herbs2, spain_crop)
  
  lc_herbs3 <- raster::raster(
    'data/environmental_data/herbaceous_vegetation3.tif')
  lc_herbs3 <- raster::crop(lc_herbs3, spain_crop)
  lc_herbs3 <- raster::mask(lc_herbs3, spain_crop)
  
  lc_herbs4 <- raster::raster(
    'data/environmental_data/herbaceous_vegetation4.tif')
  lc_herbs4 <- raster::crop(lc_herbs4, spain_crop)
  lc_herbs4 <- raster::mask(lc_herbs4, spain_crop)
  
  lc_herbs <- raster::merge(lc_herbs1, lc_herbs2, 
                            lc_herbs3, lc_herbs4)
  
  # raster::plot(lc_herbs)
  
  lc_herb_cover <- raster::resample(lc_herbs, 
                                    clim, 
                                    method = 'bilinear')
  names(lc_herb_cover) <- 'grass_cover'
  
  raster::writeRaster(lc_herb_cover, 
              'data/environmental_data/lc_herbs.grd',
              format = 'raster', 
              options = 'INTERLEAVE=BAND',
              overwrite = TRUE)
  
  rm(lc_herbs1, lc_herbs2, lc_herbs3, lc_herbs4, lc_herbs)
} else {
  
  lc_herb_cover <- raster::raster('data/environmental_data/lc_herbs.grd')
}


### Bare soil

if (!file.exists('data/environmental_data/lc_bare_soil.grd')) {
  lc_bare1 <- raster::raster('data/environmental_data/bare_soil1.tif')
  lc_bare1 <- raster::crop(lc_bare1, spain_crop)
  lc_bare1 <- raster::mask(lc_bare1, spain_crop)
  
  lc_bare2 <- raster::raster('data/environmental_data/bare_soil2.tif')
  lc_bare2 <- raster::crop(lc_bare2, spain_crop)
  lc_bare2 <- raster::mask(lc_bare2, spain_crop)
  
  lc_bare3 <- raster::raster('data/environmental_data/bare_soil3.tif')
  lc_bare3 <- raster::crop(lc_bare3, spain_crop)
  lc_bare3 <- raster::mask(lc_bare3, spain_crop)
  
  lc_bare4 <- raster::raster('data/environmental_data/bare_soil4.tif')
  lc_bare4 <- raster::crop(lc_bare4, spain_crop)
  lc_bare4 <- raster::mask(lc_bare4, spain_crop)
  
  lc_bare <- raster::merge(lc_bare1, lc_bare2, lc_bare3, lc_bare4)
  
  # raster::plot(lc_bare)
  
  lc_bare_soil <- raster::resample(lc_bare, clim, method = 'bilinear')
  names(lc_bare_soil) <- 'bare_soil'
  
  raster::writeRaster(lc_bare_soil, 
                      'data/environmental_data/lc_bare_soil.grd',
                      format = 'raster', 
                      options = 'INTERLEAVE=BAND',
                      overwrite = TRUE)
  
  rm(lc_bare1, lc_bare2, lc_bare3, lc_bare4, lc_bare)
}else{
  
  lc_bare_soil <- raster::raster('data/environmental_data/lc_bare_soil.grd')  
}



# Calculate distance to closest river or lake -----------------------------


if (!file.exists('data/environmental_data/distance_to_water.grd')) {
  water <- sf::st_read('data/environmental_data/clc2018_vector/clc2018.gpkg',
                       query = "SELECT * FROM clc2018 
                        WHERE Code_18 == 511
                        OR Code_18 == 512")
  
  plot(sf::st_geometry(water),  axes = TRUE)
  
  
  ## load only the first
  clim <- raster::raster('data/environmental_data/clim.grd')
  
  
  
  
  raster_points <-  as(clim,"SpatialPoints")
  water_poly <- as(water, "Spatial")
  
  water_poly@proj4string
  raster_points@proj4string
  
  
  
  crs1 <- sp::CRS('+proj=laea 
                  +lat_0=52 
                  +lon_0=10 
                  +x_0=4321000 
                  +y_0=3210000 
                  +ellps=GRS80 
                  +units=m 
                  +datum=WGS84 +no_defs')
  
  raster_points_transformed <- sp::spTransform(raster_points, crs1)
  plot(sf::st_as_sf(raster_points_transformed), cex=0.2, pch=15, axes=T)
  
  
  water_poly_transformed <- sp::spTransform(water_poly, crs1)
  plot(sf::st_as_sf(water_poly_transformed),  axes = TRUE)
  
  
  dist1 <- rgeos::gDistance(raster_points_transformed[1:10000,], 
                            water_poly_transformed, 
                            byid=T)
  
  dist2 <- rgeos::gDistance(raster_points_transformed[10001:20000,], 
                            water_poly_transformed, 
                            byid=T)
  
  dist3 <- rgeos::gDistance(raster_points_transformed[20001:30220,], 
                            water_poly_transformed, 
                            byid=T)
  
  
  min_distances <- c(apply(dist1,2,min),apply(dist2,2,min),apply(dist3,2,min)) 
  data.frame(min_distances)
  
  raster_points_df <- sp::SpatialPointsDataFrame(raster_points, 
                                                 data=data.frame(min_distances))
  
  
  crs2 <- raster::projection(clim)
  raster_points_df_backtransformed <- sp::spTransform(raster_points_df, 
                                                      crs2)
  
  dist_raster <- raster::rasterFromXYZ(raster_points_df_backtransformed)
  
  #raster::projection(dist_raster)
  #raster::projection(clim)
  #raster::plot(dist_raster)
  #raster::plot(clim)
  
  names(dist_raster) <- 'distance_to_water'
  raster::writeRaster(dist_raster, 
                      'data/environmental_data/distance_to_water.grd',
                      format = 'raster', 
                      options = 'INTERLEAVE=BAND',
                      overwrite = TRUE)
  
  distance_to_water <- dist_raster
}else {
  distance_to_water <- raster::raster(
    'data/environmental_data/distance_to_water.grd')
}



# Calculate distance to closest landfill ----------------------------------

if (!file.exists('data/environmental_data/distance_to_landfill.grd')) {
  landfills <- sf::st_read(
    'data/environmental_data/clc2018_vector/clc2018.gpkg',
    query = "SELECT * FROM clc2018 
    WHERE Code_18 == 132")
  
  plot(sf::st_geometry(landfills),  axes = TRUE)
  landfills
  
  ## load only the first
  clim <- raster::raster('data/environmental_data/clim.grd')
  
  
  
  
  raster_points <-  as(clim,"SpatialPoints")
  landfills_poly <- as(landfills, "Spatial")
  
  landfills_poly@proj4string
  raster_points@proj4string
  
  
  
  crs1 <- sp::CRS('+proj=laea 
                    +lat_0=52 
                    +lon_0=10 
                    +x_0=4321000 
                    +y_0=3210000 
                    +ellps=GRS80 
                    +units=m 
                    +datum=WGS84 +no_defs')
  
  raster_points_transformed <- sp::spTransform(raster_points, crs1)
  plot(sf::st_as_sf(raster_points_transformed), cex=0.2, pch=15, axes=T)
  
  
  landfills_poly_transformed <- sp::spTransform(landfills_poly, crs1)
  plot(sf::st_as_sf(landfills_poly_transformed),  axes = TRUE)
  
  
  dist1 <- rgeos::gDistance(raster_points_transformed[1:10000,], 
                            landfills_poly_transformed, 
                            byid=T)
  
  dist2 <- rgeos::gDistance(raster_points_transformed[10001:20000,], 
                            landfills_poly_transformed, 
                            byid=T)
  
  dist3 <- rgeos::gDistance(raster_points_transformed[20001:30220,], 
                            landfills_poly_transformed, 
                            byid=T)
  
  
  
  
  min_distances <- c(apply(dist1,2,min),
                     apply(dist2,2,min),
                     apply(dist3,2,min)) 
  data.frame(min_distances)
  
  raster_points_df <- sp::SpatialPointsDataFrame(
    raster_points, 
    data=data.frame(min_distances))
  
  
  crs2 <- raster::projection(clim)
  raster_points_df_backtransformed <- sp::spTransform(raster_points_df, 
                                                      crs2)
  
  dist_raster <- raster::rasterFromXYZ(raster_points_df_backtransformed)
  
  
  #raster::projection(dist_raster)
  #raster::projection(clim)
  #raster::plot(log(dist_raster+1))
  #raster::plot(clim)
  
  names(dist_raster) <- 'distance_to_landfill'
  
  raster::writeRaster(dist_raster, 
                      'data/environmental_data/distance_to_landfill.grd',
                      format = 'raster', 
                      options = 'INTERLEAVE=BAND',
                      overwrite = TRUE)
  
  distance_to_landfill <- dist_raster
} else {
  distance_to_landfill <- raster::raster(
    'data/environmental_data/distance_to_landfill.grd')
}



# Save all site covariates as one raster stack ----------------------------

variables <- raster::stack(clim, 
                           lc_tree_cover, 
                           lc_herb_cover,
                           lc_bare_soil,
                           distance_to_water,
                           distance_to_landfill)



raster::writeRaster(variables, 
            'data/environmental_data/variables_spain.grd',
            format = 'raster', 
            options = 'INTERLEAVE=BAND',
            overwrite = TRUE)

# rm(clim, lc_tree_cover, lc_herb_cover, lc_bare_soil,
#   distance_to_landfill, distance_to_water,
#   spain, spain_crop, variables)




# Changes in temperature and precipitation --------------------------------
prec_change <- raster::raster(
  'data/environmental_data/climate_change/precipitation.tiff')
temp_change <- raster::raster(
  'data/environmental_data/climate_change/temperature.tiff')


### Precipitation
prec_change <- raster::crop(prec_change, raster::extent(c(-12,5,30,50)))


#### edge length of one raster cell in km
prec_change_crs_m <- raster::projectRaster(
  prec_change, 
  crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000
  +y_0=3210000 +ellps=GRS80 +units=m +no_defs') 
poly1 <- raster::rasterToPolygons(prec_change_crs_m)
sqrt(raster::area(poly1[1,])) / 1000

prec_change_rs <- raster::resample(prec_change, clim, method = 'bilinear')
prec_change_rs <- raster::crop(prec_change_rs, spain_crop)
prec_change_rs <- raster::mask(prec_change_rs, spain_crop)

### Temperature
temp_change <- raster::crop(temp_change, raster::extent(c(-12,5,30,50)))

temp_change_rs <- raster::resample(temp_change, clim, method = 'bilinear')
temp_change_rs <- raster::crop(temp_change_rs, spain_crop)
temp_change_rs <- raster::mask(temp_change_rs, spain_crop)



# Plot the changes in a map -----------------------------------------------
pdf('results/climate_change.pdf', width=15, height=7)
par(mfrow=c(1,2), 
    oma = c(3, 4, 1, 2) + 0.1,
    mar = c(0, 4, 4, 2) + 0.1)
pal1 = colorRampPalette(c('white', 'orange', 'red'))
raster::plot(temp_change_rs, main='Temperature change',
             col = pal1(10), 
             legend.args = list(text = '°C', side = 3, 
                                font = 2, line = 1, cex = 0.8))

pal2 = colorRampPalette(c('red', 'orange', 'yellow'))
raster::plot(prec_change_rs, main='Change in\nannual precipitation',
             col = pal2(30),
             legend.args = list(text = '%', side = 3,
                                font = 2, line = 1, cex = 0.8))

dev.off()
par(mfrow=c(1,1))


# Save file as csv --------------------------------------------------------
points <- raster::rasterToPoints(raster::brick(temp_change_rs, 
                                               prec_change_rs))
change <- data.frame(points)

# avoid precision loss when saving the data frame
change$x <- sprintf("%.20f",change$x)
change$y <- sprintf("%.20f",change$y)

write.csv(change, "results/change_temp_prec.csv", row.names = FALSE)


# Clean up ----------------------------------------------------------------
rm(list = ls()) 

