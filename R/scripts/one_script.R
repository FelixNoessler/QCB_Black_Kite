
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
  
  lc_herbs1 <- raster::raster('data/environmental_data/herbaceous_vegetation1.tif')
  lc_herbs1 <- raster::crop(lc_herbs1, spain_crop)
  lc_herbs1 <- raster::mask(lc_herbs1, spain_crop)
  
  lc_herbs2 <- raster::raster('data/environmental_data/herbaceous_vegetation2.tif')
  lc_herbs2 <- raster::crop(lc_herbs2, spain_crop)
  lc_herbs2 <- raster::mask(lc_herbs2, spain_crop)
  
  lc_herbs3 <- raster::raster('data/environmental_data/herbaceous_vegetation3.tif')
  lc_herbs3 <- raster::crop(lc_herbs3, spain_crop)
  lc_herbs3 <- raster::mask(lc_herbs3, spain_crop)
  
  lc_herbs4 <- raster::raster('data/environmental_data/herbaceous_vegetation4.tif')
  lc_herbs4 <- raster::crop(lc_herbs4, spain_crop)
  lc_herbs4 <- raster::mask(lc_herbs4, spain_crop)
  
  lc_herbs <- raster::merge(lc_herbs1, lc_herbs2, lc_herbs3, lc_herbs4)
  
  # raster::plot(lc_herbs)
  
  lc_herb_cover <- raster::resample(lc_herbs, clim, method = 'bilinear')
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

# ######## Dump sites / Landfills
# 
# 
# if (!file.exists('data/environmental_data/clc2018_raster/landfills.grd')){
#   clc <- raster::raster('data/environmental_data/clc2018_raster/clc_reproject.tif')
#   
#   clc <- raster::crop(clc, spain_crop)
#   clc <- raster::mask(clc, spain_crop)
#   
#   raster::plot(clc)
#   
#   
#   dump_sites <- clc == 8
#   raster::plot(dump_sites)
#   
#   dump_sites_agg <- raster::aggregate(dump_sites, 
#                                       fact = 32, 
#                                       fun = mean)
#   dump_sites_agg
#   raster::plot(dump_sites_agg)
#   
#   dump_sites_rs <- raster::resample(dump_sites_agg, clim, method = 'bilinear')
#   names(dump_sites_rs) <- 'landfills'
#   
#   raster::writeRaster(dump_sites_rs, 
#                       'data/environmental_data/clc2018_raster/landfills.grd',
#                       format = 'raster', 
#                       options = 'INTERLEAVE=BAND',
#                       overwrite = TRUE)
#   
#   rm(dump_sites, dump_sites_agg, clc)
# } else {
#   
#   dump_sites_rs <- raster::raster('data/environmental_data/clc2018_raster/landfills.grd')
# }

# if (!file.exists('data/environmental_data/marshes.grd')) {
#   marshes <- sf::st_read('data/environmental_data/clc2018_vector/clc2018.gpkg',
#                          query = "SELECT * FROM clc2018 
#                       WHERE Code_18 == 411
#                       OR Code_18 == 421
#                       OR Code_18 == 511
#                       OR Code_18 == 512
#                       OR Code_18 == 521
#                       OR Code_18 == 522
#                       OR Code_18 == 523")
#   
#   plot(sf::st_geometry(marshes),  axes = TRUE)
#   
#   clim <- raster::brick('data/environmental_data/clim.grd')
#   
#   ########## Method 1
#   out <- raster::rasterize(marshes, clim[[1]], getCover = T)
#   out <- raster::mask(out, clim[[1]])
#   raster::plot(out)
#   
#   ########## Method 2
#   fractions <- exactextractr::coverage_fraction(clim[[1]], marshes)
#   
#   marshes_raster <- raster::raster(clim[[1]])
#   
#   for (i in 1:length(fractions)) {
#     marshes_raster <- raster::mosaic(
#       marshes_raster, 
#       fractions[[i]], 
#       fun=sum)
#   }
#   marshes_raster <- raster::mask(marshes_raster, clim[[1]])
#   raster::plot(marshes_raster)
#   
#   names(marshes_raster) <- 'marshes'
#   
#   raster::writeRaster(marshes_raster, 
#                       'data/environmental_data/marshes.grd',
#                       format = 'raster', 
#                       options = 'INTERLEAVE=BAND',
#                       overwrite = TRUE)
# } else {
#   marshes_raster <- raster::raster('data/environmental_data/marshes.grd')
# }




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
  
  raster_points_df <- sp::SpatialPointsDataFrame(raster_points, data=data.frame(min_distances))
  
  
  crs2 <- raster::projection(clim)
  raster_points_df_backtransformed <- sp::spTransform(raster_points_df, crs2)
  
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
  distance_to_water <- raster::raster('data/environmental_data/distance_to_water.grd')
}



# Calculate distance to closest landifll ----------------------------------

if (!file.exists('data/environmental_data/distance_to_landfill.grd')) {
  landfills <- sf::st_read('data/environmental_data/clc2018_vector/clc2018.gpkg',
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
  
  
  
  
  min_distances <- c(apply(dist1,2,min),apply(dist2,2,min),apply(dist3,2,min)) 
  data.frame(min_distances)
  
  raster_points_df <- sp::SpatialPointsDataFrame(raster_points, data=data.frame(min_distances))
  
  
  crs2 <- raster::projection(clim)
  raster_points_df_backtransformed <- sp::spTransform(raster_points_df, crs2)
  
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
  distance_to_landfill <- raster::raster('data/environmental_data/distance_to_landfill.grd')
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
prec_change <- raster::raster('data/environmental_data/climate_change/precipitation.tiff')
temp_change <- raster::raster('data/environmental_data/climate_change/temperature.tiff')


### Precipitation
prec_change <- raster::crop(prec_change, raster::extent(c(-12,5,30,50)))


#### edge length of one raster cell in km
prec_change_crs_m <- raster::projectRaster(prec_change, crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs') 
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
points <- raster::rasterToPoints(raster::brick(temp_change_rs, prec_change_rs))
change <- data.frame(points)

# avoid precision loss when saving the data frame
change$x <- sprintf("%.20f",change$x)
change$y <- sprintf("%.20f",change$y)

write.csv(change, "results/change_temp_prec.csv", row.names = FALSE)


# Clean up ----------------------------------------------------------------
rm(list = ls()) 

##########################################################
# Second script
#
#     1. Join the site covariates with the eBird data set
#     2. Standardize the site covariates
#     3. Do spatial subsampling
#     4. Store the data frame in the unmarked format
#
##########################################################


# Loading packages --------------------------------------------------------
library(dplyr)


# Load data ---------------------------------------------------------------

### Site covariates
variables <- raster::stack('data/environmental_data/variables_spain.grd')

### Bird data
milmig <- readr::read_csv('data/milmig.csv')

# filter for observations from the mainland of spain
# milmig <- milmig %>%
#  filter(!state_code=='ES-CN')


# Join eBird data and site covariates -------------------------------------
occ_var <- milmig %>% 
  cbind(as.data.frame(
    raster::extract(variables, 
                    milmig[,c('longitude', 'latitude')],
                    cellnumbers=T))) %>%
  tidyr::drop_na(bio1, tree_cover)

## save a data frame with not standardized covariates
write.csv(occ_var, "results/milmig_not_std.csv", row.names = FALSE)


# Standardize site covariates ---------------------------------------------

occ_var_std <- occ_var%>% 
  dplyr::mutate_at(c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 
                     'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12', 
                     'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 
                     'bio18', 'bio19',
                     'tree_cover', 'grass_cover', 'bare_soil',
                     'distance_to_water', 'distance_to_landfill'),
                   ~(scale(.) %>% as.vector))


# Convert data frame to unmarked format -----------------------------------
occ_wide <- auk::format_unmarked_occu(
  occ_var_std, site_id = 'site',
  response = 'species_observed',
  site_covs =c('cells', 'n_observations', 'latitude', 'longitude',
               'bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 
               'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12', 
               'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 
               'bio18', 'bio19', 
               'tree_cover', 'grass_cover', 'bare_soil',
               'distance_to_water', 'distance_to_landfill'), 
  obs_covs =c('time_observations_started','duration_minutes', 
              'effort_distance_km','number_observers', 'protocol_type',
              'day_of_year'))


### Convert the detection histories in 1=# presence/ 0= absence 
# instead of TRUE/FALSE
cols <- sapply(occ_wide, is.logical)
occ_wide[, cols] <-lapply(occ_wide[, cols], as.numeric)


# Do spatial subsampling --------------------------------------------------
# We can only have one (3 - 10 times repeated) observation per one grid cell! 
occ_wide_clean <- occ_wide[!duplicated(occ_wide$cells),]

# Part that is removed:
1- nrow(occ_wide_clean)/nrow(occ_wide)


# Save unmarked data as csv -----------------------------------------------
write.csv(occ_wide_clean, "results/milmig.csv", row.names = FALSE)


# Clean up ----------------------------------------------------------------
rm(list = ls()) 
##########################################################
# Third script
#
#  Selection of covariates:
#     ... are based on ecological hypotheses and from
#         from the explanatory analysis with a Random
#         Forest model
#
#     --> here some covariates are excluded because of
#         collinerarity
#
##########################################################


# Load data ---------------------------------------------------------------
occ_wide_clean <- read.csv("results/milmig.csv")


# Selected covariates -----------------------------------------------------
random_forest_selection <- c('bio3', 'bio4', 'bio7', 'bio5')

ecological_selection <- c('bio1', 'bio12', 
                          'tree_cover', 'grass_cover', 'bare_soil', 
                          'distance_to_landfill',  'distance_to_water')

selected_covariates <- c(random_forest_selection, ecological_selection)
selection <- occ_wide_clean[, selected_covariates]


# Exclude some covariates -------------------------------------------------
selected_names <- names(selection)[! names(selection) %in% c('bio7', 'bio4', 'bio5')]


# Make a cluster dendrogram -----------------------------------------------
## cluster with all selected covariates
cor1 <- abs(as.dist(cor(selection)))
clust1 <- hclust(1- cor1)
plot(clust1)

## cluster with some covariates removed
cor1 <- abs(as.dist(cor(selection[, selected_names])))
clust1 <- hclust(1- cor1)
plot(clust1)


# Correlation plots -------------------------------------------------------
## Correlation plots with all covariates
psych::pairs.panels(selection)

## Correlation plots with some covariates removed
psych::pairs.panels(selection[, selected_names])


# Test for collinearity ---------------------------------------------------
## Test for collinearity with all covariates
HH::vif(selection)

## Test for collinearity with some covariates removed
HH::vif(selection[, selected_names])


# Make maps of the final selected covariates ------------------------------
site_covariates <- raster::brick('data/environmental_data/variables_spain.grd')
site_covariates <- site_covariates[[selected_names]]

pdf('results/site_covs.pdf')
par(mfrow=c(1,1), 
    oma = c(0, 0, 0, 1) + 0.1,
    mar = c(0, 4, 10, 2) + 0.1)
raster::plot(site_covariates, main=c('Isothermality', 
                                      'Annual mean\ntemperature °C *10',
                                      'Annual mean\nprecipitation mm',
                                      'Tree cover %',
                                      'Grass cover %',
                                      'Bare soil cover %',
                                      'Distance to closest\nlandfill m',
                                      'Distance to closest\nriver or lake m'))
dev.off()



# Clean up ----------------------------------------------------------------
rm(list = ls()) 
dev.off(dev.list()["RStudioGD"])
##########################################################
# Fourth script
#
# Build models with the unmarked package
#
# --> Compare different models with 
#     information criterion
#
# --> evaluate the best model
#
# --> build an average model
#
##########################################################

# 
setwd("/home/felix/Dokumente/studium/Potsdam/Module/Biogeography/Black kite/R")

# Load packages -----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(unmarked)

# Load data ---------------------------------------------------------------
occ_um <- formatWide(read.csv("results/milmig.csv"), type = "unmarkedFrameOccu")
summary(occ_um)


# Build models ------------------------------------------------------------
## Null model
occ_null <- occu(~ 1 ~ 1, occ_um)
summary(occ_null)
backTransform(occ_null, "state")

## Model only with detection covariates
detection_cov_model <- occu(~ duration_minutes
                                      + effort_distance_km
                                      ~ 1, data=occ_um)
summary(detection_cov_model)


## Model only with site covariates
site_cov_model <-occu(~ 1
                                    ~ poly(bio1, 2)
                                    + poly(bio3, 2)
                                    + poly(bio12, 2)
                                    + poly(tree_cover, 2)
                                    + poly(grass_cover, 2)
                                    + poly(bare_soil, 2)
                                    + poly(distance_to_water, 2)
                                    + poly(distance_to_landfill, 2)
                                    , data = occ_um)
summary(site_cov_model)




## Full model with covariates
full_model <- occu(~ duration_minutes 
                                 + number_observers
                                 ~ poly(bio1, 2)
                                 + poly(bio3, 2)
                                 + poly(bio12, 2)
                                 + poly(tree_cover, 2)
                                 + poly(grass_cover, 2)
                                 + poly(bare_soil, 2)
                                 + poly(distance_to_water, 2)
                                 + poly(distance_to_landfill, 2), data = occ_um)
summary(full_model)

re <- ranef(full_model)
sum(bup(re, stat="mode"))
sum(bup(re, stat="mean"))


# Model selection ---------------------------------------------------------
## Model selection with AIC
models_list <-list(Null = occ_null, 
                   detection = detection_cov_model,
                   site = site_cov_model,
                   full_model = full_model)

un_models <- fitList(fits = models_list)
ModSelect <- modSel(un_models, nullmod = "Null")
ModSelect


## Model selection with AICc
AICcmodavg::aictab(models_list, second.ord = T)


best_model <- full_model



if (!file.exists('models.rda')) {

# Goodness of fit test of best model --------------------------------------
GOF <- parboot(best_model, nsim=500, ncores=8, report=T)
GOF

cHat <- GOF@t0 / mean(GOF@t.star)
cHat

### Another goodnes of fit test
AICcmodavg::mb.gof.test(best_model,
                        nsim=500,
                        plot.hist = F,
                        parallel=T,
                        ncores=8)



### QAICc
GOF1 <- AICcmodavg::aictab(models_list, c.hat = 1)
# --> it is the same as above, because the cHat value is below one


# Build an average model --------------------------------------------------
  ## Get the names of the detection covariates
  det_terms <- MuMIn::getAllTerms(best_model) %>% 
    purrr::discard(stringr::str_detect, pattern = "psi")
  
  ## Get combination of models, detection covariates are always present
  occ_dredge <- MuMIn::dredge(best_model, fixed = det_terms)
  
  ## Get the best models from the model list
  occ_dredge_95 <- MuMIn::get.models(occ_dredge, 
                                      subset = cumsum(weight) <  0.95)
  
  ## Get the average model based on model weights
  #occ_avg <- MuMIn::model.avg(occ_dredge, fit = TRUE, revised.var = TRUE)
  occ_avg <- MuMIn::model.avg(occ_dredge_95, fit=T)
  
  ## Calculate the AICc for the average model
  sum(occ_avg$msTable$AICc * occ_avg$msTable$weight)
  
  ## Model coefficients of the average model
  t(occ_avg$coefficients)
  
  MuMIn::importance(occ_avg)
  
  save(occ_avg, best_model, GOF, cHat, GOF1, file='models.rda')
}

# Clean up ----------------------------------------------------------------
rm(list = ls()) 


##########################################################
# Fifth script
#
#
##########################################################

library(dplyr)
library(ggplot2)

# Load data ---------------------------------------------------------------
load('models.rda')
change <- read.csv('results/change_temp_prec.csv')
variables <- raster::brick("data/environmental_data/variables_spain.grd")


# Prepare data for predictions --------------------------------------------
variables_selection <- c("bio1",
                         "bio3",
                         "bio12",
                         "tree_cover",
                         "grass_cover", 
                         "bare_soil",
                         "distance_to_water",
                         "distance_to_landfill")

variables.sel <- variables[[variables_selection]]

p_variables <- data.frame(raster::rasterToPoints(variables.sel) )
p_variables <- p_variables %>%
  tidyr::drop_na(tree_cover, bio1)

change



change_joined <- p_variables %>% 
  left_join(change, by = c('x', 'y')) %>%
  select(x,y, temperature, precipitation) %>%
  mutate(temperature = temperature * 10,
         precipitation = 1 + precipitation/100)

mean(change_joined$temperature, na.rm=T)/10
mean(change_joined$precipitation, na.rm=T)



p_variables_std <- p_variables %>% 
  mutate_at(variables_selection, ~(scale(.) %>% as.vector))

sd_bio1 <- sd(p_variables$bio1)
mean_bio1 <- mean(p_variables$bio1/ sd(p_variables$bio1))

sd_bio12 <- sd(p_variables$bio12)
mean_bio12 <- mean(p_variables$bio12/ sd(p_variables$bio12))

p_variables_std_future <- p_variables_std
p_variables_std_future$bio1 <- (p_variables$bio1 + change_joined$temperature) / sd_bio1 - mean_bio1
p_variables_std_future$bio12 <- (p_variables$bio12 * change_joined$precipitation) / sd_bio12 - mean_bio12

# Make predictions --------------------------------------------------------
# actual

# occ_avg, best_model
pred_actual <- unmarked::predict(occ_avg,
                                 newdata = select(p_variables_std,
                                                  -x, -y), 
                                 type = "state")

actual_climate <- bind_cols(p_variables_std,  
                                   probability = pred_actual$fit, # Predicted, fit
                                   SE = pred_actual$se.fit) %>% # SE, se.fit
  select(x, y, probability, SE) %>%
  tidyr::pivot_longer(cols = c(probability, SE)) 


# future

# occ_avg, best_model
pred_future <- unmarked::predict(occ_avg,
                                 newdata = select(p_variables_std_future,
                                                  -x, -y), 
                                 type = "state")

future_climate <- bind_cols(p_variables_std_future, 
                                   probability = pred_future$fit, # Predicted, fit
                                   SE = pred_future$se.fit) %>% # SE, se.fit
  select(x, y, probability, SE) %>%
  tidyr::pivot_longer(cols = c(probability, SE)) 

# join the data, preparation for plotting
data <- actual_climate %>%
  inner_join(future_climate, by = c("x", "y", "name")) %>%
  rename(actual = value.x,
         future = value.y,
         type = name)  %>%
  tidyr::pivot_longer(cols=c(actual, future))

# results/predictions_best_model.csv, or results/predictions_avg_model.csv
write.csv(data, 'results/predictions_avg_model.csv', row.names = F) 

#### Plotting the map


# Plot the map ------------------------------------------------------------

# or load data:
# data <- read.csv('results/predictions_best_model.csv')
# data <- read.csv('results/predictions_avg_model.csv')

data %>%
  ggplot(aes(x,y, fill=value))+
  geom_raster()+
  scale_fill_viridis_c(name="value", option="turbo")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        text = element_text(size=20))+
  labs(x="Longitude", y="Latitude")+
  coord_fixed()+
  facet_grid(~name ~ type)+
  theme(text = element_text(size=30, family = "LM Roman 10"))
# 'results/best_model_map.png' or 'results/avg_model_map.png'
ggsave('results/avg_model_map.png', width = 16, height=10)


# Mean probabilities
data %>%  
  group_by(name) %>% 
  filter(type == 'probability') %>%
  summarise(p = mean(value))

# sum of ells occupied
data %>%  
  group_by(name) %>% 
  filter(type == 'probability') %>%
  mutate(occ = value >= 0.5) %>%
  summarise(s = sum(occ))

data %>%
  tidyr::pivot_wider(names_from=name, values_from = value) %>%
  filter(type == 'probability') %>%
  mutate(lower = actual > future) %>%
  mutate(higher = actual < future) %>%
  summarise(lower = sum(lower), higher = sum(higher))

# total grid cells
nrow(data) / 4

# Prediction of covariates ------------------------------------------------

raw_data <- readr::read_csv('results/milmig_not_std.csv')
model_statistics <- readr::read_csv2('results/best_model.csv')

model_labels <- model_statistics %>%
  rename(p = `P(>|z|)`) %>%
  tidyr::pivot_wider(values_from=p, names_from = coef_no, id_cols=name) %>%
  filter(name != '-') %>%
  rename(first = `1`,
         second = `2`) %>%
  mutate(first = ifelse( round(first, 2) == 0, '<0.00', paste('', round(first, 2))),
         second = ifelse(round(second, 2) ==0, '<0.00',  paste('', round(second, 2))),
         p =  paste('p = ',first, ', ', second, sep='')) %>%
  select(name, p) %>%
  mutate(name = recode(name, 
                       bare_soil = "Bare soil cover (%)",
                       tree_cover = "Tree cover (%)",
                       grass_cover = "Grass cover (%)",
                       bio1 = "Annual mean\ntemperature (°C)",
                       bio3 = "Isothermality",
                       bio12 = "Annual precipitation\n(mm)",
                       distance_to_water = "Distance to closest\nriver or lake (km)",
                       distance_to_landfill = "Distance to closest\nlandfill (km)"))%>%
  mutate(x = c(10, 38, 800, 30, 30, 15, 25, 45),
         y = rep(0.1, 8))

variable_names <- c("bio1",
                    "bio3",
                    "bio12",
                    "bare_soil",
                    "tree_cover",
                    "grass_cover",
                    "distance_to_water",
                    "distance_to_landfill")

rm(old_data)
for (i in seq_along(variable_names)) {
  variable_str <- variable_names[i]
  print(variable_str)
  
  
  newdata <- setNames(data.frame(matrix(ncol = length(variable_names), nrow = 1000)), variable_names)
  
  newdata[, i] <- seq(min(raw_data[, variable_str]),
                      max(raw_data[, variable_str]),
                      length.out = 100)
  
  newdata[is.na(newdata)] <- 0
  
  sd1 <- sd(newdata[, variable_str])
  mean1<- mean(newdata[, variable_str]/sd(newdata[, variable_str]))
  
  newdata[, variable_str] <- as.numeric(scale(newdata[, variable_str]) )
  
  
  predict_newdataset <- unmarked::predict(occ_avg, # best_model, occ_avg
                                          newdata = newdata,
                                          type = "state") 
  
  plotting_data <- bind_cols(newdata, 
                             occ_prob = predict_newdataset$fit,# Predicted, fit
                             occ_se = predict_newdataset$se.fit) %>% # SE, se.fit
    select(matches(variable_str), occ_prob, occ_se) 
  
  plotting_data$x <- (plotting_data[, variable_str] + mean1)* sd1
  
  if (variable_str %in% c('bio1', 'bio2')) {
    plotting_data$x <- plotting_data$x / 10
  } else if (variable_str == 'landfills'){
    plotting_data$x <- plotting_data$x * 100
  } else if (variable_str %in% c('distance_to_water', 'distance_to_landfill')){
    plotting_data$x <- plotting_data$x /1000
  }
  
  new_data <- plotting_data %>%
    mutate(lower_se = occ_prob - occ_se,
           upper_se = occ_prob + occ_se) %>%
    select(x, occ_prob, lower_se, upper_se) %>%
    mutate(name = variable_str)
  
  if (i == 1){
    old_data <- new_data
  } else {
    old_data <- old_data %>% 
      bind_rows(new_data)
  }
}



old_data %>%
  mutate(name = recode(name, 
                       bare_soil = "Bare soil cover (%)",
                       tree_cover = "Tree cover (%)",
                       grass_cover = "Grass cover (%)",
                       bio1 = "Annual mean\ntemperature (°C)",
                       bio3 = "Isothermality",
                       bio12 = "Annual precipitation\n(mm)",
                       distance_to_water = "Distance to closest\nriver or lake (km)",
                       distance_to_landfill = "Distance to closest\nlandfill (km)")) %>%
  #filter(name != "Distance to closest\nlandfill (km)") %>%
  ggplot()+
  #geom_text(data = model_labels, aes(x = x, y = y, label = p), family="LM Roman 10", size=3.5)+
  geom_ribbon(aes(ymin = lower_se, 
                  ymax = upper_se,
                  x = x),
              fill="gray", alpha=0.7) + 
  geom_line(aes(x=x, y=occ_prob, color=factor(name)),
            size=0.8)+
  scale_color_manual(values = c("Bare soil cover (%)" = "grey36", 
                                "Annual mean\ntemperature (°C)" = "orangered3",
                                "Distance to closest\nriver or lake (km)" = "royalblue2",
                                "Distance to closest\nlandfill (km)" = "chocolate4", 
                                "Tree cover (%)" = "palegreen3",
                                "Grass cover (%)" = "chartreuse4",
                                "Annual precipitation\n(mm)" = "steelblue4",
                                "Isothermality" = "orange"))+
  labs(x=NULL, 
       y="Occupancy Probability")+
  theme_bw()+
  theme(legend.position = "none",
        panel.border = element_rect(color="black",fill="transparent"),
        text = element_text(size=10, family="LM Roman 10"),
        plot.margin=unit(c(2, 5, -5, 2), "points"),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text =  element_text(size=12, face='plain',
                                   margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title = element_text(size=12, face='plain'))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0,1),expand = expansion(mult = c(0.03, 0.03))) +
  facet_wrap(. ~ name, scales="free_x", strip.position = 'bottom', ncol=4)
# 'results/avg_model_site_covariates.png', 'results/site_covariates.png'
ggsave('results/avg_model_site_covariates.png', width = 7, height = 5)


######### detection covariates

summary(raw_data$duration_minutes)
summary(raw_data$number_observers)

variable_names <- c('number_observers', 'duration_minutes')
newdata = setNames(data.frame(matrix(ncol = length(variable_names), nrow = 2000)), variable_names)
newdata[1:1000, variable_names[1]] <- seq(1, 5, length.out=1000)
newdata[1:1000, variable_names[2]] <- colMeans(raw_data[, variable_names[2]], na.rm=T)

newdata[1001:2000, variable_names[2]] <- seq(1, 300, length.out=1000)
newdata[1001:2000, variable_names[1]] <- colMeans(raw_data[, variable_names[1]], na.rm=T)

best_model

predict_labels <- data.frame(name = c('Number of observers', 'Duration of observation (min)'),
                             p = c('p = 0.22', 'p = 0.01'),
                             x = c(3, 150),
                             y = c(0.1, 0.1))


predict_newdataset <- unmarked::predict(best_model,
                                        newdata = newdata,
                                        type = "det") 

plotting_data <- bind_cols(newdata, 
                           occ_prob = predict_newdataset$Predicted,
                           occ_se = predict_newdataset$SE) %>%
  mutate(lower_se = occ_prob - occ_se,
         upper_se = occ_prob + occ_se,
         name = c(rep('Number of observers', 1000), 
                  rep('Duration of observation (min)', 1000)))

plotting_data$x <- c(plotting_data[1:1000, 'number_observers'], 
                     plotting_data[1001:2000, 'duration_minutes'])

plotting_data %>%
  ggplot()+
  geom_text(data = predict_labels, aes(x = x, y = y, label = p), family="LM Roman 10", size=5)+
  geom_ribbon(aes(ymin = lower_se, 
                  ymax = upper_se,
                  x = x),
              fill="gray", alpha=0.7) + 
  geom_line(aes(x=x, y=occ_prob, color=factor(name)),
            size=0.8)+
  scale_color_manual(values = c("Number of observers" = "orange", 
                                "Duration of observation (min)" = "orangered3"))+
  labs(x=NULL, 
       y="Occupancy Probability")+
  theme_bw()+
  theme(legend.position = "none",
        panel.border = element_rect(color="black",fill="transparent"),
        text = element_text(size=10, family="LM Roman 10"),
        plot.margin=unit(c(2, 5, -5, 2), "points"),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text =  element_text(size=12, face='plain',
                                   margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title = element_text(size=12, face='plain'))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0,1),expand = expansion(mult = c(0.03, 0.03))) +
  facet_wrap(. ~ name, scales="free_x", strip.position = 'bottom', ncol=4)
ggsave('results/det_covariates.png', width = 6, height = 3)