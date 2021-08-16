
library(ggplot2)
library(ggnewscale)
library(dplyr)

variables <- raster::stack('data/environmental_data/variables_spain.grd')



#############################
spain <- rnaturalearth::ne_countries(country = 'spain',
                                     scale = 'medium',
                                     returnclass = 'sf')



spain_crop <- rmapshaper::ms_filter_islands(spain, 
                                            min_area = 100000000000, 
                                            drop_null_geometries=T)


##############################
#############################
###########################

#' Rotate simple features for 3D layers
#' Rotates a simple features layer using a shear matrix transformation on the 
#' \code{geometry} column. This can get nice for visualization and works with
#' points, lines and polygons.
#'
#' @param data an object of class \code{sf}
#' @param x_add integer; x value to move geometry in space
#' @param y_add integer; x value to move geometry in space
#'
#' #' @importFrom magrittr %>%

rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}

####################################
##################################
###################################
############################
### Bird data

mil <-read.csv("results/milmig.csv")
mil$det_prob <- rowSums(mil[, 2:11], na.rm = T) / mil$n_observations

mil$duration_minutes <- rowSums(mil[, 52:61], na.rm = T)/ mil$n_observations


#############################
mil_sf <- sf::st_as_sf(mil, coords = c('longitude', 'latitude'), crs = sf::st_crs(spain_crop))

observed_raster <- raster::rasterize(mil_sf, variables[[1]], 'det_prob')
observed_sf <- raster::rasterToPolygons(observed_raster)
observed_sf <- sf::st_as_sf(observed_sf)

duration_raster  <- raster::rasterize(mil_sf, variables[[1]], 'duration_minutes', 'first')
duration_sf <- raster::rasterToPolygons(duration_raster)
duration_sf <- sf::st_as_sf(duration_sf)

water_raster <- raster::rasterize(mil_sf, variables[[1]], 'distance_to_water', 'first')
water_sf <- raster::rasterToPolygons(water_raster)
water_sf <- sf::st_as_sf(water_sf)


annotation <- data.frame(
  x = c(59,59,59),
  y = c(35.8, 28, 19.8),
  label = c("Detection probability",
            "Observational covariate:\n Mean duration of observations",
            "Site covariate:\n Distance to water"))



ggplot()+
  
  geom_segment(aes(x = 38.8, y = 20.3, xend = 38.8, yend = 36.4), size=0.1)+
  geom_segment(aes(x = 35.25225, y = 16, xend = 35.25225, yend = 32.1), size=0.1)+
  geom_segment(aes(x = 63.30569, y = 16.9, xend = 63.30569, yend = 32.8), size=0.1)+
  
  
  geom_sf(data=rotate_sf(spain_crop), fill='white', size=0.3)+
  geom_sf(data=rotate_sf(observed_sf), aes(fill=layer), color=NA)+
  scale_fill_distiller(palette = "YlGn",
                       direction = 1,
                       guide = FALSE) +
  
  new_scale_fill() +
  geom_sf(data=rotate_sf(spain_crop, y_add = -8), fill='white', size=0.3)+
  geom_sf(data=rotate_sf(duration_sf, y_add = -8), aes(fill=layer), color=NA)+
  scale_fill_distiller(palette = "Reds",
                       direction = 1,
                       guide = FALSE) +
  
  new_scale_fill() +
  geom_sf(data=rotate_sf(spain_crop, y_add = -16), fill='white', size=0.3)+
  geom_sf(data=rotate_sf(water_sf, y_add = -16), aes(fill=layer), color=NA)+
  scale_fill_distiller(palette = "Blues",
                       direction = -1,
                       guide = FALSE)+
  
  geom_label(data=annotation, aes( x=x, y=y, label=label),
             color="black", 
             size=9, label.size=0,family="LM Roman 10")+
  
  theme(legend.position = "none") +
  ggsn::blank()

ggsave('results/visualization_occupancy_model.png', width=18, height=15)

