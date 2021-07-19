

library(ggplot2)
library(dplyr)

### Spain
spain <- rnaturalearth::ne_countries(country = 'spain',
                                     scale = 'medium',
                                     returnclass = 'sf')
spain %>%
  ggplot()+
  geom_sf(fill='black')


spain_crop <- rmapshaper::ms_filter_islands(spain, 
                                            min_area = 100000000000, 
                                            drop_null_geometries=T)
plot(sf::st_geometry(spain_crop))


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
  
  raster::plot(lc_herbs)
  
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
  
  raster::plot(lc_bare)
  
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

#########


######## Dump sites / Landfills


if (!file.exists('data/environmental_data/clc2018_raster/landfills.grd')){
  clc <- raster::raster('data/environmental_data/clc2018_raster/clc_reproject.tif')
  
  clc <- raster::crop(clc, spain_crop)
  clc <- raster::mask(clc, spain_crop)
  
  raster::plot(clc)
  
  
  dump_sites <- clc == 8
  raster::plot(dump_sites)
  
  dump_sites_agg <- raster::aggregate(dump_sites, 
                                      fact = 32, 
                                      fun = mean)
  dump_sites_agg
  raster::plot(dump_sites_agg)
  
  dump_sites_rs <- raster::resample(dump_sites_agg, clim, method = 'bilinear')
  names(dump_sites_rs) <- 'landfills'
  
  raster::writeRaster(dump_sites_rs, 
                      'data/environmental_data/clc2018_raster/landfills.grd',
                      format = 'raster', 
                      options = 'INTERLEAVE=BAND',
                      overwrite = TRUE)
  
  rm(dump_sites, dump_sites_agg, clc)
} else {
  
  dump_sites_rs <- raster::raster('data/environmental_data/clc2018_raster/landfills.grd')
}


#########
variables <- raster::stack(clim, 
                           lc_tree_cover, 
                           lc_herb_cover,
                           lc_bare_soil,
                           dump_sites_rs)

rm(clim, lc_tree_cover, lc_herb_cover, lc_bare_soil, 
   dump_sites_rs, 
   spain, spain_crop)

raster::writeRaster(variables, 
            'data/environmental_data/variables_spain.grd',
            format = 'raster', 
            options = 'INTERLEAVE=BAND',
            overwrite = TRUE)



