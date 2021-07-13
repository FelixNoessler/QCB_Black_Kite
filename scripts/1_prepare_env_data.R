

library(ggplot2)
library(dplyr)

### Spain
spain <- rnaturalearth::ne_countries(country = 'spain',
                                     scale = 'medium',
                                     returnclass = 'sf')
spain %>%
  ggplot()+
  geom_sf(fill='black')



spain_crop <- rmapshaper::ms_filter_islands(spain, min_area = 100000000000, drop_null_geometries=T)
plot(sf::st_geometry(spain_crop))


### Climate data
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


### Tree cover
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
rm(lc_tree1, lc_tree2, lc_tree3, lc_tree4)

lc_tree_rs <- raster::resample(lc_tree, clim, method = 'bilinear')
names(lc_tree_rs) <- 'tree_cover'

raster::writeRaster(lc_tree_rs, 
            'data/environmental_data/lc_tree.grd',
            format = 'raster', 
            options = 'INTERLEAVE=BAND',
            overwrite = TRUE)

rm(lc_tree_rs, lc_tree)


### Herbaceous vegetation
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
rm(lc_herbs1, lc_herbs2, lc_herbs3, lc_herbs4)

lc_herbs_rs <- raster::resample(lc_herbs, clim, method = 'bilinear')
names(lc_herbs_rs) <- 'grass_cover'

raster::writeRaster(lc_herbs_rs, 
            'data/environmental_data/lc_herbs.grd',
            format = 'raster', 
            options = 'INTERLEAVE=BAND',
            overwrite = TRUE)

rm(lc_herbs_rs, lc_herbs)


### Bare soil
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
rm(lc_bare1, lc_bare2, lc_bare3, lc_bare4)

lc_bare_rs <- raster::resample(lc_bare, clim, method = 'bilinear')
names(lc_bare_rs) <- 'bare_soil'

raster::writeRaster(lc_bare_rs, 
                    'data/environmental_data/lc_bare_soil.grd',
                    format = 'raster', 
                    options = 'INTERLEAVE=BAND',
                    overwrite = TRUE)

rm(lc_bare_rs, lc_bare)

#########

lc_bare_soil <- raster::raster('data/environmental_data/lc_bare_soil.grd')
lc_tree_cover <- raster::raster('data/environmental_data/lc_tree.grd')
lc_herb_cover <- raster::raster('data/environmental_data/lc_herbs.grd')



variables <- raster::stack(clim, 
                           lc_tree_cover, 
                           lc_herb_cover,
                           lc_bare_soil)

rm(clim, lc_tree_cover, lc_herb_cover, lc_bare_soil)

raster::writeRaster(variables, 
            'data/environmental_data/variables_spain.grd',
            format = 'raster', 
            options = 'INTERLEAVE=BAND',
            overwrite = TRUE)



