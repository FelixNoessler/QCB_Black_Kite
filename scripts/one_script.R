

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




library(dplyr)



variables <- raster::stack('data/environmental_data/variables_spain.grd')

#raster::plot(variables$landfills)



### Bird data
milmig <- readr::read_csv('data/milmig.csv')

milmig <- milmig %>%
  filter(!state_code=='ES-CN')


#### Join
occ_var <- milmig %>% 
  cbind(as.data.frame(
    raster::extract(variables, milmig[,c('longitude', 'latitude')] ,cellnumbers=T))) %>%
  tidyr::drop_na(bio1, tree_cover)

summary(occ_var)
n_distinct(occ_var$site)
  
#### Standardize
occ_var_std <- occ_var%>% 
  dplyr::mutate_at(c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 
                     'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12', 
                     'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 
                     'bio18', 'bio19',
                     'tree_cover', 'grass_cover', 'bare_soil', 'landfills'),
                   ~(scale(.) %>% as.vector))


sd(occ_var_std$bare_soil)


occ_var_std$observer_id
#X11()
#psych::pairs.panels(select(occ_var,c(24:45)), main = "Black kite 2019")

names(occ_var_std)


occ_wide <- auk::format_unmarked_occu(
  occ_var_std, site_id = 'site',
  response = 'species_observed',
  site_covs =c('cells', 'n_observations', 'latitude', 'longitude',
               'bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 
               'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12', 
               'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 
               'bio18', 'bio19', 
               'tree_cover', 'grass_cover', 'bare_soil', 'landfills'), 
  obs_covs =c('time_observations_started','duration_minutes', 
              'effort_distance_km','number_observers', 'protocol_type'))




######### Convert the detection histories in 1=# presence/ 0= absence instead of# TRUE/FALSE
cols <- sapply(occ_wide, is.logical)#Select columns that are logical (TRUe/FALSE)
occ_wide[, cols] <-lapply(occ_wide[, cols], as.numeric)# Transform to numeric; 1= TRUE, 0= FALSE.

####### Spatial subsampling
# We can only have one observation per one grid cell! 
# (not to confuse with detectability)
duplicated(occ_wide$cells)
occ_wide_clean <- occ_wide[!duplicated(occ_wide$cells),]

# Part that is removed:
1- nrow(occ_wide_clean)/nrow(occ_wide)

# Save variables
write.csv(occ_wide_clean, "results/milmig.csv", row.names = FALSE)

####### Unmarked format
# Store it in unmarked format, do all scaling and subsampling before!
occ_um <- unmarked::formatWide(occ_wide_clean, type = "unmarkedFrameOccu")


library(dplyr)



variables <- raster::stack('data/environmental_data/variables_spain.grd')

#raster::plot(variables$landfills)



### Bird data
milmig <- readr::read_csv('data/milmig.csv')

milmig <- milmig %>%
  filter(!state_code=='ES-CN')


#### Join
occ_var <- milmig %>% 
  cbind(as.data.frame(
    raster::extract(variables, milmig[,c('longitude', 'latitude')] ,cellnumbers=T))) %>%
  tidyr::drop_na(bio1, tree_cover)

summary(occ_var)
n_distinct(occ_var$site)
  
#### Standardize
occ_var_std <- occ_var%>% 
  dplyr::mutate_at(c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 
                     'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12', 
                     'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 
                     'bio18', 'bio19',
                     'tree_cover', 'grass_cover', 'bare_soil', 'landfills'),
                   ~(scale(.) %>% as.vector))


sd(occ_var_std$bare_soil)


occ_var_std$observer_id
#X11()
#psych::pairs.panels(select(occ_var,c(24:45)), main = "Black kite 2019")

names(occ_var_std)


occ_wide <- auk::format_unmarked_occu(
  occ_var_std, site_id = 'site',
  response = 'species_observed',
  site_covs =c('cells', 'n_observations', 'latitude', 'longitude',
               'bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 
               'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12', 
               'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 
               'bio18', 'bio19', 
               'tree_cover', 'grass_cover', 'bare_soil', 'landfills'), 
  obs_covs =c('time_observations_started','duration_minutes', 
              'effort_distance_km','number_observers', 'protocol_type'))




######### Convert the detection histories in 1=# presence/ 0= absence instead of# TRUE/FALSE
cols <- sapply(occ_wide, is.logical)#Select columns that are logical (TRUe/FALSE)
occ_wide[, cols] <-lapply(occ_wide[, cols], as.numeric)# Transform to numeric; 1= TRUE, 0= FALSE.

####### Spatial subsampling
# We can only have one observation per one grid cell! 
# (not to confuse with detectability)
duplicated(occ_wide$cells)
occ_wide_clean <- occ_wide[!duplicated(occ_wide$cells),]

# Part that is removed:
1- nrow(occ_wide_clean)/nrow(occ_wide)

# Save variables
write.csv(occ_wide_clean, "results/milmig.csv", row.names = FALSE)

####### Unmarked format
# Store it in unmarked format, do all scaling and subsampling before!
occ_um <- unmarked::formatWide(occ_wide_clean, type = "unmarkedFrameOccu")


occ_wide_clean <- read.csv("results/milmig.csv")


selection <- occ_wide_clean[, c(16:37)]

cor1 <- abs(as.dist(cor(selection)))

clust1 <- hclust(1- cor1)

## Cluster dendrogram
plot(clust1)

## Correlation plots
psych::pairs.panels(selection[, c('bio1', 'bio2', 'bio12', 'tree_cover', 'grass_cover', 'bare_soil')])

## Test for collinearity
HH::vif(selection[, c('bio1', 'bio2', 'bio12', 'tree_cover', 'grass_cover', 'bare_soil')])


library(dplyr)
library(unmarked)

# Load data ---------------------------------------------------------------
occ_um <- unmarked::formatWide(read.csv("results/milmig.csv"), type = "unmarkedFrameOccu")
unmarked::summary(occ_um)

# Model selection ---------------------------------------------------------

## Null model
occ_null <- unmarked::occu(~1~1, occ_um)
unmarked::summary(occ_null)

## Model only with the detection covariates
detection_cov_model <- unmarked::occu(~duration_minutes
                                      + effort_distance_km
                                      + number_observers
                                      + protocol_type
                                      + time_observations_started
                                      ~1, data=occ_um)
unmarked::summary(detection_cov_model)

## Model only with site covariates
site_cov_model <-unmarked::occu(~1
                                ~bio1
                                +bio2
                                +bio12
                                +tree_cover
                                +grass_cover
                                +bare_soil
                                +landfills, data = occ_um)
unmarked::summary(site_cov_model)

## Full model
full_model <- unmarked::occu(~ duration_minutes 
                             + number_observers
                             ~ poly(bio1, 2)
                             + bio2
                             + bio12
                             + poly(tree_cover, 2)
                             + poly(grass_cover, 2)
                             + poly(bare_soil, 2)
                             + landfills, data = occ_um)
unmarked::summary(full_model)


## Model selection with AIC
models_list <-list(Null = occ_null, 
                   detection = detection_cov_model,
                   site = site_cov_model,
                   fullocc = full_model)

un_models <- unmarked::fitList(fits = models_list)
ModSelect <- unmarked::modSel(un_models, nullmod = "Null")
ModSelect


## Model selection with AICc
AICcmodavg::aictab(models_list, second.ord = T)

rm(detection_cov_model, occ_null, site_cov_model, 
   models_list, ModSelect, un_models)


# Goodness of fit test of best model --------------------------------------
GOF <-unmarked::parboot(full_model, nsim=100)
GOF

### Another goodnes of fit test
AICcmodavg::mb.gof.test(full_model, nsim=100, plot.hist = F)

### cHat, overdispersion 
cHat <- GOF@t0 / mean(GOF@t.star)
cHat

### QAICc 
### You should set 'c.hat' to 1 if < 1, but values << 1 might also indicate lack of fit
AICcmodavg::aictab(models_list, c.hat = 1)
# --> it is the same as above, because the cHat value is below one



# Build an average model --------------------------------------------------

## Get the names of the detection covariates
det_terms <- MuMIn::getAllTerms(full_model) %>% 
  purrr::discard(stringr::str_detect, pattern = "psi")

## Get combination of models, detection covariates are always present
occ_dredge <- MuMIn::dredge(full_model, fixed = det_terms)

## Get the best models from the model list
occ_dredge_999 <- MuMIn::get.models(occ_dredge, 
                                    subset = cumsum(weight) <= 0.999)

## Get the average model based on model weights
occ_avg <- MuMIn::model.avg(occ_dredge_999, fit = TRUE, revised.var = TRUE)

## Calculate the AICc for the average model
sum(occ_avg$msTable$AICc * occ_avg$msTable$weight)

## Model coefficients of the average model
t(occ_avg$coefficients)

rm(occ_dredge, occ_dredge_999, det_terms)


# Load environmental data for predictions ---------------------------------

variables <- raster::brick(raster::stack("data/environmental_data/variables_spain.grd"))
variables_selection <- c("bio1",
                         "bio2",
                         "bio12",
                         "tree_cover",
                         "grass_cover", 
                         "bare_soil",
                         "landfills")

variables.sel <- variables[[variables_selection]]

p_variables <- data.frame(rasterToPoints(variables.sel) )
p_variables <- p_variables %>%
  drop_na(tree_cover, bio1)

pred_surface_std <- p_variables %>% 
  mutate_at(c("bio1", "bio2", "bio12",
              "tree_cover", "grass_cover", "bare_soil", 
              "landfills"), ~(scale(.) %>% as.vector))




# Make predictions --------------------------------------------------------


occ_pred <- predict(occ_avg, 
                    newdata = as.data.frame(pred_surface_std[,c("bio1",
                                                                         "bio2",
                                                                         "bio12",
                                                                         "tree_cover", 
                                                                         "grass_cover", 
                                                                         "bare_soil")]), 
                    type = "state")


pred_occ <- dplyr::bind_cols(pred_surface_std, 
                      occ_prob = occ_pred$fit,
                      occ_se = occ_pred$se.fit) %>%
  dplyr::select(x, y, occ_prob, occ_se)


map_proj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

r_pred <- pred_occ %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = map_proj) %>% 
  sf::st_transform(crs = raster::projection(variables.sel[[1]])) %>% 
  raster::rasterize(variables.sel[[1]])


# Select the prediction with the standard error
r_pred <- r_pred[[c("occ_prob", "occ_se")]]


r_pred_proj <- projectRaster(r_pred, crs = map_proj, method = "ngb")
spain <- ne_countries(country = 'spain', scale = "medium", returnclass = "sf")
r_pred_proj_crop <- crop(r_pred_proj, spain)
r_pred_proj_SP  <- mask(r_pred_proj_crop,spain)

r_pred_occu_SP <- subset(r_pred_proj_SP, 1, drop=TRUE)
# Convert the landscape data RasterLayer objects to data frames for ggplot
r_pred_occu_SP.df <- as.data.frame(r_pred_occu_SP, xy = TRUE, na.rm = TRUE)

# Now, we can add the Standard error in the predictions
r_pred_occuSE_SP<-subset(r_pred_proj_SP, 2, drop=TRUE)
# Convert the landscape data RasterLayer objects to data frames for ggplot
r_pred_occuSE_SP.df <- as.data.frame(r_pred_occuSE_SP, xy = TRUE, na.rm = TRUE)

#add Statistic Colum and standardize column names
r_pred_occu_SP.df$Statistic<-"Mean p(Occupancy)"
colnames(r_pred_occu_SP.df)[3]<-"Probability"
r_pred_occuSE_SP.df$Statistic<-"SE"
colnames(r_pred_occuSE_SP.df)[3]<-"Probability"

#combine data.frames
r_pred_comb<-rbind(r_pred_occu_SP.df, r_pred_occuSE_SP.df)
#make Statistic a factor
r_pred_comb$Statistic<-as.factor(r_pred_comb$Statistic)

# Plot the maps
DEOccuMap<-ggplot()+
  geom_tile(data=r_pred_comb, aes(x=x, y=y, fill=Probability, color=Probability))+
  #geom_map(data=nh.poly, map=nh.poly, aes(y=lat, x=long, map_id=id),color="black",fill="transparent",alpha=0.8, inherit.aes = FALSE)+
  scale_fill_viridis_c(name="p(occupancy)")+
  scale_color_viridis_c(name="p(occupancy)")+
  theme(panel.border=element_rect(color="black",fill="transparent"))+
  labs(x="Longitude",y="Latitude")+
  guides(alpha="none", color="none")+
  coord_fixed()
DEOccuMap.facet<-DEOccuMap+facet_grid(~Statistic)
DEOccuMap.facet
ggsave('results/average_model.png', DEOccuMap.facet, width = 16)

ggsave('facet.png', DEOccuMap.facet)







###### Full model prediction (without averaging)
occ_full_model_prediction <- unmarked::predict(full_model, 
                                     newdata = as.data.frame(pred_surface_std[,c("bio1",
                                                                                 "bio2",
                                                                                 "bio12",
                                                                                 "tree_cover", 
                                                                                 "grass_cover", 
                                                                                 "bare_soil")]), 
                                     type = "state")

pred_full_model <- bind_cols(pred_surface_std, 
                      occ_prob = occ_full_model_prediction$Predicted,
                      occ_se = occ_full_model_prediction$SE) %>%
  dplyr::select(x, y, occ_prob, occ_se)


map_proj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

r_pred <- pred_full_model %>% 
  # convert to spatial features
  st_as_sf(coords = c("x", "y"), crs = map_proj) %>% 
  st_transform(crs = raster::projection(variables.sel[[1]])) %>% 
  # rasterize
  rasterize(variables.sel[[1]])
# Select the prediction with the standard error
r_pred <- r_pred[[c("occ_prob", "occ_se")]]



# project predictions
r_pred_proj <- projectRaster(r_pred, crs = map_proj, method = "ngb")
spain <- ne_countries(country = 'spain', scale = "medium", returnclass = "sf")
r_pred_proj_crop<- crop(r_pred_proj, spain)
r_pred_proj_SP <-mask(r_pred_proj_crop,spain)

#First, with occupancy
r_pred_occu_SP <- subset(r_pred_proj_SP, 1, drop=TRUE)
# Convert the landscape data RasterLayer objects to data frames for ggplot
r_pred_occu_SP.df <- as.data.frame(r_pred_occu_SP, xy = TRUE, na.rm = TRUE)

# Now, we can add the Standard error in the predictions
r_pred_occuSE_SP<-subset(r_pred_proj_SP, 2, drop=TRUE)
# Convert the landscape data RasterLayer objects to data frames for ggplot
r_pred_occuSE_SP.df <- as.data.frame(r_pred_occuSE_SP, xy = TRUE, na.rm = TRUE)

#add Statistic Colum and standardize column names
r_pred_occu_SP.df$Statistic<-"Mean p(Occupancy)"
colnames(r_pred_occu_SP.df)[3]<-"Probability"
r_pred_occuSE_SP.df$Statistic<-"SE"
colnames(r_pred_occuSE_SP.df)[3]<-"Probability"

#combine data.frames
r_pred_comb<-rbind(r_pred_occu_SP.df, r_pred_occuSE_SP.df)
#make Statistic a factor
r_pred_comb$Statistic<-as.factor(r_pred_comb$Statistic)

# Plot the maps
DEOccuMap<-ggplot()+
  geom_tile(data=r_pred_comb, aes(x=x, y=y, fill=Probability, color=Probability))+
  #geom_map(data=nh.poly, map=nh.poly, aes(y=lat, x=long, map_id=id),color="black",fill="transparent",alpha=0.8, inherit.aes = FALSE)+
  scale_fill_viridis_c(name="p(occupancy)")+
  scale_color_viridis_c(name="p(occupancy)")+
  theme(panel.border=element_rect(color="black",fill="transparent"))+
  labs(x="Longitude",y="Latitude")+
  guides(alpha="none", color="none")+
  coord_fixed()
DEOccuMap.facet<-DEOccuMap+facet_grid(~Statistic)
DEOccuMap.facet
ggsave('results/fullmodel.png', DEOccuMap.facet, width = 16)






##### Effect of sites covariates

variable <- 'grass_cover'
newdata <- data.frame(bio1= seq(0,0,length.out=100), # -7, 186
                      bio2= seq(0,0,length.out=100), # 58, 129
                      bio12= seq(0,0,length.out=100), # 223, 1539
                      bare_soil = seq(0,0,length.out=100),  # 0, 60
                      tree_cover= seq(0,0,length.out=100), # 0, 82
                      grass_cover= seq(0,71,length.out=100)) # 0, 71


sd1 <- sd(newdata[, variable])
mean1<- mean(newdata[, variable]/sd(newdata[, variable]))


newdata_std <- newdata
newdata_std[, variable] <- as.numeric(scale(newdata[, variable]) )


predict_newdataset <- unmarked::predict(occ_avg,
                                        newdata = newdata_std,
                                        type="state") 

predict_newdataset_df <- bind_cols(newdata_std, 
                                   occ_prob = predict_newdataset$fit,
                                   occ_se = predict_newdataset$se.fit)

ggplot(data=predict_newdataset_df)+
  geom_line(aes(x=(grass_cover+mean1) *sd1, y=occ_prob),color="royalblue3")+
  labs(x="Grass cover %", y="Occupancy Probability")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        panel.background = element_rect(fill="white"),
        text = element_text(size=20))

ggsave('results/grass_cover.png')



##### Effect of detection covariates
newdata_detection <- data.frame(number_observers = seq(1,5,length.out = 100), # 1,5
                                duration_minutes = seq(0,0, lenght.out = 100)) # 1, 300

predict_newdataset <- unmarked::predict(occ_avg,
                              newdata = newdata_detection,
                              type="det") 

predict_newdataset_df <- bind_cols(newdata_detection, 
                                   occ_prob = predict_newdataset$fit,
                                   occ_se = predict_newdataset$se.fit)

ggplot(data=predict_newdataset_df)+
  geom_line(aes(x=number_observers, y=occ_prob),color="royalblue3")+
  labs(x="Number of observers", y="Occupancy Probability")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        panel.background = element_rect(fill="white"),
        text = element_text(size=20))

ggsave('results/observers.png')
