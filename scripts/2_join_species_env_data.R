

library(ggplot2)
library(dplyr)



variables <- raster::stack('data/environmental_data/variables_spain.grd')

raster::plot(variables$grass_cover)



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
                     'tree_cover', 'grass_cover', 'bare_soil'),
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
               'bio18', 'bio19', 'tree_cover', 'grass_cover', 'bare_soil'), 
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

