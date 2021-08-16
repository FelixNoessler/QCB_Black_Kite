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
