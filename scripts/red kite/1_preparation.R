rm(list =ls())
library(ggplot2)
library(dplyr)

## Load species data
occ_raw <- readr::read_csv('data/milmil_ebird_raw.csv')
dplyr::n_distinct(occ_raw$site)
summary(occ_raw)


table(occ_raw[, c('species_observed', 'observation_count')])


germany <- rnaturalearth::ne_countries(country = 'germany', scale = "medium", returnclass = "sf")

## Plot occurences
map_1 <- ggplot(germany) +
  geom_sf(fill='white') +
  geom_point(data=occ_raw, aes(x=longitude,
                               y=latitude, color=species_observed),
             alpha=0.6,shape=16,
             size=1) +
  scale_color_manual(values=c("gray","red"), name="Detected")+
  theme(panel.border=element_rect(color="black",fill="transparent"))+
    labs(x="Longitude",y="Latitude", title='Occurences of Red kite (Milvus milvus), May - July 2019')+
    guides(alpha="none", colour = guide_legend(override.aes = list(size=4)))

#get bounding box coords from eBird data
x.min=min(occ_raw$longitude, na.rm=TRUE)-0.4
x.max=max(occ_raw$longitude, na.rm=TRUE)+0.4
y.min=min(occ_raw$latitude, na.rm=TRUE)-0.3
y.max=max(occ_raw$latitude, na.rm=TRUE)+0.3

#svg('results/map.svg')
map_1 + ggsn::scalebar(dist = 40, dist_unit="km", y.min = y.min,
                       y.max = y.max, x.min =x.min, x.max = x.max,
                       model = "WGS84", transform=TRUE, st.size = 2,
                       st.dist=0.03, location = "bottomright",
                       st.bottom = FALSE, border.size=0.3,
                       anchor=c(x=15, y=47.4))
#dev.off()


## Bioclimate data 
clim <- raster::getData("worldclim", var = "bio",
                        res = 2.5, download = F, path = "data/environmental_data")

raster::xres(clim) * 111.19

# crop to the boundaries of germany
variables_selected <- raster::crop(clim, germany)
variables_selected <- raster::mask(variables_selected, germany)

plot(variables_selected)

## Grass and tree cover data
lc_tree <-raster("data/environmental_data/tree_cover.tif")
lc_tree <-crop(lc_tree, germany)
lc_tree <-mask(lc_tree, germany)

lc_grass <-raster("data/environmental_data/grass_cover.tif")
lc_grass <-crop(lc_grass, germany)
lc_grass <-mask(lc_grass, germany)

## Resample both vegetation data to the same resolution of the climate data
## Use a approriate resolution for the task!
lc_grass_rs <-resample(lc_grass, variables_selected,method = "bilinear")
lc_tree_rs <-resample(lc_tree, variables_selected,method = "bilinear")

names(lc_grass_rs) <- "grass_cover"
names(lc_tree_rs) <- "tree_cover"


# Stacking all the variables in the same file
# all variables should have the same resolution!
variables <-stack(variables_selected, lc_grass_rs,lc_tree_rs)

plot(variables)

writeRaster(variables, 
           "data/environmental_data/variables_germany.grd",
           format = "raster", 
           options = "INTERLEAVE=BAND",
           overwrite = TRUE)

# load data:
variables <- stack("data/environmental_data/variables_germany.grd")



## Load species data

occ_raw <-readr::read_csv("data/milmil_ebird_raw.csv")

occ_var <- occ_raw %>%
  cbind(as.data.frame(
    raster::extract(variables,
                    occ_raw[,c("longitude", "latitude")], cellnumbers=T)))

summary(occ_var)
n_distinct(occ_var$site)

## Drop NA for the environmental variables (depends on sample size!)
occ_var <- occ_var%>%
  tidyr::drop_na(bio16,tree_cover)

n_distinct(occ_var$site)

names(occ_var)


### Selection of variables
psych::pairs.panels(select(occ_var,c(24:44)), main = "Red kite 2019")

var_cor <-cor(occ_var[,c(24:44)])

var_dist <-abs(as.dist(var_cor))
var_clust <-hclust(1-var_dist)

x11()
plot(var_clust)

psych::pairs.panels(select(occ_var,
                    c(bio1, tree_cover,bio2, grass_cover, bio12)), 
             main = "Red kite 2019")


## Test for collinearity
testVIF <-select(occ_var,c(bio1, tree_cover,bio2, grass_cover, bio12))
HH::vif(testVIF) 


## Standardize site variables (mean = 0, sd = 1)
occ_var_std <- occ_var%>% 
  dplyr::mutate_at(c("bio1","tree_cover", "bio2", 
              "grass_cover","bio12"),~(scale(.)%>%as.vector))


mean(occ_var_std$tree_cover)
sd(occ_var_std$tree_cover)

# Converting data to unmarked format
occ_wide <-auk::format_unmarked_occu(occ_var_std,site_id = "site", 
                                response = "species_observed",
                                site_covs =c("n_observations", "cells","latitude", "longitude", "bio1","tree_cover", "bio2", "grass_cover","bio12"), 
                                obs_covs =c("time_observations_started","duration_minutes", "effort_distance_km","number_observers", "protocol_type"))

head(occ_wide)


######### Convert the detection histories in 1=# presence/ 0= absence instead of# TRUE/FALSE
cols <-sapply(occ_wide, is.logical)#Select columns that are logical (TRUe/FALSE)
occ_wide[, cols] <-lapply(occ_wide[, cols],as.numeric)# Transform to numeric; 1= TRUE, 0= FALSE.



####### Spatial subsampling
# We can only have one observation per one grid cell! 
# (not to confuse with detectability)
duplicated(occ_wide$cells)
occ_wide_clean <- occ_wide[!duplicated(occ_wide$cells),]

# Part that is removed:
1- nrow(occ_wide_clean)/nrow(occ_wide)

# Save variables
write.csv(occ_wide_clean, "results/milmil_ebird_variables.csv", row.names = FALSE)


####### Unmarked format
# Store it in unmarked format, do all scaling and subsampling before!
occ_um <- unmarked::formatWide(occ_wide_clean, type = "unmarkedFrameOccu")



