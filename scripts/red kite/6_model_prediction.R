#clear environment
setwd("/home/felix/Dokumente/studium/Potsdam/Module/Biogeography/QCB_R-session")

rm(list=ls())

library(unmarked)
library(here)
library(auk)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)
library(knitr)
library(raster)
library(tidyr)
library(psych)
library(HH)
library(pander)
library(MuMIn)
library(stringr)
library(AICcmodavg) 
# resolve namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

#set global options

theme_set(theme_bw())


## ----echo=T----------------------------------------------------------------------
occ_wide_clean <- read_csv(here("results", "milmil_ebird_variables.csv"))
occ_um <- formatWide(occ_wide_clean, type = "unmarkedFrameOccu")
# Run full model
Mod.full <- occu(~ time_observations_started +duration_minutes +    effort_distance_km + number_observers + protocol_type 
                  ~  bio2 + bio1 + bio12 + tree_cover + grass_cover, 
                  data = occ_um)
# We want to keep all detection terms and select the occupancy ones.
# get list of all possible terms, then subset to those we want to keep
det_terms <- getAllTerms(Mod.full) %>% 
  # retain the detection submodel covariates
  discard(str_detect, pattern = "psi")
# dredge all possible combinations of the occupancy covariates from the full model
occ_dredge <- dredge(Mod.full, fixed = det_terms)
# select models with the most suport for model averaging
occ_dredge_95 <- get.models(occ_dredge, subset = cumsum(weight) <= 0.95)
# average models based on model weights
occ_avg <- model.avg(occ_dredge_95, fit = TRUE, revised.var = TRUE)
# model coefficients
t(occ_avg$coefficients) %>%
  knitr::kable()

summary(occ_avg)
## ----echo=T----------------------------------------------------------------------
# Load variables for entire Germany
variables <- brick(stack('data/environmental_data/variables_germany.grd'))
# Select the variables that we used to calibrate our occupancy models
variables_selection <- c("bio1", "tree_cover", "bio2", "grass_cover", "bio12")
variables.sel <- variables[[variables_selection]]
# Transform in a dataset that we can use to predict our model
p_variables <- data.frame(rasterToPoints(variables.sel) )
# prediction surface
pred_surface <- p_variables
# Note that should have the same names of the variables
names(pred_surface)[names(pred_surface) == 'x'] <- 'longitude'
names(pred_surface)[names(pred_surface) == 'y'] <- 'latitude'
# Drop NA from the environmental variables. 
pred_surface <- pred_surface %>% 
  drop_na(tree_cover, bio1)



## ----echo=T----------------------------------------------------------------------
pred_surface_std <- pred_surface %>% mutate_at(c("bio1", "tree_cover", "bio2", "grass_cover", "bio12"), ~(scale(.) %>% as.vector))



## ----eval=FALSE------------------------------------------------------------------
## # Prediction
#occ_pred <- predict(occ_avg,
#                     newdata = as.data.frame(pred_surface_std[,c("bio1",
#                                                                 "tree_cover", "bio2",
#                                                                 "grass_cover", "bio12")]), type = "state")
#pred_occ <- bind_cols(pred_surface_std, occ_prob = occ_pred$fit,occ_se = occ_pred$se.fit) %>%
#select(longitude, latitude, occ_prob, occ_se)

## #save result
#write.csv(pred_occ,"results/occ_prediction.csv", row.names = FALSE)


## ----echo=T----------------------------------------------------------------------
pred_occ <- read_csv("results/occ_prediction.csv")
#We'll want to set the map projection so every time we plot data (vector or raster), everything will line up correctly
map_proj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

r_pred <- pred_occ %>% 
  # convert to spatial features
  st_as_sf(coords = c("longitude", "latitude"), crs = map_proj) %>% 
  st_transform(crs = raster::projection(variables.sel[[1]])) %>% 
  # rasterize
  rasterize(variables.sel[[1]])
# Select the prediction with the standard error
r_pred <- r_pred[[c("occ_prob", "occ_se")]]



## ----echo=T----------------------------------------------------------------------
# project predictions
r_pred_proj <- projectRaster(r_pred, crs = map_proj, method = "ngb")
germany <- ne_countries(country = 'germany', scale = "medium", returnclass = "sf")
r_pred_proj_crop<- crop(r_pred_proj, germany)
r_pred_proj_DE <-mask(r_pred_proj_crop,germany)

#First, with occupancy
r_pred_occu_DE <- subset(r_pred_proj_DE, 1, drop=TRUE)
# Convert the landscape data RasterLayer objects to data frames for ggplot
r_pred_occu_DE.df <- as.data.frame(r_pred_occu_DE, xy = TRUE, na.rm = TRUE)

# Now, we can add the Standard error in the predictions
r_pred_occuSE_DE<-subset(r_pred_proj_DE, 2, drop=TRUE)
# Convert the landscape data RasterLayer objects to data frames for ggplot
r_pred_occuSE_DE.df <- as.data.frame(r_pred_occuSE_DE, xy = TRUE, na.rm = TRUE)

#add Statistic Colum and standardize column names
r_pred_occu_DE.df$Statistic<-"Mean p(Occupancy)"
colnames(r_pred_occu_DE.df)[3]<-"Probability"
r_pred_occuSE_DE.df$Statistic<-"SE"
colnames(r_pred_occuSE_DE.df)[3]<-"Probability"

#combine data.frames
r_pred_comb<-rbind(r_pred_occu_DE.df, r_pred_occuSE_DE.df)
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
  guides(alpha="none", color="none")
DEOccuMap.facet<-DEOccuMap+facet_grid(~Statistic)
DEOccuMap.facet

ggsave('facet.png', DEOccuMap.facet)



## ----echo=T----------------------------------------------------------------------
newdata <- data.frame(bio1= seq(min(60 ,na.rm=TRUE), max(100,na.rm=TRUE),length.out=100), tree_cover= seq(min(3 ,na.rm=TRUE), max(68,na.rm=TRUE),length.out=100), bio2= seq(min(61 ,na.rm=TRUE), max(101,na.rm=TRUE),length.out=100), grass_cover= seq(min(4 ,na.rm=TRUE), max(34,na.rm=TRUE),length.out=100), bio12= seq(min(499 ,na.rm=TRUE), max(1309,na.rm=TRUE),length.out=100))
                                                
newdata_std <- newdata %>% mutate_at(c("bio1",
"tree_cover", "bio2", "grass_cover",
"bio12"), ~(scale(.) %>% as.vector))



## ----echo=T----------------------------------------------------------------------
predict_newdataset <- predict(occ_avg, # specify the model
newdata = newdata_std, # specify the new data
type="state") # ask R to predict occupancy // type="det" for detection 
predict_newdataset
predict_newdataset_df <- bind_cols(newdata_std, occ_prob = predict_newdataset$fit,occ_se = predict_newdataset$se.fit)


## ----echo=T----------------------------------------------------------------------
predPlot<-ggplot(data=predict_newdataset_df)+
    geom_line(aes(x=bio1, y=occ_prob),color="royalblue3")+
    labs(x="Bio1", y="Occupancy Probability")+
    theme(panel.border=element_rect(color="black",fill="transparent"),
          panel.background = element_rect(fill="white"))
predPlot
ggsave('predPlot.png', predPlot)
