

occ_wide_clean <- read.csv("results/milmig.csv")

occ_um <- unmarked::formatWide(occ_wide_clean, type = "unmarkedFrameOccu")
unmarked::summary(occ_um)


occ_null <- unmarked::occu(~1~1, occ_um)
unmarked::summary(occ_null)


detection_cov_model <- unmarked::occu(~duration_minutes
                                      + effort_distance_km
                                      + number_observers
                                      + protocol_type
                                      + time_observations_started
                                      ~1, data=occ_um)

unmarked::summary(detection_cov_model)

site_cov_model <-unmarked::occu(~1~bio2+bio1+bio12+tree_cover+grass_cover+bare_soil, data = occ_um)
unmarked::summary(site_cov_model)

full_model <- unmarked::occu(~ duration_minutes 
                             + number_observers
                             ~ bio2
                             + bio1
                             + bio12
                             + tree_cover
                             + poly(grass_cover, 2)
                             + bare_soil, data = occ_um)
unmarked::summary(full_model)


models_list <-list(Null = occ_null, 
                   detection = detection_cov_model,
                   site = site_cov_model,
                   fullocc = full_model)

un_models <- unmarked::fitList(fits = models_list)
ModSelect <- unmarked::modSel(un_models, nullmod = "Null")
ModSelect

### AICc
AICcmodavg::aictab(models_list, second.ord = T)

### Goodnis of fit
GOF <-unmarked::parboot(full_model, nsim=10)
GOF

### Another goodnis of fit test
AICcmodavg::mb.gof.test(full_model, nsim=10, plot.hist = F)

### cHat, overdispersion 
cHat <- GOF@t0 / mean(GOF@t.star)
cHat

### QAICc 
### You should set 'c.hat' to 1 if < 1, but values << 1 might also indicate lack of fit
AICcmodavg::aictab(models_list, c.hat = 1)
# --> it is the same as above, because the cHat value is below one

######################### PREDICTION

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


det_terms <- MuMIn::getAllTerms(full_model) %>% 
  purrr::discard(stringr::str_detect, pattern = "psi")

occ_dredge <- MuMIn::dredge(full_model, fixed = det_terms)



occ_dredge_95 <- get.models(occ_dredge, subset = cumsum(weight) <= 0.99)
# average models based on model weights
occ_avg <- model.avg(occ_dredge_95, fit = TRUE, revised.var = TRUE)
# model coefficients
t(occ_avg$coefficients) %>%
  knitr::kable()

variables <- brick(stack("data/environmental_data/variables_spain.grd"))
variables_selection <- c("bio1", "tree_cover", "bio2", "grass_cover", "bio12", "bare_soil")
variables.sel <- variables[[variables_selection]]

p_variables <- data.frame(rasterToPoints(variables.sel) )
p_variables <- p_variables %>%
  drop_na(tree_cover, bio1)

pred_surface_std <- p_variables %>% 
  mutate_at(c("bio1", "tree_cover", "bio2", "grass_cover", "bio12", "bare_soil"), ~(scale(.) %>% as.vector))

occ_pred <- predict(occ_avg,
                                         newdata = as.data.frame(pred_surface_std[,c("bio1",
                                                                                     "tree_cover", "bio2",
                                                                                     "grass_cover", "bio12", "bare_soil")]), type = "state")
pred_occ <- bind_cols(pred_surface_std, occ_prob = occ_pred$fit,occ_se = occ_pred$se.fit) %>%
  dplyr::select(x, y, occ_prob, occ_se)
  
 #save result
write.csv(pred_occ,"results/occ_prediction.csv", row.names = FALSE)

map_proj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

r_pred <- pred_occ %>% 
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
  guides(alpha="none", color="none")
DEOccuMap.facet<-DEOccuMap+facet_grid(~Statistic)
DEOccuMap.facet

ggsave('facet.png', DEOccuMap.facet)


newdata <- data.frame(bio1= seq(0,0,length.out=100),bare_soil = seq(0,0,length.out=100), bio2= seq(0,0,length.out=100), tree_cover= seq(0,0,length.out=100), bio12= seq(0,0,length.out=100),
                      grass_cover= seq(0,60,length.out=100))

sd.grass <- sd(newdata$grass_cover)
mean.grass<- mean(newdata$grass_cover/sd(newdata$grass_cover))



newdata_std <- newdata# %>% mutate_at(c("bare_soil"), ~(scale(.) %>% as.vector))
newdata_std$grass_cover <- as.numeric(scale(newdata$grass_cover) )#newdata$grass_cover / sd.grass - mean.grass

predict_newdataset <- predict(occ_avg, # specify the model
                              newdata = newdata_std, # specify the new data
                              type="state") # ask R to predict occupancy // type="det" for detection 
predict_newdataset
predict_newdataset_df <- bind_cols(newdata_std, occ_prob = predict_newdataset$fit,occ_se = predict_newdataset$se.fit)


## ----echo=T----------------------------------------------------------------------
ggplot(data=predict_newdataset_df)+
  geom_line(aes(x=(grass_cover+mean.grass)*sd.grass, y=occ_prob),color="royalblue3")+
  labs(x="Bare Soil", y="Occupancy Probability")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        panel.background = element_rect(fill="white"))
predPlot
ggsave('predPlot.png', predPlot)
