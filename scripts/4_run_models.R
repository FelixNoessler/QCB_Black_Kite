
library(dplyr)
library(ggplot2)
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
GOF <-unmarked::parboot(full_model, nsim=99, ncores=12, report=T)
GOF

### Another goodnes of fit test
AICcmodavg::mb.gof.test(full_model, 
                        nsim=99, 
                        plot.hist = F, 
                        parallel=T, 
                        ncores=11)

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

MuMIn::importance(occ_avg)

rm(occ_dredge, occ_dredge_999, det_terms)


# Load environmental data for predictions ---------------------------------

variables <- raster::brick("data/environmental_data/variables_spain.grd")
variables_selection <- c("bio1",
                         "bio2",
                         "bio12",
                         "tree_cover",
                         "grass_cover", 
                         "bare_soil",
                         "landfills")

variables.sel <- variables[[variables_selection]]

p_variables <- data.frame(raster::rasterToPoints(variables.sel) )
p_variables <- p_variables %>%
  tidyr::drop_na(tree_cover, bio1)

pred_surface_std <- p_variables %>% 
  mutate_at(c("bio1", "bio2", "bio12",
              "tree_cover", "grass_cover", "bare_soil", 
              "landfills"), ~(scale(.) %>% as.vector))




# Make predictions --------------------------------------------------------

###### Predictions with average model
occ_pred <- unmarked::predict(occ_avg,
                              newdata = as.data.frame(
                                pred_surface_std[,c("bio1",
                                                    "bio2",
                                                    "bio12",
                                                    "tree_cover",
                                                    "grass_cover",
                                                    "bare_soil",
                                                    "landfills")]), 
                    type = "state")


pred_occ <- 


## Make a map
data_avg_pred <- dplyr::bind_cols(pred_surface_std, 
                   probability = occ_pred$fit,
                   SE = occ_pred$se.fit) %>%
  dplyr::select(x, y, probability, SE) %>%
  tidyr::pivot_longer(cols = c(probability, SE)) 

data_avg_pred %>%
  ggplot(aes(x,y, fill=value))+
  geom_raster()+
  scale_fill_viridis_c(name="value")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        text = element_text(size=20))+
  labs(x="Longitude",y="Latitude")+
  coord_fixed()+
  facet_grid(~name)

ggsave('results/average_model.png', width = 16)




###### Prediction with full model
occ_pred_full <- unmarked::predict(full_model,
                                   newdata = as.data.frame(
                                     pred_surface_std[,c("bio1",
                                                         "bio2",
                                                         "bio12",
                                                         "tree_cover",
                                                         "grass_cover",
                                                         "bare_soil",
                                                         "landfills")]), 
                              type = "state")

## Make a map
data_full_pred <- dplyr::bind_cols(pred_surface_std, 
                                  probability = occ_pred_full$Predicted,
                                  SE = occ_pred_full$SE) %>%
  dplyr::select(x, y, probability, SE) %>%
  tidyr::pivot_longer(cols = c(probability, SE)) 

data_full_pred %>%
  ggplot(aes(x,y, fill=value))+
  geom_raster()+
  scale_fill_viridis_c(name="value")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        text = element_text(size=20))+
  labs(x="Longitude",y="Latitude")+
  coord_fixed()+
  facet_grid(~name)

ggsave('results/fullmodel.png', width = 16)






##### Effect of sites covariates
variable_names <- c("bio1",
                    "bio2",
                    "bio12",
                    "bare_soil",
                    "tree_cover",
                    "grass_cover",
                    "landfills")
x_lab_name <- c('Annual mean temperature in °C (bio1)',
                'Mean diurnal range in °C (bio2)',
                'Annual precipitation in mm (bio12)',
                'Bare soil cover %',
                'Tree cover %',
                'Grass cover %',
                'Landfills cover %')

lower_limits <- c(-7, 58, 223, 0,0,0,0)
upper_limits <- c(186, 129, 1539, 60, 82, 71, 0.1)


for (i in seq_along(variable_names)) {
  variable_str <- variable_names[i]
  print(variable_str)
  

  newdata <- setNames(data.frame(matrix(ncol = 7, nrow = 1000)), variable_names)
  
  newdata[, i] <- seq(lower_limits[i],
                      upper_limits[i],
                      length.out = 100)
  
  newdata[is.na(newdata)] <- 0
  
  sd1 <- sd(newdata[, variable_str])
  mean1<- mean(newdata[, variable_str]/sd(newdata[, variable_str]))
  
  newdata[, variable_str] <- as.numeric(scale(newdata[, variable_str]) )
  
  
  predict_newdataset <- unmarked::predict(occ_avg,
                                          newdata = newdata,
                                          type="state") 
  
  plotting_data <- bind_cols(newdata, 
                             occ_prob = predict_newdataset$fit,
                             occ_se = predict_newdataset$se.fit) %>%
    select(matches(variable_str), occ_prob, occ_se) 
  
  plotting_data$x <- (plotting_data[, variable_str] + mean1)* sd1
  
  if (variable_str %in% c('bio1', 'bio2')) {
    plotting_data$x <- plotting_data$x / 10
  } else if (variable_str == 'landfills'){
    plotting_data$x <- plotting_data$x * 100
  }
  
  plotting_data %>%
    mutate(lower_se = occ_prob - occ_se,
           upper_se = occ_prob + occ_se) %>%
    ggplot()+
    geom_ribbon(aes(ymin = lower_se, 
                    ymax = upper_se,
                    x = x),
                fill="gray", alpha=0.5) + 
    geom_line(aes(x=x, y=occ_prob),
              color="royalblue3", size=2)+
    labs(x=x_lab_name[i], 
         y="Occupancy Probability")+
    theme(panel.border=element_rect(color="black",fill="transparent"),
          panel.background = element_rect(fill="white"),
          text = element_text(size=20),
          plot.margin=unit(c(5.5, 15, 5.5, 5.5), "points"))+
    scale_x_continuous(expand = expansion(mult = c(0, 0)))
  
  img_name <- paste('results/', variable_str, '.png', sep='')
  ggsave(img_name, width = 7, height = 5)
  
}


##### Effect of detection covariates


# Number of observers
newdata_detection <- data.frame(number_observers = seq(1,5,length.out = 100), # 1,5
                                duration_minutes = mean(occ_um@obsCovs$duration_minutes, na.rm=T))

predict_newdataset <- unmarked::predict(occ_avg,
                              newdata = newdata_detection,
                              type="det") 

plotting_data <- bind_cols(newdata_detection, 
                                   occ_prob = predict_newdataset$fit,
                                   occ_se = predict_newdataset$se.fit)

plotting_data %>%
  mutate(lower_se = occ_prob - occ_se,
         upper_se = occ_prob + occ_se) %>%
  ggplot()+
  geom_ribbon(aes(ymin = lower_se, 
                  ymax = upper_se,
                  x = number_observers),
              fill="gray", alpha=0.5) + 
  geom_line(aes(x=number_observers, y=occ_prob),
            color="royalblue3", size=2)+
  labs(x="Number of observers", y="Occupancy Probability")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        panel.background = element_rect(fill="white"),
        text = element_text(size=20),
        plot.margin=unit(c(5.5, 15, 5.5, 5.5), "points"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0)))

ggsave('results/observers.png', width = 10, height = 6)


# Duration in minutes
newdata_detection <- data.frame(number_observers = mean(occ_um@obsCovs$number_observers, na.rm=T),
                                duration_minutes = seq(1,300)) # 1, 300

predict_newdataset <- unmarked::predict(occ_avg,
                                        newdata = newdata_detection,
                                        type="det") 

plotting_data <- bind_cols(newdata_detection, 
                           occ_prob = predict_newdataset$fit,
                           occ_se = predict_newdataset$se.fit)

plotting_data %>%
  mutate(lower_se = occ_prob - occ_se,
         upper_se = occ_prob + occ_se) %>%
  ggplot()+
  geom_ribbon(aes(ymin = lower_se, 
                  ymax = upper_se,
                  x = duration_minutes),
              fill="gray", alpha=0.5) + 
  geom_line(aes(x=duration_minutes, y=occ_prob),
            color="royalblue3", size=2)+
  labs(x="Duration in minutes", y="Occupancy Probability")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        panel.background = element_rect(fill="white"),
        text = element_text(size=20),
        plot.margin=unit(c(5.5, 15, 5.5, 5.5), "points"))+
  scale_x_continuous(expand = expansion(mult = c(0, 0)))

ggsave('results/duration.png', width = 10, height = 6)
