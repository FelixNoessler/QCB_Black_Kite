##########################################################
# Fifth script
#
#   Make all plots:
#       - Maps
#       - response of occupancy due to covariates
#
##########################################################

library(dplyr)
library(ggplot2)

# Load data ---------------------------------------------------------------
load('models.rda')
change <- read.csv('results/change_temp_prec.csv')
variables <- raster::brick("data/environmental_data/variables_spain.grd")


# Prepare data for predictions --------------------------------------------
variables_selection <- c("bio1",
                         "bio3",
                         "bio12",
                         "tree_cover",
                         "grass_cover", 
                         "bare_soil",
                         "distance_to_water",
                         "distance_to_landfill")

variables.sel <- variables[[variables_selection]]

p_variables <- data.frame(raster::rasterToPoints(variables.sel) )
p_variables <- p_variables %>%
  tidyr::drop_na(tree_cover, bio1)

change



change_joined <- p_variables %>% 
  left_join(change, by = c('x', 'y')) %>%
  select(x,y, temperature, precipitation) %>%
  mutate(temperature = temperature * 10,
         precipitation = 1 + precipitation/100)

mean(change_joined$temperature, na.rm=T)/10
mean(change_joined$precipitation, na.rm=T)



p_variables_std <- p_variables %>% 
  mutate_at(variables_selection, ~(scale(.) %>% as.vector))

sd_bio1 <- sd(p_variables$bio1)
mean_bio1 <- mean(p_variables$bio1/ sd(p_variables$bio1))

sd_bio12 <- sd(p_variables$bio12)
mean_bio12 <- mean(p_variables$bio12/ sd(p_variables$bio12))

p_variables_std_future <- p_variables_std
p_variables_std_future$bio1 <- 
  (p_variables$bio1 + change_joined$temperature) / sd_bio1 - mean_bio1
p_variables_std_future$bio12 <- 
  (p_variables$bio12 * change_joined$precipitation) / sd_bio12 - mean_bio12

# Make predictions --------------------------------------------------------
# actual

# occ_avg, best_model
pred_actual <- unmarked::predict(occ_avg,
                                 newdata = select(p_variables_std,
                                                  -x, -y), 
                                 type = "state")

# Predicted, fit
# SE, se.fit
actual_climate <- bind_cols(p_variables_std,  
                            probability = pred_actual$fit, 
                            SE = pred_actual$se.fit) %>%
  select(x, y, probability, SE) %>%
  tidyr::pivot_longer(cols = c(probability, SE)) 


# future

# occ_avg, best_model
pred_future <- unmarked::predict(occ_avg,
                                 newdata = select(p_variables_std_future,
                                                  -x, -y), 
                                 type = "state")

# Predicted, fit
future_climate <- bind_cols(p_variables_std_future, 
                            probability = pred_future$fit, 
                            SE = pred_future$se.fit) %>% 
  select(x, y, probability, SE) %>%
  tidyr::pivot_longer(cols = c(probability, SE)) 

# join the data, preparation for plotting
data <- actual_climate %>%
  inner_join(future_climate, by = c("x", "y", "name")) %>%
  rename(actual = value.x,
         future = value.y,
         type = name)  %>%
  tidyr::pivot_longer(cols=c(actual, future))

# results/predictions_best_model.csv, or results/predictions_avg_model.csv
write.csv(data, 'results/predictions_avg_model.csv', row.names = F) 

#### Plotting the map


# Plot the map ------------------------------------------------------------

# or load data:
# data <- read.csv('results/predictions_best_model.csv')
# data <- read.csv('results/predictions_avg_model.csv')

data %>%
  ggplot(aes(x,y, fill=value))+
  geom_raster()+
  scale_fill_viridis_c(name="value", option="turbo")+
  theme(panel.border=element_rect(color="black",fill="transparent"),
        text = element_text(size=20))+
  labs(x="Longitude", y="Latitude")+
  coord_fixed()+
  facet_grid(~name ~ type)+
  theme(text = element_text(size=30, family = "LM Roman 10"))
# 'results/best_model_map.png' or 'results/avg_model_map.png'
ggsave('results/avg_model_map.png', width = 16, height=10)


# Mean probabilities
data %>%  
  group_by(name) %>% 
  filter(type == 'probability') %>%
  summarise(p = mean(value))

# sum of ells occupied
data %>%  
  group_by(name) %>% 
  filter(type == 'probability') %>%
  mutate(occ = value >= 0.5) %>%
  summarise(s = sum(occ))

data %>%
  tidyr::pivot_wider(names_from=name, values_from = value) %>%
  filter(type == 'probability') %>%
  mutate(lower = actual > future) %>%
  mutate(higher = actual < future) %>%
  summarise(lower = sum(lower), higher = sum(higher))

# total grid cells
nrow(data) / 4

# Prediction of covariates ------------------------------------------------

raw_data <- readr::read_csv('results/milmig_not_std.csv')
model_statistics <- readr::read_csv2('results/best_model.csv')
# model_statistics <- readr::read_csv2('results/best_model.csv')


model_labels <- model_statistics %>%
  rename(p = `P(>|z|)`) %>%
  tidyr::pivot_wider(values_from=p, names_from = coef_no, id_cols=name) %>%
  filter(name != '-') %>%
  rename(first = `1`,
         second = `2`) %>%
  mutate(first = ifelse( round(first, 2) == 0, 
                         '<0.01', 
                         paste('', round(first, 2))),
         second = ifelse(round(second, 2) ==0, 
                         '<0.01',  
                         paste('', round(second, 2))),
         p =  paste('p = ',first, ', ', second, sep='')) %>%
  select(name, p) %>%
  mutate(name = 
           recode(name,
                  bare_soil = "Bare soil cover (%)",
                  tree_cover = "Tree cover (%)",
                  grass_cover = "Grass cover (%)",
                  bio1 = "Annual mean\ntemperature (°C)",
                  bio3 = "Isothermality",
                  bio12 = "Annual precipitation\n(mm)",
                  distance_to_water = "Distance to closest\nriver or lake (km)",
                  distance_to_landfill = "Distance to closest\nlandfill (km)")) %>%
  mutate(x = c(10, 38, 800, 30, 30, 15, 25, 45),
         y = rep(0.1, 8))

variable_names <- c("bio1",
                    "bio3",
                    "bio12",
                    "bare_soil",
                    "tree_cover",
                    "grass_cover",
                    "distance_to_water",
                    "distance_to_landfill")

rm(old_data)
for (i in seq_along(variable_names)) {
  variable_str <- variable_names[i]
  print(variable_str)
  
  
  newdata <- setNames(data.frame(
    matrix(ncol = length(variable_names), 
           nrow = 1000)), 
    variable_names)
  
  newdata[, i] <- seq(min(raw_data[, variable_str]),
                      max(raw_data[, variable_str]),
                      length.out = 100)
  
  newdata[is.na(newdata)] <- 0
  
  sd1 <- sd(newdata[, variable_str])
  mean1<- mean(newdata[, variable_str]/sd(newdata[, variable_str]))
  
  newdata[, variable_str] <- as.numeric(scale(newdata[, variable_str]) )
  
  # best_model, occ_avg
  predict_newdataset <- unmarked::predict(best_model, 
                                          newdata = newdata,
                                          type = "state") 
  # Predicted, fit
  # SE, se.fit
  plotting_data <- bind_cols(newdata, 
                             occ_prob = predict_newdataset$Predicted,
                             occ_se = predict_newdataset$SE) %>% 
    select(matches(variable_str), occ_prob, occ_se) 
  
  plotting_data$x <- (plotting_data[, variable_str] + mean1)* sd1
  
  if (variable_str %in% c('bio1', 'bio2')) {
    plotting_data$x <- plotting_data$x / 10
  } else if (variable_str == 'landfills'){
    plotting_data$x <- plotting_data$x * 100
  } else if (variable_str %in% c('distance_to_water', 
                                 'distance_to_landfill')){
    plotting_data$x <- plotting_data$x /1000
  }
  
  new_data <- plotting_data %>%
    mutate(lower_se = occ_prob - occ_se,
           upper_se = occ_prob + occ_se) %>%
    select(x, occ_prob, lower_se, upper_se) %>%
    mutate(name = variable_str)
  
  if (i == 1){
    old_data <- new_data
  } else {
    old_data <- old_data %>% 
      bind_rows(new_data)
  }
}



old_data %>%
  mutate(name = recode(name, 
                       bare_soil = "Bare soil cover (%)",
                       tree_cover = "Tree cover (%)",
                       grass_cover = "Grass cover (%)",
                       bio1 = "Annual mean\ntemperature (°C)",
                       bio3 = "Isothermality",
                       bio12 = "Annual precipitation\n(mm)",
                       distance_to_water = "Distance to closest\nriver or lake (km)",
                       distance_to_landfill = "Distance to closest\nlandfill (km)")) %>%
  #filter(name != "Distance to closest\nlandfill (km)") #%>%
  ggplot()+
  geom_text(data = model_labels, 
            aes(x = x, y = y, label = p), 
            family="LM Roman 10", 
            size=3.5)+
  geom_ribbon(aes(ymin = lower_se, 
                  ymax = upper_se,
                  x = x),
              fill="gray", alpha=0.7) + 
  geom_line(aes(x=x, y=occ_prob, color=factor(name)),
            size=0.8)+
  scale_color_manual(values = 
                       c("Bare soil cover (%)" = "grey36", 
                         "Annual mean\ntemperature (°C)" = "orangered3",
                         "Distance to closest\nriver or lake (km)" = "royalblue2",
                         "Distance to closest\nlandfill (km)" = "chocolate4", 
                         "Tree cover (%)" = "palegreen3",
                         "Grass cover (%)" = "chartreuse4",
                         "Annual precipitation\n(mm)" = "steelblue4",
                         "Isothermality" = "orange"))+
  labs(x=NULL, 
       y="Occupancy Probability")+
  theme_bw()+
  theme(legend.position = "none",
        panel.border = element_rect(color="black",fill="transparent"),
        text = element_text(size=10, family="LM Roman 10"),
        plot.margin=unit(c(2, 5, -5, 2), "points"),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text =  element_text(size=12, face='plain',
                                   margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title = element_text(size=12, face='plain'))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0,1),expand = expansion(mult = c(0.03, 0.03))) +
  facet_wrap(. ~ name, scales="free_x", strip.position = 'bottom', ncol=4)
# 'results/avg_model_site_covariates.png', 'results/site_covariates.png'
ggsave('results/site_covariates.pdf', device = cairo_pdf,
       width = 7, height = 5)


######### detection covariates

summary(raw_data$duration_minutes)
summary(raw_data$number_observers)

variable_names <- c('number_observers', 'duration_minutes')
newdata = setNames(data.frame(
  matrix(ncol = length(variable_names), 
         nrow = 2000)), 
  variable_names)
newdata[1:1000, variable_names[1]] <- seq(1, 5, length.out=1000)
newdata[1:1000, variable_names[2]] <- colMeans(raw_data[, variable_names[2]], 
                                               na.rm=T)

newdata[1001:2000, variable_names[2]] <- seq(1, 300, length.out=1000)
newdata[1001:2000, variable_names[1]] <- colMeans(raw_data[, variable_names[1]], 
                                                  na.rm=T)

best_model

predict_labels <- data.frame(
  name = c('Number of observers', 'Duration of observation (min)'),
  p = c('p = 0.22', 'p = 0.01'),
  x = c(3, 150),
  y = c(0.1, 0.1))


predict_newdataset <- unmarked::predict(best_model,
                                        newdata = newdata,
                                        type = "det") 

plotting_data <- bind_cols(newdata, 
                           occ_prob = predict_newdataset$Predicted,
                           occ_se = predict_newdataset$SE) %>%
  mutate(lower_se = occ_prob - occ_se,
         upper_se = occ_prob + occ_se,
         name = c(rep('Number of observers', 1000), 
                  rep('Duration of observation (min)', 1000)))

plotting_data$x <- c(plotting_data[1:1000, 'number_observers'], 
                     plotting_data[1001:2000, 'duration_minutes'])

plotting_data %>%
  ggplot()+
  geom_text(data = predict_labels, 
            aes(x = x, y = y, label = p), 
            family="LM Roman 10", size=5)+
  geom_ribbon(aes(ymin = lower_se, 
                  ymax = upper_se,
                  x = x),
              fill="gray", alpha=0.7) + 
  geom_line(aes(x=x, y=occ_prob, color=factor(name)),
            size=0.8)+
  scale_color_manual(values = 
                       c("Number of observers" = "orange", 
                         "Duration of observation (min)" = "orangered3"))+
  labs(x=NULL, 
       y="Occupancy Probability")+
  theme_bw()+
  theme(legend.position = "none",
        panel.border = element_rect(color="black",fill="transparent"),
        text = element_text(size=10, family="LM Roman 10"),
        plot.margin=unit(c(2, 5, -5, 2), "points"),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text =  element_text(size=12, face='plain',
                                   margin = margin(t = 0, r = 0, 
                                                   b = 10, l = 0)),
        axis.title = element_text(size=12, face='plain'))+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0,1), expand = 
                       expansion(mult = c(0.03, 0.03))) +
  facet_wrap(. ~ name, scales="free_x", strip.position = 'bottom', ncol=4)
ggsave('results/det_covariates.png', width = 6, height = 3)