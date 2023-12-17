data <- read.csv('results/predictions_avg_model.csv')
obs <- read.csv("results/milmig.csv")

data %>%
  filter(type == 'probability') %>%
  mutate(name = recode_factor(name, actual = "occupancy for 2019", 
                              future = "occupancy under expected temperature\n and precipitation change 2080 - 2100")) %>% 
  ggplot(aes(x,y, fill=value))+
  geom_raster()+
  scale_fill_viridis_c(name="probability", option="turbo", limits=c(0,1))+
  labs(x="Longitude", y="Latitude")+
  coord_fixed()+
  facet_grid(~name)+
  theme(text = element_text(size=18))

ggsave('results/simplified_occupancy_model.png', width = 15)
