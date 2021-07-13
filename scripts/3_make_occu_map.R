
library(ggplot2)
library(dplyr)

### Bird data
milmig <- readr::read_csv('data/milmig_breeding_ES_2019.csv')

milmig <- milmig %>%
  filter(!state_code=='ES-CN')



### Spain
spain <- rnaturalearth::ne_countries(country = 'spain',
                                     scale = 'medium',
                                     returnclass = 'sf')
spain %>%
  ggplot()+
  geom_sf(fill='black')

spain_crop <- sf::st_crop(spain, xmin = -11, ymin = 34, xmax = 8, ymax = 45)

spain_crop %>%
  ggplot()+
  geom_sf(fill='orange')

####
map_1 <- ggplot(spain_crop) +
  geom_sf(fill='white') +
  geom_point(data=milmig, aes(x=longitude,
                               y=latitude, color=species_observed),
             alpha=0.5,shape=16,
             size=1.5) +
  scale_color_manual(values=c("gray","red"), name="Detected")+
  theme(panel.border=element_rect(color="black",fill="transparent"))+
  labs(x="Longitude",
       y="Latitude", 
       title=expression(paste('Occurences of Black kite (', 
                              italic('Milvus migrans'), 
                              '), May - July 2019', sep=''))) +
  guides(alpha="none", colour = guide_legend(override.aes = list(size=4)))

map_1 + ggsn::scalebar(spain_crop, dist = 100, dist_unit="km",
                       transform=T, st.size = 3,
                       st.dist=0.02, st.bottom = F, border.size=0.3)
