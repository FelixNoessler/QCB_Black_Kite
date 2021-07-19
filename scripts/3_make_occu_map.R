
library(ggplot2)
library(dplyr)

### Bird data
milmig <- readr::read_csv('data/milmig.csv')

milmig <- milmig %>%
  filter(!state_code=='ES-CN')



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

spain_crop %>%
  ggplot()+
  geom_sf(fill='orange')

occ_var <- milmig %>% 
  cbind(as.data.frame(
    raster::extract(variables, milmig[,c('longitude', 'latitude')] ,cellnumbers=T))) %>%
  tidyr::drop_na(bio1, tree_cover)
  




occ_var
####

ggplot()+
  geom_raster(data=occ_var, mapping=aes(x=longitude, y = latitude, fill=landfills))


raster::plot(variables[['landfills']])
points(occ_var$longitude, occ_var$latitude, col=occ_var$species_observed)

ggplot(spain_crop) +
  geom_sf(fill='white') +
  geom_point(data=occ_var, aes(x=longitude,
                               y=latitude, color=species_observed),
             alpha=0.5,shape=16,
             size=1.5) +
  geom_raster(data=occ_var, mapping=aes(x=longitude, y = latitude, fill=landfills))
  scale_color_manual(values=c("gray","red"), name="Detected")+
  theme(panel.border=element_rect(color="black",fill="transparent"))+
  labs(x="Longitude",
       y="Latitude", 
       title=expression(paste('Occurences of Black kite (', 
                              italic('Milvus migrans'), 
                              '), May - July 2019', sep=''))) +
  guides(alpha="none", colour = guide_legend(override.aes = list(size=4)))+
  ggsn::scalebar(spain_crop, dist = 100, dist_unit="km",
                       transform=T, st.size = 3,
                       st.dist=0.02, st.bottom = F, border.size=0.3)
