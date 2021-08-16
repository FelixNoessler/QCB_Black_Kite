##########################################################
# Third script
#
#  Selection of covariates:
#     ... are based on ecological hypotheses and from
#         from the explanatory analysis with a Random
#         Forest model
#
#     --> here some covariates are excluded because of
#         collinerarity
#
##########################################################


# Load data ---------------------------------------------------------------
occ_wide_clean <- read.csv("results/milmig.csv")


# Selected covariates -----------------------------------------------------
random_forest_selection <- c('bio3', 'bio4', 'bio7', 'bio5')

ecological_selection <- c('bio1', 'bio12', 
                          'tree_cover', 'grass_cover', 'bare_soil', 
                          'distance_to_landfill',  'distance_to_water')

selected_covariates <- c(random_forest_selection, ecological_selection)
selection <- occ_wide_clean[, selected_covariates]


# Exclude some covariates -------------------------------------------------
selected_names <- names(selection)[! names(selection) %in% c('bio7', 'bio4', 'bio5')]


# Make a cluster dendrogram -----------------------------------------------
## cluster with all selected covariates
cor1 <- abs(as.dist(cor(selection)))
clust1 <- hclust(1- cor1)
plot(clust1)

## cluster with some covariates removed
cor1 <- abs(as.dist(cor(selection[, selected_names])))
clust1 <- hclust(1- cor1)
plot(clust1)


# Correlation plots -------------------------------------------------------
## Correlation plots with all covariates
psych::pairs.panels(selection)

## Correlation plots with some covariates removed
psych::pairs.panels(selection[, selected_names])


# Test for collinearity ---------------------------------------------------
## Test for collinearity with all covariates
HH::vif(selection)

## Test for collinearity with some covariates removed
HH::vif(selection[, selected_names])


# Make maps of the final selected covariates ------------------------------
site_covariates <- raster::brick('data/environmental_data/variables_spain.grd')
site_covariates <- site_covariates[[selected_names]]

pdf('results/site_covs.pdf')
par(mfrow=c(1,1), 
    oma = c(0, 0, 0, 1) + 0.1,
    mar = c(0, 4, 10, 2) + 0.1)
raster::plot(site_covariates, main=c('Isothermality', 
                                      'Annual mean\ntemperature °C *10',
                                      'Annual mean\nprecipitation mm',
                                      'Tree cover %',
                                      'Grass cover %',
                                      'Bare soil cover %',
                                      'Distance to closest\nlandfill m',
                                      'Distance to closest\nriver or lake m'))
dev.off()



# Clean up ----------------------------------------------------------------
rm(list = ls()) 
dev.off(dev.list()["RStudioGD"])
