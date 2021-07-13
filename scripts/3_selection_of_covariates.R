
occ_wide_clean <- read.csv("results/milmig.csv")


selection <- occ_wide_clean[, c(16:37)]

cor1 <- abs(as.dist(cor(selection)))

clust1 <- hclust(1- cor1)

## Cluster dendrogram
plot(clust1)

## Correlation plots
psych::pairs.panels(selection[, c('bio1', 'bio2', 'bio12', 'tree_cover', 'grass_cover', 'bare_soil')])

## Test for collinearity
HH::vif(selection[, c('bio1', 'bio2', 'bio12', 'tree_cover', 'grass_cover', 'bare_soil')])

