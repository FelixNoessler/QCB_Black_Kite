####  - ODMAP protocol - 

## Actual and future predicted occupancy of the Black Kite in Spain

**Authors:** Felix Nößler

**Date:** 2021-09-28



------

## Overview

#### Authorship

Contact : [felix.noessler@uni-potsdam.de](mailto:felix.noessler@uni-potsdam.de)

Study link: https://github.com/FelixNoessler/QCB_Black_Kite

#### Model objective

Model objective: Mapping and interpolation

Target output: occupancy of the Black Kite in Spain in relation to the environmental factors

#### Focal Taxon

Focal Taxon: Black Kite, Milvus migrans (Boddaert, 1783)

#### Location

Location: mainland of Spain

#### Scale of Analysis

Spatial extent: -9.25, 3.29, 36.04, 43.75 (xmin, xmax, ymin, ymax)

Spatial resolution: 4,6

Temporal extent: observational data from 2019

Temporal resolution: 1 year

Boundary: political

#### Biodiversity data

Observation type: citizen science

Response data type: presence/absence

#### Predictors

Predictor types: climatic, habitat

#### Hypotheses

Hypotheses: decrease of occupancy with higher annual mean temperature increase of occupancy with higher annual precipitation decreased occupancy with higher tree cover increased occupancy with higher grass cover decreased occupancy with higher bare soil cover decreased occupancy with higher distance to closest river or lake decreased occupancy with higher distance to closest landfill

#### Assumptions

Model assumptions: The species is at equilibrium with its  environment. Spatial sampling bias is negligible (or accounted for by  data selection).  A site is closed to changes in occupancy over the survey season The probability of occupancy is constant across all sites, or the  variation is modelled by covariates The probability of detection is constant across all sites and surveys,  or the variation is modelled by covariates The detection of a species and its detection histories at each site are  independent

#### Algorithms

Modelling techniques: occupancy

Model complexity: in the final model eight site covariates and two  detection covariates are included. The detection covariates are  important, because citizen-science data is used and with the detection  covariates it is possible to account for imperfect detection. The eight  site covariates are important, because  we want to interpolate the  occupancy of the Black Kite in the complete mainland of Spain.  Furthermore, we want to test our ecological hypothesis.

Model averaging: the full model is compared with an average model.  All the detection covariates were used as fixed terms, whereas the site  covariates may be excluded in the submodels. A list with all submodels  were generated with:

Kamil Bartoń (2020). MuMIn: Multi-Model Inference. R package version 1.43.17. https://CRAN.R-project.org/package=MuMIn

The models were sorted accordingly to the lowest AIC and all models  with a cumulative Akaike weight were selected. These models were  weighted with the AIC.

#### Workflow

Model workflow: 1.  Ecological hypothesis of the relation between the occupancy of the Black Kite and site covariates were established 2. Exploratory analysis of all detection covariates and site covariates  (bioclim variables, land cover fractions, cover of water bodies, cover  of landfills, distance to closest river or lake, distance to closest  landfill) 3. Build a final set of covariates and exclude highly collinear  covariates 4. Build a full model 5.  Build an average model 6.  Analyse the response of the occupancy of the Black kite in relation  to site and detection covariates 7. Predict the occupancy of the Black Kite for the complete mainland of  Spain under actual climate conditions 8. Predict the occupancy of the Black Kite for the mainland of Spain  under future climatic conditions

#### Software

Software: R Core Team (2021). R: A language and environment for  statistical computing. R Foundation for Statistical Computing, Vienna,  Austria. URL https://www.R-project.org/

Matthew Strimas-Mackey, Eliot Miller, and Wesley Hochachka (2018).  auk: eBird Data Extraction and Processing with AWK. R package version  0.3.0. https://cornelllabofornithology.github.io/auk/

Ian Fiske, Richard Chandler (2011). unmarked: An R Package for  Fitting Hierarchical Models of Wildlife Occurrence and Abundance.  Journal of Statistical Software, 43(10), 1-23. URL https://www.jstatsoft.org/v43/i10/.

Robert J. Hijmans (2021). raster: Geographic Data Analysis and Modeling. R package version 3.4-13. https://CRAN.R-project.org/package=raster

H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

Code availability: https://github.com/FelixNoessler/QCB_Black_Kite/tree/master/R/scripts

Data availability: https://github.com/FelixNoessler/QCB_Black_Kite/tree/master/R/data

## Data

#### Biodiversity data

Taxon names: Black Kite, Milvus migrans

Taxonomic reference system: eBird Taxonomy v2021, https://ebird.org/science/use-ebird-data/the-ebird-taxonomy

Ecological level: populations

Data sources: eBird Basic dataset, https://ebird.org/data/download, accessed in February 2021

Sullivan, B. L. et al. (2009): eBird: A citizen-based bird  observation network in the biological sciences. In: Biological  Conservation 142.10, pp. 2282-2292. doi : 10.1016/j.biocon.2009.05.006.

Sampling design: Semistructure data.

Sample size: 1032 repeated surveys, in 503 of these repeated survey was the Black Kite at least one time detected.

in total 5334 single observations, with 1316 detections of the Black Kite

Clipping: mainland of Spain

Scaling: spatial subsampling to the resolution of the grid cells (2.5 minutes, ~ 4.6 km), in the result we have only one repeated survey per  grid cell

Cleaning: filtered for repeated surveys (three to ten times),  standing or travelling surveys with a total distance up to 5 km with one to five observers

Absence data: absence data is present in the data, all surveys were included whether or not the Black Kite has been discovered

<Background data> 

#### Data partitioning

Training data: for explanatory analysis of the covariates, the  dataset was splitted into a train (80 %) and a test dataset (20 %)

Validation data: the final model was compared with an average model to check the stability of the model

#### Predictor variables

Predictor variables: Annual mean temperature (Bioclim) Annual precipitation (Bioclim) Bare soil cover (Copernicus land cover) Distance to closest landfill (CLC) Distance to closest river or lake (CLC) Grass cover (Copernicus land cover) Isothermality (Bioclim) Tree cover (Copernicus land cover)

Data sources: Bioclim: Fick, S. E. and R. J. Hijmans (2017).  WorldClim 2: new 1-km spatial  resolution climate surfaces for global land areas. In: International  Journal of Climatology 37.12, pp. 4302-4315. doi : 10.1002/joc.5086 .

CLC: Copernicus Land Monitoring Service (2018). Corine Land Cover (CLC) 2018 . European Union, European Environment Agency (EEA). url : https://land.copernicus.eu/pan-european/corine-land-cover/clc2018 .

Copernicus land cover: Buchhorn, M., M. Lesiv, et al. (2020). Copernicus Global Land Cover  Layers: Collection 2. In: Remote Sensing 12.6, p. 1044. doi :  10.3390/rs12061044 .

Spatial extent: -9.25, 3.29, 36.04, 43.75 (xmin, xmax, ymin, ymax)

Spatial resolution: Bioclim: download directly in target resolution of 2.5 minutes CLC: downloaded as vector files with minimum mapping unit of 25 hectares Copernicus land cover: 0.06 minutes

Coordinate reference system: Bioclim: +proj=longlat +datum=WGS84 +no_defs  CLC: +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs  Copernicus land cover: +proj=longlat +datum=WGS84 +no_defs

Temporal extent: Bioclim: 1970-2000 CLC: 2017-2018 Copernicus land cover: 2019

Temporal resolution: Bioclim: 30 years CLC: one year Copernicus land cover: one year

Data processing: CLC:  vector shapes were reprojected from ETRS89 to  WGS84, the distance from the middle point of the raster cells to closest water body or landfill was calculated

Dimension reduction: Copernicus land cover: the data was resampled to the target resolution of 2.5 minutes

#### Transfer data

<Data sources> 

<Spatial extent> 

<Spatial resolution> 

<Temporal extent> 

<Models and scenarios> 

<Quantification of Novelty> 

## Model

#### Variable pre-selection

Variable pre-selection: Covariates were selected by a combined  approach of explanatory analysis of preselected set of covariates and  specific ecological hypothesis. The explanatory analysis was done with a Random Forest model. The most important covariates of the Random Forest model were selected.

#### Multicollinearity

Multicollinearity: was checked with the variance inflation factor with a threshold of two, implemented in: Richard M. Heiberger (2020). HH: Statistical Analysis and Data Display: Heiberger and Holland. R package version 3.1-43. URL https://CRAN.R-project.org/package=HH

#### Model settings

occupancy: formula (occu(formula = ~duration_minutes +  number_observers ~ poly(bio1,      2) + poly(bio3, 2) + poly(bio12, 2) + poly(tree_cover, 2) +      poly(grass_cover, 2) + poly(bare_soil, 2) +  poly(distance_to_water,      2) + poly(distance_to_landfill, 2), data =  occ_um)), observationFormula (occu(formula = ~duration_minutes +  effort_distance_km ~ 1, data = occ_um))

<Model settings (extrapolation)> 

#### Model estimates

Coefficients: 

------

```
Detection covariate                            Estimate     SE        z       P(>|z|)  

(Intercept)    -0.9146     0.1043     -8.767   1.832e-18 

duration_minutes   0.00852    0.0008254   10.32    5.561e-25 

number_observers    0.2211     0.0666     3.319    0.0009029  
```

------

```
Site covariate                            Estimate     SE        z       P(>|z|)  

     (Intercept)            -0.0464    0.1049   -0.4424    0.6582   

    poly(bio1, 2)1           -10.27    3.694    -2.781    0.005419  

    poly(bio1, 2)2           -14.38    4.281    -3.359    0.0007834 

    poly(bio3, 2)1           5.023     3.257     1.542      0.123   

    poly(bio3, 2)2           -13.71    3.901    -3.514    0.0004416 

   poly(bio12, 2)1           4.336     4.508    0.9618     0.3361   

   poly(bio12, 2)2           -7.018    3.043    -2.307     0.02108  

 poly(tree_cover, 2)1        -23.27    4.494    -5.178    2.238e-07 

 poly(tree_cover, 2)2        11.24     3.071     3.659    0.0002534 

poly(grass_cover, 2)1        26.67     4.263     6.256    3.941e-10 

poly(grass_cover, 2)2        -10.46    3.603    -2.902    0.003708  

 poly(bare_soil, 2)1         -60.82    12.91    -4.712    2.45e-06  

 poly(bare_soil, 2)2         -38.9     18.17     -2.14     0.03232  

poly(distance_to_water,2)1       -17.11    2.931    -5.839    5.266e-09 

poly(distance_to_water, 2)2       11.51     2.842     4.049    5.14e-05 

poly(distance_to_landfill,2)1     2.809     3.111     0.903     0.3665 

poly(distance_to_landfill, 2)2      -4.279    2.903    -1.474     0.1404
```

------

Parameter uncertainty: the parameter uncertainty was accessed with the standard error and the p-value of the z-test

Variable importance: the importance of covariates can be directly  accessed by the table above,  the higher the absolute value of the  estimate, the higher is also the importance

#### Model selection - model averaging - ensembles

Model selection: information-theoretic approach, selection by the  cumulative AIC greater than 95 % (models were ordered by lowest AIC)

Model averaging: weighted by the AIC

#### Analysis and Correction of non-independence

<Spatial autocorrelation> 

#### Threshold selection

<Threshold selection> 

## Assessment

#### Performance statistics

Performance on training data: AIC, AICc, QAICc, cHat, Goodness of fit test

<Performance on validation data> 

<Performance on test data> 

#### Plausibility check

Response shapes: Occupancy response to all covariates were plotted, all other covariates were set to the mean 

Expert judgement: the output occupancy map was compared to the  observation that had observations of the Black Kite (very basic  plausibility check)

## Prediction

#### Prediction output

Prediction unit: Occupancy probability in grid cells in the mainland  of Spain with a resolution of 2.5 minutes under actual and future  climatic conditions

#### Uncertainty quantification

Scenario uncertainty: the uncertainty of the prediction was analysed with the standard error of the occupancy probability

<Novel environments> 