##########################################################
# Fourth script
#
# Build models with the unmarked package
#
# --> Compare different models with 
#     information criterion
#
# --> evaluate the best model
#
# --> build an average model
#
##########################################################



# Load packages -----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(unmarked)

# Load data ---------------------------------------------------------------
occ_um <- formatWide(read.csv("results/milmig.csv"), type = "unmarkedFrameOccu")
summary(occ_um)


# Build models ------------------------------------------------------------
## Null model
occ_null <- occu(~ 1 ~ 1, occ_um)
summary(occ_null)
backTransform(occ_null, "state")

## Model only with detection covariates
detection_cov_model <- occu(~ duration_minutes
                                      + effort_distance_km
                                      ~ 1, data=occ_um)
summary(detection_cov_model)


## Model only with site covariates
site_cov_model <-occu(~ 1
                                    ~ poly(bio1, 2)
                                    + poly(bio3, 2)
                                    + poly(bio12, 2)
                                    + poly(tree_cover, 2)
                                    + poly(grass_cover, 2)
                                    + poly(bare_soil, 2)
                                    + poly(distance_to_water, 2)
                                    + poly(distance_to_landfill, 2)
                                    , data = occ_um)
summary(site_cov_model)




## Full model with covariates
full_model <- occu(~ duration_minutes 
                                 + number_observers
                                 ~ poly(bio1, 2)
                                 + poly(bio3, 2)
                                 + poly(bio12, 2)
                                 + poly(tree_cover, 2)
                                 + poly(grass_cover, 2)
                                 + poly(bare_soil, 2)
                                 + poly(distance_to_water, 2)
                                 + poly(distance_to_landfill, 2), data = occ_um)
summary(full_model)

re <- ranef(full_model)
sum(bup(re, stat="mode"))
sum(bup(re, stat="mean"))


# Model selection ---------------------------------------------------------
## Model selection with AIC
models_list <-list(Null = occ_null, 
                   detection = detection_cov_model,
                   site = site_cov_model,
                   full_model = full_model)

un_models <- fitList(fits = models_list)
ModSelect <- modSel(un_models, nullmod = "Null")
ModSelect


## Model selection with AICc
AICcmodavg::aictab(models_list, second.ord = T)


best_model <- full_model



if (!file.exists('models.rda')) {

# Goodness of fit test of best model --------------------------------------
GOF <- parboot(best_model, nsim=5, ncores=8, report=T)
GOF

cHat <- GOF@t0 / mean(GOF@t.star)
cHat

### Another goodness of fit test
AICcmodavg::mb.gof.test(best_model,
                        nsim=5,
                        plot.hist = F,
                        parallel=T,
                        ncores=8)



### QAICc
GOF1 <- AICcmodavg::aictab(models_list, c.hat = 1)
# --> it is the same as above, because the cHat value is below one


# Build an average model --------------------------------------------------
  ## Get the names of the detection covariates
  det_terms <- MuMIn::getAllTerms(best_model) %>% 
    purrr::discard(stringr::str_detect, pattern = "psi")
  
  ## Get combination of models, detection covariates are always present
  occ_dredge <- MuMIn::dredge(best_model, fixed = det_terms)
  
  ## Get the best models from the model list
  occ_dredge_95 <- MuMIn::get.models(occ_dredge, 
                                      subset = cumsum(weight) <  0.95)
  
  ## Get the average model based on model weights
  #occ_avg <- MuMIn::model.avg(occ_dredge, fit = TRUE, revised.var = TRUE)
  occ_avg <- MuMIn::model.avg(occ_dredge_95, fit=T)
  
  ## Calculate the AICc for the average model
  sum(occ_avg$msTable$AICc * occ_avg$msTable$weight)
  
  ## Model coefficients of the average model
  t(occ_avg$coefficients)
  
  MuMIn::importance(occ_avg)
  
  save(occ_avg, best_model, GOF, cHat, GOF1, file='models.rda')
}

# Clean up ----------------------------------------------------------------
rm(list = ls()) 


