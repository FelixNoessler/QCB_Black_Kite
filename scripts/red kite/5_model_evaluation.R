## ----echo=T, results = 'hide', warning=FALSE, message=FALSE------------------------

#clear environment
rm(list=ls())

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
library(AICcmodavg) #new package this week
# resolve namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

#set global options

theme_set(theme_bw())


## ----echo=T------------------------------------------------------------------------
occ_wide_clean <- read_csv(here("results", "milmil_ebird_variables.csv"))
occ_um <- formatWide(occ_wide_clean, type = "unmarkedFrameOccu")
occ_null <- occu(~1 ~1, occ_um)
Mod.bio1 <- occu(~1 ~bio1, data = occ_um)
Mod.grass <- occu(~1 ~grass_cover, data = occ_um)
Mod.full.occ <- occu(~ 1
                 ~  bio2 + bio1 + bio12 + tree_cover + grass_cover, 
                  data = occ_um)
Mod.full <- occu(~ time_observations_started +duration_minutes +    effort_distance_km + number_observers + protocol_type 
                  ~  bio2 + bio1 + bio12 + tree_cover + grass_cover, 
                  data = occ_um)
# Create list of models to compare
models_list <- list(Null = occ_null,
bio1mod = Mod.bio1,
grassmod = Mod.grass,
fullocc = Mod.full.occ,
fulltotal = Mod.full) 


## ----echo=T------------------------------------------------------------------------
un_models <- fitList(fits = models_list) # Convert list into an unmarkedFitList
ModSelect <- modSel(un_models, nullmod="Null") # Compute dAIC values etc
ModSelect # View the AIC comparison table


## ----echo=T------------------------------------------------------------------------
library(AICcmodavg)
AICresults <- aictab(models_list,
second.ord = TRUE) 
# Ask for AICc (default setting) 
# Always check the defaults!!
AICresults


## ----echo=T------------------------------------------------------------------------
GOF <- parboot(Mod.full, # global (most parameterised) model
nsim = 10) # number of simulations
GOF # view output


## ----echo=T------------------------------------------------------------------------
occ_gof <- mb.gof.test(Mod.full, nsim = 10, plot.hist = FALSE)


## ----echo=T------------------------------------------------------------------------
cHat <- GOF@t0 / mean(GOF@t.star)
cHat


## ----echo=T------------------------------------------------------------------------
QAICtable <- AICcmodavg::aictab(models_list, c.hat = cHat) # apply calculated value of c-hat
QAICtable

