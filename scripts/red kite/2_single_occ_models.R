## ---- setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning= FALSE, comment = '', fig.width = 15, fig.height = 10)
knitr::opts_knit$set(root.dir = "G:/My Drive/BIOPIC/Teachng_Potsdam/Quantitative_biogeography/Practical_course/QCB_R_project/")
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)


## ----echo=T, results = 'hide', warning=FALSE, message=FALSE-------------------------

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
# resolve namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

#set global options

theme_set(theme_bw())


## ----echo=T-------------------------------------------------------------------------
occ_wide_clean <- readr::read_csv(here("results", "milmil_ebird_variables.csv"))
occ_um <- formatWide(occ_wide_clean, type = "unmarkedFrameOccu")
summary(occ_um)



## ----echo=T-------------------------------------------------------------------------
# Naive occupancy estimate

(occ_naive <- sum(apply(occ_um@y, 1, sum) > 0, na.rm = T) / nrow(occ_um@y))



## ----echo=T-------------------------------------------------------------------------
# In this case, `unmarked` already knows our response so we only have to specify
# the right hand side of the detection and occupancy formulas
# fit null model occu(~detection process ~occupancy process)

occ_null <- unmarked::occu(~1 ~1, occ_um) # Formulas specify detection and occupancy, respectively)
occ_null # show occupancy model output



## ----echo=T-------------------------------------------------------------------------
# In this simple case,we are going to use the backTransform() function to convert our occupancy (or ‘state’) estimate from the logistic scale. Back transformation can be achieved with a built-in function...

p_det <- unmarked::backTransform(occ_null, "det") # Detection probability (observation process)
# Detection probability of ~ 30% per visit given presence
p_det
# Now we can apply the confint() or confidence interval function to that backtransformed value, to generate a 95% confidence interval for detectability of red kites
p_detCI <- unmarked::confint(p_det)
p_detCI
#Detectability of red kite is 30% (± 2.76% SE), with a confidence interval of 25.6-36.4%

#Let’s repeat that process for occupancy

psi <- unmarked::backTransform(occ_null, "state") # Occupancy probability (state process)
# After accounting for detection, ~ 37% of sites estimated to have red kite present
psi
psiCI <- unmarked::confint(psi)
psiCI

