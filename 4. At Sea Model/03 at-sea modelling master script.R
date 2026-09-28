#----------------------------------------------
# Oceanographic Modelling Master Script
#----------------------------------------------

# checklist:
# 1. newest model data

rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/penguins")

library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(tidymodels)
library(themis)
library(tidysdm)
library(bonsai)
library(butcher)
library(bundle)
library(future)
library(DALEXtra)
library(vip)
library(dbarts)

# 1. Configuration 

# number of available cores
cores <- 78

# set seed
set.seed(777)

# create dataframe of species and stage options
meta <- expand.grid(species = c("ADPE", "CHPE", "EMPE", "GEPE", "KIPE", "MAPE"),
                    stage = c("chick-rearing", "incubation")) %>%
  as.data.frame()

# remove EMPE and GEPE incubation (data limitations)
meta <- meta %>%
  filter_out(species %in% c("EMPE", "GEPE") & stage == "incubation")

# add macaroni pre-moult
meta <- meta %>%
  bind_rows(data.frame(species = "MAPE", stage = "pre-moult"))

# loop over each row of meta
for(i in 9:nrow(meta)){
  
  # define species and stage
  species <- meta$species[i]
  stage <- meta$stage[i]
  
  # print initiation
  print(paste0(species, " ", stage, " initiated"))
  
  #---------------------------------------------------
  # 2. Source Modelling Scripts
  #---------------------------------------------------
  
  # random forests
  print("Random Forests")
  source("code/4. At Sea Model/03 at-sea random forests iridis.R")
  
  # boosted regression trees
  print("Boosted Regression Trees")
  source("code/4. At Sea Model/03 at-sea boosted regression trees iridis.R")
  
  # generalised additive models
  print("Generalised Additive Models")
  source("code/4. At Sea Model/03 at-sea generalised additive models iridis.R")
  
  # bayesian additive regression trees
  print("Bayesian Additive Regression Trees")
  source("code/4. At Sea Model/03 at-sea bayesian additive regression trees iridis.R")
  
}