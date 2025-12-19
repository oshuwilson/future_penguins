#----------------------------------------------
# Climatic Modelling Master Script
#----------------------------------------------

# checklist:
# 1. newest model env thinned data

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
library(gratia)

# 1. Configuration 

# number of available cores
cores <- 78

# set seed
set.seed(777)

# define species
species <- "EMPE"

#---------------------------------------------------
# 2. Source Modelling Scripts
#---------------------------------------------------

# random forests
print("Random Forests")
source("code/3. Climatic Model/03 climatic random forests iridis.R")

# boosted regression trees
print("Boosted Regression Trees")
source("code/3. Climatic Model/03 climatic boosted regression trees iridis.R")

# maxent
print("MaxEnt")
source("code/3. Climatic Model/03 climatic maxent iridis.R")

# generalised additive models
print("Generalised Additive Models")
source("code/3. Climatic Model/03 climatic generalised additive models iridis.R")

# bayesian additive regression trees
print("Bayesian Additive Regression Trees")
source("code/3. Climatic Model/03 climatic bayesian additive regression trees iridis.R")
