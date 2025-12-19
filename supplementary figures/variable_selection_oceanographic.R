#-------------------------------------------------------------------------------
# explore dropped variables in oceanographic models
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# define species and stage
species <- "MAPE"
stage <- "pre-moult"

# full candidate list of variables
candidates <- c("depth", "slope", "sst", "sal", 
                "sic", "curr", "mld", "dshelf")

# read in final VIF scores
vif <- read.csv(paste0("output/at-sea model/varselection/", species, "_", stage, "_final_vif.csv"))

# list variables
dropped_vars <- candidates[!candidates %in% vif$covariate]

# read in final variables after contribution check
final_vars <- read.csv(paste0("output/at-sea model/varselection/", species, "_", stage, "_key_vars.csv"))

# identify dropped variables after contribution check
dropped_vars2 <- vif$covariate[!vif$covariate %in% final_vars$key_vars]

# read in covariate contribution scores
varimp <- read.csv(paste0("output/at-sea model/varselection/", species, "_", stage, "_BART_varimp.csv"))

