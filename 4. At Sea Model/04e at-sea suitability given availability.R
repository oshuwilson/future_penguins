#---------------------------------------------
# Leverage Model Predictions with Availability
#---------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# define species and stage
species <- "KIPE"
stage <- "chick-rearing"

# read in predicted ensemble suitability
hs <- rast(paste0("output/global predictions/rasters/", species, "_", stage, "_4326_ensemble_mean.nc"))
plot(hs)

# read in availability
av <- rast(paste0("output/availability/", species, "_", stage, "_scam_dist2col.nc"))
plot(av)

# multiply predicted suitability by availability
hs_av <- hs * av
plot(hs_av)
