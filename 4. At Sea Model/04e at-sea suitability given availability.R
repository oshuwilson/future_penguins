#---------------------------------------------
# Leverage Model Predictions with Availability
#---------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# define species and stage
species <- "GEPE"
stage <- "chick-rearing"

# read in predicted ensemble suitability
hs <- rast(paste0("output/at-sea model/predictions/", species, "_", stage, "_simple_ensemble.tif"))
plot(hs)

# read in availability
av <- rast(paste0("output/at-sea model/availability/", species, "_", stage, "_scam_dist2coast.nc"))
plot(av)

# crop availability to suitability extent
av <- crop(av, ext(hs))

# multiply predicted suitability by availability
hs_av <- hs * av

# plot results
plot(hs_av %>% crop(ext(35, 40, -50, -40)))
plot(hs_av)
