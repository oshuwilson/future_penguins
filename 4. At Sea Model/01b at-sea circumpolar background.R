#---------------------------------
# Create Global Background Samples
#---------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
  library(rnaturalearth)
}

# species
species <- "CHPE"

# stage
stage <- "chick-rearing"

# load land file
land <- ne_countries(scale = 10, returnclass = "sv")

# load metadata
meta <- readRDS("data/working_metadata.RDS")

# read in thinned tracking data
tracks <- readRDS(paste0("output/at-sea model/thinned_tracks/", species, "_", stage, "_thinned.RDS"))

# convert to terra
trax <- vect(tracks, geom = c("lon", "lat"), crs = "EPSG:4326")

# create background extent
e <- ext(-180, 180, -80, -40)
e <- as.polygons(e)
crs(e) <- "epsg:4326"

# erase land here???
land <- crop(land, ext(e))
e <- erase(e, land)

# sample background points
n <- 20000
bg <- spatSample(e, n)

# add subarea, individual_id, and date
bg$subarea <- tracks$subarea
bg$individual_id <- tracks$individual_id
bg$date <- tracks$date

# check
plot(e)
plot(bg, add = T, pch = ".")
plot(trax, add = T, pch = ".", col = "red")

# export
saveRDS(bg, paste0("output/at-sea model/background/", species, " ", stage, " background.RDS"))
