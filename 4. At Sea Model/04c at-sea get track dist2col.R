#------------------------------------------------
# calculate distance to colonies for each species
#------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# define species and stage
species <- "KIPE"
stage <- "chick-rearing"

# read in penguin metadata
meta <- readRDS("data/metadata.RDS")

# list all track files for the species and stage
files <- list.files(path = paste0("data/tracks/", species), full.names = T)
files <- files[grepl(stage, files)]

# for each file
for(file in files){
  
  # read in tracks
  tracks <- readRDS(file)
  
  # filter metadata to those from this deployment site
  this_site <- unique(tracks$deployment_site)
  colony <- meta %>%
    filter(deployment_site == this_site &
             abbreviated_name == species) %>%
    slice(1) %>%
    vect(geom = c("deployment_decimal_longitude", 
                  "deployment_decimal_latitude"),
         crs = "EPSG:4326")
  
  # convert tracks to spatvector
  tracks <- tracks %>%
    vect(geom = c("lon", "lat"),
         crs = "EPSG:4326")
  
  # calculate distance from tracks to colony
  dist <- distance(tracks, colony, unit = "km") %>%
    as.data.frame()
  names(dist) <- "distance"
  dist <- dist %>%
    mutate(colony = this_site)
  
  # combine dist to other colonies
  if(file == files[1]){
    dist_all <- dist
  } else {
    dist_all <- rbind(dist_all, dist)
  }
}

# plot distance to colony data
ggplot(dist_all, aes(x=distance)) + 
  geom_histogram(aes(fill = colony)) +
  scale_fill_viridis_d()

# export
saveRDS(dist_all, 
        file = paste0("output/dist2colony/", species, "_", stage, "_track_dists.RDS"))
