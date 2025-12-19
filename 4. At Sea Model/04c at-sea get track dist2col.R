#------------------------------------------------
# calculate distance to colonies for each species
#------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# define species and stage
species <- "EMPE"
this.stage <- "incubation"

# read in penguin metadata
meta <- readRDS("data/working_metadata.RDS")

# read in track file
tracks <- readRDS(paste0("data/tracks/", species, ".RDS")) %>%
  filter(stage == this.stage)

# list all unique deployment sites
sites <- unique(tracks$deployment_site)

# for each site
for(this_site in sites){
  
  # filter metadata to this deployment site
  colony <- meta %>%
    filter(deployment_site == this_site) %>%
           #&
            # abbreviated_name == species) %>%
    slice(1) %>%
    vect(geom = c("deployment_decimal_longitude", 
                  "deployment_decimal_latitude"),
         crs = "EPSG:4326")
  
  # isolate tracks from this site
  these_tracks <- tracks %>%
    filter(deployment_site == this_site)
  
  # convert tracks to spatvector
  these_tracks <- these_tracks %>%
    vect(geom = c("lon", "lat"),
         crs = "EPSG:4326")
  
  # calculate distance from tracks to colony
  dist <- distance(these_tracks, colony, unit = "km") %>%
    as.data.frame()
  names(dist) <- "distance"
  dist <- dist %>%
    mutate(colony = this_site)
  
  # combine dist to other colonies
  if(this_site == sites[1]){
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
        file = paste0("output/at-sea model/dist2colony/", species, "_", this.stage, "_track_dists.RDS"))
