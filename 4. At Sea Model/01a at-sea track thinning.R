#-----------------------------------------
# Thinning tracking data
#-----------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(GeoThinneR)

# set species
species <- "ADPE"

# set breeding stage
this_stage <- "chick-rearing"

# read in tracks
tracks <- readRDS(paste0("data/tracks/", species, ".RDS"))

# limit tracks to this stage
tracks <- tracks %>%
  filter(stage == this_stage)

# read in subarea info
subareas <- readRDS("data/tracks/tracking_colony_subareas.rds")

# join subarea info to tracks
tracks <- tracks %>%
  left_join(subareas)

# plot tracks
tracks %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

# remove tracks from before 1993 (GLORYS limit)
tracks <- tracks %>%
  filter(date >= as.Date("1993-01-01"))

# summarise number of tracks per subarea
tracks %>%
  group_by(subarea) %>%
  summarise(n_tracks = n_distinct(individual_id),
            n_points = n()) %>%
  arrange(desc(n_tracks))

#----------------------------------------------
# 1. Downsample tracks for each subarea 
#----------------------------------------------

# read in depth raster for thinning
depth <- rast("~/OneDrive - University of Southampton/Documents/Predictor Data/processing/dShelf/depth.nc")

# thin spatially to one point per grid cell
quick_thin <- thin_points(
  data = tracks,
  lon_col = "lon",
  lat_col = "lat",
  method = "grid",
  raster_obj = depth
)

# get thinned data
thinned <- largest(quick_thin)

# limit to one point per individual per day
thinned <- thinned %>%
  group_by(individual_id, device_id, as_date(date)) %>%
  slice_sample(n = 1)

# plot
thinned %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

# summarise number of tracks per subarea
thinned %>%
  group_by(subarea) %>%
  summarise(n_tracks = n_distinct(individual_id),
            n_points = n()) %>%
  arrange(desc(n_tracks))

# define the upper threshold as the smallest grouping remaining
upper <- thinned %>%
  group_by(subarea) %>%
  summarise(n_points = n()) %>%
  summarise(upper = min(n_points)) %>%
  pull(upper)

# randomly subsample to this amount
thinned <- thinned %>%
  group_by(subarea) %>%
  slice_sample(n = upper, replace = FALSE) %>%
  ungroup()

# plot
thinned %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

# export
saveRDS(thinned, paste0("output/at-sea model/thinned_tracks/", species, "_", this_stage, "_thinned.RDS"))
