rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)
library(rnaturalearth)

# list species
species <- "MAPE"

# read in colony locations
colonies <- readxl::read_xlsx(paste0("data/colonies/Final Colonies/", species, "_by_colony.xlsx"))

# convert to spatvector
colonies <- colonies %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  vect(geom = c("longitude", "latitude"),
       crs = "EPSG:4326")
plot(colonies)

# read in land file
land <- ne_countries(scale = 10, returnclass = "sv")

# read in depth raster for GLORYS resolution and crs
depth <- rast("~/OneDrive - University of Southampton/Documents/Predictor Data/processing/dShelf/depth.nc")

# crop depth to below 40 degrees south
depth <- crop(depth, ext(-180, 180, -90, -40))

# crop land to below 40 degrees south
land <- crop(land, ext(-180, 180, -90, -40))

# project colonies and land to depth raster CRS
colonies <- project(colonies, crs(depth))
land <- project(land, crs(depth))

# rasterise colonies
col_rast <- rasterize(colonies, depth)
col_rast[is.na(col_rast)] <- 0
col_rast[col_rast > 0] <- 100
plot(col_rast)

# rasterise land
land_rast <- rasterize(land, depth)
land_rast[is.na(land_rast)] <- 0
plot(land_rast)

# add colonies and land rasters
grd <- col_rast + land_rast

# convert land values to NA
grd[grd == 1] <- NA
plot(grd)

# revalue colony locations
grd[grd == 100] <- 101

# calculate distance to colonies
dist_rast <- gridDist(grd, target = 101, scale = 1000)
plot(dist_rast)
plot(colonies, add = T, col = "red")

# export distance raster
writeCDF(dist_rast, 
         paste0("output/distance_to_colony/", species, "_dist_to_colony.nc"),
         overwrite = T)
