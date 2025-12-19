rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)
library(rnaturalearth)

# load in a world map
world <- ne_countries(scale = 10, returnclass = "sv")

# crop to below 35 degrees south (to capture all penguin islands)
world <- crop(world, ext(-180, 180, -90, -35))

# isolate antarctica
antarctica <- world %>%
  filter(subregion == "Antarctica")

# isolate subantarctic islands
# South Georgia, South Sandwich, Kerguelen, Crozet, Heard, Amsterdam and St Paul 
subantarctic <- world %>%
  filter(subregion == "Seven seas (open ocean)")

# still missing Bouvet, Macquarie, NZ Subantarctic Islands
# Tristan da Cunha, Gough, Falklands, Marion

# get Falklands
falklands <- world %>% filter(name == "Falkland Is.")

# get Marion
marion <- world %>% filter(name == "South Africa")

# get Bouvet
bouvet <- world %>% filter(name == "Norway")

# get Macquarie
macquarie <- world %>% filter(name == "Australia") %>%
  crop(ext(150, 160, -55, -50))

# get NZ Subantarctic Islands
nz <- world %>% filter(name == "New Zealand") %>%
  crop(ext(160, 180, -55, -47.5))

# get Tristan da Cunha and Gough
tdc <- world %>% filter(name == "Saint Helena")

# get chilean islands
chile <- world %>% filter(name == "Chile") %>%
  crop(ext(-73.2, -72.8, -54.6, -54.4))

# combine
coast <- bind_spat_rows(antarctica, bouvet, falklands, macquarie, chile, 
                        marion, nz, subantarctic, tdc)
coast <- aggregate(coast)
plot(coast)

# read in depth raster for GLORYS resolution and crs
depth <- rast("~/OneDrive - University of Southampton/Documents/Predictor Data/processing/dShelf/depth.nc")
crs(depth) <- "epsg:4326"

# crop depth to below 30 degrees south
depth <- crop(depth, ext(-180, 180, -90, -30))

# project coast to depth raster CRS
coast <- project(coast, "epsg:4326")

# read in land file
land <- ne_countries(scale = 10, returnclass = "sv")

# crop land to below 30 degrees south
land <- crop(land, ext(-180, 180, -90, -30))

# project land to depth raster CRS
land <- project(land, "epsg:4326")

# rasterise coast
col_rast <- rasterize(coast, depth)
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

# export distance raster
writeCDF(dist_rast, 
         paste0("output/at-sea model/dist2coast/dist2coast.nc"),
         overwrite = T)
