#-------------------------------------------------------------------------------
# Create distance to coast raster for emperor penguin colonies
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("E:/Satellite_Data")

library(tidyverse)
library(terra)
library(tidyterra)

# load coastline
coast <- sf::read_sf("static/coast/add_coastline_medium_res_polygon_v7_9.shp") %>%
  vect() %>%
  filter(surface == "land") %>%
  as.lines()
plot(coast)

# read in template raster
temp <- rast("monthly/ERA5/air_temp_monthly.grib")

# create raster
r <- rast(extent = ext(coast) + c(1000000, 1000000, 1000000, 1000000),
          resolution = 10000,
          crs = crs(coast))
values(r) <- 2

# rasterise coastline
coast_rast <- rasterize(coast, r, field = 1)

# change NAs to 0
values(coast_rast)[is.na(values(coast_rast))] <- 0

# calculate distance to coast
dist2coast <- gridDist(coast_rast, target = 1)
plot(dist2coast)

# project to temp crs
dist2coast <- project(dist2coast, crs(temp))
plot(dist2coast)
res(dist2coast)

# resample to temp
dist2coast <- resample(dist2coast, temp, method = "bilinear")
res(dist2coast)
plot(dist2coast)

# export
writeRaster(dist2coast, "static/dist2coast_emp.tif", overwrite = T)
