rm(list=ls())
setwd("E:/Satellite_Data/static/DEM/")

library(tidyverse)
library(terra)
library(sf)
library(tidyterra)

# read in DEM rasters
elevation <- rast("REMA_100m/REMA_100m_dem_geoid.tif")
aspect <- rast("REMA_100m/REMA_100m_aspect.tif")
rugosity <- rast("REMA_100m/REMA_100m_rugosity.tif")
slope <- rast("REMA_100m/REMA_100m_slope.tif")
rock <- rast("REMA_100m/REMA_100m_rock.tif")

# load ADD land polygon
coast <- read_sf("E:/Satellite_Data/static/coast/add_coastline_medium_res_polygon_v7_9.shp")

# only keep land
coast <- coast %>%
  filter(surface == "land")

# aggregate coastline
coast <- coast %>% 
  vect() %>%
  aggregate()

# create band 20km thick
inner <- terra::buffer(coast, -20000)

# get an outer band of the coastline
outer <- erase(coast, inner)
plot(outer)

# mask DEM layers
elevation <- mask(elevation, outer)
aspect <- mask(aspect, outer)
rugosity <- mask(rugosity, outer)
slope <- mask(slope, outer)
rock <- mask(rock, outer)

# export DEMs
writeRaster(elevation, "REMA_100m_cropped/REMA_100m_elevation_cropped.tif")
writeRaster(aspect, "REMA_100m_cropped/REMA_100m_aspect_cropped.tif")
writeRaster(slope, "REMA_100m_cropped/REMA_100m_slope_cropped.tif")
writeRaster(rugosity, "REMA_100m_cropped/REMA_100m_rugosity_cropped.tif")
writeRaster(rock, "REMA_100m_cropped/REMA_100m_rock_cropped.tif")


# 2. REMA Distance to Coast

# clear out
rm(list=ls())

# load ADD land polygon
coast <- read_sf("E:/Satellite_Data/static/coast/add_coastline_medium_res_polygon_v7_9.shp")

# load elevation raster
elevation <- rast("REMA_100m_cropped/REMA_100m_elevation_cropped.tif")

# only keep land
coast <- coast %>%
  filter(surface == "land")

# aggregate coastline
coast <- coast %>% 
  vect() %>%
  aggregate() %>%
  project(crs(elevation))

# convert to lines
# coast2 <- as.lines(coast)

# # convert to sf
# coast_sf <- st_as_sf(coast2)
# 
# # export coastline for trying in python
# sf::write_sf(coast_sf, "REMA_100m_cropped/coastline.shp")

# read in dist2coast qudrants from python script [99 REMA dist2coast.py]
nw <- rast("REMA_100m_cropped/distance_to_coast_NW.tif")
ne <- rast("REMA_100m_cropped/distance_to_coast_NE.tif")
sw <- rast("REMA_100m_cropped/distance_to_coast_SW.tif")
se <- rast("REMA_100m_cropped/distance_to_coast_se.tif")

# combine together
dist2coast <- merge(nw, ne, sw, se)
plot(dist2coast)

# create band 20km thick
inner <- buffer(coast, -20000)

# get an outer band of the coastline
outer <- erase(coast, inner)
plot(outer)

# mask dist2coast
dist2coast2 <- mask(dist2coast, outer)

# divide by 1000
dist2coast2 <- dist2coast2/1000
plot(dist2coast2)

# plot South Shetlands to test
test2 <- crop(rock, ext(-2.7e6, -2.6e6, 1.42e6, 1.68e6))
plot(test2)

# export dist2coast
writeRaster(dist2coast2, "REMA_100m_cropped/REMA_100m_dist2coast_cropped.tif")
