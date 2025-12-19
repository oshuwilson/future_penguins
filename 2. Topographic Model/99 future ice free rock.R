#-------------------------------------------------------------------------------
# Create Future Ice-Free Rock Rasters for Antarctica
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# read in existing rock raster
rock <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rock_cropped.tif")

# read in future ice-free rock shapefile (ssp585)
#future_rock <- sf::read_sf("data/ice_free_rock/Ice_Free_Rock_RCP85/PS_RCP85_Best_Future_IceFree.shp")
future_rock <- sf::read_sf("data/ice_free_rock/Ice_Free_Rock_RCP85/E_85_best_T50_IF_FINAL.shp")
future_rock <- future_rock %>% vect()

# project to rock raster crs
future_rock <- project(future_rock, crs(rock))

# mask out future rock areas
future_rock_raster <- mask(rock, future_rock)
plot(future_rock_raster)
plot(rock)

# export
writeRaster(future_rock_raster, "data/ice_free_rock/Ice_Free_Rock_RCP85/REMA_100m_future_rock_rcp85.tif", overwrite=T)


# restart for ssp126
rm(list=ls())

# read in existing rock raster
rock <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rock_cropped.tif")

# read in future ice-free rock shapefile (ssp126)
future_rock <- sf::read_sf("data/ice_free_rock/Ice_Free_Rock_RCP26/E_26_best_T50_IF_FINAL.shp")
future_rock <- future_rock %>% vect()

# project to rock raster crs
future_rock <- project(future_rock, crs(rock))

# mask out future rock areas
future_rock_raster <- mask(rock, future_rock)
plot(future_rock_raster)
plot(rock)

# export
writeRaster(future_rock_raster, "data/ice_free_rock/Ice_Free_Rock_RCP26/REMA_100m_future_rock_rcp26.tif", overwrite=T)
