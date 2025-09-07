#----------------------------------------------------------
# Resample GLO-90 DEMs to 100m resolution to match REMA DEM
#----------------------------------------------------------

rm(list=ls())
setwd("E:/Satellite_Data/static/DEM")

library(terra)
library(tidyverse)
library(tidyterra)

# list all the GLO90 regions
regions <- list.dirs("GLO90_90m_original/")[-1]

# for each region
for(this_region in regions[1:length(regions)]){
  
  # get the region name
  region_name <- basename(this_region)
  
  # create a directory in the resampling directory
  dir.create(paste0("GLO90_90m/", region_name))
  
  # read in elevation to create empty grid at 100m resolution
  elevation <- rast(paste0(this_region, "/output_hh.tif"))
  
  # create grid
  grd <- rast(ext = ext(elevation), crs = crs(elevation), res = 0.001)
  
  # read in, resample, and export DEM files one by one
  # 1. Elevation
  elevation <- resample(elevation, grd, method = "bilinear")
  writeCDF(elevation, paste0("GLO90_90m/", region_name, "/output_hh.nc"))
  
  # 2. Slope
  slope <- rast(paste0(this_region, "/slope.tif"))
  slope <- resample(slope, grd, method = "bilinear")
  plot(slope)
  writeCDF(slope, paste0("GLO90_90m/", region_name, "/slope.nc"))
  
  # 3. Aspect
  aspect <- rast(paste0(this_region, "/aspect.tif"))
  aspect <- resample(aspect, grd, method = "bilinear")
  plot(aspect)
  writeCDF(aspect, paste0("GLO90_90m/", region_name, "/aspect.nc"))
  
  # 4. Dist2Coast
  dist2coast <- rast(paste0(this_region, "/dist_to_coast.tif"))
  dist2coast <- resample(dist2coast, grd, method = "bilinear")
  plot(dist2coast)
  writeCDF(dist2coast, paste0("GLO90_90m/", region_name, "/dist_to_coast.nc"))
  
  # 5. Rugosity
  rugosity <- rast(paste0(this_region, "/rugosity.tif"))
  rugosity <- resample(rugosity, grd, method = "bilinear")
  plot(rugosity)
  writeCDF(rugosity, paste0("GLO90_90m/", region_name, "/rugosity.nc"))
  
  # print completion
  print(paste0("Completed processing for region: ", region_name))
  
}
