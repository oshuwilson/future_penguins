#------------------------------------------------------
# Calculate Nearest Open Water for CMIP-Transformed SIC
#------------------------------------------------------

# completed 13:16

rm(list=ls())
setwd("E:/cmip6_data")

library(tidyverse)
library(terra)
library(tidyterra)

# 1. Process each SIC file

# list all transformed files
files <- list.files("CMIP6/deltas/siconc/satellite_data/transformed")

# for each file
file <- files[13]

# read in SIC raster
sic <- rast(paste0("CMIP6/deltas/siconc/satellite_data/transformed/", file))

# load land mask
land_mask <- rast("~/OneDrive - University of Southampton/Documents/Chapter 03/output/extra/land_mask.tif")

n <- 159

# for each month
for(i in n:nlyr(sic)){
  
  # get raster
  slice <- sic[[i]]
  
  # assign land_mask values as 1
  slice[land_mask] <- 1
  
  # make NA values equal 0
  slice[is.na(slice)] <- 0
  
  # calculate distance to nearest open water 
  now <- gridDist(slice, target = 0)
  
  # join to other nearest open water rasters
  if(i == n){
    now_all <- now
  } else {
    now_all <- c(now_all, now)
  }
  
  # print completion
  print(paste0(i, "/", nlyr(sic)))
  
}

# bank for std::bad_alloc error
#now_banked <- now_all

# crop to below 50 south and join all together
now_banked <- crop(now_banked, ext(-180, 180, -90, -50))
now_all <- crop(now_all, ext(-180, 180, -90, -50))
now_all2 <- c(now_banked, now_all)
time(now_all2)

# get GCM name from filename
gcm <- str_split(file, "_")[[1]][1]

# get scenario from filename
scenario <- str_split(file, "_")[[1]][2]

# write out
writeCDF(now_all2, 
         filename = paste0("CMIP6/deltas/now/satellite_data/transformed/", gcm, "_", scenario, "_gloryres.nc"), 
         varname = "now", 
         varunit = "m", 
         longname = "Distance to Nearest Open Water (m)", 
         overwrite = TRUE, 
         zunit = "month", 
         zname = "time")
