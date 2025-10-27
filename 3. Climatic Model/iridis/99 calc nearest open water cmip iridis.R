#------------------------------------------------------
# Calculate Nearest Open Water for CMIP-Transformed SIC
#------------------------------------------------------

rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/")

library(dplyr)
library(lubridate)
library(stringr)
library(terra)
library(tidyterra)

# 1. Process each SIC file

# list all transformed files
files <- list.files("Satellite_Data_CMIP/siconc/")

# for each file
for(file in files){
  
  # clean up
  rm(list = setdiff(ls(), c("file", "files")))
  
  # print start of loop
  print(file)
  
  # read in SIC raster
  sic <- rast(paste0("Satellite_Data_CMIP/siconc/", file))
  
  # load land mask
  land_mask <- rast("Satellite_Data/static/land_mask.tif")
  
  n <- 1
  
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
  
  # crop to target extent
  now_all <- crop(now_all, ext(-180, 180, -90, -50))
  
  # get GCM name from filename
  gcm <- str_split(file, "_")[[1]][1]
  
  # get scenario from filename
  scenario <- str_split(file, "_")[[1]][2]
  
  # write out
  writeCDF(now_all, 
           filename = paste0("Satellite_Data_CMIP/now/", gcm, "_", scenario, "_gloryres.nc"), 
           varname = "now", 
           longname = "Distance to Nearest Open Water (m)", 
           overwrite = TRUE, 
           zname = "time")
  
}
