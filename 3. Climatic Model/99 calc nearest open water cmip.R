#------------------------------------------------------
# Calculate Nearest Open Water for each Species in CMIP
#------------------------------------------------------

rm(list=ls())
setwd("E:/cmip6_data")

library(tidyverse)
library(terra)
library(tidyterra)

# 1. Process monthly SIC for SSP126

# read in SIC raster
sic <- rast("CMIP6/deltas/siconc/satellite_data/monthly_transformed_ssp126_glorysres.nc")

# get mask for land values
land_mask <- is.na(sic[[178]])
plot(land_mask)

# only keep land mask values south of -60
land_mask <- land_mask & yFromCell(sic, 1:ncell(sic)) < -61

# get rid of NA values between -120 and -70 longitude
land_mask <- land_mask & xFromCell(sic, 1:ncell(sic)) < -65 & 
  xFromCell(sic, 1:ncell(sic)) > -150 & yFromCell(sic, 1:ncell(sic)) < -65 |
  land_mask & xFromCell(sic, 1:ncell(sic)) >= -65 |
  land_mask & xFromCell(sic, 1:ncell(sic)) <= -150
plot(land_mask)

# for each month
for(i in 174:nlyr(sic)){
  
  # get raster
  slice <- sic[[i]]
  
  # assign land_mask values as 1
  slice[land_mask] <- 1
  
  # make NA values equal 0
  slice[is.na(slice)] <- 0
  
  # calculate distance to nearest open water 
  now <- gridDist(slice, target = 0)
  
  # join to other nearest open water rasters
  if(i == 174){
    now_all <- now
  } else {
    now_all <- c(now_all, now)
  }
  
  # print completion
  print(paste0(i, "/", nlyr(sic)))
  
}

# bank for std::bad_alloc error
now_banked <- now_all

# crop to below 50 south and join all together
now_banked <- crop(now_banked, ext(-180, 180, -90, -50))
now_all <- crop(now_all, ext(-180, 180, -90, -50))
now_all2 <- c(now_banked, now_all)
time(now_all2)

# write out
writeCDF(now_all2, 
         filename = paste0("CMIP6/deltas/siconc/satellite_data/now_ssp126.nc"), 
         varname = "now", 
         varunit = "m", 
         longname = "Distance to Nearest Open Water (m)", 
         overwrite = TRUE, 
         zunit = "month", 
         zname = "time")

# 1. Process monthly SIC for SSP126

# cleanup
rm(list=ls())

# read in SIC raster
sic <- rast("CMIP6/deltas/siconc/satellite_data/monthly_transformed_ssp585_glorysres.nc")

# get mask for land values
land_mask <- is.na(sic[[178]])
plot(land_mask)

# only keep land mask values south of -60
land_mask <- land_mask & yFromCell(sic, 1:ncell(sic)) < -61

# get rid of NA values between -120 and -70 longitude
land_mask <- land_mask & xFromCell(sic, 1:ncell(sic)) < -65 & 
  xFromCell(sic, 1:ncell(sic)) > -150 & yFromCell(sic, 1:ncell(sic)) < -65 |
  land_mask & xFromCell(sic, 1:ncell(sic)) >= -65 |
  land_mask & xFromCell(sic, 1:ncell(sic)) <= -150
plot(land_mask)

# crop sic and land_mask to below 50 south
sic <- crop(sic, ext(-180, 180, -90, -50))
land_mask <- crop(land_mask, ext(-180, 180, -90, -50))

# for each month
for(i in 223:nlyr(sic)){
  
  # get raster
  slice <- sic[[i]]
  
  # assign land_mask values as 1
  slice[land_mask] <- 1
  
  # make NA values equal 0
  slice[is.na(slice)] <- 0
  
  # calculate distance to nearest open water 
  now <- gridDist(slice, target = 0)
  
  # join to other nearest open water rasters
  if(i == 223){
    now_all <- now
  } else {
    now_all <- c(now_all, now)
  }
  
  # print completion
  print(paste0(i, "/", nlyr(sic)))
  
}

# bank for std::bad_alloc error
now_banked <- now_all

# join togther
now_all_2 <- c(now_banked, now_all)
time(now_all_2)

# crop further
now_all_2 <- crop(now_all_2, ext(-180, 180, -90, -60))

# write out
writeCDF(now_all_2, 
         filename = paste0("CMIP6/deltas/siconc/satellite_data/now_ssp585.nc"), 
         varname = "now", 
         varunit = "m", 
         longname = "Distance to Nearest Open Water (m)", 
         overwrite = TRUE, 
         zunit = "month", 
         zname = "time")


# 2. Get values for each penguin species