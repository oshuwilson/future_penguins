#----------------------------------------------
# Calculate Nearest Open Water for each Species
#----------------------------------------------

rm(list=ls())
setwd("E:/Satellite_Data")

library(tidyverse)
library(terra)
library(tidyterra)

# 1. Process monthly SIC

# read in SIC raster
sic <- rast("monthly/sic/sic.nc")

# assign crs
crs(sic) <- "EPSG:4326"

# get mask for land values
land_mask <- is.na(sic[[8]])
plot(land_mask)

# only keep land mask values south of -60
land_mask <- land_mask & yFromCell(sic, 1:ncell(sic)) < -60

# get rid of NA values between -120 and -70 longitude
land_mask <- land_mask & xFromCell(sic, 1:ncell(sic)) < -65 & 
  xFromCell(sic, 1:ncell(sic)) > -120 & yFromCell(sic, 1:ncell(sic)) < -65 |
  land_mask & xFromCell(sic, 1:ncell(sic)) >= -65 |
  land_mask & xFromCell(sic, 1:ncell(sic)) <= -120
plot(land_mask)

# for each month
for(i in 1:nlyr(sic)){

# get raster
slice <- sic[[i]]

# assign land_mask values as 1
slice[land_mask] <- 1

# make NA values equal 0
slice[is.na(slice)] <- 0

# calculate distance to nearest open water 
now <- gridDist(slice, target = 0)

# join to other nearest open water rasters
if(i == 1){
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

# export netCDF
writeCDF(now_all2, 
          filename = paste0("monthly/now/now.nc"), 
          varname = "now", 
          varunit = "m", 
          longname = "Distance to Nearest Open Water (m)", 
          overwrite = TRUE, 
          zunit = "month", 
          zname = "time")


# 2. Get averages for species

# define species
species <- "KIPE"

# read in netCDF
now <- rast("monthly/now/now.nc")
plot(now[[8]])

# depending on the species, define target months
if(species == "ADPE"){
  target_months <- c(11, 12, 1, 2)
}

if(species == "CHPE"){
  target_months <- c(12, 1, 2, 3, 4)
}

if(species == "GEPE"){ #Gentoos exhibit very different chronologies - Lescroel et al. 2009
  target_months_peninsula <- c(11, 12, 1, 2, 3)
  target_months_falklands <- c(10, 11, 12, 1, 2, 3)
  target_months_crozet <- c(8, 9, 10, 11, 12)
  target_months_marion <- c(6, 7, 8, 9, 10)
  target_months_kerguelen <- c(8, 9, 10, 11, 12, 1)
  target_months_heard <- c(11, 12, 1, 2, 3)
  target_months_macquarie <- c(10, 11, 12, 1, 2)
  target_months_south_georgia <- c(10, 11, 12, 1, 2)
}

if(species == "MAPE"){
  target_months <- c(12, 1, 2)
}

if(species == "EMPE"){
  target_months <- c(5, 6, 7, 8, 9, 10, 11, 12, 1)
}

if(species == "KIPE"){
  target_months <- c(1:12)
}

# limit nearest open water to target months
now <- now[[month(time(now)) %in% target_months]]

# calculate mean open water dist
now_mean <- app(now, mean, na.rm = TRUE)

# calculate min open water dist for each year
years <- unique(year(time(now)))
for(year in years){
  now_year <- now[[year(time(now)) == year]]
  now_min <- app(now_year, min, na.rm = TRUE)
  
  if(year == years[1]){
    now_min_all <- now_min
  } else {
    now_min_all <- c(now_min_all, now_min)
  }
}
now_min_mean <- app(now_min_all, mean, na.rm = TRUE)

# calculate max open water dist for each year
for(year in years){
  now_year <- now[[year(time(now)) == year]]
  now_max <- app(now_year, max, na.rm = TRUE)
  
  if(year == years[1]){
    now_max_all <- now_max
  } else {
    now_max_all <- c(now_max_all, now_max)
  }
}
now_max_mean <- app(now_max_all, mean, na.rm = TRUE)

# plot
plot(now_mean)
plot(now_min_mean)
plot(now_max_mean)

# export netCDFs
writeCDF(now_mean, 
          filename = paste0("monthly/now/now_mean_", species, ".nc"), 
          varname = "now_mean", 
          varunit = "m", 
          longname = paste0("Mean Distance to Nearest Open Water (m) for ", species),
          overwrite = T)
writeCDF(now_min_mean,
         filename = paste0("monthly/now/now_min_mean_", species, ".nc"),
         varname = "now_min_mean",
         varunit = "m",
         overwrite = T,
         longname = paste0("Mean Minimum Distance to Nearest Open Water (m) for ", species))
writeCDF(now_max_mean,
         filename = paste0("monthly/now/now_max_mean_", species, ".nc"),
         varname = "now_max_mean",
         varunit = "m",
         overwrite = T,
         longname = paste0("Mean Maximum Distance to Nearest Open Water (m) for ", species))

