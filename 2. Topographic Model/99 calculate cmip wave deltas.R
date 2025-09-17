#-------------------------------------------------------------------------------
# Calculate deltas for Meucci wave data
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("E:/cmip6_data/CMIP6/waves_meucci/hs")

library(terra)
library(tidyterra)
library(tidyverse)

# read in ERA5 waves
era5 <- rast("E:/Satellite_Data/monthly/ERA5/significant_wave_height_monthly.grib")

# list historical mean files
files <- list.files(path = "historical", pattern = "*.nc", full.names = T)

# for each file
for(file in files){
  
  # load in historical mean
  hist <- rast(file)
  
  # create names of ssp126 and ssp585 files
  ssp126_file <- gsub("historical", "ssp126", file)
  ssp585_file <- gsub("historical", "ssp585", file)
  
  # load in ssp126 and ssp585 mean files
  ssp126 <- rast(ssp126_file)
  ssp585 <- rast(ssp585_file)
  
  # revalue NAs in historical data as 0
  hist[is.na(hist)] <- 0
  
  # calculate deltas
  delta_ssp126 <- ssp126 - hist
  delta_ssp585 <- ssp585 - hist
  
  # create list of dates for each month
  dates <- seq(as.Date("2099-01-15"), as.Date("2099-12-15"), by = "month")
  
  # assign times to delta files
  time(delta_ssp126) <- dates
  time(delta_ssp585) <- dates
  
  # get GCM name from filename
  gcm <- str_split(basename(file), "_")[[1]][1]
  
  # project deltas
  delta_ssp126 <- project(delta_ssp126, era5, method = "near")
  delta_ssp585 <- project(delta_ssp585, era5, method = "near")
  
  # save delta files
  writeCDF(delta_ssp126, paste0("E:/cmip6_data/CMIP6/deltas/hs/ssp126/", gcm, "_ssp126_hs_delta.nc"))
  writeCDF(delta_ssp585, paste0("E:/cmip6_data/CMIP6/deltas/hs/ssp585/", gcm, "_ssp585_hs_delta.nc"))
  
  # print completion
  print(paste0(gcm, " complete"))
  
}


#-------------------------------------------------------------------------------
# Create average deltas for each scenario and add to satellite data
#-------------------------------------------------------------------------------

# clean up
rm(list=ls())
setwd("E:/cmip6_data/CMIP6/deltas/hs")

# read in ERA5 waves
era5 <- rast("E:/Satellite_Data/monthly/ERA5/significant_wave_height_monthly.grib")

# list delta files
d126_files <- list.files(path = "ssp126", pattern = "*.nc", full.names = T)
d585_files <- list.files(path = "ssp585", pattern = "*.nc", full.names = T)

# read in all delta files
d126_list <- lapply(d126_files, rast)
d126_stack <- do.call(c, d126_list)
d585_list <- lapply(d585_files, rast)
d585_stack <- do.call(c, d585_list)

# calculate mean delta for each month across stacks
for(i in 1:12){
  
  # extract month
  month_126 <- d126_stack[[month(time(d126_stack)) == i]]
  month_585 <- d585_stack[[month(time(d585_stack)) == i]]
  
  # calculate mean across all GCMs
  if(i == 1){
    mean_126 <- app(month_126, mean, na.rm = T)
    mean_585 <- app(month_585, mean, na.rm = T)
  } else {
    mean_126 <- c(mean_126, app(month_126, mean, na.rm = T))
    mean_585 <- c(mean_585, app(month_585, mean, na.rm = T))
  }
}

# assign times to mean delta files
dates <- seq(as.Date("2099-01-15"), as.Date("2099-12-15"), by = "month")
time(mean_126) <- dates
time(mean_585) <- dates

# set NAs in era5 to 0
era5[is.na(era5)] <- 0

# add deltas to each month
for(i in 1:12){
  
  # extract month
  era5_month <- era5[[month(time(era5)) == i]]
  
  # limit to 2000-2020
  era5_month <- era5_month[[time(era5_month) >= as.Date("2000-01-01") & time(era5_month) <= as.Date("2020-12-31")]]
  
  # isolate deltas for that month
  delta_126_month <- mean_126[[i]]
  delta_585_month <- mean_585[[i]]
  
  # add deltas to era5
  transformed_126 <- era5_month + delta_126_month
  transformed_585 <- era5_month + delta_585_month
  
  # combine transformed datasets to all months
  if(i == 1){
    all_transformed_126 <- transformed_126
    all_transformed_585 <- transformed_585
  } else {
    all_transformed_126 <- c(all_transformed_126, transformed_126)
    all_transformed_585 <- c(all_transformed_585, transformed_585)
  }
  
  # print completion
  print(i)
}

# export transformed data
writeCDF(all_transformed_126, "satellite_data/transformed/ssp126_era5res.nc")
writeCDF(all_transformed_585, "satellite_data/transformed/ssp585_era5res.nc")
