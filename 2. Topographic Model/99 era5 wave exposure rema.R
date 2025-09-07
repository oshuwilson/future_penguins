#------------------------------------------------
# Calculate Wave Exposure for each Species (REMA)
#------------------------------------------------

rm(list=ls())
setwd("E:/Satellite_Data")

library(tidyverse)
library(terra)
library(tidyterra)


# 1. Setup

# read in wave height and angle rasters
hs <- rast("monthly/ERA5/significant_wave_height_monthly.grib")
theta <- rast("monthly/ERA5/wave_direction_monthly.grib")

# define species
species <- "ADPE"

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


# 2. Process Wave Height and Direction

# extract significant wave heights and mean angles for target months
target_hs <- hs[[month(time(hs)) %in% target_months]]
target_theta <- theta[[month(time(theta)) %in% target_months]]

# calculate mean wave height 
mean_hs <- mean(target_hs, na.rm=T)
plot(mean_hs)

# calculate mean theta (accounting for 0-360 circular)
circular_mean <- function(x) {
  radians <- x * pi / 180
  sin_mean <- mean(sin(radians), na.rm = TRUE)
  cos_mean <- mean(cos(radians), na.rm = TRUE)
  mean_angle <- atan2(sin_mean, cos_mean) * 180 / pi
  return(mean_angle)
}
mean_theta <- app(target_theta, circular_mean)

# if mean_theta is under 0, add 360
mean_theta <- app(mean_theta, function(x) ifelse(x < 0, x + 360, x))
plot(mean_theta)


# 3. Calculate Wave Exposure for DEMs

# read in extracted colony data (ADD REMA EXTRACTIONS?)
colonies <- readRDS(paste0("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Chapter 03/output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))

# get max distance to colony
max_dist <- max(colonies$dist2coast)

# read in aspect
aspect <- rast(paste0("static/DEM/REMA_100m_cropped/REMA_100m_aspect_cropped.tif"))

# project hs and theta to Antarctic CRS
hs_proj <- project(mean_hs, crs(aspect))
theta_proj <- project(mean_theta, crs(aspect))

# crop wave height and direction to this DEM
hs_crop <- crop(hs_proj, ext(aspect) + c(1, 1, 1, 1))
theta_crop <- crop(theta_proj, ext(aspect) + c(1, 1, 1, 1))

# plot
plot(hs_crop)
plot(theta_crop)

# interpolate if any missing values in cells
if(any(is.na(values(hs_crop)))) {
  grd <- rast(crs = crs(hs_crop), ext = ext(hs_crop), res = res(hs_crop))
  
  hs_crop <- focal(hs_crop, fun = mean, na.policy = "only", na.rm = T)
  theta_crop <- focal(theta_crop, fun = median, na.policy = "only", na.rm = T)
}

# resample wave height and direction to aspect resolution
hs_crop <- resample(hs_crop, aspect, method = "bilinear")
theta_crop <- resample(theta_crop, aspect, method = "bilinear")

# convert aspect and theta_crop to radians
aspect <- aspect * (pi / 180)
theta_crop <- theta_crop * (pi / 180)

# calculate relative aspect to waves for this DEM
rel_aspect <- cos(theta_crop - aspect) + 1
plot(rel_aspect)

# calculate wave exposure by integrating significant wave height
wave_exposure <- hs_crop * (cos(theta_crop - aspect) + 1) 
plot(wave_exposure)

# create a 0 raster from aspect
zero_matrix <- matrix(c(-Inf, Inf, 0),
                      ncol = 3, byrow = T)
zero_raster <- classify(aspect, zero_matrix)
plot(zero_raster)

# add zero raster to wave exposure
collection <- sds(wave_exposure, zero_raster)
wave_final <- app(collection, fun = "sum", na.rm = T)
plot(wave_final)

# antarctic peninsula
ggplot() +
  geom_spatraster(data = wave_final) +
  xlim(-2.8e+06, -2.2e+06) +
  ylim(1e+06, 1.8e+06)

# export
writeRaster(wave_final, paste0("static/DEM/REMA_100m_cropped/wave_exposure_", species, "_cropped.tif"))
