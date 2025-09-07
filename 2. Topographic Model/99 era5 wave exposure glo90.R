#-------------------------------------------------
# Calculate Wave Exposure for each Species (GLO90)
#-------------------------------------------------

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

# read in colony locations
colonies <- readxl::read_xlsx(paste0("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Chapter 03/data/colonies/Final Colonies/", species, "_by_colony.xlsx"))

# identify unique regions
regions <- unique(colonies$region)
regions

# define region DEM equivalents
dem_names <- data.frame(
  region = c("South Georgia", "Kerguelen", 
             "Crozet", "Prince Edward", 
             "Macquarie", "Falklands",
             "Heard", "Bouvetoya",
             "South Orkney Islands", "South Sandwich Islands",
             "Chile", "Chile", "Chile"),
  dem = c("south_georgia_glo90", "kerguelen_glo90", 
          "crozet_glo90", "prince_edward_islands_glo90",
          "macquarie_glo90", "falklands_glo90",
          "heard_glo90", "bouvet_glo90",
          "south_orkney_glo90", "south_sandwich_glo90",
          "chile_diego_ramirez_glo90", "chile_ildefonso_glo90",
          "chile_noir_glo90")
)

# get DEM names that correspond to colony regions
dems <- dem_names$dem[dem_names$region %in% regions]

# read in extracted colony data (ADD REMA EXTRACTIONS?)
colonies <- readRDS(paste0("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Chapter 03/output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))
colonies2 <- readRDS(paste0("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Chapter 03/output/topographic model/extractions/", species, "_colonies_extracted_rema.rds"))

# get max distance to colony
max_dist <- max(c(colonies$dist2coast, colonies2$dist2coast))

# for each dem
for(this.dem in dems){
  
  # read in aspect, elevation and distance to coast
  aspect <- rast(paste0("static/DEM/GLO90_100m/", this.dem, "/aspect.nc"))
  dist2coast <- rast(paste0("static/DEM/GLO90_100m/", this.dem, "/dist_to_coast.nc"))
  
  # revalue dist2coast values to within 2x max_dist or beyond
  m1 <- matrix(c(0, max_dist * 2, 1, 
                 max_dist * 2, Inf, 2), 
               ncol = 3, byrow = TRUE)
  m1 <- matrix(c(0, max_dist, 1, 
                 max_dist, Inf, 2), 
               ncol = 3, byrow = TRUE)
  distmask <- classify(dist2coast, m1)
  plot(distmask)
  
  # get a SpatVector of distmask values of 1
  mask <- as.polygons(distmask) %>%
    filter(dist_to_coast == 1)
  plot(mask, col = "red")
  
  # mask out aspect and elevation for this mask
  aspect <- mask(aspect, mask)
  elevation <- mask(elevation, mask)
  
  # crop wave height and direction to this DEM
  hs_crop <- crop(mean_hs, ext(aspect) + c(1, 1, 1, 1))
  theta_crop <- crop(mean_theta, ext(aspect) + c(1, 1, 1, 1))
  
  # reproject 
  hs_crop <- project(hs_crop, crs(aspect))
  theta_crop <- project(theta_crop, crs(aspect))
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
  
  # save wave_exposure
  writeCDF(wave_exposure, paste0("static/DEM/GLO90_100m/", this.dem, "/wave_exposure_", species, ".nc"))
}