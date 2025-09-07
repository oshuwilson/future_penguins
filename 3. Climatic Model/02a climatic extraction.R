#------------------------------------------------------------
# Exclude Nearby Background Samples for Colony Climate Models
#------------------------------------------------------------

# if north of 50 degrees, make nearest open water values 0 for all
# export created rasters for making predictions and adding deltas

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

#-------------------------------------------------------------------------------
# 1. Format background samples and colony data
#-------------------------------------------------------------------------------

# set species
species <- "ADPE"

# read in background samples
bg <- readRDS("output/climatic model/background/background template.rds")

# read in colonies
cols <- readRDS(paste0("data/colonies/subareas/", species, "_colonies_subareas.RDS"))

# convert both to terra
bg <- bg %>%
  vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")
cols <- cols %>%
  vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")

# apply a 100km buffer around the colonies
buff <- buffer(cols, 100000)

# erase background locations within buffer
bg <- erase(bg, buff)

# plot together
ggplot() +
  geom_spatvector(data = bg, aes(col = subarea)) +
  geom_spatvector(data = cols, col = "black")

# join together
cols <- cols %>%
  select(subarea) %>%
  mutate(pa = "presence")
bg <- bg %>%
  mutate(pa = "absence")
pts <- bind_spat_rows(cols, bg)

#-------------------------------------------------------------------------------
# 2. Compute temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# depending on the species, define target months
if(species == "ADPE"){
  target_months <- c(11, 12, 1, 2)
}

if(species == "CHPE"){
  target_months <- c(12, 1, 2, 3, 4)
}

if(species == "GEPE"){ #Gentoo colonies exhibit very different phenologies - Lescroel et al. 2009
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

# read in temperature, precipitation, and nearest open water data
temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
now <- rast("E:/Satellite_Data/monthly/now/now.nc")

# limit to target months
temp <- temp[[month(time(temp)) %in% target_months]]
prec <- prec[[month(time(prec)) %in% target_months]]
now <- now[[month(time(now)) %in% target_months]]

# limit temperature and precipitation to 2000-2020
temp <- temp[[year(time(temp)) %in% 2000:2020]]
prec <- prec[[year(time(prec)) %in% 2000:2020]]

# compute average temperature for these months
avg_temp <- mean(temp, na.rm=T)

# compute average yearly minimum and maximum temperatures for these months
for(this.year in 2000:2020){
  
  # make yearly data
  year_temp <- temp[[year(time(temp)) == this.year]]
  min_year_temp <- min(year_temp, na.rm = T)
  max_year_temp <- max(year_temp, na.rm = T)
  
  # combine together
  if(this.year == 2000){
    min_temps <- min_year_temp
    max_temps <- max_year_temp
  } else {
    min_temps <- c(min_temps, min_year_temp)
    max_temps <- c(max_temps, max_year_temp)
  }
  
}

# average of yearly data
avg_max_temp <- mean(max_temps, na.rm = T)
avg_min_temp <- mean(min_temps, na.rm = T)

# compute average precipitation for these months
avg_prec <- mean(prec, na.rm = T)

# compute average yearly minimum and maximum precipitation for these months
for(this.year in 2000:2020){
  
  # make yearly data
  year_prec <- prec[[year(time(prec)) == this.year]]
  min_year_prec <- min(year_prec, na.rm = T)
  max_year_prec <- max(year_prec, na.rm = T)
  
  # combine together
  if(this.year == 2000){
    min_precips <- min_year_prec
    max_precips <- max_year_prec
  } else {
    min_precips <- c(min_precips, min_year_prec)
    max_precips <- c(max_precips, max_year_prec)
  }
  
}

# average of yearly data
avg_max_prec <- mean(max_precips, na.rm = T)
avg_min_prec <- mean(min_precips, na.rm = T)


# compute average nearest open water
avg_now <- mean(now, na.rm = T)

# compute average yearly minimum and maximum nearest open water
for(this.year in 2000:2020){
  
  # make yearly data
  year_now <- now[[year(time(now)) == this.year]]
  min_year_now <- min(year_now, na.rm = T)
  max_year_now <- max(year_now, na.rm = T)
  
  # combine together
  if(this.year == 2000){
    min_nows <- min_year_now
    max_nows <- max_year_now
  } else {
    min_nows <- c(min_nows, min_year_now)
    max_nows <- c(max_nows, max_year_now)
  }
}

# average of yearly data
avg_max_now <- mean(max_nows, na.rm = T)
avg_min_now <- mean(min_nows, na.rm = T)
plot(avg_min_now)
plot(avg_max_now)


#-------------------------------------------------------------------------------
# 3. Extract temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# convert points to same CRS
pts <- pts %>% project("epsg:4326")

# extract fields
pts$avg_temp <- extract(avg_temp, pts, ID = F)
pts$avg_min_temp <- extract(avg_min_temp, pts, ID = F)
pts$avg_max_temp <- extract(avg_max_temp, pts, ID = F)

pts$avg_prec <- extract(avg_prec, pts, ID = F)
pts$avg_min_prec <- extract(avg_min_prec, pts, ID = F)
pts$avg_max_prec <- extract(avg_max_prec, pts, ID = F)

pts$avg_now <- extract(avg_now, pts, ID = F)
pts$avg_min_now <- extract(avg_min_now, pts, ID = F)
pts$avg_max_now <- extract(avg_max_now, pts, ID = F)

# convert to dataframe
pts <- pts %>%
  as.data.frame(geom = "XY")

# export
saveRDS(pts, paste0("output/climatic model/extraction/", species, " extracted.rds"))


#-------------------------------------------------------------------------------
# 4. Export rasters for making predictions
#-------------------------------------------------------------------------------

# temp
writeRaster(avg_temp, paste0("output/climatic model/rasters/temp/", species, "_avg_temp.tif"))
writeRaster(avg_min_temp, paste0("output/climatic model/rasters/temp/", species, "_avg_min_temp.tif"))
writeRaster(avg_max_temp, paste0("output/climatic model/rasters/temp/", species, "_avg_max_temp.tif"))

# precipitation
writeRaster(avg_prec, paste0("output/climatic model/rasters/prec/", species, "_avg_prec.tif"))
writeRaster(avg_min_prec, paste0("output/climatic model/rasters/prec/", species, "_avg_min_prec.tif"))
writeRaster(avg_max_prec, paste0("output/climatic model/rasters/prec/", species, "_avg_max_prec.tif"))

# open water
writeRaster(avg_now, paste0("output/climatic model/rasters/now/", species, "_avg_now.tif"))
writeRaster(avg_min_now, paste0("output/climatic model/rasters/now/", species, "_avg_min_now.tif"))
writeRaster(avg_max_now, paste0("output/climatic model/rasters/now/", species, "_avg_max_now.tif"))
