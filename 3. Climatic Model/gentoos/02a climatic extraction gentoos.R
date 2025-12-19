#------------------------------------------------------------
# Exclude Nearby Background Samples for Colony Climate Models
#------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
extract <- terra::extract

#-------------------------------------------------------------------------------
# 1. Format background samples and colony data
#-------------------------------------------------------------------------------

# set species
species <- "GEPE"

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


#--------------------------------------------------------------------------------------------------
# 2. Compute temperature, precipitation, and nearest open water data - split by subarea for Gentoos
#--------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# A. Run for southernmost regions first
#-------------------------------------------------------------------------------

# define target months and subareas
target_months <- c(11, 12, 1, 2, 3)
subareas <- c("Subarea 48.1", "Subarea 48.2", "Division 58.5.2", "Subarea 48.4", 
              "Bouvet", "Subarea 48.5", "Subarea 48.6", "Subarea 88.1", "Subarea 88.2", 
              "Subarea 88.3", "Division 58.4.1", "Division 58.4.2")

# # read in temperature, precipitation, and nearest open water data
# temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
# prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
# now <- rast("E:/Satellite_Data/monthly/now/now_resampled.nc")
# 
# # limit to target months
# temp <- temp[[month(time(temp)) %in% target_months]]
# prec <- prec[[month(time(prec)) %in% target_months]]
# now <- now[[month(time(now)) %in% target_months]]
# 
# # limit temperature and precipitation to 2000-2020
# temp <- temp[[year(time(temp)) %in% 2000:2020]]
# prec <- prec[[year(time(prec)) %in% 2000:2020]]
# 
# # compute average temperature for these months
# avg_temp <- mean(temp, na.rm=T)
# 
# # compute average yearly minimum and maximum temperatures for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_temp <- temp[[year(time(temp)) == this.year]]
#   min_year_temp <- min(year_temp, na.rm = T)
#   max_year_temp <- max(year_temp, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_temps <- min_year_temp
#     max_temps <- max_year_temp
#   } else {
#     min_temps <- c(min_temps, min_year_temp)
#     max_temps <- c(max_temps, max_year_temp)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_temp <- mean(max_temps, na.rm = T)
# avg_min_temp <- mean(min_temps, na.rm = T)
# 
# # compute average precipitation for these months
# avg_prec <- mean(prec, na.rm = T)
# 
# # compute average yearly minimum and maximum precipitation for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_prec <- prec[[year(time(prec)) == this.year]]
#   min_year_prec <- min(year_prec, na.rm = T)
#   max_year_prec <- max(year_prec, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_precips <- min_year_prec
#     max_precips <- max_year_prec
#   } else {
#     min_precips <- c(min_precips, min_year_prec)
#     max_precips <- c(max_precips, max_year_prec)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_prec <- mean(max_precips, na.rm = T)
# avg_min_prec <- mean(min_precips, na.rm = T)
# 
# 
# # compute average nearest open water
# avg_now <- mean(now, na.rm = T)
# 
# # compute average yearly minimum and maximum nearest open water
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_now <- now[[year(time(now)) == this.year]]
#   min_year_now <- min(year_now, na.rm = T)
#   max_year_now <- max(year_now, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_nows <- min_year_now
#     max_nows <- max_year_now
#   } else {
#     min_nows <- c(min_nows, min_year_now)
#     max_nows <- c(max_nows, max_year_now)
#   }
# }
# 
# # average of yearly data
# avg_max_now <- mean(max_nows, na.rm = T)
# avg_min_now <- mean(min_nows, na.rm = T)
# plot(avg_min_now)
# plot(avg_max_now)

avg_temp <- rast("output/climatic model/rasters/temp/GEPE_southern_avg_temp.tif")
avg_min_temp <- rast("output/climatic model/rasters/temp/GEPE_southern_avg_min_temp.tif")
avg_max_temp <- rast("output/climatic model/rasters/temp/GEPE_southern_avg_max_temp.tif")

avg_prec <- rast("output/climatic model/rasters/prec/GEPE_southern_avg_prec.tif")
avg_min_prec <- rast("output/climatic model/rasters/prec/GEPE_southern_avg_min_prec.tif")
avg_max_prec <- rast("output/climatic model/rasters/prec/GEPE_southern_avg_max_prec.tif")

avg_now <- rast("output/climatic model/rasters/now/GEPE_southern_avg_now.tif")
avg_min_now <- rast("output/climatic model/rasters/now/GEPE_southern_avg_min_now.tif")
avg_max_now <- rast("output/climatic model/rasters/now/GEPE_southern_avg_max_now.tif")

#-------------------------------------------------------------------------------
# 3. Extract temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# limit points to target subareas
pts <- bind_spat_rows(cols, bg) %>%
  filter(subarea %in% subareas)

# convert points to same CRS
pts <- pts %>% project(crs(avg_temp))

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

# assign to an ocean sector
pts <- pts %>%
  mutate(sector = case_when(
    subarea %in% c("Subarea 48.1", "Subarea 48.2", "Subarea 48.3", "Subarea 48.4",
                   "Subarea 48.5", "Subarea 48.6", "Bouvet", "Falklands", "Tristan da Cunha") ~ 
      "Atlantic",
    subarea %in% c("Subarea 88.1", "Subarea 88.2", "Subarea 88.3", "Chile", 
                   "Macquarie", "NZ Subantarctic") ~
      "Pacific",
    subarea %in% c("Division 58.4.1", "Division 58.4.2", "Division 58.5.1", "Division 58.5.2",
                   "Subarea 58.6", "Subarea 58.7", "Amsterdam and St Paul") ~
      "Indian"
  ))

# if y > -50, relabel nearest open water as 0 (NA otherwise)
pts <- pts %>%
  mutate(avg_now = ifelse(y >= -50, 0, avg_now),
         avg_min_now = ifelse(y >= -50, 0, avg_min_now),
         avg_max_now = ifelse(y >= -50, 0, avg_max_now))

# bank southern points
south_pts <- pts


#-------------------------------------------------------------------------------
# 4. Export rasters for making predictions
#-------------------------------------------------------------------------------

# # temp
# writeRaster(avg_temp, paste0("output/climatic model/rasters/temp/", species, "_southern_avg_temp.tif"))
# writeRaster(avg_min_temp, paste0("output/climatic model/rasters/temp/", species, "_southern_avg_min_temp.tif"))
# writeRaster(avg_max_temp, paste0("output/climatic model/rasters/temp/", species, "_southern_avg_max_temp.tif"))
# 
# # precipitation
# writeRaster(avg_prec, paste0("output/climatic model/rasters/prec/", species, "_southern_avg_prec.tif"))
# writeRaster(avg_min_prec, paste0("output/climatic model/rasters/prec/", species, "_southern_avg_min_prec.tif"))
# writeRaster(avg_max_prec, paste0("output/climatic model/rasters/prec/", species, "_southern_avg_max_prec.tif"))
# 
# # open water
# writeRaster(avg_now, paste0("output/climatic model/rasters/now/", species, "_southern_avg_now.tif"))
# writeRaster(avg_min_now, paste0("output/climatic model/rasters/now/", species, "_southern_avg_min_now.tif"))
# writeRaster(avg_max_now, paste0("output/climatic model/rasters/now/", species, "_southern_avg_max_now.tif"))



#-------------------------------------------------------------------------------
# B. Run for Midrange Subareas
#-------------------------------------------------------------------------------

# define target months and subareas
target_months <- c(10, 11, 12, 1, 2)
subareas <- c("Subarea 48.3", "Macquarie", "Falklands")

# # read in temperature, precipitation, and nearest open water data
# temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
# prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
# now <- rast("E:/Satellite_Data/monthly/now/now_resampled.nc")
# 
# # limit to target months
# temp <- temp[[month(time(temp)) %in% target_months]]
# prec <- prec[[month(time(prec)) %in% target_months]]
# now <- now[[month(time(now)) %in% target_months]]
# 
# # limit temperature and precipitation to 2000-2020
# temp <- temp[[year(time(temp)) %in% 2000:2020]]
# prec <- prec[[year(time(prec)) %in% 2000:2020]]
# 
# # compute average temperature for these months
# avg_temp <- mean(temp, na.rm=T)
# 
# # compute average yearly minimum and maximum temperatures for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_temp <- temp[[year(time(temp)) == this.year]]
#   min_year_temp <- min(year_temp, na.rm = T)
#   max_year_temp <- max(year_temp, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_temps <- min_year_temp
#     max_temps <- max_year_temp
#   } else {
#     min_temps <- c(min_temps, min_year_temp)
#     max_temps <- c(max_temps, max_year_temp)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_temp <- mean(max_temps, na.rm = T)
# avg_min_temp <- mean(min_temps, na.rm = T)
# 
# # compute average precipitation for these months
# avg_prec <- mean(prec, na.rm = T)
# 
# # compute average yearly minimum and maximum precipitation for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_prec <- prec[[year(time(prec)) == this.year]]
#   min_year_prec <- min(year_prec, na.rm = T)
#   max_year_prec <- max(year_prec, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_precips <- min_year_prec
#     max_precips <- max_year_prec
#   } else {
#     min_precips <- c(min_precips, min_year_prec)
#     max_precips <- c(max_precips, max_year_prec)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_prec <- mean(max_precips, na.rm = T)
# avg_min_prec <- mean(min_precips, na.rm = T)
# 
# 
# # compute average nearest open water
# avg_now <- mean(now, na.rm = T)
# 
# # compute average yearly minimum and maximum nearest open water
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_now <- now[[year(time(now)) == this.year]]
#   min_year_now <- min(year_now, na.rm = T)
#   max_year_now <- max(year_now, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_nows <- min_year_now
#     max_nows <- max_year_now
#   } else {
#     min_nows <- c(min_nows, min_year_now)
#     max_nows <- c(max_nows, max_year_now)
#   }
# }
# 
# # average of yearly data
# avg_max_now <- mean(max_nows, na.rm = T)
# avg_min_now <- mean(min_nows, na.rm = T)

avg_temp <- rast("output/climatic model/rasters/temp/GEPE_midrange_avg_temp.tif")
avg_min_temp <- rast("output/climatic model/rasters/temp/GEPE_midrange_avg_min_temp.tif")
avg_max_temp <- rast("output/climatic model/rasters/temp/GEPE_midrange_avg_max_temp.tif")

avg_prec <- rast("output/climatic model/rasters/prec/GEPE_midrange_avg_prec.tif")
avg_min_prec <- rast("output/climatic model/rasters/prec/GEPE_midrange_avg_min_prec.tif")
avg_max_prec <- rast("output/climatic model/rasters/prec/GEPE_midrange_avg_max_prec.tif")

avg_now <- rast("output/climatic model/rasters/now/GEPE_midrange_avg_now.tif")
avg_min_now <- rast("output/climatic model/rasters/now/GEPE_midrange_avg_min_now.tif")
avg_max_now <- rast("output/climatic model/rasters/now/GEPE_midrange_avg_max_now.tif")


#-------------------------------------------------------------------------------
# 3. Extract temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# limit points to target subareas
pts <- bind_spat_rows(cols, bg) %>%
  filter(subarea %in% subareas)

# convert points to same CRS
pts <- pts %>% project(crs(avg_temp))

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

# assign to an ocean sector
pts <- pts %>%
  mutate(sector = case_when(
    subarea %in% c("Subarea 48.1", "Subarea 48.2", "Subarea 48.3", "Subarea 48.4",
                   "Subarea 48.5", "Subarea 48.6", "Bouvet", "Falklands", "Tristan da Cunha") ~ 
      "Atlantic",
    subarea %in% c("Subarea 88.1", "Subarea 88.2", "Subarea 88.3", "Chile", 
                   "Macquarie", "NZ Subantarctic") ~
      "Pacific",
    subarea %in% c("Division 58.4.1", "Division 58.4.2", "Division 58.5.1", "Division 58.5.2",
                   "Subarea 58.6", "Subarea 58.7", "Amsterdam and St Paul") ~
      "Indian"
  ))

# if y > -50, relabel nearest open water as 0 (NA otherwise)
pts <- pts %>%
  mutate(avg_now = ifelse(y >= -50, 0, avg_now),
         avg_min_now = ifelse(y >= -50, 0, avg_min_now),
         avg_max_now = ifelse(y >= -50, 0, avg_max_now))

# bank midrange points
mid_pts <- pts


#-------------------------------------------------------------------------------
# 4. Export rasters for making predictions
#-------------------------------------------------------------------------------
# 
# # temp
# writeRaster(avg_temp, paste0("output/climatic model/rasters/temp/", species, "_midrange_avg_temp.tif"))
# writeRaster(avg_min_temp, paste0("output/climatic model/rasters/temp/", species, "_midrange_avg_min_temp.tif"))
# writeRaster(avg_max_temp, paste0("output/climatic model/rasters/temp/", species, "_midrange_avg_max_temp.tif"))
# 
# # precipitation
# writeRaster(avg_prec, paste0("output/climatic model/rasters/prec/", species, "_midrange_avg_prec.tif"))
# writeRaster(avg_min_prec, paste0("output/climatic model/rasters/prec/", species, "_midrange_avg_min_prec.tif"))
# writeRaster(avg_max_prec, paste0("output/climatic model/rasters/prec/", species, "_midrange_avg_max_prec.tif"))
# 
# # open water
# writeRaster(avg_now, paste0("output/climatic model/rasters/now/", species, "_midrange_avg_now.tif"))
# writeRaster(avg_min_now, paste0("output/climatic model/rasters/now/", species, "_midrange_avg_min_now.tif"))
# writeRaster(avg_max_now, paste0("output/climatic model/rasters/now/", species, "_midrange_avg_max_now.tif"))


#-------------------------------------------------------------------------------
# C. Run for Kerguelen
#-------------------------------------------------------------------------------

# define target months and subareas
target_months <- c(8, 9, 10, 11, 12)
subareas <- c("Division 58.5.1")

# # read in temperature, precipitation, and nearest open water data
# temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
# prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
# now <- rast("E:/Satellite_Data/monthly/now/now_resampled.nc")
# 
# # limit to target months
# temp <- temp[[month(time(temp)) %in% target_months]]
# prec <- prec[[month(time(prec)) %in% target_months]]
# now <- now[[month(time(now)) %in% target_months]]
# 
# # limit temperature and precipitation to 2000-2020
# temp <- temp[[year(time(temp)) %in% 2000:2020]]
# prec <- prec[[year(time(prec)) %in% 2000:2020]]
# 
# # compute average temperature for these months
# avg_temp <- mean(temp, na.rm=T)
# 
# # compute average yearly minimum and maximum temperatures for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_temp <- temp[[year(time(temp)) == this.year]]
#   min_year_temp <- min(year_temp, na.rm = T)
#   max_year_temp <- max(year_temp, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_temps <- min_year_temp
#     max_temps <- max_year_temp
#   } else {
#     min_temps <- c(min_temps, min_year_temp)
#     max_temps <- c(max_temps, max_year_temp)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_temp <- mean(max_temps, na.rm = T)
# avg_min_temp <- mean(min_temps, na.rm = T)
# 
# # compute average precipitation for these months
# avg_prec <- mean(prec, na.rm = T)
# 
# # compute average yearly minimum and maximum precipitation for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_prec <- prec[[year(time(prec)) == this.year]]
#   min_year_prec <- min(year_prec, na.rm = T)
#   max_year_prec <- max(year_prec, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_precips <- min_year_prec
#     max_precips <- max_year_prec
#   } else {
#     min_precips <- c(min_precips, min_year_prec)
#     max_precips <- c(max_precips, max_year_prec)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_prec <- mean(max_precips, na.rm = T)
# avg_min_prec <- mean(min_precips, na.rm = T)
# 
# 
# # compute average nearest open water
# avg_now <- mean(now, na.rm = T)
# 
# # compute average yearly minimum and maximum nearest open water
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_now <- now[[year(time(now)) == this.year]]
#   min_year_now <- min(year_now, na.rm = T)
#   max_year_now <- max(year_now, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_nows <- min_year_now
#     max_nows <- max_year_now
#   } else {
#     min_nows <- c(min_nows, min_year_now)
#     max_nows <- c(max_nows, max_year_now)
#   }
# }
# 
# # average of yearly data
# avg_max_now <- mean(max_nows, na.rm = T)
# avg_min_now <- mean(min_nows, na.rm = T)

avg_temp <- rast("output/climatic model/rasters/temp/GEPE_kerguelen_avg_temp.tif")
avg_min_temp <- rast("output/climatic model/rasters/temp/GEPE_kerguelen_avg_min_temp.tif")
avg_max_temp <- rast("output/climatic model/rasters/temp/GEPE_kerguelen_avg_max_temp.tif")

avg_prec <- rast("output/climatic model/rasters/prec/GEPE_kerguelen_avg_prec.tif")
avg_min_prec <- rast("output/climatic model/rasters/prec/GEPE_kerguelen_avg_min_prec.tif")
avg_max_prec <- rast("output/climatic model/rasters/prec/GEPE_kerguelen_avg_max_prec.tif")

avg_now <- rast("output/climatic model/rasters/now/GEPE_kerguelen_avg_now.tif")
avg_min_now <- rast("output/climatic model/rasters/now/GEPE_kerguelen_avg_min_now.tif")
avg_max_now <- rast("output/climatic model/rasters/now/GEPE_kerguelen_avg_max_now.tif")

#-------------------------------------------------------------------------------
# 3. Extract temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# limit points to target subareas
pts <- bind_spat_rows(cols, bg) %>%
  filter(subarea %in% subareas)

# convert points to same CRS
pts <- pts %>% project(crs(avg_temp))

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

# assign to an ocean sector
pts <- pts %>%
  mutate(sector = case_when(
    subarea %in% c("Subarea 48.1", "Subarea 48.2", "Subarea 48.3", "Subarea 48.4",
                   "Subarea 48.5", "Subarea 48.6", "Bouvet", "Falklands", "Tristan da Cunha") ~ 
      "Atlantic",
    subarea %in% c("Subarea 88.1", "Subarea 88.2", "Subarea 88.3", "Chile", 
                   "Macquarie", "NZ Subantarctic") ~
      "Pacific",
    subarea %in% c("Division 58.4.1", "Division 58.4.2", "Division 58.5.1", "Division 58.5.2",
                   "Subarea 58.6", "Subarea 58.7", "Amsterdam and St Paul") ~
      "Indian"
  ))

# if y > -50, relabel nearest open water as 0 (NA otherwise)
pts <- pts %>%
  mutate(avg_now = ifelse(y >= -50, 0, avg_now),
         avg_min_now = ifelse(y >= -50, 0, avg_min_now),
         avg_max_now = ifelse(y >= -50, 0, avg_max_now))

# bank kerguelen points
kerg_pts <- pts

#-------------------------------------------------------------------------------
# 4. Export rasters for making predictions
#-------------------------------------------------------------------------------

# # temp
# writeRaster(avg_temp, paste0("output/climatic model/rasters/temp/", species, "_kerguelen_avg_temp.tif"))
# writeRaster(avg_min_temp, paste0("output/climatic model/rasters/temp/", species, "_kerguelen_avg_min_temp.tif"))
# writeRaster(avg_max_temp, paste0("output/climatic model/rasters/temp/", species, "_kerguelen_avg_max_temp.tif"))
# 
# # precipitation
# writeRaster(avg_prec, paste0("output/climatic model/rasters/prec/", species, "_kerguelen_avg_prec.tif"))
# writeRaster(avg_min_prec, paste0("output/climatic model/rasters/prec/", species, "_kerguelen_avg_min_prec.tif"))
# writeRaster(avg_max_prec, paste0("output/climatic model/rasters/prec/", species, "_kerguelen_avg_max_prec.tif"))
# 
# # open water
# writeRaster(avg_now, paste0("output/climatic model/rasters/now/", species, "_kerguelen_avg_now.tif"))
# writeRaster(avg_min_now, paste0("output/climatic model/rasters/now/", species, "_kerguelen_avg_min_now.tif"))
# writeRaster(avg_max_now, paste0("output/climatic model/rasters/now/", species, "_kerguelen_avg_max_now.tif"))


#-------------------------------------------------------------------------------
# D. Run for Crozet
#-------------------------------------------------------------------------------

# define target months and subareas
target_months <- c(8, 9, 10, 11)
subareas <- c("Subarea 58.6")

# # read in temperature, precipitation, and nearest open water data
# temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
# prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
# now <- rast("E:/Satellite_Data/monthly/now/now_resampled.nc")
# 
# # limit to target months
# temp <- temp[[month(time(temp)) %in% target_months]]
# prec <- prec[[month(time(prec)) %in% target_months]]
# now <- now[[month(time(now)) %in% target_months]]
# 
# # limit temperature and precipitation to 2000-2020
# temp <- temp[[year(time(temp)) %in% 2000:2020]]
# prec <- prec[[year(time(prec)) %in% 2000:2020]]
# 
# # compute average temperature for these months
# avg_temp <- mean(temp, na.rm=T)
# 
# # compute average yearly minimum and maximum temperatures for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_temp <- temp[[year(time(temp)) == this.year]]
#   min_year_temp <- min(year_temp, na.rm = T)
#   max_year_temp <- max(year_temp, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_temps <- min_year_temp
#     max_temps <- max_year_temp
#   } else {
#     min_temps <- c(min_temps, min_year_temp)
#     max_temps <- c(max_temps, max_year_temp)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_temp <- mean(max_temps, na.rm = T)
# avg_min_temp <- mean(min_temps, na.rm = T)
# 
# # compute average precipitation for these months
# avg_prec <- mean(prec, na.rm = T)
# 
# # compute average yearly minimum and maximum precipitation for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_prec <- prec[[year(time(prec)) == this.year]]
#   min_year_prec <- min(year_prec, na.rm = T)
#   max_year_prec <- max(year_prec, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_precips <- min_year_prec
#     max_precips <- max_year_prec
#   } else {
#     min_precips <- c(min_precips, min_year_prec)
#     max_precips <- c(max_precips, max_year_prec)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_prec <- mean(max_precips, na.rm = T)
# avg_min_prec <- mean(min_precips, na.rm = T)
# 
# 
# # compute average nearest open water
# avg_now <- mean(now, na.rm = T)
# 
# # compute average yearly minimum and maximum nearest open water
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_now <- now[[year(time(now)) == this.year]]
#   min_year_now <- min(year_now, na.rm = T)
#   max_year_now <- max(year_now, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_nows <- min_year_now
#     max_nows <- max_year_now
#   } else {
#     min_nows <- c(min_nows, min_year_now)
#     max_nows <- c(max_nows, max_year_now)
#   }
# }
# 
# # average of yearly data
# avg_max_now <- mean(max_nows, na.rm = T)
# avg_min_now <- mean(min_nows, na.rm = T)

avg_temp <- rast("output/climatic model/rasters/temp/GEPE_crozet_avg_temp.tif")
avg_min_temp <- rast("output/climatic model/rasters/temp/GEPE_crozet_avg_min_temp.tif")
avg_max_temp <- rast("output/climatic model/rasters/temp/GEPE_crozet_avg_max_temp.tif")

avg_prec <- rast("output/climatic model/rasters/prec/GEPE_crozet_avg_prec.tif")
avg_min_prec <- rast("output/climatic model/rasters/prec/GEPE_crozet_avg_min_prec.tif")
avg_max_prec <- rast("output/climatic model/rasters/prec/GEPE_crozet_avg_max_prec.tif")

avg_now <- rast("output/climatic model/rasters/now/GEPE_crozet_avg_now.tif")
avg_min_now <- rast("output/climatic model/rasters/now/GEPE_crozet_avg_min_now.tif")
avg_max_now <- rast("output/climatic model/rasters/now/GEPE_crozet_avg_max_now.tif")


#-------------------------------------------------------------------------------
# 3. Extract temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# limit points to target subareas
pts <- bind_spat_rows(cols, bg) %>%
  filter(subarea %in% subareas)

# convert points to same CRS
pts <- pts %>% project(crs(avg_temp))

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

# assign to an ocean sector
pts <- pts %>%
  mutate(sector = case_when(
    subarea %in% c("Subarea 48.1", "Subarea 48.2", "Subarea 48.3", "Subarea 48.4",
                   "Subarea 48.5", "Subarea 48.6", "Bouvet", "Falklands", "Tristan da Cunha") ~ 
      "Atlantic",
    subarea %in% c("Subarea 88.1", "Subarea 88.2", "Subarea 88.3", "Chile", 
                   "Macquarie", "NZ Subantarctic") ~
      "Pacific",
    subarea %in% c("Division 58.4.1", "Division 58.4.2", "Division 58.5.1", "Division 58.5.2",
                   "Subarea 58.6", "Subarea 58.7", "Amsterdam and St Paul") ~
      "Indian"
  ))

# if y > -50, relabel nearest open water as 0 (NA otherwise)
pts <- pts %>%
  mutate(avg_now = ifelse(y >= -50, 0, avg_now),
         avg_min_now = ifelse(y >= -50, 0, avg_min_now),
         avg_max_now = ifelse(y >= -50, 0, avg_max_now))

# bank crozet points
crozet_pts <- pts

#-------------------------------------------------------------------------------
# 4. Export rasters for making predictions
#-------------------------------------------------------------------------------

# # temp
# writeRaster(avg_temp, paste0("output/climatic model/rasters/temp/", species, "_crozet_avg_temp.tif"))
# writeRaster(avg_min_temp, paste0("output/climatic model/rasters/temp/", species, "_crozet_avg_min_temp.tif"))
# writeRaster(avg_max_temp, paste0("output/climatic model/rasters/temp/", species, "_crozet_avg_max_temp.tif"))
# 
# # precipitation
# writeRaster(avg_prec, paste0("output/climatic model/rasters/prec/", species, "_crozet_avg_prec.tif"))
# writeRaster(avg_min_prec, paste0("output/climatic model/rasters/prec/", species, "_crozet_avg_min_prec.tif"))
# writeRaster(avg_max_prec, paste0("output/climatic model/rasters/prec/", species, "_crozet_avg_max_prec.tif"))
# 
# # open water
# writeRaster(avg_now, paste0("output/climatic model/rasters/now/", species, "_crozet_avg_now.tif"))
# writeRaster(avg_min_now, paste0("output/climatic model/rasters/now/", species, "_crozet_avg_min_now.tif"))
# writeRaster(avg_max_now, paste0("output/climatic model/rasters/now/", species, "_crozet_avg_max_now.tif"))


#-------------------------------------------------------------------------------
# E. Run for northernmost regions
#-------------------------------------------------------------------------------

# define target months and subareas
target_months <- c(6, 7, 8, 9, 10)
subareas <- c("Subarea 58.7", "Amsterdam and St Paul", "Tristan da Cunha", "NZ Subantarctic")

# # read in temperature, precipitation, and nearest open water data
# temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
# prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
# now <- rast("E:/Satellite_Data/monthly/now/now_resampled.nc")
# 
# # limit to target months
# temp <- temp[[month(time(temp)) %in% target_months]]
# prec <- prec[[month(time(prec)) %in% target_months]]
# now <- now[[month(time(now)) %in% target_months]]
# 
# # limit temperature and precipitation to 2000-2020
# temp <- temp[[year(time(temp)) %in% 2000:2020]]
# prec <- prec[[year(time(prec)) %in% 2000:2020]]
# 
# # compute average temperature for these months
# avg_temp <- mean(temp, na.rm=T)
# 
# # compute average yearly minimum and maximum temperatures for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_temp <- temp[[year(time(temp)) == this.year]]
#   min_year_temp <- min(year_temp, na.rm = T)
#   max_year_temp <- max(year_temp, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_temps <- min_year_temp
#     max_temps <- max_year_temp
#   } else {
#     min_temps <- c(min_temps, min_year_temp)
#     max_temps <- c(max_temps, max_year_temp)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_temp <- mean(max_temps, na.rm = T)
# avg_min_temp <- mean(min_temps, na.rm = T)
# 
# # compute average precipitation for these months
# avg_prec <- mean(prec, na.rm = T)
# 
# # compute average yearly minimum and maximum precipitation for these months
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_prec <- prec[[year(time(prec)) == this.year]]
#   min_year_prec <- min(year_prec, na.rm = T)
#   max_year_prec <- max(year_prec, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_precips <- min_year_prec
#     max_precips <- max_year_prec
#   } else {
#     min_precips <- c(min_precips, min_year_prec)
#     max_precips <- c(max_precips, max_year_prec)
#   }
#   
# }
# 
# # average of yearly data
# avg_max_prec <- mean(max_precips, na.rm = T)
# avg_min_prec <- mean(min_precips, na.rm = T)
# 
# 
# # compute average nearest open water
# avg_now <- mean(now, na.rm = T)
# 
# # compute average yearly minimum and maximum nearest open water
# for(this.year in 2000:2020){
#   
#   # make yearly data
#   year_now <- now[[year(time(now)) == this.year]]
#   min_year_now <- min(year_now, na.rm = T)
#   max_year_now <- max(year_now, na.rm = T)
#   
#   # combine together
#   if(this.year == 2000){
#     min_nows <- min_year_now
#     max_nows <- max_year_now
#   } else {
#     min_nows <- c(min_nows, min_year_now)
#     max_nows <- c(max_nows, max_year_now)
#   }
# }
# 
# # average of yearly data
# avg_max_now <- mean(max_nows, na.rm = T)
# avg_min_now <- mean(min_nows, na.rm = T)

avg_temp <- rast("output/climatic model/rasters/temp/GEPE_northernmost_avg_temp.tif")
avg_min_temp <- rast("output/climatic model/rasters/temp/GEPE_northernmost_avg_min_temp.tif")
avg_max_temp <- rast("output/climatic model/rasters/temp/GEPE_northernmost_avg_max_temp.tif")

avg_prec <- rast("output/climatic model/rasters/prec/GEPE_northernmost_avg_prec.tif")
avg_min_prec <- rast("output/climatic model/rasters/prec/GEPE_northernmost_avg_min_prec.tif")
avg_max_prec <- rast("output/climatic model/rasters/prec/GEPE_northernmost_avg_max_prec.tif")

avg_now <- rast("output/climatic model/rasters/now/GEPE_northernmost_avg_now.tif")
avg_min_now <- rast("output/climatic model/rasters/now/GEPE_northernmost_avg_min_now.tif")
avg_max_now <- rast("output/climatic model/rasters/now/GEPE_northernmost_avg_max_now.tif")


#-------------------------------------------------------------------------------
# 3. Extract temperature, precipitation, and nearest open water data
#-------------------------------------------------------------------------------

# limit points to target subareas
pts <- bind_spat_rows(cols, bg) %>%
  filter(subarea %in% subareas)

# convert points to same CRS
pts <- pts %>% project(crs(avg_temp))

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

# assign to an ocean sector
pts <- pts %>%
  mutate(sector = case_when(
    subarea %in% c("Subarea 48.1", "Subarea 48.2", "Subarea 48.3", "Subarea 48.4",
                   "Subarea 48.5", "Subarea 48.6", "Bouvet", "Falklands", "Tristan da Cunha") ~ 
      "Atlantic",
    subarea %in% c("Subarea 88.1", "Subarea 88.2", "Subarea 88.3", "Chile", 
                   "Macquarie", "NZ Subantarctic") ~
      "Pacific",
    subarea %in% c("Division 58.4.1", "Division 58.4.2", "Division 58.5.1", "Division 58.5.2",
                   "Subarea 58.6", "Subarea 58.7", "Amsterdam and St Paul") ~
      "Indian"
  ))

# if y > -50, relabel nearest open water as 0 (NA otherwise)
pts <- pts %>%
  mutate(avg_now = ifelse(y >= -50, 0, avg_now),
         avg_min_now = ifelse(y >= -50, 0, avg_min_now),
         avg_max_now = ifelse(y >= -50, 0, avg_max_now))

# bank northernmost points
north_pts <- pts

#-------------------------------------------------------------------------------
# 4. Export rasters for making predictions
#-------------------------------------------------------------------------------

# # temp
# writeRaster(avg_temp, paste0("output/climatic model/rasters/temp/", species, "_northernmost_avg_temp.tif"))
# writeRaster(avg_min_temp, paste0("output/climatic model/rasters/temp/", species, "_northernmost_avg_min_temp.tif"))
# writeRaster(avg_max_temp, paste0("output/climatic model/rasters/temp/", species, "_northernmost_avg_max_temp.tif"))
# 
# # precipitation
# writeRaster(avg_prec, paste0("output/climatic model/rasters/prec/", species, "_northernmost_avg_prec.tif"))
# writeRaster(avg_min_prec, paste0("output/climatic model/rasters/prec/", species, "_northernmost_avg_min_prec.tif"))
# writeRaster(avg_max_prec, paste0("output/climatic model/rasters/prec/", species, "_northernmost_avg_max_prec.tif"))
# 
# # open water
# writeRaster(avg_now, paste0("output/climatic model/rasters/now/", species, "_northernmost_avg_now.tif"))
# writeRaster(avg_min_now, paste0("output/climatic model/rasters/now/", species, "_northernmost_avg_min_now.tif"))
# writeRaster(avg_max_now, paste0("output/climatic model/rasters/now/", species, "_northernmost_avg_max_now.tif"))


#-------------------------------------------------------------------------------
# F. Combine all points together and export
#-------------------------------------------------------------------------------

# bind rows
all_pts <- bind_rows(south_pts, mid_pts, kerg_pts, crozet_pts, north_pts)

# export
saveRDS(all_pts, paste0("output/climatic model/extraction/", species, " extracted.rds"))
