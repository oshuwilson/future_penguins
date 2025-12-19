#------------------------------------------------------------
# Exclude Nearby Background Samples for Colony Climate Models
#------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

#-------------------------------------------------------------------------------
# 1. Format background samples and colony data
#-------------------------------------------------------------------------------

# set species
species <- "EMPE"

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

if(species == "EMPE"){
  target_months <- c(5, 6, 7, 8, 9, 10, 11, 12, 1)
}

# read in temperature, precipitation, and nearest open water data
temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
now <- rast("E:/Satellite_Data/monthly/now/now_resampled.nc")

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


#-------------------------------------------------------------------------------
# Extract emperor specific variables
#-------------------------------------------------------------------------------

# read in sea ice persistence, fast ice area, and distance to coastline
sip <- rast("output/climatic model/sea_ice_persistence/present_persistence.nc")
fast_ice <- rast("E:/Satellite_Data/static/fast_ice_area/emp_present.tif")
dist2coast <- rast("E:/Satellite_Data/static/dist2coast_emp.tif")

# resample sip to climatic model rasters
sip <- project(sip, crs(avg_temp))
sip <- resample(sip, avg_temp, method = "bilinear")

# interpolate sip around coast
sip <- focal(x = sip, w = 11, fun = "mean", na.policy = "only", na.rm = TRUE)
plot(sip)

# extract values
pts$sip <- extract(sip, pts, ID = F)
pts$fast_ice <- extract(fast_ice, pts, ID = F)
pts$dist2coast <- extract(dist2coast, pts, ID = F)

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

# if dist2coast is NA, make max value (125000)
pts <- pts %>%
  mutate(dist2coast = ifelse(is.na(dist2coast), 125000, dist2coast))

# if sea ice persistence is NA, make 0 is now is 0 or random number from 240 to 365 if now > 0
pts <- pts %>%
  mutate(sip = case_when(
    is.na(sip) & avg_now == 0 ~ 0,
    is.na(sip) & avg_now > 0 ~ sample(240:365, size = 1),
    TRUE ~ sip
  ))

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

# sea ice persistence
writeRaster(sip, paste0("output/climatic model/rasters/", species, "_sip.tif"))