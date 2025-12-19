#-------------------------------------------------------------------------------
# Isolate ice-free rock within a reachable elevation and distance to coast
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(dplyr)
library(lubridate)
library(ggplot2)
library(terra)
library(tidyterra)
select <- dplyr::select

# define species for this run
species <- "KIPE"

# candidate variables
preds <- c("elevation", "dist2coast", "rock")

# read in extracted colony data 
colonies <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))
colonies2 <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_rema.rds"))
colonies2$rock[is.na(colonies2$rock)] <- 0

# combine the dataframes 
colonies <- bind_rows(colonies, colonies2)
rm(colonies2)

# plot to identify outliers
ggplot(colonies, aes(x = dist2coast)) +
  geom_histogram()
ggplot(colonies, aes(x = elevation)) +
  geom_histogram()

# define max thresholds based on 95th percentile
max_dist2coast <- quantile(colonies$dist2coast, 0.95, na.rm = TRUE)
max_elevation <- quantile(colonies$elevation, 0.95, na.rm = TRUE)

# create threshold df 
thresholds <- data.frame(species = species,
                         max_dist = max_dist2coast,
                         max_elev = max_elevation)

# export thresholds
saveRDS(thresholds, paste0("output/topographic model/available areas/", species, "_topographic_thresholds.rds"))

#-------------------------------------------------------------------------------
# Delimit available GLO90 areas
#-------------------------------------------------------------------------------

# define all DEMs
dems <-  c("south_georgia_glo90", "kerguelen_glo90", 
           "crozet_glo90", "prince_edward_islands_glo90",
           "macquarie_glo90", "falklands_glo90",
           "heard_glo90", "bouvet_glo90",
           "south_orkney_glo90", "south_sandwich_glo90",
           "chile_diego_ramirez_glo90", "chile_ildefonso_glo90",
           "chile_noir_glo90")

# loop through DEMs to create available area
for(this.dem in dems){
  print(this.dem)
  
  # read in elevation, dist2coast, and rock
  elev <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/output_hh.nc"))
  dist2coast <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/dist_to_coast.nc"))
  rock <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rock.nc")) 
  
  # limit elevation, dist2coast, and rock to thresholds
  elev[elev > max_elevation | elev == 0] <- NA
  dist2coast[dist2coast > max_dist2coast] <- NA
  rock[rock == 0] <- NA
  
  # combine the three rasters
  available_area <- elev * dist2coast * rock 
  
  # vectorise 
  available_areas <- as.polygons(available_area, dissolve = TRUE)
  plot(available_areas)
  
  # join with other GLO90 available areas
  if(this.dem == dems[1]){
    all_available_areas <- available_areas
  } else {
    all_available_areas <- bind_spat_rows(all_available_areas, available_areas)
  }
}

# save all available areas
saveRDS(all_available_areas, paste0("output/topographic model/available areas/", species, "_available_areas_glo90.rds"))


#-------------------------------------------------------------------------------
# Delimit available REMA areas (present day)
#-------------------------------------------------------------------------------

# read in REMA DEM
elev <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_elevation_cropped.tif")
dist2coast <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_dist2coast_cropped.tif")
rock <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rock_cropped.tif")

# limit elevation, dist2coast, and rock to thresholds
elev[elev > max_elevation | elev == 0] <- NA
dist2coast[dist2coast > max_dist2coast] <- NA
rock[rock == 0] <- NA

# combine the three rasters
available_area <- elev * dist2coast * rock 
plot(available_area %>% crop(ext(-2.6e06, -2e06, 1e06, 1.7e06)))

# vectorise 
available_areas <- as.polygons(available_area, dissolve = TRUE)

# export
saveRDS(available_areas, paste0("output/topographic model/available areas/", species, "_available_areas_rema.rds"))


#-------------------------------------------------------------------------------
# Delimit available REMA areas (high emissions future)
#-------------------------------------------------------------------------------

#read in thresholds
thresholds <- readRDS(paste0("output/topographic model/available areas/", species, "_topographic_thresholds.rds"))
max_elevation <- thresholds %>% pull(max_elev)
max_dist2coast <- thresholds %>% pull(max_dist)

# read in REMA DEM
elev <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_elevation_cropped.tif")
dist2coast <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_dist2coast_cropped.tif")

# limit elevation, dist2coast, and rock to thresholds
elev[elev > max_elevation | elev == 0] <- NA
dist2coast[dist2coast > max_dist2coast] <- NA

# read in future ice free rock
future_rock <- rast("data/ice_free_rock/Ice_Free_Rock_RCP85/REMA_100m_future_rock_rcp85.tif")

# combine with elevation and dist2coast
future_area <- elev * dist2coast * future_rock
plot(future_area %>% crop(ext(-2.6e06, -2e06, 1e06, 1.7e06)))

# vectorise
future_areas <- as.polygons(future_area, dissolve = TRUE)

# export
saveRDS(future_areas, paste0("output/topographic model/available areas/", species, "_ssp585_areas_rema.rds"))



#-------------------------------------------------------------------------------
# Delimit available REMA areas (low emissions future)
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(dplyr)
library(lubridate)
library(ggplot2)
library(terra)
library(tidyterra)
select <- dplyr::select

# species options
species_options <- c("GEPE", "MAPE", "ADPE", "CHPE")

# loop over species
for(species in species_options){
  print(species)
  
  #read in thresholds
  thresholds <- readRDS(paste0("output/topographic model/available areas/", species, "_topographic_thresholds.rds"))
  max_elevation <- thresholds %>% pull(max_elev)
  max_dist2coast <- thresholds %>% pull(max_dist)
  
  # read in REMA DEM
  elev <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_elevation_cropped.tif")
  dist2coast <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_dist2coast_cropped.tif")
  
  # limit elevation, dist2coast, and rock to thresholds
  elev[elev > max_elevation | elev == 0] <- NA
  dist2coast[dist2coast > max_dist2coast] <- NA
  
  # read in future ice free rock
  future_rock <- rast("data/ice_free_rock/Ice_Free_Rock_RCP26/REMA_100m_future_rock_rcp26.tif")
  
  # combine with elevation and dist2coast
  future_area <- elev * dist2coast * future_rock
  plot(future_area %>% crop(ext(-2.6e06, -2e06, 1e06, 1.7e06)))
  
  # vectorise
  future_areas <- as.polygons(future_area, dissolve = TRUE)
  
  # export
  saveRDS(future_areas, paste0("output/topographic model/available areas/", species, "_ssp126_areas_rema.rds"))
  
}
