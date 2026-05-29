#----------------------------------------------------------
# Extract topographic variables for colonies from REMA DEM
#----------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# set species
species <- "GEPE"

# read in colony locations
colonies <- readxl::read_xlsx(paste0("data/colonies/Final Colonies/", species, "_by_colony.xlsx"))

# convert coords to numeric
colonies <- colonies %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

# convert to terra
colonies <- vect(colonies, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# identify unique regions
regions <- unique(colonies$region)
regions

# define region DEM equivalents
dem_names <- data.frame(
  region = c("South Georgia", "Kerguelen", 
             "Crozet", "Prince Edward", 
             "Macquarie", "Falklands",
             "Heard", "Bouvetoya",
             "South Orkney Islands", "South Sandwich Islands", "Chile"),
  dem = c("south_georgia_glo90", "kerguelen_glo90", 
          "crozet_glo90", "prince_edward_islands_glo90",
          "macquarie_glo90", "falklands_glo90",
          "heard_glo90", "bouvet_glo90",
          "south_orkney_glo90", "south_sandwich_glo90", "chile_glo90")
)

# eliminate regions in dem_names
regions <- regions[!regions %in% dem_names$region]
regions

# limit colonies to these regions
colonies <- colonies %>%
  filter(region %in% regions)

# read in REMA DEM variables
dem <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_elevation_cropped.tif")  
slope <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_slope_cropped.tif")
aspect <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_aspect_cropped.tif")
rugosity <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rugosity_cropped.tif")
rock <- rast("E:/Satellite_Data/static/DEM/REMA_100m/REMA_100m_rock.tif")
wave_exposure <- rast(paste0("E:/Satellite_Data/static/DEM/REMA_100m_cropped/wave_exposure_", species, "_cropped.tif"))

# project colonies to REMA CRS
colonies <- project(colonies, crs(dem))

# extract values
colonies$elevation <- extract(dem, colonies, ID = F)
colonies$slope <- extract(slope, colonies, ID = F)
colonies$aspect <- extract(aspect, colonies, ID = F)
colonies$rugosity <- extract(rugosity, colonies, ID = F)
colonies$rock <- extract(rock, colonies, ID = F)
colonies$wave_exposure <- extract(wave_exposure, colonies, ID = F)

# revalue rock values of NA to 0
colonies$rock[is.na(colonies$rock)] <- 0

# read in Antarctic coastline
coast <- sf::read_sf("E:/Satellite_Data/static/coast/add_coastline_medium_res_polygon_v7_9.shp")

# only keep land
coast <- coast %>%
  filter(surface == "land")

# aggregate coastline
coast <- coast %>% 
  vect() %>%
  aggregate()

# convert to lines
coast <- as.lines(coast)

# calculate distance to the coast
colonies$dist2coast <- distance(colonies, coast, unit = "km")
ggplot(colonies) + geom_spatvector(aes(col = dist2coast))

# reproject
colonies <- project(colonies, "epsg:4326")

# convert to dataframe
all_colonies <- as.data.frame(colonies, geom = "XY")

# remove elevation values of 0 (in ocean on DEM)
all_colonies <- all_colonies %>%
  filter(elevation != 0)

# plot
ggplot(all_colonies, aes(x = elevation)) + geom_histogram()
ggplot(all_colonies, aes(x = dist2coast)) + geom_histogram()
ggplot(all_colonies, aes(x = slope)) + geom_histogram()
ggplot(all_colonies, aes(x = rugosity)) + geom_histogram()
ggplot(all_colonies, aes(x = wave_exposure)) + geom_histogram()

# read in corresponding subarea names
col_subareas <- readRDS(paste0("data/colonies/subareas/", species, "_colonies_subareas.rds"))

# append to colony extractions
all_colonies <- all_colonies %>%
  left_join(col_subareas %>% select(name, subarea)) 

# export
saveRDS(all_colonies,
        file = paste0("output/topographic model/extractions/", species, "_colonies_extracted_rema.rds"))
