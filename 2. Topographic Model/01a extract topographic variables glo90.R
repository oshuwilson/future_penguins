#----------------------------------------------------------
# Extract topographic variables for colonies from GLO90 DEM
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
             "South Orkney Islands", "South Sandwich Islands"),
  dem = c("south_georgia_glo90", "kerguelen_glo90", 
          "crozet_glo90", "prince_edward_islands_glo90",
          "macquarie_glo90", "falklands_glo90",
          "heard_glo90", "bouvet_glo90",
          "south_orkney_glo90", "south_sandwich_glo90")
)

# eliminate regions not in dem_names
regions <- regions[regions %in% dem_names$region]

# loop through regions
for(this.region in regions){
  
  # get corresponding DEM name
  this.dem <- dem_names %>%
    filter(region == this.region) %>%
    pull(dem)
  
  # isolate colonies 
  these_colonies <- colonies %>%
    filter(region == this.region)
  
  # read in DEM variables
  dem <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/output_hh.nc"))
  dist2coast <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/dist_to_coast.nc"))
  slope <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/slope.nc"))
  aspect <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/aspect.nc"))
  rugosity <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rugosity.nc"))
  rock <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rock.nc"))
  wave_exposure <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/wave_exposure_", species, ".nc"))
  
  # project colonies to DEM CRS (should be same already)
  these_colonies <- project(these_colonies, crs(dem))
  
  # extract values
  these_colonies$elevation <- extract(dem, these_colonies, ID = F)
  these_colonies$dist2coast <- extract(dist2coast, these_colonies, ID = F)
  these_colonies$slope <- extract(slope, these_colonies, ID = F)
  these_colonies$aspect <- extract(aspect, these_colonies, ID = F)
  these_colonies$rugosity <- extract(rugosity, these_colonies, ID = F)
  these_colonies$rock <- extract(rock, these_colonies, ID = F)
  these_colonies$wave_exposure <- extract(wave_exposure, these_colonies, ID = F)
  
  # convert to dataframe
  these_colonies_df <- as.data.frame(these_colonies, geom = "XY")
  
  # combine with all regions
  if(this.region == regions[1]) {
    all_colonies <- these_colonies_df
  } else {
    all_colonies <- bind_rows(all_colonies, these_colonies_df)
  }
  
  # print completion
  print(this.region)
  
}

# remove elevation values of 0 (in ocean on DEM)
all_colonies <- all_colonies %>%
  filter(elevation != 0)

# plot
ggplot(all_colonies, aes(x = elevation)) + geom_histogram()
ggplot(all_colonies, aes(x = dist2coast)) + geom_histogram()
ggplot(all_colonies, aes(x = slope)) + geom_histogram()
ggplot(all_colonies, aes(x = rugosity)) + geom_histogram()

# read in corresponding subarea names
col_subareas <- readRDS(paste0("data/colonies/subareas/", species, "_colonies_subareas.rds"))

# append to colony extractions
all_colonies <- all_colonies %>%
  left_join(col_subareas %>% select(name, subarea)) %>%
  distinct()

# export
saveRDS(all_colonies,
        file = paste0("output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))
