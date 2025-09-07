#------------------------------------------------
# Generate Topographic Background Samples - GLO90
#------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# set species
species <- "ADPE"

# read in extracted colony data
colonies <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))
colonies2 <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_rema.rds"))

# get max distance to colony
max_dist <- max(c(colonies$dist2coast, colonies2$dist2coast))

# identify unique regions
regions <- unique(colonies$region)

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

# eliminate get relevant DEM list for this species
dems <- dem_names %>%
  filter(region %in% regions) %>%
  pull(dem)

# for each DEM
for(this.dem in dems){
  
  # read in DEM rasters
  dem <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/output_hh.nc"))
  dist2coast <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/dist_to_coast.nc"))
  slope <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/slope.nc"))
  aspect <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/aspect.nc"))
  rugosity <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rugosity.nc"))
  rock <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rock.nc"))
  wave_exposure <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/wave_exposure_", species, ".nc"))
  
  # revalue dist2coast values to within 2x max_dist or beyond
  m1 <- matrix(c(0, max_dist * 2, 1, 
                 max_dist * 2, Inf, 2), 
               ncol = 3, byrow = TRUE)
  distmask <- classify(dist2coast, m1)
  plot(distmask)
  
  # get a SpatVector of distmask values of 1
  mask <- as.polygons(distmask) %>%
    filter(dist_to_coast == 1)
  plot(mask, col = "red")
  
  # get colonies from this region
  these_colonies <- colonies %>%
    filter(region == dem_names$region[dem_names$dem == this.dem]) %>%
    vect(geom = c("x", "y"), crs = "EPSG:4326")
  plot(these_colonies, add = T)
  
  # create 200m buffer around colonies and erase from mask
  colony_buffer <- terra::buffer(these_colonies, width = 500)
  mask <- erase(mask, colony_buffer)
  plot(mask, col = "red")
  
  # generate background samples within the mask
  n_samples <- 1000
  bg <- spatSample(mask, n_samples)
  plot(bg)
  
  # extract values from DEM
  bg$elevation <- extract(dem, bg, ID = F)
  bg$dist2coast <- extract(dist2coast, bg, ID = F)
  bg$slope <- extract(slope, bg, ID = F)
  bg$aspect <- extract(aspect, bg, ID = F)
  bg$rugosity <- extract(rugosity, bg, ID = F)
  bg$rock <- extract(rock, bg, ID = F)
  bg$wave_exposure <- extract(wave_exposure, bg, ID = F)
  
  # convert to dataframe
  this.region <- dem_names %>%
    filter(dem == this.dem) %>%
    pull(region)
  bg_df <- as.data.frame(bg, geom = "XY") %>%
    mutate(region = this.region)
  
  # combine to all other regions
  if(this.dem == dems[1]){
    bg_all <- bg_df
  } else {
    bg_all <- bind_rows(bg_all, bg_df)
  }
  
  # print success
  print(this.region)
  
}

#plots
ggplot(bg_all, aes(x=elevation)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=elevation), fill="darkred", alpha=0.5)

ggplot(bg_all, aes(x=dist2coast)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=dist2coast), fill="darkred", alpha=0.5)

ggplot(bg_all, aes(x=slope)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=slope), fill="darkred", alpha=0.5)

ggplot(bg_all, aes(x=aspect)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=aspect), fill="darkred", alpha=0.5)

ggplot(bg_all, aes(x=rugosity)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=rugosity), fill="darkred", alpha=0.5)

ggplot(bg_all, aes(x=rock)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=rock), fill="darkred", alpha=0.5)

ggplot(bg_all, aes(x=wave_exposure)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies, aes(x=wave_exposure), fill="darkred", alpha=0.5)

# append subareas using corresponding colony subareas
col_subareas <- readRDS(paste0("data/colonies/subareas/", species, "_colonies_subareas.rds"))
region_subareas <- col_subareas %>%
  filter(region %in% regions) %>%
  group_by(region) %>%
  summarise(subarea = unique(subarea)) 
bg_all <- bg_all %>% left_join(region_subareas)


# export background samples
saveRDS(bg_all, paste0("output/topographic model/extractions/", species, "_background_glo90.rds"))
