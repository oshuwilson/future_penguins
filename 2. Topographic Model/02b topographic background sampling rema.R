#------------------------------------------------
# Generate Topographic Background Samples - REMA
#------------------------------------------------

# ADD WAVE EXPOSURE AND ICE-FREE-ROCK
# REMOVE LOCATIONS WITHIN 500m OF COLONIES

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(CCAMLRGIS)

# set species
species <- "ADPE"

# read in extracted colony data
colonies <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))
colonies2 <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_rema.rds"))

# get max distance to colony
max_dist <- max(c(colonies$dist2coast, colonies2$dist2coast))

# identify unique regions
regions <- unique(colonies$region)

# read in REMA DEM variables
dem <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_elevation_cropped.tif")  
slope <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_slope_cropped.tif")
aspect <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_aspect_cropped.tif")
rugosity <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rugosity_cropped.tif")
rock <- rast("E:/Satellite_Data/static/DEM/REMA_100m/REMA_100m_rock.tif")
wave_exposure <- rast(paste0("E:/Satellite_Data/static/DEM/REMA_100m_cropped/wave_exposure_", species, "_cropped.tif"))

# read in Antarctic coastline
coast <- sf::read_sf("E:/Satellite_Data/static/coast/add_coastline_medium_res_polygon_v7_9.shp")
coast <- coast %>% filter(surface == "land")

# aggregate coastline
coast <- coast %>% 
  vect() %>%
  aggregate() %>%
  project(crs(aspect))

# remove south orkney (not in REMA)
south_orkney <- ext(-2.5e+6, -2e+6, 2e+6, 2.5e+6) %>%
  vect(crs = crs(coast))
coast <- erase(coast, south_orkney)

# generate a mask layer within 2x max_dist of coast
inner <- terra::buffer(coast, - max_dist * 2 * 1000)
mask <- erase(coast, inner)
plot(mask)

# get spatvector of colonies
colonies <- colonies2 %>% 
  vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project(crs(coast))
plot(cols, add = T, col = "red")

# create a buffer of 500m around colonies and mask out
colony_buffer <- buffer(colonies, 500)
mask <- erase(mask, colony_buffer)

# generate background samples around whole coastline
n_samples <- 20000
bg <- spatSample(mask, n_samples)
plot(bg)

# load in CCAMLR subareas
subareas <- load_ASDs() 
subareas <- vect(subareas) %>%
  project(crs(coast))
subareas <- subareas %>% select(GAR_Name)

# extract subareas to colonies
colonies$subarea <- extract(subareas, colonies)[,2]

# list subareas that contain colonies
colony_subareas <- unique(colonies$subarea)
colony_subareas <- colony_subareas[!is.na(colony_subareas)]

# make column in subareas to indicate which are being sampled
subareas$sampled <- ifelse(subareas$GAR_Name %in% colony_subareas, TRUE, FALSE)

# plot to check
ggplot() +
  geom_spatvector(data = subareas, aes(fill = sampled)) +
  geom_spatvector(data = colonies)

# get nearest subarea to each background point
for(i in 1:length(bg)){
  
  # isolate point
  pt <- bg[i]
  
  # calculate distance to nearest subareas
  dists <- distance(pt, subareas)
  
  # get nearest ID
  nearest_idx <- which.min(dists)
  
  # get subarea name
  pt$subarea <- subareas[nearest_idx]$GAR_Name
  
  # combine to all other points
  if(i == 1){
    bg_subareas <- pt
  } else {
    bg_subareas <- bind_spat_rows(bg_subareas, pt)
  }
  
  # if i is a multiple of 1000, print progress
  if(i %% 1000 == 0){
    print(paste0("Processed ", i, " of ", length(bg), " points"))
  }
}

# plot to check
ggplot(bg_subareas) +
  geom_spatvector(aes(col = subarea))

# limit to subareas that contain colonies
bg <- bg_subareas %>%
  filter(subarea %in% colony_subareas)

# extract values from DEM
bg$elevation <- extract(dem, bg, ID = F)
bg$dist2coast <- extract(dist2coast, bg, ID = F)
bg$slope <- extract(slope, bg, ID = F)
bg$aspect <- extract(aspect, bg, ID = F)
bg$rugosity <- extract(rugosity, bg, ID = F)
bg$rock <- extract(rock, bg, ID = F)
bg$wave_exposure <- extract(wave_exposure, bg, ID = F)

# revalue rock values of NA to 0
bg$rock[is.na(bg$rock)] <- 0

# get distance to coast
coast <- as.lines(coast)
bg$dist2coast <- distance(bg, coast, unit = "km")
  
# plot
ggplot(bg, aes(x=elevation)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=elevation), fill="darkred", alpha=0.5)

ggplot(bg, aes(x=dist2coast)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=dist2coast), fill="darkred", alpha=0.5)

ggplot(bg, aes(x=slope)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=slope), fill="darkred", alpha=0.5)

ggplot(bg, aes(x=aspect)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=aspect), fill="darkred", alpha=0.5)

ggplot(bg, aes(x=rugosity)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=rugosity), fill="darkred", alpha=0.5)

ggplot(bg, aes(x=rock)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=rock), fill="darkred", alpha=0.5)

ggplot(bg, aes(x=wave_exposure)) + geom_density(fill = "steelblue", alpha = 0.5) +
  geom_density(data = colonies2, aes(x=wave_exposure), fill="darkred", alpha=0.5)

# convert to dataframe
bg_all <- bg %>%
  as.data.frame(geom = "XY") %>%
  mutate(dem = "REMA")

# export background samples
saveRDS(bg_all, paste0("output/topographic model/extractions/", species, "_background_rema.rds"))
