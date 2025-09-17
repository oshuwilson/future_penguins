#-------------------------------------------------------------------------------
# Balancing Climatic Background Samples
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(GeoThinneR)

# define species
species <- "ADPE"

# read in presence-absence data
pts <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))

# isolate background and presence location
bg <- pts %>% 
  filter(pa == "absence")
pr <- pts %>%
  filter(pa == "presence")

# load in one of the ERA5 rasters 
temp <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")

# thin background locations spatially to one point per grid cell
quick_thin <- thin_points(
  data = bg,
  lon_col = "x",
  lat_col = "y",
  method = "grid",
  raster_obj = temp[[1]]
)

# get thinned data
thinned <- largest(quick_thin)

# limit to maximum of 50 points per subarea
small <- thinned %>%
  group_by(subarea) %>%
  summarise(n = n()) %>%
  filter(n < 50) %>%
  pull(subarea)
downsampled <- thinned %>%
  filter(!subarea %in% small) %>%
  group_by(subarea) %>%
  sample_n(20)
unchanged <- thinned %>%
  filter(subarea %in% small)
thinned2 <- rbind(downsampled, unchanged)

# join the thinned data with the presence data
all <- rbind(thinned2, pr)

# convert to terra
allx <- all %>% vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")

# plot
ggplot() +
  geom_spatvector(data = allx, aes(col = subarea))
ggplot(all, aes(x = pa, y = avg_now)) +
  geom_jitter()

# export
saveRDS(all, paste0("output/climatic model/thinned/", species, " thinned.rds"))
