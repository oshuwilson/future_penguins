#-------------------------------------------------------------------------------
# Fast Ice Area maps
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(sf)
library(tidyterra)

# read in emperor colony locations
colonies <- readxl::read_xlsx("data/colonies/Final Colonies/EMPE_by_colony.xlsx") %>%
  distinct(name, .keep_all = T)

# convert to spatvector
colonies <- vect(colonies, geom = c("longitude", "latitude"), crs = "EPSG:4326") %>%
  project("EPSG:6932")

# read in fast ice area - present day
present <- rast("E:/Satellite_Data/static/fast_ice_area/emp_present.tif")

# read in all fast ice area files for ssp126
ssp126_files <- list.files("E:/Satellite_Data/static/fast_ice_area/", pattern = "_ssp126_.*\\.tif$", full.names = T)
ssp126_list <- lapply(ssp126_files, rast)
ssp126_stack <- do.call(c, ssp126_list)
ssp126_mean <- app(ssp126_stack, mean, na.rm = T)

# read in all fast ice area files for ssp585
ssp585_files <- list.files("E:/Satellite_Data/static/fast_ice_area/", pattern = "_ssp585_.*\\.tif$", full.names = T)
ssp585_list <- lapply(ssp585_files, rast)
ssp585_stack <- do.call(c, ssp585_list)
ssp585_mean <- app(ssp585_stack, mean, na.rm = T)

# project
present <- present %>% 
  crop(ext(-180.125, 179.875, -89.875, -60.125)) %>% 
  project("epsg:6932")
ssp126_mean <- ssp126_mean %>% 
  crop(ext(-180.125, 179.875, -89.875, -60.125)) %>% 
  project("epsg:6932")
ssp585_mean <- ssp585_mean %>% 
  crop(ext(-180.125, 179.875, -89.875, -60.125)) %>% 
  project("epsg:6932")

# differences
diff126 <- ssp126_mean - present
diff585 <- ssp585_mean - present

# read in coastline
coast <- readRDS("data/coast_vect.RDS")
coast <- coast %>%
  crop(ext(-2.7e6, 2.7e6, -2.6e6, 2.5e06))
plot(coast)

# max value across all three rasters
max_val <- max(c(minmax(present)[2,1], 
                 minmax(ssp126_mean)[2,1], 
                 minmax(ssp585_mean)[2,1]))

# min and max differences
diff_min <- min(c(minmax(diff126)[1,1],
                  minmax(diff585)[1,1]))
diff_max <- max(c(minmax(diff126)[2,1],
                  minmax(diff585)[2,1]))

# create graticule
grat <- st_graticule(
  lon = seq(-180, 180, 20),   # longitude spacing you want
  lat = seq(-80, -40, 10)
)
grat <- st_transform(grat, st_crs(present))
lon_labels <- grat[grat$lat == min(grat$lat), ]

# plot
p1 <- ggplot() +
  geom_spatraster(data = present) +
  geom_spatvector(data = coast, fill = "grey90", col = NA) +
  scale_fill_viridis_c(na.value = "transparent", 
                       limits = c(0, max_val), 
                       name = "Fast Ice Area (km²)") +
  theme_void() 

# plot ssp126
p2 <- ggplot() +
  geom_spatraster(data = ssp126_mean - present) +
  geom_spatvector(data = coast, fill = "grey90", col = NA) +
  scale_fill_gradient2(na.value = "transparent", 
                       limits = c(diff_min, diff_max), 
                       name = "Fast Ice Area Change (km²)") +
  theme_void() 

# plot ssp585
p3 <- ggplot() +
  geom_spatraster(data = ssp585_mean - present) +
  geom_spatvector(data = coast, fill = "grey90", col = NA) +
  scale_fill_gradient2(na.value = "transparent", 
                       limits = c(diff_min, diff_max), 
                       name = "Fast Ice Area Change (km²)") +
  theme_void()

# plot together
library(cowplot)
grid <- plot_grid(p1, p2, p3, ncol = 1)
grid + ggview::canvas(width = 9, height = 14)

# export
ggsave("text/figures/draft/supplementary/fast_ice_area_maps.png", grid, width = 9, height = 14)
