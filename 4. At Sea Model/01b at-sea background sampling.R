#----------------------------------------------------------------
# Background Sampling Within Set Distance to Coast
#----------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(foreach)
library(rnaturalearth)
library(CCAMLRGIS)

# define species and stage
species <- "KIPE"
this.stage <- "incubation"

# read in tracks for this stage
tracks <- readRDS(paste0("output/at-sea model/thinned_tracks/", species, "_", this.stage, "_thinned.RDS"))

# read in colony subareas 
subs <- readRDS("data/tracks/tracking_colony_subareas.rds")

# join subareas to tracks
tracks <- tracks %>%
  left_join(subs)

# convert to spatvector
trax <- tracks %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") 

# load in a world map
world <- ne_countries(scale = 10, returnclass = "sv")

# crop to below 35 degrees south (to capture all penguin islands)
world <- crop(world, ext(-180, 180, -90, -35))

# isolate antarctica
antarctica <- world %>%
  filter(subregion == "Antarctica")

# isolate subantarctic islands
# South Georgia, South Sandwich, Kerguelen, Crozet, Heard, Amsterdam and St Paul 
subantarctic <- world %>%
  filter(subregion == "Seven seas (open ocean)")

# still missing Bouvet, Macquarie, NZ Subantarctic Islands
# Tristan da Cunha, Gough, Falklands, Marion

# get Falklands
falklands <- world %>% filter(name == "Falkland Is.")

# get Marion
marion <- world %>% filter(name == "South Africa")

# get Bouvet
bouvet <- world %>% filter(name == "Norway")

# get Macquarie
macquarie <- world %>% filter(name == "Australia") %>%
  crop(ext(150, 160, -55, -50))

# get NZ Subantarctic Islands
nz <- world %>% filter(name == "New Zealand") %>%
  crop(ext(160, 180, -55, -47.5))

# get Tristan da Cunha and Gough
tdc <- world %>% filter(name == "Saint Helena")

# get chilean islands
chile <- world %>% filter(name == "Chile") %>%
  crop(ext(-73.2, -72.8, -54.6, -54.4))

# combine
coast <- bind_spat_rows(antarctica, bouvet, falklands, macquarie, chile, 
                        marion, nz, subantarctic, tdc)
coast <- aggregate(coast)
plot(coast)

# calculate the maximum distance from a track to the coast - often slow
distances <- distance(trax, coast)
max_dist <- max(distances, na.rm = T)

# save the max distance value for future runs
saveRDS(max_dist, paste0("output/at-sea model/distance buffers/", species, " ", this.stage, " max dist.rds"))

# buffer coastline
buff <- terra::buffer(coast, width = max_dist)

# erase land
seabuff <- erase(buff, coast)

# merge
mask <- aggregate(seabuff)
plot(mask)

# save mask for visualisations
saveRDS(mask, paste0("output/at-sea model/distance buffers/", species, " ", this.stage, " buffer mask.RDS"))

# sample background samples
bg <- spatSample(mask, 20000)

# multiply background samples for sampling different timestamps
bg2 <- deepcopy(bg)
bg3 <- deepcopy(bg)
bg4 <- deepcopy(bg)
bg <- rbind(bg, bg2, bg3, bg4)

plot(bg %>% project("epsg:6932"), pch = ".")

# sample individual id, date, and subarea from tracks
bg$individual_id <- sample(tracks$individual_id, nrow(bg), replace = T)
bg$date <- sample(tracks$date, nrow(bg), replace = T)
bg$subarea <- sample(tracks$subarea, nrow(bg), replace = T)

# export
saveRDS(bg, paste0("output/at-sea model/background/", species, " ", this.stage, " background.RDS"))

