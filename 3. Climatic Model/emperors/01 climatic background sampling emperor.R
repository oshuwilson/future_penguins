#----------------------------------------------------
# Create Background Samples for Colony Climate Models
#----------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(sf)
library(CCAMLRGIS)

# load in a world map
world <- ne_countries(scale = 10, returnclass = "sv")

# crop to below 35 degrees south (to capture all penguin islands)
world <- crop(world, ext(-180, 180, -90, -35))
world$subregion

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

# get antarctica from AAD shapefile
antarctica <- read_sf("~/OneDrive - University of Southampton/Documents/Predictor Data/coast/add_coastline_medium_res_polygon_v7_9.shp")
antarctica <- antarctica %>% vect()

# unionise antarctica
antarctica <- antarctica %>% aggregate()
plot(antarctica)

# project all non-antarctic regions to the same CRS as Antarctica
subantarctic <- subantarctic %>% project(crs(antarctica))
falklands <- falklands %>% project(crs(antarctica))
marion <- marion %>% project(crs(antarctica))
bouvet <- bouvet %>% project(crs(antarctica))
macquarie <- macquarie %>% project(crs(antarctica))
nz <- nz %>% project(crs(antarctica))
tdc <- tdc %>% project(crs(antarctica))
chile <- chile %>% project(crs(antarctica))

# join all isolated regions into one
penguin_potential <- rbind(antarctica, subantarctic, falklands, marion, bouvet, macquarie, nz, tdc, chile)

# visualise
ggplot(penguin_potential) +
  geom_spatvector(aes(col = subregion))

# convert into lines for coastline
penguin_lines <- penguin_potential %>% as.lines()

# buffer lines by max distance of any emperor penguin colony to the coast (184km)
penguin_buff <- buffer(penguin_lines, 184000)

# only keep points that intersect with ocean (no points on land)
penguin_buff <- erase(penguin_buff, penguin_potential)

# sample 30000 background points
bg <- spatSample(penguin_buff, 30000)

# check samples
nrow(bg)
plot(bg)

# load CCAMLR subareas
subareas <- load_ASDs() %>% vect() %>%
  project(crs(antarctica))

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
  
  # if i is a multiple of 100, print progress
  if(i %% 100 == 0){
    print(paste0("Processed ", i, " of ", length(bg), " points"))
  }
}

# plot results
ggplot(bg_subareas, aes(col = subarea)) +
  geom_spatvector()

# convert to data frame
background <- bg_subareas %>% 
  project("epsg:4326") %>%
  as.data.frame(geom = "XY") 

# rename NA samples to other islands
background <- background %>%
  mutate(subarea = case_when(
    x > -65 & x < -55 & y > -55 ~ "Falklands",
    x > 155 & x < 161 & y > -56 & y < -52 ~ "Macquarie",
    x > 160 & y > -55 & y < -40 | x < -170 & y > -55 & y < -40 ~ "NZ Subantarctic",
    x > 0 & x < 10 & y > -57 & y < -50 ~ "Bouvet",
    x > 75 & x < 80 & y > -42 ~ "Amsterdam and St Paul",
    x > -15 & x < -5 & y > -42 ~ "Tristan da Cunha",
    x < -65 & y > -58 ~ "Chile", 
    TRUE ~ subarea
  ))

# reconvert to terra and plot
bgp <- background %>%
  vect(geom = c("x", "y"), crs = "epsg:4326")
ggplot(bgp %>% project("epsg:3031"), aes(col = subarea)) +
  geom_spatvector() +
  scale_color_viridis_d()

# export
saveRDS(background,
        "output/climatic model/background/background template emperor.rds")
