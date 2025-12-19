#----------------------------------------
# Extract to background and presence data
#----------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}
extract <- terra::extract

# species
species <- "GEPE"

# stage
stage <- "chick-rearing"

# load dynamic extract function
source("code/R/dynamic_extract.R")


# 1. Formatting

# read in tracks
tracks <- readRDS(paste0("output/at-sea model/thinned_tracks/", species, "_", stage, "_thinned.RDS"))

# convert tracks to terra
trax <- vect(tracks, geom = c("lon", "lat"), crs = "epsg:4326")

# read in background samples
bg <- readRDS(paste0("output/at-sea model/background/", species, " ", stage, " background.RDS")) %>%
  project("epsg:4326")

# create pa column
trax$pa <- "presence"
bg$pa <- "background"

# combine
data <- bind_spat_rows(trax, bg)


# 2. Extract environmental variables
# 2.1 Static Variables

# depth
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")

# extract
data$depth <- extract(depth, data, ID=F)

# remove rows where depth is NA - will be NA for every GLORYS variable
plot(data)
data <- data %>% drop_na(depth)
plot(data)

# slope
slope <- rast("E:/Satellite_Data/static/slope/slope.nc")
data$slope <- extract(slope, data, ID=F)

# dshelf
dshelf <- rast("E:/Satellite_Data/static/dshelf/dshelf.nc")
data$dshelf <- extract(dshelf, data, ID=F)

# cleanup static
rm(depth, slope, dshelf)


#  2.2 Dynamic Variables 

# sst 
data <- dynamic_extract("sst", data, crop = F)
print("sst")

# mld
data <- dynamic_extract("mld", data, crop = F)
print("mld")

# sal
data <- dynamic_extract("sal", data, crop = F)
print("sal")

# sic
data <- dynamic_extract("sic", data, crop = F)
data$sic[is.na(data$sic)] <- 0 # SIC values of 0 print as NA in GLORYS
print("sic")

# curr
data <- dynamic_extract("uo", data, crop = F)
data <- dynamic_extract("vo", data, crop = F)
data$curr <- sqrt((data$uo^2) + (data$vo^2))
print("curr")

# export
saveRDS(data, paste0("output/at-sea model/extraction/", species, " ", stage, " extracted.RDS"))

# print completion
print(paste0(species, " ", stage, " extracted"))


# 3. Bonus - plots

# change x for var of interest
ggplot(data, aes(x = sic)) +
  geom_histogram(aes(fill = pa), alpha = 0.5)

