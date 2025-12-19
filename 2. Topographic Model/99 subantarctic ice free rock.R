rm(list=ls())
setwd("E:/Satellite_Data/static")

library(sf)
library(tidyverse)
library(tidyterra)
library(terra)

# read in Glacier locations from the Randolph Glacier Inventory
surf <- read_sf("subantarctic glaciers/RGI2000-v7.0-G-19_subantarctic_antarctic_islands.shp") %>%
  vect()

# list DEMs
dems <- list.dirs("DEM/GLO90_100m", recursive = FALSE, full.names = TRUE)
dems

# remove chilean islands, marion, falklands, macquarie and crozet (no glaciers)
dems <- dems[!grepl("chile", dems)]
dems <- dems[!grepl("prince_edward", dems)]
dems <- dems[!grepl("falklands", dems)]
dems <- dems[!grepl("crozet", dems)]
dems <- dems[!grepl("macquarie", dems)]

# remove circumpolar DEM
dems <- dems[!grepl("circumpolar", dems)]
dems

# read in DEM
for(this.dem in dems[2:6]){
  dem <- rast(paste0(this.dem, "/output_hh.nc"))
  plot(dem)
  
  # crop glaciers to this DEM
  glaciers <- crop(surf, ext(dem))
  plot(glaciers, add = T, col = "white")
  
  # mask out glaciers
  masked <- mask(dem, glaciers, inverse = TRUE)
  
  # reclassify so that masked values are 0 and 0s are NAs, all else is 1
  m1 <- matrix(c(-Inf, 0, NA,
                 0, Inf, 1,
                 NA, NA, 0),
               ncol = 3, byrow = TRUE)
  icefreerock <- classify(masked, m1)
  plot(icefreerock)
  
  # export ice free rock layer
  writeCDF(icefreerock, file = paste0(this.dem, "/rock.nc"))
}

#-------------------------------------------------------------------------------
# Repeat for ice-free regions
#-------------------------------------------------------------------------------

rm(list=ls())

# list DEMs
dems <- list.dirs("DEM/GLO90_100m", recursive = FALSE, full.names = TRUE)

# keep islands with no glaciers
chile <- dems[grepl("chile", dems)]
pei <- dems[grepl("prince_edward", dems)]
fk <- dems[grepl("falklands", dems)]
cz <- dems[grepl("crozet", dems)]
mac <- dems[grepl("macquarie", dems)]
dems <- c(chile, pei, fk, cz, mac)
dems

# read in DEM
for(this.dem in dems){
  dem <- rast(paste0(this.dem, "/output_hh.nc"))
  plot(dem)
  
  # reclassify so that masked values are 0 and 0s are NAs, all else is 1
  m1 <- matrix(c(-Inf, 0, NA,
                 0, Inf, 1,
                 NA, NA, 0),
               ncol = 3, byrow = TRUE)
  icefreerock <- classify(dem, m1)
  plot(icefreerock)
  
  # export ice free rock layer
  writeCDF(icefreerock, file = paste0(this.dem, "/rock.nc"))
}
