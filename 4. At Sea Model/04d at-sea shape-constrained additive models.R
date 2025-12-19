#----------------------------------------
# Shape Constrained Additive Models
#----------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(scam)

# define species and stage
species <- "MAPE"
stage <- "pre-moult"

# load background data
bg <- readRDS(paste0("output/at-sea model/background/", species, " ", stage, " background.RDS"))

# read in dist2coast raster
dist2coast <- rast(paste0("output/at-sea model/dist2coast/dist2coast.nc"))
plot(dist2coast)
plot(bg, add = T, pch = ".")

# sample dist2coast data
bg$dist2col <- terra::extract(dist2coast, bg, ID = F)

# read in track distance to colony data
trackdist <- readRDS(paste0("output/at-sea model/dist2colony/", species, "_", stage, "_track_dists.RDS")) %>%
  rename(dist2col = distance) %>%
  mutate(pb = 1)

# convert background data to dataframe
bg <- bg %>%
  as.data.frame() %>%
  select(dist2col) %>%
  mutate(pb = 0)

# combine track and background data
data <- bind_rows(bg, trackdist %>% select(-colony)) %>%
  drop_na()

# subsample to test scam workflow (remove)
data <- sample_n(data, 1000)

# fit shape constrained additive model
m1 <- scam(pb ~ s(dist2col, bs = "mpd"),
            data = data,
            family = binomial)
plot(m1)

# export scam model
saveRDS(m1, paste0("output/at-sea model/availability/", species, "_", stage, "_scam_model.RDS"))
#test <- readRDS(paste0("output/at-sea model/availability/", species, "_", stage, "_scam_model.RDS"))

# create dataframe for prediction
testdata <- as.points(dist2coast) %>%
  as.data.frame(geom = "XY")

# rename dist2col column
names(testdata)[1] <- "dist2col"

# predict using fitted model
pred <- predict.scam(m1, testdata, type = "response")
pred <- as.vector(pred)

# add to database
testdata$pscam <- pred

# convert prediction to points
predscam <- testdata %>%
  vect(geom = c("x", "y"), crs = crs(dist2coast))

# rasterise prediction
predrast <- rasterize(predscam, dist2coast, field = "pscam")

# plot prediction
plot(predrast)

# export prediction raster
writeCDF(predrast, 
         filename = paste0("output/at-sea model/availability/", species, "_", stage, "_scam_dist2coast.nc"))

