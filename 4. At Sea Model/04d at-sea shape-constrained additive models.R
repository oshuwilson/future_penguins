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
species <- "KIPE"
stage <- "chick-rearing"

# load background data
bg <- readRDS(paste0("output/background/", species, "/global ", stage, " background.RDS"))

# read in dist2col raster
dist2col <- rast(paste0("output/dist2colony/", species, "_dist_to_colony.nc"))
plot(dist2col)
plot(bg, add = T, pch = ".")

# sample dist2col data
bg$dist2col <- terra::extract(dist2col, bg, ID = F)

# read in track distance to colony data
trackdist <- readRDS(paste0("output/dist2colony/", species, "_", stage, "_track_dists.RDS")) %>%
  rename(dist2col = distance) %>%
  mutate(pb = 1)

# convert background data to dataframe
bg <- bg %>%
  as.data.frame() %>%
  select(colony, dist2col) %>%
  mutate(pb = 0)

# combine track and background data
data <- bind_rows(bg, trackdist)

# subsample to test scam workflow (remove)
data <- sample_n(data, 1000)

# make colony a factor
data <- data %>%
  mutate(colony = as.factor(colony))

# fit shape constrained additive model
m1 <- scam(pb ~ s(dist2col, bs = "mpd") +
            s(colony, bs = "re"), 
            data = data, 
            family = binomial)

# create dataframe for prediction
testdata <- as.points(dist2col) %>%
  as.data.frame(geom = "XY")

# rename dist2col column
names(testdata)[1] <- "dist2col"

# add test colony column
testdata$colony <- "test_colony"

# predict using fitted model
pred <- predict.scam(m1, testdata, type = "response")
pred <- as.vector(pred)

# add to database
testdata$pscam <- pred

# convert prediction to points
predscam <- testdata %>%
  vect(geom = c("x", "y"), crs = crs(dist2col))

# rasterise prediction
predrast <- rasterize(predscam, dist2col, field = "pscam")

# plot prediction
plot(predrast)

# export prediction raster
writeCDF(predrast, 
         filename = paste0("output/availability/", species, "_", stage, "_scam_dist2col.nc"))
