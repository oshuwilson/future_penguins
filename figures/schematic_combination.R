#-------------------------------------------------------------------------------
# Model combination schematic
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(scam)


#-------------------------------------------------------------------------------
# 1. Colony Coordinates
#-------------------------------------------------------------------------------

# read in colony coordinates
colonies <- readRDS("data/colonies/subareas/KIPE_colonies_subareas.RDS")

# convert to spatvector
colonies <- vect(colonies, geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.rds")

# plot
p1 <- ggplot() +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  geom_spatvector(data = colonies, col = "red3", size = 3, shape = 17) +
  scale_fill_manual(values = c("grey80", "grey50"), guide = "none") +
  theme_void()
p1 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/1. Colonies.png",
       plot = p1,
       width = 8, height = 8, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# 2. Tracking Data
#-------------------------------------------------------------------------------

# read in tracking data
tracks <- readRDS("data/track_lines.RDS")
tracks <- tracks %>% filter(species == "KIPE")

# plot
p2 <- ggplot() +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  geom_spatvector(data = tracks, col = "red3", size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "grey50"), guide = "none") +
  theme_void()
p2 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/2. Tracking Data.png",
       plot = p2,
       width = 8, height = 8, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# 3. Environmental Covariates
#-------------------------------------------------------------------------------

# read in air temperature and precipitation
airt <- rast("E:/Satellite_Data/monthly/ERA5/air_temp_monthly.grib")
airt <- airt[[1]]
prec <- rast("E:/Satellite_Data/monthly/ERA5/total_precipitation_monthly.grib")
prec <- prec[[1]]

# project
airt <- airt %>%
  crop(ext(-180.125, 179.875, -89.875, -39.875)) %>%
  project("epsg:6932")
prec <- prec %>%
  crop(ext(-180.125, 179.875, -89.875, -39.875)) %>%
  project("epsg:6932")

# plot
p3 <- ggplot() +
  geom_spatraster(data = airt) + 
  geom_spatvector(data = coast, fill = NA, col = "white") +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  theme_void()
p3 + ggview::canvas(width = 8, height = 8)
p4 <- ggplot() +
  geom_spatraster(data = prec) + 
  geom_spatvector(data = coast, fill = NA, col = "white") +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  theme_void()
p4 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/3. Air Temperature.png",
       plot = p3,
       width = 8, height = 8, units = "in", dpi = 300)
ggsave("text/figures/draft/schematic_combination/4. Precipitation.png",
       plot = p4,
       width = 8, height = 8, units = "in", dpi = 300)

# read in sea surface temperature and sea ice concentration
sst <- rast("E:/Satellite_Data/monthly/sst/sst.nc")
sst <- sst[[1]]
sic <- rast("E:/Satellite_Data/monthly/sic/sic.nc")
sic <- sic[[1]]
values(sic) <- ifelse(is.na(values(sic)), 0, values(sic))

# project
crs(sst) <- "epsg:4326"
sst <- sst %>%
  crop(ext(-180, 180, -80, -40)) %>%
  project("epsg:6932")
crs(sic) <- "epsg:4326"
sic <- sic %>%
  crop(ext(-180, 180, -80, -40)) %>%
  project("epsg:6932")

# plot
p5 <- ggplot() +
  geom_spatraster(data = sst) + 
  geom_spatvector(data = coast, fill = "white", col = "white") +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  theme_void()
p5 + ggview::canvas(width = 8, height = 8)

p6 <- ggplot() +
  geom_spatraster(data = sic) + 
  geom_spatvector(data = coast, fill = "white", col = "white") +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  theme_void()
p6 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/5. Sea Surface Temperature.png",
       plot = p5,
       width = 8, height = 8, units = "in", dpi = 300)
ggsave("text/figures/draft/schematic_combination/6. Sea Ice Concentration.png",
       plot = p6,
       width = 8, height = 8, units = "in", dpi = 300)


#--------------------------------------------------------------------------------
# Breeding Habitat Prediction
#--------------------------------------------------------------------------------

# read in breeding habitat prediction
pred <- rast("output/climatic model/predictions/KIPE_simple_ensemble.tif")

# get core habitat threshold
threshold <- readRDS("output/climatic model/thresholds/KIPE_tss_max_threshold.RDS")

# classify raster into core and non-core habitat
pred_class <- pred >= threshold

# project core habitat
pred_class <- pred_class %>%
  crop(ext(-180.125, 179.875, -89.875, -39.875)) %>%
  project("epsg:6932")

# plot
p7 <- ggplot() +
  geom_spatraster(data = pred_class) + 
  scale_fill_gradient(guide = "none", na.value = "transparent") +
  geom_spatvector(data = coast, fill = NA, col = "white") +
  theme_void() 
p7 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/7. Breeding Habitat Prediction.png",
       plot = p7,
       width = 8, height = 8, units = "in", dpi = 300)


#--------------------------------------------------------------------------------
# Foraging Habitat Prediction
#--------------------------------------------------------------------------------

# read in foraging habitat prediction
pred2 <- rast("output/at-sea model/predictions/KIPE_chick-rearing_simple_ensemble.tif")
pred3 <- rast("output/at-sea model/predictions/KIPE_incubation_simple_ensemble.tif")
pred <- mean(pred2, pred3)

# project
pred <- pred %>%
  crop(ext(-180, 180, -80, -40)) %>%
  project("epsg:6932")

# plot
p8 <- ggplot() + 
  geom_spatraster(data = pred) +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  geom_spatvector(data = coast, fill = "white", col = "white") +
  theme_void()
p8 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/8. Foraging Habitat Prediction.png",
       plot = p8,
       width = 8, height = 8, units = "in", dpi = 300)


#--------------------------------------------------------------------------------
# Combined Output
#--------------------------------------------------------------------------------

# load in combined output
combined <- rast("output/combined/predictions/KIPE_mean_combined_suitability.tif")

# project
combined <- combined %>%
  crop(ext(-180, 180, -80, -40)) %>%
  project("epsg:6932")

# plot
p9 <- ggplot() + 
  geom_spatraster(data = combined) +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  geom_spatvector(data = coast, fill = "white", col = "white") +
  theme_void()
p9 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/9. Combined Habitat Prediction.png",
       plot = p9,
       width = 8, height = 8, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Ice free rock overlap
#-------------------------------------------------------------------------------

# read in ice-free rock
rock <- readRDS("output/topographic model/available areas/KIPE_available_areas_glo90.rds")

# limit rock to South Georgia
rock <- crop(rock, ext(-50, -30, -55, -35))
plot(rock)

# project
rock <- rock %>%
  project("epsg:6932")

# crop coast to rock extent
coast <- crop(coast, ext(rock))

# project both to epsg 4326
coast <- coast %>%
  project("epsg:4326")
rock <- rock %>%
  project("epsg:4326")

# plot
px1 <- ggplot() +
  geom_spatvector(data = coast, fill = "grey", col = "grey") +
  geom_spatvector(data = rock, fill = "darkred", col = "darkred") +
  theme_void() 
px1 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/X1. Ice Free Rock Overlap.png",
       plot = px1,
       width = 8, height = 8, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Shape-constrained additive model prediction
#-------------------------------------------------------------------------------

# species code
species <- "KIPE"

# load in available breeding habitat (ice-free rock at accessible elevation and distance to coast)
subantarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_available_areas_glo90.rds"))
antarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_available_areas_rema.rds"))

# project antarctic habitat
antarctic_habitat <- antarctic_habitat %>%
  project(crs(subantarctic_habitat))

# combine the two
available_habitat <- bind_spat_rows(subantarctic_habitat, antarctic_habitat)
rm(subantarctic_habitat, antarctic_habitat)

# read in climatic prediction
climatic_prediction <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))

# read in colony locations and background samples
colonies <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))

# convert to terra
colonies <- colonies %>%
  vect(geom = c("x", "y"), crs = crs(climatic_prediction))

# extract values to data
colonies$prediction <- terra::extract(climatic_prediction, colonies, ID = F)

# get dataframe of truth level and prediction
df <- colonies %>%
  as.data.frame() %>%
  select(pa, prediction) %>%
  mutate(pa = as.factor(pa)) %>%
  na.omit()

# get threshold using max TSS
threshold <- tidysdm::optim_thresh(df$pa, df$prediction, metric = "tss_max", event_level = "second")

# export threshold
saveRDS(threshold, paste0("output/climatic model/thresholds/", species, "_tss_max_threshold.RDS"))

# binarize climatic prediction
climatic_core_habitat <- climatic_prediction >= threshold

# convert to polygons
climatic_core_habitat <- climatic_core_habitat %>%
  as.polygons() %>%
  filter(mean == 1)

# areas that overlap with available habitat
present_habitat <- climatic_core_habitat %>%
  project(crs(available_habitat)) %>%
  terra::intersect(available_habitat)
plot(present_habitat)

# cleanup
rm(list = setdiff(ls(), c("species", "longname", "present_habitat")))

# define possible stages
if(species == "MAPE"){
  stage_options <- c("chick-rearing", "incubation", "pre-moult")
} else if(species %in% c("GEPE", "EMPE")) {
  stage_options <- "chick-rearing" 
} else {
  stage_options <- c("chick-rearing", "incubation")
}

# for each stage, read in at-sea suitability
this_stage <- "incubation"

# read in predicted ensemble suitability
hs <- rast(paste0("output/at-sea model/predictions/", species, "_", this_stage, "_simple_ensemble.tif"))
plot(hs)

# project present habitat to at-sea model crs
present_habitat <- present_habitat %>%
  project(crs(hs))

# rasterize present habitat
hab_rast <- rasterize(present_habitat, hs, touches = T)
hab_rast[is.na(hab_rast)] <- 0
hab_rast[hab_rast > 0] <- 100

# read in land file
land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sv")

# crop land to below 40 degrees south
land <- crop(land, ext(-180, 180, -90, -40))

# project land to depth raster CRS
land <- project(land, "epsg:4326")

# rasterise land
land_rast <- rasterize(land, hs, touches = T)
land_rast[is.na(land_rast)] <- 0

# add habitat and land rasters
grd <- hab_rast + land_rast

# convert land values to NA
grd[grd == 1] <- NA

# revalue habitat locations
grd[grd == 100] <- 101

# calculate distance to colonies
dist_rast <- gridDist(grd, target = 101, scale = 1000)

# read in scam model for availability
scam <- readRDS(paste0("output/at-sea model/availability/", species, "_", this_stage, "_scam_model.RDS"))

# create dataframe for prediction
testdata <- as.points(dist_rast) %>%
  as.data.frame(geom = "XY")

# rename dist2col column
names(testdata)[1] <- "dist2col"

# predict using fitted model
pred <- predict.scam(scam, testdata, type = "response")
pred <- as.vector(pred)

# add to database
testdata$pscam <- pred

# convert prediction to points
predscam <- testdata %>%
  vect(geom = c("x", "y"), crs = crs(dist_rast))

# rasterise prediction
predrast <- rasterize(predscam, dist_rast, field = "pscam")

# project
predrast <- predrast %>%
  project("epsg:6932")

# coast
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
px2 <- ggplot() +
  geom_spatraster(data = predrast) +
  scale_fill_viridis_c(guide = "none", na.value = "transparent") +
  geom_spatvector(data = coast, fill = "white", col = "white") +
  theme_void()
px2 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_combination/X2. Scam Prediction.png",
       plot = px2,
       width = 8, height = 8, units = "in", dpi = 300)
