#-------------------------------------------------------------------------------
# Bring Together Present-Day Predictions
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)
library(scam)

# define species
species <- "KIPE"
longname <- "King Penguin"

#-------------------------------------------------------------------------------
# 1. Read in accessible habitat
#-------------------------------------------------------------------------------

# load in available breeding habitat (ice-free rock at accessible elevation and distance to coast)
subantarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_available_areas_glo90.rds"))
antarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_available_areas_rema.rds"))

# project antarctic habitat
antarctic_habitat <- antarctic_habitat %>%
  project(crs(subantarctic_habitat))

# combine the two
available_habitat <- bind_spat_rows(subantarctic_habitat, antarctic_habitat)
rm(subantarctic_habitat, antarctic_habitat)


#-------------------------------------------------------------------------------
# 2. Combine with climatic core habitat
#-------------------------------------------------------------------------------

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


#-------------------------------------------------------------------------------
# 3. Integrate Foraging Habitat
#-------------------------------------------------------------------------------

# define possible stages
if(species == "MAPE"){
  stage_options <- c("chick-rearing", "incubation", "pre-moult")
} else if(species %in% c("GEPE", "EMPE")) {
  stage_options <- "chick-rearing" 
} else {
  stage_options <- c("chick-rearing", "incubation")
}

# for each stage, read in at-sea suitability
for(this_stage in stage_options){
  
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
  
  # multiply predicted suitability by availability
  hs_av <- hs * predrast
  
  # export suitability raster
  writeRaster(hs_av, paste0("output/combined/predictions/", species, "_", this_stage, "_combined_suitability.tif"),
              overwrite = TRUE)
  
  # view around the South Atlantic
  plot(hs_av %>% crop(ext(-90, -30, -70, -50)))
  
  # view around the South Indian Ocean
  plot(hs_av %>% crop(ext(20, 100, -60, -40)))
  
  # view around Macquarie
  plot(hs_av %>% crop(ext(150, 170, -60, -50)))
  
  # project 
  # hs_av_proj <- project(hs_av, "epsg:6932")
  # plot(hs_av_proj)
  
  # stack with other stages
  if(this_stage == stage_options[1]){
    combined_stack <- hs_av
  } else {
    combined_stack <- c(combined_stack, hs_av)
  }
}

# average stages
mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)
plot(mean_hs_av)

# export
writeRaster(mean_hs_av, paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"),
            overwrite = TRUE)

# list all extraction subsampled files for this species
files <- list.files("output/at-sea model/extraction/", pattern = paste0(species, " "), full.names = TRUE)
files <- files[grep("subsampled", files)]

# read in data
for(file in files){
  this_data <- readRDS(file)
  if(file == files[1]){
    data <- this_data
  } else {
    data <- bind_rows(data, this_data)
  }
}

# convert to terra
data <- data %>%
  vect(geom = c("x", "y"), crs = "epsg:4326")

# extract values to data
data$prediction <- terra::extract(mean_hs_av, data, ID = F)

# get dataframe of truth level and prediction
df <- data %>%
  as.data.frame() %>%
  select(pb, prediction) %>%
  mutate(pb = as.factor(pb)) %>%
  na.omit()

# get threshold using sensitivity (explore sensitivity values until no present day overprediction)
if(species == "KIPE"){
  sens_val <- 0.6
}
if(species == "CHPE"){
  sens_val <- 0.7
}
if(species == "GEPE"){
  sens_val <- 0.8
}
if(species == "MAPE"){
  sens_val <- 0.7
}
if(species == "ADPE"){
  sens_val <- 0.7
}
threshold <- tidysdm::optim_thresh(df$pb, df$prediction, metric = c("sensitivity", sens_val), event_level = "second")

# export threshold
saveRDS(threshold, paste0("output/combined/thresholds/", species, "_threshold.RDS"))

# classify raster using threshold
mat1 <- matrix(c(0, threshold, 10,
                 threshold, 1, 20),
               ncol = 3, byrow = T)
bins <- classify(mean_hs_av, mat1)
plot(bins)

# export current suitability bins
writeRaster(bins, paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"),
            overwrite = TRUE)

# read in present day suitability bins for plotting
bins <- rast(paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"))

# read in present day suitability for plotting
mean_hs_av <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))

# vectorise bins for plot
bins_vect <- bins %>%
  as.polygons() %>%
  filter(mean == 20)

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
mean_hs_av <- mean_hs_av %>% project("epsg:6932")
bins_vect <- bins_vect %>% project("epsg:6932")
p1 <- ggplot() +
  geom_spatraster(data = mean_hs_av) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  geom_spatvector(data = bins_vect, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", guide = "none") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
p1 + ggview::canvas(width = 8, height = 8)

# export
ggsave(paste0("output/imagery/combined suitability/", species, "_suitability.png"),
       plot = p1,
       width = 8, height = 8, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Alternative version for emperors using dist2coast and climatic suitability 
#-------------------------------------------------------------------------------
# emperors do not breed on land but on sea ice so cannot use same approach for 
# constraining the at-sea predictions

# cleanup
rm(list=ls())

# define species
species <- "EMPE"
longname <- "Emperor Penguin"

# read in climatic prediction
climatic_prediction <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))

# read in colony locations and background samples
colonies <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))

# convert to terra
colonies <- colonies %>%
  vect(geom = c("x", "y"), crs = crs(climatic_prediction))

# plot over each other to visualise
plot(climatic_prediction)
plot(colonies %>% filter(pa == "presence"), add = T)

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

# binarize climatic prediction and plot to check
climatic_core_habitat <- climatic_prediction >= threshold
plot(climatic_core_habitat)
plot(colonies %>% filter(pa == "presence"), add = T, col ="red")

# convert to polygons
climatic_core_habitat <- climatic_core_habitat %>%
  as.polygons() %>%
  filter(mean == 1)

# get max dist2coast of any colonies
colony_dist2coast <- colonies %>%
  filter(pa == "presence") %>%
  pull(dist2coast) %>%
  max()

# read in dist2coast file
dist2coast <- rast("E:/Satellite_Data/static/dist2coast_emp.tif")

# make values beyond max dist2coast NA
dist2coast_masked <- dist2coast
dist2coast_masked[dist2coast_masked > colony_dist2coast] <- NA
plot(dist2coast_masked)

# convert to polygons
dist2coast_habitat <- dist2coast_masked %>%
  as.polygons() 

# overlap of climatic suitable habitat and dist2coast habitat
present_habitat <- climatic_core_habitat %>%
  project(crs(dist2coast_habitat)) %>%
  terra::intersect(dist2coast_habitat)
plot(present_habitat)

# ALTERNATIVE METHOD USING KNOWN COLONY LOCATIONS
# # only retain colony locations for constraining at-sea predictions
# present_habitat <- colonies %>%
#   filter(pa == "presence")

# define possible stages
if(species == "MAPE"){
  stage_options <- c("chick-rearing", "incubation", "pre-moult")
} else if(species %in% c("GEPE", "EMPE")) {
  stage_options <- "chick-rearing" 
} else {
  stage_options <- c("chick-rearing", "incubation")
}

# for each stage, read in at-sea suitability
for(this_stage in stage_options){
  
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
  
  # multiply predicted suitability by availability
  hs_av <- hs * predrast
  
  # export suitability raster
  writeRaster(hs_av, paste0("output/combined/predictions/", species, "_", this_stage, "_combined_suitability.tif"),
              overwrite = TRUE)
  
  # view around the South Atlantic
  plot(hs_av %>% crop(ext(-90, -30, -70, -50)))
  
  # view around the South Indian Ocean
  plot(hs_av %>% crop(ext(20, 100, -60, -40)))
  
  # view around Macquarie
  plot(hs_av %>% crop(ext(150, 170, -60, -50)))
  
  # project 
  # hs_av_proj <- project(hs_av, "epsg:6932")
  # plot(hs_av_proj)
  
  # stack with other stages
  if(this_stage == stage_options[1]){
    combined_stack <- hs_av
  } else {
    combined_stack <- c(combined_stack, hs_av)
  }
}

# average stages
mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)
plot(mean_hs_av)

# export
writeRaster(mean_hs_av, paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"),
            overwrite = TRUE)


# list all extraction subsampled files for this species
files <- list.files("output/at-sea model/extraction/", pattern = paste0(species, " "), full.names = TRUE)
files <- files[grep("subsampled", files)]

# read in data
for(file in files){
  this_data <- readRDS(file)
  if(file == files[1]){
    data <- this_data
  } else {
    data <- bind_rows(data, this_data)
  }
}

# convert to terra
data <- data %>%
  vect(geom = c("x", "y"), crs = "epsg:4326")

# extract values to data
data$prediction <- terra::extract(mean_hs_av, data, ID = F)

# get dataframe of truth level and prediction
df <- data %>%
  as.data.frame() %>%
  select(pb, prediction) %>%
  mutate(pb = as.factor(pb)) %>%
  na.omit()

# get threshold using sensitivity (explore sensitivity values until no present day overprediction)
if(species == "EMPE"){
  sens_val <- 0.83
}
threshold <- tidysdm::optim_thresh(df$pb, df$prediction, metric = c("sensitivity", sens_val), event_level = "second")

# export threshold
saveRDS(threshold, paste0("output/combined/thresholds/", species, "_threshold.RDS"))

# classify raster using threshold
mat1 <- matrix(c(0, threshold, 10,
                 threshold, 1, 20),
               ncol = 3, byrow = T)
bins <- classify(mean_hs_av, mat1)
plot(bins)

# export current suitability bins
writeRaster(bins, paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"),
            overwrite = TRUE)

# vectorise bins for plot
bins_vect <- bins %>%
  as.polygons() %>%
  filter(mean == 20)

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p1 <- ggplot() +
  geom_spatraster(data = mean_hs_av %>% project("epsg:6932")) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  geom_spatvector(data = bins_vect %>% project("epsg:6932"), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", guide = "none") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
p1 + ggview::canvas(width = 8, height = 8)

# export
ggsave(paste0("output/imagery/combined suitability/", species, "_suitability.png"),
       plot = p1,
       width = 8, height = 8, units = "in", dpi = 300)

