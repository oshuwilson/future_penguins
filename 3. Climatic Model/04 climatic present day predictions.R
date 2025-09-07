#--------------------------------------------------------
# Make contemporary predictions of climatic suitability
#--------------------------------------------------------

# setup the ensemble predictions to discard rasters with CBIs under 0.4


rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(tidymodels)
library(tidysdm)
library(bonsai)

# define species
species <- "ADPE"


#------------------------------------------------------------
# Organise Rasters
#------------------------------------------------------------

# read in rasters
# temp
avg_temp <- rast(paste0("output/climatic model/rasters/temp/", species, "_avg_temp.tif"))
avg_min_temp <- rast(paste0("output/climatic model/rasters/temp/", species, "_avg_min_temp.tif"))
avg_max_temp <- rast(paste0("output/climatic model/rasters/temp/", species, "_avg_max_temp.tif"))

# precipitation
avg_prec <- rast(paste0("output/climatic model/rasters/prec/", species, "_avg_prec.tif"))
avg_min_prec <- rast(paste0("output/climatic model/rasters/prec/", species, "_avg_min_prec.tif"))
avg_max_prec <- rast(paste0("output/climatic model/rasters/prec/", species, "_avg_max_prec.tif"))

# open water
avg_now <- rast(paste0("output/climatic model/rasters/now/", species, "_avg_now.tif"))
avg_min_now <- rast(paste0("output/climatic model/rasters/now/", species, "_avg_min_now.tif"))
avg_max_now <- rast(paste0("output/climatic model/rasters/now/", species, "_avg_max_now.tif"))

# project open water (DO THIS EARLIER IN PROCESS)
avg_now <- project(avg_now, crs(avg_temp))
avg_min_now <- project(avg_min_now, crs(avg_temp))
avg_max_now <- project(avg_max_now, crs(avg_temp))

# resample open water (DO THIS EARLIER IN PROCESS)
avg_now <- resample(avg_now, avg_temp, method = "bilinear")
avg_min_now <- resample(avg_min_now, avg_temp, method = "bilinear")
avg_max_now <- resample(avg_max_now, avg_temp, method = "bilinear")

# make NAs in NOW equal 0
values(avg_now)[is.na(values(avg_now))] <- 0
values(avg_min_now)[is.na(values(avg_min_now))] <- 0
values(avg_max_now)[is.na(values(avg_max_now))] <- 0

# crop to 80 degrees south and above
e <- ext(-180.125, 179.875, -80, -29.875)
avg_temp <- crop(avg_temp, e)
avg_min_temp <- crop(avg_min_temp, e)
avg_max_temp <- crop(avg_max_temp, e)
avg_prec <- crop(avg_prec, e)
avg_min_prec <- crop(avg_min_prec, e)
avg_max_prec <- crop(avg_max_prec, e)
avg_now <- crop(avg_now, e)
avg_min_now <- crop(avg_min_now, e)
avg_max_now <- crop(avg_max_now, e)


#------------------------------------------------------------
# Random Forests
#------------------------------------------------------------

# load in random forest model
rf <- readRDS(paste0("output/climatic model/random forests/", species, "_rf_model.RDS"))

# get predictor names for this model
predictors <- rf$pre$actions$recipe$recipe$var_info$variable
predictors <- predictors[predictors != "pa"]

# choose these predictors
temp <- get(predictors[1])
prec <- get(predictors[2])
now <- get(predictors[3])

# stack predictors
stack <- c(temp, prec, now)

# rename to names of predictors
names(stack) <- predictors

# predict raster
pred_raster <- predict_raster(rf, stack, type = "prob")

# limit to presences only
pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
plot(pred_raster)

# plot in polar view
polar_pred <- project(pred_raster, "epsg:6932")
plot(polar_pred)

# export the random forest prediction
writeRaster(pred_raster, 
            filename = paste0("output/climatic model/predictions/", species, "_rf_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(rf, predictors, temp, prec, now, stack, pred_raster, polar_pred)


#------------------------------------------------------------
# Boosted Regression Trees
#------------------------------------------------------------

# load in boosted regression tree model
brt <- readRDS(paste0("output/climatic model/boosted regression trees/", species, "_brt_model.RDS"))

# get predictor names for this model
predictors <- brt$pre$actions$recipe$recipe$var_info$variable
predictors <- predictors[predictors != "pa"]

# choose these predictors
temp <- get(predictors[1])
prec <- get(predictors[2])
now <- get(predictors[3])

# stack predictors
stack <- c(temp, prec, now)

# rename to names of predictors
names(stack) <- predictors

# predict raster
pred_raster <- predict_raster(brt, stack, type = "prob")

# limit to presences only
pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
plot(pred_raster)

# plot in polar view
polar_pred <- project(pred_raster, "epsg:6932")
plot(polar_pred)

# export the boosted regression tree prediction
writeRaster(pred_raster, 
            filename = paste0("output/climatic model/predictions/", species, "_brt_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(brt, predictors, temp, prec, now, stack, pred_raster, polar_pred)


#------------------------------------------------------------
# MaxEnt
#------------------------------------------------------------

# load in maxent model
maxent <- readRDS(paste0("output/climatic model/maxent/", species, "_maxent_model.RDS"))

# get predictor names for this model
predictors <- maxent$pre$actions$recipe$recipe$var_info$variable
predictors <- predictors[predictors != "pa"]

# choose these predictors
temp <- get(predictors[1])
prec <- get(predictors[2])
now <- get(predictors[3])

# stack predictors
stack <- c(temp, prec, now)

# rename to names of predictors
names(stack) <- predictors

# predict raster
pred_raster <- predict_raster(maxent, stack, type = "prob")

# limit to presences only
pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
plot(pred_raster)

# plot in polar view
polar_pred <- project(pred_raster, "epsg:6932")
plot(polar_pred)

# export the maxent prediction
writeRaster(pred_raster, 
            filename = paste0("output/climatic model/predictions/", species, "_maxent_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(maxent, predictors, temp, prec, now, stack, pred_raster, polar_pred)


#------------------------------------------------------------
# Generalised Additive Models
#------------------------------------------------------------

# load in gam model
gam <- readRDS(paste0("output/climatic model/generalised additive models/", species, "_gam_model.RDS"))

# get predictor names for this model
predictors <- gam$pre$actions$recipe$recipe$var_info$variable
predictors <- predictors[predictors != "pa"]

# choose these predictors
temp <- get(predictors[1])
prec <- get(predictors[2])
now <- get(predictors[3])

# stack predictors
stack <- c(temp, prec, now)

# rename to names of predictors
names(stack) <- predictors

# predict raster
pred_raster <- predict_raster(gam, stack, type = "prob")

# limit to presences only
pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
plot(pred_raster)

# plot in polar view
polar_pred <- project(pred_raster, "epsg:6932")
plot(polar_pred)

# export the gam prediction
writeRaster(pred_raster, 
            filename = paste0("output/climatic model/predictions/", species, "_gam_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(gam, predictors, temp, prec, now, stack, pred_raster, polar_pred)


#------------------------------------------------------------
# Bayesian Additive Regression Trees
#------------------------------------------------------------

# load in bart model
bart <- readRDS(paste0("output/climatic model/bayesian additive regression trees/", species, "_bart_model.RDS"))
bart <- bundle::unbundle(bart)

# get predictor names for this model
predictors <- bart$pre$actions$recipe$recipe$var_info$variable
predictors <- predictors[predictors != "pa"]

# choose these predictors
temp <- get(predictors[1])
prec <- get(predictors[2])
now <- get(predictors[3])

# stack predictors
stack <- c(temp, prec, now)

# rename to names of predictors
names(stack) <- predictors

# get all values of raster
vals <- terra::as.matrix(stack)

# create empty output vector
blank_output <- as.numeric(rep(NA, nrow(vals)))

# Get indices of non-NA values in the input matrix
which_vals <- which(complete.cases(vals))

# Remove NA values from the input matrix
input_matrix <- vals[complete.cases(vals), , drop = FALSE]

# chunk over input matrix
total_length <- nrow(input_matrix)

# set chunk size 
chunk_size <- 10000

# define number of chunks
num_chunks <- ceiling(total_length / chunk_size)

# for each chunk
i <- 1

# subset matrix to this number of chunks
while(i <= num_chunks){
  # get start and end of chunk
  start <- (i - 1) * chunk_size + 1
  end <- min(i * chunk_size, total_length)
  
  # if last chunk, change end to end value of matrix
  if(i == num_chunks){
    end <- total_length
  }
  
  # get chunk
  input_chunk <- input_matrix[start:end, ]
  
  # make predictions
  pred_chunk <- predict(bart, input_chunk, type = "prob") %>%
    pull(.pred_presence)
  
  # merge with other data
  if(i == 1){
    prediction_vals <- pred_chunk
  } else{
    prediction_vals <- c(prediction_vals, pred_chunk)
  }
  
  # print progress
  print(paste0(i, "/", num_chunks))
  
  # increment i
  i <- i + 1
}

# join predictions to blank output
blank_output[which_vals] <- prediction_vals

# create empty raster
pred_raster <- rast(ext = ext(stack), crs = crs(stack), res = res(stack))

# assign values to raster
values(pred_raster) <- blank_output
plot(pred_raster)

# plot in polar view
polar_pred <- project(pred_raster, "epsg:6932")
plot(polar_pred)

# export the bart prediction
writeRaster(pred_raster, 
            filename = paste0("output/climatic model/predictions/", species, "_bart_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(bart, predictors, temp, prec, now, stack, pred_raster, polar_pred)


#------------------------------------------------------------
# Ensemble Prediction
#------------------------------------------------------------

# read in all predicted rasters
rf <- rast(paste0("output/climatic model/predictions/", species, "_rf_prediction.tif"))
brt <- rast(paste0("output/climatic model/predictions/", species, "_brt_prediction.tif"))
maxent <- rast(paste0("output/climatic model/predictions/", species, "_maxent_prediction.tif"))
gam <- rast(paste0("output/climatic model/predictions/", species, "_gam_prediction.tif"))
bart <- rast(paste0("output/climatic model/predictions/", species, "_bart_prediction.tif"))

# stack predictions
pred_stack <- c(rf, brt, maxent, gam, bart)

# simple ensemble
simple <- app(pred_stack, mean, na.rm = TRUE)
plot(simple)

# read in cbi scores for weighted ensemble
rf_cbi <- readRDS(paste0("output/climatic model/random forests/", species, "_cbi_scores.rds")) %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(mean)

brt_cbi <- readRDS(paste0("output/climatic model/boosted regression trees/", species, "_cbi_scores.rds")) %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(mean)

maxent_cbi <- readRDS(paste0("output/climatic model/maxent/", species, "_cbi_scores.rds")) %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(mean)

gam_cbi <- readRDS(paste0("output/climatic model/generalised additive models/", species, "_cbi_scores.rds")) %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(mean)

bart_cbi <- readRDS(paste0("output/climatic model/bayesian additive regression trees/", species, "_cbi_scores.rds")) %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(mean)

# multiply predictions by relevant cbi scores
rf <- rf * rf_cbi
brt <- brt * brt_cbi
maxent <- maxent * maxent_cbi
gam <- gam * gam_cbi
bart <- bart * bart_cbi

# stack predictions
pred_stack2 <- c(rf, brt, maxent, gam, bart)

# weighted ensemble
weighted <- app(pred_stack2, mean, na.rm = TRUE)
plot(weighted)

# plot ensembles in polar view
polar_simple <- project(simple, "epsg:6932")
plot(polar_simple)
polar_weighted <- project(weighted, "epsg:6932")
plot(polar_weighted)

# export the ensemble predictions
writeRaster(simple, 
            filename = paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"),
            overwrite = TRUE)
writeRaster(weighted, 
            filename = paste0("output/climatic model/predictions/", species, "_weighted_ensemble.tif"),
            overwrite = TRUE)
