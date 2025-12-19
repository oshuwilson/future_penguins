#--------------------------------------------------------
# Make future predictions of climatic suitability
#--------------------------------------------------------

# setup the ensemble predictions to discard rasters with CBIs under 0.4
# setup to predict each GCM in parallel

rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/")

library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(tidymodels)
library(tidysdm)
library(bonsai)
library(foreach)
library(doParallel)

# define species
species <- "MAPE"

# loop over each scenario
for(scenario in c("ssp126", "ssp585")){
  
  # number of cores on HPC
  n_cores <- 78
  
  # list of all GCMs
  gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
            "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")
  
  # register parallelisation
  registerDoParallel(cores = n_cores)
  
  #loop to run through each gcm
  foreach(z = 1:8) %dopar% {
    
    # define gcm
    gcm <- gcms[z]
    
    #------------------------------------------------------------
    # Create Rasters for Future Predictions
    #------------------------------------------------------------
    
    # depending on the species, define target months
    if(species == "ADPE"){
      target_months <- c(11, 12, 1, 2)
    }
    
    if(species == "CHPE"){
      target_months <- c(12, 1, 2, 3, 4)
    }
    
    if(species == "GEPE"){ #Gentoos exhibit very different chronologies - Lescroel et al. 2009
      target_months_peninsula <- c(11, 12, 1, 2, 3)
      target_months_falklands <- c(10, 11, 12, 1, 2, 3)
      target_months_crozet <- c(8, 9, 10, 11, 12)
      target_months_marion <- c(6, 7, 8, 9, 10)
      target_months_kerguelen <- c(8, 9, 10, 11, 12, 1)
      target_months_heard <- c(11, 12, 1, 2, 3)
      target_months_macquarie <- c(10, 11, 12, 1, 2)
      target_months_south_georgia <- c(10, 11, 12, 1, 2)
    }
    
    if(species == "MAPE"){
      target_months <- c(12, 1, 2)
    }
    
    if(species == "EMPE"){
      target_months <- c(5, 6, 7, 8, 9, 10, 11, 12, 1)
    }
    
    if(species == "KIPE"){
      target_months <- c(1:12)
    }
    
    # read in temperature, precipitation, and nearest open water data
    temp <- rast(paste0("Satellite_Data_CMIP/tas/", gcm, "_", scenario, "_glorysres.tif"))
    prec <- rast(paste0("Satellite_Data_CMIP/prec/", gcm, "_", scenario, "_glorysres.tif"))
    now <- rast(paste0("Satellite_Data_CMIP/now/", gcm, "_", scenario, "_gloryres.nc"))
    
    # assign time sequences (lost in HPC upload)
    # for temp and prec this is 1993-01-01 to 2020-12-01 with all years of jan then all years of feb etc
    date_years <- 1993:2020
    date_months <- 1:12
    tas_dates <- with(expand.grid(year = date_years, month = date_months),
                      as.Date(sprintf("%d-%02d-01", year, month)))
    
    # for now this is the same but only from 2000-01-01
    now_dates <- with(expand.grid(year = 2000:2020, month = date_months),
                      as.Date(sprintf("%d-%02d-01", year, month)))
    
    # assign time
    time(temp) <- tas_dates
    time(prec) <- tas_dates
    time(now) <- now_dates
    
    # limit to target months
    temp <- temp[[month(time(temp)) %in% target_months]]
    prec <- prec[[month(time(prec)) %in% target_months]]
    now <- now[[month(time(now)) %in% target_months]]
    
    # limit temperature and precipitation to 2000-2020
    temp <- temp[[year(time(temp)) %in% 2000:2020]]
    prec <- prec[[year(time(prec)) %in% 2000:2020]]
    
    # compute average temperature for these months
    avg_temp <- mean(temp, na.rm=T)
    
    # compute average yearly minimum and maximum temperatures for these months
    for(this.year in 2000:2020){
      
      # make yearly data
      year_temp <- temp[[year(time(temp)) == this.year]]
      min_year_temp <- min(year_temp, na.rm = T)
      max_year_temp <- max(year_temp, na.rm = T)
      
      # combine together
      if(this.year == 2000){
        min_temps <- min_year_temp
        max_temps <- max_year_temp
      } else {
        min_temps <- c(min_temps, min_year_temp)
        max_temps <- c(max_temps, max_year_temp)
      }
      
    }
    
    # average of yearly data
    avg_max_temp <- mean(max_temps, na.rm = T)
    avg_min_temp <- mean(min_temps, na.rm = T)
    
    
    # compute average precipitation for these months
    avg_prec <- mean(prec, na.rm = T)
    
    # compute average yearly minimum and maximum precipitation for these months
    for(this.year in 2000:2020){
      
      # make yearly data
      year_prec <- prec[[year(time(prec)) == this.year]]
      min_year_prec <- min(year_prec, na.rm = T)
      max_year_prec <- max(year_prec, na.rm = T)
      
      # combine together
      if(this.year == 2000){
        min_precips <- min_year_prec
        max_precips <- max_year_prec
      } else {
        min_precips <- c(min_precips, min_year_prec)
        max_precips <- c(max_precips, max_year_prec)
      }
      
    }
    
    # average of yearly data
    avg_max_prec <- mean(max_precips, na.rm = T)
    avg_min_prec <- mean(min_precips, na.rm = T)
    
    
    # compute average nearest open water
    avg_now <- mean(now, na.rm = T)
    
    # compute average yearly minimum and maximum nearest open water
    for(this.year in 2000:2020){
      
      # make yearly data
      year_now <- now[[year(time(now)) == this.year]]
      min_year_now <- min(year_now, na.rm = T)
      max_year_now <- max(year_now, na.rm = T)
      
      # combine together
      if(this.year == 2000){
        min_nows <- min_year_now
        max_nows <- max_year_now
      } else {
        min_nows <- c(min_nows, min_year_now)
        max_nows <- c(max_nows, max_year_now)
      }
    }
    
    # average of yearly data
    avg_max_now <- mean(max_nows, na.rm = T)
    avg_min_now <- mean(min_nows, na.rm = T)
    
    # cleanup
    rm(list=setdiff(ls(), c("avg_now", "avg_prec", "avg_temp", "avg_min_now", "avg_min_prec", "avg_min_temp",
                            "avg_max_now", "avg_max_prec", "avg_max_temp", "species", "scenario", "gcm")))
    
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
    rf <- readRDS(paste0("penguins/output/climatic model/random forests/", species, "_rf_model.rds"))
    
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
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_rf_prediction.tif"),
                overwrite = TRUE)
    
    # cleanup
    rm(rf, predictors, temp, prec, now, stack, pred_raster, polar_pred)
    
    
    #------------------------------------------------------------
    # Boosted Regression Trees
    #------------------------------------------------------------
    
    # load in boosted regression tree model
    brt <- readRDS(paste0("penguins/output/climatic model/boosted regression trees/", species, "_brt_model.rds"))
    
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
    
    # export the prediction
    writeRaster(pred_raster, 
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_brt_prediction.tif"),
                overwrite = TRUE)
    
    # cleanup
    rm(brt, predictors, temp, prec, now, stack, pred_raster, polar_pred)
    
    
    #------------------------------------------------------------
    # MaxEnt
    #------------------------------------------------------------
    
    # load in maxent model
    maxent <- readRDS(paste0("penguins/output/climatic model/maxent/", species, "_maxent_model.rds"))
    
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
    
    # export the prediction
    writeRaster(pred_raster, 
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_maxent_prediction.tif"),
                overwrite = TRUE)
    
    # cleanup
    rm(maxent, predictors, temp, prec, now, stack, pred_raster, polar_pred)
    
    
    #------------------------------------------------------------
    # Generalised Additive Models
    #------------------------------------------------------------
    
    # load in gam model
    gam <- readRDS(paste0("penguins/output/climatic model/generalised additive models/", species, "_gam_model.rds"))
    
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
    
    # export the prediction
    writeRaster(pred_raster, 
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_gam_prediction.tif"),
                overwrite = TRUE)
    
    
    # cleanup
    rm(gam, predictors, temp, prec, now, stack, pred_raster, polar_pred)
    
    
    #------------------------------------------------------------
    # Bayesian Additive Regression Trees
    #------------------------------------------------------------
    
    # load in bart model
    bart <- readRDS(paste0("penguins/output/climatic model/bayesian additive regression trees/", species, "_bart_model.rds"))
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
    
    # export the prediction
    writeRaster(pred_raster, 
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_bart_prediction.tif"),
                overwrite = TRUE)
    
    # cleanup
    rm(bart, predictors, temp, prec, now, stack, pred_raster, polar_pred)
    
    
    #------------------------------------------------------------
    # Ensemble Prediction
    #------------------------------------------------------------
    
    # read in all predicted rasters
    rf <- rast(paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_rf_prediction.tif"))
    brt <- rast(paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_brt_prediction.tif")) 
    maxent <- rast(paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_maxent_prediction.tif"))
    gam <- rast(paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_gam_prediction.tif"))
    bart <- rast(paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_bart_prediction.tif"))
    
    # stack predictions
    pred_stack <- c(rf, brt, gam, bart)
    
    # simple ensemble
    simple <- app(pred_stack, mean, na.rm = TRUE)
    plot(simple)
    
    # read in cbi scores for weighted ensemble
    rf_cbi <- readRDS(paste0("penguins/output/climatic model/random forests/", species, "_cbi_scores.rds")) %>%
      arrange(desc(mean)) %>%
      slice(1) %>%
      pull(mean)
    
    brt_cbi <- readRDS(paste0("penguins/output/climatic model/boosted regression trees/", species, "_cbi_scores.rds")) %>%
      arrange(desc(mean)) %>%
      slice(1) %>%
      pull(mean)
    
    maxent_cbi <- readRDS(paste0("penguins/output/climatic model/maxent/", species, "_cbi_scores.rds")) %>%
      arrange(desc(mean)) %>%
      slice(1) %>%
      pull(mean)
    
    gam_cbi <- readRDS(paste0("penguins/output/climatic model/generalised additive models/", species, "_cbi_scores.rds")) %>%
      arrange(desc(mean)) %>%
      slice(1) %>%
      pull(mean)
    
    bart_cbi <- readRDS(paste0("penguins/output/climatic model/bayesian additive regression trees/", species, "_cbi_scores.rds")) %>%
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
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_simple_ensemble.tif"),
                overwrite = TRUE)
    writeRaster(weighted, 
                filename = paste0("penguins/output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_weighted_ensemble.tif"),
                overwrite = TRUE)
    
    
    # print gcm completion
    print(paste(gcm, scenario))
  }
}