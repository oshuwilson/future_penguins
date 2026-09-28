#--------------------------------------------------------
# Make contemporary predictions of at-sea suitability
#--------------------------------------------------------

# setup the ensemble predictions to discard rasters with CBIs under 0.4

# checklist:
# 1. newest thinned_tracks
# 2. newest key_vars
# 3. newest models
# 4. newest cbi_scores
# 5. delete predictions


rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/")

library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(tidymodels)
library(tidysdm)
library(bonsai)
library(bundle)

# create dataframe of species and stage options
meta <- expand.grid(species = c("ADPE", "CHPE", "EMPE", "GEPE", "KIPE", "MAPE"),
                    stage = c("chick-rearing", "incubation")) %>%
  as_tibble()

# remove EMPE and GEPE incubation (data limitations)
meta <- meta %>%
  filter_out(species %in% c("EMPE", "GEPE") & stage == "incubation")

# add macaroni pre-moult
meta <- meta %>%
  bind_rows(tibble(species = "MAPE", stage = "pre-moult"))

# loop over each row of meta
for(i in c(1:6, 8:11)){
  
  # define species and stage
  species <- meta$species[i]
  stage <- meta$stage[i]
  
  # print initiation
  print(paste0(species, " ", stage, " initiated"))
  
  #------------------------------------------------------------
  # Compile Raster Stack
  #------------------------------------------------------------
  
  # load in model training data
  tracks <- readRDS(paste0("penguins/output/at-sea model/thinned_tracks/", species, "_", stage, "_thinned.RDS"))
  
  # get months of the data
  month_props <- tracks %>%
    group_by(month = month(date)) %>%
    summarise(prop = n()/nrow(tracks))
  
  # remove months that account for under 2% of locations
  month_props <- month_props %>%
    filter(prop >= 0.02)
  
  # get unique months
  months <- unique(month_props$month)
  
  # load covariate names
  all_vars <- read.csv(paste0("penguins/output/at-sea model/varselection/", species, "_", stage, "_key_vars.csv")) %>%
    pull(key_vars)
  
  # separate out static and dynamic covariates
  static_vars <- all_vars[all_vars %in% c("depth", "slope", "dshelf")]
  dynamic_vars <- all_vars[!all_vars %in% static_vars]
  
  # set out target extent
  e <- ext(-180, 180, -90, -40)
  
  # for each static covariate
  for(var in static_vars){
    
    # read in raster
    var_rast <- rast(paste0("Satellite_Data/static/", var, "/", var, ".nc"))
    
    # assign crs (missing in netCDFs)
    crs(var_rast) <- "EPSG:4326"
    
    # crop to target extent
    var_rast <- crop(var_rast, e)
    
    # combine with other static rasters
    if(var == static_vars[1]) {
      static_stack <- var_rast
    } else {
      var_rast <- resample(var_rast, static_stack[[1]], method = "bilinear")
      static_stack <- c(static_stack, var_rast)
    }
  }
  
  # apply names
  names(static_stack) <- static_vars
  
  # for each dynamic covariate
  for(var in dynamic_vars){
    
    # read in monthly raster
    var_rast <- rast(paste0("Satellite_Data/monthly/", var, "/", var, ".nc"))
    
    # limit to target months
    var_rast <- var_rast[[month(time(var_rast)) %in% months]]
    
    # apply CRS
    crs(var_rast) <- "EPSG:4326"
    
    # crop to target extent
    var_rast <- crop(var_rast, e)
    
    # if SIC, revalue NAs to 0
    if(var == "sic"){
      values(var_rast) <- ifelse(is.na(values(var_rast)), 0, values(var_rast))
    }
    
    # combine with other dynamic rasters
    if(var == dynamic_vars[1]) {
      dynamic_stack <- var_rast
    } else {
      dynamic_stack <- c(dynamic_stack, var_rast)
    }
    
    # print completion
    print(var)
  }
  
  # create subarea raster to enable predictions
  subarea_rast <- rast(ext = ext(dynamic_stack), crs = "epsg:4326", res = res(dynamic_stack))
  values(subarea_rast) <- "test"
  names(subarea_rast) <- "subarea"
  
  # resample the static stack to the dynamic stack 
  static_stack <- resample(static_stack, dynamic_stack[[1]], method = "bilinear")
  
  #------------------------------------------------------------
  # Random Forests
  #------------------------------------------------------------
  
  # print initialization
  print("Predicting Random Forests")
  
  # load in random forest model
  rf <- readRDS(paste0("penguins/output/at-sea model/random forests/", species, "_", stage, "_rf_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("zos_", "ssh_", names(stack))
    names(stack) <- gsub("so_", "sal_", names(stack))
    names(stack) <- gsub("thetao_", "sst_", names(stack))
    names(stack) <- gsub("siconc_", "sic_", names(stack))
    names(stack) <- gsub("mlotst_", "mld_", names(stack))
    
    # remove everything following the first underscore from names
    names(stack) <- gsub("_.*", "", names(stack))
    
    # predict raster
    pred_raster <- predict_raster(rf, stack, type = "prob")
    
    # limit to presences only
    pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
    
    # apply timestamp to raster
    time(pred_raster) <- slice
    time(pred_raster)
    
    # combine with other predictions
    if(slice == timeslices[1]) {
      preds <- pred_raster
    } else {
      preds <- c(preds, pred_raster)
    }
    
    # print completion
    print(slice)
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # get weighting for this month
    weight <- month_props %>%
      filter(month == this_month) %>%
      pull(prop)
    
    # multiply by proportion
    month_preds <- month_preds * weight
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # sum monthly predictions to get breeding stage prediction
  rf_pred <- app(all_month_preds, sum, na.rm = TRUE)
  
  # export the random forest prediction
  writeRaster(rf_pred, 
              filename = paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_rf_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(rf, rf_pred)
  
  
  #------------------------------------------------------------
  # Boosted Regression Trees
  #------------------------------------------------------------
  
  # print initialization
  print("Predicting Boosted Regression Trees")
  
  # load in boosted regression tree model
  brt <- readRDS(paste0("penguins/output/at-sea model/boosted regression trees/", species, "_", stage, "_brt_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("zos_", "ssh_", names(stack))
    names(stack) <- gsub("so_", "sal_", names(stack))
    names(stack) <- gsub("thetao_", "sst_", names(stack))
    names(stack) <- gsub("siconc_", "sic_", names(stack))
    names(stack) <- gsub("mlotst_", "mld_", names(stack))
    
    # remove everything following the first underscore from names
    names(stack) <- gsub("_.*", "", names(stack))
    
    # predict raster
    pred_raster <- predict_raster(brt, stack, type = "prob")
    
    # limit to presences only
    pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
    
    # apply timestamp to raster
    time(pred_raster) <- slice
    time(pred_raster)
    
    # combine with other predictions
    if(slice == timeslices[1]) {
      preds <- pred_raster
    } else {
      preds <- c(preds, pred_raster)
    }
    
    # print completion
    print(slice)
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # get weighting for this month
    weight <- month_props %>%
      filter(month == this_month) %>%
      pull(prop)
    
    # multiply by proportion
    month_preds <- month_preds * weight
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # sum monthly predictions to get breeding stage prediction
  brt_pred <- app(all_month_preds, sum, na.rm = TRUE)
  
  # export the BRT prediction
  writeRaster(brt_pred, 
              filename = paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_brt_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(brt, brt_pred)
  
  
  #------------------------------------------------------------
  # Generalised Additive Models
  #------------------------------------------------------------
  
  # print initialization
  print("Predicting Generalised Additive Models")
  
  # load in GAM model
  gam <- readRDS(paste0("penguins/output/at-sea model/generalised additive models/", species, "_", stage, "_gam_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("zos_", "ssh_", names(stack))
    names(stack) <- gsub("so_", "sal_", names(stack))
    names(stack) <- gsub("thetao_", "sst_", names(stack))
    names(stack) <- gsub("siconc_", "sic_", names(stack))
    names(stack) <- gsub("mlotst_", "mld_", names(stack))
    
    # remove everything following the first underscore from names
    names(stack) <- gsub("_.*", "", names(stack))
    
    # predict raster
    pred_raster <- predict_raster(gam, stack, type = "prob")
    
    # limit to presences only
    pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
    
    # apply timestamp to raster
    time(pred_raster) <- slice
    time(pred_raster)
    
    # combine with other predictions
    if(slice == timeslices[1]) {
      preds <- pred_raster
    } else {
      preds <- c(preds, pred_raster)
    }
    
    # print completion
    print(slice)
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # get weighting for this month
    weight <- month_props %>%
      filter(month == this_month) %>%
      pull(prop)
    
    # multiply by proportion
    month_preds <- month_preds * weight
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # sum monthly predictions to get breeding stage prediction
  gam_pred <- app(all_month_preds, sum, na.rm = TRUE)
  
  # export the gam prediction
  writeRaster(gam_pred, 
              filename = paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_gam_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(gam, gam_pred)
  
  
  #------------------------------------------------------------
  # Bayesian Additive Regression Trees
  #------------------------------------------------------------
  
  # print initialization
  print("Predicting Bayesian Additive Regression Trees")
  
  # load in BART model
  bart <- readRDS(paste0("penguins/output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_bart_model.rds"))
  bart <- unbundle(bart)
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("zos_", "ssh_", names(stack))
    names(stack) <- gsub("so_", "sal_", names(stack))
    names(stack) <- gsub("thetao_", "sst_", names(stack))
    names(stack) <- gsub("siconc_", "sic_", names(stack))
    names(stack) <- gsub("mlotst_", "mld_", names(stack))
    
    # remove everything following the first underscore from names
    names(stack) <- gsub("_.*", "", names(stack))
    
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
      
      # # print progress if i is divisible by 10
      # if(i%%10 == 0){
      #   print(paste0(i, "/", num_chunks))
      # }
      
      # increment i
      i <- i + 1
    }
    
    # join predictions to blank output
    blank_output[which_vals] <- prediction_vals
    
    # create empty raster
    pred_raster <- rast(ext = ext(stack), crs = crs(stack), res = res(stack))
    
    # assign values to raster
    values(pred_raster) <- blank_output
    
    # apply timestamp to raster
    time(pred_raster) <- slice
    time(pred_raster)
    
    # combine with other predictions
    if(slice == timeslices[1]) {
      preds <- pred_raster
    } else {
      preds <- c(preds, pred_raster)
    }
    
    # print completion
    print(slice)
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # get weighting for this month
    weight <- month_props %>%
      filter(month == this_month) %>%
      pull(prop)
    
    # multiply by proportion
    month_preds <- month_preds * weight
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # sum monthly predictions to get breeding stage prediction
  bart_pred <- app(all_month_preds, sum, na.rm = TRUE)
  
  # export the bart prediction
  writeRaster(bart_pred, 
              filename = paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_bart_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(bart, bart_pred)
  
  #------------------------------------------------------------
  # Ensemble Prediction
  #------------------------------------------------------------
  
  # print initialization
  print("Predicting Ensembles")
  
  # clearout
  rm(list=setdiff(ls(), c("species", "stage", "meta")))
  
  # read in all predicted rasters
  rf <- rast(paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_rf_prediction.tif"))
  brt <- rast(paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_brt_prediction.tif"))
  gam <- rast(paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_gam_prediction.tif"))
  bart <- rast(paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_bart_prediction.tif"))
  
  # read in cbi scores
  rf_cbi <- readRDS(paste0("penguins/output/at-sea model/random forests/", species, "_", stage, "_cbi_scores.rds")) %>%
    pull(.estimate) %>%
    mean(na.rm = T)
  brt_cbi <- readRDS(paste0("penguins/output/at-sea model/boosted regression trees/", species, "_", stage, "_cbi_scores.rds")) %>%
    pull(.estimate) %>%
    mean(na.rm = T)
  gam_cbi <- readRDS(paste0("penguins/output/at-sea model/generalised additive models/", species, "_", stage, "_cbi_scores.rds")) %>%
    pull(.estimate) %>%
    mean(na.rm = T)
  bart_cbi <- readRDS(paste0("penguins/output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_cbi_scores.rds")) %>%
    pull(.estimate) %>%
    mean(na.rm = T)
  
  # create stack of accurate predictions 
  pred_list <- list()
  
  if (rf_cbi >= 0.4)   pred_list <- c(pred_list, list(rf))
  if (brt_cbi >= 0.4)  pred_list <- c(pred_list, list(brt))
  if (gam_cbi >= 0.4)  pred_list <- c(pred_list, list(gam))
  if (bart_cbi >= 0.4) pred_list <- c(pred_list, list(bart))
  
  pred_stack <- do.call(c, pred_list)
  
  # simple ensemble
  simple <- app(pred_stack, mean, na.rm = TRUE)
  
  # create ranked mean rasters
  # ranked mean rf
  rf_vals <- values(rf, mat = FALSE)
  rf_n <- sum(!is.na(rf_vals))
  rf_rank <- setValues(rf,
                       rank(rf_vals, na.last = "keep", ties.method = "average") / rf_n)
  
  # ranked mean brt
  brt_vals <- values(brt, mat = FALSE)
  brt_n <- sum(!is.na(brt_vals))
  brt_rank <- setValues(brt,
                        rank(brt_vals, na.last = "keep", ties.method = "average") / brt_n)
  
  # ranked mean gam
  gam_vals <- values(gam, mat = FALSE)
  gam_n <- sum(!is.na(gam_vals))
  gam_rank <- setValues(gam,
                        rank(gam_vals, na.last = "keep", ties.method = "average") / gam_n)
  
  # ranked mean bart
  bart_vals <- values(bart, mat = FALSE)
  bart_n <- sum(!is.na(bart_vals))
  bart_rank <- setValues(bart,
                         rank(bart_vals, na.last = "keep", ties.method = "average") / bart_n)   
  
  # create stack of accurate predictions 
  pred_list2 <- list()
  
  if (rf_cbi >= 0.4)   pred_list2 <- c(pred_list2, list(rf_rank))
  if (brt_cbi >= 0.4)  pred_list2 <- c(pred_list2, list(brt_rank))
  if (gam_cbi >= 0.4)  pred_list2 <- c(pred_list2, list(gam_rank))
  if (bart_cbi >= 0.4) pred_list2 <- c(pred_list2, list(bart_rank))
  
  pred_stack2 <- do.call(c, pred_list2)
  
  # ranked mean
  ranked <- app(pred_stack2, mean, na.rm = TRUE)
  
  # export ensemble predictions
  writeRaster(simple, 
              filename = paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_simple_ensemble.tif"),
              overwrite = TRUE)
  writeRaster(ranked,
              filename = paste0("penguins/output/at-sea model/predictions/", species, "_", stage, "_ranked_ensemble.tif"),
              overwrite = TRUE)
}