#--------------------------------------------------------
# Make future predictions of at-sea suitability
#--------------------------------------------------------
# unique script to account for asynchronous breeding of gentoos

# setup the ensemble predictions to discard rasters with CBIs under 0.4

# checklist:
# 1. newest thinned_tracks
# 2. newest key_vars
# 3. newest models
# 4. newest cbi_scores
# 5. delete projections

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
library(foreach)
library(doParallel)

# number of available cores
n_cores <- 78

# define species and stage
species <- "GEPE"
stage <- "chick-rearing"

# define scenario - ssp126 or ssp585
scenario <- "ssp585"

# list of all GCMs
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# get unique months when gentoos breed
months <- c(6:12, 1:3)

# load covariate names
all_vars <- read.csv(paste0("penguins/output/at-sea model/varselection/", species, "_", stage, "_key_vars.csv")) %>%
  pull(key_vars)

# separate out static and dynamic covariates
static_vars <- all_vars[all_vars %in% c("depth", "slope", "dshelf")]
dynamic_vars <- all_vars[!all_vars %in% static_vars]

# set out target extent
e <- ext(-180, 180, -90, -40)

# register parallelisation
registerDoParallel(cores = n_cores)

#loop to run through each gcm
foreach(z = 1:8) %dopar% {
  
  # define gcm
  gcm <- gcms[z]
  
  #------------------------------------------------------------
  # Compile Raster Stack
  #------------------------------------------------------------
  
  # load in model training data
  tracks <- readRDS(paste0("penguins/output/at-sea model/thinned_tracks/", species, "_", stage, "_thinned.RDS"))
  
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
      static_stack <- c(static_stack, var_rast)
    }
  }
  
  # apply names
  names(static_stack) <- static_vars
  
  # data frame of dynamic var possibilities in GLORYS and CMIP6
  varnames <- data.frame(glorys = c("sst", "ssh", "sic", "sal", "mld", "curr"),
                         cmip = c("tos", "zos", "siconc", "sos", "mlotst", "curr"))
  
  # for each dynamic covariate
  for(var in dynamic_vars){
    
    # get cmip6 name equivalent
    cmip_var <- varnames %>% filter(glorys == var) %>% pull(cmip)
    
    # read in monthly raster
    var_rast <- rast(paste0("Satellite_Data_CMIP/", cmip_var, "/", gcm, "_", scenario, "_glorysres.tif"))
    
    # create sequence of months according to original time info
    allyears <- 2000:2020
    allmonths <- 1:12
    month_seq <- unlist(lapply(allmonths, function(m) {
      as.Date(paste(allyears, m, "01", sep = "-"))
    }))
    month_seq <- as_date(month_seq)
    
    # append to raster
    time(var_rast) <- month_seq
    
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
  subarea_rast <- rast(ext = ext(static_stack), crs = "epsg:4326", res = res(static_stack))
  values(subarea_rast) <- "test"
  names(subarea_rast) <- "subarea"
  
  
  #------------------------------------------------------------
  # Random Forests
  #------------------------------------------------------------
  
  # print initialization
  print(paste0("Predicting Random Forests for ", gcm))
  
  # load in random forest model
  rf <- readRDS(paste0("penguins/output/at-sea model/random forests/", species, "_", stage, "_rf_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique() %>% sort()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("deptho", "depth", names(stack))
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
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # create a southern range prediction
  southern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(11, 12, 1, 2, 3)]]
  southern_pred <- app(southern_stack, mean, na.rm = TRUE)
  
  # create a midrange prediction
  midrange_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(10, 11, 12, 1, 2)]]
  midrange_pred <- app(midrange_stack, mean, na.rm = TRUE)
  
  # create a kerguelen prediction
  kerguelen_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11, 12)]]
  kerguelen_pred <- app(kerguelen_stack, mean, na.rm = TRUE)
  
  # create a crozet prediction
  crozet_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11)]]
  crozet_pred <- app(crozet_stack, mean, na.rm = TRUE)
  
  # create a northern range prediction
  northern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(6, 7, 8, 9, 10)]]
  northern_pred <- app(northern_stack, mean, na.rm = TRUE)
  
  # export the random forest predictions
  writeRaster(southern_pred, 
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_southern_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(midrange_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_midrange_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(kerguelen_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_kerguelen_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(crozet_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_crozet_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(northern_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_northern_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(rf, northern_pred, crozet_pred, kerguelen_pred, midrange_pred, southern_pred)
  
  
  #------------------------------------------------------------
  # Boosted Regression Trees
  #------------------------------------------------------------
  
  # print initialization
  print(paste0("Predicting Boosted Regression Trees for ", gcm))
  
  # load in boosted regression tree model
  brt <- readRDS(paste0("penguins/output/at-sea model/boosted regression trees/", species, "_", stage, "_brt_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique() %>% sort()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("deptho", "depth", names(stack))
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
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # create a southern range prediction
  southern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(11, 12, 1, 2, 3)]]
  southern_pred <- app(southern_stack, mean, na.rm = TRUE)
  
  # create a midrange prediction
  midrange_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(10, 11, 12, 1, 2)]]
  midrange_pred <- app(midrange_stack, mean, na.rm = TRUE)
  
  # create a kerguelen prediction
  kerguelen_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11, 12)]]
  kerguelen_pred <- app(kerguelen_stack, mean, na.rm = TRUE)
  
  # create a crozet prediction
  crozet_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11)]]
  crozet_pred <- app(crozet_stack, mean, na.rm = TRUE)
  
  # create a northern range prediction
  northern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(6, 7, 8, 9, 10)]]
  northern_pred <- app(northern_stack, mean, na.rm = TRUE)
  
  # export the random forest predictions
  writeRaster(southern_pred, 
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_southern_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(midrange_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_midrange_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(kerguelen_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_kerguelen_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(crozet_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_crozet_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(northern_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_northern_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(brt, northern_pred, crozet_pred, kerguelen_pred, midrange_pred, southern_pred)
  
  
  #------------------------------------------------------------
  # MaxEnt
  #------------------------------------------------------------
  
  # print initialization
  print(paste0("Predicting MaxEnt for ", gcm))
  
  # load in maxent model
  maxent <- readRDS(paste0("penguins/output/at-sea model/maxent/", species, "_", stage, "_maxent_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique() %>% sort()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("deptho", "depth", names(stack))
    names(stack) <- gsub("zos_", "ssh_", names(stack))
    names(stack) <- gsub("so_", "sal_", names(stack))
    names(stack) <- gsub("thetao_", "sst_", names(stack))
    names(stack) <- gsub("siconc_", "sic_", names(stack))
    names(stack) <- gsub("mlotst_", "mld_", names(stack))
    
    # remove everything following the first underscore from names
    names(stack) <- gsub("_.*", "", names(stack))
    
    # predict raster
    pred_raster <- predict_raster(maxent, stack, type = "prob")
    
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
    
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # create a southern range prediction
  southern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(11, 12, 1, 2, 3)]]
  southern_pred <- app(southern_stack, mean, na.rm = TRUE)
  
  # create a midrange prediction
  midrange_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(10, 11, 12, 1, 2)]]
  midrange_pred <- app(midrange_stack, mean, na.rm = TRUE)
  
  # create a kerguelen prediction
  kerguelen_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11, 12)]]
  kerguelen_pred <- app(kerguelen_stack, mean, na.rm = TRUE)
  
  # create a crozet prediction
  crozet_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11)]]
  crozet_pred <- app(crozet_stack, mean, na.rm = TRUE)
  
  # create a northern range prediction
  northern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(6, 7, 8, 9, 10)]]
  northern_pred <- app(northern_stack, mean, na.rm = TRUE)
  
  # export the random forest predictions
  writeRaster(southern_pred, 
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_maxent_southern_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(midrange_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_maxent_midrange_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(kerguelen_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_maxent_kerguelen_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(crozet_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_maxent_crozet_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(northern_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_maxent_northern_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(maxent, northern_pred, crozet_pred, kerguelen_pred, midrange_pred, southern_pred)
  
  
  #------------------------------------------------------------
  # Generalised Additive Models
  #------------------------------------------------------------
  
  # print initialization
  print(paste0("Predicting Generalised Additive Models for ", gcm))
  
  # load in GAM model
  gam <- readRDS(paste0("penguins/output/at-sea model/generalised additive models/", species, "_", stage, "_gam_model.rds"))
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique() %>% sort()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("deptho", "depth", names(stack))
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
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # create a southern range prediction
  southern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(11, 12, 1, 2, 3)]]
  southern_pred <- app(southern_stack, mean, na.rm = TRUE)
  
  # create a midrange prediction
  midrange_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(10, 11, 12, 1, 2)]]
  midrange_pred <- app(midrange_stack, mean, na.rm = TRUE)
  
  # create a kerguelen prediction
  kerguelen_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11, 12)]]
  kerguelen_pred <- app(kerguelen_stack, mean, na.rm = TRUE)
  
  # create a crozet prediction
  crozet_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11)]]
  crozet_pred <- app(crozet_stack, mean, na.rm = TRUE)
  
  # create a northern range prediction
  northern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(6, 7, 8, 9, 10)]]
  northern_pred <- app(northern_stack, mean, na.rm = TRUE)
  
  # export the random forest predictions
  writeRaster(southern_pred, 
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_southern_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(midrange_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_midrange_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(kerguelen_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_kerguelen_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(crozet_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_crozet_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(northern_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_northern_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(gam, northern_pred, crozet_pred, kerguelen_pred, midrange_pred, southern_pred)
  
  
  #------------------------------------------------------------
  # Bayesian Additive Regression Trees
  #------------------------------------------------------------
  
  # print initialization
  print(paste0("Predicting Bayesian Additive Regression Trees for ", gcm))
  
  # load in BART model
  bart <- readRDS(paste0("penguins/output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_bart_model.rds"))
  bart <- bundle::unbundle(bart)
  
  # list each month in the dynamic stack
  timeslices <- time(dynamic_stack) %>% unique()
  timeslices <- timeslices %>% sort()
  
  # for each slice
  for(j in 1:length(timeslices)){
    slice <- timeslices[j]
    
    # limit dynamic stack to current slice
    dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
    
    # combine static, dynamic, and subarea rasters
    stack <- c(static_stack, dynamic_slice, subarea_rast)
    
    # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
    names(stack) <- gsub("deptho", "depth", names(stack))
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
    plot(pred_raster)
    
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
    print(paste0(slice, " ", gcm))
  }
  
  # for each month, average predictions
  for(this_month in months){
    
    # isolate predictions for this month
    month_preds <- preds[[month(time(preds)) == this_month]]
    
    # average predictions
    month_preds <- app(month_preds, mean, na.rm = TRUE)
    
    # assign time as 2010 for this month
    time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
    
    # join to all monthly predictions
    if(this_month == months[1]) {
      all_month_preds <- month_preds
    } else {
      all_month_preds <- c(all_month_preds, month_preds)
    }
  }
  
  # create a southern range prediction
  southern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(11, 12, 1, 2, 3)]]
  southern_pred <- app(southern_stack, mean, na.rm = TRUE)
  
  # create a midrange prediction
  midrange_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(10, 11, 12, 1, 2)]]
  midrange_pred <- app(midrange_stack, mean, na.rm = TRUE)
  
  # create a kerguelen prediction
  kerguelen_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11, 12)]]
  kerguelen_pred <- app(kerguelen_stack, mean, na.rm = TRUE)
  
  # create a crozet prediction
  crozet_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(8, 9, 10, 11)]]
  crozet_pred <- app(crozet_stack, mean, na.rm = TRUE)
  
  # create a northern range prediction
  northern_stack <- all_month_preds[[month(time(all_month_preds)) %in% c(6, 7, 8, 9, 10)]]
  northern_pred <- app(northern_stack, mean, na.rm = TRUE)
  
  # export the random forest predictions
  writeRaster(southern_pred, 
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_southern_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(midrange_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_midrange_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(kerguelen_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_kerguelen_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(crozet_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_crozet_prediction.tif"),
              overwrite = TRUE)
  
  writeRaster(northern_pred,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_northern_prediction.tif"),
              overwrite = TRUE)
  
  # cleanup
  rm(bart, northern_pred, crozet_pred, kerguelen_pred, midrange_pred, southern_pred)
  
  #------------------------------------------------------------
  # Ensemble Prediction
  #------------------------------------------------------------
  
  # clearout
  rm(list=setdiff(ls(), c("species", "stage", "gcm", "scenario")))
  
  print(paste0("Predicting Ensembles for ", gcm))
  
  # read in all predicted rasters - northern range
  rf_northern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_northern_prediction.tif"))
  brt_northern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_northern_prediction.tif"))
  gam_northern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_northern_prediction.tif"))
  bart_northern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_northern_prediction.tif"))
  
  # stack predictions
  northern_stack <- c(rf_northern, brt_northern, gam_northern, bart_northern)
  
  # simple ensemble
  northern_ensemble <- app(northern_stack, mean, na.rm = TRUE)
  
  # export ensemble
  writeRaster(northern_ensemble,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_northern_simple_ensemble.tif"),
              overwrite = TRUE)
  
  # clearout
  rm(list=setdiff(ls(), c("species", "stage", "gcm", "scenario")))
  
  # read in all predicted rasters - southern range
  rf_southern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_southern_prediction.tif"))
  brt_southern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_southern_prediction.tif"))
  gam_southern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_southern_prediction.tif"))
  bart_southern <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_southern_prediction.tif"))
  
  # stack predictions
  southern_stack <- c(rf_southern, brt_southern, gam_southern, bart_southern)
  
  # simple ensemble
  southern_ensemble <- app(southern_stack, mean, na.rm = TRUE)
  
  # export ensemble
  writeRaster(southern_ensemble,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_southern_simple_ensemble.tif"),
              overwrite = TRUE)
  
  # clearout
  rm(list=setdiff(ls(), c("species", "stage", "gcm", "scenario")))
  
  # read in all predicted rasters - midrange
  rf_midrange <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_midrange_prediction.tif"))
  brt_midrange <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_midrange_prediction.tif"))
  gam_midrange <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_midrange_prediction.tif"))
  bart_midrange <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_midrange_prediction.tif"))
  
  # stack predictions
  midrange_stack <- c(rf_midrange, brt_midrange, gam_midrange, bart_midrange)
  
  # simple ensemble
  midrange_ensemble <- app(midrange_stack, mean, na.rm = TRUE)
  
  # export ensemble
  writeRaster(midrange_ensemble,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_midrange_simple_ensemble.tif"),
              overwrite = TRUE)
  
  # clearout
  rm(list=setdiff(ls(), c("species", "stage", "gcm", "scenario")))
  
  # read in all predicted rasters - kerguelen
  rf_kerguelen <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_kerguelen_prediction.tif"))
  brt_kerguelen <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_kerguelen_prediction.tif"))
  gam_kerguelen <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_kerguelen_prediction.tif"))
  bart_kerguelen <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_kerguelen_prediction.tif"))
  
  # stack predictions
  kerguelen_stack <- c(rf_kerguelen, brt_kerguelen, gam_kerguelen, bart_kerguelen)
  
  # simple ensemble
  kerguelen_ensemble <- app(kerguelen_stack, mean, na.rm = TRUE)
  
  # export ensemble
  writeRaster(kerguelen_ensemble,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_kerguelen_simple_ensemble.tif"),
              overwrite = TRUE)
  
  # clearout
  rm(list=setdiff(ls(), c("species", "stage", "gcm", "scenario")))
  
  # read in all predicted rasters - crozet
  rf_crozet <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_rf_crozet_prediction.tif"))
  brt_crozet <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_brt_crozet_prediction.tif"))
  gam_crozet <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_gam_crozet_prediction.tif"))
  bart_crozet <- rast(paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_bart_crozet_prediction.tif"))
  
  # stack predictions
  crozet_stack <- c(rf_crozet, brt_crozet, gam_crozet, bart_crozet)
  
  # simple ensemble
  crozet_ensemble <- app(crozet_stack, mean, na.rm = TRUE)
  
  # export ensemble
  writeRaster(crozet_ensemble,
              filename = paste0("penguins/output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", stage, "_", gcm, "_", scenario, "_crozet_simple_ensemble.tif"),
              overwrite = TRUE)
  
}