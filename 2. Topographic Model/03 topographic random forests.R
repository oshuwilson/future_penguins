#----------------------------------------------
# Fit Topographic Random Forests
#----------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

{
  library(terra)
  library(tidyterra)
  library(tidyverse)
  library(tidymodels)
  library(themis)
  library(tidysdm)
  library(future)
  library(miceRanger)
  library(bonsai)
}

# 1. Configuration 

# set seed
set.seed(777)

# define species
species <- "ADPE"

# read in machine learning data
data <- readRDS(paste0("output/topographic model/model_data/", species, "_ml_data.rds"))

#------------------------------
# 2. Run Models
#------------------------------

# set number of folds to number of subareas
v <- length(unique(data$subarea))

# define RF
rf_mod <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger", #use ranger package
             importance = "impurity" #gini index for importance
  ) %>%
  set_args(trees = 1000, #1000 trees
           mtry = tune(), #tune mtry
           min_n = 1) #minimum number of samples in a node

# create workflow
rf_wf <- workflow() %>%
  add_model(rf_mod)

# define hyperparameter values to vary over 
mtry <- c(1, 2, 3)
grid <- expand_grid(mtry = mtry)

# create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = subarea, #split training/testing data by subarea
                        v = v, #number of folds
                        balance = "groups" #the same number of subareas in each fold (1)
)

# define formula for modelling
rec <- recipe(pb ~ ., data = data) %>%
  update_role(subarea, new_role = "ID") %>%
  step_downsample(pb)

# update workflow
rf_wf <- rf_wf %>%
  add_recipe(rec)

# enable parallelisation
cores <- 10
plan(multisession, workers = cores)

# run models with tuning
tun <- tune_grid(rf_wf,
                 resamples = folds,
                 grid = grid,
                 metrics = sdm_metric_set(),
                 control = control_grid(verbose=F)) 

# get metric scores for each tuning value
metrics <- collect_metrics(tun, summarize = F)

# extract best model
best <- show_best(tun, metric = "boyce_cont") %>%
  filter(n == v)

# set up model
best_mod <- rand_forest() %>%
  set_engine(engine = "ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  set_args(trees = 1000, mtry = best$mtry[1], min_n = 1)

# update workflow
best_wf <- rf_wf %>%
  update_model(best_mod)

# run best model on all data
best_fit <- best_wf %>%
  fit(data)     


#---------------------------------------------
# 3. Get Model Info for Supplementary Material
#---------------------------------------------

# need the variable importance scores, partial dependence plot values
# and CBI scores

# 3a. CBI scores (from metrics)
metrics <- metrics %>%
  filter(.metric == "boyce_cont")

# identify which subarea was being tested in each resample 
for(i in 1:nrow(folds)){
  
  # get fold
  this_fold <- assessment(folds$splits[[i]])
  
  # extract subarea
  this_subarea <- unique(this_fold$subarea)
  
  # extract resample id
  this_resample <- folds$id[i]
  
  # create df
  df <- data.frame(id = this_resample, 
                   subarea = this_subarea)
  
  # join to other resamples
  if(i == 1){
    resample_subareas <- df
  } else {
    resample_subareas <- rbind(resample_subareas, df)
  }
}

# append subarea info to metrics
metrics <- metrics %>% left_join(resample_subareas)

# only keep relevant columns
metrics <- metrics %>%
  dplyr::select(subarea, mtry, .estimate)

# plot
ggplot(metrics, aes(x = as.factor(mtry), y = .estimate)) +
  geom_boxplot() +
  geom_point(aes(col = subarea), size = 4, alpha = 0.4) +
  theme_bw()

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(mtry == best$mtry[1])
  
# export
saveRDS(metrics, 
        paste0("output/topographic model/random forests/", species, "_cbi_scores.rds"))


# 3b. Variable Importance Scores
vi_scores <- vip::vi(best_fit)

# plot
vip::vip(best_fit)

# export
saveRDS(vi_scores, 
        paste0("output/topographic model/random forests/", species, "_varimp_scores.rds"))


# 3c. Partial Dependence Plot Data
library(DALEXtra)

#get explainer
explainer <- explain_tidymodels(model = best_fit, 
                                   data = dplyr::select(data, -pb),
                                   y = as.integer(data$pb),
                                   verbose = T)

#compute partial dependence
pdps <- model_profile(explainer, 
                      variables = names(data)[!names(data) %in% c("pb", "subarea")],
                      N = 500)

#extract pdp predictive values
pdp_ovr <- as_tibble(pdps$agr_profiles) %>%
  rename(x = `_x_`, yhat = `_yhat_`, var = `_vname_`) %>%
  dplyr::select(var, x, yhat) 

# plot PDPs
p1 <- ggplot(pdp_ovr, aes(x, yhat)) + 
  geom_line(color = "darkblue", linewidth = 1.2) + 
  facet_wrap(~var, scales = "free_x", nrow = 1) + 
  ylim(0, 1) + 
  theme_bw() +
  ylab("Predicted habitat suitability") + 
  xlab("Predictor values")
p1

# export PDP values
saveRDS(pdp_ovr, 
        paste0("output/topographic model/random forests/", species, "_pdp_values.rds"))

# remove large DALEXtra objects
rm(pdps, pdp_ovr, explainer)

#---------------------------------------------
# 4. Create Raster Predictions for Present Day
#---------------------------------------------

# 4a. For GLO-90 DEMs

# define subarea DEM equivalents - ADD TO THIS LIST
dem_names <- data.frame(
  subarea = c("Subarea 48.2", "Subarea 48.4"),
  dem = c("south_orkney_glo90", "south_sandwich_glo90"))

# limit dems to those in data
dem_names <- dem_names %>% 
  filter(subarea %in% unique(data$subarea)) %>%
  pull(dem) %>%
  unique()

# for each subarea
for(this.dem in dem_names){
  
  # read in DEM
  dem <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/output_hh.nc"))
  dist2coast <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/dist_to_coast.nc"))
  slope <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/slope.nc"))
  rugosity <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rugosity.nc"))
  rock <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/rock.nc"))
  wave_exposure <- rast(paste0("E:/Satellite_Data/static/DEM/GLO90_100m/", this.dem, "/wave_exposure_", species, ".nc"))
  
  # make test subarea raster
  subarea <- rast(e = ext(dem), crs = crs(dem), res = res(dem))
  subarea[] <- "test"
  
  # stack predictors
  stack <- c(dem, dist2coast, slope, rugosity, rock, wave_exposure, subarea)
  
  # set names
  names(stack) <- c("elevation", "dist2coast", "slope", "rugosity", "rock", "wave_exposure", "subarea")
  
  # only keep predictors in data
  stack <- stack[[names(stack) %in% names(data)[!names(data) %in% c("pb")]]]
  
  # make a mask of dist2coast < 2x max_dist of colonies
  max_dist <- data %>% filter(pb == "presence") %>%
    pull(dist2coast) %>%
    max()
  
  m1 <- matrix(c(-Inf, 0, NA,
                 0, max_dist * 2, 1, 
                 max_dist * 2, Inf, NA), 
               ncol = 3, byrow = TRUE)
  distmask <- classify(dist2coast, m1)
  plot(distmask)
  
  mask <- as.polygons(distmask)
  plot(mask, col = "red")
  
  # mask stack
  stack <- mask(stack, mask)
  
  # predict raster
  pred_raster <- predict_raster(best_fit, stack, type = "prob")
  
  # limit to presences only
  pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
  plot(pred_raster)
  
  # plot colonies
  cols <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds")) %>%
    vect(geom = c("x", "y"), crs = "epsg:4326")
  plot(cols, add = T, col = "red", alpha = 0.5)
  
  # save raster
  writeRaster(pred_raster,
              paste0("output/topographic model/raster predictions/", species, "_", this.dem, ".tif"),
              overwrite = T)
  
  # print completion
  print(paste0("Completed ", this.dem))
  
}


# 4b. For REMA

# read in DEM
dem <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_elevation_cropped.tif")
dist2coast <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_dist2coast_cropped.tif")
slope <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_slope_cropped.tif")
rugosity <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rugosity_cropped.tif")
rock <- rast("E:/Satellite_Data/static/DEM/REMA_100m_cropped/REMA_100m_rock_cropped.tif")
wave_exposure <- rast(paste0("E:/Satellite_Data/static/DEM/REMA_100m_cropped/wave_exposure_", species, "_cropped.tif"))

# make test subarea raster
m1 <- matrix(c(-Inf, Inf, 1000),
             ncol = 3, byrow = T)
subarea <- classify(dem, m1)

# stack predictors
stack <- c(dem, dist2coast, slope, rugosity, rock, wave_exposure, subarea)

# set names
names(stack) <- c("elevation", "dist2coast", "slope", "rugosity", "rock", "wave_exposure", "subarea")

# only keep predictors in data
stack <- stack[[names(stack) %in% names(data)[!names(data) %in% c("pb")]]]

# remove individual predictors
rm(dem, slope, rugosity, rock, wave_exposure, subarea)

# make a mask of dist2coast < 2x max_dist of colonies
m1 <- matrix(c(-Inf, 0, NA,
               0, max_dist * 2, 1, 
               max_dist * 2, Inf, NA), 
             ncol = 3, byrow = TRUE)
distmask <- classify(dist2coast, m1)
plot(distmask)

mask <- as.polygons(distmask)
plot(mask, col = "red")

# mask stack
stack <- mask(stack, mask)

# remove everything unnecessary to free up memory
rm(list = setdiff(ls(), c("stack", "best_fit", "species")))

# free unused R memory
gc()

# create tiles for predictions
tile_template <- rast(ext = ext(stack), crs = crs(stack), res = 500000)
tiles <- getTileExtents(stack, tile_template)

# predict each tile and store predictions in the list
for(j in 1:nrow(tiles)){
  
  tile <- tiles[j,]
  
  # get tile extent
  tile_e <- ext(tile[1], tile[2], tile[3], tile[4])
  
  # predict in chunks over raster
  # get all values of raster
  vals <- terra::as.matrix(stack %>% crop(tile_e))
  
  # create empty output vector
  blank_output <- as.numeric(rep(NA, nrow(vals)))
  
  # Get indices of non-NA values in the input matrix
  which_vals <- which(complete.cases(vals))
  
  # Remove NA values from the input matrix
  input_matrix <- vals[complete.cases(vals), , drop = FALSE] %>%
    as.data.frame()
  
  # convert subarea to character
  input_matrix$subarea <- as.character(input_matrix$subarea)
  
  # chunk over input matrix
  total_length <- nrow(input_matrix)
  
  # set chunk size 
  chunk_size <- 100000
  
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
    pred_chunk <- predict(best_fit, input_chunk, type = "prob") %>%
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
  if(num_chunks > 0){
    blank_output[which_vals] <- prediction_vals
  }
  
  # create empty raster
  pred_raster <- rast(ext = tile_e, crs = crs(stack), res = res(stack))
  
  # assign values to raster
  values(pred_raster) <- blank_output
  
  # blank list for inputting rasters
  if(j == 1){
    total_preds <- list()
  }
  
  # store prediction in list
  total_preds[[j]] <- pred_raster
  
  # print tile completion
  print(paste0("tile ", j, "/", nrow(tiles)))
  
  # free unused R memory
  gc()
}

# combine all tiles into one raster
pred_raster <- do.call(terra::merge, total_preds)
