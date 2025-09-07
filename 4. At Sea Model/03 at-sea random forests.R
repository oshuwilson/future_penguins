#----------------------------------------------
# Fit Global Random Forests
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

# define species and stage
species <- "ADPE"
stage <- "chick-rearing"

# read in data
data <- readRDS(paste0("output/at-sea model/model_data/", species, "_", stage, "_data.rds"))

# convert pb to ordered factor
data <- data %>% mutate(pb = as.factor(pb))
data$pb <- ordered(data$pb, levels = c("presence", "background"))


#------------------------------
# 2. Run Models
#------------------------------

# set number of folds to number of subareas
v <- length(unique(data$subarea))

#check for NAs and impute
if(sum(is.na(data)) > 0){
  mice <- miceRanger(data, m=1)
  data <- completeData(mice)[[1]]
}

#define RF
rf_mod <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger", #use ranger package
             importance = "impurity" #gini index for importance
  ) %>%
  set_args(trees = 1000, #1000 trees
           mtry = tune(), #tune mtry
           min_n = 1) #minimum number of samples in a node

#create workflow
rf_wf <- workflow() %>%
  add_model(rf_mod)

#define hyperparameter values to vary over 
mtry <- c(1, 2, 3)
grid <- expand_grid(mtry = mtry)

#create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = subarea, #split training/testing data by individual ID
                        v = v, #number of folds
                        balance = "groups" #one subarea per fold
)

#define formula for modelling
rec <- recipe(pb~ ., data = data) %>%
  update_role(subarea, new_role = "ID") %>%
  step_downsample(pb)

#update workflow
rf_wf <- rf_wf %>%
  add_recipe(rec)

# enable parallelisation
cores <- 10
plan(multisession, workers = cores)

#run models with tuning
tun <- tune_grid(rf_wf,
                 resamples = folds,
                 grid = grid,
                 metrics = sdm_metric_set(),
                 control = control_grid(verbose=F)) 

#get metric scores for each tuning value
metrics <- collect_metrics(tun, summarize = F)

#extract best model
best <- show_best(tun, metric = "boyce_cont") %>%
  filter(n == v)

#set up model
best_mod <- rand_forest() %>%
  set_engine(engine = "ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  set_args(trees = 1000, mtry = best$mtry[1], min_n = 1)

#update workflow
best_wf <- rf_wf %>%
  update_model(best_mod)

#run best model on all data
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
        paste0("output/at-sea model/random forests/", species, "_", stage, "_cbi_scores.rds"))


# 3b. Variable Importance Scores
vi_scores <- vip::vi(best_fit)

# plot
vip::vip(best_fit)

# export
saveRDS(vi_scores, 
        paste0("output/at-sea model/random forests/", species, "_", stage, "_varimp_scores.rds"))


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
  dplyr::select(var, x, yhat) %>%
  mutate(yhat = 1-yhat)

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
        paste0("output/at-sea model/random forests/", species, "_", stage, "_pdp_values.rds"))

# remove large DALEXtra objects
rm(pdps, pdp_ovr, explainer)


#---------------------------------------------
# 4. Export the model
#---------------------------------------------

saveRDS(best_fit, 
        paste0("output/at-sea model/random forests/", species, "_", stage, "_rf_model.rds"))
