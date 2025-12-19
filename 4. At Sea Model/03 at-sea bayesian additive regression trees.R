#-------------------------------------------------------
# Fit Oceanographic Bayesian Additive Regression Trees
#-------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

{
  library(terra)
  library(tidyterra)
  library(tidyverse)
  library(tidymodels)
  library(themis)
  library(bundle)
  library(butcher)
  library(tidysdm)
  library(future)
  library(miceRanger)
}

# define species and stage
species <- "CHPE"
stage <- "chick-rearing"

# read in data
data <- readRDS(paste0("output/at-sea model/model_data/", species, "_", stage, "_data.rds"))

# convert pb to ordered factor
data <- data %>% mutate(pb = as.factor(pb))
data$pb <- ordered(data$pb, levels = c("presence", "background"))


# 2. Create BARTs

#check for NAs and impute
if(sum(is.na(data)) > 0){
  mice <- miceRanger(data, m=1)
  data <- completeData(mice)[[1]]
}

# convert subarea to numeric
data$subarea <- as.numeric(as.factor(data$subarea))

# set number of folds to number of subareas
v <- length(unique(data$subarea))

#define BART
bart_mod <- parsnip::bart() %>%
  set_mode("classification") %>%
  set_engine("dbarts") %>%
  set_args(trees = tune()) #tune trees

#create workflow
bart_wf <- workflow() %>%
  add_model(bart_mod)

#define tree values to vary over 
trees <- c(50, 100, 200, 300)
grid <- expand_grid(trees = trees)

# create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = subarea, #split training/testing data by subarea
                        v = v, #number of folds
                        balance = "groups" #the same number of regions in each fold
)

#define formula for modelling
rec <- recipe(pb ~ ., data = data) %>%
  update_role(subarea, new_role = "ID") %>%
  step_downsample(pb)

#update workflow
bart_wf <- bart_wf %>%
  add_recipe(rec)

# enable parallelisation
cores <- 10
plan(multisession, workers = cores)

#run models with tuning
tun <- tune_grid(bart_wf,
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
best_mod <- parsnip::bart() %>%
  set_engine(engine = "dbarts") %>%
  set_mode("classification") %>%
  set_args(trees = best$trees[1])

#update workflow
best_wf <- bart_wf %>%
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
  dplyr::select(subarea, trees, .estimate)

# get corresponding subarea to original names
data2 <- readRDS(paste0("output/at-sea model/model_data/", species, "_", stage, "_data.rds"))
data2 <- data2 %>% rename(subarea_name = subarea)
data2$subarea <- as.numeric(as.factor(data2$subarea_name))
subareas <- data2 %>% group_by(subarea) %>%
  summarise(subarea_name = unique(subarea_name)) 

# append these names
metrics <- metrics %>%
  left_join(subareas) %>%
  dplyr::select(-subarea) %>%
  rename(subarea = subarea_name)

# plot
ggplot(metrics, aes(x = as.factor(trees), y = .estimate)) +
  geom_boxplot() +
  geom_point(aes(col = subarea), size = 4, alpha = 0.4) +
  theme_bw()

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(trees == best$trees[1])

# export
saveRDS(metrics, 
        paste0("output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_cbi_scores.rds"))


# 3b. Variable Importance Scores
library(dbarts)

# extract the underlying dbarts model
bart1 <- extract_fit_parsnip(best_fit)$fit

# get variable usage counts from posterior
var_counts <- bart1$varcount

# get mean for each variable
mean_vi <- colMeans(var_counts)

# create a data frame with variable names and their importance scores
vi_scores <- data.frame(Variable = names(mean_vi), 
                        Importance = mean_vi)

# subtract the minimum VI score
vi_scores$Importance <- vi_scores$Importance - min(vi_scores$Importance) + 5

# plot
ggplot(vi_scores, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance") +
  theme_bw()

# export
saveRDS(vi_scores, 
        paste0("output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_varimp_scores.rds"))


# 3c. Partial Dependence Plot Data

# compute partial dependence values
part <- pdbart(bart1, pl = F)

# define initial max and min val as 0
max_val <- 0
min_val <- 0

# for each variable
for(i in 1:length(part$xlbs)){
  
  # get the variable name
  var <- part$xlbs[i]
  
  # get the levels of this variable where points are logged
  levs <- part$levs[[i]]
  
  # get the partial dependence values
  vals <- part$fd[[i]]
  
  # get the mean partial dependence value for each level
  mean_vals <- colMeans(vals)
  
  # get the max and min values for this variable
  max_val_i <- max(vals)
  min_val_i <- min(vals)
  
  # if this is the biggest max val or smallest min val, update
  if(max_val_i > max_val){
    max_val <- max_val_i
  }
  if(min_val_i < min_val){
    min_val <- min_val_i
  }
  
  # join into a data frame
  pdp <- data.frame(var = var, 
                    x = levs, 
                    yhat = mean_vals)
  
  # join to all vars
  if(i == 1){
    pdps <- pdp
  } else {
    pdps <- rbind(pdps, pdp)
  }
}

# scale yhat by the min and max values
pdps <- pdps %>%
  mutate(yhat = (yhat - min_val) / (max_val - min_val)) %>%
  mutate(yhat = 1 - yhat)

# plot
p2 <- ggplot(pdps, aes(x, yhat)) + 
  geom_line(color = "darkblue", linewidth = 1.2) + 
  facet_wrap(~var, scales = "free_x", nrow = 1) + 
  ylim(0, 1) + 
  theme_bw() +
  ylab("Predicted habitat suitability") + 
  xlab("Predictor values")
p2

# export PDP values
saveRDS(pdps, 
        paste0("output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_pdp_values.rds"))

#---------------------------------------------
# 4. Export the model
#---------------------------------------------

# butcher and bundle to retain pointers
bart2 <- butcher::butcher(best_fit)
bart3 <- bundle::bundle(bart2)

# save
saveRDS(bart3, 
        paste0("output/at-sea model/bayesian additive regression trees/", species, "_", stage, "_bart_model.rds"))
