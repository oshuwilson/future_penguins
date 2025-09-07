#-------------------------------
# Fit Topographic Maxent Models
#-------------------------------

# include more feature classes and regularisation multipliers

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

# remove NAs from any columns
data <- na.omit(data)

# make rock and pb factors
data <- data %>%
  mutate(rock = as.factor(rock),
         pb = as.factor(pb))

# set presence as reference level
data <- data %>%
  mutate(pb = relevel(pb, ref = "presence")) # presence is 1, absence is 0


#------------------------------
# 2. Run Models
#------------------------------

# set number of folds to number of regions
v <- length(unique(data$subarea))

#define Maxent
max_mod <- maxent() %>%
  set_mode("classification") %>%
  set_engine("maxnet") %>% #use maxnet package 
  set_args(feature_classes = tune(), #tune feature classes
           regularization_multiplier = tune()) #tune regularization multiplier

#create workflow
max_wf <- workflow() %>%
  add_model(max_mod)

# define regularization multiplier values to vary over (Morales 2017)
#regularization_multiplier <- c(0.5, 1, 2) # for quicker test runs
regularization_multiplier <- c(1, 2, 5, 10, 15, 20)

# define feature_classes to tune over (all combinations of lqpht up to 2 classes)
#feature_classes <- c("lq", "lp", "lh", "qp", "qh", "ph") # for quicker test runs
feature_classes <- c("lc", "qc", "tc", "hc", "lqc", "hqc", "lqpc", "lqtc", "hqpc", "hqtc", "lqhptc", "hqptc")

#create tuning grid
grid <- expand_grid(regularization_multiplier = regularization_multiplier,
                    feature_classes = feature_classes)

# create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = subarea, #split training/testing data by subarea
                        v = v, #number of folds
                        balance = "groups" #the same number of regions in each fold
)

# define formula for modelling - don't downsample MaxEnt following Santini et al. (2021)
rec <- recipe(pb ~ ., data = data) %>%
  update_role(subarea, new_role = "ID") %>%
  step_downsample(pb)

#update workflow
max_wf <- max_wf %>%
  add_recipe(rec)

# enable parallelisation
cores <- 10
plan(multisession, workers = cores)

#run models with tuning
tun <- tune_grid(max_wf,
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
best_mod <- maxent() %>%
  set_engine(engine = "maxnet") %>%
  set_mode("classification") %>%
  set_args(regularization_multiplier = best$regularization_multiplier[1],
           feature_classes = best$feature_classes[1])

#update workflow
best_wf <- max_wf %>%
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
  dplyr::select(subarea, feature_classes, regularization_multiplier, .estimate)

# plot
ggplot(metrics, aes(x = as.factor(feature_classes), y = .estimate)) +
  geom_boxplot() +
  geom_point(aes(col = subarea), size = 4, alpha = 0.4) +
  theme_bw()

ggplot(metrics, aes(x = as.factor(regularization_multiplier), y = .estimate)) +
  geom_boxplot() +
  geom_point(aes(col = subarea), size = 4, alpha = 0.4) +
  theme_bw()

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(feature_classes == best$feature_classes[1],
         regularization_multiplier == best$regularization_multiplier[1])

# export
saveRDS(metrics,
        paste0("output/topographic model/maxent/", species, "_cbi_scores.rds"))


# 3b. Variable Importance Scores
library(DALEXtra)
explainer <- explain_tidymodels(model = best_fit, 
                                data = dplyr::select(data, -pb),
                                y = (as.numeric(data$pb) - 2) * -1,
                                verbose = T)

# compute variable importance scores
vip_scores <- model_parts(explainer = explainer)

# get scores from vip_scores
vi_scores <- vip_scores %>%
  filter(!variable %in% c("_full_model_", "subarea", "_baseline_")) %>%
  mutate(dropout_loss = (1 - dropout_loss) * 100)  %>%
  select(variable, dropout_loss)

# calculate mean per variable
vi_scores <- vi_scores %>%
  group_by(variable) %>%
  summarise(dropout_loss = mean(dropout_loss, na.rm = T)) %>%
  ungroup() %>%
  arrange(desc(dropout_loss))

# match names with formatting of vip::vi output
vi_scores <- vi_scores %>%
  rename(Variable = variable,
         Importance = dropout_loss)

# plot
ggplot(vi_scores, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance") +
  theme_bw()

# export
saveRDS(vi_scores,
        paste0("output/topographic model/maxent/", species, "_varimp_scores.rds"))


# 3c. Partial Dependence Plot Data

#compute partial dependence
pdps <- model_profile(explainer, 
                      variables = names(data)[!names(data) %in% c("pb", "subarea")],
                      N = 500)

#extract pdp predictive values
pdp_ovr <- as_tibble(pdps$agr_profiles) %>%
  rename(x = `_x_`, yhat = `_yhat_`, var = `_vname_`) %>%
  dplyr::select(var, x, yhat) %>%
  mutate(yhat = 1 - yhat)

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
        paste0("output/topographic model/maxent/", species, "_pdp_values.rds"))

# remove large DALEXtra objects
rm(pdps, pdp_ovr, explainer)
