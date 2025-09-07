#----------------------------------------------
# Fit Topographic Generalised Additive Models
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

# read in regression data
data <- readRDS(paste0("output/topographic model/model_data/", species, "_regression_data.rds"))

# make pb a factors
data <- data %>%
  mutate(pb = as.factor(pb))

# set presence as reference level
data <- data %>%
  mutate(pb = relevel(pb, ref = "presence")) # presence is 1, absence is 0

#------------------------------
# 2. Run Models
#------------------------------

# set number of folds to number of regions
v <- length(unique(data$subarea))

#define GAM
gam_mod <- gen_additive_mod() %>%
  set_mode("classification") %>%
  set_engine("mgcv") %>% #use mgcv package 
  set_args(select_features = F, #allow for feature selection
           adjust_deg_free = tune())

# get columns from data
cols <- names(data)[!names(data) %in% c("pb", "subarea", "rock")]

# great formula from columns
model_formula <- as.formula(paste("pb ~", paste(paste0("s(", cols, ", bs = 'ts', k = 5)"), 
                                          collapse = " + ")))

# if rock is in cols, add as a simple addition
if("rock" %in% names(data)) {
  model_formula <- update(model_formula, . ~ . + rock)
}

#create workflow
gam_wf <- workflow() %>%
  add_model(gam_mod,
            formula = model_formula)

# create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = subarea, #split training/testing data by subarea
                        v = v, #number of folds
                        balance = "groups" #the same number of regions in each fold
)

# define formula for modelling - don't downsample GAMs
rec <- recipe(pb ~ ., data = data) %>%
  update_role(subarea, new_role = "ID") 

#update workflow
gam_wf <- gam_wf %>%
  add_recipe(rec)

# degrees of freedom to tune over
adjust_deg_free <- c(0.5, 1, 1.5, 2)

# tuning grid
grid <- expand_grid(adjust_deg_free = adjust_deg_free)

# enable parallelisation
cores <- 10
plan(multisession, workers = cores)

#run models with tuning
tun <- tune_grid(gam_wf,
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
best_mod <- gen_additive_mod() %>%
  set_engine(engine = "mgcv") %>%
  set_mode("classification") %>%
  set_args(select_features = T, 
           adjust_deg_free = best$adjust_deg_free[1]) 

#update workflow
best_wf <- gam_wf %>%
  update_model(best_mod,
               formula = model_formula)

#run best model on all data
best_fit <- best_wf %>%
  fit(data)   

# get the adjust_deg_free value for the best model
best_adjust_deg_free <- best$adjust_deg_free[1]
k_val <- best_adjust_deg_free * 5
k_val <- round(k_val, 0)

# update formula to use the best k value 
model_formula2 <- as.formula(paste("pb ~", 
                                          paste(paste0("s(", cols, ", bs = 'ts', k = k_val)"), 
                                                collapse = " + ")))

# if rock is in cols, add as a simple addition
if("rock" %in% names(data)) {
  model_formula2 <- update(model_formula2, . ~ . + rock)
}

# convert pb to binary code for GAMs
data2 <- data %>%
  mutate(pb = ifelse(pb == "presence", 1, 0)) # convert to binary code

# fit GAM with mgcv for extracting supplementary info
gam1 <- mgcv::gam(model_formula2, 
                        data = data2, 
                        family = "binomial")


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
  dplyr::select(subarea, adjust_deg_free, .estimate)

# plot
ggplot(metrics, aes(x = as.factor(adjust_deg_free), y = .estimate)) +
  geom_boxplot() +
  geom_point(aes(col = subarea), size = 4, alpha = 0.4) +
  theme_bw()

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(adjust_deg_free == best$adjust_deg_free[1])

# export
saveRDS(metrics,
        paste0("output/topographic model/generalised additive models/", species, "_cbi_scores.rds"))


# 3b. Variable importance scores
library(DALEXtra)

# create predict function 
pred_function <- function(model, new_data){
  predict(model$fit, new_data = new_data, type = "prob")
}

# explain model
explainer <- explain(model = gam1, 
                     data = dplyr::select(data2, -pb, -subarea),
                     y = (data2$pb),
                     verbose = T)

# compute variable importance scores
vip_scores <- model_parts(explainer)
plot(vip_scores)

# get scores from vip_scores
vi_scores <- vip_scores %>%
  filter(!variable %in% c("_full_model_", "subarea", "_baseline_")) %>%
  mutate(dropout_loss = dropout_loss * 100)  %>%
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
        paste0("output/topographic model/generalised additive models/", species, "_varimp_scores.rds"))


# 3c. Model response curves

library(gratia)

# get smooths
sm <- smooth_estimates(gam1, n = 1000) %>%
  add_confint()

# backtransform smooths
sm <- sm %>%
  mutate(.estimate = plogis(.estimate))

# pivot longer for plotting
sm <- sm %>%
  pivot_longer(cols = c(all_of(cols)),
               names_to = "var",
               values_to = "x") %>%
  drop_na(x)

# plot predictions
p1 <- ggplot(sm, aes(x, .estimate)) + 
  geom_line(color = "darkblue", linewidth = 1.2) + 
  facet_wrap(~var, scales = "free_x", nrow = 1) + 
  ylim(0, 1) + 
  theme_bw() +
  ylab("Predicted habitat suitability") + 
  xlab("Predictor values")
p1

# format same as PDPs
sm <- sm %>%
  rename(yhat = .estimate) %>%
  select(var, x, yhat)

# export
saveRDS(sm,
        paste0("output/topographic model/generalised additive models/", species, "_smooths.rds"))
