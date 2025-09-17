#----------------------------------------------
# Fit Climatic Boosted Regression Trees
#----------------------------------------------

# select predictors with boyce index or maximum log likelihood?
# group cross validation?

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

# read in thinned data
data <- readRDS(paste0("output/climatic model/thinned/", species, " env thinned.rds")) %>%
  ungroup()

# convert presence-absence to ordered factor
data <- data %>% mutate(pa = as.factor(pa))
data$pa <- ordered(data$pa, levels = c("presence", "absence"))

# list all possible combinations of predictors (27)
temps <- c("avg_temp", "avg_min_temp", "avg_max_temp")
precips <- c("avg_prec", "avg_min_prec", "avg_max_prec")
nows <- c("avg_now", "avg_min_now", "avg_max_now")
pred_combos <- expand.grid(temp = temps, prec = precips, now = nows)


#---------------------------------------
# 2. Run Models for each Predictor Combo
#---------------------------------------

# loop over each combo
for(i in 1:3){
  
  # get predictors
  predictors <- pred_combos[i,] %>%
    pivot_longer(1:3) %>%
    pull(value) %>%
    as.character()
  
  # isolate dataset with only these predictors
  data2 <- data %>%
    select(all_of(predictors), pa)

  #define BRT
  brt_mod <- boost_tree() %>%
    set_mode("classification") %>%
    set_engine("lightgbm" #use lightgbm package
    ) %>%
    set_args(trees = tune(),
             tree_depth = tune(), 
             learn_rate = tune(), 
             min_n = 20) 
  
  #create workflow
  brt_wf <- workflow() %>%
    add_model(brt_mod)
  
  #define hyperparameter values to vary over 
  learn.rate <- c(0.005, 0.01, 0.5)
  tree.depth <- c(1, 3, 5)
  trees <- c(200, 500, 1000, 2000, 5000)
  grid <- expand_grid(learn_rate = learn.rate, tree_depth = tree.depth, trees = trees)
  
  #create cross-validation folds
  folds <- vfold_cv(data = data2, 
                    v = 10
  )
  
  #define formula for modelling
  rec <- recipe(pa ~ ., data = data2) %>%
    step_downsample(pa)
  
  #update workflow
  brt_wf <- brt_wf %>%
    add_recipe(rec)
  
  # enable parallelisation
  cores <- 10
  plan(multisession, workers = cores)
  
  #run models with tuning
  tun <- tune_grid(brt_wf,
                   resamples = folds,
                   grid = grid,
                   metrics = sdm_metric_set(),
                   control = control_grid(verbose=F)) 
  
  #get metric scores for each tuning value
  metrics <- collect_metrics(tun, summarize = F)
  
  #extract best model
  best <- show_best(tun, metric = "boyce_cont") %>%
    filter(n == 10)
  
  # append predictors
  best <- best %>%
    mutate(preds_index = i)
  
  # join to all other predictor sets
  if(i == 1){
    all_best <- best
  } else {
    all_best <- rbind(all_best, best)
  }
  
  # print completion
  print(paste0(i, " of 27 complete"))
  
}


# get the best predictor index from comparison
i <- all_best %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(preds_index)

# get predictors
predictors <- pred_combos[i,] %>%
  pivot_longer(1:3) %>%
  pull(value) %>%
  as.character()

# isolate dataset with only these predictors
data2 <- data %>%
  select(all_of(predictors), pa)

# get the best hyperparameter values from comparison
best_trees <- all_best %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(trees)

best_tree_depth <- all_best %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(tree_depth)

best_learn_rate <- all_best %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(learn_rate)

#set up model
best_mod <- boost_tree() %>%
  set_mode("classification") %>%
  set_engine("lightgbm" #use lightgbm package
  ) %>%
  set_args(trees = best_trees,
           tree_depth = best_tree_depth, 
           learn_rate = best_learn_rate, 
           min_n = 20) 

#create new workflow
best_wf <- workflow() %>%
  add_model(best_mod)

#define formula for modelling
rec <- recipe(pa ~ ., data = data2) %>%
  step_downsample(pa)

#update workflow
best_wf <- best_wf %>%
  add_recipe(rec)

#run best model on all data
best_fit <- best_wf %>%
  fit(data2)


#---------------------------------------------
# 3. Get Model Info for Supplementary Material
#---------------------------------------------

# need the variable importance scores, partial dependence plot values
# and CBI scores

# 3a. CBI scores (from best scores)
metrics <- all_best 

# create preds_index in predictor combos
pred_combos <- pred_combos %>%
  mutate(preds_index = row_number())

# collapse all other columns
pred_combos <- pred_combos %>%
  mutate(predictors = paste(temp, prec, now, sep = ", "))

# join to CBI scores
metrics <- metrics %>%
  left_join(select(pred_combos, preds_index, predictors))

# only keep relevant columns
metrics <- metrics %>%
  select(trees, tree_depth, learn_rate, mean, std_err, predictors)  

# export
saveRDS(metrics,
        paste0("output/climatic model/boosted regression trees/", species, "_cbi_scores.rds"))


# 3b. Variable Importance Scores
vi_scores <- vip::vi(best_fit)

# plot
vip::vip(best_fit)

# export
saveRDS(vi_scores,
        paste0("output/climatic model/boosted regression trees/", species, "_varimp_scores.rds"))


# 3c. Partial Dependence Plot Data
library(DALEXtra)

#get explainer
explainer <- explain_tidymodels(model = best_fit, 
                                data = dplyr::select(data2, -pa),
                                y = as.integer(data2$pa),
                                verbose = T)

#compute partial dependence
pdps <- model_profile(explainer, 
                      variables = names(data2)[!names(data2) %in% c("pa")],
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
        paste0("output/climatic model/boosted regression trees/", species, "_pdp_values.rds"))

# remove large DALEXtra objects
rm(pdps, pdp_ovr, explainer)


#---------------------------------------------
# 4. Export the model
#---------------------------------------------

saveRDS(best_fit,
        paste0("output/climatic model/boosted regression trees/", species, "_brt_model.rds"))
