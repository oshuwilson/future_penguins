#------------------------------------------------
# Fit Climatic Bayesian Additive Regression Trees
#------------------------------------------------

# 1. Configuration 

rm(list=setdiff(ls(), c("cores", "species")))

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
for(i in 1:27){ 
  
  # get predictors
  predictors <- pred_combos[i,] %>%
    pivot_longer(1:3) %>%
    pull(value) %>%
    as.character()
  
  # isolate dataset with only these predictors
  data2 <- data %>%
    select(all_of(predictors), pa, sector)
  
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
  
  # set number of folds to number of sectors
  v <- length(unique(data$sector))

  #create cross-validation folds
  folds <- group_vfold_cv(data = data2,
                          group = sector, #split training/testing data by ocean sector
                          v = v, #number of folds
                          balance = "groups" #one subarea per fold
  )

  #define formula for modelling
  rec <- recipe(pa ~ ., data = data2)  %>%
    update_role(sector, new_role = "ID") %>%
    step_downsample(pa)

  #update workflow
  bart_wf <- bart_wf %>%
    add_recipe(rec)
  
  # enable parallelisation
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
  best <- show_best(tun, metric = "tss_max") %>%
    filter(n == v)
  
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

# get the best number of trees from comparison
best_trees <- all_best %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(trees)

#set up model
best_mod <- parsnip::bart() %>%
  set_engine(engine = "dbarts") %>%
  set_mode("classification") %>%
  set_args(trees = best_trees)

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
  dplyr::select(trees, mean, std_err, predictors)

# export
saveRDS(metrics,
        paste0("output/climatic model/bayesian additive regression trees/", species, "_cbi_scores.rds"))


# 3b. Variable Importance Scores

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

# export
saveRDS(vi_scores,
        paste0("output/climatic model/bayesian additive regression trees/", species, "_varimp_scores.rds"))


# 3c. Partial Dependence Plot Data

# compute partial depndence values
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

# export PDP values
saveRDS(pdps,
        paste0("output/climatic model/bayesian additive regression trees/", species, "_pdp_values.rds"))


#---------------------------------------------
# 4. Export the model
#---------------------------------------------

# butcher and bundle to retain pointers
bart2 <- butcher::butcher(best_fit)
bart3 <- bundle::bundle(bart2)

saveRDS(bart3,
        paste0("output/climatic model/bayesian additive regression trees/", species, "_bart_model.rds"))
