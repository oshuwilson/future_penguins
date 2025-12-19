#----------------------------------------------
# Fit Climatic Generalised Additive Models
#----------------------------------------------

# 1. Configuration 

rm(list=setdiff(ls(), c("cores", "species")))

# read in thinned data
data <- readRDS(paste0("output/climatic model/thinned/", species, " env thinned.rds")) %>%
  ungroup()

# convert presence-absence to ordered factor
data <- data %>% mutate(pa = as.factor(pa))

# set presence as reference level
data <- data %>%
  mutate(pa = relevel(pa, ref = "presence")) # presence is 1, absence is 0

# list all possible combinations of predictors (27)
temps <- c("avg_temp", "avg_min_temp", "avg_max_temp")
precips <- c("avg_prec", "avg_min_prec", "avg_max_prec")
nows <- c("avg_now", "avg_min_now", "avg_max_now")
pred_combos <- expand.grid(temp = temps, prec = precips, now = nows,
                           sip = "sip", fast_ice = "fast_ice", dist2coast = "dist2coast")


#---------------------------------------
# 2. Run Models for each Predictor Combo
#---------------------------------------

# loop over each combo
for(i in 1:27){
 
  # get predictors
  predictors <- pred_combos[i,] %>%
    pivot_longer(1:6) %>%
    pull(value) %>%
    as.character()
  

  # isolate dataset with only these predictors
  data2 <- data %>%
    select(all_of(predictors), pa, sector)
  
  #define GAM
  gam_mod <- gen_additive_mod() %>%
    set_mode("classification") %>%
    set_engine("mgcv") %>% #use mgcv package 
    set_args(select_features = F, 
             adjust_deg_free = tune())
  
  # get columns from data
  cols <- names(data2)[!names(data2) %in% c("pa", "sector")]
  
  # create formula from columns
  model_formula <- as.formula(paste("pa ~", paste(paste0("s(", cols, ", bs = 'ts', k = 5)"), 
                                                  collapse = " + ")))
  
  #create workflow
  gam_wf <- workflow() %>%
    add_model(gam_mod,
              formula = model_formula)  

  # degrees of freedom to tune over
  adjust_deg_free <- c(0.5, 1, 1.5, 2)
  
  # tuning grid
  grid <- expand_grid(adjust_deg_free = adjust_deg_free)  
  
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
    update_role(sector, new_role = "ID")
    
  #update workflow
  gam_wf <- gam_wf %>%
    add_recipe(rec) 

  # enable parallelisation
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
  pivot_longer(1:6) %>%
  pull(value) %>%
  as.character()

# isolate dataset with only these predictors
data2 <- data %>%
  select(all_of(predictors), pa)

# get the best adjust_deg_free value from comparison
best_adf <- all_best %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(adjust_deg_free)

#set up model
best_mod <- gen_additive_mod() %>%
  set_engine(engine = "mgcv") %>%
  set_mode("classification") %>%
  set_args(select_features = F, 
           adjust_deg_free = best_adf) 

# create formula from predictors
model_formula <- as.formula(paste("pa ~", paste(paste0("s(", predictors, ", bs = 'ts', k = 5)"), 
                                                collapse = " + ")))

#create new workflow
best_wf <- workflow() %>%
  add_model(best_mod,
            formula = model_formula)

#define formula for modelling
rec <- recipe(pa ~ ., data = data2)

#update workflow
best_wf <- best_wf %>%
  add_recipe(rec)

#run best model on all data
best_fit <- best_wf %>%
  fit(data2)

# convert pb to binary code for GAMs
data3 <- data2 %>%
  mutate(pa = ifelse(pa == "presence", 1, 0)) # convert to binary code

# get the adjust_deg_free value for the best model
k_val <- best_adf * 5
k_val <- round(k_val, 0)

# update formula to use the best k value 
model_formula2 <- as.formula(paste("pa ~", 
                                   paste(paste0("s(", predictors, ", bs = 'ts', k = k_val)"), 
                                         collapse = " + ")))

# fit GAM with mgcv for extracting supplementary info
gam1 <- mgcv::gam(model_formula2, 
                  data = data3, 
                  family = "binomial")


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
  mutate(predictors = paste(temp, prec, now, sip, fast_ice, dist2coast, sep = ", "))

# join to CBI scores
metrics <- metrics %>%
  left_join(select(pred_combos, preds_index, predictors))

# only keep relevant columns
metrics <- metrics %>%
  dplyr::select(adjust_deg_free, mean, std_err, predictors)

# export
saveRDS(metrics,
        paste0("output/climatic model/generalised additive models/", species, "_cbi_scores.rds"))


# 3b. Variable Importance Scores

# explain model
explainer <- explain(model = gam1, 
                     data = dplyr::select(data3, -pa),
                     y = (data3$pa),
                     verbose = T)

# compute variable importance scores
vip_scores <- model_parts(explainer)

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

# export
saveRDS(vi_scores,
        paste0("output/climatic model/generalised additive models/", species, "_varimp_scores.rds"))


# 3c. Partial Dependence Plot Data

# get smooths
sm <- smooth_estimates(gam1, n = 1000) %>%
  add_confint()

# backtransform smooths
sm <- sm %>%
  mutate(.estimate = plogis(.estimate))

# pivot longer for plotting
sm <- sm %>%
  pivot_longer(cols = c(all_of(predictors)),
               names_to = "var",
               values_to = "x") %>%
  drop_na(x)

# format same as PDPs
sm <- sm %>%
  rename(yhat = .estimate) %>%
  select(var, x, yhat)

# export
saveRDS(sm,
        paste0("output/climatic model/generalised additive models/", species, "_pdp_values.rds"))

# remove large DALEXtra objects
rm(explainer)


#---------------------------------------------
# 4. Export the model
#---------------------------------------------

saveRDS(best_fit,
        paste0("output/climatic model/generalised additive models/", species, "_gam_model.rds"))
