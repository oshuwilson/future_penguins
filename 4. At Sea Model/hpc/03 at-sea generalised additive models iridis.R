#----------------------------------------------
# Fit Oceanographic Generalised Additive Models
#----------------------------------------------

# 1. Configuration

rm(list=setdiff(ls(), c("cores", "species", "stage")))

# read in data
data <- readRDS(paste0("output/at-sea model/model_data/", species, "_", stage, "_data.rds"))

# make pb a factors
data <- data %>%
  mutate(pb = as.factor(pb))

# set presence as reference level
data <- data %>%
  mutate(pb = relevel(pb, ref = "presence")) # presence is 1, absence is 0


#------------------------------
# 2. Run Models
#------------------------------

# set number of folds to number of subareas
v <- length(unique(data$subarea))

#define GAM
gam_mod <- gen_additive_mod() %>%
  set_mode("classification") %>%
  set_engine("mgcv") %>% #use mgcv package 
  set_args(select_features = F, 
           adjust_deg_free = tune())

# get columns from data
cols <- names(data)[!names(data) %in% c("pb", "subarea", "rock")]

# create formula from columns
model_formula <- as.formula(paste("pb ~", paste(paste0("s(", cols, ", bs = 'ts', k = 5)"), 
                                                collapse = " + ")))

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

#define formula for modelling
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
  filter(n == max(n))

#set up model
best_mod <- gen_additive_mod() %>%
  set_engine(engine = "mgcv") %>%
  set_mode("classification") %>%
  set_args(select_features = F, 
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

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(adjust_deg_free == best$adjust_deg_free[1])

# export
saveRDS(metrics,
        paste0("output/at-sea model/generalised additive models/", species, "_", stage, "_cbi_scores.rds"))


# 3b. Variable importance scores
library(DALEXtra)

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
  dplyr::select(variable, dropout_loss)

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
        paste0("output/at-sea model/generalised additive models/", species, "_", stage, "_varimp_scores.rds"))


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

# format same as PDPs
sm <- sm %>%
  rename(yhat = .estimate) %>%
  dplyr::select(var, x, yhat)

# export
saveRDS(sm,
        paste0("output/at-sea model/generalised additive models/", species, "_", stage, "_pdp_values.rds"))

#---------------------------------------------
# 4. Export the model
#---------------------------------------------

saveRDS(best_fit, 
        paste0("output/at-sea model/generalised additive models/", species, "_", stage, "_gam_model.rds"))
