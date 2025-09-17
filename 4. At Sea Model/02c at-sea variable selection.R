#----------------------------------------------------------------
# Variable Selection using Shallow BARTs and Collinearity Testing
#----------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

{
  library(terra)
  library(tidyterra)
  library(tidyverse)
  library(tidymodels)
  library(tidysdm)
  library(future)
  library(miceRanger)
  library(vip)
}

# candidate variables
candidate_vars <- c("depth", "slope", "sst", "sal", 
                    "sic", "curr", "mld")

# define species and stage
species <- "CHPE"
stage <- "chick-rearing"

# read in extracted data
data <- readRDS(paste0("output/at-sea model/extraction/", species, " ", stage, " extracted subsampled.RDS"))

#-------------------------------------------------------
# Run collinearity testing
#-------------------------------------------------------

# test for collinearity using variance inflation factors 
library(car)
library(embarcadero)

# fit regression model
regdata <- data %>%
  mutate(pb1 = ifelse(pb == "presence", 1, 0)) %>%
  dplyr::select(pb1, all_of(candidate_vars))
m1 <- lm(pb1 ~ ., data = regdata)

# calculate variance inflation factor
original_scores <- vif(m1)
scores <- original_scores
scores

# identify variable with highest VIF (above 5 only), remove and rerun
high_scores <- scores[scores > 5] 
high_scores

# if all under 5, then we are good to go
if(max(scores) < 5){
  regression_vars <- colnames(regdata)[colnames(regdata) != "pb1"] 
}

# if any scores above 5 exist, iteratively remove them and rerun the test
while(max(scores) > 5){
  
  # identify the covariate with the greatest collinearity
  highest <- names(high_scores)[which.max(high_scores)]
  
  # run model without highest correlation
  regdata <- regdata %>% dplyr::select(-all_of(highest))
  m2 <- lm(pb1 ~ ., data = regdata)
  scores <- vif(m2)
  scores
  
  # are any scores above 5
  high_scores <- scores[scores > 5]
  
  # if all under 5, then we are good to go
  if(max(scores) < 5){
    regression_vars <- colnames(regdata)[colnames(regdata) != "pb1"] 
  }
  
}

# balance presence and absence data
n_p <- nrow(regdata %>% filter(pb1 == 1))
regdata <- regdata %>%
  group_by(pb1) %>%
  slice_sample(n = n_p) %>%
  ungroup()

# rerun variable.step with the regression_vars
xdata <- regdata %>% dplyr::select(all_of(regression_vars))
ydata <- regdata %>% rename(pb = pb1) %>% pull(pb)

# run varimp.diag
p1 <- varimp.diag(xdata, ydata, iter = 30)
p1

# get data from plot
p1data <- p1 %>% pluck("data")

# calculate range of importance for each variable and isolate scores at 10 and 20
p1data %>%
  group_by(variable) %>%
  summarise(diff = max(imp) - min(imp))
p1data %>%
  pivot_wider(names_from = trees, values_from = imp) %>%
  rename(ten = `10`, twenty = `20`) %>%
  dplyr::select(variable, ten, twenty)

# define key vars based on p1 and differences
# criteria: is the VarImp below 0.05 for any number of trees?
# criteria: does the VarImp decline when using 10 AND 20 trees?
# criteria: does the range of VarImp scores for that variable exceed 0.1 (if declining with fewer trees)?
key_reg_vars <- c("depth", "sst", "slope", "sic", "sal", "mld")

# set regression dataframe to use key_vars only
regdata <- data %>%
  dplyr::select(pb, subarea, all_of(key_reg_vars))

# # run variable.step - to select the top variables 
# key_reg_vars <- variable.step(xdata, ydata, iter = 50)
# 
# 
# # get RMSE plot
# p3 <- last_plot()
# 
# # get data from plot
# p3data <- p3 %>% pluck("data")

# save regression dataframe
saveRDS(regdata,
        paste0("output/at-sea model/model_data/", species, "_", stage, "_data.rds"))

# save the plots
# ggsave(paste0("output/at-sea model/varselection/", species, "_", stage, "_RMSE_regression.png"),
#        p3, width = 10, height = 10)
ggsave(paste0("output/at-sea model/varselection/", species, "_", stage, "_BART_varimp.png"),
       p1, width = 10, height = 10)

# save varimp data
write_csv(p1data, 
          paste0("output/at-sea model/varselection/", species, "_", stage, "_BART_varimp.csv"))

# # save RMSE data
# write_csv(p3data, 
#           paste0("output/at-sea model/varselection/", species, "_", stage, "_RMSE_regression.csv"))

# save key variable list
key_reg_var_df <- data.frame(key_vars = key_reg_vars, species_name = species, stage_name = stage)
write_csv(key_reg_var_df, 
          paste0("output/at-sea model/varselection/", species, "_", stage, "_key_vars.csv"))

# save original and final VIF scores
original_scores <- data.frame(covariate = names(original_scores),
                              vif = original_scores, 
                              row.names = 1:length(original_scores))
final_scores <- data.frame(covariate = names(scores),
                           vif = scores, 
                           row.names = 1:length(scores))

write_csv(original_scores,
          paste0("output/at-sea model/varselection/", species, "_", stage, "_original_vif.csv"))
write_csv(final_scores,
          paste0("output/at-sea model/varselection/", species, "_", stage, "_final_vif.csv"))
