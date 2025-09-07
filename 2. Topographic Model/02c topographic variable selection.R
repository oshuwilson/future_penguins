#------------------------------------------
# Variable Selection using Shallow BARTs
#------------------------------------------

# retain variables with importance above 0.05 in shallow trees regardless of RMSE

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
candidate_vars <- c("elevation", "dist2coast", "slope", 
                    "rugosity", "wave_exposure", "rock")

# define species and stage
species <- "ADPE"

# read in extracted colony data 
colonies <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_glo90.rds"))
colonies2 <- readRDS(paste0("output/topographic model/extractions/", species, "_colonies_extracted_rema.rds"))
colonies2$rock[is.na(colonies2$rock)] <- 0

# combine the dataframes 
colonies <- bind_rows(colonies, colonies2)
rm(colonies2)

# read in extracted background data 
background <- readRDS(paste0("output/topographic model/extractions/", species, "_background_glo90.rds")) %>%
  select(-dist_to_coast, -region)
background2 <- readRDS(paste0("output/topographic model/extractions/", species, "_background_rema.rds")) %>%
  select(-dem)

# combine the dataframes
background <- bind_rows(background, background2)
rm(background2)

# combine the two dataframes
colonies <- colonies %>% mutate(pb = "presence") %>%
  select(pb, subarea, all_of(candidate_vars))
background <- background %>% mutate(pb = "background") %>%
  select(pb, subarea, all_of(candidate_vars))
data <- rbind(colonies, background)

# convert rock to a categorical variable
data <- data %>%
  mutate(rock = as.factor(rock))


#-----------------------------------------------------
# Find important variables for machine learning models
#-----------------------------------------------------

# package for variable importance
library(embarcadero)

# set up data for embarcadero
xdata <- data %>% dplyr::select(all_of(candidate_vars)) %>%
  as.data.frame()
ydata <- data %>% mutate(pb = ifelse(pb == "presence", 1, 0)) %>% pull(pb)

# run varimp.diag - INCREASE NUMBER OF ITERATIONS
# p1 <- varimp.diag(xdata, ydata, iter = 5)
# p1

# run variable.step - to select the top variables 
key_vars <- variable.step(xdata, ydata, iter = 50)

# set machine learning dataframe to use key_vars only
ml_data <- data %>%
  dplyr::select(pb, subarea, all_of(key_vars))

# get RMSE plot
p2 <- last_plot()

# get data from plot
p2data <- p2 %>% pluck("data")

# save machine learning dataframe
saveRDS(ml_data, 
        paste0("output/topographic model/model_data/", species, "_ml_data.rds"))

# save the plots
# ggsave(paste0("output/topographic model/varimps/", species, "_varimp.png"),
#        p1, width = 10, height = 10)
ggsave(paste0("output/topographic model/varselection/", species, "_RMSE_dropped.png"),
       p2, width = 10, height = 10)

# save RMSE data
write_csv(p2data, 
          paste0("output/topographic model/varselection/", species, "_RMSE_dropped.csv"))

# save key variable list
key_var_df <- data.frame(key_vars = key_vars, species_name = species)
write_csv(key_var_df, 
          paste0("output/topographic model/varselection/", species, "_key_vars.csv"))


#-------------------------------------------------------
# Repeat with collinearity testing for regression models
#-------------------------------------------------------

# test for collinearity using variance inflation factors 
library(car)

# fit regression model
regdata <- data %>%
  mutate(pb1 = ifelse(pb == "presence", 1, 0)) %>%
  dplyr::select(-pb, -subarea)
m1 <- lm(pb1 ~ ., data = regdata)

# calculate variance inflation factor
original_scores <- vif(m1)
scores <- original_scores

# identify variable with highest VIF (above 5 only), remove and rerun
high_scores <- scores[scores > 5] 

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

# rerun variable.step with the regression_vars
xdata <- data %>% dplyr::select(all_of(regression_vars))
ydata <- data %>% mutate(pb = ifelse(pb == "presence", 1, 0)) %>% pull(pb)

# run variable.step - to select the top variables 
key_reg_vars <- variable.step(xdata, ydata, iter = 50)

# set regression dataframe to use key_vars only
regdata <- data %>%
  dplyr::select(pb, subarea, all_of(key_reg_vars))

# get RMSE plot
p3 <- last_plot()

# get data from plot
p3data <- p3 %>% pluck("data")

# save regression dataframe
saveRDS(regdata, 
        paste0("output/topographic model/model_data/", species, "_regression_data.rds"))

# save the plots
ggsave(paste0("output/topographic model/varselection/", species, "_RMSE_regression.png"),
       p3, width = 10, height = 10)

# save RMSE data
write_csv(p3data, 
          paste0("output/topographic model/varselection/", species, "_RMSE_regression.csv"))

# save key variable list
key_reg_var_df <- data.frame(key_vars = key_reg_vars, species_name = species)
write_csv(key_reg_var_df, 
          paste0("output/topographic model/varselection/", species, "_key_reg_vars.csv"))

# save original and final VIF scores
original_scores <- data.frame(covariate = names(original_scores),
           vif = original_scores, 
           row.names = 1:length(original_scores))
final_scores <- data.frame(covariate = names(scores),
           vif = scores, 
           row.names = 1:length(scores))

write_csv(original_scores,
          paste0("output/topographic model/varselection/", species, "_original_vif.csv"))
write_csv(final_scores,
          paste0("output/topographic model/varselection/", species, "_final_vif.csv"))



#--------------------------------------------------------------------------
# # correlation matrix additional code
# regdata <- data %>%
#   dplyr::select(-pb, -region)
# correlation_matrix <- cor(regdata, use = "pairwise.complete.obs")
# correlation_matrix
# 
# # filter to values with absolute values over 0.7
# high_correlation <- which(abs(correlation_matrix) > 0.7, arr.ind = TRUE)
# high_correlation %>% as.data.frame() %>%
#   filter(row != col)
# 
# # if agrees with VIF, don't worry
# # if not, remove collinear variable with the lowest variable importance and rerun
