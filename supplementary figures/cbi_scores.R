#-------------------------------------------------
# Boyce Index scores for each species and stage
#-------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# definitions
species <- "ADPE"
stage <- "chick-rearing"
model <- "at-sea model"

# list files that contain the species, stage, and "cbi"
files <- list.files(path = paste0("output/", model), 
                    pattern = paste0(species, "_", stage, "_cbi_scores"), 
                    full.names = TRUE,
                    recursive = TRUE)

# read the files and combine them into a single data frame
for(file in files){
  
  # read in scores
  cbi <- readRDS(file)
  
  # get model name from file
  model_name <- str_split(file, pattern = "/")[[1]][3]
  
  # add model name
  cbi <- cbi %>%
    mutate(algorithm = model_name) %>%
    select(.estimate, algorithm, subarea)
  
  # join to other files
  if(file == files[1]){
    cbi_all <- cbi
  } else {
    cbi_all <- bind_rows(cbi_all, cbi)
  }
}

# change model algorithm codes
cbi_all <- cbi_all %>%
  mutate(algorithm = case_when(
    algorithm == "generalised additive models" ~ "GAM",
    algorithm == "random forests" ~ "RF",
    algorithm == "boosted regression trees" ~ "BRT",
    algorithm == "maxent" ~ "MaxEnt",
    algorithm == "bayesian additive regression trees" ~ "BART"
  )) 

ggplot(cbi_all, aes(x = algorithm, y = .estimate)) +
  geom_hline(yintercept = 0.4, linetype = "dashed", col = "grey30") +
  stat_summary(fun.data = mean_se, geom = "crossbar", col = "grey20", fill = "grey", 
               alpha = 0.3, width = 0.2) +
  geom_point(aes(col = subarea), size = 3, alpha = 1) +
  #stat_summary(fun.y = mean, geom = "point", shape = 18, col = "darkred", size = 6) +
  theme_bw() +
  ylim(0, 1) +
  scale_color_viridis_d(option = "B", end = 0.9, name = "Test Subarea") +
  ggtitle(paste0(species, " ", stage, " Continuous Boyce Index cross-validation scores"))

