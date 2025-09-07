#------------------------------------------------------
# Variable importance scores for each species and stage
#------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# definitions
species <- "ADPE"
stage <- "incubation"
model <- "at-sea model"

# list files that contain the species, stage, and "varimp"
files <- list.files(path = paste0("output/", model), 
                    pattern = paste0(species, "_", stage, "_varimp_scores"), 
                    full.names = TRUE,
                    recursive = TRUE)

# read in the files
for(file in files){
  
  # read in this file
  varimp <- readRDS(file)
  
  # get model name from file
  model_name <- str_split(file, pattern = "/")[[1]][3]
  
  # add model name
  varimp <- varimp %>%
    mutate(algorithm = model_name)
  
  # join to other files
  if(file == files[1]){
    vi <- varimp
  } else {
    vi <- bind_rows(vi, varimp)
  }
}

# change model algorithm codes
vi <- vi %>%
  mutate(algorithm = case_when(
    algorithm == "generalised additive models" ~ "GAM",
    algorithm == "random forests" ~ "RF",
    algorithm == "boosted regression trees" ~ "BRT",
    algorithm == "maxent" ~ "MaxEnt",
    algorithm == "bayesian additive regression trees" ~ "BART"
  ))

# group by alogorithm and scale to 0-100
vi <- vi %>%
  group_by(algorithm) %>%
  mutate(Importance = Importance / max(Importance) * 100) %>%
  ungroup()

# plot
ggplot(vi, aes(x = Variable, y = Importance)) +
  geom_boxplot(outliers = F) +
  geom_point(aes(col = algorithm), size = 3, position = position_dodge(0.5)) + 
  scale_color_viridis_d(end = 0.9, option = "B") +
  theme_bw() +
  ylab("Relative Variable Importance") + 
  xlab("Variable") +
  ggtitle(paste0(species, " ", stage, " Variable Importance"))
