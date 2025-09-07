#------------------------------------------------------
# Combine PDPs across models for each species and stage
#------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# definitions
species <- "ADPE"
stage <- "incubation"
model <- "at-sea model"

# list files that contain the species, stage, and "pdp"
files <- list.files(path = paste0("output/", model), 
                    pattern = paste0(species, "_", stage, "_pdp_values"), 
                    full.names = TRUE,
                    recursive = TRUE)

# for each file
for(file in files){
  
  # get model name from file
  model_name <- str_split(file, pattern = "/")[[1]][3]
  
  # read in the file
  pdp_data <- readRDS(file)
  
  # add model name
  pdp_data <- pdp_data %>%
    mutate(algorithm = model_name)
  
  # join to other files
  if(file == files[1]) {
    pdp <- pdp_data
  } else {
    pdp <- bind_rows(pdp, pdp_data)
  }
}

# change model algorithm codes
pdp <- pdp %>%
  mutate(algorithm = case_when(
    algorithm == "generalised additive models" ~ "GAM",
    algorithm == "random forests" ~ "RF",
    algorithm == "boosted regression trees" ~ "BRT",
    algorithm == "maxent" ~ "MaxEnt",
    algorithm == "bayesian additive regression trees" ~ "BART"
  )) 

# plot
ggplot(pdp, aes(x = x, y = yhat)) +
  geom_line(aes(col = algorithm), lwd = 1.2, alpha = 0.75) +
  facet_wrap(~var, scales = "free_x", nrow = 1) + 
  ylim(0, 1) + 
  scale_color_viridis_d(end = 0.9, option = "B") +
  theme_bw() +
  ylab("Predicted habitat suitability") + 
  xlab("Predictor values") +
  ggtitle(paste0(species, " ", stage, " Partial Dependence Plots"))

