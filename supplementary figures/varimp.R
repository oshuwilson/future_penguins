#------------------------------------------------------
# Variable importance scores for each species and stage
#------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# 1. oceanographic model
model <- "at-sea model"

# all possible combinations of species, longname, and stage
iters <- data.frame(
  species = c("ADPE", "ADPE", "CHPE", "CHPE", "GEPE", "MAPE", "MAPE", "MAPE", "KIPE", "KIPE", "EMPE"),
  longname = c("Adelie Penguin", "Adelie Penguin", "Chinstrap Penguin", "Chinstrap Penguin", 
               "Gentoo Penguin", "Macaroni Penguin", "Macaroni Penguin", "Macaroni Penguin", 
               "King Penguin", "King Penguin", "Emperor Penguin"),
  stage = c("incubation", "chick-rearing", "incubation", "chick-rearing", "chick-rearing", 
            "incubation", "chick-rearing", "pre-moult", "incubation", "chick-rearing", "chick-rearing") 
)

# loop over the species and stages
for(i in 1:nrow(iters)){
  
  # define parameters
  species <- iters$species[i]
  longname <- iters$longname[i]
  stage <- iters$stage[i]
  print(paste0("Processing ", longname, " (", stage, ")"))
  
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
  
  # scale importance from 0-1
  vi <- vi %>%
    group_by(algorithm) %>%
    mutate(Importance = (Importance-min(Importance))/(max(Importance)-min(Importance))) %>%
    ungroup()
  
  # scale importance from 0.1 to 1
  vi <- vi %>%
    group_by(algorithm) %>%
    mutate(Importance = Importance * 0.9 + 0.1) %>%
    ungroup()
  
  # remove maxent as an option
  vi <- vi %>%
    filter(algorithm != "MaxEnt")
  
  # append species and stage to variable names
  vi <- vi %>%
    mutate(species = species,
           stage = stage,
           longname = longname)
  
  # join to other iterations
  if(i == 1){
    vi_all <- vi
  } else {
    vi_all <- bind_rows(vi_all, vi)
  }
  
}

# plot
p1 <- ggplot(vi_all, aes(x = Variable, y = Importance)) +
  geom_bar(aes(fill = algorithm), alpha = 1, stat = "identity", position = position_dodge(0.75)) + 
  scale_fill_viridis_d(end = 0.9, option = "D", name = "Algorithm") +
  theme_minimal() +
  ylab("Relative Covariate Importance") + 
  xlab("Covariate") +
  facet_wrap(~longname * stage, ncol = 2) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
p1 + ggview::canvas(width = 8, height = 14)

# export
ggsave(filename = paste0("text/figures/draft/supplementary/oceanographic variable importance.png"),
       plot = p1,
       width = 8,
       height = 14,
       units = "in",
       dpi = 300)



# 1. terrestrial model
rm(list=ls())
model <- "climatic model"

# all possible combinations of species and longname
iters <- data.frame(
  species = c("ADPE", "CHPE", "GEPE", "MAPE", "KIPE", "EMPE"),
  longname = c("Adelie Penguin", "Chinstrap Penguin", "Gentoo Penguin", 
               "Macaroni Penguin", "King Penguin", "Emperor Penguin") 
)

# loop over the species and stages
for(i in 1:nrow(iters)){
  
  # define parameters
  species <- iters$species[i]
  longname <- iters$longname[i]
  
  # list files that contain the species, stage, and "varimp"
  files <- list.files(path = paste0("output/", model), 
                      pattern = paste0(species, "_varimp_scores"), 
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
  
  # scale importance from 0-1
  vi <- vi %>%
    group_by(algorithm) %>%
    mutate(Importance = (Importance-min(Importance))/(max(Importance)-min(Importance))) %>%
    ungroup()
  
  # scale importance from 0.1 to 1
  vi <- vi %>%
    group_by(algorithm) %>%
    mutate(Importance = Importance * 0.9 + 0.1) %>%
    ungroup()
  
  # remove maxent as an option
  vi <- vi %>%
    filter(algorithm != "MaxEnt")
  
  # append species and stage to variable names
  vi <- vi %>%
    mutate(species = species,
           longname = longname)
  
  # join to other iterations
  if(i == 1){
    vi_all <- vi
  } else {
    vi_all <- bind_rows(vi_all, vi)
  }
  
}

# recode all covariates to temp, now, or prec
vi_all <- vi_all %>%
  mutate(Variable = case_when(
    Variable %in% c("avg_min_temp", "avg_temp", "avg_max_temp") ~ "temp",
    Variable %in% c("avg_min_prec", "avg_prec", "avg_max_prec") ~ "prec",
    Variable %in% c("avg_min_now", "avg_now", "avg_max_now") ~ "now",
    TRUE ~ Variable
  ))

# plot
p1 <- ggplot(vi_all, aes(x = Variable, y = Importance)) +
  geom_bar(aes(fill = algorithm), alpha = 1, stat = "identity", position = position_dodge(0.75)) + 
  scale_fill_viridis_d(end = 0.9, option = "D", name = "Algorithm") +
  theme_minimal() +
  ylab("Relative Covariate Importance") + 
  xlab("Covariate") +
  facet_wrap(~longname, ncol = 2, scales = "free") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2), expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
p1 + ggview::canvas(width = 8, height = 10)

# export
ggsave(filename = paste0("text/figures/draft/supplementary/climatic variable importance.png"),
       plot = p1,
       width = 8,
       height = 10,
       units = "in",
       dpi = 300)
