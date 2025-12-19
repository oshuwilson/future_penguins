#-------------------------------------------------------------------------------
# explore selected variables in terrestrial models
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# BARTs
# list all cbi scores
bart_cbi <- list.files("output/climatic model/bayesian additive regression trees/", pattern = "_cbi_scores.rds", full.names = T)

# for each file
for(file in bart_cbi){
  
  # read in file
  cbi <- readRDS(file)
  
  # take the highest mean scores and obtain predictors
  preds <- cbi %>%
    arrange(mean) %>%
    slice(1) %>%
    pull(predictors)
  
  # separate out by comma into temp, prec, and now
  temp <- strsplit(preds, ",")[[1]][1]
  prec <- strsplit(preds, ",")[[1]][2]
  now <- strsplit(preds, ",")[[1]][3]
  
  # remove leading spaces
  prec <- trimws(prec)
  now <- trimws(now)
  
  # get species
  species <- strsplit(basename(file), "_")[[1]][1]
  
  # make df
  df <- data.frame(species = species,
                   temp = temp,
                   prec = prec,
                   now = now,
                   algo = "BART")
  
  # combine with all other files
  if(file == bart_cbi[1]){
    all_bart <- df
  } else {
    all_bart <- rbind(all_bart, df)
  }
}

# BRTs
# list all cbi scores
brt_cbi <- list.files("output/climatic model/boosted regression trees/", pattern = "_cbi_scores.rds", full.names = T)

# for each file
for(file in brt_cbi){
  
  # read in file
  cbi <- readRDS(file)
  
  # take the highest mean scores and obtain predictors
  preds <- cbi %>%
    arrange(mean) %>%
    slice(1) %>%
    pull(predictors)
  
  # separate out by comma into temp, prec, and now
  temp <- strsplit(preds, ",")[[1]][1]
  prec <- strsplit(preds, ",")[[1]][2]
  now <- strsplit(preds, ",")[[1]][3]
  
  # remove leading spaces
  prec <- trimws(prec)
  now <- trimws(now)
  
  # get species
  species <- strsplit(basename(file), "_")[[1]][1]
  
  # make df
  df <- data.frame(species = species,
                   temp = temp,
                   prec = prec,
                   now = now,
                   algo = "BRT")
  
  # combine with all other files
  if(file == brt_cbi[1]){
    all_brt <- df
  } else {
    all_brt <- rbind(all_brt, df)
  }
}


# RFs
# list all cbi scores
rf_cbi <- list.files("output/climatic model/random forests/", pattern = "_cbi_scores.rds", full.names = T)

# for each file
for(file in rf_cbi){
  
  # read in file
  cbi <- readRDS(file)
  
  # take the highest mean scores and obtain predictors
  preds <- cbi %>%
    arrange(mean) %>%
    slice(1) %>%
    pull(predictors)
  
  # separate out by comma into temp, prec, and now
  temp <- strsplit(preds, ",")[[1]][1]
  prec <- strsplit(preds, ",")[[1]][2]
  now <- strsplit(preds, ",")[[1]][3]
  
  # remove leading spaces
  prec <- trimws(prec)
  now <- trimws(now)
  
  # get species
  species <- strsplit(basename(file), "_")[[1]][1]
  
  # make df
  df <- data.frame(species = species,
                   temp = temp,
                   prec = prec,
                   now = now,
                   algo = "RF")
  
  # combine with all other files
  if(file == rf_cbi[1]){
    all_rf <- df
  } else {
    all_rf <- rbind(all_rf, df)
  }
}

# GAMs
# list all cbi scores
gam_cbi <- list.files("output/climatic model/generalised additive models/", pattern = "_cbi_scores.rds", full.names = T)

# for each file
for(file in gam_cbi){
  
  # read in file
  cbi <- readRDS(file)
  
  # take the highest mean scores and obtain predictors
  preds <- cbi %>%
    arrange(mean) %>%
    slice(1) %>%
    pull(predictors)
  
  # separate out by comma into temp, prec, and now
  temp <- strsplit(preds, ",")[[1]][1]
  prec <- strsplit(preds, ",")[[1]][2]
  now <- strsplit(preds, ",")[[1]][3]
  
  # remove leading spaces
  prec <- trimws(prec)
  now <- trimws(now)
  
  # get species
  species <- strsplit(basename(file), "_")[[1]][1]
  
  # make df
  df <- data.frame(species = species,
                   temp = temp,
                   prec = prec,
                   now = now,
                   algo = "GAM")
  
  # combine with all other files
  if(file == gam_cbi[1]){
    all_gam <- df
  } else {
    all_gam <- rbind(all_gam, df)
  }
}

# combine all together
all_models <- rbind(all_bart, all_brt, all_rf, all_gam)

# pivot
all_models_long <- all_models %>%
  pivot_longer(cols = c("temp", "prec", "now"), names_to = "variable_type", values_to = "variable")

# recode to max, min, or avg
all_models_long <- all_models_long %>%
  mutate(variable_code = case_when(
    grepl("max", variable) ~ "Yearly Maximum",
    grepl("min", variable) ~ "Yearly Minimum",
    grepl("avg", variable) ~ "Yearly Average"
  ))

# recode environmental names
all_models_long <- all_models_long %>%
  mutate(variable_type = case_when(
    variable_type == "temp" ~ "Temperature",
    variable_type == "prec" ~ "Precipitation",
    variable_type == "now" ~ "Nearest Open Water"
  ))

# recode species names
all_models_long <- all_models_long %>%
  mutate(species = case_when(
    species == "ADPE" ~ "Adelie",
    species == "CHPE" ~ "Chinstrap",
    species == "GEPE" ~ "Gentoo",
    species == "EMPE" ~ "Emperor",
    species == "KIPE" ~ "King",
    species == "MAPE" ~ "Macaroni"
  ))

# plot
p1 <- ggplot(all_models_long, aes(x = species, y = variable_code, fill = algo)) +
  geom_tile(color = "white") +
  facet_wrap(~variable_type*algo,
             labeller = label_wrap_gen(multi_line=TRUE)) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme_minimal() +
  ylab("") +
  xlab("Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# preview
p1 + ggview::canvas(width = 10, height = 6)

# export
ggsave("text/figures/draft/supplementary/terrestrial_variable_selection.png",
       plot = p1,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)
