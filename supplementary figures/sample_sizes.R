#-------------------------------------------------------------------------------
# Model sample size info
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# 1. Terrestrial Model

# initial sample sizes
# list all colony files
files <- list.files("data/colonies/subareas", full.names = T)

# for each file
for(file in files){
  
  # get species name
  species_name <- str_split(basename(file), "_")[[1]][1]
  
  # read in file
  data <- readRDS(file)
  
  # get nrow
  n <- nrow(data)
  
  # make table
  df <- data.frame(species = species_name, n = n)
  
  # combine to other species
  if(file == files[1]){
    df_all <- df
  } else {
    df_all <- rbind(df_all, df)
  }
}

# view
df_all

# post-processing sample sizes
# list all model_data files
files <- list.files("output/climatic model/thinned/", full.names = T)

# for each file
for(file in files){
  
  # get species name
  species_name <- str_split(basename(file), " ")[[1]][1]
  
  # read in file
  data <- readRDS(file)
  
  # get number of presences
  np <- nrow(data %>% filter(pa == "presence"))

  # make table
  df <- data.frame(species = species_name, np = np)
  
  # combine to other species
  if(file == files[1]){
    df_all <- df
  } else {
    df_all <- rbind(df_all, df)
  }
}

# view
df_all



# 1. Oceanographic Model

# initial sample sizes
# adelie
readRDS("data/tracks/ADPE.RDS") %>%
  group_by(stage) %>% 
  summarise(n = n())

# chinstrap
readRDS("data/tracks/CHPE.RDS") %>%
  group_by(stage) %>% 
  summarise(n = n())

# gentoo
readRDS("data/tracks/GEPE.RDS") %>%
  group_by(stage) %>% 
  summarise(n = n())

# emperor
readRDS("data/tracks/EMPE.RDS") %>%
  group_by(stage) %>% 
  summarise(n = n())

# king
readRDS("data/tracks/KIPE.RDS") %>%
  group_by(stage) %>% 
  summarise(n = n())

# macaroni
readRDS("data/tracks/MAPE.RDS") %>%
  group_by(stage) %>% 
  summarise(n = n())


# post-processing sample sizes
# adpe chick-rearing
readRDS("output/at-sea model/thinned_tracks/ADPE_chick-rearing_thinned.RDS") %>%
  nrow()

# adpe incubation
readRDS("output/at-sea model/thinned_tracks/ADPE_incubation_thinned.RDS") %>%
  nrow()

# chpe chick-rearing
readRDS("output/at-sea model/thinned_tracks/CHPE_chick-rearing_thinned.RDS") %>%
  nrow()

# chpe incubation
readRDS("output/at-sea model/thinned_tracks/CHPE_incubation_thinned.RDS") %>%
  nrow()

# gepe chick-rearing
readRDS("output/at-sea model/thinned_tracks/GEPE_chick-rearing_thinned.RDS") %>%
  nrow()

# gepe incubation
readRDS("output/at-sea model/thinned_tracks/GEPE_incubation_thinned.RDS") %>%
  nrow()

# kipe chick-rearing
readRDS("output/at-sea model/thinned_tracks/KIPE_chick-rearing_thinned.RDS") %>%
  nrow()

# kipe incubation
readRDS("output/at-sea model/thinned_tracks/KIPE_incubation_thinned.RDS") %>%
  nrow()

# mape chick-rearing
readRDS("output/at-sea model/thinned_tracks/MAPE_chick-rearing_thinned.RDS") %>%
  nrow()

# mape incubation
readRDS("output/at-sea model/thinned_tracks/MAPE_incubation_thinned.RDS") %>%
  nrow()

# mape pre-moult
readRDS("output/at-sea model/thinned_tracks/MAPE_pre-moult_thinned.RDS") %>%
  nrow()

# empe chick-rearing
readRDS("output/at-sea model/thinned_tracks/EMPE_chick-rearing_thinned.RDS") %>%
  nrow()

# empe incubation
readRDS("output/at-sea model/thinned_tracks/EMPE_incubation_thinned.RDS") %>%
  nrow()
