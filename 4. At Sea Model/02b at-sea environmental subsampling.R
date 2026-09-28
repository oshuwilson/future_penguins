#--------------------------------------------------------------------------------------
# Thin oceanographic data using an environmental clustering approach (Pili et al. 2025)
#--------------------------------------------------------------------------------------

# adapted from script on Arman Pili's Github
# https://github.com/UP-macroecology/Pili_EnvSubsampling/blob/master/functions/processData_E_clustering.R

rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/penguins/")

library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(sf)
library(umap)
library(dbscan)

# create dataframe of species and stage options
meta <- expand.grid(species = c("ADPE", "CHPE", "EMPE", "GEPE", "KIPE", "MAPE"),
            stage = c("chick-rearing", "incubation")) %>%
  as_tibble()

# remove EMPE and GEPE incubation (data limitations)
meta <- meta %>%
  filter_out(species %in% c("EMPE", "GEPE") & stage == "incubation")

# add macaroni pre-moult
meta <- meta %>%
  bind_rows(tibble(species = "MAPE", stage = "pre-moult"))

# loop over each row of meta
for(i in 1:nrow(meta)){
  
  # define species and stage
  species <- meta$species[i]
  stage <- meta$stage[i]
  
  # print initiation
  print(paste0(species, " ", stage, " initiated"))
  
  # read in extracted data
  data <- readRDS(paste0("output/at-sea model/extraction/", species, " ", stage, " extracted.RDS"))
  
  # convert data to a dataframe
  data <- data %>%
    as.data.frame(geom = "XY") %>%
    rename(pb = pa)
  
  # candidate variables
  preds <- c("depth", "slope", "sst", "sal", 
             "sic", "curr", "mld", "dshelf")
  
  # select variables and other key info
  data <- data %>%
    select(all_of(preds), pb, date, subarea, x, y)
  
  # remove NAs
  data <- data %>%
    na.omit()

  # scale the environmental data
  scaled <- data %>%
    select(all_of(preds)) %>%
    mutate(across(all_of(preds), scale))
  
  # dimensionality reduction with umap
  umap_config <- umap.defaults
  umap_config$random_state <- 7
  umap_config$n_neighbors <- 5
  umap_config$n_components <- 5
  
  scaled_umap <- umap(scaled, config = umap_config)$layout %>%
    data.frame()
  
  # clustering with dbscan
  set.seed(777)
  cluster <- hdbscan(scaled_umap, minPts = 2)$cluster
  
  # bind cluster info
  data <- data %>%
    bind_cols(cluster = cluster)
  
  # isolate points not assigned to a cluster
  zeroes <- data %>%
    filter(cluster == 0)
  zeroes_pres <- zeroes %>% 
    filter(pb == "presence")
  zeroes_abs <- zeroes %>% 
    filter(pb == "background")
  
  # isolate presences and absences
  pres <- data %>%
    filter(pb == "presence")
  abs <- data %>%
    filter(pb == "background")
  
  # filter to 1 location per cluster
  pres <- pres %>%
    group_by(cluster) %>%
    sample_n(1) %>%
    ungroup() %>%
    bind_rows(zeroes_pres)
  abs <- abs %>%
    group_by(cluster) %>%
    sample_n(1) %>%
    ungroup() %>%
    bind_rows(zeroes_abs)
  
  # join the data together
  all <- bind_rows(pres, abs) %>% 
    select(-cluster)
  
  # export
  saveRDS(all, paste0("output/at-sea model/extraction/", species, " ", stage, " extracted subsampled.RDS"))
}
