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
meta <- expand.grid(species = c("ADPE", "CHPE", "KIPE"),
            stage = c("chick-rearing", "incubation")) %>%
  as_tibble()

# loop over each row of meta
for(i in 1:nrow(meta)){
  
  # define species and stage
  species <- meta$species[i]
  stage <- meta$stage[i]
  
  # print species and stage
  print(paste(species, stage))
  
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
  
  # isolate background data
  bg <- data %>%
    filter(pb == "background")
    
  # if dataset is bigger than 30,000 points, randomly sample background points to 30,000 [to work with dbscan]
  if(nrow(bg) > 30000){
    bg <- sample_n(bg, 30000)
  }
  
  # scale the environmental data
  scaled <- bg %>%
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
  bg <- bg %>%
    bind_cols(cluster = cluster)
  
  # isolate points not assigned to a cluster
  zeroes <- bg %>%
    filter(cluster == 0)
  
  # filter to 1 location per cluster
  bg <- bg %>%
    group_by(cluster) %>%
    sample_n(1) %>%
    ungroup() %>%
    filter(cluster != 0)
  
  # get presence data
  pres <- data %>%
    filter(pb == "presence")
  
  # join the data
  all <- bind_rows(bg, pres, zeroes) %>% 
    select(-cluster)
  
  # convert to terra
  allx <- all %>% vect(geom = c("x", "y"), crs = "epsg:4326") %>%
    project("epsg:6932")
  
  # export
  saveRDS(all, paste0("output/at-sea model/extraction/", species, " ", stage, " extracted subsampled.RDS"))
}