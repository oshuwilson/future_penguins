#-----------------------------------------------------------------------------------
# Thin  climatic data using an environmental clustering approach (Pili et al. 2025)
#-----------------------------------------------------------------------------------

# adapted from script on Arman Pili's Github
# https://github.com/UP-macroecology/Pili_EnvSubsampling/blob/master/functions/processData_E_clustering.R
# supplementary material suggests k-means clustering and umap reduction for explaining the niche


rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(umap)
library(dbscan)

# define species
species <- "ADPE"

# read in extracted info
data <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))

# if nearest open water is NA, replace with 0
data <- data %>%
  mutate(avg_now = ifelse(is.na(avg_now), 0, avg_now),
         avg_min_now = ifelse(is.na(avg_min_now), 0 , avg_min_now),
         avg_max_now = ifelse(is.na(avg_max_now), 0, avg_max_now))

# remove NAs
data <- data %>%
  na.omit()

# predictor column names
preds <- c("avg_temp", "avg_min_temp", "avg_max_temp",
           "avg_prec", "avg_min_prec", "avg_max_prec",
           "avg_now", "avg_min_now", "avg_max_now")

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
cluster <- hdbscan(scaled_umap, minPts = 10)$cluster

# bind cluster info
data <- data %>%
  bind_cols(cluster = cluster)

# isolate presences and absences
pres <- data %>%
  filter(pa == "presence")
abs <- data %>%
  filter(pa == "absence")

# filter to 1 location per cluster
pres <- pres %>%
  group_by(cluster) %>%
  sample_n(1) %>%
  ungroup()
abs <- abs %>%
  group_by(cluster) %>%
  sample_n(1) %>%
  ungroup()

# limit further to avoid bias towards antarctic coast in absences
small <- abs %>%
  group_by(subarea) %>%
  summarise(n = n()) %>%
  filter(n < 20) %>%
  pull(subarea)
downsampled <- abs %>%
  filter(!subarea %in% small) %>%
  group_by(subarea) %>%
  sample_n(20) %>%
  ungroup()
unchanged <- abs %>%
  filter(subarea %in% small)
thinned <- rbind(downsampled, unchanged)

# join the thinned data with the presence data
all <- rbind(thinned, pres) %>% 
  select(-cluster)

# convert to terra
allx <- all %>% vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")

# plot
ggplot() +
  geom_spatvector(data = allx, aes(col = pa))
ggplot(all, aes(x = pa, y = avg_max_temp)) +
  geom_jitter()

# export
saveRDS(all, paste0("output/climatic model/thinned/", species, " env thinned.rds"))
