# assign colony and stage to old data
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/PenguinTrack")

library(tidyverse)

# define species
species <- "adelie"
spp_code <- "ADPE"

# load old data
tracks <- readRDS(paste0("tracks/", species, ".RDS"))
stages <- readRDS(paste0("stages/", species, ".RDS"))
meta <- readRDS("meta/working_metadata.RDS") %>%
  filter(abbreviated_name == spp_code)

# append deployment_sites to tracks
tracks <- tracks %>%
  left_join(meta %>% select(deployment_site, individual_id, device_id))

# assign stages to tracks
#identify possible stages
stage_options <- unique(stages$stage)

#for each stage
for(j in stage_options){
  
  #isolate stage dates to this stage
  this_stage <- stages %>%
    filter(stage == j)
  
  #join start and end dates to tracks for each individual
  stage_tracks <- tracks %>%
    left_join(select(this_stage, individual_id, device_id, start, end))
  
  #filter tracks to those that fall into this stage
  stage_tracks <- stage_tracks %>%
    filter(date >= start & date <= end) %>%
    distinct(individual_id, device_id, date, .keep_all = T)
  
  #create stage column
  stage_tracks <- stage_tracks %>%
    mutate(stage = j) %>%
    select(-start, -end)
  
  # combine with all stages
  if(j == stage_options[1]){
    all_stage_tracks <- stage_tracks
  } else {
    all_stage_tracks <- bind_rows(all_stage_tracks, stage_tracks)
  }
}

# read in new tracks 
new <- readRDS(paste0("newdata/", spp_code, "_ssm_qc_tracks.RDS"))

# combine old and new tracks
all_tracks <- bind_rows(all_stage_tracks, new)

# plot chick-rearing and incubation
all_tracks %>%
  filter(stage == "incubation") %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

all_tracks %>%
  filter(stage == "chick-rearing") %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

# export tracks if happy
saveRDS(all_tracks, paste0("tracks/v2/", species, ".RDS"))
