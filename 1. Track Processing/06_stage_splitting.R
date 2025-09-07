# split into trips and assign automatic stage dates
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/PenguinTrack")

library(tidyverse)
library(terra)
library(tidyterra)

#---------------------------------------------------------
# 1. Configuration
#---------------------------------------------------------

# read in coastline from CCAMLRGIS R package
coast <- vect("~/OneDrive - University of Southampton/Documents/Chapter 03/data/coast_vect.RDS")

# define species
species <- "ADPE"

# read in state space modelled tracks for this species
gps_tracks <- readRDS(paste0("ssm_tracks/", species, "_gps_state_space_modelled.RDS"))
ptt_tracks <- readRDS(paste0("ssm_tracks/", species, "_ptt_state_space_modelled.RDS"))
tracks <- rbind(gps_tracks, ptt_tracks)
rm(gps_tracks, ptt_tracks)

# read in metadata for this species
meta <- readRDS("meta/working_metadata.RDS") %>%
  filter(abbreviated_name == species)

# join deployment_sites to tracking data
tracks <- tracks %>%
  left_join(select(meta, deployment_site, individual_id, device_id))

# get colonies in new data
colony_names <- tracks %>%
  select(deployment_site) %>%
  distinct() %>%
  pull()

#read in automatic stage dates
stage_dates <- readRDS(paste0("auto_stages/", species, "config.RDS"))


#----------------------------------------------------------------------
# 2. Calculate the distance of locations from deployment sites by ID
#----------------------------------------------------------------------

#run over each ID
for(i in unique(tracks$individual_id)){
  
  #tracks for this id
  trax <- tracks %>% filter(individual_id == i)
  
  #deployment lat/lon for this id
  dep <- meta %>%
    filter(individual_id == i) %>%
    select(deployment_decimal_longitude, deployment_decimal_latitude) %>%
    rename(lon = deployment_decimal_longitude, lat = deployment_decimal_latitude)
  
  #convert tracks and deployment to terra
  trax <- vect(trax, geom = c("lon", "lat"), crs = "epsg:4326")
  dep <- vect(dep, geom = c("lon", "lat"), crs = "epsg:4326")
  
  #calculate distance
  trax$distance <- distance(trax, dep)
  
  #convert back to dataframe
  trax <- as.data.frame(trax, geom="XY")
  trax <- trax %>%
    rename(lon = x, lat = y)
  
  #append to all tracks
  if(i == unique(tracks$individual_id)[1]) {
    all_tracks <- trax
  } else {
    all_tracks <- bind_rows(all_tracks, trax)
  }
}

#rename all_tracks and cleanup
tracks <- all_tracks
rm(all_tracks, dep, trax, i)


#-----------------------------------------------------------------------
# 3. Split tracks into trips
#-----------------------------------------------------------------------

#load trip splitting function
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/trip_split.R")

#loop over each colony
for(colony_name in colony_names){
  
  # print initialisation
  print(colony_name)
  
  #split tracks to tracks in this colony
  colony_tracks <- tracks %>% 
    filter(deployment_site == colony_name)
  
  #convert tracks to terra stereographic view
  trax <- vect(colony_tracks, geom = c("lon", "lat"), crs = "epsg:4326")
  trax <- project(trax, "epsg:6932")
  plot(trax, pch = ".")
  
  # erase all points on land
  trax <- erase(trax, coast)
  
  #split up trips
  if(colony_name == "Esperanza, Antarctic Peninsula"){
    colony_tracks <- trip_split(trax, meta, buff.dist = 18000)
  } else {
    colony_tracks <- trip_split(trax, meta, buff.dist = 3000)
  }
  
  #crop coastline to track extent
  crop_coast <- crop(coast, ext(trax))
  
  
  # 3a. Assign stage dates to each trip
  
  #extract first point of each trip
  trip_starts <- colony_tracks %>%
    group_by(individual_id, trip) %>%
    slice(1) %>%
    ungroup() %>%
    select(individual_id, device_id, date, trip)
  
  #create yday column for trip starts
  trip_starts <- trip_starts %>%
    mutate(yday = yday(date))
  
  #create column for automatic stage assignment in trip starts
  trip_starts <- trip_starts %>%
    mutate(stage = NA)
  
  #join stage dates to trip starts by stage
  for(i in 1:nrow(stage_dates)){
    
    #define stage
    this.stage <- stage_dates[i,]
    
    #check if stage covers new year
    if(this.stage$end_day < this.stage$start_day){
      new_year <- TRUE
    } else {
      new_year <- FALSE
    }
    
    #assign stage to trip starts
    if(new_year == FALSE){
      trip_starts <- trip_starts %>%
        mutate(stage = ifelse(yday >= this.stage$start_day & yday <= this.stage$end_day, this.stage$stage, stage))
    } else {
      trip_starts <- trip_starts %>%
        mutate(stage = ifelse(yday >= this.stage$start_day | yday <= this.stage$end_day, this.stage$stage, stage))
    }
  }
  
  #join stage to tracks
  colony_tracks <- colony_tracks %>%
    left_join(select(trip_starts, individual_id, device_id, trip, stage))
  
  #cleanup
  rm(trip_starts, this.stage, new_year)
  
  
  # 3b. Visualize tracks for each stage
  
  #convert trip to factor
  colony_tracks$trip <- as.factor(colony_tracks$trip)
  
  #define all stages present
  stages <- unique(colony_tracks$stage)
  
  #define all ids present
  ids <- unique(colony_tracks$individual_id)
  
  #setup pdf export
  pdf(file = paste0("stages/checks/", species, " ", colony_name, " checks.pdf"), width = 8, height = 10, pointsize = 16)
  
  #plot tracks for each id and stage and highlight trips
  for(i in ids){
    
    #subset tracks for this id
    trax <- colony_tracks %>% filter(individual_id == i)
    
    for(j in stages){
      
      #subset tracks for this stage
      stage_trax <- trax %>% filter(stage == j)
      
      #if empty skip
      if(nrow(stage_trax) == 0){
        next
      }
      
      #convert to terra
      stage_trax <- vect(stage_trax, geom = c("x", "y"), crs = "epsg:6932")
      
      #create terra object for all other tracks of this stage
      all_stage <- colony_tracks %>% 
        filter(individual_id != i & stage == j) %>%
        vect(geom = c("x", "y"), crs = "epsg:6932")
      
      #plot together
      if(length(all_stage) > 0){
        p1 <- ggplot() + geom_spatvector(data = crop_coast, fill = "white") +
          geom_spatvector(data = all_stage, size = 0.5, color = "grey") +
          geom_spatvector(data = stage_trax, size = 2, shape = 17, aes(color = trip)) + 
          theme_bw() + scale_color_viridis_d() +
          labs(title = paste(i, j))
      } else{
        p1 <- ggplot() + geom_spatvector(data = crop_coast, fill = "white") +
          geom_spatvector(data = stage_trax, size = 2, shape = 17, aes(color = trip)) + 
          theme_bw() + scale_color_viridis_d() +
          labs(title = paste(i, j))
      }
      print(p1)
    }
  }
  
  #finish pdf export
  dev.off()

  #join colony tracks to all other tracks
  if(colony_name == colony_names[1]) {
    all_tracks <- colony_tracks
  } else {
    all_tracks <- bind_rows(all_tracks, colony_tracks)
  }
  
  # print completion
  print(paste0(colony_name, " processed"))
}



#-----------------------------------------------------------------------
# 4. Create stage dates for each id
#-----------------------------------------------------------------------

#format final stage dates
stage_by_id <- all_tracks %>% 
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

#save initial stage dates
saveRDS(stage_by_id, paste0("stages/new/", species, "_stages_prelim.RDS"))

#save tracks with trips attached
saveRDS(all_tracks, paste0("stages/new/", species, "_tracks_with_stage_trips.RDS"))

  
  
  