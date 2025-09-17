# state space modelling
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/PenguinTrack")

#load libraries
{
  library(tidyverse)
  library(data.table)
  library(sf)
  library(terra)
  library(tidyterra)
  library(CCAMLRGIS)
  library(aniMotum)
  library(patchwork)
}

# define species
species <- "CHPE"

# get max velocity
vmax <- read_csv("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/speed_filters.csv",
                 show_col_types = F) %>%
  rename(spp = species) %>%
  filter(spp == species) %>%
  select(max_speed) %>%
  as.numeric()

#source quality control plot function
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/qcplot.R")



# 1. Data preparation

# read in new tracking data
tracks <- readRDS(paste0("newdata/", species, "_filtered_tracks.RDS"))

# read in metadata and filter to this species
meta <- readRDS("meta/working_metadata.RDS") %>%
  filter(abbreviated_name == species)

# create id code from individual_id and device_id
tracks <- tracks %>%
  mutate(id = paste(individual_id, device_id, sep = "!!"))

# calculate time difference between each point for each individual
tracks <- tracks %>%
  group_by(id) %>% 
  mutate(lag_date = lag(date)) %>%
  mutate(timediff = as.numeric(difftime(date, lag_date, units = "hours")))

# if time gaps bigger than 2 days exist, create a new id for each segment
tracks <- tracks %>%
  mutate(new_id = ifelse(timediff > 48, 1, 0)) %>%
  mutate(new_id = ifelse(is.na(new_id), 0, new_id)) %>%
  mutate(new_id = cumsum(new_id)) %>%
  mutate(new_id = paste(id, new_id, sep = "_")) %>%
  ungroup()

# append metadata to tracks
tracks <- tracks %>%
  left_join(select(meta, individual_id, device_type, device_type))

# split into PTT and GPS
ptt_tracks <- tracks %>%
  filter(device_type == "PTT") %>%
  select(-device_type)
gps_tracks <- tracks %>%
  filter(device_type == "GPS") %>%
  select(-device_type)


# 2. Process PTT tracks

# select only key variables 
ptt_tracks <- ptt_tracks %>% 
  select(new_id, date, lon, lat, lc) %>% 
  rename(id = new_id) %>%
  arrange(id, date)

# remove segments with fewer than 10 points for aniMotum to work
ptt_tracks <- ptt_tracks %>%
  group_by(id) %>%
  filter(n() >= 10) %>%
  ungroup()

# remove segments shorter than 10 hours for aniMotum to work
ptt_tracks <- ptt_tracks %>% 
  group_by(id) %>% 
  filter(difftime(max(date), min(date), units = "hours") >= 10) %>%
  ungroup()

# fit state-space-model
fit_ptt <- fit_ssm(ptt_tracks,
                    time.step = 2, #time sampling interval in hours
                    vmax = vmax, #speed filter in m/s
                    model="crw")

# reroute tracks around land
#fit_ptt <- route_path(fit_ptt)

# quality control plots
qcplot(fit = fit_ptt, 
       spp = species, 
       df = ptt_tracks,
       device_type = "PTT",  
       step_duration = 2)

#grab predicted locations from GPS fit
ptt_ssm_tracks <- grab(fit_ptt, what = "predicted")

#join device ids with individual ids
individuals <- ptt_tracks %>%
  mutate(new_id = gsub("_\\d+$", "", id)) %>%
  mutate(individual_id = gsub("!!.*", "", new_id),
         device_id = gsub(".*!!", "", new_id)) %>%
  select(id, individual_id, device_id) %>%
  distinct()
ptt_ssm_tracks <- ptt_ssm_tracks %>% left_join(individuals)

#select important columns and standardise names
ptt_ssm_tracks <- ptt_ssm_tracks %>% 
  ungroup() %>%
  select(individual_id, device_id, date, lon, lat, x.se, y.se) %>%
  rename(lon_se_km = x.se, lat_se_km = y.se)

#export
saveRDS(ptt_ssm_tracks,
        file = paste0("ssm_tracks/", species, "_ptt_state_space_modelled.RDS"))


# 3. Process GPS tracks

# select only key variables 
gps_tracks <- gps_tracks %>% 
  select(new_id, date, lon, lat, lc) %>% 
  rename(id = new_id) %>%
  arrange(id, date)

# remove segments with fewer than 10 points for aniMotum to work
gps_tracks <- gps_tracks %>%
  group_by(id) %>%
  filter(n() >= 10) %>%
  ungroup()

# remove segments shorter than 5 hours for aniMotum to work
gps_tracks <- gps_tracks %>% 
  group_by(id) %>% 
  filter(difftime(max(date), min(date), units = "hours") >= 5) %>%
  ungroup()

# fit state-space-model
fit_gps <- fit_ssm(gps_tracks,
                   time.step = 1, #time sampling interval in hours
                   vmax = vmax, #speed filter in m/s
                   model="crw")

# reroute tracks around land
#fit_gps <- route_path(fit_gps)

# quality control plots
qcplot(fit = fit_gps, 
       spp = species, 
       df = gps_tracks,
       device_type = "GPS",  
       step_duration = 1)

#grab predicted locations from GPS fit
gps_ssm_tracks <- grab(fit_gps, what = "predicted")

#join device ids with individual ids
individuals <- gps_tracks %>%
  mutate(new_id = gsub("_\\d+$", "", id)) %>%
  mutate(individual_id = gsub("!!.*", "", new_id),
         device_id = gsub(".*!!", "", new_id)) %>%
  select(id, individual_id, device_id) %>%
  distinct()
gps_ssm_tracks <- gps_ssm_tracks %>% left_join(individuals)

#select important columns and standardise names
gps_ssm_tracks <- gps_ssm_tracks %>% 
  ungroup() %>%
  select(individual_id, device_id, date, lon, lat, x.se, y.se) %>%
  rename(lon_se_km = x.se, lat_se_km = y.se)

#export
saveRDS(gps_ssm_tracks,
        file = paste0("ssm_tracks/", species, "_gps_state_space_modelled.RDS"))
