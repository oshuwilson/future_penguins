#standardisation

#cleanup
rm(list=ls())

{
  library(data.table)
  library(tidyverse)
  library(terra)
}

##-------------------------------------##
## User Input Only Required for Step 1 ##
##-------------------------------------##

# 1. Read in tracking data and relevant metadata

#set required variables
species_code <- "EMPE" #the 4-letter RAATD code
study_code <- "Kooyman_Cape_Washington" #Dataset identifier
device_type <- "PTT" #either PTT, GLS, or GPS
zonediff <- 0 #timezone difference relative to UTC

##----------------##
## User Input End ##
##----------------##

#set working directory to draw raw files from
setwd(paste0("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/", species_code, "/", study_code, ""))

#list all available files
files <- list.files(pattern = "*.rds") #change pattern depending on filetype

# remove metadata from list
files <- files[!grepl("meta", files)] #remove metadata file if present

#read in tracks
tracks <- readRDS(files)


# 2. Format data for prefiltering

#create lc column for GPS or GLS data
if(device_type == "GPS"){
  tracks$lc <- "G"
}

if(device_type == "GLS"){
  tracks$lc <- "GL"
}

#read in device_id from metadata
meta <- readRDS("~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
meta <- meta %>% filter(dataset_identifier == study_code & abbreviated_name == species_code)

#create device_id if not in tracks
if(!"device_id" %in% colnames(tracks)){
  tracks$device_id <- paste0(tracks$individual_id)
}

#join together - ADAPT FOR MULTIPLE DEVICES PER INDIVIDUAL
tracks <- tracks %>% 
  mutate(individual_id = as.character(individual_id), device_id = as.character(device_id)) %>%
  left_join(select(meta, individual_id, device_id))

#rename and select the six required columns
tracks <- tracks %>%
  rename(date = datetime) %>%
  select(device_id, individual_id, date, lon, lat, lc)

#make individual_id and device_id factors
tracks <- tracks %>% 
  mutate(device_id = as.factor(device_id),
         individual_id = as.factor(individual_id))

#change timezone
tracks$date <- tracks$date - hours(zonediff)

#remove no data values and NAs
no_data_vals <- c(0, "LAT", "LON", NA) #common no_data_vals include 0, LAT, LON
tracks <- tracks %>% 
  filter(!lat %in% no_data_vals & !lon %in% no_data_vals) %>%
  filter(!is.na(lat) & !is.na(lon))


# 3. Filtering
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/standardise_tracks.R")

tracks <- tracks %>% mutate(lat = as.numeric(lat), lon = as.numeric(lon)) 

tracks <- standardise(df = tracks, #tracking dataframe
                      device_type = device_type) #either PTT, GPS, or GLS


# 4. Checks

# functions that run checks
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/standardisation_helpers.R")

# check for temporal overlap in redeployed devices
redeploy_check(meta, tracks)

# check that data isn't missing from February 29th on leap years
leap_check(tracks)


# 5. Export standardised data for trimming

#export standardised data
saveRDS(tracks, paste0("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/", species_code, "/", study_code, "/standardised_tracks.RDS"))

