#filtering

#cleanup
rm(list=ls())

{
  library(tidyverse)
  library(data.table)
  library(terra)
  library(tidyterra)
  library(patchwork)
}

##-------------------------------------##
## User Input Required in Step 1 and 3 ##
##-------------------------------------##

# 1. Read in standardised tracks and relevant metadata

#set required variables
species_code <- "ADPE" #the 4-letter RAATD code
study_code <- "NSF_Palmer_ADPE" #Dataset identifier
device_type <- "GPS" #either GPS, PTT, or GLS

##----------------##
## User Input End ##
##----------------##

#set working directory to draw raw files from
setwd(paste0("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/", species_code, "/", study_code))

#read in tracks
tracks <- readRDS("trimmed_tracks.RDS")

#read in and filter metadata
all_meta <- readRDS("~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
meta <- all_meta %>% filter(dataset_identifier == study_code & abbreviated_name == species_code)


# 2. Prefilter

#source functions
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/prefilter.R")

#run prefiltering
tracks <- prefilter(tracks, device_type = device_type)

#run argos fix
tracks <- argos_fix(tracks)


# 3. Export

#import existing filtered tracks for this species and merge if applicable
try({
  spp_tracks <- readRDS(paste0("~/OneDrive - University of Southampton/Documents/PenguinTrack/newdata/", species_code, "_filtered_tracks.RDS"))
  tracks <- bind_rows(spp_tracks, tracks)
  tracks <- tracks %>% distinct(.keep_all = T) #check that no repeats are included
})

#export all tracks for this species
saveRDS(tracks, file = paste0("~/OneDrive - University of Southampton/Documents/PenguinTrack/newdata/", species_code, "_filtered_tracks.RDS"))

