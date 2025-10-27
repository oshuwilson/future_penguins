#trimming 
rm(list=ls())

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
  library(ggmap)
  library(CCAMLRGIS)
}

##-------------------------------------##
## User Input Only Required for Step 1 ##
##-------------------------------------##

# 1. Read in standardised tracks and relevant metadata

#set required variables
species_code <- "EMPE"
study_code <- "Kooyman_Cape_Washington"

##----------------##
## User Input End ##
##----------------##

#set working directory to draw raw files from
setwd(paste0("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/", species_code, "/", study_code))

#read in tracks
tracks <- readRDS("standardised_tracks.RDS")

#read in and filter metadata
meta <- readRDS("~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
meta <- meta %>% filter(dataset_identifier == study_code & abbreviated_name == species_code)


# 2. Trim tracks using interactive function

# function will load plots in X11 windows 
# plots display time since deployment against distance from deployment site
# click the start and end point of the section of the track to KEEP
# if over 50 individuals, delete windows once 50 have been opened
# 
# exclude sections where:
# - tags are turned on early
# - animals remain at deployment site for extended period at the start/end
# - location quality or frequency deteriorates towards end of PTT tracks
# - animals remain in a small area for extended period at end of track
#
# returns filtered dataset without trimmed locations

#if time gaps of over 50 days exist, split into trips by assigning new device id
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/trimming_helpers.R")
tracks <- segment_tracks(tracks)

#update metadata if tracks segmented
meta <- update_meta(tracks, meta)

#run track trimming function
source("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Code/functions/track_trim.R")
tracks <- track_trim(tracks, meta)

#visualise
tracks_terra <- vect(tracks, geom = c("lon", "lat"), crs = "epsg:4326")
tracks_terra <- project(tracks_terra, "epsg:6932")

# coastfile from CCAMLRGIS R Package (function load_Coastline())
coast <- vect("~/OneDrive - University of Southampton/Documents/Chapter 03/data/coast_vect.RDS")
crop_coast <- crop(coast, ext(tracks_terra))

ggplot() + geom_spatvector(data = tracks_terra, size = 0.5) +
  geom_spatvector(data = crop_coast) +
  theme_minimal()


# 3. Export

#format df for export
tracks <- tracks %>%
  select(-location_to_keep)

#export
saveRDS(tracks, paste0("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/", species_code, "/", study_code, "/trimmed_tracks.RDS"))

#update metadata for individuals removed
tracks <- tracks %>%
  mutate(individual_id = as.character(individual_id),
         device_id = as.character(device_id)) %>%
  mutate(individual_id = as.factor(individual_id),
         device_id = as.factor(device_id))

discards <- meta %>% 
  filter(!(individual_id %in% tracks$individual_id) & !(device_id %in% tracks$device_id)) %>%
  mutate(keepornot = "discard")

meta <- meta %>% anti_join(discards, by = c("individual_id", "device_id")) %>%
  bind_rows(discards)

all_meta <- readRDS("~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
all_meta <- all_meta %>% 
  filter(dataset_identifier != study_code | abbreviated_name != species_code) %>% 
  bind_rows(meta)
saveRDS(all_meta, "~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
saveRDS(meta, "metadata.RDS")
