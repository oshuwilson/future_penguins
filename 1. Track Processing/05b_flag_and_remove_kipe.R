# code to flag individuals during quality control
# remove tracks and flag metadata where 2 or more users have flagged the same individual

# cleanup
rm(list=ls())

# load libraries
library(tidyverse)

##------------------##
## User Input Start ##
##------------------##

# 1. Define Key Variables
species_code <- "KIPE" # four letter RAATD species code

##----------------##
## User Input End ##
##----------------##

# 2. Read in dataframes

# set working directory
setwd("~/OneDrive - University of Southampton/Documents/PenguinTrack")

# load metadata for this species
meta <- readRDS("meta/working_metadata.RDS")
sp_meta <- meta %>% filter(abbreviated_name == species_code)

# read in state-space-modelled tracks
gps_tracks <- readRDS(paste0("ssm_tracks/", species_code, "_gps_state_space_modelled.RDS"))
ptt_tracks <- readRDS(paste0("ssm_tracks/", species_code, "_ptt_state_space_modelled.RDS"))
tracks <- bind_rows(gps_tracks, ptt_tracks)

##------------------##
## User Input Start ##
##------------------##

# 3. Flag individuals with erroneous tracks

# define individual IDs flagged for removal
ind_flag <- c("BAS_SG_KIPE_3b", "BAS_SG_KIPE_H1", "BAS_SG_KIPE_H11", "BAS_SG_KIPE_H2",
              "BAS_SG_KIPE_GPS_C10", "BAS_SG_KIPE_GPS_C7", "BAS_SG_KIPE_GPS_P5")

# define corresponding device IDs flagged for removal - MUST BE SAME ORDER AS INDIVIDUAL IDs
dev_flag <- c("769_3b_11448", "769_H1_11463", "769_H11_11464", "769_H2_11465",
              "776_C10_11183", "776_C7_11187", "777_P5_11200")

##----------------##
## User Input End ##
##----------------##

# combine individual and device flag columns
flagged <- data.frame(individual = ind_flag, device = dev_flag)

# create merged column to ensure duplicated individuals or devices aren't also removed
flagged <- flagged %>% 
  mutate(combined_IDs = paste(individual, device, sep = "_"))

# check that combined_IDs match those in the metadata and identify any that don't
sp_meta <- sp_meta %>% 
  mutate(combined_IDs = as.factor(paste(individual_id, device_id, sep = "_")))

if(!all(flagged$combined_IDs %in% sp_meta$combined_IDs)){
  error <- flagged %>% 
    filter(!combined_IDs %in% sp_meta$combined_IDs) %>%
    select(combined_IDs) 
  print(paste0("The following combined IDs were not found in the metadata: ", error))
  print("Please check for spelling mistakes in ind_flag or dev_flag")
  print("Also check that individual and device IDs have been written in the same order")
  stop("Read above messages")
}

# create dataframe of flagged IDs
flagged <- flagged %>%
  select(combined_IDs) %>%
  mutate(combined_IDs = as.factor(combined_IDs))


# 4. Remove flagged individuals from tracks and flag in metadata

# extract a vector of combined_IDs from flag_IDs where count >= 2
removal_IDs <- flagged %>%
  select(combined_IDs) %>%
  mutate(combined_IDs = as.character(combined_IDs)) %>%
  as.vector() %>%
  unlist()

# create combined_IDs column in tracks
tracks <- tracks %>% 
  mutate(combined_IDs = as.factor(paste(individual_id, device_id, sep = "_")))

# remove tracks from individuals that have been flagged
tracks <- tracks %>% 
  filter(!combined_IDs %in% removal_IDs)

# identify remaining IDs
remaining_IDs <- tracks %>%
  mutate(combined_IDs = as.character(combined_IDs)) %>%
  select(combined_IDs) %>%
  distinct() %>%
  pull(combined_IDs)

# change final keepornot column
sp_meta <- sp_meta %>% 
  mutate(keepornot = if_else(combined_IDs %in% removal_IDs, "discard", keepornot)) %>%
  mutate(keepornot = ifelse(is.na(keepornot), "keep", keepornot))


# remove combined_IDs column from tracks and metadata
tracks <- tracks %>% 
  select(-combined_IDs)
sp_meta <- sp_meta %>% 
  select(-combined_IDs)


# 5. Export dataframes

# export tracks 
saveRDS(tracks, file = paste0("ssm_tracks/", species_code, "_ssm_qc.RDS"))

#replace keepornot column in meta with that from sp_meta
meta <- meta %>%
  filter(abbreviated_name != species_code) %>%
  mutate(keepornot = as.character(keepornot)) %>%
  bind_rows(sp_meta)

#export updated metadata
saveRDS(meta, 
        file = "meta/working_metadata.RDS")
