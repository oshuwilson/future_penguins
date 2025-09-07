#-----------------------------------------------
# Format SeabirdTracking Data with right Columns
#-----------------------------------------------


rm(list=ls())

# set working directory to folder with data
setwd("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/GEPE/NMU_GEPE_Marion_2018")

{
  library(tidyverse)
  library(tidyterra)
}

# list SeabirdTracking file
file <- list.files(pattern = "*.csv")

# read in tracks
tracks <- read.csv(file)

# rename columns of tracks for processing
tracks <- tracks %>%
  rename(individual_id = bird_id,
         device_id = track_id,
         lat = latitude,
         lon = longitude,
         lc = argos_quality) %>%
  mutate(datetime = paste(date_gmt, time_gmt))

# format datetime as POSIXct
tracks <- tracks %>%
  mutate(datetime = parse_date_time(datetime, orders = c("Ymd HMS", "dmY HMS")))

# create metadata
meta <- tracks %>% 
  group_by(individual_id, device_id) %>%
  summarise(common_name = first(common_name),
            scientific_name = first(scientific_name),
            age_class = first(age),
            sex = first(sex),
            colony_name = first(colony_name),
            site_name = first(site_name),
            deployment_decimal_latitude = first(lat_colony),
            deployment_decimal_longitude = first(lon_colony),
            device_type = first(device))

# merge colony and site name
meta <- meta %>%
  mutate(deployment_site = paste(colony_name, site_name, sep = ", ")) %>%
  select(-colony_name, -site_name) %>%
  ungroup()

# limit tracks to key columns
tracks <- tracks %>%
  select(individual_id, device_id, datetime, lat, lon, lc)

# export
write.csv(meta, "existing_meta.csv", row.names = F)
saveRDS(tracks, "seabirdtracking_tracks.rds")
