#formatting RAATD metadata 
#requires an RDS file with standardised column names (datetime, lon, lat, individual_id, device_id, lc)
{
  library(tidyverse)
  library(terra)
  library(tidyterra)
  library(CCAMLRGIS)
  library(hms)
}

# coastfile for visualisations - from rnaturalearth originally
coast <- vect("~/OneDrive - University of Southampton/Documents/Chapter 03/data/coast_vect.RDS")
rm(list=setdiff(ls(), "coast"))

# functions that help with formatting
source("~/OneDrive - University of Southampton/Documents/Chapter 03/code/R/meta_helpers.R")


# 1. Read in and format track dataframe

#set species, study and colony/region code
species_code <- "EMPE"
study_code <- "Kooyman_Cape_Washington"

#check that study hasn't already been processed before beginning
all_meta <- readRDS("~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
if(study_code %in% all_meta$dataset_identifier){
  stop("The metadata for this study has already been processed.")
}

#set working directory to read files from
setwd(paste0("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Data/", species_code, "/", study_code))

#list all available files
files <- list.files(pattern = "*.rds", recursive = T, full.names = T) #change pattern depending on filetype

#read in tracks
tracks <- readRDS(files[1])

# plot tracks
tracks %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  plot(pch = ".")

#change timezone to UTC
zonediff <- 0
tracks$datetime <- tracks$datetime - hours(zonediff)

#cleanup
rm(zonediff)


# 2. Read in Existing Metadata and keep relevant columns
# skip this step if no existing metadata

#read in existing metadata
ext_meta <- readRDS("existing_meta.rds")

#relevant columns often found in metadata: 
# individual_id 
# device_id 
# device_type
# sex
# age_class
# deployment_site
# deployment_day
# deployment_month
# deployment_year
# deployment_time
# deployment_latitude
# deployment_longitude

#keep and rename relevant columns
# ext_meta <- ext_meta %>%
#   mutate(individual_id = paste(Dep_ID, GPS, TDR, "GPS", sep = "_")) %>%
#   mutate(individual_id = gsub(" ", "_", individual_id)) %>%
#   mutate(device_id = individual_id) %>%
#   mutate(sex = "unknown") %>%
#   select(individual_id, device_id, sex)



# 3. Create dataframe for individual study

### device_id ###
# use individual_id if not in existing metadata
if(!exists("device_id", where = tracks)){
  tracks <- tracks %>%
    mutate(device_id = individual_id)
}

### group by individual_id and device_id, and append study_code and species_code ###
meta <- tracks %>% group_by(individual_id, device_id) %>%
  summarise(dataset_identifier = study_code, abbreviated_name = species_code) %>%
  ungroup()

### join existing metadata ###
meta <- meta %>% 
  left_join(ext_meta, by = c("individual_id", "device_id"))

### scientific_name, common_name ### 
meta <- get_names(meta, species_code)

### additional fields ###
# add all manually
meta <- meta %>%
  mutate(
    sex = "unknown", #either male, female, or unknown
    age_class = "adult", #either adult, juvenile, or unknown
    device_type = "PTT", #either GPS, PTT, or GLS
    deployment_site = "Cape Washington, Ross Sea", # name of colony and region
    deployment_decimal_longitude = 165.393, # lon of colony - use NA if different for different individuals
    deployment_decimal_latitude = -74.643, # lat of colony - use NA if different for different individuals
    data_contact = "Kimberley Goetz", # contact who provided the data
    contact_email = "kim.goetz@noaa.gov", # email of contact
    file_name = files, # name of file, should be files by default
    keepornot = NA # leave as NA
  )

# get deployment info in case dates or lat/lons are missing
deployments <- get_deployments(tracks, coast)  

# if dates missing, use deployment info
if(!exists("deployment_time", where = meta)){
  meta <- meta %>%
    left_join(
      select(deployments, individual_id, device_id, deployment_year, deployment_month, deployment_day, deployment_time)
    )
}

# if all lats and lons are NA, use deployment info
if(any(is.na(meta$deployment_decimal_longitude)) == T){
  meta <- meta %>%
    select(-deployment_decimal_longitude, -deployment_decimal_latitude) %>%
    left_join(
      select(deployments, individual_id, device_id, deployment_decimal_longitude, deployment_decimal_latitude)
    )
}


# 4. Join with existing metadata

# run checks 
meta <- meta_check(meta, all_meta)

#join together
all_meta <- all_meta %>% bind_rows(meta) %>%
  arrange(abbreviated_name, dataset_identifier, individual_id) %>%
  distinct(abbreviated_name, dataset_identifier, individual_id, device_id, .keep_all = TRUE)

#save updated metadata
saveRDS(all_meta,
        file = "~/OneDrive - University of Southampton/Documents/PenguinTrack/meta/working_metadata.RDS")
saveRDS(meta, file = "metadata.RDS")
