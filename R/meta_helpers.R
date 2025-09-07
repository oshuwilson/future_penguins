# functions to help with metadata processing

#-----------------------------------------------------------------------------------------

# get_names - retrieves the associated common and latin name for each species code

get_names <- function(meta, species_code){
  
  #import list of names
  names <- read.csv("~/OneDrive - University of Southampton/Documents/RAATD 2.0/Metadata/species_codes.csv")
  
  #filter to this species
  names <- filter(names, abbreviated_name == species_code)
  
  #assign common and scientific name from this
  meta$scientific_name <- names$scientific_name
  meta$common_name <- names$common_name
  
  #return meta
  return(meta)
}


#-----------------------------------------------------------------------------------------

# get_deployments - retrieves a database of the first location and time of each track
# also plots a map of deployment locations for quality control

get_deployments <- function(tracks, coast){
  
  # eliminate error track values and ensure lat/lon is numeric
  tracks <- tracks %>% filter(lat != 0 & lon != 0 & lat != "LAT" & lon != "LON")
  tracks <- tracks %>% mutate(lat = as.numeric(lat), lon = as.numeric(lon))
  
  # get first track location for each individual
  deployments <- tracks %>% 
    group_by(individual_id, device_id) %>% 
    drop_na(all_of(c("lat", "lon"))) %>%
    summarise(start_time = first(datetime),
              start_lat = first(lat),
              start_lon = first(lon)) %>%
    ungroup()
  
  #visualise to check
  deps <- vect(deployments, geom = c("start_lon", "start_lat"), crs = "EPSG:4326")
  deps <- project(deps, "EPSG:6932")
  e <- ext(deps) + c(10000, 10000, 10000, 10000)
  crop_coast <- crop(coast, e)
  plot(crop_coast)
  plot(deps, add = T)
  
  #extract deployment time and date
  deployments <- deployments %>% 
    mutate(deployment_year = year(start_time),
           deployment_month = month(start_time),
           deployment_day = day(start_time),
           deployment_time = as_hms(start_time))
  
  #format columns for metadata
  deployments <- deployments %>% 
    rename(deployment_decimal_latitude = start_lat,
           deployment_decimal_longitude = start_lon) %>%
    select(-start_time)
  
  # return deployment df
  return(deployments)
}

#-----------------------------------------------------------------------------------------

# meta_check - checks metadata for potential issues
# - checks that sex, age, and device_type use valid names
# - selects only relevant columns
# - checks for repeated individual ID codes in metadata and tracks
# - ensures that individual_id, device_id, deployment_time are characters for binding
# - checks for NAs

meta_check <- function(meta, all_meta){
  
  #check that sex is either male, female, or NA
  poss_sexes <- c("male", "female", "unknown")
  invalid <- meta %>%
    filter(!sex %in% poss_sexes)
  if(nrow(invalid) > 0){
    print(invalid)
    stop("sex must be male, female or unknown. Please rename others.")
  }
  
  #check that age_class is either adult, juvenile, or unknown
  poss_ages <- c("adult", "juvenile", "unknown")
  invalid <- meta %>%
    filter(!age_class %in% poss_ages)
  if(nrow(invalid) > 0){
    print(invalid)
    stop("age_class must be adult, juvenile or unknown. Please rename others.")
  }
  
  #check that device_type has been saved as GPS, PTT, or GLS
  poss_devices <- c("GPS", "PTT", "GLS")
  invalid <- meta %>%
    filter(!device_type %in% poss_devices)
  if(nrow(invalid) > 0){
    print(invalid)
    stop("device_type must be GPS, PTT, or GLS. Please rename others.")
  }
  
  # select relevant columns
  meta <- meta %>% select(dataset_identifier, file_name, individual_id, keepornot, scientific_name, 
                          common_name, abbreviated_name, sex, age_class, device_id, device_type, 
                          deployment_site, deployment_year, deployment_month, deployment_day,
                          deployment_time, deployment_decimal_longitude, deployment_decimal_latitude,
                          data_contact, contact_email)
  
  #check whether there are duplicate individual ID names before joining dataset
  inds <- meta %>% 
    mutate(individual_id = as.character(individual_id)) %>%
    pull(individual_id)
  dups <- all_meta %>% 
    filter(individual_id %in% inds)
  if(nrow(dups) > 0){
    print(dups)
    stop("Duplicate IDs found in metadata (see df) - rename individual_id codes in tracks and metadata.")
  }
  
  # ensure correct classes are used
  meta <- meta %>%
    mutate(device_id = as.character(device_id),
           individual_id = as.character(individual_id),
           deployment_time = as.character(deployment_time))
  
  # check for NA values
  na_check <- colSums(is.na(meta %>% select(-keepornot)))
  if(sum(na_check) > 0){
    print(na_check)
    stop("NAs found in metadata (see df)")
  }
  
  # return meta
  return(meta)
  
}