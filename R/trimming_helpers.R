# helper functions for track trimming

#-------------------------------------------------------------------------------

# segment_tracks - splits up tracks into segments when gaps > 50 days exist

segment_tracks <- function(tracks){

  tracks <- tracks %>%
    group_by(individual_id, device_id) %>%
    arrange(individual_id, device_id, date) %>%
    mutate(lagtime = lag(date)) %>%
    mutate(timediff = difftime(date, lagtime, units = "days")) %>%
    mutate(trip = ifelse(timediff > 50, 1, 0)) %>%
    mutate(trip = ifelse(is.na(trip), 0, trip)) %>%
    mutate(trip = cumsum(trip)) %>%
    mutate(device_id = if_else(trip != 0, paste0(device_id, "_", trip), device_id)) %>%
    select(-lagtime, -timediff, -trip) %>%
    ungroup()
  
  return(tracks)
}

#-------------------------------------------------------------------------------

# update_meta - add new track IDs to metadata as device_ids

update_meta <- function(tracks, meta){

#add new device_ids to metadata
new_devs <- levels(as.factor(tracks$device_id))
new_devs <- subset(new_devs, !new_devs %in% levels(as.factor(meta$device_id)))

# update metadata with new device_ids
if(length(new_devs) > 1){
  new_meta <- tracks %>%
    filter(device_id %in% new_devs) %>%
    group_by(individual_id, device_id) %>%
    summarise(deployment_date = first(date),
              deployment_decimal_latitude = first(lat),
              deployment_decimal_longitude = first(lon)) %>%
    mutate(deployment_year = year(deployment_date),
           deployment_month = month(deployment_date),
           deployment_day = day(deployment_date),
           deployment_time = as_hms(deployment_date)) %>%
    mutate(deployment_time = as.character(deployment_time)) %>%
    select(-deployment_date) %>% 
    left_join(select(meta, -device_id, -deployment_year, -deployment_month,
                     -deployment_day, -deployment_time, -deployment_decimal_latitude,
                     -deployment_decimal_longitude), 
              by = "individual_id")
  meta <- bind_rows(meta, new_meta)
}

return(meta)

}