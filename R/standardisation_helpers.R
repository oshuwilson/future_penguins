# functions to check track data during standardisation

#-------------------------------------------------------------------------------

# redeploy_check - check that redeployed device_ids don't have temporal overlap

redeploy_check <- function(meta, tracks){
  
  #identify redeployed devices
  redeployments <- meta %>%
    group_by(device_id) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>%
    pull(device_id)
  
  #extract first and last datetime for each device deployment
  for(i in redeployments){
    #identify individual ids corresponding to this device id
    individual_ids <- meta %>% 
      mutate(individual_id = as.character(individual_id),
             device_id = as.character(device_id)) %>%
      filter(device_id == i) %>%
      pull(individual_id)
    
    #extract first and last datetime for each individual id
    for(j in individual_ids){
      #extract track data for that individual
      track <- tracks %>%
        filter(individual_id == j & device_id == i)
      
      #extract first and last datetime for that individual
      first <- track %>% 
        slice_head(n = 1) %>%
        pull(date)
      last <- track %>%
        slice_tail(n = 1) %>%
        pull(date)
      
      #create dataframe with first and last datetime
      if(j == individual_ids[1]){
        deployment_dates <- data.frame(individual_id = j,
                                       device_id = i,
                                       first = first,
                                       last = last)
      } else {
        deployment_dates <- deployment_dates %>% 
          bind_rows(data.frame(individual_id = j,
                               device_id = i,
                               first = first,
                               last = last))
      }
      
      #cleanup
      rm(track, first, last)
    }
    
    #check for overlap in deployment dates
    for(k in 1:nrow(deployment_dates)){
      
      #extract date range for deployment
      deployment_range <- c(deployment_dates$first[k], deployment_dates$last[k])
      
      #check whether other start or end dates fall into this range
      overlap <- deployment_dates %>%
        filter(individual_id != deployment_dates$individual_id[k]) %>%
        mutate(overlap = ifelse(first >= deployment_range[1] & first <= deployment_range[2] | 
                                  last >= deployment_range[1] & last <= deployment_range[2], 
                                1, 0))
      
      #if there is overlap in any track, stop and warn user
      if(sum(overlap$overlap) > 0){
        stop(paste("There is temporal overlap of device_id", i, "for individual", deployment_dates$individual_id[k], 
                   "and individual(s)", paste(overlap$individual_id[overlap$overlap == 1], collapse = ", "), 
                   "\n Please check and amend these tracks before proceeding"))
      }
      
      #cleanup
      rm(deployment_range, overlap)
    }
    
  }
  
  # print successful completion
  print("no overlap in redeployed devices found")
  
}

#-------------------------------------------------------------------------------

# leap_check - check for data missing on Feb 29th during leap years

leap_check <- function(tracks){

#extract tracks from leap years
leap_tracks <- tracks %>%
  filter(leap_year(date) == TRUE)

#extract data from Feb 28th, 29th, and March 1st
leap_dates <- leap_tracks %>%
  filter(month(date) == 2 & day(date) %in% c(28, 29) | month(date) == 3 & day(date) == 1)

#subset to each date
feb28 <- filter(leap_tracks, yday(date) == 59)
feb29 <- filter(leap_tracks, yday(date) == 60)
mar01 <- filter(leap_tracks, yday(date) == 61)

#if there is no data on Feb 29th but data on Feb 28th and March 1st, stop and warn user
if(nrow(feb29) == 0 & nrow(feb28) > 0 & nrow(mar01) > 0){
  stop("There is no data on February 29th for leap years. Please check these tracks before proceeding")
}

#print successful completion
print("no missing data for Feb 29th in leap years")

}