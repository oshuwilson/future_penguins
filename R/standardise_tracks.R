##' Standardise
##'
##' Prepare standardised track data for state-space filtering by performing
##' further standardisation and culling unacceptably short deployments
##'
##' @title Standardise
##' @param df tracking data dataframe with standardised column names
##' @param min_obs minimum number of observation records a deployment must have
##' @param min_days minimum number days a deployment must last
##' @param device_type either GPS, GLS or PTT
##'
##' @import data.table
##' @import tidyverse
##' 
##' @export

standardise <- function(df, device_type){
  
  if(is.null(df)) stop("An input data frame must be supplied")
  if(is.null(device_type)) stop("A device type (GPS, GLS, or PTT) must be supplied")
  
  #arrange by individual_id, device_id, and date
  sub1 <- tracks %>% 
    arrange(individual_id, device_id, date) #arrange by date
  
  #remove points within 1 second of preceding point
  sub1 <- sub1 %>% 
      group_by(individual_id, device_id) %>% 
      mutate(timegap = difftime(date, lag(date), unit = "secs")) %>%
      filter(timegap > 0.99) %>%
      select(-timegap)
  
  #if GPS, remove points with identical lat/lon as preceding point
  if(device_type == "GPS"){
  sub1 <- sub1 %>% 
    mutate(lag_lat = lag(lat), 
           lag_lon = lag(lon)) %>%
    mutate(lat_diff = lat - lag_lat,
           lon_diff = lon - lag_lon) %>%
    filter(lat_diff != 0 | lon_diff != 0) %>%
    select(-lat_diff, -lon_diff, -lag_lat, -lag_lon)
  }
  
  #if Argos, remove points with difference under 0.00001 and same location class
  if(device_type == "PTT"){
    sub1 <- sub1 %>% 
      mutate(lag_lat = lag(lat), 
             lag_lon = lag(lon),
             lag_lc = lag(lc)) %>%
      mutate(lat_diff = lat - lag_lat,
             lon_diff = lon - lag_lon) 
    removals <- sub1 %>%
      filter(abs(lat_diff) < 0.00001 & lc == lag_lc & abs(lon_diff) < 0.00001)
    
    #remove rows from sub1 that match removals
    sub1 <- sub1 %>% 
      anti_join(removals) %>%
      select(-lat_diff, -lon_diff, -lag_lat, -lag_lon, -lag_lc)
    
    rm(removals)
  }
  
  #print number of points removed in this step
  diff_1 <- nrow(tracks) - nrow(sub1)
  cat(paste0("\n duplicate points removed: ", diff_1))
  
  #remove locations in N hemisphere, eg. when tags turned on by manufacturer
  sub2 <- sub1 %>% 
    filter(lat < 0)
  
  #print number of points removed in this step
  diff_2 <- nrow(sub1) - nrow(sub2)
  cat(paste0("\n points from northern hemisphere removed: ", diff_2))
  
  #transform lon from 0//360 to -180//180
  sub3 <- sub2 %>% 
    mutate(lon = (lon + 180) %% 360 - 180)
  
  #remove any NAs in individual_id or date and assign device_type
  sub4 <- sub3 %>% 
    ungroup() %>%
    mutate(device_type = device_type) %>%
    filter(!is.na(individual_id) & !is.na(date))
  
  #print number of points removed in this step
  diff_3 <- nrow(sub2) - nrow(sub4)
  cat(paste0("\n points with missing information removed: ", diff_3))
  
  #return processed data
  sub4
  
}