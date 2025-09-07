##' Prefilter
##'
##' Prepare standardised track data for state-space filtering by performing
##' further standardisation and culling unacceptably short deployments
##'
##' @title Prefilter
##' @param df tracking data dataframe with standardised column names
##' @param device_type either GPS, GLS or PTT
##'
##' @import data.table
##' @import tidyverse
##' 
##' @export

prefilter <- function(df, device_type){
  
  if(is.null(df)) stop("An input data frame must be supplied")
  if(is.null(device_type)) stop("A device type (GPS, GLS, or PTT) must be supplied")
  
  #remove duplicate records
  sub1 <- tracks %>% 
    group_by(individual_id) %>% 
    distinct(date, .keep_all = T)
  
  #print number of points removed in this step
  diff_1 <- nrow(tracks) - nrow(sub1)
  cat(paste0("\n duplicate records removed: ", diff_1))
  
  #remove deployments with fewer than 20 locations
  sub2 <- sub1 %>% 
    filter(n() >= 20)
  
  #print number of individuals removed in this step
  diff_2 <- n_groups(sub1) - n_groups(sub2)
  cat(paste0("\n individuals with fewer than 20 points removed: ", diff_2))
  
  #return filtered data
  sub2 <- sub2 %>% 
    ungroup() %>%
    mutate(device_type = device_type)
  return(sub2)
}

#-------------------------------------------------------------------------------

# argos_fix - reassigns erroneous argos lc values

argos_fix <- function(tracks){
  
  #deal with common PTT lc alternative designations
  if(device_type == "PTT"){
    tracks <- tracks %>% 
      mutate(lc = ifelse(lc == "-9", "Z", lc)) %>%
      mutate(lc = ifelse(lc == "-3", "Z", lc)) %>%
      mutate(lc = ifelse(lc == "-2", "B", lc)) %>%
      mutate(lc = ifelse(lc == "-1", "A", lc))
  }
  
  #if lc unknown assign as "Z"
  err <- tracks %>% filter(lc == "" | is.na(lc)) %>% mutate(lc="Z")
  tracks <- tracks %>% 
    anti_join(err, by = c("individual_id", "device_id", "date", "lon", "lat")) %>%
    bind_rows(err) %>%
    arrange(individual_id, device_id, date)
  
  #validate lc levels against list of acceptable levels
  lc_levels <- c("G", "GL", "3", "2", "1", "0", "A", "B", "Z")
  lc_error <- tracks %>% filter(!lc %in% lc_levels) 
  
  #if prints "Inspect LC" there are lc levels that won't work in state-space modelling
  if(nrow(lc_error) == 0) {
    print("Proceed")
  } else {
    print("Inspect LC")
  }
  
  #return tracks
  return(tracks)
  
}