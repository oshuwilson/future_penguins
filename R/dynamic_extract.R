#functions used in dynamic extraction

#--------------------------------------------
# 1. dynamic_extract - for general use

#extracts variables to points based on the day and year of that point
#requires terra, tidyterra, dplyr, and lubridate
#tracks must have a datetime column called date and be a SpatVector

dynamic_extract <- function(predictor, tracks, crop=TRUE){
  
  #first create a list of the years within the tracks
  tracks$year <- as.factor(year(tracks$date))
  years <- levels(tracks$year)
  
  #empty variable for loop to feed into
  tracks_extracted <- NULL
  
  #for loop by year - bind = TRUE
  for(z in years){
    trax <- filter(tracks, year==z)
    try(pred <- rast(paste0("E:/Satellite_Data/daily/", predictor, "/", predictor, "_", z, ".nc")))
    #if no predictor exists for that year, assign all values as NA
    if(!exists("pred")){
      xtractions <- as.data.frame(trax, geom = "XY")
      xtractions[predictor] <- NA
      xtractions$yday <- NA
      tracks_extracted <- rbind(tracks_extracted, xtractions)
      next
    }
    
    #remove extra slice for front_freq
    if(predictor == "front_freq"){
      pred <- pred[[-1]]
    }
    
    #crop predictor to extent of tracks
    if(crop == TRUE){
      e <- ext(trax) + c(0.5,0.5,0.5,0.5) #create SpatExtent for cropping raster
      pred_crop <- crop(pred, e) #crop to increase speed
    } else {
      pred_crop <- pred
    }
    
    #get days of the year that tracks are present for
    trax$yday <- yday(trax$date) #extract all yday numbers from data
    ydays <- unique(trax$yday) #different levels of ydays
    
    #get days of the year that predictor is present for
    pred_ydays <- yday(time(pred_crop))
    
    #get list of days where tracks are present but there is no predictor data
    missing_ydays <- setdiff(ydays, pred_ydays)
    
    #create empty list for next loop to feed into
    xtractions <- NULL 
    
    #for loop by yday
    for(i in ydays){
      points <- filter(trax, yday==i) #subsets by yday
      
      #if yday is not missing in predictor data
      if(!i %in% missing_ydays){
        yday.date <- as_date(first(points$date)) #extracts date for yday
        slice <- pred_crop[[time(pred_crop) == yday.date]] #slices raster by yday
        xtracted <- extract(slice, points, ID=F, bind=T) #extract values from slice
        xtracted_df <- as.data.frame(xtracted, geom="XY") #create dataframe for binding
        names(xtracted_df)[length(names(xtracted_df))-2] <- predictor #rename column to predictor name
        xtractions <- rbind(xtractions, xtracted_df) #bind with previous ydays
      } else {
        xtracted_df <- as.data.frame(points, geom = "XY") # create dataframe for binding
        xtracted_df[predictor] <- NA #assign NA values where predictor information is missing
        xtractions <- rbind(xtractions, xtracted_df) #bind with previous ydays
      }
    }
    
    tracks_extracted <- rbind(tracks_extracted, xtractions) #bind together all years
    rm(pred) #remove pred to maintain next statement
  }
  
  #remove yday column for next predictor to work
  tracks_extracted <- dplyr::select(tracks_extracted, -yday, -year)
  
  #reformat into SpatVector
  tracks_extracted <- vect(tracks_extracted,
                           geom=c("x", "y"),
                           crs=crs(tracks))
  
  return(tracks_extracted)
  
}
