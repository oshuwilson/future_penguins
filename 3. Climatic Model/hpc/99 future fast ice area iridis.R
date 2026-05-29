#-------------------------------------------------------------------------------
# Repeat for Future Layers
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/")

library(lubridate)
library(dplyr)
library(readr)
library(terra)
library(tidyterra)
library(doParallel)
library(foreach)

# define scenario
for(scenario in c("ssp585")){
  print(scenario)
  
  # read in Andrew Einhorn's fast ice ratios
  ratios <- read_csv("penguins/data/pack_ice_ratios/PI_FI_ratio_10_deg_19_May_mean.csv")
  
  # pivot longer
  ratios <- ratios %>% 
    pivot_longer(cols = -...1,
                 names_to = "lat_band",
                 values_to = "ratio") %>%
    rename(month = ...1)
  
  # list of all GCMs
  gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
            "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")
  
  # register parallelisation
  registerDoParallel(cores = 36)
  
  # loop over each gcm in parallel
  foreach(z = 1:8) %dopar% {
    
    # define gcm
    gcm <- gcms[z]
    print(gcm)
    
    # read in monthly SIC (transformed)
    sic <- rast(paste0("Satellite_Data_CMIP/siconc/", gcm, "_", scenario, "_glorysres.tif"))
    
    # create sequence of months according to original time info
    allyears <- 2000:2020
    allmonths <- 1:12
    month_seq <- unlist(lapply(allmonths, function(m) {
      as.Date(paste(allyears, m, "01", sep = "-"))
    }))
    month_seq <- as_date(month_seq)
    
    # append to raster
    time(sic) <- month_seq
    
    # for each month
    for(this_month in c(5, 6, 7, 8, 9, 10, 11, 12, 1)){
      print(paste(this_month, gcm))
      
      # get sea ice ratios for this month
      month_ratios <- ratios %>%
        filter(month == this_month)
      
      # limit sea ice concentration to that month
      sic_monthly <- sic[[month(time(sic)) == this_month]]
      
      # loop over each layer
      for(i in 1:nlyr(sic_monthly)){
        this_layer <- sic_monthly[[i]]
        
        # get year 
        this_year <- year(time(this_layer))
        
        # loop over each latitude band
        for(lat_bin in seq(-180, 170, 10)){
          
          # get ratio for this band
          this_ratio <- month_ratios %>%
            filter(lat_band == as.character(lat_bin)) %>%
            pull(ratio)
          
          # create extent for this latitude band
          lat_extent <- ext(lat_bin, lat_bin + 10, -90, -40)
          
          # crop to this extent
          this_crop <- crop(this_layer, lat_extent)
          
          # compute area of each cell
          area_rast <- cellSize(this_crop, unit = "km")
          
          # multiply area by sea ice concentration to get sea ice area per cell
          this_crop <- this_crop * area_rast
          
          # calculate area of sea ice in this band
          band_area <- global(this_crop, sum, na.rm = TRUE) %>% pull(sum)
          
          # divide by ratio to get fast ice area
          band_fast_ice_area <- band_area / this_ratio
          
          # create data frame
          fast_ice_df <- data.frame(
            month = this_month,
            year = this_year,
            lat_band = lat_bin,
            fast_ice_area = band_fast_ice_area)
          
          # bind to other data for this layer
          if(lat_bin == -180){
            layer_fast_ice <- fast_ice_df
          } else {
            layer_fast_ice <- bind_rows(layer_fast_ice, fast_ice_df)
          }
        }
        
        # join to all data for this month
        if(i == 1){
          month_fast_ice <- layer_fast_ice
        } else {
          month_fast_ice <- bind_rows(month_fast_ice, layer_fast_ice)
        }
        
        # if last run, calculate average over years
        if(i == nlyr(sic_monthly)){
          month_fast_ice <- month_fast_ice %>%
            group_by(month, lat_band) %>%
            summarise(mean_fast_ice_area = mean(fast_ice_area, na.rm = TRUE)) %>%
            ungroup()
        }
        
      }
      
      # join to all months
      if(this_month == 5){
        all_fast_ice <- month_fast_ice
      } else {
        all_fast_ice <- bind_rows(all_fast_ice, month_fast_ice)
      }
    }
    
    # average all fast ice bands over months
    fast_ice_clim <- all_fast_ice %>%
      group_by(lat_band) %>%
      summarise(mean_fast_ice = mean(mean_fast_ice_area, na.rm = TRUE),
                min_fast_ice = min(mean_fast_ice_area, na.rm = T),
                max_fast_ice = max(mean_fast_ice_area, na.rm = T)) %>%
      ungroup()
    
    # read in climate variable for raster dims
    temp <- rast("Satellite_Data_CMIP/tas/ACCESS-ESM1-5_ssp126_glorysres.tif")
    
    # create raster of bands
    r <- rast(extent = ext(temp),
              resolution = res(temp),
              crs = crs(temp))
    values(r) <- NA
    
    # loop over each latitude band and assign fast ice area
    for(lat_bin in seq(-180, 170, 10)){
      print(lat_bin)
      
      # create extent for this latitude band
      lat_extent <- ext(lat_bin, lat_bin + 10, -90, -30)
      
      # crop raster to this extent
      r_crop <- crop(r, lat_extent)
      
      # get fast ice area for this band
      this_fast_ice <- fast_ice_clim %>%
        filter(lat_band == lat_bin) %>%
        pull(min_fast_ice)
      
      # assign fast ice area to all cells in this band
      values(r_crop) <- this_fast_ice
      
      # merge back to full raster
      r <- mosaic(r, r_crop, fun = "mean")
    }
    
    # export raster
    writeRaster(r, paste0("Satellite_Data_CMIP/deltas/fast_ice_area/", gcm, "_", scenario, "_emp_future.tif"), overwrite = TRUE)
  }
}