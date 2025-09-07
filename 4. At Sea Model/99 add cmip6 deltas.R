#-----------------------------------------
# Add deltas to GLORYS data
#-----------------------------------------

rm(list=ls())
setwd("E://")

library(terra)
library(lubridate)

#define CMIP6 GCM
gcm <- "NorESM2-MM"

# create table of cmip and satellite variable names
cmip_vars <- c("zos", "siconc", "tos", "mlotst", "sos", "curr")
satellite_vars <- c("ssh", "sic", "sst", "mld", "sal", "curr")

# loop over each variable
for(z in 1:length(cmip_vars)){
  
  # reset
  rm(list = setdiff(ls(), c("z", "gcm", "cmip_vars", "satellite_vars")))
  
  # set var names
  cmip_var <- cmip_vars[z]
  satellite_var <- satellite_vars[z]
  
  # 1. Setup
  
  #read in mean ssp126 and ssp585 delta for this variable
  delta_126 <- rast(paste0("cmip6_data/CMIP6/deltas/", cmip_var, "/ssp126/", gcm, "_ssp126_", cmip_var, "_delta.nc"))
  delta_585 <- rast(paste0("cmip6_data/CMIP6/deltas/", cmip_var, "/ssp585/", gcm, "_ssp585_", cmip_var, "_delta.nc"))
  
  #read in monthly satellite data
  all_satellite <- rast(paste0("Satellite_Data/monthly/", satellite_var, "/", satellite_var, ".nc"))
  
  # define CRS of satellite data
  crs(all_satellite) <- "EPSG:4326"
  
  #resample cmip data to satellite grid
  delta_126 <- resample(delta_126, all_satellite, method = "mode")
  delta_585 <- resample(delta_585, all_satellite, method = "mode")
  
  #for sea ice, convert cmip6 data from percentage to fraction
  if(cmip_var == "siconc"){
    delta_126 <- delta_126 / 100
    delta_585 <- delta_585 / 100
  }
  
  # 2. Transformations
  
  #transform each month one by one
  for(i in 1:12){
    
    #extract all satellite layers for that month
    monthly_satellite <- all_satellite[[month(time(all_satellite)) == i]]
    
    #extract the deltas for that month
    monthly_126 <- delta_126[[month(time(delta_126)) == i]]
    monthly_585 <- delta_585[[month(time(delta_585)) == i]]
    
    #add the deltas to the monthly satellite data
    transformed_126 <- monthly_satellite + monthly_126
    transformed_585 <- monthly_satellite + monthly_585
    
    #combine transformed datasets to all months
    if(i == 1){
      all_transformed_126 <- transformed_126
      all_transformed_585 <- transformed_585
    } else {
      all_transformed_126 <- c(all_transformed_126, transformed_126)
      all_transformed_585 <- c(all_transformed_585, transformed_585)
    }
  }
  
  # in transformed data, convert negatives to 0 where negative values are implausible
  if(cmip_var %in% c("siconc", "mlotst", "curr")){
    all_transformed_126[all_transformed_126 < 0] <- 0
    all_transformed_585[all_transformed_585 < 0] <- 0
  }
  
  #sanity check
  plot(all_satellite[[1]])
  plot(delta_585[[1]])
  plot(all_transformed_585[[1]])
  
  
  # 3. Export
  
  # create transformation folder
  dir.create(paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/satellite_data/transformed/"), showWarnings = FALSE, recursive = TRUE)
  
  #export transformed satellite data
  writeRaster(all_transformed_126, paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/satellite_data/transformed/", gcm, "_ssp126_glorysres.tif"), overwrite = T)
  writeRaster(all_transformed_585, paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/satellite_data/transformed/", gcm, "_ssp585_glorysres.tif"), overwrite = T)
  
  # print completion
  print(paste0(cmip_var, " ", gcm))
  
}
