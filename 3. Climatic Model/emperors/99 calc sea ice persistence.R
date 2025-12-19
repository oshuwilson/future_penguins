#-------------------------------------------------------------------------------
# Calculate sea ice persistence per year - Present Day
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("E:/Satellite_Data/daily/sic")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}

# list all netcdf files
files <- list.files(pattern = ".nc$", full.names = TRUE)

# limit to 2000 to 2020
files <- files[grep("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020", files)]
files

# loop over files
for(file in files){
  print(file)
  
  # read in raster
  sic <- rast(file)
  
  # reclass all values above 15% to 1
  sic <- sic > 0.15
  
  # sum number of days that each cell is above 15%
  persistence <- app(sic, fun = sum, na.rm = T)
  
  # assign time as year
  year <- str_extract(file, "20[0-9]{2}")
  time(persistence) <- as_date(paste0(year, "-01-01"))
  
  # join to all other files
  if (file == files[1]) {
    persistence_all <- persistence
  } else {
    persistence_all <- c(persistence_all, persistence)
  }
  
  # print year completion
  print(paste("Completed year:", year))
}

# calculate mean persistence over all years
mean_persistence <- app(persistence_all, fun = mean, na.rm = T)

# apply CRS
crs(mean_persistence) <- "EPSG:4326"

# export persistence file
writeCDF(mean_persistence, "~/OneDrive - University of Southampton/Documents/Chapter 03/output/climatic model/sea_ice_persistence/present_persistence.nc")

test2 <- rast("~/OneDrive - University of Southampton/Documents/Chapter 03/output/climatic model/sea_ice_persistence/present_persistence.nc")

#-------------------------------------------------------------------------------
# Future Projections
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())
setwd("E:/")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}

# define ssp
scenario <- "ssp585"

# list all present-day netcdf files
files <- list.files(pattern = ".nc$", path = "Satellite_Data/daily/sic",  full.names = TRUE)

# limit to 2000 to 2020
files <- files[grep("2000|2001|2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020", files)]
files

# read in first sic file
sic <- rast(files[1])

# define all gcms
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# loop over each gcm
gcm <- gcms[1]

# read in delta file
delta <- rast(paste0("cmip6_data/CMIP6/deltas/siconc/", scenario, "/", gcm, "_", scenario, "_siconc_delta.nc"))

# resample cmip data to satellite grid
delta <- resample(delta, sic, method = "med")

# divide deltas by 100 to convert from % to fraction
delta <- delta / 100

# loop over files
for(file in files){
  print(file)
  
  # read in raster 
  sic <- rast(file)
  
  # apply crs to sic
  crs(sic) <- "EPSG:4326"
  
  #transform each month one by one
  for(i in 1:12){
    print(i)
    
    #extract all satellite layers for that month
    monthly_sic <- sic[[month(time(sic)) == i]]
    
    #extract the deltas for that month
    monthly_delta <- delta[[month(time(delta)) == i]]
    
    #add the deltas to the monthly satellite data
    transformed_sic <- monthly_sic + monthly_delta
    
    #combine transformed datasets to all months
    if(i == 1){
      all_transformed_sic <- transformed_sic
    } else {
      all_transformed_sic <- c(all_transformed_sic, transformed_sic)
    }
  }
  
  # reclass all values above 15% to 1
  all_transformed_sic <- all_transformed_sic > 0.15
  
  # sum number of days that each cell is above 15%
  persistence <- app(all_transformed_sic, fun = sum, na.rm = T)
  
  # assign time as year
  year <- str_extract(file, "20[0-9]{2}")
  time(persistence) <- as_date(paste0(year, "-01-01"))
  
  # join to all other files
  if (file == files[1]) {
    persistence_all <- persistence
  } else {
    persistence_all <- c(persistence_all, persistence)
  }
  
  # print year completion
  print(paste("Completed year:", year))
}

# calculate mean persistence over all years
mean_persistence <- app(persistence_all, fun = mean, na.rm = T)

# apply CRS
crs(mean_persistence) <- "EPSG:4326"

# export persistence file
writeRaster(mean_persistence, paste0("~/OneDrive - University of Southampton/Documents/Chapter 03/output/climatic model/sea_ice_persistence/", scenario, "/future_persistence_", gcm, "_", scenario, ".tif"))

test <- rast(paste0("~/OneDrive - University of Southampton/Documents/Chapter 03/output/climatic model/sea_ice_persistence/", scenario, "/future_persistence_", gcm, "_", scenario, ".tif"))

diff <- test - test2
diff <- project(diff, "epsg:6932")
ggplot() +
  geom_spatraster(data = diff) +
  scale_fill_gradient2() +
  xlim(-5e06, 5e06) +
  ylim(-5e06, 5e06) 
