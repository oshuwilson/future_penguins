#-------------------------------------------------------------------------------
# Plot mean deltas for each covariate month by month
#-------------------------------------------------------------------------------

# change to use same numeric scale for SSP126 and SSP585 for the same var
# change order of colours in some

rm(list=ls())
setwd("E:/cmip6_data/CMIP6/deltas")

library(tidyverse)
library(terra)
library(tidyterra)

# set variable
var <- "curr"

# long name
long <- "Current Velocity"

# set scenario
ssp <- "ssp585"

# list of all models
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# read in each model one by one and combine
for(gcm in gcms){ 
  
  # read in delta file
  this_delta <- rast(paste0(var, "/", ssp, "/", gcm, "_", ssp, "_", var, "_delta.nc"))
  
  # combine with all other deltas
  if(gcm == gcms[1]){
    deltas <- this_delta
  } else {
    deltas <- c(deltas, this_delta)
  }
}

# for each month, calculate mean delta
for(this_month in 1:12){
  
  # get all layers for that month
  month_deltas <- deltas[[month(time(deltas)) == this_month]]
  
  # calculate average month delta
  mean_na <- function(x){mean(x, na.rm = T)}
  month_delta <- app(month_deltas, fun = "mean_na")
  
  # assign time
  time(month_delta) <- as_date(paste0("2099-", this_month, "-16"))
  
  # combine with all others
  if(this_month == 1){
    all_months <- month_delta
  } else {
    all_months <- c(all_months, month_delta)
  }
}

# crop to 40 degrees south
e <- ext(-180, 180, -90, -40)
all_months <- crop(all_months, e)

# project to EPSG 6932
all_months <- project(all_months, "epsg:6932")

# read in coastline
coast <- readRDS("~/OneDrive - University of Southampton/Documents/Chapter 03/data/coast_ice_vect.RDS")

# assign names
names(all_months) <- month.name

# plot each month
maps <- ggplot() +
  geom_spatraster(data = all_months) +
  geom_spatvector(data = coast, fill = "white") +
  scale_fill_gradient2(low = "steelblue4", high = "red4", 
                       na.value = "transparent", name = expression(Delta)) +
  theme_void() +
  facet_wrap(~lyr, ncol = 3) +
  ggtitle(paste0(long, " (", ssp, ")")) +
  theme(plot.title = element_text(hjust = 0.5))

# export
ggsave(paste0("~/OneDrive - University of Southampton/Documents/Chapter 03/output/imagery/deltas/",
              long, " (", ssp, ").png"), maps,
       height = 12, width = 12)
