#-------------------------------------------------------------------------------
# Tracking Data Maps
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# define species
species <- "KIPE"
longname <- "King Penguin"

# list stage options
if(species == "MAPE"){
  stage_options <- c("chick-rearing", "incubation", "pre-moult")
} else {
  stage_options <- c("chick-rearing", "incubation")
}

# for each stage
for(this_stage in stage_options){
  print(this_stage)
  
  # read in background samples
  bg <- readRDS(paste0("output/at-sea model/background/", species, " ", this_stage, " background.RDS")) %>%
    crop(ext(-180, 180, -90, -40)) %>%
    project("epsg:6932")
  
  # read in coastline
  coast <- readRDS("data/coast_ice_vect.RDS")
  
  # read in stereographic depth
  depth <- readRDS("~/OneDrive - University of Southampton/Documents/Predictor Data/depth_stereographic_cropped.RDS")
  
  # plot
  p1 <- ggplot() +
    geom_spatraster(data = depth) +
    geom_spatvector(data = bg, size = 0.1) +
    geom_spatvector(data = coast, col = NA, fill = "white") +
    # scale_x_continuous(limits = c(-5e06, 5e06), expand = c(0,0)) +
    # scale_y_continuous(limits = c(-5e06, 5e06), expand = c(0,0)) +
    scale_fill_gradient(guide = "none", na.value = "white", low = "steelblue4", high = "steelblue1") +
    theme_void() +
    ggtitle(paste0(longname, " (", this_stage, ")")) +
    theme(plot.title = element_text(hjust = 0.5))
  p1
  
  # save plot
  ggsave(paste0("output/imagery/background sampling/", species, "_", this_stage, ".png"),
         plot = p1,
         width = 10,
         height = 10,
         units = "in",
         dpi = 300)
}
