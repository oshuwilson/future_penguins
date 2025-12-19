#-------------------------------------------------------------------------------
# SCAM response curve
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(scam)
library(tidyverse)
library(gratia)

# list all scam models
scam_files <- list.files(path = "output/at-sea model/availability/",
                         full.names = TRUE)

# limit to files containing "scam_model"
scam_files <- scam_files[grep("scam_model", scam_files)]

# for each file
for(file in scam_files){
  
  # get species name from file
  species <- strsplit(basename(file), "_")[[1]][1]
  
  # convert to longname format
  species_name <- case_when(
    species == "ADPE" ~ "Adelie Penguin",
    species == "CHPE" ~ "Chinstrap Penguin",
    species == "GEPE" ~ "Gentoo Penguin",
    species == "MAPE" ~ "Macaroni Penguin",
    species == "EMPE" ~ "Emperor Penguin",
    species == "KIPE" ~ "King Penguin"
  )
  
  # get stage name from file
  stage <- strsplit(basename(file), "_")[[1]][2]
  
  # capitalise 
  stage <- tools::toTitleCase(stage)
  
  # read in model
  m1 <- readRDS(file)
  
  # get range of values to predict to
  max_val <- case_when(
    file == scam_files[1] ~ 280,
    file == scam_files[2] ~ 600,
    file == scam_files[3] ~ 200,
    file == scam_files[4] ~ 300,
    file == scam_files[5] ~ 400,
    file == scam_files[6] ~ 200,
    file == scam_files[7] ~ 2500,
    file == scam_files[8] ~ 2000,
    file == scam_files[9] ~ 500,
    file == scam_files[10] ~ 1200,
    file == scam_files[11] ~ 1500
  )
  vals <- seq(0, max_val, length.out = 400)
  vals <- data.frame(dist2col = vals)
  
  # predict values
  vals$sm <- predict.scam(m1, newdata = vals, type = "response")
  
  # scale prediction to 0 to 1
  vals$sm <- (vals$sm - min(vals$sm)) / (max(vals$sm) - min(vals$sm))
  
  # plot
  p1 <- ggplot() +
    #geom_ribbon(data = sm, aes(x = xval, ymin = .lower_ci, ymax = .upper_ci), 
    #            alpha = 0.3, fill = "grey80") +
    geom_line(data = vals, aes(x = dist2col, y = sm), size = 1, col = "grey30") +
    labs(x = "Distance to Coast (km)",
         y = "Multiplier") +
    theme_minimal() +
    scale_x_continuous(expand = c(0,10)) +
    scale_y_continuous(limits = c(-0.01, 1.01), expand = c(0,0)) +
    theme_bw() +
    ggtitle(paste(species_name, stage)) 
  p1 + ggview::canvas(width = 9, height = 6)
  
  # export
  ggsave(paste0("output/at-sea model/availability/plots/",
                species, "_", stage, "_scam_response_curve_dist2coast.png"),
         plot = p1,
         width = 9,
         height = 6)
}
