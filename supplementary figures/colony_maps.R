#-------------------------------------------------------------------------------
# Plot the collated occurrence data
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# species options
species_options <- c("GEPE", "EMPE", "KIPE", "CHPE", "MAPE", "ADPE")

# loop over each species
for(species in species_options){
  
  # read in colony locations
  colonies <- readxl::read_xlsx(paste0("data/colonies/Final Colonies/", species, "_by_colony.xlsx")) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>%
    drop_na(longitude, latitude) %>%
    select(longitude, latitude, name)
  
  # convert to spatvector
  colonies <- vect(colonies, geom = c("longitude", "latitude"), crs = "epsg:4326") %>%
    project("epsg:6932")
  
  # add species column
  colonies$species <- species
  
  # join to other species
  if(species == species_options[1]){
    combined_colonies <- colonies
  } else {
    combined_colonies <- bind_spat_rows(combined_colonies, colonies)
  }
}

# read in ccamlr subareas
subareas <- CCAMLRGIS::load_ASDs() 
subareas <- vect(subareas)
colonies <- combined_colonies 

# get nearest subarea to each background point
for(i in 1:length(colonies)){
  
  # isolate point
  pt <- colonies[i]
  
  # calculate distance to nearest subareas
  dists <- distance(pt, subareas)
  
  # get nearest ID
  nearest_idx <- which.min(dists)
  
  # get subarea name
  pt$subarea <- subareas[nearest_idx]$GAR_Name
  
  # combine to all other points
  if(i == 1){
    col_subareas <- pt
  } else {
    col_subareas <- bind_spat_rows(col_subareas, pt)
  }
  
  # if i is a multiple of 100, print progress
  if(i %% 100 == 0){
    print(paste0("Processed ", i, " of ", length(colonies), " points"))
  }
}

# convert to dataframe
col_subareas <- col_subareas %>%
  project("epsg:4326") %>%
  as.data.frame(geom = "XY")

# override colonies from Falklands, Chile, Bouvet, and Macquarie
col_subareas <- col_subareas %>%
  mutate(subarea = case_when(
    x > -64 & x < -55 & y > -55 ~ "Falklands",
    x > 155 & x < 160 & y > -55 & y < -53 ~ "Macquarie",
    x > 160 & x < 180 & y > -55 & y < -47.5 ~ "NZ Subantarctic",
    x > 0 & x < 10 & y > -55 & y < -50 ~ "Bouvet",
    x > 75 & x < 80 & y > -42 ~ "Amsterdam and St Paul",
    x > -15 & x < -5 & y > -42 ~ "Tristan da Cunha",
    x < -64 & y > -57 ~ "Chile", 
    TRUE ~ subarea
  ))
colonies <- vect(col_subareas, geom = c("x", "y"), crs = "epsg:4326") 

# plot
ggplot() +
  geom_spatvector(data = subareas, fill = "white") +
  geom_spatvector(data = colonies %>% project("epsg:6932"), 
                  aes(col = subarea)) +
  theme_minimal()

# export figdata
saveRDS(colonies, "output/climatic model/figdata/colonies_subareas.RDS")


#-------------------------------------------------------------------------------
# Make Plots
#-------------------------------------------------------------------------------

