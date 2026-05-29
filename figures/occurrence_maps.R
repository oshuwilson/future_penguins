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

# loop over species
for(species in species_options){
  print(species)
  
  # read in tracking data
  tracks <- readRDS(paste0("data/tracks/", species, ".RDS"))
  
  # define stage options according to those used in these models
  if(species == "MAPE"){
    stage_options <- c("chick-rearing", "incubation", "pre-moult")
  } else if(species %in% c("GEPE", "EMPE")){
    stage_options <- "chick-rearing"
  } else {
    stage_options <- c("chick-rearing", "incubation")
  }
  
  # for each stage
  for(this_stage in stage_options){
    print(this_stage)
    
    # limit tracks to this stage
    stage_tracks <- tracks %>%
      filter(stage == this_stage)
    
    # convert to spatvector
    stage_trax <- vect(stage_tracks, geom = c("lon", "lat"), crs = "epsg:4326") %>%
      project("epsg:6932")
    
    # combine with other stages
    if(this_stage == stage_options[1]){
      all_trax <- stage_trax
    } else {
      all_trax <- bind_spat_rows(all_trax, stage_trax)
    }
  }
  
  # add species column
  all_trax$species <- species
  
  # join with all species
  if(species == species_options[1]){
    combined_trax <- all_trax
  } else {
    combined_trax <- bind_spat_rows(combined_trax, all_trax)
  }
}


# repeat for colony locations
for(species in species_options){
  
  # read in colony locations
  colonies <- readxl::read_xlsx(paste0("data/colonies/Final Colonies/", species, "_by_colony.xlsx")) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))
  
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

# create lines for each individual in the tracks - this is slow!!!
# inds <- unique(combined_trax$individual_id)
# for(i in 1:length(inds)){
#   ind <- inds[i]
#   ind_trax <- combined_trax %>%
#     filter(individual_id == ind)
#   start_date <- min(ind_trax$date)
#   ind_trax <- as.lines(ind_trax)
#   ind_trax$date <- start_date
#   ind_trax$individual_id <- ind
#   if(ind == inds[1]){
#     trax_lines <- ind_trax
#   } else {
#     trax_lines <- c(trax_lines, ind_trax)
#   }
#   if(i %% 100 == 0){
#     print(paste0(i, "/", length(inds)))
#   }
# }
# trax_lines <- vect(trax_lines)
# 
# # append species from individual_ids
# ind_species <- combined_trax %>%
#   as.data.frame() %>%
#   select(individual_id, species) %>%
#   distinct()
# trax_lines <- trax_lines %>%
#   left_join(ind_species, by = "individual_id")
# 
# ggplot() +
#   geom_spatvector(data = trax_lines, aes(col = species), lwd = 0.5)
# 
# # save this for later use
# saveRDS(trax_lines, "data/track_lines.RDS")

#-------------------------------------------------------------------------------
# Plot map for all species
#-------------------------------------------------------------------------------

# read in track lines
combined_trax <- readRDS("data/track_lines.RDS")

# read in front positions
fronts <- sf::read_sf("data/fronts/antarctic_circumpolar_current_fronts.shp") %>%
  vect()
fronts <- fronts %>%
  filter(NAME %in% c("Polar Front (PF)", "Subantarctic Front (SAF)",
                     "Southern Antarctic Circumpolar Current Front (sACCf)"))

# create bounding box of fronts
fronts_bbox <- as.polygons(ext(-180, 180, -80, -40)) %>%
  buffer(0)

# crop and project fronts
fronts <- crop(fronts, ext(fronts_bbox))
fronts <- fronts %>%
  project("epsg:6932")
fronts <- fronts[-c(3:4)]

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot tracks
p1 <- ggplot() +
  geom_spatvector(data = fronts, col = "grey70", aes(linetype = NAME), linewidth = 0.6,
                  alpha = 0.6) +
  geom_spatvector(data = combined_trax %>% filter(species == "KIPE"), col = "#F67F13", lwd = 0.5, alpha = 1) +
  geom_spatvector(data = combined_trax %>% filter(species == "MAPE"), col = "#EAC10B", lwd = 0.5, alpha = 1) +
  geom_spatvector(data = combined_trax %>% filter(species == "ADPE"), col = "#000004", lwd = 0.5, alpha = 1) +
  geom_spatvector(data = combined_trax %>% filter(species == "CHPE"), col = "#00768B", lwd = 0.5, alpha = 1) +
  geom_spatvector(data = combined_trax %>% filter(species == "EMPE"), col = "#84206B", lwd = 0.5, alpha = 1) +
  geom_spatvector(data = combined_trax %>% filter(species == "GEPE"), col = "#C9404A", lwd = 0.5, alpha = 1) +
  theme_void() +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey50"), guide = "none") +
  scale_linetype(guide = "none")
p1 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/track_map/ggplot_export.png", p1,
       width = 8, height = 8, units = "in", dpi = 300)

