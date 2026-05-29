#-------------------------------------------------------------------------------
# Maps of ASPA retention per species
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)
library(scam)
library(cowplot)

# read in ASPA info with present penguin colonies
aspas <- read.csv("data/aspas/aspa_penguins.csv") %>%
  filter(ASPA_No != 182)

# pivot longer
aspas <- aspas %>%
  pivot_longer(cols = 2:7, names_to = "species", values_to = "colony_present")

# read in present ASPA suitability
present_df <- readRDS("output/imagery/aspas/present_df.rds")

# read in future core habitat presence in ASPAs
core_check_126 <- readRDS("output/imagery/aspas/ssp126core_check.rds")
core_check_585 <- readRDS("output/imagery/aspas/ssp585core_check.rds")

# read in future foraging habitat proportion in ASPAs
foraging_126 <- readRDS("output/imagery/aspas/ssp126foraging_summary.rds")
foraging_585 <- readRDS("output/imagery/aspas/ssp585foraging_summary.rds")

# for each species
for(this_species in c("ADPE", "CHPE", "EMPE", "GEPE")){
  
  # isolate ASPAs that contain colonies
  these_aspas <- aspas %>% 
    filter(species == this_species,
           colony_present == 1) %>%
    pull(ASPA_No)
  
  # minimum threshold based on 5th percentile
  min_thresh <- 0.15
  
  # for each aspa
  for(this_aspa in these_aspas){
    
    # get present core check and area prop
    present_core_check <- present_df %>%
      filter(ASPA_No == this_aspa,
             species == this_species) %>%
      pull(present_breeding_habitat)
    present_area_prop <- present_df %>%
      filter(ASPA_No == this_aspa,
             species == this_species) %>%
      pull(present_area_prop)
    
    # does present area prop fall below min thresh
    if(present_area_prop < min_thresh) {
      subthresh <- TRUE
    } else {
      subthresh <- FALSE
    }
    
    # get ssp 126 core check and area prop
    ssp126_core_check <- core_check_126 %>%
      filter(ASPA_No == this_aspa,
             species == this_species) %>%
      pull(aspa_core_check)
    ssp126_area_prop <- foraging_126 %>%
      filter(ASPA_No == this_aspa,
             species == this_species) %>%
      pull(area_mean)
    
    # get ssp 585 core check and area prop
    ssp585_core_check <- core_check_585 %>%
      filter(ASPA_No == this_aspa,
             species == this_species) %>%
      pull(aspa_core_check)
    ssp585_area_prop <- foraging_585 %>%
      filter(ASPA_No == this_aspa,
             species == this_species) %>%
      pull(area_mean)
    
    # if mean core checks are over 0.5, value at 1
    if(mean(ssp126_core_check) > 0.5) {
      ssp126_core_check <- 1
    } else{
      ssp126_core_check <- 0
    }
    if(mean(ssp585_core_check) > 0.5) {
      ssp585_core_check <- 1
    } else {
      ssp585_core_check <- 0
    }
    
    # for each scenario, does core check become 0 and does core area fall below
    # the minimum threshold or present-day value (if already subthreshold)
    ssp126_risk <- ifelse(ssp126_area_prop >= min_thresh,
                          "Retain",
                          "Lost")
    ssp585_risk <- ifelse(ssp585_area_prop >= min_thresh,
                          "Retain",
                          "Lost")
    
    # create df for this ASPA
    df <- data.frame(
      aspa = this_aspa,
      present_core_check = present_core_check,
      present_area_prop = present_area_prop,
      ssp126_core_check = ssp126_core_check,
      ssp126_area_prop = ssp126_area_prop,
      ssp126_risk = ssp126_risk,
      ssp585_core_check = ssp585_core_check,
      ssp585_area_prop = ssp585_area_prop,
      ssp585_risk = ssp585_risk,
      species = this_species
    )
    
    # join to all ASPAs
    if(this_aspa == these_aspas[1]) {
      aspa_summary <- df
    } else {
      aspa_summary <- rbind(aspa_summary, df)
    }
  }
  
  # join to all species
  if(this_species == "ADPE") {
    final_summary <- aspa_summary
  } else {
    final_summary <- rbind(final_summary, aspa_summary)
  }
}

final_summary <- readRDS("output/imagery/aspas/final_summary.rds")

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------


# for each species
for(this_species in c("ADPE", "CHPE", "EMPE", "GEPE")){
  
  # load in coast
  coast <- readRDS("data/coast_ice_vect.RDS")
  
  # read in aspa points
  aspa_pts <- readRDS("data/aspas/aspas_pts.rds")
  
  # ensure same projection as coast
  aspa_pts <- project(aspa_pts, crs(coast))
  
  # crop coast
  coast_crop <- crop(coast, ext(aspa_pts))
  
  # limit final_summary to this species
  df <- final_summary %>%
    filter(species == this_species)
  
  # characterise ASPA as retained in all scenarios, only in 126, only in 585 and in none
  df <- df %>%
    mutate(retention_category = case_when(
      ssp126_risk == "Retain" & ssp585_risk == "Retain" ~ "All",
      ssp126_risk == "Retain" & ssp585_risk == "Lost" ~ "SSP1-2.6 Only",
      ssp126_risk == "Lost" & ssp585_risk == "Retain" ~ "SSP5-8.5 Only",
      ssp126_risk == "Lost" & ssp585_risk == "Lost" ~ "None"
    ))
  
  # if ASPA is in southern ross sea, retain (satellite imagery limited but not much loss)
  df <- df %>%
    mutate(retention_category = ifelse(aspa %in% c(105, 121, 124, 157), "All", retention_category))
  
  # limit to ASPAs used by this species
  aspa_pts <- aspa_pts %>%
    filter(ASPA_No %in% df$aspa)
  
  # limit to one point per ASPA_No
  aspa_pts <- aspa_pts %>%
    group_by(ASPA_No) %>%
    slice(1) %>%
    ungroup()
  
  # append retention category to aspa points
  aspa_pts <- aspa_pts %>%
    left_join(df %>% select(aspa, retention_category), by = c("ASPA_No" = "aspa"))
  
  # change species name for plotting
  species_name <- case_when(
    this_species == "ADPE" ~ "Adélie",
    this_species == "CHPE" ~ "Chinstrap",
    this_species == "EMPE" ~ "Emperor",
    this_species == "GEPE" ~ "Gentoo"
  )
  
  # plot by retention category
  p1 <- ggplot() +
    geom_spatvector(data = coast_crop, aes(fill = surface), col = NA) +
    geom_spatvector(data = aspa_pts, aes(fill = retention_category), size = 3,
                    shape = 21, color = "grey90") +
    scale_fill_manual(values = c("All" = "#00798c", "SSP1-2.6 Only" = "#edae49",
                                 "SSP5-8.5 Only" = "#ED7B4A", "None" = "#d1495b",
                                 "Land" = "grey50", "Ice" = "grey70"),
                      guide = "none") +
    theme_void() +
    ggtitle(species_name) + 
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) 
  
  # join to other species
  if(this_species == "ADPE") {
    p_all <- list(p1)
  } else {
    p_all <- c(p_all, p1)
  }
}

# grid
grid <- plot_grid(plotlist = p_all)
grid + ggview::canvas(10, 10)

# export
ggsave("text/figures/draft/supplementary/aspa_species_maps.png", grid, width = 10, height = 10, units = "in", dpi = 300)
