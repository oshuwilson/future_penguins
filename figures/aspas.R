#-------------------------------------------------------------------------------
# Antarctic Special Protected Areas
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)
library(scam)

# read in ASPAs
aspa <- readRDS("data/aspas/aspa_polygons.RDS")

# remove marine ASPAs
aspa <- aspa %>% filter(Marine == 0)

# read in coast
coast <- rnaturalearth::ne_countries(scale = 10, returnclass = "sv")

# project to 4326
aspa <- project(aspa, "EPSG:4326")

# read in max travel distances by species/stage
dists <- read.csv("output/at-sea model/background/maxdist.csv") %>%
  filter(stg == "chick-rearing")

# list all species
all_spp <- unique(dists$sp)

#-------------------------------------------------------------------------------
# For each species, quantify present suitability around ASPAs
#-------------------------------------------------------------------------------

# for each species
for(species in c("ADPE", "CHPE", "EMPE", "GEPE", "KIPE", "MAPE")){
  print(species)
  
  # identify breeding habitat
  climatic_prediction <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))
  threshold <- readRDS(paste0("output/climatic model/thresholds/", species, "_tss_max_threshold.RDS"))
  climatic_core_habitat <- climatic_prediction >= threshold
  climatic_core_habitat <- climatic_core_habitat %>%
    as.polygons() %>%
    filter(mean == 1) %>%
    project("epsg:4326")
  
  # pull max distance
  maxdist <- dists %>%
    filter(sp == species) %>%
    pull(dist)
  
  # for each ASPA
  for(i in 1:length(aspa)){
    print(i)
    this_aspa <- aspa[i]
    
    # does the aspa contain climatic core habitat?
    aspa_core <- climatic_core_habitat %>%
      intersect(this_aspa)
    if(length(aspa_core) > 0) {
      aspa_core_check <- 1
    } else {
      aspa_core_check <- 0
    }
    
    # buffer aspa by max distance
    aspa_buff <- buffer(this_aspa, maxdist*1000)
    
    # erase coast from buffer
    aspa_buff <- erase(aspa_buff, coast %>% crop(ext(aspa_buff)))
    
    # read in core habitat bins for this species 
    bins <- rast(paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"))
    
    # vectorise bins
    bins_vect <- bins %>%
      as.polygons() %>%
      filter(mean == 20)
    
    # extract core habitat area within buffered ASPA
    core_area <- bins_vect %>%
      intersect(aspa_buff)
    
    # calculate area of core_area
    core_area_km2 <- expanse(core_area, unit = "km")
    
    # if 0 core area then set to 0
    if(length(core_area_km2) == 0) {
      core_area_km2 <- 0
    }
    
    # calculate area of ASPA buffer
    aspa_buff_km2 <- expanse(aspa_buff, unit = "km")
    
    # proportion of core habitat
    if(length(aspa_buff_km2) > 0) {
      prop_core <- core_area_km2 / aspa_buff_km2
    } else {
      prop_core <- 0
    }
    
    # create data frame of aspa number and core area
    aspa_area <- data.frame(
      ASPA_No = this_aspa$ASPA_No,
      present_area_prop = prop_core,
      present_breeding_habitat = aspa_core_check
    )
    
    # join to other ASPAs
    if(i == 1) {
      all_aspa_area <- aspa_area
    } else {
      all_aspa_area <- rbind(all_aspa_area, aspa_area)
    }
  }
  
  # assign species
  all_aspa_area$species <- species
  
  # join to other species
  if(species == "ADPE"){
    present_df <- all_aspa_area
  } else {
    present_df <- rbind(present_df, all_aspa_area)
  }
}

# group by ASPA number and take mean prop and breeding habitat presence across species
present_summary <- present_df %>%
  group_by(ASPA_No, species) %>%
  summarise(present_area_prop = mean(present_area_prop),
            present_breeding_habitat = max(present_breeding_habitat))

# export
saveRDS(present_summary, "output/imagery/aspas/present_df.rds")

#-------------------------------------------------------------------------------
# Future core breeding habitat overlap
#-------------------------------------------------------------------------------

scenario <- "ssp585"

for(species in c("ADPE", "CHPE", "EMPE", "GEPE", "KIPE", "MAPE")){
  print(species)
  
  # loop to check if core breeding habitat preserved
  for(i in 1:length(aspa)){
    print(i)
    this_aspa <- aspa[i]
    
    # list of all GCMs
    gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
              "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")
    
    # loop over each gcm
    for(gcm in gcms){
      
      # read in climatic prediction
      climatic_prediction <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_simple_ensemble.tif"))
      
      # read in threshold for suitable climatic conditions
      threshold <- readRDS(paste0("output/climatic model/thresholds/", species, "_tss_max_threshold.RDS"))
      
      # binarize climatic prediction
      climatic_core_habitat <- climatic_prediction >= threshold
      
      # convert to polygons
      climatic_core_habitat <- climatic_core_habitat %>%
        as.polygons() %>%
        filter(mean == 1) %>%
        project(crs(this_aspa))
      
      # does the aspa contain climatic core habitat?
      aspa_core <- climatic_core_habitat %>%
        intersect(this_aspa)
      if(length(aspa_core) > 0) {
        aspa_core_check <- 1
      } else {
        aspa_core_check <- 0
      }
      
      # create df of gcm
      df <- data.frame(
        ASPA_No = this_aspa$ASPA_No,
        gcm = gcm,
        aspa_core_check = aspa_core_check,
        scenario = scenario
      )
      
      # bind to other GCMs
      if(gcm == gcms[1]) {
        gcm_df <- df
      } else {
        gcm_df <- rbind(gcm_df, df)
      }
    }
    
    # join gcm_df to other ASPAs
    if(i == 1) {
      all_gcm_df <- gcm_df
    } else {
      all_gcm_df <- rbind(all_gcm_df, gcm_df)
    }
  }
  
  # assign species
  all_gcm_df$species <- species
  
  # join to other species
  if(species == "ADPE"){
    gcm_summary <- all_gcm_df
  } else {
    gcm_summary <- rbind(gcm_summary, all_gcm_df)
  }
}

# summarise by ASPA number, species, and GCM
gcm_summary <- gcm_summary %>%
  group_by(ASPA_No, species, gcm) %>%
  summarise(aspa_core_check = max(aspa_core_check))

# export
saveRDS(gcm_summary, paste0("output/imagery/aspas/", scenario, "core_check.rds"))

#-------------------------------------------------------------------------------
# Future foraging habitat proportion
#-------------------------------------------------------------------------------

scenario <- "ssp126"

for(species in c("ADPE", "CHPE", "EMPE", "GEPE", "KIPE", "MAPE")){
  print(species)
  
  for(i in 1:length(aspa)){
    print(i)
    this_aspa <- aspa[i]
    
    # pull max distance
    maxdist <- dists %>%
      filter(sp == species) %>%
      pull(dist)
    
    # buffer aspa by max distance
    aspa_buff <- buffer(this_aspa, maxdist*1000)
    
    # erase coast from buffer
    aspa_buff <- erase(aspa_buff, coast %>% crop(ext(aspa_buff)))
    
    
    # read in ssp126 core habitat bins for this species
    bins <- rast(paste0("output/combined/projections/", scenario, "/", species, "_gcm_core_habitat_bins.tif"))
    
    # for each GCM 
    for(j in 1:nlyr(bins)){
      
      # isolate this layer
      this_bin <- bins[[j]]
      
      # vectorise bins
      bins_vect <- this_bin %>%
        as.polygons() %>%
        filter(mean == 3)
      
      # extract core habitat area within buffered ASPA
      core_area <- bins_vect %>%
        intersect(aspa_buff)
      
      # calculate area of core_area
      core_area_km2 <- expanse(core_area, unit = "km")
      
      # if 0 core area then set to 0
      if(length(core_area_km2) == 0) {
        core_area_km2 <- 0
      }
      
      # calculate area of ASPA buffer
      aspa_buff_km2 <- expanse(aspa_buff, unit = "km")
      
      # proportion of core habitat
      if(length(aspa_buff_km2) > 0) {
        prop_core <- core_area_km2 / aspa_buff_km2
      } else {
        prop_core <- 0
      }
      
      # create df
      df <- data.frame(
        ASPA_No = this_aspa$ASPA_No,
        core_area = prop_core,
        gcm = j,
        scenario = scenario
      )
      
      # join to other GCMs
      if(j == 1) {
        aspa_area <- df
      } else {
        aspa_area <- rbind(aspa_area, df)
      }
      
    }
    
    # calculate mean and SE across GCMs for this ASPA
    suppressMessages(ssp_summary <- aspa_area %>%
      group_by(ASPA_No, scenario) %>%
      summarise(area_mean = mean(core_area),
                area_se = sd(core_area)/sqrt(n())))
    
    # join to other ASPAs
    if(i == 1) {
      all_ssp_summary <- ssp_summary
    } else {
      all_ssp_summary <- rbind(all_ssp_summary, ssp_summary)
    }
  }
  
  # summarise again by ASPA number and scenario
  all_ssp_summary <- all_ssp_summary %>%
    group_by(ASPA_No, scenario) %>%
    summarise(area_mean = mean(area_mean),
              area_se = mean(area_se))
  
  # assign species to this dataset
  all_ssp_summary$species <- species
  
  # join to other species
  if(species == "ADPE"){
    final_ssp_summary <- all_ssp_summary
  } else {
    final_ssp_summary <- rbind(final_ssp_summary, all_ssp_summary)
  }
}

# export
saveRDS(final_ssp_summary, paste0("output/imagery/aspas/", scenario, "foraging_summary.rds"))



#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

# clean up
rm(list=ls())

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
  
  # minimum threshold based on 95th quantile
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

# if ASPA is in southern ross sea, retain (satellite imagery limited but not much loss)
final_summary <- final_summary %>%
  mutate(ssp126_risk = ifelse(aspa %in% c(105, 121, 124, 157), "Retain", ssp126_risk),
         ssp585_risk = ifelse(aspa %in% c(105, 121, 124, 157), "Retain", ssp585_risk))

# Adelie ASPAs on Antarctic Peninsula should lose suitability under 585, but buffers leak into Weddell Sea
final_summary <- final_summary %>%
  mutate(ssp585_risk = ifelse(aspa %in% c(107, 115, 117, 139) & species == "ADPE", "Lost", ssp585_risk))

# export final_summary for species maps
saveRDS(final_summary, "output/imagery/aspas/final_summary.rds")


# for each species, how many ASPAs remain under each scenario
by_species <- final_summary %>%
  group_by(species) %>%
  summarise(ssp126_retain = sum(ssp126_risk == "Retain"),
            ssp126_lost = sum(ssp126_risk == "Lost"),
            ssp585_retain = sum(ssp585_risk == "Retain")) %>%
  mutate(total = ssp126_retain + ssp126_lost) %>%
  select(-ssp126_lost)

# pivot longer
by_species <- by_species %>%
  pivot_longer(cols = 2:4, names_to = "scenario", values_to = "count") %>%
  mutate(scenario = case_when(
    scenario == "ssp126_retain" ~ "SSP1-2.6",
    scenario == "ssp585_retain" ~ "SSP5-8.5",
    scenario == "total" ~ "Present"
  ))

# reorder so present comes first
by_species$scenario <- factor(by_species$scenario, levels = c("Present", "SSP1-2.6", "SSP5-8.5"))

# rename species to proper names
by_species$species <- recode(by_species$species,
                               "ADPE" = "Adélie",
                               "CHPE" = "Chinstrap",
                               "EMPE" = "Emperor",
                               "GEPE" = "Gentoo")

# plot
p1 <- ggplot(by_species, aes(x = species, y = count, fill = interaction(species, scenario))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(x = "", y = "Number of ASPAs with Colonies") + 
  scale_fill_manual(values = c("#000004", "#005766", "#631850", "#7F242A",
                                "grey30", "#00768B", "#C530A0", "#C9404A",
                                "grey50", "#009CB8", "#DF7CC6", "#D26067"),
                    guide = "none") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(panel.spacing = unit(-0.01, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 12, color = "grey20"))
p1 + ggview::canvas(8, 5)

# export
ggsave("text/figures/draft/aspas/aspas_barplot.svg", p1,
       width = 8, height = 5, units = "in", dpi = 300)

# Antarctic map with ASPAs

# load in coast
coast <- readRDS("data/coast_ice_vect.RDS")

# load in aspa point data
aspa_points <- readRDS("data/aspas/aspas_pts.rds") %>% 
  project(crs(coast))

# limit to one point per ASPA_No
aspa_points <- aspa_points %>%
  group_by(ASPA_No) %>%
  slice(1)

# crop coast
coast_crop <- crop(coast, ext(aspa_points))

# read in ASPA info with present penguin colonies
aspas <- read.csv("data/aspas/aspa_penguins.csv") %>%
  filter(ASPA_No != 182)

# get ASPA numbers that contain colonies for each species
colony_aspas <- aspas %>%
  pivot_longer(cols = 2:7, names_to = "species", values_to = "colony_present") %>%
  filter(colony_present == 1) %>%
  pull(ASPA_No) %>%
  unique()

# filter aspa points to those with colonies
aspa_points <- aspa_points %>%
  filter(ASPA_No %in% colony_aspas)

# plot together - add shadow to points
p2 <- ggplot() +
  geom_spatvector(data = coast_crop, aes(fill = surface), col = NA) +
  geom_spatvector(data = aspa_points, color = "grey90", fill = "black", size = 4, 
                  shape = 21) +
  scale_fill_manual(values = c("grey70", "grey50"), guide = "none") +
  theme_void()
p2 + ggview::canvas(8, 8)

# export
ggsave("text/figures/draft/aspas/aspas_map.png", p2, width = 8, height = 8, units = "in", dpi = 300)

# # inset for Antarctic Peninsula
# ap <- coast_crop %>% crop(ext(-2.7e06, -2.2e06, 1e06, 2.5e06))
# p3 <- ggplot() + 
#   geom_spatvector(data = ap, aes(fill = surface), col = NA) +
#   geom_spatvector(data = aspa_points %>% crop(ext(ap)), color = "grey90", fill = "black", size = 2, 
#                   shape = 21) +
#   scale_fill_manual(values = c("grey70", "grey50"), guide = "none") +
#   theme_void() 
# p3 + ggview::canvas(2, 4)
# 
# # export
# ggsave("text/figures/draft/aspas/aspas_ap_inset.png", p3, width = 2, height = 4, units = "in", dpi = 300)
# 
# # inset for Ross Sea
# rs <- coast_crop %>% crop(ext(0, 0.8e06, -2.5e06, -1.2e06)) 
# p4 <- ggplot() + 
#   geom_spatvector(data = rs, aes(fill = surface), col = NA) +
#   geom_spatvector(data = aspa_points %>% crop(ext(rs)), color = "grey90", fill = "black", size = 3, 
#                   shape = 21) +
#   scale_fill_manual(values = c("grey70", "grey50"), guide = "none") +
#   theme_void()
# p4 + ggview::canvas(3, 5)
# 
# # export 
# ggsave("text/figures/draft/aspas/aspas_rs_inset.png", p4, width = 3, height = 5, units = "in", dpi = 300)
# 
# # alternative version of main map with points in insets erased and bboxes for insets
# ap_bbox <- ext(-2.7e06, -2.2e06, 1e06, 2.5e06) %>% as.polygons()
# crs(ap_bbox) <- crs(coast_crop)
# rs_bbox <- ext(0, 0.8e06, -2.5e06, -1.2e06) %>% as.polygons()
# crs(rs_bbox) <- crs(coast_crop)
# 
# p5 <- ggplot() +
#   geom_spatvector(data = coast_crop, aes(fill = surface), col = NA) +
#   geom_spatvector(data = aspa_points %>% erase(ap_bbox) %>% erase(rs_bbox), 
#                   color = "grey90", fill = "black", size = 4, shape = 21) +
#   scale_fill_manual(values = c("grey70", "grey50"), guide = "none") +
#   theme_void() 
# p5 + ggview::canvas(8, 8)
# 
# # export
# ggsave("text/figures/draft/aspas/aspas_map_no_inset_points.png", p5, width = 8, height = 8, units = "in", dpi = 300)

