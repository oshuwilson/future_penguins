#-------------------------------------------------------------------------------
# Plot protected area coverage per subarea
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# load in mpa data
full <- readRDS("data/mpas/full_mpas_agg.rds")
high <- readRDS("data/mpas/high_mpas_agg.rds")
light <- readRDS("data/mpas/light_mpas_agg.rds")
inc <- readRDS("data/mpas/incompatible_mpas_agg.rds")
prop <- readRDS("data/mpas/proposed_mpas_agg.rds") 
unknown <- readRDS("data/mpas/unknown_mpas_agg.rds")

#-------------------------------------------------------------------------------
# Present day coverage for each species
#-------------------------------------------------------------------------------

# define species
species <- "CHPE"
longname <- "Chinstrap Penguin"

# read in current core habitat bins
bins <- rast(paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"))

# convert to polygons
polys <- as.polygons(bins, dissolve = T) %>%
  filter(mean == 20)

# project to same CRS as mpas
polys <- project(polys, crs(full))

# intersect with each mpa type
try({
  fully <- intersect(polys, full)
  highly <- intersect(polys, high)
  lightly <- intersect(polys, light)
  incomp <- intersect(polys, inc)
  na <- intersect(polys, unknown)
  proposed <- intersect(polys, prop)
})
unprotected <- polys %>% 
  erase(fully) %>% 
  erase(highly) %>% 
  erase(lightly) %>% 
  erase(incomp) %>% 
  erase(na) %>% 
  erase(proposed)

# read in CCAMLR Subareas
subareas <- readRDS("data/subareas/CCAMLR_subareas.rds")
plot(bins)
plot(subareas, add = T)

# unique subarea names
subarea_names <- unique(subareas$GAR_Name) %>% sort()

# discard subareas without any land
subarea_names <- subarea_names[!subarea_names %in% c(
  "Division 58.4.3a", "Division 58.4.3b", "Division 58.4.4a", "Division 58.4.4b",
  "Subarea 38.3", "Subarea 38.4", "Subarea 38.6"
)]

# within each subarea
for(this_subarea in subarea_names){
  
  # get subarea shape
  subarea_bound <- subareas %>%
    filter(GAR_Name == this_subarea)
  
  # isolate core habitat area within this subarea
  suppressWarnings(try(this_total <- intersect(polys, subarea_bound)))
  
  # calculate protected, proposed, and unprotected areas within this subarea
  suppressWarnings(try({
    this_fully <- intersect(subarea_bound, fully)
    this_highly <- intersect(subarea_bound, highly)
    this_lightly <- intersect(subarea_bound, lightly)
    this_incomp <- intersect(subarea_bound, incomp)
    this_na <- intersect(subarea_bound, na)
    this_proposed <- intersect(subarea_bound, proposed)
  }))
  this_unprotected <- this_total %>% 
    erase(this_fully) %>% 
    erase(this_highly) %>% 
    erase(this_lightly) %>% 
    erase(this_incomp) %>% 
    erase(this_na)
  
  # calculate areas
  if(nrow(this_total) == 0){
    present_area_km2 = 0
  } else {
    present_area_km2 <- expanse(this_total, unit = "km")
  }
  
  if(nrow(this_fully) == 0){
    fully_area_km2 = 0
  } else {
    fully_area_km2 <- expanse(this_fully, unit = "km")
  }
  
  if(nrow(this_highly) == 0){
    highly_area_km2 = 0
  } else {
    highly_area_km2 <- expanse(this_highly, unit = "km")
  }
  
  if(nrow(this_lightly) == 0){
    lightly_area_km2 = 0
  } else {
    lightly_area_km2 <- expanse(this_lightly, unit = "km")
  }
  
  if(nrow(this_incomp) == 0){
    incomp_area_km2 = 0
  } else {
    incomp_area_km2 <- expanse(this_incomp, unit = "km")
  }
  
  if(nrow(this_na) == 0){
    na_area_km2 = 0
  } else {
    na_area_km2 <- expanse(this_na, unit = "km")
  }
  
  if(nrow(this_proposed) == 0){
    proposed_area_km2 = 0
  } else {
    proposed_area_km2 <- expanse(this_proposed, unit = "km")
  }
  
  if(nrow(this_unprotected) == 0){
    unprotected_area_km2 = 0
  } else {
    unprotected_area_km2 <- expanse(this_unprotected, unit = "km")
  }
  
  # create dataframe
  subarea_df <- data.frame(
    subarea = this_subarea,
    species = species,
    ssp = "Present",
    type = c("Total", "Fully", "Highly", "Lightly", "Incompatible", "Unknown", "Proposed", "Unprotected"),
    area_km2 = c(present_area_km2, fully_area_km2, highly_area_km2, lightly_area_km2,
                  incomp_area_km2, na_area_km2, proposed_area_km2, unprotected_area_km2)
  )
  
  # combine with other subareas
  if(this_subarea == subarea_names[1]){
    present_df <- subarea_df
  } else {
    present_df <- bind_rows(present_df, subarea_df)
  }
}

#-------------------------------------------------------------------------------
# Future coverage
#-------------------------------------------------------------------------------

# list all files from combined projections
files <- list.files("output/combined/projections", recursive = T, full.names = T)

# limit to those containing gcm_core_habitat_bins
files <- files[grep("gcm_core_habitat_bins", files)]

# limit to files containing this species name
files <- files[grep(species, files)]

# loop over each file
for(file in files){
  
  # get ssp from file name
  ssp <- strsplit(file, "/")[[1]][4]
  print(ssp)
  
  # load in raster
  r <- rast(file)
  
  # for each layer
  for(i in 1:nlyr(r)){
    
    # get layer
    this_layer <- r[[i]] * 1
    
    # convert to polygons
    polys <- as.polygons(this_layer, dissolve = T) %>%
      filter(mean == 3)
    
    # project to same CRS as mpas
    polys <- project(polys, crs(full))
    
    # intersect with each mpa type
    try({
      fully <- intersect(polys, full)
      highly <- intersect(polys, high)
      lightly <- intersect(polys, light)
      incomp <- intersect(polys, inc)
      na <- intersect(polys, unknown)
      proposed <- intersect(polys, prop)
    })
    unprotected <- polys %>% 
      erase(fully) %>% 
      erase(highly) %>% 
      erase(lightly) %>% 
      erase(incomp) %>% 
      erase(na) %>% 
      erase(proposed)
    
    # within each subarea
    for(this_subarea in subarea_names){
      
      # get subarea shape
      subarea_bound <- subareas %>%
        filter(GAR_Name == this_subarea)
      
      # isolate core habitat area within this subarea
      suppressWarnings(try(this_total <- intersect(polys, subarea_bound)))
      
      # intersect with each mpa type
      suppressWarnings(try({
        this_fully <- intersect(subarea_bound, fully)
        this_highly <- intersect(subarea_bound, highly)
        this_lightly <- intersect(subarea_bound, lightly)
        this_incomp <- intersect(subarea_bound, incomp)
        this_na <- intersect(subarea_bound, na)
        this_proposed <- intersect(subarea_bound, proposed)
      }))
      this_unprotected <- this_total %>% 
        erase(this_fully) %>% 
        erase(this_highly) %>% 
        erase(this_lightly) %>% 
        erase(this_incomp) %>% 
        erase(this_na)
      
      # calculate areas
      if(nrow(this_total) == 0){
        present_area_km2 = 0
      } else {
        present_area_km2 <- expanse(this_total, unit = "km")
      }
      
      if(nrow(this_fully) == 0){
        fully_area_km2 = 0
      } else {
        fully_area_km2 <- expanse(this_fully, unit = "km")
      }
      
      if(nrow(this_highly) == 0){
        highly_area_km2 = 0
      } else {
        highly_area_km2 <- expanse(this_highly, unit = "km")
      }
      
      if(nrow(this_lightly) == 0){
        lightly_area_km2 = 0
      } else {
        lightly_area_km2 <- expanse(this_lightly, unit = "km")
      }
      
      if(nrow(this_incomp) == 0){
        incomp_area_km2 = 0
      } else {
        incomp_area_km2 <- expanse(this_incomp, unit = "km")
      }
      
      if(nrow(this_na) == 0){
        na_area_km2 = 0
      } else {
        na_area_km2 <- expanse(this_na, unit = "km")
      }
      
      if(nrow(this_proposed) == 0){
        proposed_area_km2 = 0
      } else {
        proposed_area_km2 <- expanse(this_proposed, unit = "km")
      }
      
      if(nrow(this_unprotected) == 0){
        unprotected_area_km2 = 0
      } else {
        unprotected_area_km2 <- expanse(this_unprotected, unit = "km")
      }
      
      # create dataframe
      subarea_df <- data.frame(
        subarea = this_subarea,
        species = species,
        ssp = ssp,
        gcm = i,
        type = c("Total", "Fully", "Highly", "Lightly", "Incompatible", "Unknown", "Proposed", "Unprotected"),
        area_km2 = c(present_area_km2, fully_area_km2, highly_area_km2, lightly_area_km2,
                     incomp_area_km2, na_area_km2, proposed_area_km2, unprotected_area_km2)
      )
      
      # combine with other subareas
      if(this_subarea == subarea_names[1]){
        future_df <- subarea_df
      } else {
        future_df <- bind_rows(future_df, subarea_df)
      }
    }
    
    # join to other layers
    if(i == 1){
      species_future_df <- future_df
    } else {
      species_future_df <- bind_rows(species_future_df, future_df)
    }
  }
  
  # append ssp and combine with other ssp
  if(file == files[1]){
    all_species_future_df <- species_future_df
  } else {
    all_species_future_df <- bind_rows(all_species_future_df, species_future_df)
  }
}


#-------------------------------------------------------------------------------
# Bring it all together 
#-------------------------------------------------------------------------------

# combine present and future data
data <- bind_rows(present_df %>% mutate(gcm = 10), all_species_future_df) %>%
  filter(type != "Total")

# get mean ssp126 and ssp585 values
data <- data %>%
  group_by(ssp, type, subarea) %>%
  summarise(mean_area_km2 = mean(area_km2), na.rm = T,
            sd_area_km2 = sd(area_km2), na.rm = T) %>%
  mutate(sd_area_km2 = ifelse(is.na(sd_area_km2), 0, sd_area_km2)) %>%
  mutate(lower_ci = mean_area_km2 - (1.96 * (sd_area_km2 / sqrt(8))),
         upper_ci = mean_area_km2 + (1.96 * (sd_area_km2 / sqrt(8))))

# reorder protection type levels
data$type <- factor(data$type, levels = c("Unprotected", "Proposed", "Unknown",
                                          "Incompatible", "Lightly", "Highly", 
                                          "Fully"))

# reorder ssp levels
data$ssp <- factor(data$ssp, levels = c("ssp585", "ssp126", "Present"))

# recode SSP names to capitals
data$ssp <- recode(data$ssp,
                   "ssp126" = "SSP126",
                   "ssp585" = "SSP585",
                   "Present" = "Present")

# remove subareas where there is zero area across all ssp and protection types
subareas_to_remove <- data %>%
  group_by(subarea) %>%
  summarise(total_area = sum(mean_area_km2)) %>%
  filter(total_area < 0.01*max(total_area)) %>%
  pull(subarea)
data <- data %>%
  filter(!subarea %in% subareas_to_remove)

# recode subareas to common names
names <- data.frame(subarea = c("Division 58.5.1", "Division 58.5.2", "Subarea 38.1", "Subarea 38.2",
                                 "Subarea 38.5", "Subarea 38.7", "Subarea 48.1", "Subarea 48.2",
                                 "Subarea 48.3", "Subarea 48.4", "Subarea 48.5", "Subarea 48.6",
                                 "Division 58.4.1", "Division 58.4.2", "Subarea 58.6", "Subarea 58.7",
                                 "Subarea 88.1", "Subarea 88.2", "Subarea 88.3",
                                 "Division 58.4.3a", "Division 58.4.3b", "Division 58.4.4a", "Division 58.4.4b"),
                    common_name = c("Kerguelen", "Heard", "Chile", "Bouvet",
                                    "Macquarie", "Falklands", "Antarctic Peninsula", "South Orkney",
                                    "South Georgia", "South Sandwich", "Weddell Sea", "Queen Maud Land",
                                    "Enderby-Wilkes East", "Enderby-Wilkes West", "Crozet", "Marion",
                                    "Eastern Ross Sea", "Western Ross Sea", "Amundsen-Bellingshausen Seas",
                                    "Southwest of Heard", "Southeast of Heard", "South of Marion", "South of Crozet"))
data <- data %>%
  left_join(names, by = "subarea") %>%
  select(-subarea) %>%
  rename(subarea = common_name)

# manually reorder subareas
data$subarea <- factor(data$subarea, levels = c("Falklands", "Marion", "Crozet", "Macquarie", "Kerguelen",
                                                                  "Chile", "Heard", "South Georgia", "South Sandwich", 
                                                                  "Bouvet", "South of Marion", "South of Crozet",
                                                                  "Southwest of Heard", "Southeast of Heard", 
                                                                  "South Orkney", "Antarctic Peninsula", "Amundsen-Bellingshausen Seas", 
                                                                  "Weddell Sea", "Queen Maud Land", "Enderby-Wilkes West",
                                                                  "Enderby-Wilkes East", "Eastern Ross Sea", "Western Ross Sea"
))

# export data for future reuse
saveRDS(data, paste0("output/combined/figdata/mpa_coverage_by_subarea/", species, "_mpa_coverage_by_subarea.rds"))

# read in data
data <- readRDS(paste0("output/combined/figdata/mpa_coverage_by_subarea/", species, "_mpa_coverage_by_subarea.rds"))

# plot
p1 <- ggplot(data, aes(x = ssp, y = mean_area_km2, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  coord_flip() +
  facet_wrap(~subarea, ncol = 1, strip.position = "left") +
  scale_fill_viridis_d(option = "B") +
  labs(x = "", y = "Area (km²)", fill = "Protection Type",
       title = paste0(longname)) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  +
  scale_x_discrete(expand = c(0,0)) +
  theme(panel.spacing = unit(0.4, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"), 
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.y.left = element_text(face = "bold", hjust = 1, angle = 0),
        strip.placement = "outside")
p1 + ggview::canvas(width = 10, height = 10)

# export plot
ggsave(paste0("text/figures/draft/mpa_coverage_by_subarea/", species, "_mpa_coverage_by_subarea.svg"), p1,
       width = 10, height = 10)


#-------------------------------------------------------------------------------
# Combined plot for all species
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())

# list all species mpa coverage data
files <- list.files("output/combined/figdata/mpa_coverage_by_subarea/", full.names = T)

# loop over each file and combine
for(file in files){
  df <- readRDS(file)
  species <- strsplit(basename(file), "_")[[1]][1]
  df$species <- species
  if(file == files[1]){
    data <- df
  } else {
    data <- bind_rows(data, df)
  }
}

# recode species
data$species <- recode(data$species,
                       "ADPE" = "Adelie",
                       "CHPE" = "Chinstrap",
                       "EMPE" = "Emperor",
                       "GEPE" = "Gentoo",
                       "KIPE" = "King",
                       "MAPE" = "Macaroni")

# recode protection type
data$type <- recode(data$type,
                    "Unprotected" = "Unprotected",
                    "Proposed" = "Proposed",
                    "Unknown" = "Unknown",
                    "Incompatible" = "Incompatible",
                    "Lightly" = "Lightly Protected",
                    "Highly" = "Highly Protected",
                    "Fully" = "Fully Protected")


# calculate mean area per species and ssp
by_mpa_type <- data %>%
  group_by(species, ssp, type) %>%
  summarise(area_km2 = sum(mean_area_km2, na.rm = T))

# scale by max area for the species
by_mpa_type <- by_mpa_type %>%
  group_by(species) %>%
  mutate(present_area = sum(area_km2[ssp == "Present"])) %>%
  ungroup() %>%
  mutate(scaled_area = area_km2 / present_area)

p2 <- ggplot(by_mpa_type, aes(x = ssp, y = scaled_area, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("grey30", "grey60", "#420A68", "#DD513A", "#FCA50A", "#FCFFA4", "#932667")) +
  labs(x = "", y = "Proportion of Core Habitat Area Scaled to Present Day", fill = "Protection Type") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1, strip.position = "left") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 1, 0.2))  +
  scale_x_discrete(expand = expansion(add = 1.2)) +
  theme(panel.spacing = unit(-0.01, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"), 
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.y.left = element_text(face = "bold", vjust = 1, hjust = 1, angle = 0),
        strip.placement = "outside")
p2 + ggview::canvas(width = 8, height = 8)

# export plot
ggsave("text/figures/draft/mpa_coverage/combined_mpa_coverage_specific_barplot.png", p2,
       width = 8, height = 8)


# combine all protected into one category
by_mpa_type_combined <- data %>%
  mutate(type = ifelse(type %in% c("Fully Protected", "Highly Protected", "Lightly Protected", "Incompatible", "Unknown"),
                       "Protected", as.character(type))) %>%
  group_by(species, ssp, type) %>%
  summarise(area_km2 = sum(mean_area_km2, na.rm = T))

# scale by max area for the species
by_mpa_type_combined <- by_mpa_type_combined %>%
  group_by(species) %>%
  mutate(present_area = sum(area_km2[ssp == "Present"])) %>%
  ungroup() %>%
  mutate(scaled_area = area_km2 / present_area)

# reorder protection type
by_mpa_type_combined$type <- factor(by_mpa_type_combined$type, levels = c("Unprotected", "Proposed", "Protected"))

p3 <- ggplot(by_mpa_type_combined, aes(x = ssp, y = scaled_area, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#d1495b", "#edae49", "#00798c")) +
  labs(x = "", y = "Proportion of Core Habitat Area Scaled to Present Day", fill = "Protection Type") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1, strip.position = "left") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 1, 0.2))  +
  scale_x_discrete(expand = expansion(add = 1.2)) +
  theme(panel.spacing = unit(-0.01, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"), 
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.y.left = element_text(face = "bold", vjust = 1, hjust = 1, angle = 0),
        strip.placement = "outside")
p3
p3 + ggview::canvas(width = 8, height = 8)

# export plot
ggsave("text/figures/draft/mpa_coverage/combined_mpa_coverage_barplot.png", p3,
       width = 8, height = 8)


# combine protected areas into no-take zones and others
by_mpa_type_notake <- data %>%
  mutate(type = case_when(
    type %in% c("Fully Protected", "Highly Protected") ~ "Existing MPAs: No-Take Zones",
    type %in% c("Lightly Protected", "Incompatible", "Unknown") ~ "Existing MPAs: Other",
    type == "Proposed" ~ "Proposed MPAs",
    type == "Unprotected" ~ "Unprotected Areas"
  )) %>%
  group_by(species, ssp, type) %>%
  summarise(area_km2 = sum(mean_area_km2, na.rm = T))

# scale by max area for the species
by_mpa_type_notake <- by_mpa_type_notake %>%
  group_by(species) %>%
  mutate(present_area = sum(area_km2[ssp == "Present"])) %>%
  ungroup() %>%
  mutate(scaled_area = area_km2 / present_area)

# reorder protection type
by_mpa_type_notake$type <- factor(by_mpa_type_notake$type, levels = c("Unprotected Areas", "Proposed MPAs", 
                                                                      "Existing MPAs: Other", "Existing MPAs: No-Take Zones"))
# plot
p4 <- ggplot(by_mpa_type_notake, aes(x = ssp, y = scaled_area, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#d1495b", "#edae49", "#00798c", "#003f5c"), guide = "none") +
  labs(x = "", y = "Proportion of Core Habitat Area Scaled to Present Day", fill = "Protection Type") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1, strip.position = "left") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 1, 0.2))  +
  scale_x_discrete(expand = expansion(add = 1.2)) +
  theme(panel.spacing = unit(-0.01, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"), 
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.y.left = element_text(face = "bold", vjust = 1, hjust = 1, angle = 0),
        strip.placement = "outside")
p4
p4 + ggview::canvas(width = 8, height = 10)

# export plot
ggsave("text/figures/draft/mpa_coverage/combined_mpa_coverage_notake_barplot.png", p4,
       width = 8, height = 10)

# plot proportion unscaled
p5 <- ggplot(by_mpa_type_notake, aes(x = ssp, y = scaled_area, fill = type)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#d1495b", "#edae49", "#00798c", "#003f5c"), guide = "none") +
  labs(x = "", y = "Proportion of Core Habitat Area", fill = "Protection Type") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1, strip.position = "left") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 1, 0.2))  +
  scale_x_discrete(expand = expansion(add = 1.2)) +
  theme(panel.spacing = unit(-0.01, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"), 
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.y.left = element_text(face = "bold", vjust = 1, hjust = 1, angle = 0),
        strip.placement = "outside")
p5 + ggview::canvas(width = 8, height = 10)

# export plot
ggsave("text/figures/draft/mpa_coverage/combined_mpa_coverage_notake_proportional_barplot.png", p5,
       width = 8, height = 10)

# calculate percentages of protected areas per species and ssp
percentages <- by_mpa_type_notake %>%
  group_by(species, ssp) %>%
  mutate(total_area = sum(area_km2)) %>%
  ungroup() %>%
  mutate(percentage = (area_km2 / total_area) * 100) %>%
  select(species, ssp, type, percentage)

# add a total protected row for each ssp and species
protected_totals <- percentages %>%
  filter(type %in% c("Existing MPAs: No-Take Zones", "Existing MPAs: Other")) %>%
  group_by(species, ssp) %>%
  summarise(percentage = sum(percentage)) %>%
  mutate(type = "Total Protected Areas") %>%
  select(species, ssp, type, percentage)
percentages <- bind_rows(percentages, protected_totals)
percentages <- percentages %>% arrange(species, type)

# area stats for each species/scenario
area <- data %>%
  group_by(species, ssp) %>%
  summarise(area = sum(mean_area_km2)) %>%
  ungroup() 

# scale area stats to present day
area <- area %>%
  group_by(species) %>%
  mutate(present_area = sum(area[ssp == "Present"])) %>%
  ungroup() %>%
  mutate(scaled_area = area / present_area)
