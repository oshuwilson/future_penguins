#-------------------------------------------------------------------------------
# Evaluate how future-proof proposed MPAs are under climate change scenarios
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# read in proposed MPAs
prop <- readRDS("data/mpas/proposed_mpas.rds")

# read in existing MPAs
imp <- readRDS("data/mpas/implemented_mpas_agg.rds")

# load in planning domains
domains <- readRDS("data/mpas/domains.rds")

#-------------------------------------------------------------------------------
# Antarctic Peninsula Proposal
#-------------------------------------------------------------------------------

# isolate antarctic peninsula MPA
wap <- prop %>% 
  filter(site_name == "Western Antarctic Peninsula and South Scotia Arc") %>%
  project("epsg:4326") %>%
  fillHoles()

# isolate planning domain 1
domain1 <- domains %>%
  filter(Name == "1")

# isolate implemented mpas within domain 1
imp_domain1 <- imp %>%
  intersect(domain1) 

# read in coastline
coast <- readRDS("data/coast_vect.RDS") %>%
  project("epsg:4326") %>%
  crop(ext(domain1))

# read in present day suitability bins
adpe_bins <- rast("output/combined/predictions/adpe_core_habitat_bins.tif")
chpe_bins <- rast("output/combined/predictions/chpe_core_habitat_bins.tif")
gepe_bins <- rast("output/combined/predictions/gepe_core_habitat_bins.tif")
mape_bins <- rast("output/combined/predictions/mape_core_habitat_bins.tif")
empe_bins <- rast("output/combined/predictions/empe_core_habitat_bins.tif")
kipe_bins <- rast("output/combined/predictions/kipe_core_habitat_bins.tif")

# convert to polygons
adpe_bins <- as.polygons(adpe_bins) %>%
  filter(mean == 20) %>%
  intersect(domain1)
chpe_bins <- as.polygons(chpe_bins) %>%
  filter(mean == 20) %>%
  intersect(domain1)
gepe_bins <- as.polygons(gepe_bins) %>%
  filter(mean == 20) %>%
  intersect(domain1)
mape_bins <- as.polygons(mape_bins) %>%
  filter(mean == 20) %>%
  intersect(domain1)
empe_bins <- as.polygons(empe_bins) %>%
  filter(mean == 20) %>%
  intersect(domain1)
kipe_bins <- as.polygons(kipe_bins) %>%
  filter(mean == 20) %>%
  intersect(domain1)

# visualise suitability bins and MPA
ggplot() +
  geom_spatvector(data = adpe_bins, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins, fill = "#3a0964", col = NA, alpha = 0.5) +
  geom_spatvector(data = gepe_bins, fill = "#cd3d4c", col = NA, alpha = 0.5) +
  geom_spatvector(data = mape_bins, fill = "#f5d63d", col = NA, alpha = 0.5) +
  geom_spatvector(data = empe_bins, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = kipe_bins, fill = "#f57f13", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, aes(col = name), fill = NA) + 
  geom_spatvector(data = imp_domain1, fill = NA, col = "green") +
  scale_color_manual(values = c("blue", "red"), guide = "none") +
  geom_spatvector(data = coast, fill = "black", col = "black") +
  geom_spatvector(data = domain1, fill = NA, col = "black") +
  theme_void()


# split proposed MPA into krill fishing zone and general protection zone
gpz <- wap %>%
  filter(name == "Domain 1 General Protection Zone (GPZ)")
kfz <- wap %>%
  filter(name == "Domain 1 Krill Fishing Zone (KFZ)")

# function to calculate the area for each species within each zone
calc_area_in_zones <- function(bins){
  
  # intersect with general protection zone
  bins_gpz <- bins %>%
    intersect(gpz)
  
  # intersect with krill fishing zone
  bins_kfz <- bins %>%
    intersect(kfz)
  
  # intersect with implemented mpas
  bins_imp <- bins %>%
    intersect(imp_domain1)
  
  # calculate area in each zone
  area_gpz <- expanse(bins_gpz, unit = "km")
  area_kfz <- expanse(bins_kfz, unit = "km")
  area_imp <- expanse(bins_imp, unit = "km")
  
  # return as a tibble
  return(tibble(
    area_gpz = sum(area_gpz),
    area_kfz = sum(area_kfz),
    area_imp = sum(area_imp)
  ))
}

# calculate area for each species
adpe_area <- calc_area_in_zones(adpe_bins)
chpe_area <- calc_area_in_zones(chpe_bins)
gepe_area <- calc_area_in_zones(gepe_bins)
mape_area <- calc_area_in_zones(mape_bins)
empe_area <- calc_area_in_zones(empe_bins)
kipe_area <- calc_area_in_zones(kipe_bins)

# combine results
area_results <- bind_rows(
  adpe_area %>% mutate(species = "Adelie"),
  chpe_area %>% mutate(species = "Chinstrap"),
  gepe_area %>% mutate(species = "Gentoo"),
  mape_area %>% mutate(species = "Macaroni"),
  empe_area %>% mutate(species = "Emperor"),
  kipe_area %>% mutate(species = "King")
)

# pivot area longer
area_long <- area_results %>%
  select(species, area_gpz, area_kfz, area_imp) %>%
  pivot_longer(cols = starts_with("area_"), names_to = "zone", values_to = "area_km2")

# reorder zone factor levels
area_long$zone <- factor(area_long$zone, levels = c("area_kfz", "area_gpz", "area_imp"),
                        labels = c("Krill Fishing Zone", "General Protection Zone", "Implemented MPA"))

# plot area
ggplot(area_long, aes(x = species, y = area_km2)) +
  geom_bar(stat = "identity", aes(fill = zone), position = "stack") +
  labs(x = "Species", y = "Area of Core Habitat Protected (km²)", fill = "Zone") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")


#-------------------------------------------------------------------------------
# Integrate future projections
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_126 <- rast("output/combined/projections/ssp126/ADPE_gcm_core_habitat_bins.tif")
adpe_bins_585 <- rast("output/combined/projections/ssp585/ADPE_gcm_core_habitat_bins.tif")
chpe_bins_126 <- rast("output/combined/projections/ssp126/CHPE_gcm_core_habitat_bins.tif")
chpe_bins_585 <- rast("output/combined/projections/ssp585/CHPE_gcm_core_habitat_bins.tif")
gepe_bins_126 <- rast("output/combined/projections/ssp126/GEPE_gcm_core_habitat_bins.tif")
gepe_bins_585 <- rast("output/combined/projections/ssp585/GEPE_gcm_core_habitat_bins.tif")
mape_bins_126 <- rast("output/combined/projections/ssp126/MAPE_gcm_core_habitat_bins.tif")
mape_bins_585 <- rast("output/combined/projections/ssp585/MAPE_gcm_core_habitat_bins.tif")
empe_bins_126 <- rast("output/combined/projections/ssp126/EMPE_gcm_core_habitat_bins.tif")
empe_bins_585 <- rast("output/combined/projections/ssp585/EMPE_gcm_core_habitat_bins.tif")
kipe_bins_126 <- rast("output/combined/projections/ssp126/KIPE_gcm_core_habitat_bins.tif")
kipe_bins_585 <- rast("output/combined/projections/ssp585/KIPE_gcm_core_habitat_bins.tif")


# function to calculate the area for each species within each zone
calc_area_in_zones <- function(bins, scenario){
  
  # for each layer of bins
  for(i in 1:nlyr(bins)){
    # isolate layer
    bin_layer <- bins[[i]] * 1
    
    # convert to polygons
    bin_polys <- as.polygons(bin_layer) %>%
      filter(mean == 3) %>%
      intersect(domain1)
    
    suppressWarnings({
      # intersect with general protection zone
      bins_gpz <- bin_polys %>%
        intersect(gpz)
      
      # intersect with krill fishing zone
      bins_kfz <- bin_polys %>%
        intersect(kfz)
      
      # intersect with implemented mpas
      bins_imp <- bin_polys %>%
        intersect(imp_domain1)
    })
    
    # calculate area in each zone
    area_gpz <- expanse(bins_gpz, unit = "km")
    area_kfz <- expanse(bins_kfz, unit = "km")
    area_imp <- expanse(bins_imp, unit = "km")
    
    # create tibble
    area_tibble <- tibble(
      area_gpz = sum(area_gpz),
      area_kfz = sum(area_kfz),
      area_imp = sum(area_imp),
      gcm = i
    )
    
    # join to all other layers
    if(i == 1){
      all_areas <- area_tibble
    } else {
      all_areas <- bind_rows(all_areas, area_tibble)
    }
  }
  
  # calculate average across gcms
  avg_areas <- all_areas %>%
    summarise(
      area_gpz = mean(area_gpz),
      area_kfz = mean(area_kfz),
      area_imp = mean(area_imp)
    ) %>%
    mutate(scenario = scenario)
  
  # return all areas
  return(avg_areas)
}

# calculate area for each species and scenario
adpe_area_126 <- calc_area_in_zones(adpe_bins_126, "ssp126")
adpe_area_585 <- calc_area_in_zones(adpe_bins_585, "ssp585")
chpe_area_126 <- calc_area_in_zones(chpe_bins_126, "ssp126")
chpe_area_585 <- calc_area_in_zones(chpe_bins_585, "ssp585")
gepe_area_126 <- calc_area_in_zones(gepe_bins_126, "ssp126")
gepe_area_585 <- calc_area_in_zones(gepe_bins_585, "ssp585")
mape_area_126 <- calc_area_in_zones(mape_bins_126, "ssp126")
mape_area_585 <- calc_area_in_zones(mape_bins_585, "ssp585")
empe_area_126 <- calc_area_in_zones(empe_bins_126, "ssp126")
empe_area_585 <- calc_area_in_zones(empe_bins_585, "ssp585")
kipe_area_126 <- calc_area_in_zones(kipe_bins_126, "ssp126")
kipe_area_585 <- calc_area_in_zones(kipe_bins_585, "ssp585")

# combine results
future_area_results <- bind_rows(
  adpe_area_126 %>% mutate(species = "Adelie"),
  adpe_area_585 %>% mutate(species = "Adelie"),
  chpe_area_126 %>% mutate(species = "Chinstrap"),
  chpe_area_585 %>% mutate(species = "Chinstrap"),
  gepe_area_126 %>% mutate(species = "Gentoo"),
  gepe_area_585 %>% mutate(species = "Gentoo"),
  mape_area_126 %>% mutate(species = "Macaroni"),
  mape_area_585 %>% mutate(species = "Macaroni"),
  empe_area_126 %>% mutate(species = "Emperor"),
  empe_area_585 %>% mutate(species = "Emperor"),
  kipe_area_126 %>% mutate(species = "King"),
  kipe_area_585 %>% mutate(species = "King")
)

# pivot area longer
future_area_long <- future_area_results %>%
  select(species, scenario, area_gpz, area_kfz, area_imp) %>%
  pivot_longer(cols = starts_with("area_"), names_to = "zone", values_to = "area_km2")

# reorder zone factor levels
future_area_long$zone <- factor(future_area_long$zone, levels = c("area_kfz", "area_gpz", "area_imp"),
                        labels = c("Krill Fishing Zone", "General Protection Zone", "Implemented MPA"))

# combine with present day results
area_long <- area_long %>%
  mutate(scenario = "present") %>%
  bind_rows(future_area_long)

# plot
ggplot(area_long, aes(x = scenario, y = area_km2)) +
  geom_bar(stat = "identity", aes(fill = zone), position = "stack") +
  labs(x = "Species", y = "Area of Core Habitat Protected (km²)", fill = "Zone") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~species)
