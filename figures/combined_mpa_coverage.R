#-------------------------------------------------------------------------------
# Plot protected area coverage per subarea
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# load in mpa data
imp <- readRDS("data/mpas/implemented_mpas_agg.rds") 
prop <- readRDS("data/mpas/proposed_mpas_agg.rds") 

#-------------------------------------------------------------------------------
# Present day coverage for each species
#-------------------------------------------------------------------------------

# define species
species <- "ADPE"
longname <- "Adelie Penguin"

# read in current suitability
present <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))

# list all extraction subsampled files for this species
files <- list.files("output/at-sea model/extraction/", pattern = paste0(species, " "), full.names = TRUE)
files <- files[grep("subsampled", files)]

# read in data
for(file in files){
  this_data <- readRDS(file)
  if(file == files[1]){
    data <- this_data
  } else {
    data <- bind_rows(data, this_data)
  }
}

# convert to terra
data <- data %>%
  vect(geom = c("x", "y"), crs = "epsg:4326")

# extract values to data
data$prediction <- terra::extract(present, data, ID = F)

# get dataframe of truth level and prediction
df <- data %>%
  as.data.frame() %>%
  select(pb, prediction) %>%
  mutate(pb = as.factor(pb)) %>%
  na.omit()

# get threshold using sensitivity (explore sensitivity values until no present day overprediction)
if(species == "KIPE"){
  sens_val <- 0.6
}
if(species == "CHPE"){
  sens_val <- 0.7
}
if(species == "GEPE"){
  sens_val <- 0.8
}
if(species == "MAPE"){
  sens_val <- 0.7
}
if(species == "ADPE"){
  sens_val <- 0.7
}
if(species == "EMPE"){
  sens_val <- 0.85
}

threshold <- tidysdm::optim_thresh(df$pb, df$prediction, metric = c("sensitivity", sens_val), event_level = "second")

# classify raster using threshold
mat1 <- matrix(c(0, threshold, 10,
                 threshold, 1, 20),
               ncol = 3, byrow = T)
bins <- classify(present, mat1)

# convert to polygons
polys <- as.polygons(bins, dissolve = T) %>%
  filter(mean == 20)

# project to same CRS as mpas
polys <- project(polys, crs(imp))

# intersect with each mpa type
existing <- intersect(polys, imp)
proposed <- intersect(polys, prop)
unprotected <- polys %>% erase(existing) %>% erase(proposed)

# calculate areas
existing_area_km2 <- expanse(existing, unit = "km")
proposed_area_km2 <- expanse(proposed, unit = "km")
unprotected_area_km2 <- expanse(unprotected, unit = "km")

# create dataframe
present_df <- data.frame(
  species = species,
  ssp = "Present",
  type = c("Existing", "Proposed", "Unprotected"),
  area_km2 = c(existing_area_km2, proposed_area_km2, unprotected_area_km2)
)


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
    polys <- project(polys, crs(imp))

    # intersect with each mpa type
    try(existing <- intersect(polys, imp))
    proposed <- intersect(polys, prop)
    unprotected <- polys %>% erase(existing) %>% erase(proposed)
    
    # calculate areas
    existing_area_km2 <- expanse(existing, unit = "km")
    if(length(existing_area_km2) == 0){
      existing_area_km2 <- 0
    }
    proposed_area_km2 <- expanse(proposed, unit = "km")
    if(length(proposed_area_km2) == 0){
      proposed_area_km2 <- 0
    }
    unprotected_area_km2 <- expanse(unprotected, unit = "km")
    
    # create dataframe
    future_df <- data.frame(
      species = species,
      ssp = ssp,
      type = c("Existing", "Proposed", "Unprotected"),
      area_km2 = c(existing_area_km2, proposed_area_km2, unprotected_area_km2),
      gcm = i
    )
    
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
data <- bind_rows(present_df %>% mutate(gcm = 10), all_species_future_df)

# export data for future reuse
saveRDS(data, paste0("output/combined/figdata/mpa_coverage/", species, "_mpa_coverage.rds"))

# read in data
data <- readRDS(paste0("output/combined/figdata/mpa_coverage/", species, "_mpa_coverage.rds"))

# get mean ssp126 and ssp585 values
data <- data %>%
  group_by(ssp, type) %>%
  summarise(mean_area_km2 = mean(area_km2, na.rm = T),
            sd_area_km2 = sd(area_km2, na.rm = T)) %>%
  mutate(sd_area_km2 = ifelse(is.na(sd_area_km2), 0, sd_area_km2)) %>%
  mutate(lower_ci = mean_area_km2 - (1.96 * (sd_area_km2 / sqrt(8))),
         upper_ci = mean_area_km2 + (1.96 * (sd_area_km2 / sqrt(8))))

# reorder protection type levels
data$type <- factor(data$type, levels = c("Unprotected", "Proposed", "Existing"))

# reorder ssp levels
data$ssp <- factor(data$ssp, levels = c("ssp585", "ssp126", "Present"))

# recode SSP names to capitals
data$ssp <- recode(data$ssp,
                   "ssp126" = "SSP126",
                   "ssp585" = "SSP585",
                   "Present" = "Present")

# plot
p1 <- ggplot(data, aes(x = ssp, y = mean_area_km2, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#d1495b", "#edae49", "#00798c")) +
  labs(x = "Scenario", y = "Area (km²)", fill = "Protection Type",
       title = paste0(longname)) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  +
  scale_x_discrete(expand = c(0,0)) +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"),
        strip.text = element_text(face = "bold", size = 12))
p1
p1 + ggview::canvas(width = 8, height = 4)

# export plot
ggsave(paste0("text/figures/draft/mpa_coverage/", species, "_mpa_coverage_barplot.svg"), p1,
       width = 8, height = 4)


#-------------------------------------------------------------------------------
# Combined plot for all species
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())

# list all species mpa coverage data
files <- list.files("output/combined/figdata/mpa_coverage/", full.names = T)

# loop over each file and combine
data <- files %>%
  map(readRDS) %>%
  bind_rows()

# calculate mean area per species and ssp
data <- data %>%
  group_by(species, ssp, type) %>%
  summarise(mean_area_km2 = mean(area_km2, na.rm = T),
            sd_area_km2 = sd(area_km2, na.rm = T)) %>%
  mutate(sd_area_km2 = ifelse(is.na(sd_area_km2), 0, sd_area_km2)) %>%
  mutate(lower_ci = mean_area_km2 - (1.96 * (sd_area_km2 / sqrt(8))),
         upper_ci = mean_area_km2 + (1.96 * (sd_area_km2 / sqrt(8)))) %>%
  ungroup()

# reorder protection type levels
data$type <- factor(data$type, levels = c("Unprotected", "Proposed", "Existing"))

# reorder ssp levels
data$ssp <- factor(data$ssp, levels = c("ssp585", "ssp126", "Present"))

# recode SSP names to capitals
data$ssp <- recode(data$ssp,
                   "ssp126" = "SSP126",
                   "ssp585" = "SSP585",
                   "Present" = "Present")

# scale area to the proportion of present day total area for each species
# data <- data %>%
#   group_by(species, ssp) %>%
#   mutate(total_area = sum(mean_area_km2)) %>%
#   ungroup() %>%
#   group_by(species) %>%
#   mutate(max_area = max(total_area)) %>%
#   ungroup() %>%
#   mutate(scaled_area = mean_area_km2 / max_area)
data <- data %>%
  group_by(species) %>%
  mutate(present_area = sum(mean_area_km2[ssp == "Present"])) %>%
  ungroup() %>%
  mutate(scaled_area = mean_area_km2 / present_area)

# recode species
data$species <- recode(data$species,
                       "ADPE" = "Adelie",
                       "CHPE" = "Chinstrap",
                       "EMPE" = "Emperor",
                       "GEPE" = "Gentoo",
                       "KIPE" = "King",
                       "MAPE" = "Macaroni")


# plot (change geom_bar position from "stack" to "fill" for proportions)
p2 <- ggplot(data, aes(x = ssp, y = scaled_area, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#d1495b", "#edae49", "#00798c")) +
  labs(x = "", y = "Area Scaled to Present Day", fill = "Protection Type") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1, strip.position = "left") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  +
  scale_x_discrete(expand = expansion(add = 1.2)) +
  theme(panel.spacing = unit(-0.01, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80"), 
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text.y.left = element_text(face = "bold", vjust = 1, hjust = 1, angle = 0),
        strip.placement = "outside")
p2
p2 + ggview::canvas(width = 8, height = 8)

# export plot
ggsave("text/figures/draft/mpa_coverage/combined_mpa_coverage_barplot.svg", p2,
       width = 8, height = 8)
