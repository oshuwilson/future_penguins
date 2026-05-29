#-------------------------------------------------------------------------------
# Oceanographic Difference Plots
#-------------------------------------------------------------------------------
      
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)

# define species and stage
species <- "MAPE"
stage <- "pre-moult"


#-------------------------------------------------------------------------------
# Plot differences between present and future GCM average with model variability
#-------------------------------------------------------------------------------

# read in present day raster
present <- rast(paste0("output/at-sea model/predictions/", species, "_", stage, "_simple_ensemble.tif"))

# read in future ensemble predictions for ssp126
access126 <- rast(paste0("output/at-sea model/projections/ssp126/ACCESS-ESM1-5/", species, "_", stage, "_ACCESS-ESM1-5_ssp126_simple_ensemble.tif"))
can126 <- rast(paste0("output/at-sea model/projections/ssp126/CanESM5/", species, "_", stage, "_CanESM5_ssp126_simple_ensemble.tif"))
cesm126 <- rast(paste0("output/at-sea model/projections/ssp126/CESM2-WACCM/", species, "_", stage, "_CESM2-WACCM_ssp126_simple_ensemble.tif"))
hadgem126 <- rast(paste0("output/at-sea model/projections/ssp126/HadGEM3-GC31-LL/", species, "_", stage, "_HadGEM3-GC31-LL_ssp126_simple_ensemble.tif"))
ipsl126 <- rast(paste0("output/at-sea model/projections/ssp126/IPSL-CM6A-LR/", species, "_", stage, "_IPSL-CM6A-LR_ssp126_simple_ensemble.tif"))
mri126 <- rast(paste0("output/at-sea model/projections/ssp126/MRI-ESM2-0/", species, "_", stage, "_MRI-ESM2-0_ssp126_simple_ensemble.tif"))
nor126 <- rast(paste0("output/at-sea model/projections/ssp126/NorESM2-MM/", species, "_", stage, "_NorESM2-MM_ssp126_simple_ensemble.tif"))
ukesm126 <- rast(paste0("output/at-sea model/projections/ssp126/UKESM1-0-LL/", species, "_", stage, "_UKESM1-0-LL_ssp126_simple_ensemble.tif"))

# read in future ensemble predictions for ssp585
access585 <- rast(paste0("output/at-sea model/projections/ssp585/ACCESS-ESM1-5/", species, "_", stage, "_ACCESS-ESM1-5_ssp585_simple_ensemble.tif"))
can585 <- rast(paste0("output/at-sea model/projections/ssp585/CanESM5/", species, "_", stage, "_CanESM5_ssp585_simple_ensemble.tif"))
cesm585 <- rast(paste0("output/at-sea model/projections/ssp585/CESM2-WACCM/", species, "_", stage, "_CESM2-WACCM_ssp585_simple_ensemble.tif"))
hadgem585 <- rast(paste0("output/at-sea model/projections/ssp585/HadGEM3-GC31-LL/", species, "_", stage, "_HadGEM3-GC31-LL_ssp585_simple_ensemble.tif"))
ipsl585 <- rast(paste0("output/at-sea model/projections/ssp585/IPSL-CM6A-LR/", species, "_", stage, "_IPSL-CM6A-LR_ssp585_simple_ensemble.tif"))
mri585 <- rast(paste0("output/at-sea model/projections/ssp585/MRI-ESM2-0/", species, "_", stage, "_MRI-ESM2-0_ssp585_simple_ensemble.tif"))
nor585 <- rast(paste0("output/at-sea model/projections/ssp585/NorESM2-MM/", species, "_", stage, "_NorESM2-MM_ssp585_simple_ensemble.tif"))
ukesm585 <- rast(paste0("output/at-sea model/projections/ssp585/UKESM1-0-LL/", species, "_", stage, "_UKESM1-0-LL_ssp585_simple_ensemble.tif"))

# calculate mean and sd of projections
stack126 <- c(access126, can126, cesm126, hadgem126, ipsl126, mri126, nor126, ukesm126)
mean126 <- app(stack126, mean, na.rm=T)
sd126 <- app(stack126, sd, na.rm=T)

stack585 <- c(access585, can585, cesm585, hadgem585, ipsl585, mri585, nor585, ukesm585)
mean585 <- app(stack585, mean, na.rm=T)
sd585 <- app(stack585, sd, na.rm=T)

# crop means, sds and present day rasters
e <- ext(-180.125, 179.875, -80.125, -39.875)
present <- crop(present, e)
mean126 <- crop(mean126, e)
sd126 <- crop(sd126, e)
mean585 <- crop(mean585, e)
sd585 <- crop(sd585, e)

# project means, sds and present day rasters
present <- project(present, "epsg:6932")
mean126 <- project(mean126, "epsg:6932")
sd126 <- project(sd126, "epsg:6932")
mean585 <- project(mean585, "epsg:6932")
sd585 <- project(sd585, "epsg:6932")

# calculate difference rasters
diff126 <- mean126 - present
diff585 <- mean585 - present

# read in coastline from CCAMLR
coast <- readRDS("data/coast_vect.RDS")

# get max suitability values across all mean/present rasters
max_hs <- max(c(minmax(present)[2,], minmax(mean126)[2,], minmax(mean585)[2,]))

# plot present day suitability 
p1 <- ggplot() +
  geom_spatraster(data = present) +
  scale_fill_viridis_c(limits = c(0, max_hs), na.value = "transparent", name = "Habitat Suitability") +
  geom_spatvector(data = coast, fill = NA, col = "white") +
  theme_void() +
  ggtitle("2000-2020") +
  theme(plot.title = element_text(hjust = 0.5))
p1

# plot ssp126 suitability
p2 <- ggplot() +
  geom_spatraster(data = mean126) +
  scale_fill_viridis_c(limits = c(0, max_hs), na.value = "transparent", name = "Habitat Suitability") +
  geom_spatvector(data = coast, fill = NA, col = "white") +
  theme_void() +
  ggtitle("2080-2100 SSP126 (GCM Average)") +
  theme(plot.title = element_text(hjust = 0.5))
p2

# plot ssp585
p3 <- ggplot() +
  geom_spatraster(data = mean585) +
  scale_fill_viridis_c(limits = c(0, max_hs), na.value = "transparent", name = "Habitat Suitability") +
  geom_spatvector(data = coast, fill = NA, col = "white") +
  theme_void() +
  ggtitle("2080-2100 SSP585 (GCM Average)") +
  theme(plot.title = element_text(hjust = 0.5))
p3

# get min and max difference values
mindiff <- min(c(minmax(diff126)[1,], minmax(diff585)[1,]))
maxdiff <- max(c(minmax(diff126)[2,], minmax(diff585)[2,]))

# plot ssp126 suitability differences
p4 <- ggplot() +
  geom_spatraster(data = diff126) +
  scale_fill_gradient2(limits = c(mindiff, maxdiff), na.value = "transparent", name = expression(Delta~"Habitat Suitability"), mid = "grey90") +
  geom_spatvector(data = coast, fill = NA, col = "black") +
  theme_void() +
  ggtitle("Suitability Differences SSP126") +
  theme(plot.title = element_text(hjust = 0.5))
p4 

# plot ssp585 suitability differences
p5 <- ggplot() +
  geom_spatraster(data = diff585) +
  scale_fill_gradient2(limits = c(mindiff, maxdiff), na.value = "transparent", name = expression(Delta~"Habitat Suitability"), mid = "grey90") +
  geom_spatvector(data = coast, fill = NA, col = "black") +
  theme_void() +
  ggtitle("Suitability Differences SSP585") +
  theme(plot.title = element_text(hjust = 0.5))
p5

# get max standard deviation value
maxsd <- max(c(minmax(sd126)[2,], minmax(sd585)[2,]))

# plot ssp126 standard deviation
p6 <- ggplot() +
  geom_spatraster(data = sd126) +
  scale_fill_viridis_c(limits = c(0, maxsd), na.value = "transparent", name = "Habitat Suitability\nStandard Deviation") +
  geom_spatvector(data = coast, fill = NA, col = "white") +
  theme_void() +
  ggtitle("2080-2100 SSP126 (GCM Variability)") +
  theme(plot.title = element_text(hjust = 0.5))
p6

# plot ssp585 standard deviation
p7 <- ggplot() +
  geom_spatraster(data = sd585) +
  scale_fill_viridis_c(limits = c(0, maxsd), na.value = "transparent", name = "Habitat Suitability\nStandard Deviation") +
  geom_spatvector(data = coast, fill = NA, col = "white") +
  theme_void() +
  ggtitle("2080-2100 SSP585 (GCM Variability)") +
  theme(plot.title = element_text(hjust = 0.5))
p7

# plot all together
# create row of mean plots
mean_legend <- get_legend(p2)
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
meanplots <- plot_grid(p2, mean_legend, p3, nrow = 1, rel_widths = c(0.4, 0.2, 0.4))

# create row of difference plots
diff_legend <- get_legend(p4)
p4 <- p4 + theme(legend.position = "none")
p5 <- p5 + theme(legend.position = "none")
diffplots <- plot_grid(p4, diff_legend, p5, nrow = 1, rel_widths = c(0.4, 0.2, 0.4))

# create row of sd plots
sd_legend <- get_legend(p6)
p6 <- p6 + theme(legend.position = "none")
p7 <- p7 + theme(legend.position = "none")
sdplots <- plot_grid(p6, sd_legend, p7, nrow = 1, rel_widths = c(0.4, 0.2, 0.4))

# plot all together
full_grid <- plot_grid(p1, meanplots, diffplots, sdplots, ncol = 1)

# export
ggsave(paste0("output/at-sea model/species plots/", species, "_", stage, "_mean_diff_sd.png"),
       full_grid, width = 10, height = 16)


#-------------------------------------------------------------------------------
# Plot "core habitat" in terms of environmental suitability
#-------------------------------------------------------------------------------

# reset variables
rm(list = setdiff(ls(), c("species", "stage")))

# read in present day raster
present <- rast(paste0("output/at-sea model/predictions/", species, "_", stage, "_simple_ensemble.tif"))

# read in data
data <- readRDS(paste0("output/at-sea model/extraction/", species, " ", stage, " extracted subsampled.rds"))

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

# get threshold using max TSS
threshold <- tidysdm::optim_thresh(df$pb, df$prediction, metric = "tss_max", event_level = "second")

# classify raster using threshold
mat1 <- matrix(c(0, threshold, 10,
                 threshold, 1, 20),
               ncol = 3, byrow = T)
bins <- classify(present, mat1)
plot(bins)

# predict future binary classes for each gcm
mat2 <- matrix(c(0, threshold, 2,
                 threshold, 1, 3),
               ncol = 3, byrow = T)

# read in future ensemble predictions for ssp126
access126 <- rast(paste0("output/at-sea model/projections/ssp126/ACCESS-ESM1-5/", species, "_", stage, "_ACCESS-ESM1-5_ssp126_simple_ensemble.tif"))
can126 <- rast(paste0("output/at-sea model/projections/ssp126/CanESM5/", species, "_", stage, "_CanESM5_ssp126_simple_ensemble.tif"))
cesm126 <- rast(paste0("output/at-sea model/projections/ssp126/CESM2-WACCM/", species, "_", stage, "_CESM2-WACCM_ssp126_simple_ensemble.tif"))
hadgem126 <- rast(paste0("output/at-sea model/projections/ssp126/HadGEM3-GC31-LL/", species, "_", stage, "_HadGEM3-GC31-LL_ssp126_simple_ensemble.tif"))
ipsl126 <- rast(paste0("output/at-sea model/projections/ssp126/IPSL-CM6A-LR/", species, "_", stage, "_IPSL-CM6A-LR_ssp126_simple_ensemble.tif"))
mri126 <- rast(paste0("output/at-sea model/projections/ssp126/MRI-ESM2-0/", species, "_", stage, "_MRI-ESM2-0_ssp126_simple_ensemble.tif"))
nor126 <- rast(paste0("output/at-sea model/projections/ssp126/NorESM2-MM/", species, "_", stage, "_NorESM2-MM_ssp126_simple_ensemble.tif"))
ukesm126 <- rast(paste0("output/at-sea model/projections/ssp126/UKESM1-0-LL/", species, "_", stage, "_UKESM1-0-LL_ssp126_simple_ensemble.tif"))

# read in future ensemble predictions for ssp585
access585 <- rast(paste0("output/at-sea model/projections/ssp585/ACCESS-ESM1-5/", species, "_", stage, "_ACCESS-ESM1-5_ssp585_simple_ensemble.tif"))
can585 <- rast(paste0("output/at-sea model/projections/ssp585/CanESM5/", species, "_", stage, "_CanESM5_ssp585_simple_ensemble.tif"))
cesm585 <- rast(paste0("output/at-sea model/projections/ssp585/CESM2-WACCM/", species, "_", stage, "_CESM2-WACCM_ssp585_simple_ensemble.tif"))
hadgem585 <- rast(paste0("output/at-sea model/projections/ssp585/HadGEM3-GC31-LL/", species, "_", stage, "_HadGEM3-GC31-LL_ssp585_simple_ensemble.tif"))
ipsl585 <- rast(paste0("output/at-sea model/projections/ssp585/IPSL-CM6A-LR/", species, "_", stage, "_IPSL-CM6A-LR_ssp585_simple_ensemble.tif"))
mri585 <- rast(paste0("output/at-sea model/projections/ssp585/MRI-ESM2-0/", species, "_", stage, "_MRI-ESM2-0_ssp585_simple_ensemble.tif"))
nor585 <- rast(paste0("output/at-sea model/projections/ssp585/NorESM2-MM/", species, "_", stage, "_NorESM2-MM_ssp585_simple_ensemble.tif"))
ukesm585 <- rast(paste0("output/at-sea model/projections/ssp585/UKESM1-0-LL/", species, "_", stage, "_UKESM1-0-LL_ssp585_simple_ensemble.tif"))

# classify each gcm projection - ssp126
access126_bins <- classify(access126, mat2)
can126_bins <- classify(can126, mat2)
cesm126_bins <- classify(cesm126, mat2)
hadgem126_bins <- classify(hadgem126, mat2)
ipsl126_bins <- classify(ipsl126, mat2)
mri126_bins <- classify(mri126, mat2)
nor126_bins <- classify(nor126, mat2)
ukesm126_bins <- classify(ukesm126, mat2)

# classify each gcm projection - ssp585
access585_bins <- classify(access585, mat2)
can585_bins <- classify(can585, mat2)
cesm585_bins <- classify(cesm585, mat2)
hadgem585_bins <- classify(hadgem585, mat2)
ipsl585_bins <- classify(ipsl585, mat2)
mri585_bins <- classify(mri585, mat2)
nor585_bins <- classify(nor585, mat2)
ukesm585_bins <- classify(ukesm585, mat2)

# stack binned projections
bin_stack126 <- c(access126_bins, can126_bins, cesm126_bins, hadgem126_bins,
                  ipsl126_bins, mri126_bins, nor126_bins, ukesm126_bins)
bin_stack585 <- c(access585_bins, can585_bins, cesm585_bins, hadgem585_bins,
                  ipsl585_bins, mri585_bins, nor585_bins, ukesm585_bins)

# subtract present day from future
diff126 <- bin_stack126 - bins
diff585 <- bin_stack585 - bins

# substitute values
diff126 <- diff126 %>% 
  subst(c(-18, -17, -8, -7),
        c(-1, 0, NA, 1))
diff585 <- diff585 %>%
  subst(c(-18, -17, -8, -7),
        c(-1, 0, NA, 1))

# add values
total126 <- app(diff126, fun = "sum", na.rm = T)
total585 <- app(diff585, fun = "sum", na.rm = T)

# crop to -40 S
e <- ext(-180, 180, -80, -40)
total126 <- crop(total126, e)
total585 <- crop(total585, e)

# project
total126 <- project(total126, "epsg:6932")
total585 <- project(total585, "epsg:6932")

# read in coastline from CCAMLR
coast <- readRDS("data/coast_vect.RDS")

# plot ssp126 core habitat change
p1 <- ggplot() +
  geom_spatraster(data = total126) +
  scale_fill_steps2(na.value = "transparent", name = "Projected Core Habitat Change", 
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  geom_spatvector(data = coast, fill = NA, col = "black") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        legend.text = element_text(face = "bold")) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barwidth = 15, 
                               barheight = 0.5)) +
  ggtitle("SSP126")
p1

# plot ssp585 core habitat change
p2 <- ggplot() +
  geom_spatraster(data = total585) +
  scale_fill_steps2(na.value = "transparent", name = "Projected Core Habitat Change", 
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  geom_spatvector(data = coast, fill = NA, col = "black") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        legend.text = element_text(face = "bold")) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barwidth = 15, 
                               barheight = 0.5)) +
  ggtitle("SSP585")
p2

# plot all together
dualplot <- plot_grid(p1, p2)

# export
ggsave(paste0("output/at-sea model/species plots/", species, "_", stage, "_core_habitat_change.png"),
       dualplot, width = 12, height = 10)


#-------------------------------------------------------------------------------
# Plot projected differences over extant colonies
#-------------------------------------------------------------------------------

# reset variables
rm(list = setdiff(ls(), c("species", "stage")))

# read in present day raster
present <- rast(paste0("output/at-sea model/predictions/", species, "_", stage, "_simple_ensemble.tif"))

# read in colony locations
colonies <- readRDS(paste0("data/colonies/subareas/", species, "_colonies_subareas.RDS"))
colonies <- vect(colonies, geom = c("x", "y"), crs = "epsg:4326") %>%
  project(crs(present))

# create buffers around colonies using max distance from colony of that stage
max_dist <- readRDS(paste0("output/at-sea model/distance buffers/", species, " ", stage, " max dist.RDS"))
colonies <- terra::buffer(colonies, max_dist)

# extract present-day values to colonies
colonies$present <- terra::extract(present, colonies, fun = mean, na.rm = T, ID = F)

# list all gcms
GCMs <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL",
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM",  "UKESM1-0-LL")

# for each GCM
for(gcm in GCMs){
  
  # print initialisation
  print(gcm)
  
  # read in ssp126 prediction
  ssp126 <- rast(paste0("output/at-sea model/projections/ssp126/", gcm, "/", species, "_", stage, "_", gcm, "_ssp126_simple_ensemble.tif"))
  
  # read in ssp585 prediction
  ssp585 <- rast(paste0("output/at-sea model/projections/ssp585/", gcm, "/", species, "_", stage, "_", gcm, "_ssp585_simple_ensemble.tif"))
  
  # extract future values to colonies
  colonies[[paste0(gcm, "_ssp126")]] <- terra::extract(ssp126, colonies, fun = mean, na.rm = T, ID = F)
  colonies[[paste0(gcm, "_ssp585")]] <- terra::extract(ssp585, colonies, fun = mean, na.rm = T, ID = F)
}

# convert to dataframe
colonies <- as.data.frame(colonies)

# pivot longer to condense GCMs
colonies <- pivot_longer(colonies, c(`ACCESS-ESM1-5_ssp126`:`UKESM1-0-LL_ssp585`),
                         names_to = c("GCM", "SSP"),
                         names_sep = "_ssp",
                         values_to = "future") 

# rename SSPs to include SSP
colonies <- colonies %>%
  mutate(SSP = case_when(
    SSP == "126" ~ "SSP126",
    SSP == "585" ~ "SSP585"
  ))

# calculate differences
colonies <- colonies %>%
  mutate(difference = future - present)

# calculate mean difference per subarea
mean_diff <- colonies %>%
  group_by(subarea, SSP) %>%
  summarise(mean_difference = mean(difference, na.rm = T),
            sd_difference = sd(difference, na.rm = T)) 

# create error bar values
mean_diff <- mean_diff %>%
  mutate(lower = mean_difference - sd_difference,
         upper = mean_difference + sd_difference)

# get minimum and maximum differences for plot limits
min_diff <- min(colonies$difference, na.rm = T)
max_diff <- max(colonies$difference, na.rm = T)
abs_diff <- max(abs(min_diff), abs(max_diff)) + 0.1

# plot differences by subarea and ssp
p1 <- ggplot(colonies, aes(x = subarea, y = difference)) +
  geom_jitter(width = 0.2, shape = 16, aes(color = difference)) +
  geom_point(data = mean_diff, aes(x = subarea, y = mean_difference), color = "grey10", size = 4) +
  # geom_linerange(data = mean_diff, aes(x = subarea, y = mean_difference, ymin = lower, ymax = upper),
  #                linewidth = 1) +
  geom_hline(yintercept = 0) +
  scale_color_gradient2(mid = "grey80") +
  coord_flip() +
  facet_wrap(~SSP) +
  ylim(-abs_diff, abs_diff) +
  labs(y = "Projected Habitat Suitability Difference",
       x = "CCAMLR Subarea",
       color = "Difference") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.border = element_rect(fill = NA, color = "grey80", linewidth = 0.6),
        panel.background = element_rect(fill = "white"))
p1

# export
ggsave(paste0("output/at-sea model/species plots/", species, "_", stage, "_colony_differences.png"),
       p1, width = 10, height = 12)



