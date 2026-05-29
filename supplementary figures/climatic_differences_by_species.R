#-------------------------------------------------------------------------------
# Climatic Difference Plots
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)
library(CCAMLRGIS)
library(cowplot)

# define species
species <- "EMPE"


#-------------------------------------------------------------------------------
# Plot differences between present and future GCM average with model variability
#-------------------------------------------------------------------------------

# read in present day raster
present <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))

# read in future ensemble predictions for ssp126
access126 <- rast(paste0("output/climatic model/projections/ssp126/ACCESS-ESM1-5/", species, "_ACCESS-ESM1-5_ssp126_simple_ensemble.tif"))
can126 <- rast(paste0("output/climatic model/projections/ssp126/CanESM5/", species, "_CanESM5_ssp126_simple_ensemble.tif"))
cesm126 <- rast(paste0("output/climatic model/projections/ssp126/CESM2-WACCM/", species, "_CESM2-WACCM_ssp126_simple_ensemble.tif"))
hadgem126 <- rast(paste0("output/climatic model/projections/ssp126/HadGEM3-GC31-LL/", species, "_HadGEM3-GC31-LL_ssp126_simple_ensemble.tif"))
ipsl126 <- rast(paste0("output/climatic model/projections/ssp126/IPSL-CM6A-LR/", species, "_IPSL-CM6A-LR_ssp126_simple_ensemble.tif"))
mri126 <- rast(paste0("output/climatic model/projections/ssp126/MRI-ESM2-0/", species, "_MRI-ESM2-0_ssp126_simple_ensemble.tif"))
nor126 <- rast(paste0("output/climatic model/projections/ssp126/NorESM2-MM/", species, "_NorESM2-MM_ssp126_simple_ensemble.tif"))
ukesm126 <- rast(paste0("output/climatic model/projections/ssp126/UKESM1-0-LL/", species, "_UKESM1-0-LL_ssp126_simple_ensemble.tif"))

# read in future ensemble predictions for ssp585
access585 <- rast(paste0("output/climatic model/projections/ssp585/ACCESS-ESM1-5/", species, "_ACCESS-ESM1-5_ssp585_simple_ensemble.tif"))
can585 <- rast(paste0("output/climatic model/projections/ssp585/CanESM5/", species, "_CanESM5_ssp585_simple_ensemble.tif"))
cesm585 <- rast(paste0("output/climatic model/projections/ssp585/CESM2-WACCM/", species, "_CESM2-WACCM_ssp585_simple_ensemble.tif"))
hadgem585 <- rast(paste0("output/climatic model/projections/ssp585/HadGEM3-GC31-LL/", species, "_HadGEM3-GC31-LL_ssp585_simple_ensemble.tif"))
ipsl585 <- rast(paste0("output/climatic model/projections/ssp585/IPSL-CM6A-LR/", species, "_IPSL-CM6A-LR_ssp585_simple_ensemble.tif"))
mri585 <- rast(paste0("output/climatic model/projections/ssp585/MRI-ESM2-0/", species, "_MRI-ESM2-0_ssp585_simple_ensemble.tif"))
nor585 <- rast(paste0("output/climatic model/projections/ssp585/NorESM2-MM/", species, "_NorESM2-MM_ssp585_simple_ensemble.tif"))
ukesm585 <- rast(paste0("output/climatic model/projections/ssp585/UKESM1-0-LL/", species, "_UKESM1-0-LL_ssp585_simple_ensemble.tif"))

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
meanplots

# create row of difference plots
diff_legend <- get_legend(p4)
p4 <- p4 + theme(legend.position = "none")
p5 <- p5 + theme(legend.position = "none")
diffplots <- plot_grid(p4, diff_legend, p5, nrow = 1, rel_widths = c(0.4, 0.2, 0.4))
diffplots

# create row of sd plots
sd_legend <- get_legend(p6)
p6 <- p6 + theme(legend.position = "none")
p7 <- p7 + theme(legend.position = "none")
sdplots <- plot_grid(p6, sd_legend, p7, nrow = 1, rel_widths = c(0.4, 0.2, 0.4))
sdplots

# plot all together
full_grid <- plot_grid(p1, meanplots, diffplots, sdplots, ncol = 1)

# export
ggsave(paste0("output/climatic model/species plots/", species, "_mean_diff_sd.png"),
       full_grid, width = 10, height = 16)


#-------------------------------------------------------------------------------
# Plot "core habitat" in terms of environmental suitability
#-------------------------------------------------------------------------------

# reset variables
rm(list = setdiff(ls(), "species"))

# read in present day raster
present <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))

# read in colony locations and background samples
colonies <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))

# convert to terra
colonies <- colonies %>%
  vect(geom = c("x", "y"), crs = crs(present))

# extract values to data
colonies$prediction <- terra::extract(present, colonies, ID = F)

# get dataframe of truth level and prediction
df <- colonies %>%
  as.data.frame() %>%
  select(pa, prediction) %>%
  mutate(pa = as.factor(pa)) %>%
  na.omit()

# get threshold using max TSS
threshold <- tidysdm::optim_thresh(df$pa, df$prediction, metric = "tss_max", event_level = "second")

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
access126 <- rast(paste0("output/climatic model/projections/ssp126/ACCESS-ESM1-5/", species, "_ACCESS-ESM1-5_ssp126_simple_ensemble.tif"))
can126 <- rast(paste0("output/climatic model/projections/ssp126/CanESM5/", species, "_CanESM5_ssp126_simple_ensemble.tif"))
cesm126 <- rast(paste0("output/climatic model/projections/ssp126/CESM2-WACCM/", species, "_CESM2-WACCM_ssp126_simple_ensemble.tif"))
hadgem126 <- rast(paste0("output/climatic model/projections/ssp126/HadGEM3-GC31-LL/", species, "_HadGEM3-GC31-LL_ssp126_simple_ensemble.tif"))
ipsl126 <- rast(paste0("output/climatic model/projections/ssp126/IPSL-CM6A-LR/", species, "_IPSL-CM6A-LR_ssp126_simple_ensemble.tif"))
mri126 <- rast(paste0("output/climatic model/projections/ssp126/MRI-ESM2-0/", species, "_MRI-ESM2-0_ssp126_simple_ensemble.tif"))
nor126 <- rast(paste0("output/climatic model/projections/ssp126/NorESM2-MM/", species, "_NorESM2-MM_ssp126_simple_ensemble.tif"))
ukesm126 <- rast(paste0("output/climatic model/projections/ssp126/UKESM1-0-LL/", species, "_UKESM1-0-LL_ssp126_simple_ensemble.tif"))

# read in future ensemble predictions for ssp585
access585 <- rast(paste0("output/climatic model/projections/ssp585/ACCESS-ESM1-5/", species, "_ACCESS-ESM1-5_ssp585_simple_ensemble.tif"))
can585 <- rast(paste0("output/climatic model/projections/ssp585/CanESM5/", species, "_CanESM5_ssp585_simple_ensemble.tif"))
cesm585 <- rast(paste0("output/climatic model/projections/ssp585/CESM2-WACCM/", species, "_CESM2-WACCM_ssp585_simple_ensemble.tif"))
hadgem585 <- rast(paste0("output/climatic model/projections/ssp585/HadGEM3-GC31-LL/", species, "_HadGEM3-GC31-LL_ssp585_simple_ensemble.tif"))
ipsl585 <- rast(paste0("output/climatic model/projections/ssp585/IPSL-CM6A-LR/", species, "_IPSL-CM6A-LR_ssp585_simple_ensemble.tif"))
mri585 <- rast(paste0("output/climatic model/projections/ssp585/MRI-ESM2-0/", species, "_MRI-ESM2-0_ssp585_simple_ensemble.tif"))
nor585 <- rast(paste0("output/climatic model/projections/ssp585/NorESM2-MM/", species, "_NorESM2-MM_ssp585_simple_ensemble.tif"))
ukesm585 <- rast(paste0("output/climatic model/projections/ssp585/UKESM1-0-LL/", species, "_UKESM1-0-LL_ssp585_simple_ensemble.tif"))

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
e <- ext(-180.125, 179.875, -80.125, -39.875)
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
ggsave(paste0("output/climatic model/species plots/", species, "_core_habitat_change.png"),
       dualplot, width = 12, height = 10)


#-------------------------------------------------------------------------------
# Plot projected suitability differences over extant colonies
#-------------------------------------------------------------------------------

# reset variables
rm(list = setdiff(ls(), "species"))

# read in present day raster
present <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))

# read in colony locations
colonies <- readRDS(paste0("data/colonies/subareas/", species, "_colonies_subareas.RDS"))
colonies <- vect(colonies, geom = c("x", "y"), crs = "epsg:4326") %>%
  project(crs(present))

# extract present-day values to colonies
colonies$present <- terra::extract(present, colonies, ID = F)

# list all gcms
GCMs <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL",
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM",  "UKESM1-0-LL")

# for each GCM
for(gcm in GCMs){
  
  # read in ssp126 prediction
  ssp126 <- rast(paste0("output/climatic model/projections/ssp126/", gcm, "/", species, "_", gcm, "_ssp126_simple_ensemble.tif"))
  
  # read in ssp585 prediction
  ssp585 <- rast(paste0("output/climatic model/projections/ssp585/", gcm, "/", species, "_", gcm, "_ssp585_simple_ensemble.tif"))
  
  # extract future values to colonies
  colonies[[paste0(gcm, "_ssp126")]] <- terra::extract(ssp126, colonies, ID = F)
  colonies[[paste0(gcm, "_ssp585")]] <- terra::extract(ssp585, colonies, ID = F)
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

# export plot
ggsave(paste0("output/climatic model/species plots/", species, "_colony_differences.png"),
       p1, width = 10, height = 12)

# export data
saveRDS(colonies, 
        paste0("output/climatic model/figdata/", species, "_suitability_differences_data.RDS"))



#-------------------------------------------------------------------------------
# Plot projected core habitat gain and loss by subarea
#-------------------------------------------------------------------------------

# reset variables
rm(list = setdiff(ls(), "species"))

# read in present day raster
present <- rast(paste0("output/climatic model/predictions/", species, "_simple_ensemble.tif"))

# read in colony locations and background samples
colonies <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))

# convert to terra
colonies <- colonies %>%
  vect(geom = c("x", "y"), crs = crs(present))

# extract values to data
colonies$prediction <- terra::extract(present, colonies, ID = F)

# get dataframe of truth level and prediction
df <- colonies %>%
  as.data.frame() %>%
  select(pa, prediction) %>%
  mutate(pa = as.factor(pa)) %>%
  na.omit()

# get threshold using max_kappa (or max TSS?)
threshold <- tidysdm::optim_thresh(df$pa, df$prediction, metric = "tss_max", event_level = "second")

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
access126 <- rast(paste0("output/climatic model/projections/ssp126/ACCESS-ESM1-5/", species, "_ACCESS-ESM1-5_ssp126_simple_ensemble.tif"))
can126 <- rast(paste0("output/climatic model/projections/ssp126/CanESM5/", species, "_CanESM5_ssp126_simple_ensemble.tif"))
cesm126 <- rast(paste0("output/climatic model/projections/ssp126/CESM2-WACCM/", species, "_CESM2-WACCM_ssp126_simple_ensemble.tif"))
hadgem126 <- rast(paste0("output/climatic model/projections/ssp126/HadGEM3-GC31-LL/", species, "_HadGEM3-GC31-LL_ssp126_simple_ensemble.tif"))
ipsl126 <- rast(paste0("output/climatic model/projections/ssp126/IPSL-CM6A-LR/", species, "_IPSL-CM6A-LR_ssp126_simple_ensemble.tif"))
mri126 <- rast(paste0("output/climatic model/projections/ssp126/MRI-ESM2-0/", species, "_MRI-ESM2-0_ssp126_simple_ensemble.tif"))
nor126 <- rast(paste0("output/climatic model/projections/ssp126/NorESM2-MM/", species, "_NorESM2-MM_ssp126_simple_ensemble.tif"))
ukesm126 <- rast(paste0("output/climatic model/projections/ssp126/UKESM1-0-LL/", species, "_UKESM1-0-LL_ssp126_simple_ensemble.tif"))

# read in future ensemble predictions for ssp585
access585 <- rast(paste0("output/climatic model/projections/ssp585/ACCESS-ESM1-5/", species, "_ACCESS-ESM1-5_ssp585_simple_ensemble.tif"))
can585 <- rast(paste0("output/climatic model/projections/ssp585/CanESM5/", species, "_CanESM5_ssp585_simple_ensemble.tif"))
cesm585 <- rast(paste0("output/climatic model/projections/ssp585/CESM2-WACCM/", species, "_CESM2-WACCM_ssp585_simple_ensemble.tif"))
hadgem585 <- rast(paste0("output/climatic model/projections/ssp585/HadGEM3-GC31-LL/", species, "_HadGEM3-GC31-LL_ssp585_simple_ensemble.tif"))
ipsl585 <- rast(paste0("output/climatic model/projections/ssp585/IPSL-CM6A-LR/", species, "_IPSL-CM6A-LR_ssp585_simple_ensemble.tif"))
mri585 <- rast(paste0("output/climatic model/projections/ssp585/MRI-ESM2-0/", species, "_MRI-ESM2-0_ssp585_simple_ensemble.tif"))
nor585 <- rast(paste0("output/climatic model/projections/ssp585/NorESM2-MM/", species, "_NorESM2-MM_ssp585_simple_ensemble.tif"))
ukesm585 <- rast(paste0("output/climatic model/projections/ssp585/UKESM1-0-LL/", species, "_UKESM1-0-LL_ssp585_simple_ensemble.tif"))

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

# project
diff126 <- project(diff126, "epsg:6932")
diff585 <- project(diff585, "epsg:6932")

# read in the coastline
coast <- readRDS("data/coast_vect.RDS")

# load in subareas
subareas <- load_ASDs() %>%
  vect()

# mask the projections using the coastline
mask126 <- mask(diff126, as.lines(coast))
mask585 <- mask(diff585, as.lines(coast))

# for each layer of mask126
print("SSP126")
for(z in 1:nlyr(mask126)){
  print(paste0("Processing layer ", z, " of ", nlyr(mask126)))
  
  # convert to polygons
  this_poly <- as.polygons(mask126[[z]], values = T, dissolve = T)
  
  # split up with ccamlr subarea lines
  this_poly <- split(this_poly, subareas)
  
  # get nearest subarea to each polygon
  for(i in 1:nrow(this_poly)){
    
    # isolate polygon
    pt <- this_poly[i,]
    
    # calculate distance to nearest subareas
    dists <- distance(pt, subareas)
    
    # get nearest ID
    nearest_idx <- which.min(dists)
    
    # get subarea name
    pt$subarea <- subareas[nearest_idx]$GAR_Name
    
    # combine to all other polygons
    if(i == 1){
      poly_subareas <- pt
    } else {
      poly_subareas <- bind_spat_rows(poly_subareas, pt)
    }
    
    # if i is a multiple of 100, print progress
    if(i %% 100 == 0){
      print(paste0("Processed ", i, " of ", nrow(this_poly), " polygons"))
    }
  }
  
  # convert to a dataframe
  these_area_stats <- poly_subareas %>%
    as.data.frame()
  
  # append centroids of polygons
  these_area_stats <- these_area_stats %>%
    bind_cols(
      poly_subareas %>%
        centroids() %>%
        project("epsg:4326") %>%
        as.data.frame(geom = "XY") %>%
        select(x, y)
    )
  
  # override subarea assignments for polygons outside of CCAMLR
  these_area_stats <- these_area_stats %>%
    mutate(subarea = case_when(
      x > -63 & x < -55 & y > -55 & y < -50 ~ "Falklands",
      x > 155 & x < 160 & y > -57 & y < -53 ~ "Macquarie",
      x > 160 & x < 180 & y > -55 ~ "NZ Subantarctic",
      x < -170 & y > -55 ~ "NZ Subantarctic",
      x < 160 & x > 130 & y > -50 ~ "Tasmania",
      x > 0 & x < 10 & y > -55 & y < -50 ~ "Bouvet",
      x > 75 & x < 80 & y > -42 ~ "Amsterdam and St Paul",
      x > -15 & x < -5 & y > -42 ~ "Tristan da Cunha",
      x < -63 & x > -170 & y > -58 ~ "Chile", 
      x < -60 & x > -120 & y > -50 ~ "Chile",
      TRUE ~ subarea
    ))
  
  # calculate loss and gain per subarea
  these_area_stats <- these_area_stats %>%
    mutate(area = expanse(poly_subareas)) %>%
    group_by(subarea, mean) %>%
    summarise(area_km2 = sum(area)/1e6) %>%
    pivot_wider(names_from = mean, values_from = area_km2) %>%
    rename(loss = `-1`,
           no_change = `0`,
           gain = `1`) %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    mutate(original_area = loss + no_change,
           net_change = gain - loss,
           gcm = z) 
  
  # combine to all other layers
  if(z == 1){
    area_stats126 <- these_area_stats
  } else {
    area_stats126 <- rbind(area_stats126, these_area_stats)
  }
}

# for each layer of mask585
print("SSP585")
for(z in 1:nlyr(mask585)){
  print(paste0("Processing layer ", z, " of ", nlyr(mask585)))
  
  # convert to polygons
  this_poly <- as.polygons(mask585[[z]], values = T, dissolve = T)
  
  # split up with ccamlr subarea lines
  this_poly <- split(this_poly, subareas)
  
  # get nearest subarea to each polygon
  for(i in 1:nrow(this_poly)){
    
    # isolate polygon
    pt <- this_poly[i,]
    
    # calculate distance to nearest subareas
    dists <- distance(pt, subareas)
    
    # get nearest ID
    nearest_idx <- which.min(dists)
    
    # get subarea name
    pt$subarea <- subareas[nearest_idx]$GAR_Name
    
    # combine to all other polygons
    if(i == 1){
      poly_subareas <- pt
    } else {
      poly_subareas <- bind_spat_rows(poly_subareas, pt)
    }
    
    # if i is a multiple of 100, print progress
    if(i %% 100 == 0){
      print(paste0("Processed ", i, " of ", nrow(this_poly), " polygons"))
    }
  }
  
  # convert to a dataframe
  these_area_stats <- poly_subareas %>%
    as.data.frame()
  
  # append centroids of polygons
  these_area_stats <- these_area_stats %>%
    bind_cols(
      poly_subareas %>%
        centroids() %>%
        project("epsg:4326") %>%
        as.data.frame(geom = "XY") %>%
        select(x, y)
    )
  
  # override subarea assignments for polygons outside of CCAMLR
  these_area_stats <- these_area_stats %>%
    mutate(subarea = case_when(
      x > -63 & x < -55 & y > -55 & y < -50 ~ "Falklands",
      x > 155 & x < 160 & y > -57 & y < -53 ~ "Macquarie",
      x > 160 & x < 180 & y > -55 ~ "NZ Subantarctic",
      x < -170 & y > -55 ~ "NZ Subantarctic",
      x < 160 & x > 130 & y > -50 ~ "Tasmania",
      x > 0 & x < 10 & y > -55 & y < -50 ~ "Bouvet",
      x > 75 & x < 80 & y > -42 ~ "Amsterdam and St Paul",
      x > -15 & x < -5 & y > -42 ~ "Tristan da Cunha",
      x < -63 & x > -170 & y > -58 ~ "Chile", 
      x < -60 & x > -120 & y > -50 ~ "Chile",
      TRUE ~ subarea
    ))
  
  # calculate loss and gain per subarea
  these_area_stats <- these_area_stats %>%
    mutate(area = expanse(poly_subareas)) %>%
    group_by(subarea, mean) %>%
    summarise(area_km2 = sum(area)/1e6) %>%
    pivot_wider(names_from = mean, values_from = area_km2) %>%
    rename(loss = `-1`,
           no_change = `0`,
           gain = `1`) %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    mutate(original_area = loss + no_change,
           net_change = gain - loss,
           gcm = z) 
  
  # combine to all other layers
  if(z == 1){
    area_stats585 <- these_area_stats
  } else {
    area_stats585 <- rbind(area_stats585, these_area_stats)
  }
}

# combine ssp126 and ssp585 area stats
area_stats126 <- area_stats126 %>%
  mutate(SSP = "SSP126")
area_stats585 <- area_stats585 %>%
  mutate(SSP = "SSP585")
area_stats <- rbind(area_stats126, area_stats585)

# minimum and maximum net change for plotting
min_diff <- min(area_stats$net_change, na.rm = T)
max_diff <- max(area_stats$net_change, na.rm = T)
abs_diff <- max(abs(min_diff), abs(max_diff))

# calculate mean net_change per gcm and ssp
mean_diff <- area_stats %>%
  group_by(subarea, SSP) %>%
  summarise(mean_difference = mean(net_change, na.rm = T))
  
# plot area gains by GCM and subarea
p1 <- ggplot(area_stats, aes(x = subarea, y = net_change)) +
  geom_hline(yintercept = 0) +
  geom_jitter(width = 0.2, shape = 16, aes(color = net_change)) +
  geom_point(data = mean_diff, aes(x = subarea, y = mean_difference), color = "grey10", size = 4) +
  scale_color_gradient2(mid = "grey80") +
  coord_flip() +
  facet_wrap(~SSP) +
  ylim(-abs_diff, abs_diff) +
  labs(y = "Projected Change in Core Habitat (km²)",
       x = "CCAMLR Subarea",
       color = "Area Change (km²)") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.border = element_rect(fill = NA, color = "grey80", linewidth = 0.6),
        panel.background = element_rect(fill = "white"))
p1


# export plot
ggsave(paste0("output/climatic model/species plots/", species, "_area_differences.png"),
       p1, width = 10, height = 12)

# export data
saveRDS(area_stats, 
        paste0("output/climatic model/figdata/", species, "_area_differences_data.RDS"))

