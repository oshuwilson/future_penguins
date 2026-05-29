#-------------------------------------------------------------------------------
# Schematic for CMIP6 Delta Method
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)

#-------------------------------------------------------------------------------
# 1. CMIP6 Climatologies and Delta
#-------------------------------------------------------------------------------

# load CMIP6 historical data
hist <- rast("E:/cmip6_data/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/Omon/tos/gn/20190429/tos_Omon_CanESM5_historical_r1i1p1f1_gn_185001-201412__climatology.nc")

# limit to january
hist <- hist[[1]]

# load CMIP6 ssp585 data
ssp585 <- rast("E:/cmip6_data/CMIP6/ScenarioMIP/CCCma/CanESM5/ssp585/r1i1p1f1/Omon/tos/gn/20190429/tos_Omon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012__climatology.nc")

# limit to january
ssp585 <- ssp585[[1]]

# calculate delta
delta <- ssp585 - hist

# crop
e <- ext(-180, 180, -90, -40)
hist <- crop(hist, e)
ssp585 <- crop(ssp585, e)
delta <- crop(delta, e)

# project
hist <- project(hist, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")
delta <- project(delta, "epsg:6932")

# load in coast
coast <- readRDS("data/coast_ice_vect.RDS")

# get minmax values
min <- min(c(minmax(hist)[1,], minmax(ssp585)[1,]))
max <- max(c(minmax(hist)[2,], minmax(ssp585)[2,]))

# plot
p1 <- ggplot() +
  geom_spatraster(data = hist) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none",
                       limits = c(min, max)) +
  theme_void()
p2 <- ggplot() +
  geom_spatraster(data = ssp585) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none",
                       limits = c(min, max))+
  theme_void()
p3 <- ggplot() +
  geom_spatraster(data = delta) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_gradient2(na.value = "white", low = "steelblue4", high = "red4",
                       guide = "none") +
  theme_void()

# export
ggsave("text/figures/draft/schematic_deltas/1. CanESM Historical.png",
       p1, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/2. CanESM SSP585.png",
       p2, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/3. CanESM Delta.png",
       p3, width = 8, height = 8)

#-------------------------------------------------------------------------------
# 2. Observational Data Transformed
#-------------------------------------------------------------------------------

# load observational data (transformed)
obs <- rast("E:/cmip6_data/CMIP6/deltas/tos/satellite_data/transformed/CanESM5_ssp585_glorysres.tif")

# limit to january
obs <- obs[[month(time(obs)) == 1]]

# mean
obs <- app(obs, mean, na.rm = T)

# crop
obs <- crop(obs, e)

# project
obs <- project(obs, "epsg:6932")

# plot
p4 <- ggplot() +
  geom_spatraster(data = obs) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none",
                       limits = c(min, max)) +
  theme_void()

# export
ggsave("text/figures/draft/schematic_deltas/4. Observations Transformed.png",
       p4, width = 8, height = 8)


#-------------------------------------------------------------------------------
# 3. Algorithm Ensemble Mean
#-------------------------------------------------------------------------------

# load algorithm-specific projections
bart <- rast("output/at-sea model/projections/ssp585/CanESM5/KIPE_chick-rearing_CanESM5_ssp585_bart_prediction.tif")
brt <- rast("output/at-sea model/projections/ssp585/CanESM5/KIPE_chick-rearing_CanESM5_ssp585_brt_prediction.tif")
rf <- rast("output/at-sea model/projections/ssp585/CanESM5/KIPE_chick-rearing_CanESM5_ssp585_rf_prediction.tif")
gam <- rast("output/at-sea model/projections/ssp585/CanESM5/KIPE_chick-rearing_CanESM5_ssp585_gam_prediction.tif")

# load simple ensemble
ensemble <- rast("output/at-sea model/projections/ssp585/CanESM5/KIPE_chick-rearing_CanESM5_ssp585_simple_ensemble.tif")

# project
bart <- project(bart, "epsg:6932")
brt <- project(brt, "epsg:6932")
rf <- project(rf, "epsg:6932")
gam <- project(gam, "epsg:6932")
ensemble <- project(ensemble, "epsg:6932")

# plot
p5 <- ggplot() +
  geom_spatraster(data = bart) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
p6 <- ggplot() +
  geom_spatraster(data = brt) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
p7 <- ggplot() +
  geom_spatraster(data = rf) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
p8 <- ggplot() +
  geom_spatraster(data = gam) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
p9 <- ggplot() +
  geom_spatraster(data = ensemble) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()

# export
ggsave("text/figures/draft/schematic_deltas/5. BART Projection.png",
       p5, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/6. BRT Projection.png",
       p6, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/7. RF Projection.png",
       p7, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/8. GAM Projection.png",
       p8, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/9. Simple Ensemble Projection.png",
       p9, width = 8, height = 8)

#-------------------------------------------------------------------------------
# GCM Ensemble
#-------------------------------------------------------------------------------

# load simple ensembles for the other GCMs
ensemble2 <- rast("output/at-sea model/projections/ssp585/UKESM1-0-LL/KIPE_chick-rearing_UKESM1-0-LL_ssp585_simple_ensemble.tif")
ensemble3 <- rast("output/at-sea model/projections/ssp585/NorESM2-MM/KIPE_chick-rearing_NorESM2-MM_ssp585_simple_ensemble.tif")
ensemble4 <- rast("output/at-sea model/projections/ssp585/ACCESS-ESM1-5/KIPE_chick-rearing_ACCESS-ESM1-5_ssp585_simple_ensemble.tif")
ensemble5 <- rast("output/at-sea model/projections/ssp585/CESM2-WACCM/KIPE_chick-rearing_CESM2-WACCM_ssp585_simple_ensemble.tif")
ensemble6 <- rast("output/at-sea model/projections/ssp585/HadGEM3-GC31-LL/KIPE_chick-rearing_HadGEM3-GC31-LL_ssp585_simple_ensemble.tif")
ensemble7 <- rast("output/at-sea model/projections/ssp585/IPSL-CM6A-LR/KIPE_chick-rearing_IPSL-CM6A-LR_ssp585_simple_ensemble.tif")
ensemble8 <- rast("output/at-sea model/projections/ssp585/MRI-ESM2-0/KIPE_chick-rearing_MRI-ESM2-0_ssp585_simple_ensemble.tif")

# project
ensemble2 <- project(ensemble2, "epsg:6932")
ensemble3 <- project(ensemble3, "epsg:6932")
ensemble4 <- project(ensemble4, "epsg:6932")
ensemble5 <- project(ensemble5, "epsg:6932")
ensemble6 <- project(ensemble6, "epsg:6932")
ensemble7 <- project(ensemble7, "epsg:6932")
ensemble8 <- project(ensemble8, "epsg:6932")

# plot
px1 <- ggplot() +
  geom_spatraster(data = ensemble2) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px2 <- ggplot() +
  geom_spatraster(data = ensemble3) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px3 <- ggplot() +
  geom_spatraster(data = ensemble4) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px4 <- ggplot() +
  geom_spatraster(data = ensemble5) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px5 <- ggplot() +
  geom_spatraster(data = ensemble6) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px6 <- ggplot() +
  geom_spatraster(data = ensemble7) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px7 <- ggplot() +
  geom_spatraster(data = ensemble8) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()

# export
ggsave("text/figures/draft/schematic_deltas/X1. UKESM1-0-LL Simple Ensemble Projection.png",
       px1, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/X2. NorESM2-MM Simple Ensemble Projection.png",
       px2, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/X3. ACCESS-ESM1-5 Simple Ensemble Projection.png",
       px3, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/X4. CESM2-WACCM Simple Ensemble Projection.png",
       px4, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/X5. HadGEM3-GC31-LL Simple Ensemble Projection.png",
       px5, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/X6. IPSL-CM6A-LR Simple Ensemble Projection.png",
       px6, width = 8, height = 8)
ggsave("text/figures/draft/schematic_deltas/X7. MRI-ESM2-0 Simple Ensemble Projection.png",
       px7, width = 8, height = 8)

# average of ensembles
gcm_ensemble <- app(c(ensemble, ensemble2, ensemble3, ensemble4,
                      ensemble5, ensemble6, ensemble7, ensemble8), 
                    mean, na.rm = T)

# plot
px8 <- ggplot() +
  geom_spatraster(data = gcm_ensemble) +
  geom_spatvector(data = coast, fill = "white", color = NA) +
  scale_fill_viridis_c(na.value = "white", guide = "none") +
  theme_void()
px8 + ggview::canvas(width = 8, height = 8)

# export
ggsave("text/figures/draft/schematic_deltas/X3. GCM Ensemble Projection.png",
       px8, width = 8, height = 8)
