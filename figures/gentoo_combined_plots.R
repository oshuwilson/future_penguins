#-------------------------------------------------------------------------------
# Plot gentoo present and future combined outputs
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(rnaturalearth)

# define species
species <- "GEPE"

# read in present day suitability bins for plotting
bins <- rast(paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"))

# read in present day suitability for plotting
mean_hs_av <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))

# vectorise bins for plot
bins_vect <- bins %>%
  as.polygons() %>%
  filter(mean == 20)

# isolate key regions for plotting

# chile/falklands
fk <- mean_hs_av %>% crop(ext(-75, -55, -58, -50))

# antarctic peninsula
ap <- mean_hs_av %>% crop(ext(-80, -50, -72, -60))

# south atlantic islands
sa <- mean_hs_av %>% crop(ext(-48, -25, -62, -52))

# marion/crozet
mc <- mean_hs_av %>% crop(ext(36, 55, -48, -44))

# kerguelen/heard
kh <- mean_hs_av %>% crop(ext(65, 76, -55, -47))

# macquarie
mq <- mean_hs_av %>% crop(ext(156, 162, -56, -53))

# create bounding box for each region
fk_bbox <- ext(-75, -55, -58, -50) %>% as.polygons()
ap_bbox <- ext(-80, -50, -72, -60) %>% as.polygons()
sa_bbox <- ext(-48, -25, -62, -52) %>% as.polygons()
mc_bbox <- ext(36, 55, -48, -44) %>% as.polygons()
kh_bbox <- ext(65, 76, -55, -47) %>% as.polygons()
mq_bbox <- ext(156, 162, -56, -53) %>% as.polygons()

# combine bboxes
bboxes <- rbind(fk_bbox, ap_bbox, sa_bbox, mc_bbox, kh_bbox, mq_bbox)

# project to epsg:6932 for plotting
crs(bboxes) <- "epsg:4326"
bboxes_proj <- bboxes %>% project("epsg:6932")
plot(bboxes_proj)

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
mean_hs_av_proj <- mean_hs_av %>% project("epsg:6932")
bins_vect_proj <- bins_vect %>% project("epsg:6932")
p1 <- ggplot() +
  geom_spatraster(data = mean_hs_av_proj) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  geom_spatvector(data = bboxes_proj, fill = NA, col = "white", linewidth = 1) +
  scale_fill_viridis_c(na.value = "white", option = "D", guide = "none") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
p1 + ggview::canvas(width = 8, height = 8)

# export
ggsave(paste0("output/imagery/combined suitability/", species, "_suitability.png"),
       plot = p1,
       width = 8, height = 8, units = "in", dpi = 300)

# get max suitability value from mean_hs_av
max_val <- minmax(mean_hs_av)[2,]

# read in countries from rnaturalearth
countries <- ne_countries(returnclass = "sv", scale = 10)

# plot each region separately
p_fk <- ggplot() +
  geom_spatraster(data = fk) +
  geom_spatvector(data = countries %>% crop(ext(fk)), col = NA, fill = "white") +
  geom_spatvector(data = bins_vect %>% crop(ext(fk)), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_fk + ggview::canvas(width = 4, height = 3)

p_ap <- ggplot() +
  geom_spatraster(data = ap) +
  geom_spatvector(data = countries %>% crop(ext(ap)), col = NA, fill = "white") +
  geom_spatvector(data = bins_vect %>% crop(ext(ap)), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_ap + ggview::canvas(width = 4, height = 4)

p_sa <- ggplot() +
  geom_spatraster(data = sa) +
  geom_spatvector(data = countries %>% crop(ext(sa)), col = NA, fill = "white") +
   geom_spatvector(data = bins_vect %>% crop(ext(sa)), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_sa + ggview::canvas(width = 3.8, height = 3)

p_mc <- ggplot() +
  geom_spatraster(data = mc) +
  geom_spatvector(data = countries %>% crop(ext(mc)), col = NA, fill = "white") +
   geom_spatvector(data = bins_vect %>% crop(ext(mc)), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_mc + ggview::canvas(width = 4, height = 1.5)

p_kh <- ggplot() +
  geom_spatraster(data = kh) +
  geom_spatvector(data = countries %>% crop(ext(kh)), col = NA, fill = "white") +
   geom_spatvector(data = bins_vect %>% crop(ext(kh)), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_kh + ggview::canvas(width = 3.5, height = 4)

p_mq <- ggplot() +
  geom_spatraster(data = mq) +
  geom_spatvector(data = countries %>% crop(ext(mq)), col = NA, fill = "white") +
   geom_spatvector(data = bins_vect %>% crop(ext(mq)), fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_mq + ggview::canvas(width = 3, height = 2.7)


# export plots 
ggsave(paste0("output/imagery/combined suitability/gentoo/", species, "_suitability_fk.png"),
       plot = p_fk,
       width = 4, height = 3, units = "in", dpi = 300)
ggsave(paste0("output/imagery/combined suitability/gentoo/", species, "_suitability_ap.png"),
       plot = p_ap,
       width = 4, height = 4, units = "in", dpi = 300)
ggsave(paste0("output/imagery/combined suitability/gentoo/", species, "_suitability_sa.png"),
       plot = p_sa,
       width = 3.8, height = 3, units = "in", dpi = 300)
ggsave(paste0("output/imagery/combined suitability/gentoo/", species, "_suitability_mc.png"),
       plot = p_mc,
       width = 4, height = 1.5, units = "in", dpi = 300)
ggsave(paste0("output/imagery/combined suitability/gentoo/", species, "_suitability_kh.png"),
       plot = p_kh,
       width = 3.5, height = 4, units = "in", dpi = 300)
ggsave(paste0("output/imagery/combined suitability/gentoo/", species, "_suitability_mq.png"),
       plot = p_mq,
       width = 3, height = 2.7, units = "in", dpi = 300)



#-------------------------------------------------------------------------------
# Future 
#-------------------------------------------------------------------------------

# 1. Future Suitability

# define scenario
scenario <- "ssp585"

# gcms
gcms <-  c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
           "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")
  
# clean up
rm(list=setdiff(ls(), c("species", "scenario", "longname", "gcms")))

# loop over each gcm and read in mean suitability
for(gcm in gcms){
  
  # read in mean suitability
  mean_hs_av <- rast(paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_mean_combined_suitability.tif"))
  
  # stack with others
  if(gcm == gcms[1]){
    combined_stack <- mean_hs_av
  } else {
    combined_stack <- c(combined_stack, mean_hs_av)
  }
}

# mean across GCMs
mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)

# read in countries for plotting
countries <- ne_countries(returnclass = "sv", scale = 10)

# read in threshold to outline average core habitat
threshold <- readRDS(paste0("output/combined/thresholds/", species, "_threshold.RDS"))

# classify raster using threshold
mat1 <- matrix(c(0, threshold, 10,
                 threshold, 1, 20),
               ncol = 3, byrow = T)
bins <- classify(mean_hs_av, mat1)

# convert to vector
bins_vect <- bins %>%
  as.polygons(dissolve = T) %>%
  filter(mean == 20) 

# get max value
max_val <- minmax(mean_hs_av)[2,]

# plot falklands
fk <- mean_hs_av %>% crop(ext(-75, -55, -58, -50))
fk_bins <- bins_vect %>% crop(ext(-75, -55, -58, -50))
p_fk <- ggplot() +
  geom_spatraster(data = fk) +
  geom_spatvector(data = countries %>% crop(ext(fk)), col = NA, fill = "white") +
  geom_spatvector(data = fk_bins, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_fk + ggview::canvas(width = 4, height = 3)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_suitability_fk_.png"),
       plot = p_fk,
       width = 4, height = 3, units = "in", dpi = 300)

# plot antarctic peninsula
ap <- mean_hs_av %>% crop(ext(-80, -50, -72, -60))
ap_bins <- bins_vect %>% crop(ext(-80, -50, -72, -60))
p_ap <- ggplot() +
  geom_spatraster(data = ap) +
  geom_spatvector(data = countries %>% crop(ext(ap)), col = NA, fill = "white") +
  geom_spatvector(data = ap_bins, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_ap + ggview::canvas(width = 4, height = 4)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_suitability_ap_.png"),
       plot = p_ap,
       width = 4, height = 4, units = "in", dpi = 300)

# plot south atlantic islands
sa <- mean_hs_av %>% crop(ext(-48, -25, -62, -52))
sa_bins <- bins_vect %>% crop(ext(-48, -25, -62, -52))
p_sa <- ggplot() +
  geom_spatraster(data = sa) +
  geom_spatvector(data = countries %>% crop(ext(sa)), col = NA, fill = "white") +
  geom_spatvector(data = sa_bins, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_sa + ggview::canvas(width = 3.8, height = 3)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_suitability_sa_.png"),
       plot = p_sa,
       width = 3.8, height = 3, units = "in", dpi = 300)

# plot marion/crozet
mc <- mean_hs_av %>% crop(ext(36, 55, -48, -44))
mc_bins <- bins_vect %>% crop(ext(36, 55, -48, -44))
p_mc <- ggplot() +
  geom_spatraster(data = mc) +
  geom_spatvector(data = countries %>% crop(ext(mc)), col = NA, fill = "white") +
  geom_spatvector(data = mc_bins, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_mc + ggview::canvas(width = 4, height = 1.5)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_suitability_mc_.png"),
       plot = p_mc,
       width = 4, height = 1.5, units = "in", dpi = 300)

# plot kerguelen/heard
kh <- mean_hs_av %>% crop(ext(65, 76, -55, -47))
kh_bins <- bins_vect %>% crop(ext(65, 76, -55, -47))
p_kh <- ggplot() +
  geom_spatraster(data = kh) +
  geom_spatvector(data = countries %>% crop(ext(kh)), col = NA, fill = "white") +
  geom_spatvector(data = kh_bins, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_kh + ggview::canvas(width = 3.5, height = 4)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_suitability_kh_.png"),
       plot = p_kh,
       width = 3.5, height = 4, units = "in", dpi = 300)

# plot macquarie
mq <- mean_hs_av %>% crop(ext(156, 162, -56, -53))
mq_bins <- bins_vect %>% crop(ext(156, 162, -56, -53))
p_mq <- ggplot() +
  geom_spatraster(data = mq) +
  geom_spatvector(data = countries %>% crop(ext(mq)), col = NA, fill = "white") +
  geom_spatvector(data = mq_bins, fill = NA, col = "white") +
  scale_fill_viridis_c(na.value = "white", option = "D", limits = c(0, max_val), guide = "none") +
  theme_void()
p_mq + ggview::canvas(width = 3, height = 2.7)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_suitability_mq_.png"),
       plot = p_mq,
       width = 3, height = 2.7, units = "in", dpi = 300)


# 2. Difference

# read in current suitability
present <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))

# calculate difference
diff <- mean_hs_av - present

# get max absolute value
max_val <- abs(c(minmax(diff)[1,1], minmax(diff)[2, 1])) %>%
  max()

# plot difference for falklands
fk_diff <- diff %>% crop(ext(-75, -55, -58, -50))
p_fkd <- ggplot() +
  geom_spatraster(data = fk_diff) +
  geom_spatvector(data = countries %>% crop(ext(fk_diff)), col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       guide = "none") +
  theme_void()
p_fkd + ggview::canvas(width = 4, height = 3)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_diff_fk.png"),
       plot = p_fkd,
       width = 4, height = 3, units = "in", dpi = 300)

# plot difference for antarctic peninsula
ap_diff <- diff %>% crop(ext(-80, -50, -72, -60))
p_apd <- ggplot() +
  geom_spatraster(data = ap_diff) +
  geom_spatvector(data = countries %>% crop(ext(ap_diff)), col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       guide = "none") +
  theme_void()
p_apd + ggview::canvas(width = 4, height = 4)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_diff_ap.png"),
       plot = p_apd,
       width = 4, height = 4, units = "in", dpi = 300)

# plot difference for south atlantic islands
sa_diff <- diff %>% crop(ext(-48, -25, -62, -52))
p_sad <- ggplot() +
  geom_spatraster(data = sa_diff) +
  geom_spatvector(data = countries %>% crop(ext(sa_diff)), col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       guide = "none") +
  theme_void()
p_sad + ggview::canvas(width = 3.8, height = 3)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_diff_sa.png"),
       plot = p_sad,
       width = 3.8, height = 3, units = "in", dpi = 300)

# plot difference for marion/crozet
mc_diff <- diff %>% crop(ext(36, 55, -48, -44))
p_mcd <- ggplot() +
  geom_spatraster(data = mc_diff) +
  geom_spatvector(data = countries %>% crop(ext(mc_diff)), col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       guide = "none") +
  theme_void()
p_mcd + ggview::canvas(width = 4, height = 1.5)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_diff_mc.png"),
       plot = p_mcd,
       width = 4, height = 1.5, units = "in", dpi = 300)

# plot difference for kerguelen/heard
kh_diff <- diff %>% crop(ext(65, 76, -55, -47))
p_khd <- ggplot() +
  geom_spatraster(data = kh_diff) +
  geom_spatvector(data = countries %>% crop(ext(kh_diff)), col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       guide = "none") +
  theme_void()
p_khd + ggview::canvas(width = 3.5, height = 4)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_diff_kh.png"),
       plot = p_khd,
       width = 3.5, height = 4, units = "in", dpi = 300)

# plot difference for macquarie
mq_diff <- diff %>% crop(ext(156, 162, -56, -53))
p_mqd <- ggplot() +
  geom_spatraster(data = mq_diff) +
  geom_spatvector(data = countries %>% crop(ext(mq_diff)), col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       guide = "none") +
  theme_void()
p_mqd + ggview::canvas(width = 3, height = 2.7)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_diff_mq.png"),
       plot = p_mqd,
       width = 3, height = 2.7, units = "in", dpi = 300)


# 3. Model Agreement

total <- rast(paste0("output/combined/projections/", scenario, "/", species, "_core_habitat_change.tif"))

ggplot() +
  geom_spatraster(data = total) +
  scale_fill_steps2(na.value = "transparent", guide = "none", 
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void() 

# plot falklands
fk_total <- total %>% crop(ext(-75, -55, -58, -50))
fk_bbox <- ext(-75, -55, -58, -50) %>% as.polygons()
crs(fk_bbox) <- "epsg:4326"
p_fkt <- ggplot() +
  geom_spatraster(data = fk_total) +
  geom_spatvector(data = countries %>% crop(ext(fk_total)) %>% aggregate(), 
                  col = "black", fill = "white") +
  geom_spatvector(data = fk_bbox, fill = NA, col = "black", linewidth = .5) +
  scale_fill_steps2(na.value = "transparent", guide = "none",
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void() 
p_fkt + ggview::canvas(width = 4, height = 3)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_agreement_fk.png"),
       plot = p_fkt,
       width = 4, height = 3, units = "in", dpi = 300)

# plot antarctic peninsula
ap_total <- total %>% crop(ext(-80, -50, -72, -60))
ap_bbox <- ext(-80, -50, -72, -60) %>% as.polygons()
crs(ap_bbox) <- "epsg:4326"
p_apt <- ggplot() +
  geom_spatraster(data = ap_total) +
  geom_spatvector(data = countries %>% crop(ext(ap_total)) %>% aggregate(), 
                  col = "black", fill = "white") +
  geom_spatvector(data = ap_bbox, fill = NA, col = "black", linewidth = .5) +
  scale_fill_steps2(na.value = "transparent", guide = "none",
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void()
p_apt + ggview::canvas(width = 4, height = 4)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_agreement_ap.png"),
       plot = p_apt,
       width = 4, height = 4, units = "in", dpi = 300)

# plot south atlantic islands
sa_total <- total %>% crop(ext(-48, -25, -62, -52))
sa_bbox <- ext(-48, -25, -62, -52) %>% as.polygons()
crs(sa_bbox) <- "epsg:4326"
p_sat <- ggplot() +
  geom_spatraster(data = sa_total) +
  geom_spatvector(data = countries %>% crop(ext(sa_total)) %>% aggregate(), 
                  col = "black", fill = "white") +
  geom_spatvector(data = sa_bbox, fill = NA, col = "black", linewidth = .5) +
  scale_fill_steps2(na.value = "transparent", guide = "none",
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void() 
p_sat + ggview::canvas(width = 3.8, height = 3)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_agreement_sa.png"),
       plot = p_sat,
       width = 3.8, height = 3, units = "in", dpi = 300)

# plot marion/crozet
mc_total <- total %>% crop(ext(36, 55, -48, -44))
mc_bbox <- ext(36, 55, -48, -44) %>% as.polygons()
crs(mc_bbox) <- "epsg:4326"
p_mct <- ggplot() +
  geom_spatraster(data = mc_total) +
  geom_spatvector(data = countries %>% crop(ext(mc_total)) %>% aggregate(), 
                  col = "black", fill = "white") +
  geom_spatvector(data = mc_bbox, fill = NA, col = "black", linewidth = .5) +
  scale_fill_steps2(na.value = "transparent", guide = "none",
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void() 
p_mct + ggview::canvas(width = 4, height = 1.5)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_agreement_mc.png"),
       plot = p_mct,
       width = 4, height = 1.5, units = "in", dpi = 300)

# plot kerguelen/heard
kh_total <- total %>% crop(ext(65, 76, -55, -47))
kh_bbox <- ext(65, 76, -55, -47) %>% as.polygons()
crs(kh_bbox) <- "epsg:4326"
p_kht <- ggplot() +
  geom_spatraster(data = kh_total) +
  geom_spatvector(data = countries %>% crop(ext(kh_total)) %>% aggregate(), 
                  col = "black", fill = "white") +
  geom_spatvector(data = kh_bbox, fill = NA, col = "black", linewidth = .5) +
  scale_fill_steps2(na.value = "transparent", guide = "none",
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void() 
p_kht + ggview::canvas(width = 3.5, height = 4)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_agreement_kh.png"),
       plot = p_kht,
       width = 3.5, height = 4, units = "in", dpi = 300)

# plot macquarie
mq_total <- total %>% crop(ext(156, 162, -56, -53))
mq_bbox <- ext(156, 162, -56, -53) %>% as.polygons()
crs(mq_bbox) <- "epsg:4326"
p_mqt <- ggplot() +
  geom_spatraster(data = mq_total) +
  geom_spatvector(data = countries %>% crop(ext(mq_total)) %>% aggregate(), 
                  col = "black", fill = "white") +
  geom_spatvector(data = mq_bbox, fill = NA, col = "black", linewidth = .5) +
  scale_fill_steps2(na.value = "transparent", guide = "none",
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  theme_void() 
p_mqt + ggview::canvas(width = 3, height = 2.7)
ggsave(paste0("output/imagery/combined suitability/gentoo/", scenario, "_agreement_mq.png"),
       plot = p_mqt,
       width = 3, height = 2.7, units = "in", dpi = 300)
