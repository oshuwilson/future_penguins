#-------------------------------------------------------------------------------
# Evaluate how future-proof the proposed Domain 1 MPA is
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

# read in voluntary buffer zones
vbz <- readRDS("data/mpas/ARK_buffers.rds") %>%
  project("epsg:6932")

# load in planning domains
domains <- readRDS("data/mpas/domains.rds")

# isolate antarctic peninsula MPA
wap <- prop %>% 
  filter(site_name == "Western Antarctic Peninsula and South Scotia Arc") %>%
  project("epsg:6932") 

# aggregate wap by designation
spz <- wap %>% filter(designation == "SPZ")
gpz <- wap %>% filter(designation == "GPZ")
spz <- terra::aggregate(spz)
gpz <- terra::aggregate(gpz)
wap <- rbind(spz, gpz)
plot(wap)

# isolate planning domain 1
domain1 <- domains %>%
  filter(Name == "1") %>%
  project("epsg:6932")

# isolate implemented mpas within domain 1
imp_domain1 <- imp %>%
  project(crs(domain1)) %>%
  intersect(domain1) 

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS") %>%
  crop(ext(domain1) - c(100000, 100000, 0, 200000)) 
plot(coast)

#-------------------------------------------------------------------------------
# Present Day
#-------------------------------------------------------------------------------

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
  project(crs(domain1)) %>%
  intersect(domain1)
chpe_bins <- as.polygons(chpe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain1)) %>%
  intersect(domain1)
gepe_bins <- as.polygons(gepe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain1)) %>%
  intersect(domain1)
mape_bins <- as.polygons(mape_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain1)) %>%
  intersect(domain1)
empe_bins <- as.polygons(empe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain1)) %>%
  intersect(domain1)
kipe_bins <- as.polygons(kipe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain1)) %>%
  intersect(domain1)

# only macaronis, kings and gentoos
p1_present <- ggplot() +
  geom_spatvector(data = mape_bins, fill = "#f5d63d", col = NA, alpha = 0.5) +
  geom_spatvector(data = kipe_bins, fill = "#f57f13", col = NA, alpha = 0.5) +
  geom_spatvector(data = gepe_bins, fill = "#cd3d4c", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, fill = NA, col = "black") + 
  geom_spatvector(data = imp_domain1, col = "black", fill = NA) +
  geom_spatvector(data = vbz, col = "black", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
p1_present

# only adelies, chinstraps, and emperors
p2_present <- ggplot() + 
  geom_spatvector(data = adpe_bins, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins, fill = "#00768B", col = NA, alpha = 0.5) +
  geom_spatvector(data = empe_bins, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, fill = NA, col = "black") + 
  geom_spatvector(data = vbz, fill = NA, col = "black") +
  geom_spatvector(data = imp_domain1, col = "black", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
p2_present


#-------------------------------------------------------------------------------
# SSP126
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_126 <- rast("output/combined/projections/ssp126/ADPE_mean_future_core_habitat.tif")
chpe_bins_126 <- rast("output/combined/projections/ssp126/CHPE_mean_future_core_habitat.tif")
gepe_bins_126 <- rast("output/combined/projections/ssp126/GEPE_mean_future_core_habitat.tif")
mape_bins_126 <- rast("output/combined/projections/ssp126/MAPE_mean_future_core_habitat.tif")
empe_bins_126 <- rast("output/combined/projections/ssp126/EMPE_mean_future_core_habitat.tif")
kipe_bins_126 <- rast("output/combined/projections/ssp126/KIPE_mean_future_core_habitat.tif")

# limit to areas of core habitat
adpe_bins_126 <- adpe_bins_126 %>% 
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
chpe_bins_126 <- chpe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
gepe_bins_126 <- gepe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
mape_bins_126 <- mape_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
empe_bins_126 <- empe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
kipe_bins_126 <- kipe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)

# plot kings, gentoos, macaronis
p1_126 <- ggplot() +
  geom_spatvector(data = mape_bins_126, fill = "#f5d63d", col = NA, alpha = 0.5) +
  geom_spatvector(data = kipe_bins_126, fill = "#f57f13", col = NA, alpha = 0.5) +
  geom_spatvector(data = gepe_bins_126, fill = "#cd3d4c", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, fill = NA, col = "black") + 
  geom_spatvector(data = imp_domain1, col = "black", fill = NA) +
  geom_spatvector(data = vbz, col = "black", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
p1_126 

# plot adelies, chinstraps, and emperors
p2_126 <- ggplot() +
  geom_spatvector(data = adpe_bins_126, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins_126, fill = "#00768B", col = NA, alpha = 0.5) +
  geom_spatvector(data = empe_bins_126, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, fill = NA, col = "black") + 
  geom_spatvector(data = imp_domain1, col = "black", fill = NA) +
  geom_spatvector(data = vbz, col = "black", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
p2_126

#-------------------------------------------------------------------------------
# SSP585
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_585 <- rast("output/combined/projections/ssp585/ADPE_mean_future_core_habitat.tif")
chpe_bins_585 <- rast("output/combined/projections/ssp585/CHPE_mean_future_core_habitat.tif")
gepe_bins_585 <- rast("output/combined/projections/ssp585/GEPE_mean_future_core_habitat.tif")
mape_bins_585 <- rast("output/combined/projections/ssp585/MAPE_mean_future_core_habitat.tif")
empe_bins_585 <- rast("output/combined/projections/ssp585/EMPE_mean_future_core_habitat.tif")
kipe_bins_585 <- rast("output/combined/projections/ssp585/KIPE_mean_future_core_habitat.tif")

# limit to areas of core habitat
adpe_bins_585 <- adpe_bins_585 %>% 
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
chpe_bins_585 <- chpe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
gepe_bins_585 <- gepe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
mape_bins_585 <- mape_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
empe_bins_585 <- empe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)
kipe_bins_585 <- kipe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain1)) %>%
  intersect(domain1)

# plot kings, gentoos, macaronis
p1_585 <- ggplot() +
  geom_spatvector(data = mape_bins_585, fill = "#f5d63d", col = NA, alpha = 0.5) +
  geom_spatvector(data = kipe_bins_585, fill = "#f57f13", col = NA, alpha = 0.5) +
  geom_spatvector(data = gepe_bins_585, fill = "#cd3d4c", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, fill = NA, col = "black") + 
  geom_spatvector(data = imp_domain1, col = "black", fill = NA) +
  geom_spatvector(data = vbz, col = "black", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
p1_585 

# plot adelies, chinstraps, and emperors
p2_585 <- ggplot() +
  geom_spatvector(data = adpe_bins_585, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins_585, fill = "#00768B", col = NA, alpha = 0.5) +
  geom_spatvector(data = empe_bins_585, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = wap, fill = NA, col = "black") + 
  geom_spatvector(data = imp_domain1, col = "black", fill = NA) +
  geom_spatvector(data = vbz, col = "black", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
p2_585


# plot all together
library(cowplot)
p1 <- plot_grid(p1_present, p1_126, p1_585, ncol = 1)
p2 <- plot_grid(p2_present, p2_126, p2_585, ncol = 1)
all_grid <- plot_grid(p1, p2, ncol = 2)
all_grid
all_grid2 <- plot_grid(p1_present, p2_present, p1_126, p2_126, p1_585, p2_585,
                       ncol = 3, byrow = F)

all_grid2 + ggview::canvas(width = 12, height = 12)
ggsave("text/figures/draft/domainplots/domain1/corehabitat.png", all_grid2,
       width = 12, height = 12)


# bonus plots
# just plot proposed MPAs, existing MPAs, and the coast
proposal <- ggplot() +
  geom_spatvector(data = gpz, fill = "#AA9ABA", col = NA, alpha = 1) +
  geom_spatvector(data = spz, fill = "#baaa9a", col = NA, alpha = 1) +
  geom_spatvector(data = vbz, fill = "#BAF2BB", col = NA, alpha = 0.8) +
  geom_spatvector(data = imp_domain1, col = NA, fill = "#007a8a", alpha = 0.8) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
proposal + ggview::canvas(width = 7, height = 10)
ggsave("text/figures/draft/domainplots/domain1/proposal.png",
       width = 7, height = 10) 

# plot of all domains
domains <- domains %>% project("epsg:6932")
domainplot <- ggplot() +
  geom_spatvector(data = domains, fill = "grey", col = NA) +
  geom_spatvector(data = domains %>% filter(Name == "1"), fill = "darkred", col = NA) +
  scale_fill_grey(guide = "none") +
  theme_void()
domainplot + ggview::canvas(width = 6, height = 6)
ggsave("text/figures/draft/domainplots/domain1/domain_highlight.png")
