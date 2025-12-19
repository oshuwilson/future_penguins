#-------------------------------------------------------------------------------
# Evaluate how future-proof the proposed Domain 3 MPA is
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

# isolate Weddell Sea MPA
weddell <- prop %>% 
  filter(site_name %in% c("Weddell Sea", "Weddell Sea Marine Protected Area")) %>%
  project("epsg:6932") 

# isolate planning domain 3
domain3 <- domains %>%
  filter(Name %in% c("3", "4")) %>%
  project(crs(weddell)) %>%
  crop(ext(weddell))

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS") %>%
  crop(ext(weddell) + c(100000, 100000, 100000, 100000))

#-------------------------------------------------------------------------------
# Present Day
#-------------------------------------------------------------------------------

# read in present day suitability bins
adpe_bins <- rast("output/combined/predictions/adpe_core_habitat_bins.tif")
empe_bins <- rast("output/combined/predictions/empe_core_habitat_bins.tif")

# convert to polygons
adpe_bins <- as.polygons(adpe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain3)) %>%
  intersect(domain3)
empe_bins <- as.polygons(empe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain3)) %>%
  intersect(domain3)

# only adelies, and emperors
p2_present <- ggplot() + 
  geom_spatvector(data = empe_bins, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = adpe_bins, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = weddell, fill = NA, col = "black") +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  #geom_spatvector(data = domain1, fill = NA, col = "black") +
  theme_void()
p2_present

#-------------------------------------------------------------------------------
# SSP126
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_126 <- rast("output/combined/projections/ssp126/ADPE_mean_future_core_habitat.tif")
empe_bins_126 <- rast("output/combined/projections/ssp126/EMPE_mean_future_core_habitat.tif")

# limit to areas of core habitat
adpe_bins_126 <- adpe_bins_126 %>% 
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain3)) %>%
  intersect(domain3)
empe_bins_126 <- empe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain3)) %>%
  intersect(domain3)

# plot adelies and emperors
p2_126 <- ggplot() +
  geom_spatvector(data = adpe_bins_126, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = empe_bins_126, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = weddell, fill = NA, col = "black") + 
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  #geom_spatvector(data = domain1, fill = NA, col = "black") +
  theme_void()
p2_126

#-------------------------------------------------------------------------------
# SSP585
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_585 <- rast("output/combined/projections/ssp585/ADPE_mean_future_core_habitat.tif")
empe_bins_585 <- rast("output/combined/projections/ssp585/EMPE_mean_future_core_habitat.tif")
  
# limit to areas of core habitat
adpe_bins_585 <- adpe_bins_585 %>% 
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain3)) %>%
  intersect(domain3)
empe_bins_585 <- empe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain3)) %>%
  intersect(domain3)

# plot adelies and emperors
p2_585 <- ggplot() +
  geom_spatvector(data = adpe_bins_585, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = empe_bins_585, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = weddell, fill = NA, col = "black") +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  #geom_spatvector(data = domain1, fill = NA, col = "black") +
  theme_void()
p2_585


# plot all together
library(cowplot)
p2 <- plot_grid(p2_present, p2_126, p2_585, ncol = 1)
p2 + ggview::canvas(width = 7, height = 14)
ggsave("text/figures/draft/domainplots/domain3/corehabitat.png",
       width = 7, height = 14)


# bonus plots
# just plot proposed MPAs, existing MPAs, and the coast
proposal <- ggplot() +
  geom_spatvector(data = weddell, aes(fill = name), col = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey80", "grey20", "#FFAF87", "#AA9ABA", "#FFAF87",  "#AA9ABA", "#baaa9a"),
                    guide = "none") +
  theme_void()
proposal + ggview::canvas(width = 12, height = 10)
ggsave("text/figures/draft/domainplots/domain3/proposal.png",
       width = 12, height = 10) 

# plot of all domains
domains <- domains %>% project("epsg:6932")
domainplot <- ggplot() +
  geom_spatvector(data = domains, fill = "grey", col = NA) +
  geom_spatvector(data = domain3, fill = "darkred", col = NA) +
  scale_fill_grey(guide = "none") +
  theme_void()
domainplot + ggview::canvas(width = 6, height = 6)
ggsave("text/figures/draft/domainplots/domain3/domain_highlight.png")
