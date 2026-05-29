#-------------------------------------------------------------------------------
# Evaluate how future-proof the proposed Domain 7 MPA is
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

# isolate domain 7 MPAs
mpas <- prop %>% 
  filter(site_name == "East Antarctic") %>%
  project("epsg:6932") 

# isolate planning domain 7
domain7 <- domains %>%
  filter(Name == "7") %>%
  project("epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS") %>%
  crop(ext(domain7)) 
plot(coast)

#-------------------------------------------------------------------------------
# Present Day
#-------------------------------------------------------------------------------

# read in present day suitability bins
adpe_bins <- rast("output/combined/predictions/adpe_core_habitat_bins.tif")
chpe_bins <- rast("output/combined/predictions/chpe_core_habitat_bins.tif")
empe_bins <- rast("output/combined/predictions/empe_core_habitat_bins.tif")

# convert to polygons
adpe_bins <- as.polygons(adpe_bins) %>%
  filter(mean == 20) %>% 
  project(crs(domain7)) %>%
  intersect(domain7)
chpe_bins <- as.polygons(chpe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain7)) %>%
  intersect(domain7)
empe_bins <- as.polygons(empe_bins) %>%
  filter(mean == 20) %>%
  project(crs(domain7)) %>%
  intersect(domain7)

# only adelies, chinstraps, and emperors
p2_present <- ggplot() + 
  geom_spatvector(data = domain7, col = "grey60", fill = NA, linewidth = .5) +
  geom_spatvector(data = adpe_bins, fill = "#000004", col = NA, alpha = 1) +
  geom_spatvector(data = empe_bins, fill = "#822069", col = NA, alpha = 0.8) +
  geom_spatvector(data = chpe_bins, fill = "#00768B", col = NA, alpha = 0.8) +
  geom_spatvector(data = mpas, fill = NA, col = "grey60", linewidth = .5) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey90", "grey40"), guide = "none") +
  scale_color_manual(values = c("grey90", "grey40"), guide = "none") +
  theme_void()
p2_present + ggview::canvas(width = 12, height = 9)

#-------------------------------------------------------------------------------
# SSP126
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_126 <- rast("output/combined/projections/ssp126/ADPE_mean_future_core_habitat.tif")
chpe_bins_126 <- rast("output/combined/projections/ssp126/CHPE_mean_future_core_habitat.tif")
empe_bins_126 <- rast("output/combined/projections/ssp126/EMPE_mean_future_core_habitat.tif")

# limit to areas of core habitat
adpe_bins_126 <- adpe_bins_126 %>% 
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain7)) %>%
  intersect(domain7)
chpe_bins_126 <- chpe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain7)) %>%
  intersect(domain7)
empe_bins_126 <- empe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain7)) %>%
  intersect(domain7)

# plot adelies, chinstraps, and emperors
p2_126 <- ggplot() + 
  geom_spatvector(data = domain7, col = "grey60", fill = NA, linewidth = .5) +
  geom_spatvector(data = adpe_bins_126, fill = "#000004", col = NA, alpha = 1) +
  geom_spatvector(data = empe_bins_126, fill = "#822069", col = NA, alpha = 0.8) +
  geom_spatvector(data = chpe_bins_126, fill = "#00768B", col = NA, alpha = 0.8) +
  geom_spatvector(data = mpas, fill = NA, col = "grey60", linewidth = .5) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey90", "grey40"), guide = "none") +
  scale_color_manual(values = c("grey90", "grey40"), guide = "none") +
  theme_void()
p2_126 + ggview::canvas(width = 12, height = 9)

#-------------------------------------------------------------------------------
# SSP585
#-------------------------------------------------------------------------------

# read in future suitability bins
adpe_bins_585 <- rast("output/combined/projections/ssp585/ADPE_mean_future_core_habitat.tif")
chpe_bins_585 <- rast("output/combined/projections/ssp585/CHPE_mean_future_core_habitat.tif")
empe_bins_585 <- rast("output/combined/projections/ssp585/EMPE_mean_future_core_habitat.tif")

# limit to areas of core habitat
adpe_bins_585 <- adpe_bins_585 %>% 
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain7)) %>%
  intersect(domain7)
chpe_bins_585 <- chpe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain7)) %>%
  intersect(domain7)
empe_bins_585 <- empe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(domain7)) %>%
  intersect(domain7)

# plot adelies, chinstraps, and emperors
p2_585 <- ggplot() + 
  geom_spatvector(data = domain7, col = "grey60", fill = NA, linewidth = .5) +
  geom_spatvector(data = adpe_bins_585, fill = "#000004", col = NA, alpha = 1) +
  geom_spatvector(data = empe_bins_585, fill = "#822069", col = NA, alpha = 0.8) +
  geom_spatvector(data = chpe_bins_585, fill = "#00768B", col = NA, alpha = 1) +
  geom_spatvector(data = mpas, fill = NA, col = "grey60", linewidth = .5) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey90", "grey40"), guide = "none") +
  scale_color_manual(values = c("grey90", "grey40"), guide = "none") +
  theme_void()
p2_585 + ggview::canvas(12, 9)


# plot all together
library(cowplot)
p2 <- plot_grid(p2_present, p2_126, p2_585, ncol = 3)
p2 + ggview::canvas(width = 12, height = 9)
ggsave("text/figures/draft/domainplots/domain7/corehabitat.png", p2,
       width = 12, height = 9)


# bonus plots
# just plot proposed MPAs, existing MPAs, and the coast
proposal <- ggplot() +
  geom_spatvector(data = domain7, col = "darkred", fill = NA, linewidth = .5) +
  geom_spatvector(data = mpas, fill = "#DF7CA8", col = NA) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_color_manual(values = c("grey90", "grey40"), guide = "none") +
  scale_fill_manual(values = c("grey90", "grey40"), guide = "none") +
  theme_void()
proposal + ggview::canvas(width = 10, height = 10)
ggsave("text/figures/draft/domainplots/domain7/proposal.png",
       width = 10, height = 10) 

# plot of all domains
domains <- domains %>% project("epsg:6932")
domainplot <- ggplot() +
  geom_spatvector(data = domains, fill = "grey", col = NA) +
  geom_spatvector(data = domains %>% filter(Name == "7"), fill = "darkred", col = NA) +
  scale_fill_grey(guide = "none") +
  theme_void()
domainplot + ggview::canvas(width = 6, height = 6)
ggsave("text/figures/draft/domainplots/domain7/domain_highlight.png",
       domainplot,
       width = 6, height = 6)
