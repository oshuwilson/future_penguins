#-------------------------------------------------------------------------------
# Evaluate projected species distribution changes in Domain 9
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# load in planning domains
domains <- readRDS("data/mpas/domains.rds")

# isolate planning domain 9
this_domain <- domains %>%
  filter(Name == "9") %>%
  project("epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS") %>%
  crop(ext(this_domain)) 
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
  project(crs(this_domain)) %>%
  intersect(this_domain)
chpe_bins <- as.polygons(chpe_bins) %>%
  filter(mean == 20) %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
gepe_bins <- as.polygons(gepe_bins) %>%
  filter(mean == 20) %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
mape_bins <- as.polygons(mape_bins) %>%
  filter(mean == 20) %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
empe_bins <- as.polygons(empe_bins) %>%
  filter(mean == 20) %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
kipe_bins <- as.polygons(kipe_bins) %>%
  filter(mean == 20) %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)

# only adelies, chinstraps, and emperors
p2_present <- ggplot() + 
  geom_spatvector(data = empe_bins, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = adpe_bins, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins, fill = "#00768B", col = NA, alpha = 0.5) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  #geom_spatvector(data = this_domain, fill = NA, col = "black") +
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
  project(crs(this_domain)) %>%
  intersect(this_domain)
chpe_bins_126 <- chpe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
gepe_bins_126 <- gepe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
mape_bins_126 <- mape_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
empe_bins_126 <- empe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
kipe_bins_126 <- kipe_bins_126 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)

# plot adelies, chinstraps, and emperors
p2_126 <- ggplot() +
  geom_spatvector(data = empe_bins_126, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = adpe_bins_126, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins_126, fill = "#00768B", col = NA, alpha = 0.5) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  #geom_spatvector(data = this_domain, fill = NA, col = "black") +
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
  project(crs(this_domain)) %>%
  intersect(this_domain)
chpe_bins_585 <- chpe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
gepe_bins_585 <- gepe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
mape_bins_585 <- mape_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
empe_bins_585 <- empe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)
kipe_bins_585 <- kipe_bins_585 %>%
  filter(mean == 1) %>%
  as.polygons() %>%
  project(crs(this_domain)) %>%
  intersect(this_domain)

# plot adelies, chinstraps, and emperors
p2_585 <- ggplot() +
  geom_spatvector(data = empe_bins_585, fill = "#822069", col = NA, alpha = 0.5) +
  geom_spatvector(data = adpe_bins_585, fill = "#000004", col = NA, alpha = 0.5) +
  geom_spatvector(data = chpe_bins_585, fill = "#00768B", col = NA, alpha = 0.5) +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  #geom_spatvector(data = this_domain, fill = NA, col = "black") +
  theme_void()
p2_585


# plot all together
library(cowplot)
p2_future <- plot_grid(p2_126, p2_585, ncol = 2)
all_grid <- plot_grid(p2_present, p2_future, ncol = 1, align = "v")
all_grid + ggview::canvas(width = 12, height = 12)
ggsave("text/figures/draft/domainplots/domain9/corehabitat.png", all_grid,
       width = 12, height = 12)


# bonus plots
# just plot proposed MPAs, existing MPAs, and the coast
proposal <- ggplot() +
  geom_spatvector(data = coast, aes(fill = surface, col = surface)) +
  scale_color_manual(values = c("grey80", "grey20"), guide = "none") +
  scale_fill_manual(values = c("grey80", "grey20"), guide = "none") +
  theme_void()
proposal + ggview::canvas(width = 10, height = 7)
ggsave("text/figures/draft/domainplots/domain9/proposal.png",
       width = 10, height = 7) 

# plot of all domains
domains <- domains %>% project("epsg:6932")
domainplot <- ggplot() +
  geom_spatvector(data = domains, fill = "grey", col = NA) +
  geom_spatvector(data = domains %>% filter(Name == "9"), fill = "darkred", col = NA) +
  scale_fill_grey(guide = "none") +
  theme_void()
domainplot + ggview::canvas(width = 6, height = 6)
ggsave("text/figures/draft/domainplots/domain9/domain_highlight.png")
