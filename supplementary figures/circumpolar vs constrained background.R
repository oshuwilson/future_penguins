#-------------------------------------------------------------------------------
# Plot the impact of circumpolar background sampling vs contstrained
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(terra)
library(tidyverse)
library(tidyterra)

# read in circumpolar background predictions
cp <- rast("output/at-sea model/predictions2 - original/ADPE_chick-rearing_simple_ensemble.tif")

# read in final at-sea prediction
fin <- rast("output/at-sea model/predictions/ADPE_chick-rearing_simple_ensemble.tif")

# project
cp <- project(cp, "epsg:6932")
fin <- project(fin, "epsg:6932")

# read in adelie colony locations
colonies <- readxl::read_xlsx("data/colonies/Final Colonies/ADPE_by_colony.xlsx")
colonies <- vect(colonies, geom = c("longitude", "latitude"), crs = "epsg:4326") %>%
  project("epsg:6932")

# plot
p1 <- ggplot() +
  geom_spatraster(data = cp) +
  geom_spatvector(data = colonies, size = 0.5) +
  scale_fill_viridis_c(na.value = "transparent", name = "Habitat Suitability") +
  theme_void() + 
  ggtitle("Unconstrained Background Sampling") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot() +
  geom_spatraster(data = fin) +
  geom_spatvector(data = colonies, size = 0.5) +
  scale_fill_viridis_c(na.value = "transparent", name = "Habitat Suitability") +
  theme_void() + 
  ggtitle("Constrained Background Sampling") +
  theme(plot.title = element_text(hjust = 0.5))

# plot together
library(cowplot)
grid <- plot_grid(p1, p2, ncol = 2)
grid + ggview::canvas(width = 10, height = 6)

# export
ggsave("text/figures/draft/supplementary/circumpolar vs constrained background sampling.png", 
       plot = grid, width = 10, height = 6, dpi = 300)
