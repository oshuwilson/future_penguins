#-------------------------------------------------------------------------------
# CCAMLR Planning Domain Map
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# load in planning domains
domains <- readRDS("data/mpas/domains.rds")

# load in coast
coast <- readRDS("data/coast_ice_vect.RDS")

# project domains
domains <- project(domains, crs(coast))

# isolate domain 8
domain8 <- domains %>% filter(Name == 8)

# aggregate domain 8
domain8_agg <- domain8 %>% buffer(width = 1) %>%
  aggregate()
plot(domain8_agg)

# rejoin domain 8
domains <- domains %>%
  filter(Name != 8) %>%
  rbind(domain8_agg)

# plot
p1 <- ggplot() +
  geom_spatvector(data = domains, col = "grey10", fill = NA) +
  geom_spatvector(data = coast, fill = "grey10", color = "grey10") +
  theme_void()

# export
ggsave("text/figures/draft/domainplots/domain_map.png", p1, width = 8, height = 8, units = "in", dpi = 300)
