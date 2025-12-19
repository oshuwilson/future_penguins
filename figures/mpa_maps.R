#-------------------------------------------------------------------------------
# Protected Area Map for Subplot
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# load in mpa data
imp <- readRDS("data/mpas/implemented_mpas_agg.rds") 
prop <- readRDS("data/mpas/proposed_mpas_agg.rds") 

plot(imp)
plot(prop)

# project to polar view
imp <- imp %>% project("epsg:6932")
prop <- prop %>% project("epsg:6932")

# load in coastline
coast <- readRDS("data/coast_vect.RDS")

# load in subareas
subareas <- CCAMLRGIS::load_ASDs() %>% vect()

# plot 
p1 <- ggplot() +
  geom_spatvector(data = imp, fill = "#00798c", col = NA) +
  geom_spatvector(data = prop, fill = "#edae49", col = NA) +
  geom_spatvector(data = coast, fill = "grey40", col = NA) +
  #geom_spatvector(data = subareas, fill = NA, col = "grey40", lwd = 0.3) +
  theme_void()
p1

# export
ggsave("text/figures/draft/mpa_coverage/protected_area_map_subplot.png", p1, 
       width = 10, height = 10, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Repeat for varying levels of protection
#-------------------------------------------------------------------------------

# load in specific mpa data
full <- readRDS("data/mpas/full_mpas_agg.rds")
high <- readRDS("data/mpas/high_mpas_agg.rds")
light <- readRDS("data/mpas/light_mpas_agg.rds")
inc <- readRDS("data/mpas/incompatible_mpas_agg.rds") 
unknown <- readRDS("data/mpas/unknown_mpas_agg.rds")

# project
full <- full %>% project("epsg:6932")
high <- high %>% project("epsg:6932")
light <- light %>% project("epsg:6932")
inc <- inc %>% project("epsg:6932")
unknown <- unknown %>% project("epsg:6932")

# plot
p2 <- ggplot() +
  geom_spatvector(data = full, fill = "#932667", col = NA) +
  geom_spatvector(data = high, fill = "#FCFFA4", col = NA) +
  geom_spatvector(data = light, fill = "#FCA50A", col = NA) +
  geom_spatvector(data = inc, fill = "#DD513A", col = NA) +
  geom_spatvector(data = unknown, fill = "#420A68", col = NA) +
  geom_spatvector(data = prop, fill = "#999999", col = NA) +
  geom_spatvector(data = coast, fill = "grey40", col = NA) +
  #geom_spatvector(data = subareas, fill = NA, col = "grey40", lwd = 0.3) +
  theme_void()
p2

# export
ggsave("text/figures/draft/mpa_coverage/protected_area_map_subplot_by_level.png", p2, 
       width = 10, height = 10, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Repeat for varying levels of protection - No Take Zones vs Other
#-------------------------------------------------------------------------------

# combine mpa types into no-take zone and other
ntz <- rbind(full, high)
other <- rbind(light, inc, unknown)

# plot
p3 <- ggplot() +
  geom_spatvector(data = ntz, fill = "#003f5c", col = NA) +
  geom_spatvector(data = other, fill = "#00798c", col = NA) +
  geom_spatvector(data = prop, fill = "#edae49", col = NA) +
  geom_spatvector(data = coast, fill = "grey40", col = NA) +
  #geom_spatvector(data = subareas, fill = NA, col = "grey40", lwd = 0.3) +
  theme_void()
p3 + ggview::canvas(width = 10, height = 10)

# export
ggsave("text/figures/draft/mpa_coverage/protected_area_map_subplot_by_ntz.png", p3, 
       width = 10, height = 10, units = "in", dpi = 300)
