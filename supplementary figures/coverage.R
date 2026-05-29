#-------------------------------------------------------------------------------
# Plot data gaps in tracking data
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)
library(cowplot)

# read in CCAMLR subareas
subareas <- readRDS("data/subareas/subareas_stereographic.rds")

# read in coast
coast <- readRDS("data/coast_ice_vect.rds")

# get list of subarea names
sub_names <- subareas$GAR_Long_Label %>% sort()
sub_names


# for each species list the subareas where:
# 1. there are tracking data
# 2. there are no tracking data but the species breeds there

# define species
species <- "King Penguin"

# define tracking data regions
tracked <- sub_names[c(3, 13, 14, 15, 16)]

# define untracked breeding regions
breeding <- sub_names[c()]

# plot (add extralimital subareas in affinity)
p1 <- ggplot() +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% tracked), fill = "steelblue", col = NA) +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% breeding), fill = "darkred", col = NA) +
  geom_spatvector(data = subareas, col = "grey20", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey20"), guide = "none") +
  theme_void() +
  ggtitle(species)
p1 + ggview::canvas(6, 6)

# export
ggsave("text/figures/draft/supplementary/coverage/king_penguin.png", p1, width = 6, height = 6, units = "in", dpi = 300)

# clear 
rm(species, tracked, breeding, p1)

# define species
species <- "Adelie Penguin"

# define tracking data regions
tracked <- sub_names[c(1, 2, 7, 8, 17)]

# define untracked breeding regions
breeding <- sub_names[c(4, 18, 19)]

# plot (add extralimital subareas in affinity)
p1 <- ggplot() +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% tracked), fill = "steelblue", col = NA) +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% breeding), fill = "darkred", col = NA) +
  geom_spatvector(data = subareas, col = "grey20", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey20"), guide = "none") +
  theme_void() +
  ggtitle(species)
p1 + ggview::canvas(6, 6)

# export
ggsave("text/figures/draft/supplementary/coverage/adelie_penguin.png", p1, width = 6, height = 6, units = "in", dpi = 300)

# clear
rm(species, tracked, breeding, p1)

# define species
species <- "Chinstrap Penguin"

# define tracking data regions
tracked <- sub_names[c(1, 2, 4)]

# define untracked breeding regions
breeding <- sub_names[c(3, 17)]

# plot (add extralimital subareas in affinity)
p1 <- ggplot() +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% tracked), fill = "steelblue", col = NA) +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% breeding), fill = "darkred", col = NA) +
  geom_spatvector(data = subareas, col = "grey20", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey20"), guide = "none") +
  theme_void() +
  ggtitle(species)
p1 + ggview::canvas(6, 6)

# export
ggsave("text/figures/draft/supplementary/coverage/chinstrap_penguin.png", p1, width = 6, height = 6, units = "in", dpi = 300)

# clear
rm(species, tracked, breeding, p1)

# define species
species <- "Gentoo Penguin"

# define tracking data regions
tracked <- sub_names[c(1, 2, 3, 16)]

# define untracked breeding regions
breeding <- sub_names[c(4, 13, 14, 15)]

# plot (add extralimital subareas in affinity)
p1 <- ggplot() +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% tracked), fill = "steelblue", col = NA) +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% breeding), fill = "darkred", col = NA) +
  geom_spatvector(data = subareas, col = "grey20", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey20"), guide = "none") +
  theme_void() +
  ggtitle(species)
p1 + ggview::canvas(6, 6)

# export
ggsave("text/figures/draft/supplementary/coverage/gentoo_penguin.png", p1, width = 6, height = 6, units = "in", dpi = 300)

# clear
rm(species, tracked, breeding, p1)

# define species
species <- "Macaroni Penguin"

# define tracking data regions
tracked <- sub_names[c(3, 13, 14, 16)]

# define untracked breeding regions
breeding <- sub_names[c(1, 2, 4, 15)]

# plot (add extralimital subareas in affinity)
p1 <- ggplot() +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% tracked), fill = "steelblue", col = NA) +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% breeding), fill = "darkred", col = NA) +
  geom_spatvector(data = subareas, col = "grey20", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey20"), guide = "none") +
  theme_void() +
  ggtitle(species)
p1 + ggview::canvas(6, 6)

# export
ggsave("text/figures/draft/supplementary/coverage/macaroni_penguin.png", p1, width = 6, height = 6, units = "in", dpi = 300)

# clear
rm(species, tracked, breeding, p1)

# define species
species <- "Emperor Penguin"

# define tracking data regions
tracked <- sub_names[c(17, 18, 19, 7, 8)]

# define untracked breeding regions
breeding <- sub_names[c(1, 5, 6)]

# plot (add extralimital subareas in affinity)
p1 <- ggplot() +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% tracked), fill = "steelblue", col = NA) +
  geom_spatvector(data = subareas %>% filter(GAR_Long_Label %in% breeding), fill = "darkred", col = NA) +
  geom_spatvector(data = subareas, col = "grey20", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey20"), guide = "none") +
  theme_void() +
  ggtitle(species)
p1 + ggview::canvas(6, 6)

# export
ggsave("text/figures/draft/supplementary/coverage/emperor_penguin.png", p1, width = 6, height = 6, units = "in", dpi = 300)       
