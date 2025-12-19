#-------------------------------------------------------------------------------
# Justification for dropping the incubation period in emperor penguins
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

# read in emperor penguin incubation prediction
emp <- rast("output/at-sea model/predictions/EMPE_incubation_simple_ensemble.tif")

# project
emp <- project(emp, "epsg:6932")

# read in colony locations
colonies <- readRDS("data/colonies/subareas/EMPE_colonies_subareas.RDS")

# project
colonies <- colonies %>% vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")
colonies$name

# incubation tracks
tracks <- readRDS("data/tracks/EMPE.RDS") %>%
  filter(stage == "incubation")
unique(tracks$deployment_site)

# subset colonies used to train models
col_train <- colonies %>% filter(name %in% c("Auster", "Point Geologie"))
plot(col_train)

# plot together
p1 <- ggplot() +
  geom_spatraster(data = emp) +
  geom_spatvector(data = colonies) +
  geom_spatvector(data = col_train, col = "red3", size = 3) +
  scale_fill_viridis_c(na.value = "transparent", name = "Habitat Suitability") +
  theme_void()
p1 + ggview::canvas(width = 10, height = 10)

# export
ggsave("text/figures/draft/supplementary/drop_emps.png",
       p1, width = 10, height = 10, dpi = 300)
