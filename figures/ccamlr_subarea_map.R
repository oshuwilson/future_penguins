#-------------------------------------------------------------------------------
# Subplot for CCAMLR subareas
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")


library(tidyverse)
library(terra)
library(tidyterra)
library(CCAMLRGIS)

# read in CCAMLR Subareas
subareas <- load_ASDs() %>% vect()

# read in coast line
coast <- load_Coastline() %>% vect()

subareas %>%
  as.data.frame()

# plot
ggplot() +
  geom_spatvector(data = subareas, aes(fill = GAR_Long_Label), col = NA) +
  scale_fill_manual(values = c("#FF7B00", "#FF8D21", "#FFA652", "#FFB76B", "#FFCD90", "#FFF4DF",
                               "#003A6B", "#012a4a", "#013a63", "#01497c", "#2a6f97", "#46769B", "#468faf", "#61a5c2", "#89c2d9", "#a9d6e5",
                               "#cabfe7", "#e7b7f9", "#d488f6"),
                    guide = "none") +
  geom_spatvector(data = coast, fill = "grey40", col = NA) +
  theme_void()
  # geom_spatvector_text(data = subareas, aes(label = GAR_Long_Label, color = GAR_Long_Label),
  #                      fontface = "bold", size = 4) +
  # scale_color_manual(values = c("white", "white", "black", "black", "black", "black",
  #                               "white", "white", "white", "white", "black", "black", "black", "black", "black", "black",
  #                               "black", "black", "black"))

# export
ggsave("output/imagery/maps/ccamlr_subareas.png", width = 10, height = 10)

#-------------------------------------------------------------------------------
# Subplot for CCAMLR subareas - greyscale
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")


library(tidyverse)
library(terra)
library(tidyterra)
library(CCAMLRGIS)

# read in CCAMLR Subareas
subareas <- load_ASDs() %>% vect()

# read in coast line
coast <- load_Coastline() %>% vect()

subareas %>%
  as.data.frame()

# plot
ggplot() +
  geom_spatvector(data = subareas, aes(fill = GAR_Long_Label), col = NA) +
  scale_fill_manual(values = c("grey70", "grey90", "grey80", "grey65", "grey80", "grey90",
                               "grey70", "grey55", "grey80", "grey60", "grey80", "grey70", "grey60", "grey90", "grey80", "grey65",
                               "grey90", "grey80", "grey65"),
                    guide = "none") +
  geom_spatvector(data = coast, fill = "grey40", col = NA) +
  theme_void()
# geom_spatvector_text(data = subareas, aes(label = GAR_Long_Label, color = GAR_Long_Label),
#                      fontface = "bold", size = 4) +
# scale_color_manual(values = c("white", "white", "black", "black", "black", "black",
#                               "white", "white", "white", "white", "black", "black", "black", "black", "black", "black",
#                               "black", "black", "black"))

# export
ggsave("output/imagery/maps/ccamlr_subareas_greyscale.png", width = 10, height = 10)
                    

#-------------------------------------------------------------------------------
# Subplot for CCAMLR subareas - outlines only
#-------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(terra)
library(tidyterra)
library(CCAMLRGIS)

# read in CCAMLR Subareas
subareas <- load_ASDs() %>% vect()

# read in coast line
coast <- load_Coastline() %>% vect()

subareas %>%
  as.data.frame()

# plot
ggplot() +
  geom_spatvector(data = subareas, col = "grey50", fill = NA) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  scale_fill_manual(values = c("grey90", "grey50"), guide = "none") +
  theme_void()
# geom_spatvector_text(data = subareas, aes(label = GAR_Long_Label, color = GAR_Long_Label),
#                      fontface = "bold", size = 4) +
# scale_color_manual(values = c("white", "white", "black", "black", "black", "black",
#                               "white", "white", "white", "white", "black", "black", "black", "black", "black", "black",
#                               "black", "black", "black"))

# export
ggsave("output/imagery/maps/ccamlr_subareas_outline.png", width = 10, height = 10)
