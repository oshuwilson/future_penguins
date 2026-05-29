#-------------------------------------------------------------------------------
# Plot all species proportionate change per subarea
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)

# list all figdata files
files <- list.files("output/combined/figdata/subarea_prop_change", full.names = T)

# read in and combine all figdata files
figdata <- files %>%
  map(readRDS) %>%
  bind_rows()

# get ssp scenarios where diff_prop mean doesn't exceed 0.01
sub0 <- figdata %>%
  group_by(ssp, species, common_name) %>%
  summarise(max_diff = max(abs(diff_prop_mean))) %>%
  filter(max_diff <= 0.01)

# identify combos where both ssp126 and ssp585 don't exceed 0.01
sub0 <- sub0 %>%
  group_by(species, common_name) %>%
  summarise(n = n()) %>%
  filter(n == 2) %>%
  select(species, common_name)

# don't include combos where species currently live
sub0 <- sub0 %>%
  filter(species != "CHPE" | common_name != "Eastern Ross Sea") %>%
  filter(species != "GEPE" | common_name != "Macquarie")

# remove these combos from the data
figdata <- figdata %>%
  anti_join(sub0, by = c("species", "common_name"))

# change two subarea names
figdata$common_name <- recode(figdata$common_name, 
                              "Enderby-Wilkes West" = "Enderby Land",
                              "Enderby-Wilkes East" = "Wilkes Land")

# swap eastern and western ross sea
figdata$common_name <- recode(figdata$common_name, 
                              "Eastern Ross Sea" = "Temp_Ross_East",
                              "Western Ross Sea" = "Eastern Ross Sea")
figdata$common_name <- recode(figdata$common_name,
                              "Temp_Ross_East" = "Western Ross Sea")

# manually reorder subareas
figdata$common_name <- factor(figdata$common_name, levels = rev(c("Falklands", "Marion", "Crozet", "Macquarie", "Kerguelen",
                                                                  "Chilean Islands", "Heard", "South Georgia", "South Sandwich", 
                                                                  "Bouvet", "South of Marion", "South of Crozet",
                                                                  "Southwest of Heard", "Southeast of Heard", 
                                                                  "South Orkney", "Antarctic Peninsula", "Amundsen-Bellingshausen Seas", 
                                                                  "Weddell Sea", "Queen Maud Land", "Enderby Land",
                                                                  "Wilkes Land", "Eastern Ross Sea", "Western Ross Sea"
)))

# rename Chile
figdata$common_name <- recode(figdata$common_name, "Chilean Islands" = "Chile")

# rename species 
figdata$species <- recode(figdata$species,
                          "ADPE" = "Adélie",
                          "CHPE" = "Chinstrap",
                          "EMPE" = "Emperor",
                          "GEPE" = "Gentoo",
                          "KIPE" = "King",
                          "MAPE" = "Macaroni")

# rename scenarios
figdata$ssp <- recode(figdata$ssp,
                       "ssp126" = "SSP126",
                       "ssp585" = "SSP585")

# get unique subareas (in the same order as plotting)
subareas <- levels(factor(figdata$common_name))

# create a dataframe for background rectangles
bg_df <- data.frame(
  subarea = subareas,
  xmin = seq_along(subareas) - 0.5,
  xmax = seq_along(subareas) + 0.5,
  fill = rep(c("grey95", "white"), length.out = length(subareas)))

# custom function to have a legend with a horizontal linerange
draw_key_horizontal_linerange <- function(data, params, size) {
  grid::segmentsGrob(
    x0 = 0.1, x1 = 0.9,
    y0 = 0.5, y1 = 0.5,
    gp = grid::gpar(
      col   = alpha(data$colour %||% data$fill, data$alpha),
      lwd   = (data$linewidth %||% 1) * .pt,
      lty   = data$linetype %||% 1
    )
  )
}

# multiply by 100 to convert to percentage points
figdata <- figdata %>%
  mutate(diff_prop_mean = diff_prop_mean * 100,
         lower_prop = lower_prop * 100,
         upper_prop = upper_prop * 100)

# get max absolute value
max_abs <- max(abs(figdata$lower_prop), abs(figdata$upper_prop))

# plot
p1 <- ggplot(figdata, aes(x = common_name, y = diff_prop_mean)) +
  geom_rect(data = bg_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), 
            inherit.aes = FALSE, alpha = 0.5) +
  geom_hline(yintercept = 0, col = "black") +
  geom_point(aes(color = species), size = 3, position = position_dodge2(width = 0.8, reverse = T)) +
  geom_linerange(aes(ymin = lower_prop, ymax = upper_prop, color = species), position = position_dodge2(width = 0.8, reverse = T),
                 key_glyph = draw_key_horizontal_linerange) + 
  scale_fill_manual(values = c("grey95" = "grey80", "white" = "white"), guide = "none") +
  scale_color_manual(values = c("#000004", "#00768B", "#84206B", "#C9404A", "#F67F13", "#F6D645")) +
  facet_wrap(~ssp) +
  ylim(-max_abs, max_abs) +
  coord_flip() +
  labs(y = "Change in Share of Core Habitat (%)",
       x = "Subarea",
       color = "Species") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12)) 
p1

# change axis text and axis labels font size
p1 <- p1 + theme(axis.text.y = element_text(size = 12, vjust = 0),
                 axis.text.x = element_text(size = 12),
                 axis.title = element_text(size = 12),
                 legend.text = element_text(size = 10),
                 legend.title = element_text(size = 12))

# preview export dims
p1 + 
  ggview::canvas(width = 12, height = 10)

# export
ggsave("text/figures/draft/prop_diff/ggplot_export.svg", p1,
       width = 12, height = 10, units = "in", dpi = 300)
ggsave("text/figures/draft/prop_diff/ggplot_export.png", p1,
       width = 12, height = 10, units = "in", dpi = 300)


