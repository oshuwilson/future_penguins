#-------------------------------------------------------------------------------
# Plot suitability differences for all species by subarea
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(cowplot)

# read in data for all species (add GEPE and EMPE later)
adpe <- readRDS("output/climatic model/figdata/ADPE_suitability_differences_data.RDS")
chpe <- readRDS("output/climatic model/figdata/CHPE_suitability_differences_data.RDS")
kipe <- readRDS("output/climatic model/figdata/KIPE_suitability_differences_data.RDS")
mape <- readRDS("output/climatic model/figdata/MAPE_suitability_differences_data.RDS")

# combine all together
data <- bind_rows(adpe %>% mutate(species = "Adelie"),
                  chpe %>% mutate(species = "Chinstrap"),
                  kipe %>% mutate(species = "King"),
                  mape %>% mutate(species = "Macaroni"))

# for each species calculate mean and sd of suitability differences by subarea and SSP
data_sum <- data %>%
  group_by(species, SSP, subarea) %>%
  summarise(mean_suitability_diff = mean(difference, na.rm = TRUE),
            sd_suitability_diff = sd(difference, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower = mean_suitability_diff - sd_suitability_diff,
         upper = mean_suitability_diff + sd_suitability_diff)

# get unique subareas (in the same order as plotting)
subareas <- levels(factor(data_sum$subarea))

# create a dataframe for background rectangles
bg_df <- data.frame(
  subarea = subareas,
  xmin = seq_along(subareas) - 0.5,
  xmax = seq_along(subareas) + 0.5,
  fill = rep(c("grey95", "white"), length.out = length(subareas)))

# rename the subareas from codes to names
data_sum$subarea <- case_match(data_sum$subarea,
                           "Division 58.4.1" ~ "Enderby-Wilkes West",
                           "Division 58.4.2" ~ "Enderby-Wilkes East",
                           "Division 58.5.1" ~ "Kerguelen",
                           "Division 58.5.2" ~ "Heard",
                           "Subarea 48.1" ~ "Antarctic Peninsula",
                           "Subarea 48.2" ~ "South Orkney",
                           "Subarea 48.3" ~ "South Georgia",
                           "Subarea 48.4" ~ "South Sandwich",
                           "Subarea 48.5" ~ "Weddell Sea",
                           "Subarea 48.6" ~ "Queen Maud Land",
                           "Subarea 58.6" ~ "Crozet",
                           "Subarea 58.7" ~ "Marion",
                           "Subarea 88.1" ~ "Eastern Ross Sea",
                           "Subarea 88.2" ~ "Western Ross Sea",
                           "Subarea 88.3" ~ "Amundsen-Bellingshausen Seas",
                           .default = data_sum$subarea)

# manually reorder subareas
data_sum$subarea <- factor(data_sum$subarea, levels = rev(c("Marion", "Crozet", "Kerguelen", "Heard", "Macquarie",
                                                        "Chile", "Falklands", "South Georgia", "South Sandwich", 
                                                        "Bouvet", "South Orkney", "Antarctic Peninsula", 
                                                        "Weddell Sea", "Queen Maud Land", "Enderby-Wilkes West",
                                                        "Enderby-Wilkes East", "Eastern Ross Sea", "Western Ross Sea",
                                                        "Amundsen-Bellingshausen Seas")))


p1 <- ggplot(data_sum, aes(x = subarea, y = mean_suitability_diff)) +
  geom_rect(data = bg_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), 
            inherit.aes = FALSE, alpha = 0.5) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(color = species), size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color = species), position = position_dodge2(width = 0.5)) + 
  scale_fill_manual(values = c("grey95" = "grey80", "white" = "white"), guide = "none") +
  scale_color_viridis_d(end = 0.8, option = "mako") +
  facet_wrap(~SSP) +
  coord_flip()  +
  ylim(-1, 1) +
  labs(y = "Projected Habitat Suitability Difference",
       x = "Subarea",
       color = "Species") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12))
p1 + ggview::canvas(width = 12, height = 12)

# export
ggsave("output/climatic model/figures/climatic_suitability_differences_test.png", 
       p1, width = 12, height = 12, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Plot area differences for all species by subarea
#-------------------------------------------------------------------------------

# is there a way to make this relative area gain/loss?
# i.e. divide the area gain/loss by the length of available coastline

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

# read in data for all species (add GEPE and EMPE later)
adpe <- readRDS("output/climatic model/figdata/ADPE_area_differences_data.RDS")
chpe <- readRDS("output/climatic model/figdata/CHPE_area_differences_data.RDS")
kipe <- readRDS("output/climatic model/figdata/KIPE_area_differences_data.RDS")
mape <- readRDS("output/climatic model/figdata/MAPE_area_differences_data.RDS")

# combine all together
data <- bind_rows(adpe %>% mutate(species = "Adelie"),
                  chpe %>% mutate(species = "Chinstrap"),
                  kipe %>% mutate(species = "King"),
                  mape %>% mutate(species = "Macaroni")) %>%
  filter(subarea != "Chile" & subarea != "NZ Subantarctic") # Chile and NZ have anomalously large areas due to big coastlines

# read in total coastal area for each subarea
total_area <- readRDS("output/climatic model/figdata/subarea_coastal_area_total.RDS")

# append the total area to the data
data <- data %>%
  left_join(total_area, by = "subarea") %>%
  rename(available_area = area_km2)

# calculate proportionate gain or loss
data <- data %>%
  mutate(prop_change = net_change/available_area)

# calculate percentage change per subarea
data <- data %>%
  mutate(perc_change = net_change/original_area * 100)

# for each species calculate mean and sd of area differences, proportionate changes and percentage changes by subarea and SSP
data_sum <- data %>%
  group_by(species, SSP, subarea) %>%
  summarise(mean_area_diff = mean(net_change, na.rm = TRUE),
            sd_area_diff = sd(net_change, na.rm = TRUE),
            mean_perc = mean(perc_change, na.rm = TRUE),
            sd_perc = sd(perc_change, na.rm = TRUE),
            mean_prop = mean(prop_change, na.rm = TRUE),
            sd_prop = sd(prop_change, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lower_area = mean_area_diff - sd_area_diff,
         upper_area = mean_area_diff + sd_area_diff,
         lower_perc = mean_perc - sd_perc,
         upper_perc = mean_perc + sd_perc,
         lower_prop = mean_prop - sd_prop,
         upper_prop = mean_prop + sd_prop)

# get unique subareas (in the same order as plotting)
subareas <- levels(factor(data_sum$subarea))

# create a dataframe for background rectangles
bg_df <- data.frame(
  subarea = subareas,
  xmin = seq_along(subareas) - 0.5,
  xmax = seq_along(subareas) + 0.5,
  fill = rep(c("grey95", "white"), length.out = length(subareas)))

# rename the subareas from codes to names
data_sum$subarea <- case_match(data_sum$subarea,
                               "Division 58.4.1" ~ "Enderby-Wilkes West",
                               "Division 58.4.2" ~ "Enderby-Wilkes East",
                               "Division 58.5.1" ~ "Kerguelen",
                               "Division 58.5.2" ~ "Heard",
                               "Subarea 48.1" ~ "Antarctic Peninsula",
                               "Subarea 48.2" ~ "South Orkney",
                               "Subarea 48.3" ~ "South Georgia",
                               "Subarea 48.4" ~ "South Sandwich",
                               "Subarea 48.5" ~ "Weddell Sea",
                               "Subarea 48.6" ~ "Queen Maud Land",
                               "Subarea 58.6" ~ "Crozet",
                               "Subarea 58.7" ~ "Marion",
                               "Subarea 88.1" ~ "Eastern Ross Sea",
                               "Subarea 88.2" ~ "Western Ross Sea",
                               "Subarea 88.3" ~ "Amundsen-Bellingshausen Seas",
                               .default = data_sum$subarea)

# manually reorder subareas
data_sum$subarea <- factor(data_sum$subarea, levels = rev(c("Marion", "Crozet", "Kerguelen", "Heard", "Macquarie",
                                                            "Falklands", "South Georgia", "South Sandwich", 
                                                            "Bouvet", "South Orkney", "Antarctic Peninsula", 
                                                            "Weddell Sea", "Queen Maud Land", "Enderby-Wilkes West",
                                                            "Enderby-Wilkes East", "Eastern Ross Sea", "Western Ross Sea",
                                                            "Amundsen-Bellingshausen Seas")))

# truncate upper and lower proportionate change to -1 to 1
data_sum <- data_sum %>%
  mutate(upper_prop = ifelse(upper_prop > 1, 1, upper_prop),
         mean_prop = ifelse(mean_prop > 1, 1, mean_prop)) %>%
  mutate(lower_prop = ifelse(lower_prop < -1, -1, lower_prop),
         mean_prop = ifelse(mean_prop < -1, -1, mean_prop))

# plot proportionate change
p_prop <- ggplot(data_sum, aes(x = subarea, y = mean_prop)) +
  geom_rect(data = bg_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), 
            inherit.aes = FALSE, alpha = 0.5) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(color = species), size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lower_prop, ymax = upper_prop, color = species), position = position_dodge2(width = 0.5)) + 
  scale_fill_manual(values = c("grey95" = "grey80", "white" = "white"), guide = "none") +
  scale_color_viridis_d(end = 0.8, option = "mako") +
  facet_wrap(~SSP) +
  coord_flip() +
  labs(y = "Proportionate Core Habitat Change",
       x = "Subarea",
       color = "Species") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12))

# view proportionate change plot
p_prop + ggview::canvas(width = 12, height = 12)

# export
ggsave("output/climatic model/figures/climatic_proportionate_area_differences.png", 
       p_prop, width = 12, height = 12, units = "in", dpi = 300)

# truncate mean percentage values above 100
data_sum <- data_sum %>%
  mutate(mean_perc = ifelse(mean_perc > 100, 100, mean_perc),
         upper_perc = ifelse(upper_perc > 100, 100, upper_perc)) %>%
  mutate(mean_perc = ifelse(mean_perc < -100, -100, mean_perc),
         lower_perc = ifelse(lower_perc < -100, -100, lower_perc))

# plot percentage change
ggplot(data_sum, aes(x = subarea, y = mean_perc)) +
  geom_rect(data = bg_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), 
            inherit.aes = FALSE, alpha = 0.5) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(color = species), size = 3, position = position_dodge(width = 0.5)) +
  #geom_linerange(aes(ymin = lower_perc, ymax = upper_perc, color = species), position = position_dodge2(width = 0.5)) + 
  scale_fill_manual(values = c("grey95" = "grey80", "white" = "white"), guide = "none") +
  scale_color_viridis_d(end = 0.8, option = "mako") +
  facet_wrap(~SSP) +
  coord_flip() +
  labs(y = "Projected Habitat Area Difference (km²)",
       x = "Subarea",
       color = "Species") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12))


# create subantarctic vs antarctic category to allow for differences of scale
data_sum <- data_sum %>%
  mutate(region = ifelse(subarea %in% c("Marion", "Crozet", "Kerguelen", "Heard", "Macquarie",
                                        "Falklands", "South Georgia", "South Sandwich", 
                                        "Bouvet", "South Orkney"), "Islands", "Antarctic Continent"))

# get maximum absolute values for islands and antarctic continent
max_island <- max(abs(data_sum %>% filter(region == "Islands") %>% 
                        select(mean_area_diff, lower_area, upper_area)), na.rm = TRUE)
max_continent <- max(abs(data_sum %>% filter(region == "Antarctic Continent") %>% 
                          select(mean_area_diff, lower_area, upper_area)), na.rm = TRUE)

# plot area change for islands only
p1 <- ggplot(data_sum %>% filter(region == "Islands"), aes(x = subarea, y = mean_area_diff)) +
  geom_rect(data = bg_df[1:10,], aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), 
            inherit.aes = FALSE, alpha = 0.5) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(color = species), size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lower_area, ymax = upper_area, color = species), position = position_dodge2(width = 0.5)) + 
  scale_fill_manual(values = c("grey95" = "grey80", "white" = "white"), guide = "none") +
  scale_color_viridis_d(end = 0.8, option = "mako") +
  facet_wrap(~SSP) +
  ylim(-max_island, max_island) +
  coord_flip() +
  labs(y = "Projected Habitat Area Difference (km²)",
       x = "Subarea",
       color = "Species") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12))

# plot area change for antarctic continent only
p2 <- ggplot(data_sum %>% filter(region == "Antarctic Continent"), aes(x = subarea, y = mean_area_diff)) +
  geom_rect(data = bg_df[1:8,], aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill), 
            inherit.aes = FALSE, alpha = 0.5) + 
  geom_hline(yintercept = 0) +
  geom_point(aes(color = species), size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lower_area, ymax = upper_area, color = species), position = position_dodge2(width = 0.5)) + 
  scale_fill_manual(values = c("grey95" = "grey80", "white" = "white"), guide = "none") +
  scale_color_viridis_d(end = 0.8, option = "mako") +
  facet_wrap(~SSP) +
  ylim(-max_continent, max_continent) +
  coord_flip() +
  labs(y = "Projected Habitat Area Difference (km²)",
       x = "Subarea",
       color = "Species") +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12))

# get species legend
legend <- get_legend(p1)

# grid of the two main plots
dualplot <- plot_grid(p1 + ylab("") + xlab("") + guides(col = "none"),
          p2 + xlab("") + theme(strip.text.x = element_blank()) + guides(col = "none"), 
          ncol = 1, align = "v", rel_heights = c(0.55, 0.45))

# add legend
plot_grid(dualplot, legend, nrow = 1, rel_widths = c(0.9, 0.15))
