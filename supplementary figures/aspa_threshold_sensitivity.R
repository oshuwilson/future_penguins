#-------------------------------------------------------------------------------
# ASPA threshold sensitivity analysis
#-------------------------------------------------------------------------------

# clean up
rm(list=ls())

library(tidyverse)
library(tidyterra)
library(terra)
library(scam)
library(cowplot)

# read in present ASPA suitability
present_df <- readRDS("output/imagery/aspas/present_df.rds")

# read in ASPA info with present penguin colonies
aspas <- read.csv("data/aspas/aspa_penguins.csv") %>%
  filter(ASPA_No != 182)

# pivot longer
aspas <- aspas %>%
  pivot_longer(cols = 2:7, names_to = "species", values_to = "colony_present")

# find aspas with species present
aspa_colonies <- aspas %>% 
  filter(colony_present == 1)

# append present area proportion
aspa_colonies <- aspa_colonies %>%
  left_join(present_df %>% 
              select(ASPA_No, species, present_area_prop),
            by = c("ASPA_No", "species"))

# get quantiles of present area proportion for ASPAs with colonies
quantiles <- quantile(aspa_colonies$present_area_prop, probs = seq(0, 1, 0.01))
df <- data.frame(quantile = names(quantiles),
                 value = quantiles) %>%
  mutate(quantile = as.numeric(gsub("%", "", quantile)))

# 95% of colonies >0.15 - iterate along 0.05 to 0.3 by 0.05

# visualise
ggplot(df, aes(x = quantile, y = value)) +
  geom_line() +
  geom_hline(yintercept = 0.15, linetype = "dashed", color = "red") +
  labs(x = "Quantile", y = "Present Area Proportion") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 12, color = "grey20"))

# read in future core habitat presence in ASPAs
core_check_126 <- readRDS("output/imagery/aspas/ssp126core_check.rds")
core_check_585 <- readRDS("output/imagery/aspas/ssp585core_check.rds")

# read in future foraging habitat proportion in ASPAs
foraging_126 <- readRDS("output/imagery/aspas/ssp126foraging_summary.rds")
foraging_585 <- readRDS("output/imagery/aspas/ssp585foraging_summary.rds")


# define minimum threshold
for(min_thresh in seq(0.05, 0.3, by = 0.05)){
  
  # for each species
  for(this_species in c("ADPE", "CHPE", "EMPE", "GEPE")){
    
    # isolate ASPAs that contain colonies
    these_aspas <- aspas %>% 
      filter(species == this_species,
             colony_present == 1) %>%
      pull(ASPA_No)
    
    # for each aspa
    for(this_aspa in these_aspas){
      
      # get present core check and area prop
      present_core_check <- present_df %>%
        filter(ASPA_No == this_aspa,
               species == this_species) %>%
        pull(present_breeding_habitat)
      present_area_prop <- present_df %>%
        filter(ASPA_No == this_aspa,
               species == this_species) %>%
        pull(present_area_prop)
      
      # does present area prop fall below min thresh
      if(present_area_prop < min_thresh) {
        subthresh <- TRUE
      } else {
        subthresh <- FALSE
      }
      
      # get ssp 126 core check and area prop
      ssp126_core_check <- core_check_126 %>%
        filter(ASPA_No == this_aspa,
               species == this_species) %>%
        pull(aspa_core_check)
      ssp126_area_prop <- foraging_126 %>%
        filter(ASPA_No == this_aspa,
               species == this_species) %>%
        pull(area_mean)
      
      # get ssp 585 core check and area prop
      ssp585_core_check <- core_check_585 %>%
        filter(ASPA_No == this_aspa,
               species == this_species) %>%
        pull(aspa_core_check)
      ssp585_area_prop <- foraging_585 %>%
        filter(ASPA_No == this_aspa,
               species == this_species) %>%
        pull(area_mean)
      
      # if mean core checks are over 0.5, value at 1
      if(mean(ssp126_core_check) > 0.5) {
        ssp126_core_check <- 1
      } else{
        ssp126_core_check <- 0
      }
      if(mean(ssp585_core_check) > 0.5) {
        ssp585_core_check <- 1
      } else {
        ssp585_core_check <- 0
      }
      
      # for each scenario, does core check become 0 and does core area fall below
      # the minimum threshold or present-day value (if already subthreshold)
      ssp126_risk <- ifelse(ssp126_area_prop >= min_thresh,
                            "Retain",
                            "Lost")
      ssp585_risk <- ifelse(ssp585_area_prop >= min_thresh,
                            "Retain",
                            "Lost")
      
      # create df for this ASPA
      df <- data.frame(
        aspa = this_aspa,
        present_core_check = present_core_check,
        present_area_prop = present_area_prop,
        ssp126_core_check = ssp126_core_check,
        ssp126_area_prop = ssp126_area_prop,
        ssp126_risk = ssp126_risk,
        ssp585_core_check = ssp585_core_check,
        ssp585_area_prop = ssp585_area_prop,
        ssp585_risk = ssp585_risk,
        species = this_species
      )
      
      # join to all ASPAs
      if(this_aspa == these_aspas[1]) {
        aspa_summary <- df
      } else {
        aspa_summary <- rbind(aspa_summary, df)
      }
    }
    
    # join to all species
    if(this_species == "ADPE") {
      final_summary <- aspa_summary
    } else {
      final_summary <- rbind(final_summary, aspa_summary)
    }
  }
  
  # if ASPA is in southern ross sea, retain (satellite imagery limited but not much loss)
  final_summary <- final_summary %>%
    mutate(ssp126_risk = ifelse(aspa %in% c(105, 121, 124, 157), "Retain", ssp126_risk),
           ssp585_risk = ifelse(aspa %in% c(105, 121, 124, 157), "Retain", ssp585_risk))
  
  # override Antarctic Peninsula Adelies
  final_summary <- final_summary %>%
    mutate(ssp585_risk = ifelse(aspa %in% c(107, 115, 117, 139) & species == "ADPE", "Lost", ssp585_risk))
  
  
  # for each species, how many ASPAs remain under each scenario
  by_species <- final_summary %>%
    group_by(species) %>%
    summarise(ssp126_retain = sum(ssp126_risk == "Retain"),
              ssp126_lost = sum(ssp126_risk == "Lost"),
              ssp585_retain = sum(ssp585_risk == "Retain")) %>%
    mutate(total = ssp126_retain + ssp126_lost) %>%
    select(-ssp126_lost)
  
  # pivot longer
  by_species <- by_species %>%
    pivot_longer(cols = 2:4, names_to = "scenario", values_to = "count") %>%
    mutate(scenario = case_when(
      scenario == "ssp126_retain" ~ "SSP1-2.6",
      scenario == "ssp585_retain" ~ "SSP5-8.5",
      scenario == "total" ~ "Present"
    ))
  
  # reorder so present comes first
  by_species$scenario <- factor(by_species$scenario, levels = c("Present", "SSP1-2.6", "SSP5-8.5"))
  
  # rename species to proper names
  by_species$species <- recode(by_species$species,
                               "ADPE" = "Adélie",
                               "CHPE" = "Chinstrap",
                               "EMPE" = "Emperor",
                               "GEPE" = "Gentoo")
  
  # min thresh as percentage
  min_thresh_perc <- min_thresh * 100
  
  # plot
  p1 <- ggplot(by_species, aes(x = species, y = count, fill = interaction(species, scenario))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    labs(x = "", y = "Number of ASPAs with Colonies") + 
    scale_fill_manual(values = c("#000004", "#005766", "#631850", "#7F242A",
                                 "grey30", "#00768B", "#C530A0", "#C9404A",
                                 "grey50", "#009CB8", "#DF7CC6", "#D26067"),
                      guide = "none") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    ggtitle(paste0("Minimum Area Proportion Threshold: ", min_thresh_perc, "%")) +
    theme(panel.spacing = unit(-0.01, "cm"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey80"), 
          axis.text = element_text(size = 11), 
          axis.title = element_text(size = 12, color = "grey20"))
  print(p1)
  
  # append to list
  if(min_thresh == 0.05){
    thresh_plots <- list(p1)
  } else {
    thresh_plots <- c(thresh_plots, list(p1))
  }
}

# plot all together
grid <- cowplot::plot_grid(plotlist = thresh_plots, ncol = 2) 
grid + ggview::canvas(10, 10)

# export grid
ggsave("text/figures/draft/supplementary/aspa_threshold_sensitivity.png", grid, width = 10, height = 12)
