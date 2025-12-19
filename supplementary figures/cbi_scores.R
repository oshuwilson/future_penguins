#-------------------------------------------------
# Boyce Index scores for each species and stage
#-------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# definitions
species <- "CHPE"
longname <- "Chinstrap Penguin"
stage <- "chick-rearing"
model <- "at-sea model"

# list files that contain the species, stage, and "cbi"
files <- list.files(path = paste0("output/", model), 
                    pattern = paste0(species, "_", stage, "_cbi_scores"), 
                    full.names = TRUE,
                    recursive = TRUE)

# read the files and combine them into a single data frame
for(file in files){
  
  # read in scores
  cbi <- readRDS(file)
  
  # get model name from file
  model_name <- str_split(file, pattern = "/")[[1]][3]
  
  # add model name
  cbi <- cbi %>%
    mutate(algorithm = model_name) %>%
    select(.estimate, algorithm, subarea)
  
  # join to other files
  if(file == files[1]){
    cbi_all <- cbi
  } else {
    cbi_all <- bind_rows(cbi_all, cbi)
  }
}

# change model algorithm codes
cbi_all <- cbi_all %>%
  mutate(algorithm = case_when(
    algorithm == "generalised additive models" ~ "GAM",
    algorithm == "random forests" ~ "RF",
    algorithm == "boosted regression trees" ~ "BRT",
    algorithm == "maxent" ~ "MaxEnt",
    algorithm == "bayesian additive regression trees" ~ "BART"
  ))  %>%
  filter(algorithm != "MaxEnt")

# table of GAR Names and equivalent interpretable names
names <- data.frame(subarea = c("Division 58.5.1", "Division 58.5.2", "Subarea 38.1", "Bouvet Island",
                                 "Macquarie Island", "Falkland Islands", "Subarea 48.1", "Subarea 48.2",
                                 "Subarea 48.3", "Subarea 48.4", "Subarea 48.5", "Subarea 48.6",
                                 "Division 58.4.1", "Division 58.4.2", "Subarea 58.6", "Subarea 58.7",
                                 "Subarea 88.1", "Subarea 88.2", "Subarea 88.3", "Tierra del Fuego",
                                 "Division 58.4.3a", "Division 58.4.3b", "Division 58.4.4a", "Division 58.4.4b"),
                    common_name = c("Kerguelen", "Heard", "Chilean Islands", "Bouvet",
                                    "Macquarie", "Falklands", "Antarctic Peninsula", "South Orkney",
                                    "South Georgia", "South Sandwich", "Weddell Sea", "Queen Maud Land",
                                    "Enderby-Wilkes East", "Enderby-Wilkes West", "Crozet", "Marion",
                                    "Eastern Ross Sea", "Western Ross Sea", "Amundsen-Bellingshausen Seas", "Tierra del Fuego",
                                    "Southwest of Heard", "Southeast of Heard", "South of Marion", "South of Crozet"))

# join to cbi_all
cbi_all <- cbi_all %>%
  left_join(names, by = "subarea")

ggplot(cbi_all, aes(x = algorithm, y = .estimate)) +
  geom_hline(yintercept = 0.4, linetype = "dashed", col = "grey30") +
  geom_hline(yintercept = 0, linetype = "solid", col = "grey10") +
  geom_point(aes(col = common_name), alpha = 0.8, size = 3) +
  theme_bw() +
  ylim(-1, 1) +
  ylab("Continuous Boyce Index") +
  xlab("Algorithm") +
  scale_color_viridis_d(option = "B", end = 0.9, name = "Test Subarea") +
  ggtitle(paste0("Spatial Cross Validation Scores: ", longname, " (", stage, ")")) 


#-------------------------------------------------------------------------------
# Overall boyce index scores
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# define model
model <- "at-sea model"

# list all files for this model
files <- list.files(path = paste0("output/", model), 
                    pattern = "_cbi_scores", 
                    full.names = TRUE,
                    recursive = TRUE)

# remove cbi scores containing maxent
files <- files[!str_detect(files, "maxent")]

# for each file
for(file in files){
  
  # get the algorithm
  algo <- str_split(file, pattern = "/")[[1]][3]
  
  # get the species
  species <- str_split(basename(file), pattern = "_")[[1]][1]
  
  # read in scores
  cbi <- readRDS(file)
  
  # if model is climatic model, arrange by mean score and take the best setup
  if(model == "climatic model"){
    match <- cbi %>%
      arrange(desc(mean)) %>%
      slice(1) %>%
      select(-mean, -std_err)
    mean_cbi <- cbi %>%
      semi_join(match) %>%
      select(mean) %>%
      mutate(algorithm = algo,
             species = species) %>%
      rename(mean_cbi = mean)
    
  } else {
  
  # get mean cbi
  mean_cbi <- cbi %>%
    summarise(mean_cbi = mean(.estimate, na.rm = T)) %>%
    mutate(algorithm = algo,
           species = species)
  }
  
  # combine with other files
  if(file == files[1]){
    cbi_overall <- mean_cbi
  } else {
    cbi_overall <- bind_rows(cbi_overall, mean_cbi)
  }
}

# mean per species
cbi_mean <- cbi_overall %>%
  mutate(species = case_when(
    species == "CHPE" ~ "Chinstrap",
    species == "EMPE" ~ "Emperor",
    species == "GEPE" ~ "Gentoo",
    species == "ADPE" ~ "Adelie",
    species == "MAPE" ~ "Macaroni",
    species == "KIPE" ~ "King"
  )) %>%
  group_by(species, algorithm) %>%
  summarise(mean_cbi = mean(mean_cbi, na.rm = T)) %>%
  ungroup()

# recode algorithm names
cbi_mean <- cbi_mean %>%
  mutate(algorithm = case_when(
    algorithm == "generalised additive models" ~ "GAM",
    algorithm == "random forests" ~ "RF",
    algorithm == "boosted regression trees" ~ "BRT",
    algorithm == "bayesian additive regression trees" ~ "BART"
  ))

# get minimum value
min_cbi <- min(cbi_mean$mean_cbi)
if(min_cbi > 0){
  min_cbi <- 0
}

# plot 
p <- ggplot(cbi_mean, aes(x = algorithm, y = mean_cbi, fill = species)) +
  geom_hline(yintercept = 0, linetype = "solid", col = "grey10") +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 1) +
  annotate(geom = "rect", xmin = 0.3, xmax = 4.7, ymin = 0.39, ymax = 0.41,
           fill = "grey80", alpha = 0.8) +
  theme_bw() +
  scale_y_continuous(limits = c(min_cbi, 1), expand = expansion(c(0.05, 0.05))) +
  scale_x_discrete(expand = c(0,0)) +
  ylab("Continuous Boyce Index") +
  xlab("Algorithm") +
  scale_fill_manual(values = c("#000004", "#00768B", "#84206B", 
                               "#C9404A", "#F67F13", "#F6D645")) 
p + ggview::canvas(width = 8, height = 6)

# export
ggsave(paste0("text/figures/draft/supplementary/cbi_scores_", 
              str_replace_all(tolower(model), " ", "_"), ".png"),
       p, width = 8, height = 6)
