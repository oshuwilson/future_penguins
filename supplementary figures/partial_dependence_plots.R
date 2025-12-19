#------------------------------------------------------
# Combine PDPs across models for each species and stage
#------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)

# definitions
species <- "MAPE"
longname <- "Macaroni Penguin"
stage <- "pre-moult"
model <- "at-sea model"

# list files that contain the species, stage, and "pdp"
files <- list.files(path = paste0("output/", model), 
                    pattern = paste0(species, "_", stage, "_pdp_values"), 
                    full.names = TRUE,
                    recursive = TRUE)

# for each file
for(file in files){
  
  # get model name from file
  model_name <- str_split(file, pattern = "/")[[1]][3]
  
  # read in the file
  pdp_data <- readRDS(file)
  
  # add model name
  pdp_data <- pdp_data %>%
    mutate(algorithm = model_name)
  
  # join to other files
  if(file == files[1]) {
    pdp <- pdp_data
  } else {
    pdp <- bind_rows(pdp, pdp_data)
  }
}

# change model algorithm codes
pdp <- pdp %>%
  mutate(algorithm = case_when(
    algorithm == "generalised additive models" ~ "GAM",
    algorithm == "random forests" ~ "RF",
    algorithm == "boosted regression trees" ~ "BRT",
    algorithm == "maxent" ~ "MaxEnt",
    algorithm == "bayesian additive regression trees" ~ "BART"
  ))  %>%
  filter(algorithm != "MaxEnt")

# scale all yhat values to 0-1
pdp <- pdp %>%
  group_by(var, algorithm) %>%
  mutate(yhat = (yhat - min(yhat)) / (max(yhat) - min(yhat))) %>%
  ungroup()

# for each variable
for(this_var in unique(pdp$var)){
  
  # get x values from RF models
  x_vals <- pdp %>%
    filter(algorithm == "RF" & var == this_var) %>%
    pull(x)
  
  # select gam x values closest to RF x values
  gam_x_vals <- pdp %>%
    filter(algorithm == "GAM" & var == this_var) %>%
    pull(x)
  closest_vals <- sapply(x_vals, function(x) gam_x_vals[which.min(abs(gam_x_vals - x))])
  gam <- pdp %>%
    filter(algorithm == "GAM" & var == this_var) %>%
    filter(x %in% closest_vals)
  
  # bring dataset together
  pdp_var <- pdp %>%
    filter(var == this_var & algorithm != "GAM") %>%
    bind_rows(gam)
  
  # join to other variables
  if(this_var == unique(pdp$var)[1]){
    pdp_final <- pdp_var
  } else {
    pdp_final <- bind_rows(pdp_final, pdp_var)
  }
}
pdp <- pdp_final

# list of substitute names for variables
var_names <- c(
  "depth" = "Depth (m)",
  "slope" = "Seafloor Slope (°)",
  "dshelf" = "Distance to Shelf Break (km)",
  "sst" = "Sea Surface Temperature (°C)",
  "sal" = "Salinity (PSU)",
  "mld" = "Mixed Layer Depth (m)",
  "sic" = "Sea Ice Concentration (%)",
  "curr" = "Current Velocity (m/s)"
)

# rename variables
pdp <- pdp %>%
  mutate(var = recode(var, !!!var_names))

# plot
p1 <- ggplot(pdp, aes(x = x, y = yhat)) +
  geom_line(aes(group = algorithm), col = "grey70", lwd = 0.5, alpha = 0.75) +
  geom_smooth(method = "gam", se = F, col = "black", lwd = 1) +
  facet_wrap(~var, scales = "free_x", nrow = 2) + 
  ylim(-0.01, 1.01) + 
  theme_bw() +
  ylab("Partial Effect") + 
  xlab("Predictor Values") +
  ggtitle(paste0(longname, " (", stage, ")"))
p1

# export
ggsave(p1, filename = paste0("output/imagery/pdps/", model, "/", species, "_", stage, "_pdp_combined.png"),
       width = 14, height = 8)
