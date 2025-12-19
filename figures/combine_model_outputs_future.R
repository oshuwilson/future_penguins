#-------------------------------------------------------------------------------
# Bring Together Present-Day Predictions
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)
library(scam)

# loop over scenarios
for(scenario in c("ssp126")){
  print(scenario)
  
  # define species
  species <- "GEPE"
  longname <- "Gentoo Penguin"
  
  # list of all GCMs
  gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
            "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")
  
  # loop over each gcm
  for(gcm in gcms){
    print(gcm)
    
    #-------------------------------------------------------------------------------
    # 1. Read in accessible habitat
    #-------------------------------------------------------------------------------
    
    # load in available breeding habitat (ice-free rock at accessible elevation and distance to coast)
    subantarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_available_areas_glo90.rds"))
    if(scenario == "ssp585"){
      antarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_ssp585_areas_rema.rds"))
    } else {
      antarctic_habitat <- readRDS(paste0("output/topographic model/available areas/", species, "_ssp126_areas_rema.rds"))
    }
    
    # project antarctic habitat
    antarctic_habitat <- antarctic_habitat %>%
      project(crs(subantarctic_habitat))
    
    # combine the two
    available_habitat <- bind_spat_rows(subantarctic_habitat, antarctic_habitat)
    rm(subantarctic_habitat, antarctic_habitat)
    
    
    #-------------------------------------------------------------------------------
    # 2. Combine with climatic core habitat
    #-------------------------------------------------------------------------------
    
    # read in climatic prediction
    climatic_prediction <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_simple_ensemble.tif"))
    
    # read in threshold for suitable climatic conditions
    threshold <- readRDS(paste0("output/climatic model/thresholds/", species, "_tss_max_threshold.RDS"))
    
    # binarize climatic prediction
    climatic_core_habitat <- climatic_prediction >= threshold
    
    # convert to polygons
    climatic_core_habitat <- climatic_core_habitat %>%
      as.polygons() %>%
      filter(mean == 1)
    
    # areas that overlap with available habitat
    future_habitat <- climatic_core_habitat %>%
      project(crs(available_habitat)) %>%
      terra::intersect(available_habitat)
    #plot(future_habitat)
    
    # cleanup
    rm(list = setdiff(ls(), c("species", "scenario", "gcm", "gcms", "longname", "future_habitat")))
    
    
    #-------------------------------------------------------------------------------
    # 3. Integrate Foraging Habitat
    #-------------------------------------------------------------------------------
    
    # define possible stages
    if(species == "MAPE"){
      stage_options <- c("chick-rearing", "incubation", "pre-moult")
    } else if(species %in% c("GEPE", "EMPE")){
      stage_options <- c("chick-rearing")
    } else {
      stage_options <- c("chick-rearing", "incubation")
    }
    
    # for each stage, read in at-sea suitability
    for(this_stage in stage_options){
      
      # read in predicted ensemble suitability
      hs <- rast(paste0("output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", this_stage, "_", gcm, "_", scenario, "_simple_ensemble.tif"))
      #plot(hs)
      
      # project future habitat to at-sea model crs
      future_habitat <- future_habitat %>%
        project(crs(hs))
      
      # rasterize future habitat
      hab_rast <- rasterize(future_habitat, hs, touches = T)
      hab_rast[is.na(hab_rast)] <- 0
      hab_rast[hab_rast > 0] <- 100
      
      # read in land file
      land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sv")
      
      # crop land to below 40 degrees south
      land <- crop(land, ext(-180, 180, -90, -40))
      
      # project land to depth raster CRS
      land <- project(land, "epsg:4326")
      
      # rasterise land
      land_rast <- rasterize(land, hs, touches = T)
      land_rast[is.na(land_rast)] <- 0
      
      # add habitat and land rasters
      grd <- hab_rast + land_rast
      
      # convert land values to NA
      grd[grd == 1] <- NA
      
      # revalue habitat locations
      grd[grd == 100] <- 101
      
      # calculate distance to colonies
      dist_rast <- gridDist(grd, target = 101, scale = 1000)
      
      # read in scam model for availability
      scam <- readRDS(paste0("output/at-sea model/availability/", species, "_", this_stage, "_scam_model.RDS"))
      
      # create dataframe for prediction
      testdata <- as.points(dist_rast) %>%
        as.data.frame(geom = "XY")
      
      # rename dist2col column
      names(testdata)[1] <- "dist2col"
      
      # predict using fitted model
      pred <- predict.scam(scam, testdata, type = "response")
      pred <- as.vector(pred)
      
      # add to database
      testdata$pscam <- pred
      
      # convert prediction to points
      predscam <- testdata %>%
        vect(geom = c("x", "y"), crs = crs(dist_rast))
      
      # rasterise prediction
      predrast <- rasterize(predscam, dist_rast, field = "pscam")
      
      # multiply predicted suitability by availability
      hs_av <- hs * predrast
      
      # export suitability raster
      writeRaster(hs_av, paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_", this_stage, "_combined_suitability.tif"),
                  overwrite = TRUE)
      
      # # view around the South Atlantic
      # plot(hs_av %>% crop(ext(-90, -30, -70, -50)))
      # 
      # # view around the South Indian Ocean
      # plot(hs_av %>% crop(ext(20, 100, -60, -40)))
      # 
      # # view around Macquarie
      # plot(hs_av %>% crop(ext(150, 170, -60, -50)))
      # 
      # # project 
      # # hs_av_proj <- project(hs_av, "epsg:6932")
      # # plot(hs_av_proj)
      # 
      # stack with other stages
      if(this_stage == stage_options[1]){
        combined_stack <- hs_av
      } else {
        combined_stack <- c(combined_stack, hs_av)
      }
    }
    
    # average stages
    mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)
    plot(mean_hs_av)
    
    # export
    writeRaster(mean_hs_av, paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_mean_combined_suitability.tif"),
                overwrite = TRUE)
  }
  
  #-------------------------------------------------------------------------------
  # Visualise Across GCMs
  #-------------------------------------------------------------------------------
  
  # clean up
  rm(list=setdiff(ls(), c("species", "scenario", "longname", "gcms")))
  
  # loop over each gcm and read in mean suitability
  for(gcm in gcms){
    
    # read in mean suitability
    mean_hs_av <- rast(paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_mean_combined_suitability.tif"))
    
    # stack with others
    if(gcm == gcms[1]){
      combined_stack <- mean_hs_av
    } else {
      combined_stack <- c(combined_stack, mean_hs_av)
    }
  }
  
  # mean across GCMs
  mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)
  
  # read in coastline for plotting
  coast <- readRDS("data/coast_ice_vect.RDS")
  
  # read in threshold to outline average core habitat
  threshold <- readRDS(paste0("output/combined/thresholds/", species, "_threshold.RDS"))
  
  # classify raster using threshold
  mat1 <- matrix(c(0, threshold, 10,
                   threshold, 1, 20),
                 ncol = 3, byrow = T)
  bins <- classify(mean_hs_av, mat1)
  plot(bins)
  
  # convert to vector
  bins_vect <- bins %>%
    as.polygons(dissolve = T) %>%
    filter(mean == 20) %>%
    project("epsg:6932")
  
  # plot
  p1 <- ggplot() +
    geom_spatraster(data = mean_hs_av %>% project("epsg:6932")) +
    geom_spatvector(data = coast, col = NA, fill = "white") +
    geom_spatvector(data = bins_vect, fill = NA, col = "white") +
    scale_fill_viridis_c(na.value = "white", option = "D", name = "Habitat Suitability") +
    theme_void() +
    ggtitle(paste0(longname)) +
    theme(plot.title = element_text(hjust = 0.5))
  #p1
  
  # export
  ggsave(paste0("output/imagery/combined suitability/", species, "_", scenario, "_suitability.png"),
         plot = p1,
         width = 8, height = 6, units = "in", dpi = 300)
  
  # read in current suitability
  present <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))
  
  # calculate difference
  diff <- mean_hs_av - present
  
  # export difference
  writeRaster(diff, paste0("output/combined/projections/", scenario, "/", species, "_suitability_difference.tif"),
              overwrite = TRUE)
  
  # get max absolute value
  max_val <- abs(c(minmax(diff)[1,1], minmax(diff)[2, 1])) %>%
    max()
  
  # plot difference
  p2 <- ggplot() +
    geom_spatraster(data = diff %>% project("epsg:6932")) +
    geom_spatvector(data = coast, col = NA, fill = "white") +
    scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                         name = "Change in\nHabitat Suitability", limits = c(-max_val, max_val)) +
    theme_void() +
    ggtitle(paste0(longname, " - Change in Habitat Suitability")) +
    theme(plot.title = element_text(hjust = 0.5))
  #p2 + ggview::canvas(width = 8, height = 6)
  
  # export
  ggsave(paste0("output/imagery/combined suitability/", species, "_", scenario, "_suitability_difference.png"),
         plot = p2,
         width = 8, height = 6, units = "in", dpi = 300)
  
  
  #-------------------------------------------------------------------------------
  # Visualise Core Habitat Change Across GCMs
  #-------------------------------------------------------------------------------
  
  # read in current suitability
  present <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))
  
  # read in present core habitat bins
  bins <- rast(paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"))
  
  # predict future binary classes for each gcm
  mat2 <- matrix(c(0, threshold, 2,
                   threshold, 1, 3),
                 ncol = 3, byrow = T)
  
  # apply to gcm stack
  for(i in 1:nlyr(combined_stack)){
    bins_gcm <- classify(combined_stack[[i]], mat2)
    if(i == 1){
      gcm_bins_stack <- bins_gcm
    } else {
      gcm_bins_stack <- c(gcm_bins_stack, bins_gcm)
    }
  }
  
  # export gcm_bins_stack
  writeRaster(gcm_bins_stack, paste0("output/combined/projections/", scenario, "/", species, "_gcm_core_habitat_bins.tif"),
              overwrite = TRUE)
  
  # subtract present day from future
  change_stack <- gcm_bins_stack - bins
  
  # substitute values
  change_stack <- change_stack %>% 
    subst(c(-18, -17, -8, -7),
          c(-1, 0, NA, 1))
  
  # add values
  total <- app(change_stack, fun = "sum", na.rm = T)
  plot(total)
  
  # export
  writeRaster(total, paste0("output/combined/projections/", scenario, "/", species, "_core_habitat_change.tif"),
              overwrite = TRUE)
  
  # project
  total <- project(total, "epsg:6932")
  
  # plot core habitat change
  p3 <- ggplot() +
    geom_spatraster(data = total) +
    scale_fill_steps2(na.value = "transparent", name = "Projected Core Habitat Change", 
                      mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                      labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                     x == 8 ~ "All models\nproject gain",
                                                     TRUE ~ "")) +
    geom_spatvector(data = coast, fill = NA, col = "black") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom",
          legend.text = element_text(face = "bold")) +
    guides(fill = guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5, 
                                 barwidth = 15, 
                                 barheight = 0.5)) +
    ggtitle(paste0(longname, " (", scenario, ")"))
  #p3
  
  # save image
  ggsave(paste0("output/imagery/combined suitability/", species, "_", scenario, "_core_habitat_change.png"),
         plot = p3,
         width = 10, height = 10, units = "in", dpi = 300)
  
  #-------------------------------------------------------------------------------
  # Relate to CCAMLR Subareas
  #-------------------------------------------------------------------------------
  
  # read in CCAMLR Subareas
  subareas <- readRDS("data/subareas/CCAMLR_subareas.rds")
  plot(bins)
  plot(subareas, add = T)
  
  # convert present day bins to vector
  bins2 <- bins %>%
    as.polygons(dissolve = T) %>%
    filter(mean == 20)
  
  # split bins by subarea
  bins2 <- split(bins2, subareas)
  
  # extract subarea name for each polygon
  bins2 <- terra::intersect(bins2, subareas)
  
  # table of GAR Names and equivalent interpretable names
  names <- data.frame(GAR_Name = c("Division 58.5.1", "Division 58.5.2", "Subarea 38.1", "Subarea 38.2",
                                   "Subarea 38.5", "Subarea 38.7", "Subarea 48.1", "Subarea 48.2",
                                   "Subarea 48.3", "Subarea 48.4", "Subarea 48.5", "Subarea 48.6",
                                   "Division 58.4.1", "Division 58.4.2", "Subarea 58.6", "Subarea 58.7",
                                   "Subarea 88.1", "Subarea 88.2", "Subarea 88.3",
                                   "Division 58.4.3a", "Division 58.4.3b", "Division 58.4.4a", "Division 58.4.4b"),
                      common_name = c("Kerguelen", "Heard", "Chilean Islands", "Bouvet",
                                      "Macquarie", "Falklands", "Antarctic Peninsula", "South Orkney",
                                      "South Georgia", "South Sandwich", "Weddell Sea", "Queen Maud Land",
                                      "Enderby-Wilkes East", "Enderby-Wilkes West", "Crozet", "Marion",
                                      "Eastern Ross Sea", "Western Ross Sea", "Amundsen-Bellingshausen Seas",
                                      "Southwest of Heard", "Southeast of Heard", "South of Marion", "South of Crozet"))
  
  # recode GAR_Names into interpretable names
  bins2 <- bins2 %>%
    left_join(names, by = "GAR_Name")
  
  # plot to check assignments
  ggplot() +
    geom_spatvector(data = bins2, aes(fill = common_name), col = NA) +
    theme_void() +
    theme(legend.position = "right")
  
  # calculate area of present day core habitat in each subarea
  area_df <- bins2 %>%
    mutate(area_km2 = expanse(bins2, unit = "km")) %>%
    as.data.frame() %>%
    select(common_name, area_km2) %>%
    group_by(common_name) %>%
    summarise(present_area_km2 = sum(area_km2)) %>%
    ungroup() %>%
    mutate(present_prop = present_area_km2 / sum(present_area_km2))
  
  # repeat for all layers of gcm_bin_stack
  for(i in 1:nlyr(gcm_bins_stack)){
    
    # convert to polygons
    bins_gcm <- gcm_bins_stack[[i]] %>%
      as.polygons(dissolve = T) %>%
      filter(mean == 3)
    
    # split by subarea
    bins_gcm <- split(bins_gcm, subareas)
    
    # extract subarea name for each polygon
    bins_gcm <- terra::intersect(bins_gcm, subareas)
    
    # recode GAR_Names into interpretable names
    bins_gcm <- bins_gcm %>%
      left_join(names, by = "GAR_Name")
    
    # calculate area of future core habitat in each subarea
    area_gcm_df <- bins_gcm %>%
      mutate(area_km2 = expanse(bins_gcm, unit = "km")) %>%
      as.data.frame() %>%
      select(common_name, area_km2) %>%
      group_by(common_name) %>%
      summarise(future_area_km2 = sum(area_km2)) %>%
      ungroup() %>%
      complete(common_name = unique(names$common_name), fill = list(future_area_km2 = 0)) %>%
      mutate(gcm = gcms[i], ssp = scenario)
    
    # calculate proportion of total core habitat per subarea
    area_gcm_df <- area_gcm_df %>%
      mutate(prop_area = future_area_km2 / sum(future_area_km2))
    
    # join to other gcms
    if(i == 1){
      all_future_area <- area_gcm_df
    } else {
      all_future_area <- bind_rows(all_future_area, area_gcm_df)
    }
  }
  
  # append present day data
  all_future_area <- all_future_area %>% left_join(area_df, by = "common_name") %>%
    mutate(present_area_km2 = ifelse(is.na(present_area_km2), 0, present_area_km2),
           present_prop = ifelse(is.na(present_prop), 0, present_prop)) %>%
    mutate(diff_prop = prop_area - present_prop)
  
  # calculate mean and standard deviation across gcms
  future_summ <- all_future_area %>%
    group_by(common_name) %>%
    summarise(diff_prop_mean = mean(diff_prop),
              diff_prop_sd = sd(diff_prop)) %>%
    ungroup() %>%
    mutate(lower_prop = diff_prop_mean - (1.96 * (diff_prop_sd / sqrt(length(gcms)))),
           upper_prop = diff_prop_mean + (1.96 * (diff_prop_sd / sqrt(length(gcms)))))
  
  # manually reorder subareas
  future_summ$common_name <- factor(future_summ$common_name, levels = rev(c("Falklands", "Marion", "Crozet", "Macquarie", "Kerguelen",
                                                                            "Chilean Islands", "Heard", "South Georgia", "South Sandwich", 
                                                                            "Bouvet", "South of Marion", "South of Crozet",
                                                                            "Southwest of Heard", "Southeast of Heard", 
                                                                            "South Orkney", "Antarctic Peninsula", "Amundsen-Bellingshausen Seas", 
                                                                            "Weddell Sea", "Queen Maud Land", "Enderby-Wilkes West",
                                                                            "Enderby-Wilkes East", "Eastern Ross Sea", "Western Ross Sea"
  )))
  
  # remove subareas with no land
  future_summ <- future_summ %>%
    filter(!(common_name %in% c("South of Marion", "South of Crozet",
                                "Southwest of Heard", "Southeast of Heard")))
  
  # plot
  ggplot(future_summ, aes(x = common_name, y = diff_prop_mean)) +
    geom_point(stat = "identity", fill = "steelblue4") +
    geom_linerange(aes(ymin = lower_prop, ymax = upper_prop)) +
    coord_flip() +
    ylab("Projected Change in Proportion of Core Habitat") +
    xlab("CCAMLR Subarea") +
    ggtitle(paste0(longname, " - Projected Change in Core Habitat by CCAMLR Subarea (", scenario, ")")) +
    theme_minimal() +
    geom_hline(yintercept = 0) +
    theme(panel.spacing = unit(3, "lines"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey75"),
          strip.text = element_text(face = "bold", size = 12))
  
  # modify data for export
  future_summ <- future_summ %>%
    mutate(species = species,
           ssp = scenario)
  
  # export
  saveRDS(future_summ, paste0("output/combined/figdata/subarea_prop_change/", scenario, "_", species, "_prop_change_by_subarea.rds"))
  
}

#-------------------------------------------------------------------------------
# Alternative version for emperors using dist2coast and climatic suitability 
#-------------------------------------------------------------------------------
# emperors do not breed on land but on sea ice so cannot use same approach for 
# constraining the at-sea predictions

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(tidyterra)
library(terra)
library(scam)

# define species
species <- "EMPE"
longname <- "Emperor Penguin"

# define scenario
scenario <- "ssp585"

# list of all GCMs
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# loop over each gcm
for(gcm in gcms){
  print(gcm)
  
  #-------------------------------------------------------------------------------
  # 1. Create accessible habitat from climatic core habitat
  #-------------------------------------------------------------------------------
  
  # read in climatic prediction
  climatic_prediction <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/", species, "_", gcm, "_", scenario, "_simple_ensemble.tif"))
  
  # read in threshold for suitable climatic conditions
  threshold <- readRDS(paste0("output/climatic model/thresholds/", species, "_tss_max_threshold.RDS"))
  
  # binarize climatic prediction
  climatic_core_habitat <- climatic_prediction >= threshold
  
  # convert to polygons
  climatic_core_habitat <- climatic_core_habitat %>%
    as.polygons() %>%
    filter(mean == 1)
  
  # read in colony locations and background samples
  colonies <- readRDS(paste0("output/climatic model/extraction/", species, " extracted.rds"))
  
  # get max dist2coast of any colonies
  colony_dist2coast <- colonies %>%
    filter(pa == "presence") %>%
    pull(dist2coast) %>%
    max()
  
  # read in dist2coast file
  dist2coast <- rast("E:/Satellite_Data/static/dist2coast_emp.tif")
  
  # make values beyond max dist2coast NA
  dist2coast_masked <- dist2coast
  dist2coast_masked[dist2coast_masked > colony_dist2coast] <- NA
  plot(dist2coast_masked)
  
  # convert to polygons
  dist2coast_habitat <- dist2coast_masked %>%
    as.polygons() 
  
  # overlap of climatic suitable habitat and dist2coast habitat
  future_habitat <- climatic_core_habitat %>%
    project(crs(dist2coast_habitat)) %>%
    terra::intersect(dist2coast_habitat)
  plot(future_habitat)
  
  # cleanup
  rm(list = setdiff(ls(), c("species", "scenario", "gcm", "gcms", "longname", "future_habitat")))
  
  
  #-------------------------------------------------------------------------------
  # 2. Integrate Foraging Habitat
  #-------------------------------------------------------------------------------
  
  # define possible stages
  if(species == "MAPE"){
    stage_options <- c("chick-rearing", "incubation", "pre-moult")
  } else if(species %in% c("GEPE", "EMPE")){
    stage_options <- c("chick-rearing")
  } else {
    stage_options <- c("chick-rearing", "incubation")
  }
  
  # for each stage, read in at-sea suitability
  for(this_stage in stage_options){
    
    # read in predicted ensemble suitability
    hs <- rast(paste0("output/at-sea model/projections/", scenario, "/", gcm, "/", species, "_", this_stage, "_", gcm, "_", scenario, "_simple_ensemble.tif"))
    #plot(hs)
    
    # project future habitat to at-sea model crs
    future_habitat <- future_habitat %>%
      project(crs(hs))
    
    # rasterize future habitat
    hab_rast <- rasterize(future_habitat, hs, touches = T)
    hab_rast[is.na(hab_rast)] <- 0
    hab_rast[hab_rast > 0] <- 100
    
    # read in land file
    land <- rnaturalearth::ne_countries(scale = 10, returnclass = "sv")
    
    # crop land to below 40 degrees south
    land <- crop(land, ext(-180, 180, -90, -40))
    
    # project land to depth raster CRS
    land <- project(land, "epsg:4326")
    
    # rasterise land
    land_rast <- rasterize(land, hs, touches = T)
    land_rast[is.na(land_rast)] <- 0
    
    # add habitat and land rasters
    grd <- hab_rast + land_rast
    
    # convert land values to NA
    grd[grd == 1] <- NA
    
    # revalue habitat locations
    grd[grd == 100] <- 101
    
    # calculate distance to colonies
    dist_rast <- gridDist(grd, target = 101, scale = 1000)
    
    # read in scam model for availability
    scam <- readRDS(paste0("output/at-sea model/availability/", species, "_", this_stage, "_scam_model.RDS"))
    
    # create dataframe for prediction
    testdata <- as.points(dist_rast) %>%
      as.data.frame(geom = "XY")
    
    # rename dist2col column
    names(testdata)[1] <- "dist2col"
    
    # predict using fitted model
    pred <- predict.scam(scam, testdata, type = "response")
    pred <- as.vector(pred)
    
    # add to database
    testdata$pscam <- pred
    
    # convert prediction to points
    predscam <- testdata %>%
      vect(geom = c("x", "y"), crs = crs(dist_rast))
    
    # rasterise prediction
    predrast <- rasterize(predscam, dist_rast, field = "pscam")
    
    # multiply predicted suitability by availability
    hs_av <- hs * predrast
    
    # export suitability raster
    writeRaster(hs_av, paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_", this_stage, "_combined_suitability.tif"),
                overwrite = TRUE)
    
    # # view around the South Atlantic
    # plot(hs_av %>% crop(ext(-90, -30, -70, -50)))
    # 
    # # view around the South Indian Ocean
    # plot(hs_av %>% crop(ext(20, 100, -60, -40)))
    # 
    # # view around Macquarie
    # plot(hs_av %>% crop(ext(150, 170, -60, -50)))
    # 
    # # project 
    # # hs_av_proj <- project(hs_av, "epsg:6932")
    # # plot(hs_av_proj)
    # 
    # stack with other stages
    if(this_stage == stage_options[1]){
      combined_stack <- hs_av
    } else {
      combined_stack <- c(combined_stack, hs_av)
    }
  }
  
  # average stages
  mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)
  plot(mean_hs_av)
  
  # export
  writeRaster(mean_hs_av, paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_mean_combined_suitability.tif"),
              overwrite = TRUE)
}

#-------------------------------------------------------------------------------
# Visualise Across GCMs
#-------------------------------------------------------------------------------

# clean up
rm(list=setdiff(ls(), c("species", "scenario", "longname", "gcms")))

# loop over each gcm and read in mean suitability
for(gcm in gcms){
  
  # read in mean suitability
  mean_hs_av <- rast(paste0("output/combined/projections/", scenario, "/", gcm, "/", species, "_mean_combined_suitability.tif"))
  
  # stack with others
  if(gcm == gcms[1]){
    combined_stack <- mean_hs_av
  } else {
    combined_stack <- c(combined_stack, mean_hs_av)
  }
}

# mean across GCMs
mean_hs_av <- app(combined_stack, fun = mean, na.rm = TRUE)

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# read in threshold to outline average core habitat
threshold <- readRDS(paste0("output/combined/thresholds/", species, "_threshold.RDS"))

# classify raster using threshold
mat1 <- matrix(c(0, threshold, 10,
                 threshold, 1, 20),
               ncol = 3, byrow = T)
bins <- classify(mean_hs_av, mat1)
plot(bins)

# convert to vector
bins_vect <- bins %>%
  as.polygons(dissolve = T) %>%
  filter(mean == 20) %>%
  project("epsg:6932")

# plot
p1 <- ggplot() +
  geom_spatraster(data = mean_hs_av %>% project("epsg:6932")) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  geom_spatvector(data = bins_vect, col = "white", fill = NA) +
  scale_fill_viridis_c(na.value = "white", option = "D", name = "Habitat Suitability") +
  theme_void() +
  ggtitle(paste0(longname)) +
  theme(plot.title = element_text(hjust = 0.5))
p1 + ggview::canvas(width = 8, height = 6)

# export
ggsave(paste0("output/imagery/combined suitability/", species, "_", scenario, "_suitability.png"),
       plot = p1,
       width = 8, height = 6, units = "in", dpi = 300)

# read in current suitability
present <- rast(paste0("output/combined/predictions/", species, "_mean_combined_suitability.tif"))

# calculate difference
diff <- mean_hs_av - present

# export difference
writeRaster(diff, paste0("output/combined/projections/", scenario, "/", species, "_suitability_difference.tif"),
            overwrite = TRUE)

# get max absolute value
max_val <- abs(c(minmax(diff)[1,1], minmax(diff)[2, 1])) %>%
  max()

# plot difference
p2 <- ggplot() +
  geom_spatraster(data = diff %>% project("epsg:6932")) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  scale_fill_gradient2(na.value = "white", low = "darkred", mid = "grey90", high = "steelblue4", 
                       name = "Change in\nHabitat Suitability", limits = c(-max_val, max_val)) +
  theme_void() +
  ggtitle(paste0(longname, " - Change in Habitat Suitability")) +
  theme(plot.title = element_text(hjust = 0.5))
p2 + ggview::canvas(width = 8, height = 6)

# export
ggsave(paste0("output/imagery/combined suitability/", species, "_", scenario, "_suitability_difference.png"),
       plot = p2,
       width = 8, height = 6, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# Visualise Core Habitat Change Across GCMs
#-------------------------------------------------------------------------------

# read in habitat suitability thresholds
threshold <- readRDS(paste0("output/combined/thresholds/", species, "_threshold.RDS"))

# read in current suitability bins
bins <- rast(paste0("output/combined/predictions/", species, "_core_habitat_bins.tif"))

# predict future binary classes for each gcm
mat2 <- matrix(c(0, threshold, 2,
                 threshold, 1, 3),
               ncol = 3, byrow = T)

# apply to gcm stack
for(i in 1:nlyr(combined_stack)){
  bins_gcm <- classify(combined_stack[[i]], mat2)
  if(i == 1){
    gcm_bins_stack <- bins_gcm
  } else {
    gcm_bins_stack <- c(gcm_bins_stack, bins_gcm)
  }
}

# export gcm_bins_stack
writeRaster(gcm_bins_stack, paste0("output/combined/projections/", scenario, "/", species, "_gcm_core_habitat_bins.tif"),
            overwrite = TRUE)

# subtract present day from future
change_stack <- gcm_bins_stack - bins

# substitute values
change_stack <- change_stack %>% 
  subst(c(-18, -17, -8, -7),
        c(-1, 0, NA, 1))

# add values
total <- app(change_stack, fun = "sum", na.rm = T)
plot(total)

# export
writeRaster(total, paste0("output/combined/projections/", scenario, "/", species, "_core_habitat_change.tif"),
            overwrite = TRUE)

# project
total <- project(total, "epsg:6932")

# plot core habitat change
p3 <- ggplot() +
  geom_spatraster(data = total) +
  scale_fill_steps2(na.value = "transparent", name = "Projected Core Habitat Change", 
                    mid = "grey90", breaks = -9:9, limits = c(-9, 8),
                    labels = function(x) case_when(x == -9 ~ "All models\nproject loss",
                                                   x == 8 ~ "All models\nproject gain",
                                                   TRUE ~ "")) +
  geom_spatvector(data = coast, fill = NA, col = "black") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        legend.text = element_text(face = "bold")) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5, 
                               barwidth = 15, 
                               barheight = 0.5)) +
  ggtitle(paste0(longname, " (", scenario, ")"))
p3 + ggview::canvas(width = 10, height = 10)

# save image
ggsave(paste0("output/imagery/combined suitability/", species, "_", scenario, "_core_habitat_change.png"),
       plot = p3,
       width = 10, height = 10, units = "in", dpi = 300)

#-------------------------------------------------------------------------------
# Relate to CCAMLR Subareas
#-------------------------------------------------------------------------------

# read in CCAMLR Subareas
subareas <- readRDS("data/subareas/CCAMLR_subareas.rds")
plot(bins)
plot(subareas, add = T)

# convert present day bins to vector
bins2 <- bins %>%
  as.polygons(dissolve = T) %>%
  filter(mean == 20)

# split bins by subarea
bins2 <- split(bins2, subareas)

# extract subarea name for each polygon
bins2 <- terra::intersect(bins2, subareas)

# table of GAR Names and equivalent interpretable names
names <- data.frame(GAR_Name = c("Division 58.5.1", "Division 58.5.2", "Subarea 38.1", "Subarea 38.2",
                                 "Subarea 38.5", "Subarea 38.7", "Subarea 48.1", "Subarea 48.2",
                                 "Subarea 48.3", "Subarea 48.4", "Subarea 48.5", "Subarea 48.6",
                                 "Division 58.4.1", "Division 58.4.2", "Subarea 58.6", "Subarea 58.7",
                                 "Subarea 88.1", "Subarea 88.2", "Subarea 88.3",
                                 "Division 58.4.3a", "Division 58.4.3b", "Division 58.4.4a", "Division 58.4.4b"),
                    common_name = c("Kerguelen", "Heard", "Chilean Islands", "Bouvet",
                                    "Macquarie", "Falklands", "Antarctic Peninsula", "South Orkney",
                                    "South Georgia", "South Sandwich", "Weddell Sea", "Queen Maud Land",
                                    "Enderby-Wilkes East", "Enderby-Wilkes West", "Crozet", "Marion",
                                    "Eastern Ross Sea", "Western Ross Sea", "Amundsen-Bellingshausen Seas",
                                    "Southwest of Heard", "Southeast of Heard", "South of Marion", "South of Crozet"))

# recode GAR_Names into interpretable names
bins2 <- bins2 %>%
  left_join(names, by = "GAR_Name")

# plot to check assignments
ggplot() +
  geom_spatvector(data = bins2, aes(fill = common_name), col = NA) +
  theme_void() +
  theme(legend.position = "right")

# calculate area of present day core habitat in each subarea
area_df <- bins2 %>%
  mutate(area_km2 = expanse(bins2, unit = "km")) %>%
  as.data.frame() %>%
  select(common_name, area_km2) %>%
  group_by(common_name) %>%
  summarise(present_area_km2 = sum(area_km2)) %>%
  ungroup() %>%
  mutate(present_prop = present_area_km2 / sum(present_area_km2))

# repeat for all layers of gcm_bin_stack
for(i in 1:nlyr(gcm_bins_stack)){
  
  # convert to polygons
  bins_gcm <- gcm_bins_stack[[i]] %>%
    as.polygons(dissolve = T) %>%
    filter(mean == 3)
  
  # split by subarea
  bins_gcm <- split(bins_gcm, subareas)
  
  # extract subarea name for each polygon
  bins_gcm <- terra::intersect(bins_gcm, subareas)
  
  # recode GAR_Names into interpretable names
  bins_gcm <- bins_gcm %>%
    left_join(names, by = "GAR_Name")
  
  # calculate area of future core habitat in each subarea
  area_gcm_df <- bins_gcm %>%
    mutate(area_km2 = expanse(bins_gcm, unit = "km")) %>%
    as.data.frame() %>%
    select(common_name, area_km2) %>%
    group_by(common_name) %>%
    summarise(future_area_km2 = sum(area_km2)) %>%
    ungroup() %>%
    complete(common_name = unique(names$common_name), fill = list(future_area_km2 = 0)) %>%
    mutate(gcm = gcms[i], ssp = scenario)
  
  # calculate proportion of total core habitat per subarea
  area_gcm_df <- area_gcm_df %>%
    mutate(prop_area = future_area_km2 / sum(future_area_km2))
  
  # join to other gcms
  if(i == 1){
    all_future_area <- area_gcm_df
  } else {
    all_future_area <- bind_rows(all_future_area, area_gcm_df)
  }
}

# append present day data
all_future_area <- all_future_area %>% left_join(area_df, by = "common_name") %>%
  mutate(present_area_km2 = ifelse(is.na(present_area_km2), 0, present_area_km2),
         present_prop = ifelse(is.na(present_prop), 0, present_prop)) %>%
  mutate(diff_prop = prop_area - present_prop)

# calculate mean and standard deviation across gcms
future_summ <- all_future_area %>%
  group_by(common_name) %>%
  summarise(diff_prop_mean = mean(diff_prop),
            diff_prop_sd = sd(diff_prop)) %>%
  ungroup() %>%
  mutate(lower_prop = diff_prop_mean - (1.96 * (diff_prop_sd / sqrt(length(gcms)))),
         upper_prop = diff_prop_mean + (1.96 * (diff_prop_sd / sqrt(length(gcms)))))

# manually reorder subareas
future_summ$common_name <- factor(future_summ$common_name, levels = rev(c("Falklands", "Marion", "Crozet", "Macquarie", "Kerguelen",
                                                                          "Chilean Islands", "Heard", "South Georgia", "South Sandwich", 
                                                                          "Bouvet", "South of Marion", "South of Crozet",
                                                                          "Southwest of Heard", "Southeast of Heard", 
                                                                          "South Orkney", "Antarctic Peninsula", "Amundsen-Bellingshausen Seas", 
                                                                          "Weddell Sea", "Queen Maud Land", "Enderby-Wilkes West",
                                                                          "Enderby-Wilkes East", "Eastern Ross Sea", "Western Ross Sea"
)))

# remove subareas with no land
future_summ <- future_summ %>%
  filter(!(common_name %in% c("South of Marion", "South of Crozet",
                              "Southwest of Heard", "Southeast of Heard")))

# plot
ggplot(future_summ, aes(x = common_name, y = diff_prop_mean)) +
  geom_point(stat = "identity", fill = "steelblue4") +
  geom_linerange(aes(ymin = lower_prop, ymax = upper_prop)) +
  coord_flip() +
  ylab("Projected Change in Proportion of Core Habitat") +
  xlab("CCAMLR Subarea") +
  ggtitle(paste0(longname, " - Projected Change in Core Habitat by CCAMLR Subarea (", scenario, ")")) +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  theme(panel.spacing = unit(3, "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey75"),
        strip.text = element_text(face = "bold", size = 12))

# modify data for export
future_summ <- future_summ %>%
  mutate(species = species,
         ssp = scenario)

# export
saveRDS(future_summ, paste0("output/combined/figdata/subarea_prop_change/", scenario, "_", species, "_prop_change_by_subarea.rds"))


