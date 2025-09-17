# correct stage assignments based on obvious errors and the literature
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/PenguinTrack")

library(tidyverse)
library(tidyterra)

#------------------------------------------------------------------------------
# Adelie Penguins
#------------------------------------------------------------------------------

# define species
species <- "ADPE"

# read in automatically assigned stage dates
stage_dates <- readRDS(paste0("stages/new/", species, "_stages_prelim.RDS")) %>% 
  ungroup()

# read in tracks with trip IDs
tracks <- readRDS(paste0("stages/new/", species, "_tracks_with_stage_trips.RDS"))

# append deployment sites from tracks to stage dates
deps <- tracks %>%
  group_by(individual_id, device_id) %>%
  summarise(deployment_site = first(deployment_site))
stage_dates <- stage_dates %>%
  left_join(deps)
rm(deps)

# unique deployment sites 
deployment_sites <- unique(tracks$deployment_site)
deployment_sites


# 1. Humble Island, Anvers Island
# all correctly assigned as chick-rearing

# get stages
humble_stages <- stage_dates %>% filter(deployment_site == "Humble Island, Anvers Island")

# get tracks
humble_tracks <- tracks %>% filter(deployment_site == "Humble Island, Anvers Island")

# remove artificial looping trips
humble_tracks <- humble_tracks %>%
  filter(!trip %in% c("437134.12_2"))


# 2. Torgersen Island, Anvers Island
# all correctly assigned as chick-rearing

# get stages
torgersen_stages <- stage_dates %>% filter(deployment_site == "Torgersen Island, Anvers Island")

# get tracks
torgersen_tracks <- tracks %>% filter(deployment_site == "Torgersen Island, Anvers Island")

# remove artificial looping trips
torgersen_tracks <- torgersen_tracks %>%
  filter(!trip %in% c("0488.5_1"))

# 3. Biscoe Point, Anvers Island
# all correctly assigned as chick-rearing

# get stages
biscoe_stages <- stage_dates %>% filter(deployment_site == "Biscoe Point, Anvers Island")

# get tracks
biscoe_tracks <- tracks %>% filter(deployment_site == "Biscoe Point, Anvers Island")

# remove artificial looping trips
biscoe_tracks <- biscoe_tracks %>%
  filter(!trip %in% c("04AA.49_2", "04E7.21_2", "04E7.31_3", "1A7D.32_8"))


# 4. PT8, Anvers Island
# all correctly assigned as chick-rearing

# get stages
pt8_stages <- stage_dates %>% filter(deployment_site == "PT8, Anvers Island")

# get tracks
pt8_tracks <- tracks %>% filter(deployment_site == "PT8, Anvers Island")


# 5. Hukuro Cove, Syowa
# all correctly assigned as chick-rearing

# get stages
hukuro_stages <- stage_dates %>% filter(deployment_site == "Hukuro Cove, Syowa")

# get tracks
hukuro_tracks <- tracks %>% filter(deployment_site == "Hukuro Cove, Syowa")


# 6. Mizukuguri Cove, Syowa
# all correctly assigned as chick-rearing

# get stages
mizukuguri_stages <- stage_dates %>% filter(deployment_site == "Mizukuguri Cove, Syowa")

# get tracks
mizukuguri_tracks <- tracks %>% filter(deployment_site == "Mizukuguri Cove, Syowa")


# 7. Joubin Islands, Anverse Island
# all correctly assigned as chick-rearing

# get stages
joubin_stages <- stage_dates %>% filter(deployment_site == "Joubin Islands, Anvers Island")

# get tracks
joubin_tracks <- tracks %>% filter(deployment_site == "Joubin Islands, Anvers Island")


# 8. Hop Island, East Antarctica
# some chick-rearing trips misassigned as incubation

# get tracks
hop_tracks <- tracks %>% filter(deployment_site == "Hop Island, East Antarctica")

# trip IDs that need changing to chick-rearing
cr <- c("Adelie005_3", "Adelie006_1", "Adelie006_2", "Adelie007_0", 
        "Adelie007_2", "Adelie008_1", "Adelie008_2", "Adelie009_0", 
        "Adelie009_2", "Adelie010_0", "Adelie010_3", "Adelie011_0",
        "Adelie012_2", "Adelie013_3", "Adelie015_0")

# trip IDs that need removing
remove <- c("Adelie007_1", "Adelie010_1", "Adelie012_1", "Adelie012_4", 
            "Adelie012_8", "Adelie021_2", "Adelie026_2", "Adelie027_7")

# trip IDs that need changing to incubation
inc <- c("Adelie030_1")

# remove error trips
hop_tracks <- hop_tracks %>%
  filter(!trip %in% remove)

# change stages where necessary
hop_tracks <- hop_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
hop_stages <- hop_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 9. Cape Bird, Ross Sea
# should all be chick-rearing, but some labelled as incubation

# get tracks
bird_tracks <- tracks %>% filter(deployment_site == "Cape Bird, Ross Sea")

# trip IDs that need removing
remove <- c("band68648_1", "band68353_1", "band68611_1", "band30454_2")

# remove error trips
bird_tracks <- bird_tracks %>%
  filter(!trip %in% remove)

# change stage to chick-rearing for remaining tracks
bird_tracks <- bird_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
bird_stages <- bird_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 10. Cape Hallett, Ross Sea
# all correctly labelled as chick-rearing

# get stages
hallett_stages <- stage_dates %>% filter(deployment_site == "Cape Hallett, Ross Sea")

# get tracks
hallett_tracks <- tracks %>% filter(deployment_site == "Cape Hallett, Ross Sea")


# 11. Inexpressible Island, Ross Sea
# should all be chick-rearing but all labelled as incubation

# get stages
inexpressible_stages <- stage_dates %>% filter(deployment_site == "Inexpressible Island, Ross Sea")

# get tracks
inexpressible_tracks <- tracks %>% filter(deployment_site == "Inexpressible Island, Ross Sea")

# change stage names in both
inexpressible_tracks <- inexpressible_tracks %>%
  mutate(stage = "chick-rearing")
inexpressible_stages <- inexpressible_stages %>%
  mutate(stage = "chick-rearing")


# 12. Terra Nova Bay, Ross Sea
# all correctly assigned as chick-rearing

# get stages
terranova_stages <- stage_dates %>% filter(deployment_site == "Terra Nova Bay, Ross Sea")

# get tracks
terranova_tracks <- tracks %>% filter(deployment_site == "Terra Nova Bay, Ross Sea")


# 13. Powell Island, South Orkney
# some need reassigning as pre-moult

# get tracks
powell_tracks <- tracks %>% filter(deployment_site == "Powell Island, South Orkney")

# trip IDs that need changing to pre-moult
pm <- c("BAS_South_Orkney_ADPE_1219_20", "BAS_South_Orkney_ADPE_1220_11",
        "BAS_South_Orkney_ADPE_1221_6", "BAS_South_Orkney_ADPE_1222_10",
        "BAS_South_Orkney_ADPE_1223_8", "BAS_South_Orkney_ADPE_1241_3",
        "BAS_South_Orkney_ADPE_1242_5", "BAS_South_Orkney_ADPE_1243_6",
        "BAS_South_Orkney_ADPE_1247_28", "BAS_South_Orkney_ADPE_1248_9")

# change stages where necessary
powell_tracks <- powell_tracks %>%
  mutate(stage = case_when(
    trip %in% pm ~ "pre-moult",
    TRUE ~ stage
  ))

# plot
trax <- vect(powell_tracks, geom = c("x", "y"), crs = "epsg:6932")
ggplot() +
  geom_spatvector(data = trax, aes(col = stage))

# create new stage dates
powell_stages <- powell_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 14. Signy Island, South Orkney
# some need reassigning as pre-moult

# get tracks
signy_tracks <- tracks %>% filter(deployment_site == "Signy Island, South Orkney")

# trip IDs that need changing to pre-moult
pm <- c("BAS_South_Orkney_ADPE_1224_7", "BAS_South_Orkney_ADPE_1225_7",
        "BAS_South_Orkney_ADPE_1226_6", "BAS_South_Orkney_ADPE_1227_1",
        "BAS_South_Orkney_ADPE_1228_5", "BAS_South_Orkney_ADPE_1229_1",
        "BAS_South_Orkney_ADPE_1230_3", "BAS_South_Orkney_ADPE_1231_10",
        "BAS_South_Orkney_ADPE_1232_4", "BAS_South_Orkney_ADPE_1233_6")

# change stages where necessary
signy_tracks <- signy_tracks %>%
  mutate(stage = case_when(
    trip %in% pm ~ "pre-moult",
    TRUE ~ stage
  ))

# plot
trax <- vect(signy_tracks, geom = c("x", "y"), crs = "epsg:6932")
ggplot() +
  geom_spatvector(data = trax, aes(col = stage))

# create new stage dates
signy_stages <- signy_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 15. Laurie Island, South Orkney
# some need reassigning as pre-moult

# get tracks
laurie_tracks <- tracks %>% filter(deployment_site == "Laurie Island, South Orkney")

# trip IDs that need changing to pre-moult
pm <- c("BAS_South_Orkney_ADPE_1300_8", "BAS_South_Orkney_ADPE_1299_3",
        "BAS_South_Orkney_ADPE_1298_5", "BAS_South_Orkney_ADPE_1297_1")

# change stages where necessary
laurie_tracks <- laurie_tracks %>%
  mutate(stage = case_when(
    trip %in% pm ~ "pre-moult",
    TRUE ~ stage
  ))

# plot
trax <- vect(laurie_tracks, geom = c("x", "y"), crs = "epsg:6932")
ggplot() +
  geom_spatvector(data = trax, aes(col = stage))

# create new stage dates
laurie_stages <- laurie_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 16. Esperanza, Antarctic Peninsula
# some need reassigning as pre-moult

# get tracks
esperanza_tracks <- tracks %>% filter(deployment_site == "Esperanza, Antarctic Peninsula")

# trip IDs that need changing to pre-moult
pm <- c("depid1249_11", "depid1245_14", "depid1240_3", "depid1237_4",
        "depid1236_19")

# change stages where necessary
esperanza_tracks <- esperanza_tracks %>%
  mutate(stage = case_when(
    trip %in% pm ~ "pre-moult",
    TRUE ~ stage
  ))

# create new stage dates
esperanza_stages <- esperanza_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 17. Bring all the tracks and stages together

# combine all tracks
all_tracks <- bind_rows(
  humble_tracks, torgersen_tracks, biscoe_tracks, pt8_tracks,
  hukuro_tracks, mizukuguri_tracks, joubin_tracks, hop_tracks,
  bird_tracks, hallett_tracks, inexpressible_tracks, terranova_tracks,
  powell_tracks, signy_tracks, laurie_tracks, esperanza_tracks
)

# combine all stages
all_stages <- bind_rows(
  humble_stages, torgersen_stages, biscoe_stages, pt8_stages,
  hukuro_stages, mizukuguri_stages, joubin_stages, hop_stages,
  bird_stages, hallett_stages, inexpressible_stages, terranova_stages,
  powell_stages, signy_stages, laurie_stages, esperanza_stages
)

# reproject tracks to EPSG:4326
library(terra)
all_tracks <- all_tracks %>%
  vect(geom = c("x", "y"), crs = "epsg:6932") %>%
  project("epsg:4326") %>%
  as.data.frame(geom = "XY")

# rename and select key columns
all_tracks <- all_tracks %>%
  rename(lat = y, lon = x) %>%
  select(individual_id, device_id, date, lon, lat, lon_se_km, lat_se_km,
         deployment_site, stage)

# export stages and tracks
saveRDS(all_stages,
        paste0("stages/new/", species, "_stages_corrected.RDS"))
saveRDS(all_tracks,
        paste0("newdata/", species, "_ssm_qc_tracks.RDS"))


#------------------------------------------------------------------------------
# Chinstrap Penguins
#------------------------------------------------------------------------------

# reset
rm(list=ls())

# define species
species <- "CHPE"

# read in automatically assigned stage dates
stage_dates <- readRDS(paste0("stages/new/", species, "_stages_prelim.RDS")) %>% 
  ungroup()

# read in tracks with trip IDs
tracks <- readRDS(paste0("stages/new/", species, "_tracks_with_stage_trips.RDS"))

# append deployment sites from tracks to stage dates
deps <- tracks %>%
  group_by(individual_id, device_id) %>%
  summarise(deployment_site = first(deployment_site))
stage_dates <- stage_dates %>%
  left_join(deps)
rm(deps)

# unique deployment sites 
deployment_sites <- unique(tracks$deployment_site)
deployment_sites %>% sort()


# 1. Barton Peninsula, South Shetland Islands
# should all be chick-rearing but some labelled as incubation

# get stages
barton_stages <- stage_dates %>% filter(deployment_site == "Barton Peninsula, South Shetland Islands")

# get tracks
barton_tracks <- tracks %>% filter(deployment_site == "Barton Peninsula, South Shetland Islands")

# change stage to chick-rearing for remaining tracks
barton_tracks <- barton_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
barton_stages <- barton_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 2. Ardley Island, South Shetland
# should all be chick-rearing but all labelled as incubation

# get stages
ardley_stages <- stage_dates %>% filter(deployment_site == "Ardley Island, South Shetland")

# get tracks
ardley_tracks <- tracks %>% filter(deployment_site == "Ardley Island, South Shetland")

# change stage names in both
ardley_tracks <- ardley_tracks %>%
  mutate(stage = "chick-rearing")
ardley_stages <- ardley_stages %>%
  mutate(stage = "chick-rearing")


# 3. Deception Island, South Shetland
# should all be chick-rearing but some labelled as incubation

# get stages
deception_stages <- stage_dates %>% filter(deployment_site == "Deception Island, South Shetland")

# get tracks
deception_tracks <- tracks %>% filter(deployment_site == "Deception Island, South Shetland")

# change stage to chick-rearing for remaining tracks
deception_tracks <- deception_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
deception_stages <- deception_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 4. Harmony Point, South Shetland Islands
# should all be chick-rearing but some labelled as incubation

# get stages
harmony_stages <- stage_dates %>% filter(deployment_site == "Harmony Point, South Shetland Islands")

# get tracks
harmony_tracks <- tracks %>% filter(deployment_site == "Harmony Point, South Shetland Islands")

# change stage to chick-rearing for remaining tracks
harmony_tracks <- harmony_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
harmony_stages <- harmony_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 5. Nattriss Point, Saunders Islands
# lots of looping movements removed manually and saved separately
# see file 99_nattriss_track_correction
# some pre-moult trips assigned as chick-rearing
# all else should be chick-rearing

# get stages
nattriss_stages <- stage_dates %>% filter(deployment_site == "Nattriss Point, Saunders Island")

# get tracks
nattriss_tracks <- readRDS("stages/new/CHPE_nattriss_tracks_with_stage_trips.RDS")

# trip IDs that need changing to pre-moult
pm <- c("1336_9", "1337_6", "1344_22", "1345_18", "1346_24",
        "1347_32", "1348_16", "1351_9", "1354_12", "1355_33")

# change stages where necessary
nattriss_tracks <- nattriss_tracks %>%
  mutate(stage = case_when(
    trip %in% pm ~ "pre-moult",
    TRUE ~ "chick-rearing"
  ))

# create new stage dates
nattriss_stages <- nattriss_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean up
rm(pm)


# 6. Nyroysa, Bouvetoya
# some chick-rearing trips mislabelled as incubation

# get stages
nyroysa_stages <- stage_dates %>% filter(deployment_site == "Nyroysa, Bouvetoya")

# get tracks
nyroysa_tracks <- tracks %>% filter(deployment_site == "Nyroysa, Bouvetoya")

# trip IDs that need changing to chick-rearing
cr <- c("Tag14405_2", "Tag14408_1", "Tag14408_2", "Tag14408_3", "Tag14466_3",
        "Tag14466_4", "Tag14469_3", "Tag14469_4", "Tag14469_5", "Tag14480_1",
        "Tag14480_2", "Tag14480_3", "Tag14505_1", "Tag14505_2", "Tag14505_3",
        "Tag14505_4", "Tag14507_2", "Tag41018_0", "Tag41018_1", "Tag41018_2",
        "Tag41018_3", "Tag41023_2", "Tag41028_1", "Tag41028_2", "Tag41065_1",
        "Tag41065_1")

# change stages where necessary
nyroysa_tracks <- nyroysa_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    TRUE ~ stage
  ))

# create new stage dates
nyroysa_stages <- nyroysa_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean up
rm(cr)


# 7. Signy Island, South Orkney
# lots of incubating and chick-rearing trips mislabelled

# get stages
signy_stages <- stage_dates %>% filter(deployment_site == "Signy Island, South Orkney")

# get tracks
signy_tracks <- tracks %>% filter(deployment_site == "Signy Island, South Orkney")

# trip IDs that need changing to incubation
inc <- c("SIC073_1", "SIC074_3", "SIC075_3", "SIC078_1", "SIC080_1",
         "SIC081_1", "SIC083_1")

# trip IDs that need changing to chick-rearing - CONTINUE 105
cr <- c("SIC091_1", "SIC091_3", "SIC091_5", "SIC092_1",
        "SIC092_4", "SIC093_1", "SIC094_4", "SIC096_2",
        "SIC096_3", "SIC098_3", "SIC098_5", "SIC102_1", 
        "SIC102_3", "SIC102_9", "SIC102_11", "SIC103_1", 
        "SIC103_3", "SIC105_5", "SIC105_8", "SIC106_3",
        "SIC106_7", "SIC106_9", "SIC107_6", "SIC108_1",
        "SIC108_3", "SIC108_5", "SIC109_7", "SIC109_9",
        "SIC110_1", "SIC110_3", "SIC111_1", "SIC111_4", 
        "SIC111_6", "SIC111_7", "SIC112_1", "SIC112_2",
        "SIC112_3", "SIC112_5", "SIC113_1", "SIC113_3",
        "SIC113_5", "SIC114_3", "SIC114_5", "SIC114_7",
        "SIC114_9", "SIC114_10", "SIC116_2", "SIC117_1",
        "SIC118_1", "SIC118_4", "SIC118_6", "SIC119_1",
        "SIC119_10", "SIC120_1", "SIC120_3", "SIC120_4",
        "SIC121_3", "SIC122_1", "SIC128_2")

# change stages where necessary
signy_tracks <- signy_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
signy_stages <- signy_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean up
rm(cr, inc)


# 18. Bring all the tracks and stages together

# combine all tracks
all_tracks <- bind_rows(
  barton_tracks, ardley_tracks, deception_tracks, harmony_tracks,
  nattriss_tracks, nyroysa_tracks, signy_tracks
)

# combine all stages
all_stages <- bind_rows(
  barton_stages, ardley_stages, deception_stages, harmony_stages,
  nattriss_stages, nyroysa_stages, signy_stages
)

# remove deployment_site
all_stages <- all_stages %>%
  select(-deployment_site)

# reproject tracks to EPSG:4326
library(terra)
all_tracks <- all_tracks %>%
  vect(geom = c("x", "y"), crs = "epsg:6932") %>%
  project("epsg:4326") %>%
  as.data.frame(geom = "XY")
all_tracks %>%
  vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  plot(pch = ".")

# rename and select key columns
all_tracks <- all_tracks %>%
  rename(lat = y, lon = x) %>%
  select(individual_id, device_id, date, lon, lat, lon_se_km, lat_se_km,
         deployment_site, stage)

# export stages and tracks
saveRDS(all_stages,
        paste0("stages/new/", species, "_stages_corrected.RDS"))
saveRDS(all_tracks,
        paste0("newdata/", species, "_ssm_qc_tracks.RDS"))
