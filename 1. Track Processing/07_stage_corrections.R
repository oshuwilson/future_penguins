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

# trip IDs that need changing to chick-rearing 
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


# 8. Bring all the tracks and stages together

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


#------------------------------------------------------------------------------
# King Penguins
#------------------------------------------------------------------------------

# reset
rm(list=ls())

# define species
species <- "KIPE"

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


# 1. Green Gorge, Macquarie Island
# should all be chick-rearing but some labelled as incubation

# get stages
gorge_stages <- stage_dates %>% filter(deployment_site == "Green Gorge, Macquarie Island")

# get tracks
gorge_tracks <- tracks %>% filter(deployment_site == "Green Gorge, Macquarie Island")

# change stage to chick-rearing for remaining tracks
gorge_tracks <- gorge_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
gorge_stages <- gorge_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 2. Hound Bay, South Georgia
# one erroneous trip, others have breeding stage info from SeabirdTracking

# get stages
hound_stages <- stage_dates %>% filter(deployment_site == "Hound Bay, South Georgia")

# get tracks
hound_tracks <- tracks %>% filter(deployment_site == "Hound Bay, South Georgia")

# erroneous trip
err <- "BAS_SG_KIPE_10_2"

# trips that should be assigned as incubation but aren't
inc <- c("BAS_SG_KIPE_1a_0", "BAS_SG_KIPE_1b_1", "BAS_SG_KIPE_2a_1", "BAS_SG_KIPE_2b_1",
         "BAS_SG_KIPE_3a_1", "BAS_SG_KIPE_4a_1", "BAS_SG_KIPE_4b_0", "BAS_SG_KIPE_5a_0",
         "BAS_SG_KIPE_9_1", "BAS_SG_KIPE_9_2", "BAS_SG_KIPE_9_3", "BAS_SG_KIPE_9_4",
         "BAS_SG_KIPE_9_5", "BAS_SG_KIPE_depid988_1", "BAS_SG_KIPE_depid988_0", "BAS_SG_KIPE_depid989_0")

# trips that should be assigned as chick-rearing but aren't
cr <- c("BAS_SG_KIPE_GPS_H3_0", "BAS_SG_KIPE_GPS_H5_1", "BAS_SG_KIPE_GPS_H6_0", "BAS_SG_KIPE_GPS_H9_0",
        "BAS_SG_KIPE_GPS_P3_1", "BAS_SG_KIPE_GPS_P4_1", "BAS_SG_KIPE_P1_0")

# change stages where necessary
hound_tracks <- hound_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# remove erroneous track
hound_tracks <- hound_tracks %>%
  filter(trip != err)

# create new stage dates
hound_stages <- hound_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(inc, cr, err)


# 3. Kildalkey Bay, Marion Island
# true stages available from SeabirdTracking

# get stages
kildalkey_stages <- stage_dates %>%
  filter(deployment_site == "Kildalkey Bay, Marion Island")

# get tracks
kildalkey_tracks <- tracks %>%
  filter(deployment_site == "Kildalkey Bay, Marion Island")

# trips that should be assigned as incubation but aren't
inc <- c("KIPE_Marion_201848_1", "KIPE_Marion_201860_1", "KIPE_Marion_201861_1",
         "KIPE_Marion_201862_1", "KIPE_Marion_201863_1", "KIPE_Marion_201864_1",
         "KIPE_Marion_201865_1", "KIPE_Marion_201866_1", "KIPE_Marion_201867_1",
         "KIPE_Marion_201868_0", "KIPE_Marion_201869_1", "KIPE_Marion_201871_0",
         "KIPE_Marion_201872_1", "KIPE_Marion_201873_1", "KPN20_022018_0")

# trips that should be assigned as chick-rearing but aren't
cr <- c("KIPE_Marion_201811_1", "KIPE_Marion_201814_1", "KIPE_Marion_20184_1",
        "KIPE_Marion_201854_1", "KIPE_Marion_201857_1", "KIPE_Marion_201858_1",
        "KIPE_Marion_201875_1", "KIPE_Marion_201875_2", "KIPE_Marion_201876_1",
        "KNP11_21012017_1", "KNP11_21012017_2", "KNP15_21012017_1", "KNP16_21012017_1",
        "KNP17_21012017_0", "KNP22_21012017_1", "KPN13_022018_0", "KPN14_022018_0",
        "KPN15_022018_1", "KPN16_022018_0", "KPN18_022018_0")

# change stages where necessary
kildalkey_tracks <- kildalkey_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
kildalkey_stages <- kildalkey_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(cr, inc)


# 4. Ratmanoff, Kerguelen Islands
# should all be incubation according to SeabirdTracking data, and have all been correctly assigned

# get stages
ratmanoff_stages <- stage_dates %>%
  filter(deployment_site == "Ratmanoff, Kerguelen Islands")

# get tracks
ratmanoff_tracks <- tracks %>%
  filter(deployment_site == "Ratmanoff, Kerguelen Islands")


# 5. Salisbury Plain, South Georgia
# should all be chick-rearing but some should be post-guard

# get stages
salisbury_stages <- stage_dates %>%
  filter(deployment_site == "Salisbury Plain, South Georgia")

# get tracks
salisbury_tracks <- tracks %>%
  filter(deployment_site == "Salisbury Plain, South Georgia")

# trips that should be post-guard
pg <- c("BAS_SG_KIPE_35_1", "BAS_SG_KIPE_39_1")

# change stages where necessary
salisbury_tracks <- salisbury_tracks %>%
  mutate(stage = case_when(
    trip %in% pg ~ "post-guard",
    TRUE ~ stage
  ))

# create new stage dates
salisbury_stages <- salisbury_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(pg)


# 6. Sandy Bay, Macquarie Island
# should all be chick-rearing but all labelled as incubation

# get stages
sandy_stages <- stage_dates %>%
  filter(deployment_site == "Sandy Bay, Macquarie Island")

# get tracks
sandy_tracks <- tracks %>%
  filter(deployment_site == "Sandy Bay, Macquarie Island")

# change all stages to chick-rearing
sandy_tracks <- sandy_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
sandy_stages <- sandy_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 7. Ship's Cove, Marion Island
# use SeabirdTracking data to correct

# get stages
ship_stages <- stage_dates %>%
  filter(deployment_site == "Ship's Cove, Marion Island")

# get tracks
ship_tracks <- tracks %>%
  filter(deployment_site == "Ship's Cove, Marion Island")

# trips that should be incubation
inc <- c("KIPE_Marion_201828_0", "KIPE_Marion_201832_0", "KIPE_Marion_201835_0",
         "KIPE_Marion_201836_0", "KIPE_Marion_201837_0", "KIPE_Marion_201839_0")

# trips that should be chick-rearing
cr <- c("KIPE_Marion_201816_0", "KIPE_Marion_201823_0", "KIPE_Marion_201825_0",
        "KIPE_Marion_201829_0", "KIPE_Marion_201855_0", "KIPE_Marion_201856_0",
        "KIPE_Marion_20189_0")

# erroneous trip
err <- "KIPE_Marion_20188_1"

# remove erroneous trip
ship_tracks <- ship_tracks %>%
  filter(trip != err)

# change stages where necessary
ship_tracks <- ship_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
ship_stages <- ship_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(cr, inc)


# 8. Bring all the tracks and stages together

# combine all tracks
all_tracks <- bind_rows(
  gorge_tracks, hound_tracks, kildalkey_tracks, ratmanoff_tracks,
  salisbury_tracks, sandy_tracks, ship_tracks
)

# combine all stages
all_stages <- bind_rows(
  gorge_stages, hound_stages, kildalkey_stages, ratmanoff_stages,
  salisbury_stages, sandy_stages, ship_stages
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


#------------------------------------------------------------------------------
# Macaroni/Royal Penguins
#------------------------------------------------------------------------------

# define species
species <- "MAPE"

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


# 1. Bauer Bay, Macquarie Island
# trips labelled as incubation that should be early or late chick-rearing
# true stages are listed in provided metadata from Jaslyn Allnut

# get stages
bauer_stages <- stage_dates %>% filter(deployment_site == "Bauer Bay, Macquarie Island")

# get tracks
bauer_tracks <- tracks %>% filter(deployment_site == "Bauer Bay, Macquarie Island")

# trips that should be early chick-rearing
ecr <- c("Royal_BB_35448_1", "Royal_BB_35486_1", "Royal_BB_35486_3", "Royal_BB_35486_4",
        "Royal_BB_35486_6", "Royal_BB_35486_7", "Royal_BB_35486_8", "Royal_BB_35486_10")

# trips that should be late chick-rearing
lcr <- c("Royal_BB_35468_1", "Royal_BB_35471_1", "Royal_BB_35501_1", "Royal_BB_35540_1",
         "Royal_BB_35543_1", "Royal_BB_35620_1", "Royal_BB_35620_3")

# change stages where necessary
bauer_tracks <- bauer_tracks %>%
  mutate(stage = case_when(
    trip %in% ecr ~ "early chick-rearing",
    trip %in% lcr ~ "late chick-rearing",
    TRUE ~ stage
  ))

# create new stage dates
bauer_stages <- bauer_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# remove the NA stage date at the end
bauer_stages <- bauer_stages %>%
  filter(!is.na(stage))

# clean
rm(ecr, lcr)


# 2. Cap Cotter, Kerguelen Islands
# should all be early chick-rearing

# get stages
cotter_stages <- stage_dates %>% filter(deployment_site == "Cap Cotter, Kerguelen Islands")

# get tracks
cotter_tracks <- tracks %>% filter(deployment_site == "Cap Cotter, Kerguelen Islands")

# change all stages to early chick-rearing
cotter_tracks <- cotter_tracks %>%
  mutate(stage = "early chick-rearing")

# create new stage dates
cotter_stages <- cotter_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 3. Hurd Point, Macquarie Island
# trips labelled as incubation that should be late chick-rearing
# true stages are listed in provided metadata from Jaslyn Allnut

# get stages
hurd_stages <- stage_dates %>% filter(deployment_site == "Hurd Point, Macquarie Island")

# get tracks
hurd_tracks <- tracks %>% filter(deployment_site == "Hurd Point, Macquarie Island")

# trips that should be late chick-rearing
lcr <- c("Royal_HP_35454_1", "Royal_HP_35460_1", "Royal_HP_35461_1", "Royal_HP_35461_3",
         "Royal_HP_35461_5", "Royal_HP_35461_7", "Royal_HP_35461_9", "Royal_HP_35461_10",
         "Royal_HP_35500_2", "Royal_HP_35500_4", "Royal_HP_35527_1", "Royal_HP_35527_2",
         "Royal_HP_35529_1", "Royal_HP_35537_1", "Royal_HP_35546_1", "Royal_HP_35546_2",
         "Royal_HP_35546_4")

# change stages where necessary
hurd_tracks <- hurd_tracks %>%
  mutate(stage = case_when(
    trip %in% lcr ~ "late chick-rearing",
    TRUE ~ stage
  ))

# create new stage dates
hurd_stages <- hurd_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(lcr)


# 4. Nyroysa, Bouvetoya
# early chick-rearing and incubation trips mostly right, but some mislabelled
# name of individual indicated breeding stage status

# get stages
nyroysa_stages <- stage_dates %>% filter(deployment_site == "Nyroysa, Bouvetoya")

# get tracks
nyroysa_tracks <- tracks %>% filter(deployment_site == "Nyroysa, Bouvetoya")

# change trips so that trips beginning cr become early chick-rearing and trips beggining inc become incubation
nyroysa_tracks <- nyroysa_tracks %>%
  mutate(stage = case_when(
    grepl("cr", individual_id) ~ "early chick-rearing",
    grepl("inc", individual_id) ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
nyroysa_stages <- nyroysa_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 5. Rogers Head, Heard Island
# all trips should be early chick-rearing

# get stages
rogers_stages <- stage_dates %>% filter(deployment_site == "Rogers Head, Heard Island")

# get tracks
rogers_tracks <- tracks %>% filter(deployment_site == "Rogers Head, Heard Island")

# change all stages to early chick-rearing
rogers_tracks <- rogers_tracks %>%
  mutate(stage = "early chick-rearing")

# create new stage dates
rogers_stages <- rogers_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 6. Bring all the tracks and stages together

# combine all tracks
all_tracks <- bind_rows(bauer_tracks, cotter_tracks, hurd_tracks, nyroysa_tracks, rogers_tracks)

# combine all stages
all_stages <- bind_rows(bauer_stages, cotter_stages, hurd_stages, nyroysa_stages, rogers_stages)

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


#------------------------------------------------------------------------------
# Emperor Penguins
#------------------------------------------------------------------------------

# define species
species <- "EMPE"

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


# 1. Cape Washington, Ross Sea
# all correctly assigned as chick-rearing

# get stages
washington_stages <- stage_dates %>% filter(deployment_site == "Cape Washington, Ross Sea")

# get tracks
washington_tracks <- tracks %>% filter(deployment_site == "Cape Washington, Ross Sea")


# 2. Bring all the tracks and stages together

# combine all tracks
all_tracks <- bind_rows(washington_tracks)

# combine all stages
all_stages <- bind_rows(washington_stages)

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



#------------------------------------------------------------------------------
# Gentoo Penguins
#------------------------------------------------------------------------------

# define species
species <- "GEPE"

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


# 1. Admiralty Bay, South Shetland
# some erroneous trips to remove

# get stages
admiralty_stages <- stage_dates %>% filter(deployment_site == "Admiralty Bay, South Shetland")

# get tracks
admiralty_tracks <- tracks %>% filter(deployment_site == "Admiralty Bay, South Shetland")

# erroneous trips to remove
err <- c("GEPE14_1", "GEPE14_2", "GEPE15_1", "GEPE56_2", "GEPE58_1",
         "GEPE80_2")

# remove erroneous trips
admiralty_tracks <- admiralty_tracks %>%
  filter(!trip %in% err)

# create new stage dates
admiralty_stages <- admiralty_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(err)


# 2. Ardley Island, South Shetland
# some erroneous trips to remove

# get stages
ardley_stages <- stage_dates %>% filter(deployment_site == "Ardley Island, South Shetland")

# get tracks
ardley_tracks <- tracks %>% filter(deployment_site == "Ardley Island, South Shetland")

# erroneous trips to remove
err <- c("P003_2122_2", "P003_2122_3", "P011_2021_6", "P014_1920_1",
         "P018_2021_2", "P019_2122_1", "P020_2122_2", "P023_2122_2",
         "P023_2122_4")

# remove erroneous trips
ardley_tracks <- ardley_tracks %>%
  filter(!trip %in% err)

# create new stage dates
ardley_stages <- ardley_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(err)


# 3. Barton Peninsula, South Shetland Islands
# some erroneous trips to remove

# get stages
barton_stages <- stage_dates %>% filter(deployment_site == "Barton Peninsula, South Shetland Islands")

# get tracks
barton_tracks <- tracks %>% filter(deployment_site == "Barton Peninsula, South Shetland Islands")

# erroneous trips to remove
err <- c("2016-2017 G03_1_1", "2016-2017 G06_1_1", "2016-2017 G09_1",
         "2018-2019 G06_0", "2018-2019 G17_1")

# remove erroneous trips
barton_tracks <- barton_tracks %>%
  filter(!trip %in% err)

# create new stage dates
barton_stages <- barton_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(err)


# 4. Bauer Bay, Macquarie Island
# all Gentoos should be chick-rearing according to metadata provided
# one erroneous trip

# get stages
bauer_stages <- stage_dates %>% filter(deployment_site == "Bauer Bay, Macquarie Island")

# get tracks
bauer_tracks <- tracks %>% filter(deployment_site == "Bauer Bay, Macquarie Island")

# erroneous trip to remove
err <- "Gentoo_BB_35498_6"

# remove erroneous trip
bauer_tracks <- bauer_tracks %>%
  filter(trip != err)

# change all stages to chick-rearing
bauer_tracks <- bauer_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
bauer_stages <- bauer_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(err)


# 5. Bullard Beach, Marion Island
# should both be chick-rearing (different breeding season at Marion)

# get stages
bullard_stages <- stage_dates %>% filter(deployment_site == "Bullard Beach, Marion Island")

# get tracks
bullard_tracks <- tracks %>% filter(deployment_site == "Bullard Beach, Marion Island")

# change all stages to chick-rearing
bullard_tracks <- bullard_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
bullard_stages <- bullard_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 6. Cape Shirreff, South Shetland Islands
# some erroneous trips to remove
# some chick-rearing trips mislabelled as post-breeding

# get stages
shirreff_stages <- stage_dates %>% filter(deployment_site == "Cape Shirreff, South Shetland Islands")

# get tracks
shirreff_tracks <- tracks %>% filter(deployment_site == "Cape Shirreff, South Shetland Islands")

# erroneous trips to remove
err <- c("Shirreff_GEPE110_1", "Shirreff_GEPE125_1", "Shirreff_GEPE55_6", "Shirreff_GEPE73_9")

# chick-rearing trips that are mislabelled
cr <- c("Shirreff_GEPE10_5", "Shirreff_GEPE10_6", "Shirreff_GEPE115_0",
        "Shirreff_GEPE133_1", "Shirreff_GEPE135_1", "Shirreff_GEPE135_2",
        "Shirreff_GEPE141_14", "Shirreff_GEPE141_15", "Shirreff_GEPE141_20",
        "Shirreff_GEPE142_1", "Shirreff_GEPE142_3", "Shirreff_GEPE146_0",
        "Shirreff_GEPE146_7", "Shirreff_GEPE146_17", "Shirreff_GEPE150_5",
        "Shirreff_GEPE151_6", "Shirreff_GEPE153_0", "Shirreff_GEPE153_3")

# remove erroneous trips
shirreff_tracks <- shirreff_tracks %>%
  filter(!trip %in% err)

# change stages where necessary
shirreff_tracks <- shirreff_tracks %>%
  mutate(stage = case_when(
    trip %in% cr ~ "chick-rearing",
    TRUE ~ stage
  ))

# create new stage dates
shirreff_stages <- shirreff_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(err, cr)


# 7. Devil's Point, South Shetland
# should all be chick-rearing according to Masello et al. (2021)

# get stages
devils_stages <- stage_dates %>% filter(deployment_site == "Devil's Point, South Shetland")

# get tracks
devils_tracks <- tracks %>% filter(deployment_site == "Devil's Point, South Shetland")

# change all stages to chick-rearing
devils_tracks <- devils_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
devils_stages <- devils_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 8. Duiker's Point, Marion Island
# Marion birds 1 to 7 from 2018 should be incubation, others chick-rearing
# all Duiker's Point birds are therefore incubation

# get stages
duikers_stages <- stage_dates %>% filter(deployment_site == "Duiker's Point, Marion Island")

# get tracks
duikers_tracks <- tracks %>% filter(deployment_site == "Duiker's Point, Marion Island")

# change all stages to incubation
duikers_tracks <- duikers_tracks %>%
  mutate(stage = "incubation")

# create new stage dates
duikers_stages <- duikers_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 9. Funk Bay, Marion Island
# same as above, so all are chick-rearing here

# get stages
funk_stages <- stage_dates %>% filter(deployment_site == "Funk Bay, Marion Island")

# get tracks
funk_tracks <- tracks %>% filter(deployment_site == "Funk Bay, Marion Island")

# change all stages to chick-rearing
funk_tracks <- funk_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
funk_stages <- funk_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 10. Harmony Point, South Shetland
# all correctly assigned

# get stages
harmony_stages <- stage_dates %>% filter(deployment_site == "Harmony Point, South Shetland")

# get tracks
harmony_tracks <- tracks %>% filter(deployment_site == "Harmony Point, South Shetland")


# 11. Hurd Point, Macquarie Island
# all should be chick-rearing according to metadata provided

# get stages
hurd_stages <- stage_dates %>% filter(deployment_site == "Hurd Point, Macquarie Island")

# get tracks
hurd_tracks <- tracks %>% filter(deployment_site == "Hurd Point, Macquarie Island")

# change all stages to chick-rearing
hurd_tracks <- hurd_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
hurd_stages <- hurd_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 12. Kopaitic Island, Antarctic Peninsula
# all correctly assigned

# get stages
kopaitic_stages <- stage_dates %>% filter(deployment_site == "Kopaitic Island, Antarctic Peninsula")

# get tracks
kopaitic_tracks <- tracks %>% filter(deployment_site == "Kopaitic Island, Antarctic Peninsula")


# 13. Landing Beach, South Georgia
# true stages are available from SeabirdTracking data

# get stages
landing_stages <- stage_dates %>% filter(deployment_site == "Landing Beach, South Georgia")

# get tracks
landing_tracks <- tracks %>% filter(deployment_site == "Landing Beach, South Georgia")

# trips that should be incubation
inc <- c("LandingBeach_726_1", "LandingBeach_728_0", "LandingBeach_728_1",
         "LandingBeach_728_2", "LandingBeach_739_2", "LandingBeach_739_1",
         "LandingBeach_746_1", "LandingBeach_748_1", "LandingBeach_748_2",
         "LandingBeach_748_3", "LandingBeach_748_4", "LandingBeach_748_5",
         "LandingBeach_748_6", "LandingBeach_749_1", "LandingBeach_749_2",
         "LandingBeach_754_1", "LandingBeach_754_2")

# erroneous trips
err <- c("LandingBeach_750_1")

# remove erroneous trips
landing_tracks <- landing_tracks %>%
  filter(!trip %in% err)

# change stages where necessary
landing_tracks <- landing_tracks %>%
  mutate(stage = case_when(
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
landing_stages <- landing_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(inc, err)


# 14. Lower Natural Arch, South Georgia
# true stages are available from SeabirdTracking data
# all are correct

# get stages
arch_stages <- stage_dates %>% filter(deployment_site == "Lower Natural Arch, South Georgia")

# get tracks
arch_tracks <- tracks %>% filter(deployment_site == "Lower Natural Arch, South Georgia")


# 15. New Island, Falkland Islands
# all should be chick-rearing according to Masello et al. (2021)
# all are correct

# get stages
new_stages <- stage_dates %>% filter(deployment_site == "New Island, Falkland Islands")

# get tracks
new_tracks <- tracks %>% filter(deployment_site == "New Island, Falkland Islands")


# 16. Signy Island, South Orkney
# stage data in provided metadata from BAS website

# get stages
signy_stages <- stage_dates %>% filter(deployment_site == "Signy Island, South Orkney")

# get tracks
signy_tracks <- tracks %>% filter(deployment_site == "Signy Island, South Orkney")

# trips that should be incubation
inc <- c("SIG002_IGOTU_4_A08270_GPS_1", "SIG004_IGOTU_9_A09515_GPS_1",
         "SIG005_IGOTU_13_A09512_GPS_1", "SIG009_IGOTU_20_A08264_GPS_1",
         "SIG009_IGOTU_20_A08264_GPS_2", "SIG010_IGOTU_15_A08268_GPS_1")

# change stages where necessary
signy_tracks <- signy_tracks %>%
  mutate(stage = case_when(
    trip %in% inc ~ "incubation",
    TRUE ~ stage
  ))

# create new stage dates
signy_stages <- signy_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

# clean
rm(inc)


# 17. Triegaardt Bay, Marion Island
# should all be chick-rearing

# get stages
triegaardt_stages <- stage_dates %>% filter(deployment_site == "Triegaardt Bay, Marion Island")

# get tracks
triegaardt_tracks <- tracks %>% filter(deployment_site == "Triegaardt Bay, Marion Island")

# change all stages to chick-rearing
triegaardt_tracks <- triegaardt_tracks %>%
  mutate(stage = "chick-rearing")

# create new stage dates
triegaardt_stages <- triegaardt_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 18. Upper Natural Arch, South Georgia
# true stages are available from SeabirdTracking data
# should be incubation

# get stages
upperarch_stages <- stage_dates %>% filter(deployment_site == "Upper Natural Arch, South Georgia")

# get tracks
upperarch_tracks <- tracks %>% filter(deployment_site == "Upper Natural Arch, South Georgia")

# change all stages to incubation
upperarch_tracks <- upperarch_tracks %>%
  mutate(stage = "incubation")

# create new stage dates
upperarch_stages <- upperarch_tracks %>%
  group_by(individual_id, device_id, stage) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()


# 19. VJM Station, Macquarie Island
# all should be chick-rearing according to metadata provided
# all are correct

# get stages
vjm_stages <- stage_dates %>% filter(deployment_site == "VJM Station Macquarie Island")

# get tracks
vjm_tracks <- tracks %>% filter(deployment_site == "VJM Station Macquarie Island")


# 20. Bring all the tracks and stages together
# combine all tracks
all_tracks <- bind_rows(
  admiralty_tracks, ardley_tracks, barton_tracks, bauer_tracks, bullard_tracks,
  shirreff_tracks, devils_tracks, duikers_tracks, funk_tracks, harmony_tracks,
  hurd_tracks, kopaitic_tracks, landing_tracks, arch_tracks, new_tracks,
  signy_tracks, triegaardt_tracks, vjm_tracks
)

# combine all stages
all_stages <- bind_rows(
  admiralty_stages, ardley_stages, barton_stages, bauer_stages, bullard_stages,
  shirreff_stages, devils_stages, duikers_stages, funk_stages, harmony_stages,
  hurd_stages, kopaitic_stages, landing_stages, arch_stages, new_stages,
  signy_stages, triegaardt_stages, vjm_stages
)

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
