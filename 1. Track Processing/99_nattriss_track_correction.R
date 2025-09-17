# list all unique ids
nattriss_ids <- unique(nattriss_tracks$individual_id)

# get id
id <- nattriss_ids[20]

# define post-moult trip ID
pm <- "1354_12"

# get track
this_track <- nattriss_tracks %>% filter(individual_id == id)

# read in original tracks
original_tracks <- readRDS(paste0("newdata/", species, "_filtered_tracks.RDS")) %>%
  filter(individual_id %in% nattriss_ids)

# get this track
this_original <- original_tracks %>%
  filter(individual_id == id)

# get dates of original track
these_dates <- unique(this_original$date)

# calculate time difference to the nearest original date
this_track <- this_track %>%
  rowwise() %>%
  mutate(
    nearest_ref = these_dates[which.min(abs(as.numeric(difftime(date, these_dates, units = "hours"))))],
    diff_hours   = difftime(date, nearest_ref, units = "hours")
  ) %>%
  ungroup()

# if time difference is over two hours, remove
altered <- this_track %>%
  filter(abs(diff_hours) < 2 | trip == pm)

# plot to compare
this_track_terra <- vect(this_track, geom = c("x", "y"), crs = "epsg:6932")
altered_terra <- vect(altered, geom = c("x", "y"), crs = "epsg:6932")

ggplot() +
  geom_spatvector(data = this_track_terra, col = "black") +
  geom_spatvector(data = altered_terra, col = "red")


# join to all other altered segments
if(id == nattriss_ids[1]){
  all_altered <- altered
} else {
  all_altered <- rbind(all_altered, altered)
}

# plot all altered
all_altered %>%
  vect(geom = c("x", "y"), crs = "epsg:6932") %>%
  plot(pch = ".")

nattriss_tracks <- all_altered
saveRDS(nattriss_tracks, "stages/new/CHPE_nattriss_tracks_with_stage_trips.RDS")
