Sys.setenv(MAPBOX_TOKEN = "pk.eyJ1IjoicHJpbmNlb2IiLCJhIjoiY205eDVvY3JqMHhkNjJrc2FpaTY5cHp0YyJ9.dr1pXDjpG2p_yZ4R5OzYGA")
library(spanishoddata)
library(flowmapblue)
library(tidyverse)
library(sf)

# Correct data directory setting - remove the extra quotes
spod_set_data_dir(data_dir = "/Users/prince/Documents/Masters Thesis/spanish_od_data")

# Flows between districts for a typical working day
od_20210407 <- spod_get("od", zones = "distr", dates = "2021-04-07")
head(od_20210407)

# District zones polygons to mathch the flows
districts_v1 <- spod_get_zones("dist", ver = 1)
head(districts_v1)

# data.frame with id, optional name, as well as lat and lon for coordinates of the locations in WGS84 (EPSG: 4326) coordinate reference system.
str(flowmapblue::ch_locations)

# data.frame with origin, dest, and count for the flows between locations, where origin and dest must match with id’s of the locations
str(flowmapblue::ch_flows)

# Aggregate data - count total flows between districts
od_20210407_total <- od_20210407 |>
  group_by(origin = id_origin, dest = id_destination) |>
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
  collect()

head(od_20210407_total)

# create location table with coordinates
districts_v1_centroids <- districts_v1 |>
  st_transform(4326) |> 
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(id = districts_v1$id) |>
  rename(lon = X, lat = Y)

head(districts_v1_centroids)

# Interactive flowmap with flowmapblue
flowmap <- flowmapblue(
  locations = districts_v1_centroids,
  flows = od_20210407_total,
  mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"),
  darkMode = TRUE,
  animation = TRUE,
  clustering = TRUE
)

flowmap


######################## Adding time filter to flows ###########################
od_20210407_time <- od_20210407 |>
  mutate(time = as.POSIXct(paste0(date, "T", hour, ":00:00"))) |>
  group_by(origin = id_origin, dest = id_destination, time) |>
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
  collect()

head(od_20210407_time)

# Zone filter Barcelona and 10Km radius around it
zones_barcelona <- districts_v1 |>
  filter(grepl("Barcelona", district_names_in_v2, ignore.case = TRUE))

zones_barcelona_fua <- districts_v1[
  st_buffer(zones_barcelona, dist = 10000)
  ,
]

zones_barcelona_fua_plot <- ggplot() +
  geom_sf(data = zones_barcelona_fua, fill=NA, col = "grey60", linewidth = 0.3) +
  theme_minimal()

zones_barcelona_fua_plot

# Table with coordinates for flowmap
zones_barcelona_fua_coords <- zones_barcelona_fua |>
  st_transform(crs = 4326) |>
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(id = zones_barcelona_fua$id) |>
  rename(lon = X, lat = Y)

head(zones_barcelona_fua_coords)

# zone ids from the zones_barcelona_fua data to select the flows that correspond to Barcelona and the 10 km radius around it.
od_20210407_time_barcelona <- od_20210407_time |>
  filter(origin %in% zones_barcelona_fua$id & dest %in% zones_barcelona_fua$id)

# Visualise the flows for Barcelona and surrounding areas
flowmap_time <- flowmapblue(
  locations = zones_barcelona_fua_coords,
  flows = od_20210407_time_barcelona,
  mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"),
  darkMode = TRUE,
  animation = TRUE,
  clustering = TRUE
)

flowmap_time
