# Implement Heterogeneous CVRP for office in Bengaluru

# Load libraries -------------------------------------
library(tidyverse)
library(sf)
library(tidygraph)
library(sfnetworks)

# Set working directory ------------------------------
setwd("C:\\Stuff\\Datasets\\GitHub\\cvrp_bengaluru_intel\\")

# Define location of office ----------------------------
sf_point_office <- st_point(x = c(77.6844, 12.926047), dim = "XY") %>% 
  st_sfc(crs = 4326)

# Fetch city municipality limits -----------------------
# sf_city_limits <- osmdata::opq_enclosing(lon = sf_point_office %>% st_coordinates() %>% `[[`(1),
#                                          lat = sf_point_office %>% st_coordinates() %>% `[[`(2),
#                                          key = "name", value = "Bengaluru",
#                                          enclosing = "relation", timeout = 100) %>%
#   osmdata::osmdata_sf() %>%
#   # Keep polygon
#   `$`(osm_multipolygons) %>% 
#   # Select columns
#   select(osm_id, name, admin_level, boundary)
#   
# write_rds(x = sf_city_limits, file = "sf_city_limits.rds")
sf_city_limits <- read_rds(file = "sf_city_limits.rds")

# Create bounding box for city limits ----------------------
sf_bbox <- st_bbox(sf_city_limits) %>% 
  # Convert to sf
  st_as_sfc(crs = st_crs(sf_city_limits))

# Fetch road network of city --------------------------
# feat_major_road <- osmdata::opq(bbox = sf_bbox, timeout = 100) %>%
#   osmdata::add_osm_feature(key = "highway",
#                   value = c("motorway", "motorway_link",
#                             "trunk", "trunk_link",
#                             "primary", "primary_link",
#                             "secondary", "secondary_link",
#                             "tertiary", "tertiary_link")) %>%
#   osmdata::osmdata_sf()
# 
# # Save set of roads
# sf_city_road <- bind_rows(feat_major_road$osm_lines %>%
#                             select(osm_id, highway, name),
#                           feat_major_road$osm_polygons %>%
#                             select(osm_id, highway, name) %>%
#                             st_cast(to = "LINESTRING")) %>%
#   # Truncate roads to stay within bounding box
#   st_intersection(y = sf_city_limits %>% select(geometry)) %>% 
#   # Keep columns
#   select(osm_id, highway, name)
# 
# # Save
# write_rds(x = sf_city_road, file = "city_roads.rds")
sf_city_road <- read_rds(file = "city_roads.rds")

# Fetch bus stops of city --------------------------
# sf_bus_stops <- osmdata::opq(bbox = sf_bbox, timeout = 100) %>%
#   osmdata::add_osm_feature(key = "highway",
#                   value = c("bus_stop")) %>%
#   osmdata::osmdata_sf() %>% 
#   `$`(osm_points) %>% 
#   # Only keep points within city limits
#   st_intersection(y = sf_city_limits %>% select(geometry)) %>% 
#   # keep columns
#   select(osm_id, highway, name)
# 
# # Save
# write_rds(x = sf_bus_stops, file = "sf_bus_stops.rds")
# sf_bus_stops <- read_rds(file = "sf_bus_stops.rds")

# Load Population raster ------------------------------
rs_built <- terra::rast(x = "GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C26.tif") %>% 
  # Remove layers
  tidyterra::rename(built = GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C26) %>% 
  # Crop to city bounding box
  terra::crop(y = terra::vect(x = sf_bbox %>% 
                                st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"))) %>% 
  # Change projection
  terra::project(y = "epsg:4326") %>% 
  # Mask values outside city limits
  terra::mask(mask = sf_city_limits, inverse = FALSE, updatevalue = NA)

# PLot ---------------------
plot_temp <- ggplot() +
  # tidyterra::geom_spatraster(data = rs_built) + 
  geom_sf(data = sf_city_limits, colour = "purple", linewidth = 2, fill = NA) +
  # geom_sf(data = sf_bus_stops, colour = "blue", alpha = 0.5) +
  geom_sf(data = sf_bus_stops_condensed, colour = "darkred", alpha = 0.8)
  # scale_fill_viridis_b(breaks = c(1000, 2000, 3000, 4000, 5000, 6000))

ggsave(filename = "plot_temp.png", plot = plot_temp, device = "png", 
       width = 15, height = 15, units = "cm", dpi = 300)

# # Group points within 100 metres of each other --------------------------
# sf_bus_stops_condensed <- sf_bus_stops %>% 
#   # Convert to mollweide projection
#   st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") %>% 
#   # Convert to coordinates
#   st_coordinates() %>% 
#   as.data.frame() %>% 
#   # Run dbscan algorithm
#   dbscan::dbscan(eps = 200, minPts = 1) %>% 
#   # Extract clusters for each point
#   `$`(cluster) %>% 
#   # Convert to tibble
#   as_tibble_col(column_name = "cluster_id") %>% 
#   # Bind to original tibble
#   bind_cols(sf_bus_stops, .) %>% 
#   # Group by clusters and get centroid
#   group_by(cluster_id) %>% 
#   summarise(geometry = st_union(x = geometry)) %>% 
#   st_centroid() %>% 
#   # Transform coordinates
#   st_transform(crs = 4326)

# Convert to SF object ------------------------
# sf_built <- rs_built %>% terra::as.points() %>% st_as_sf()
# Calculate weight of each clustered bus stop, proportional to the level of built up area ----------------------
# data_stop_popn <-  sf_bus_stops_condensed %>% 
#   # Calculate distance to all points in raster sf
#   st_distance(y = sf_built, by_element = FALSE) %>% 
#   # Convert to matrix
#   as.matrix() %>% 
#   # Drop units due to matrix operations being hampered when sf is loaded
#   units::drop_units() %>% 
#   # Calculate multiplier for demand based on distance
#   (function(a) exp(-(a^2)/(500^2))) %>%
#   # Multiply by demand generated  by each point
#   `%*%` (sf_built$built) %>% 
#   as.data.frame() %>% 
#   # Convert to tibble
#   as_tibble() %>% 
#   rename(popn = V1) %>% 
#   # Round figure
#   mutate(popn = round(popn)) %>% 
#   # Map Point generator fields
#   bind_cols(sf_bus_stops_condensed, .)
# write_rds(x = data_stop_popn, file = "data_stop_popn.rds")
data_stop_popn <- read_rds(file = "data_stop_popn.rds") %>% 
  # Remove stop with 0
  filter(popn > 0) %>% 
  # Normalize popn, in the range 1 to 11
  mutate(popn = ceiling(10 * (popn + 1 - min(popn))/(max(popn) - min(popn))))

# Workflow -----------------------------
# Initiate population with random order of stops
# Generate sequence of vehicles for routes based on probabilities of each type,
# proportional to number of vehicles of each type
# Start from beginning of chromosome, assign stops to that vehicle type till capacity is reached
# Repeat till all stops are assigned to vehicles, which form the final routes
# Calculate fitness function based on distance covered, and vehicle running cost
# Use OVRP for above
# Iterate till limits reached

st_geometry(sf_city_road) <- lapply(st_geometry(sf_city_road), function(x) {
  round(x, 3)
})


# Process Road Network ---------------------------------------
sfnet_road <- sf_city_road %>%
  # Convert all to Multi line string, to enable later conversion to LInestring
  st_cast(to = "MULTILINESTRING") %>% 
  st_cast(to = "LINESTRING")
  # slice_head(n = 1) %>% 
  mutate(geometry = map2(.x = geometry, .y = osm_id, .f = function(x, y) {
    print(y)
    return(round(x, 4))
  }))
  # Round precision to 3 decimals
  st_set_geometry(value = st_geometry(.) %>% 
                    lapply(FUN = function(x) round(x, 4)) %>% 
                    st_sfc(crs = st_crs(x = sf_city_road))) %>% 
  # Set speeds
  mutate(speed = case_when(str_detect(string = highway, pattern = "motorway|trunk") ~ 60L,
                           str_detect(string = highway, pattern = "primary") ~ 50L,
                           str_detect(string = highway, pattern = "secondary") ~ 40L,
                           str_detect(string = highway, pattern = "tertiary") ~ 30L,
                           TRUE ~ 20L)) %>% 
  as_sfnetwork(x = shp_major_road, directed = FALSE) %>% 
  # Subdivude edges
  convert(.f = to_spatial_subdivision, .clean = TRUE) %>% 
  # Sum up weights for combined
  activate(what = "edges") %>% 
  # Calculate time to cross edge
  mutate(edge_dist = edge_length() %>% as.numeric(),
         # Calculate time
         edge_time = edge_dist/((5/18) * speed)
         # edge_time = edge_dist
  ) %>% 
  activate(what = "nodes") %>% 
  # Smooth network
  convert(.f = to_spatial_smooth, store_original_data = TRUE, .clean = TRUE) %>% 
  # Back to edges
  activate(what = "edges") %>% 
  # Replace attributes with old ones
  mutate(edge_time_alt = map_dbl(.x = .orig_data, 
                                 .f = function(x){
                                   x %>% 
                                     # Covnert to tibble
                                     as_tibble() %>% 
                                     # Select weight column
                                     select(edge_time) %>% 
                                     sum()
                                 })) %>% 
  # If original is blank replace with alt
  mutate(edge_time = case_when(is.na(edge_time) ~ edge_time_alt,
                               TRUE ~ edge_time)) %>% 
  # Remove extra columns
  select(-c(.orig_data, edge_time_alt)) %>% 
  # remove multiple edges and loops
  filter(!(is.na(edge_is_loop()) | is.na(edge_is_multiple())))

