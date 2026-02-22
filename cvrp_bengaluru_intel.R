# Implement Heterogeneous CVRP for office in Bengaluru

# Load libraries -------------------------------------
library(tidyverse)
library(sf)
library(tidygraph)
library(sfnetworks)

# Set working directory ------------------------------
setwd("C:\\Stuff\\Datasets\\GitHub\\cvrp_bengaluru_intel\\")

# Define location of office ----------------------------
sf_point_office <- st_point(x = c(77.6844, 12.926047), dim = "XY")  %>%  
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

# # Fetch road network of city --------------------------
# feat_major_road <- osmdata::opq(bbox = sf_bbox, timeout = 200) %>%
#   osmdata::add_osm_feature(key = "highway",
#                   value = c("motorway", "motorway_link",
#                             "trunk", "trunk_link",
#                             "primary", "primary_link",
#                             "secondary", "secondary_link",
#                             "tertiary", "tertiary_link")) %>%
#   osmdata::osmdata_sf()
# write_rds(x = feat_major_road, file = "feat_major_road.rds")

# # Save set of roads -----------------------------------
# feat_major_road <- read_rds(file = "feat_major_road.rds")
# sf_city_road <- bind_rows(feat_major_road$osm_lines %>%
#                             select(osm_id, highway, name, oneway),
#                           feat_major_road$osm_polygons %>%
#                             select(osm_id, highway, name, oneway) %>%
#                             st_cast(to = "LINESTRING")) %>%
#   # Remove tertiary roads
#   filter(!highway %in% c("tertiary", "tertiary_link")) %>% 
#   # Truncate roads to stay within bounding box
#   # Use st_intersects otherwise lines are split if they cross boundary twice
#   st_filter(y = sf_city_limits, .predicate = st_intersects) %>%
#   # Keep columns
#   select(osm_id, highway, name, oneway)
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

# # Load Population raster ------------------------------
# rs_built <- terra::rast(x = "GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C26.tif") %>%
#   # Remove layers
#   tidyterra::rename(built = GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100_V1_0_R8_C26) %>%
#   # Crop to city bounding box
#   terra::crop(y = terra::vect(x = sf_bbox %>%
#                                 st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"))) %>%
#   # Change projection
#   terra::project(y = "epsg:4326") %>%
#   # Mask values outside city limits
#   terra::mask(mask = sf_city_limits, inverse = FALSE, updatevalue = NA)

# PLot ---------------------
ggplot() +
  # tidyterra::geom_spatraster(data = rs_built) + 
  geom_sf(data = sf_city_limits, colour = "purple", linewidth = 1, fill = NA) +
  geom_sf(data = sf_city_road)
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
#   as_tibble_col(column_name = "stop_index") %>%
#   # Bind to original tibble
#   bind_cols(sf_bus_stops, .) %>%
#   # Group by clusters and get centroid
#   group_by(stop_index) %>%
#   summarise(geometry = st_union(x = geometry)) %>%
#   st_centroid() %>%
#   # Transform coordinates
#   st_transform(crs = 4326)

# # Convert to SF object ------------------------
# sf_built <- rs_built %>% terra::as.points() %>% st_as_sf()
# # Calculate weight of each clustered bus stop, proportional to the level of built up area ----------------------
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
#   bind_cols(sf_bus_stops_condensed, .) %>% 
#   # Remove stop with 0
#   filter(popn > 0) %>% 
#   # Normalize popn, in the range 1 to 11
#   mutate(popn = ceiling(10 * (popn + 1 - min(popn))/(max(popn) - min(popn))))
# write_rds(x = data_stop_popn, file = "data_stop_popn.rds")
data_stop_popn <- read_rds(file = "data_stop_popn.rds")
  

# Workflow -----------------------------
# Initiate population with random order of stops
# Generate sequence of vehicles for routes based on probabilities of each type,
# proportional to number of vehicles of each type
# Start from beginning of chromosome, assign stops to that vehicle type till capacity is reached
# Repeat till all stops are assigned to vehicles, which form the final routes
# Calculate fitness function based on distance covered, and vehicle running cost
# Use OVRP for above
# Iterate till limits reached

# Function to calculate clusters for any SF point object ----------------------
fx_dbscan <- function(sf_points, eps){
  # sf_points <- sfnet_road %>% st_as_sf("nodes")
  
  # Extract coordinates
  mtx_coords <- sf_points %>% 
    # Transform to mollweide coordinates to be able to set parameters in meters
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") %>% 
    # Fetch coordinate matrix
    st_coordinates()
  
  # Fetch clusters
  vec_cluster <- dbscan::dbscan(x = mtx_coords, eps = eps, minPts = 1)$cluster
  
  # Bind to object
  return(sf_points %>% mutate(cluster = vec_cluster))
}

# # Process Road Network ---------------------------------------
# t0 <- Sys.time()
# sfnet_road <- sf_city_road %>%
#   # Convert all to Multi line string, to enable later conversion to Linestring
#   st_cast(to = "MULTILINESTRING") %>% 
#   st_cast(to = "LINESTRING") %>%
#   # Round precision to 4 decimals
#   st_set_geometry(value = st_geometry(.) %>% 
#                     lapply(FUN = function(x) round(x, 4)) %>% 
#                     st_sfc(crs = st_crs(x = sf_city_road))) %>% 
#   # Set speeds
#   mutate(speed = case_when(str_detect(string = highway, pattern = "motorway|trunk") ~ 45L,
#                            str_detect(string = highway, pattern = "primary") ~ 36L,
#                            str_detect(string = highway, pattern = "secondary") ~ 27L,
#                            str_detect(string = highway, pattern = "tertiary") ~ 18L,
#                            TRUE ~ 9L)) %>% 
#   # Keep only geometries
#   select(speed) %>% 
#   # Convert to SF NETWORK
#   # Keep as non-directed to keep number of edges to minimum
#   as_sfnetwork(directed = FALSE) %>% 
#   # Subdivide edges at locations which are interior points to more than one edge
#   convert(.f = to_spatial_subdivision, .clean = TRUE) %>% 
#   # Group components
#   activate(nodes) %>% 
#   mutate(cmp = group_components()) %>% 
#   # Keep only the first component, which is the largest one
#   filter(cmp == 1) %>% 
#   select(-cmp) %>% 
#   # Sum up weights for combined
#   activate(edges) %>% 
#   # Calculate time to cross edge
#   mutate(edge_dist = edge_length() %>% as.numeric(),
#          # Calculate time
#          edge_time = edge_dist/((5/18) * speed)) %>% 
#   # Remove zero length edges
#   filter(edge_dist > 0) %>% 
#   # Keep required fields
#   select(-c(speed, edge_dist)) %>% 
#   # Contract network by replacing clustered notes with centroids
#   activate(nodes) %>% 
#   # Apply function
#   fx_dbscan(eps = 100) %>% 
#   # Find the components again, so that only points in the same network are amalgamated
#   mutate(component = group_components()) %>% 
#   # Contract network
#   convert(.f = to_spatial_contracted, cluster, component,
#           simplify = TRUE, .clean = TRUE, 
#           summarise_attributes = list(edge_time = "sum", "ignore")) %>% 
#   # Smooth network
#   activate(nodes) %>% 
#   convert(.f = to_spatial_smooth, 
#           summarise_attributes = list(edge_time = "sum"), .clean = TRUE) %>% 
#   # Simplify network (remove multiple edges and loops)
#   activate(edges) %>% 
#   # Arrange in ascending order 
#   arrange(edge_length()) %>% 
#   filter(!(is.na(edge_is_loop()) | is.na(edge_is_multiple())))
# difftime(time1 = Sys.time(), time2 = t0)
# write_rds(x = sfnet_road, file = "sfnet_road.rds")
sfnet_road <- read_rds(file = "sfnet_road.rds")

ggplot() +
  geom_sf(data = sf_city_limits, colour = NA, fill = "grey87", linewidth = 1) +
  geom_sf(data = sfnet_road %>% st_as_sf("edges"), linewidth = 1, colour = "dodgerblue") +
  # geom_sf(data = sfnet_road2 %>% st_as_sf("nodes"), size = 2, alpha = 0.6, colour = "firebrick") +
  geom_sf(data = sf_poi, size = 2, alpha = 0.6, colour = "firebrick") +
  # coord_sf(xlim = c(77.56, 77.58), ylim = c(12.96, 12.98)) +
  theme_light()

# Keep PoIs which are within specific distance of the edges ---------------------
sf_poi <- data_stop_popn %>% 
  # Find nearest edge of network to each of the bus stops
  mutate(nearest_edge = st_nearest_feature(x = geometry, y = sfnet_road %>% 
                                             st_as_sf("edges"))) %>% 
  # Fetch geometry of nearest feature
  left_join(y = sfnet_road %>% st_as_sf("edges") %>% 
              mutate(index = row_number()) %>% select(index) %>% as_tibble(), 
            by = c("nearest_edge" = "index"), suffix = c("", "_edge")) %>% 
  # Get distance
  mutate(dist_nearest_edge = st_distance(x = geometry, y = geometry_edge, 
                                         by_element = TRUE) %>% as.numeric()) %>% 
  # remove Linestring geometry
  select(-c(nearest_edge, geometry_edge)) %>% 
  # remove points more than 200m from nearest edge
  filter(dist_nearest_edge <= 200)

# Create blended network based on PoIs -----------------------------------