# Implement Heterogeneous CVRP for office in Bengaluru

# Load libraries -------------------------------------
library(tidyverse)
library(sf)

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
# # Get main roads
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
  terra::mask(mask = sf_city_limits, inverse = FALSE, updatevalue = NA) %>% 
  # The high population area is incorrect, change values
  tidyterra::mutate(popn = case_when(popn >= 270 ~ 100, TRUE ~ popn))
# Write to disk
# terra::writeRaster(x = rs_popn, filename = "raster_city_popn.tif", overwrite = TRUE)

ggplot() +
  tidyterra::geom_spatraster(data = rs_built) + 
  geom_sf(data = sf_city_limits, colour = "white", linewidth = 2, fill = NA) +
  scale_fill_viridis_b(breaks = c(1000, 2000, 3000, 4000, 5000, 6000))
