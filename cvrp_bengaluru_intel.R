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
#   `$`(osm_multipolygons)
# write_rds(x = sf_city_limits, file = "sf_city_limits.rds")
sf_city_limits <- read_rds(file = "sf_city_limits.rds")

# Create bounding box for city limits ----------------------
sf_bbox <- st_bbox(sf_city_limits) %>% 
  # Convert to sf
  st_as_sfc(crs = st_crs(sf_city_limits))

# Fetch road network of city --------------------------
# Get main roads
feat_major_road <- opq(bbox = sf_bbox, timeout = 100) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "motorway_link",
                            "trunk", "trunk_link",
                            "primary", "primary_link",
                            "secondary", "secondary_link",
                            "tertiary", "tertiary_link")) %>%
  osmdata_sf()
# Save
# write_rds(x = feat_major_road, file = "kalyani_road.rds")
feat_major_road <- read_rds(file = "kalyani_road.rds")

# Create sf object combining lines and polygons ----------------------------
sf_city_road <- bind_rows(feat_major_road$osm_lines %>% 
                            select(osm_id, highway),
                          feat_major_road$osm_polygons %>% 
                            select(osm_id, highway) %>% 
                            st_cast(to = "LINESTRING")) %>% 
  # Truncate roads to stay within bounding box
  st_intersection(y = sf_city_limits)

# Load Population raster ------------------------------
rs_popn <- terra::rast(x = "GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R7_C27.tif") %>% 
  # Remove layers
  tidyterra::rename(popn = GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R7_C27) %>% 
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
