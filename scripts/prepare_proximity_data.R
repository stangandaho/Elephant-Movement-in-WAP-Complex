source("scripts/setup.R")

# Import WAP shapefile
wap <- sf::st_read("datasets/wa_pa/wa_protected_area.shp") %>%
  dplyr::filter(NAME %in% c("Pendjari", "Boucle de la Pendjari",
                     "W (Benin)", "Djona", "Parc National du W du Niger"))

# Import water
water1 <- sf::read_sf("datasets/water/Cours d'eau permanent_WAP Benin.shp") %>%
  dplyr::select(geometry) %>%
  sf::st_combine()

water2 <- sf::read_sf("datasets/water/Cours d'eau secondaire_WAP Benin.shp") %>%
  dplyr::select(geometry) %>%
  sf::st_zm(drop = TRUE, what = "ZM") %>%
  sf::st_combine()

water_point <- sf::read_sf("datasets/water/Point d'eau permanent_WAP Benin.shp") %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(pr = 1) %>%
  sf::st_zm(drop = TRUE, what = "ZM")

## Import road data
roads <- sf::read_sf("datasets/shape/roads.shp") %>%
  dplyr::filter(type %in% c("primary", "secondary", "tertiary")) %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(pr = 1)

## Import crisis data
influencing_events <- c(
  "Armed clash",
  "Remote explosive/landmine/IED",
  "Looting/property destruction",
  "Mob violence",
  "Sexual violence",
  "Attack",
  "Disrupted weapons use",
  "Headquarters or base established"
)

crisis <- read.csv(file = "datasets/1900-01-01-2025-01-20-Western_Africa-Benin.csv") %>%
  dplyr::filter(year %in% c(2019:2024),
                sub_event_type %in% influencing_events) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_intersection(y = wap %>% sf::st_buffer(dist = 5000)) %>%
  dplyr::select(sub_event_type) %>%
  mutate(pr = 1)


#1. Rasterize water
ref_raster <- terra::rast("datasets/climate/clim_data.tif") # reference raster

water_raster <- terra::rasterize(x = water %>% sf::st_transform(crs = sf::st_crs(ref_raster)),
                                 y = ref_raster,
                                 field = "pr")

# Compute proximity for water
water_proximity <- terra::distance(x = water_raster,
                                   unit = "km")
terra::writeRaster(water_proximity, "datasets/proximity/water_proximity.tif")


#2. Rasterize road
road_raster <- terra::rasterize(x = roads %>% sf::st_transform(crs = sf::st_crs(ref_raster)),
                                 y = ref_raster,
                                 field = "pr")

# Compute proximity for road
road_proximity <- terra::distance(x = road_raster,
                                   unit = "km")
terra::writeRaster(road_proximity, "datasets/proximity/road_proximity.tif")


#3. Rasterize water point
wp_raster <- terra::rasterize(x = water_point %>% sf::st_transform(crs = sf::st_crs(ref_raster)),
                                y = ref_raster,
                                field = "pr")

# Compute proximity for water point
wp_proximity <- terra::distance(x = wp_raster,
                                  unit = "km")
terra::writeRaster(wp_proximity, "datasets/proximity/water_point_proximity.tif")

#4. Rasterize crisis data
cris_raster <- terra::rasterize(x = crisis %>% sf::st_transform(crs = sf::st_crs(ref_raster)),
                              y = ref_raster,
                              field = "pr")

# Compute proximity for water point
cris_proximity <- terra::distance(x = cris_raster,
                                unit = "km")
terra::writeRaster(cris_proximity, "datasets/proximity/crisis_proximity.tif")


# remove objected created in this script
rm(water1, water2, water_point, roads, ref_raster, water_raster, water_proximity, road_raster, road_proximity)
