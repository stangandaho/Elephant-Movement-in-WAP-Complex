library(geodata)

for (c in c("Benin", "Niger")) {
  geodata::worldclim_country(country = c,
                             var = "bio",
                             res = .5,
                             path = "datasets",
                             version = "2.1")
}
rm(c)

# Mosaic data
ben_clim <- terra::rast("datasets/climate/wc2.1_country/BEN_wc2.1_30s_bio.tif")
nig_clim <- terra::rast("datasets/climate/wc2.1_country/NER_wc2.1_30s_bio.tif")

clim_data <- terra::mosaic(ben_clim, nig_clim)

# Crop to area of interest
wap_buffer <- sf::st_read("datasets/wa_pa/wa_protected_area.shp") %>%
  filter(NAME %in% c("Pendjari", "Boucle de la Pendjari",
                     "W (Benin)", "Djona", "Parc National du W du Niger"))%>%
  sf::st_buffer(dist = 5000)

#
clim_data_crop <- clim_data %>%
  terra::crop(y = wap_buffer, mask = TRUE, snap = "out")
terra::plot(clim_data_crop)


# save
terra::writeRaster(clim_data_crop, "datasets/climate/clim_data.tif")

# remove objected created in this script
rm(ben_clim, nig_clim, clim_data, clim_data_crop, wap_buffer)
rm(ben_clim, nig_clim, clim_data, clim_data_crop, wap_buffer)
