# load library
source("scripts/setup.R")
source("scripts/utils.R")

##
elephant_hr <- list.files(path = "home_range",
                          pattern = "^elephant.*\\.shp$",
                          full.names = TRUE)

elephant_hr_sf <- lapply(elephant_hr,
                         function(x){sf::read_sf(x) %>%
                             dplyr::mutate(species = gsub(".shp", "", basename(x)))
                         }) %>%
  dplyr::bind_rows() %>%
  tidyr::separate_wider_delim(data = ., cols = species, delim = "_",
                              names = c("species", "sex", "season", "park")) %>%
  select(-c(park, species)) %>%
  dplyr::filter(percent == 90) %>%
  st_as_sf()

##
wap <- sf::read_sf("datasets/wa_pa/wap.shp") %>%
  summarise(park = st_union(geometry), .by = "park")

for (season in c("dry", "wet")) {
  for (sex in c("male", "female")) {
    hr_sf <- elephant_hr_sf %>%
      dplyr::filter(season == !!season, sex == !!sex) %>%
      st_transform(crs = st_crs(wap)) %>%
      dplyr::mutate(alpha = seq(.1, 1, length.out = nrow(.)))

    ncol <- 5; nrow <- 5; width <- 25; height = 16
    if (sex == "male") {
      ncol <- 4; nrow <- 2; width <- 18; height <- 10
    }

    ggplot()+
      geom_sf(data = wap, color = "#D9CCC5", fill = "#D9CCC5")+
      geom_sf(data = hr_sf,
              mapping = aes(geometry = geometry), color = "#2E4159", fill = "#2E4159")+
      facet_wrap(facets = ~ id, ncol = ncol, nrow = nrow)+
      theme_void()+
      theme(
        strip.background = element_rect(fill = clr_set2[1], colour = clr_set2[1]),
        strip.text = element_text(color = "white", family = "msb",
                                  size = 16),
        strip.text.x = element_text(margin = margin(t = .4, b = .4, unit = "lines")),
        strip.text.y = element_text(margin = margin(l = .4, r = .4, unit = "lines"),
                                    angle = 90),
      )

    file_name <- paste0("plots/elephant_hr_", sex, "_", season, ".jpeg")
      ggsave(filename = file_name,
             width = width, height = height, units = "cm")
  }
}
rm(season, sex)
