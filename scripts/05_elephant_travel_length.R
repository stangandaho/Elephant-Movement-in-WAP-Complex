## Analysis of elephant displacement against sex and season (dry and wet)
source("scripts/setup.R")
source("scripts/utils.R")

## Import data
db_colliers_elephants <- read_excel(dpath, sheet = all_sheets[1]) %>%
  dplyr::rename(id = `ER ID`,
                season = `Période 1 (Saison sèche ou saison des pluies)`) %>%
  mutate(season = case_when(season == "Sèche" ~ "Dry", TRUE ~ "Wet")) %>%
  ## Filter data
  dplyr::filter(!is.na(Longitude), !is.na(Latitude),
                Sex %in% c("Male", "Femelle"), Latitude > 10.5,
                id %in% elephant_id,
                ) %>%
  ## Remove duplicated coordinate
  dplyr::mutate(obs = paste0(Longitude, Latitude)) %>%
  dplyr::distinct(obs, .keep_all = TRUE) %>%
  dplyr::select(-obs) %>%
  ## Coerce to sf object
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = "EPSG:32631")
  ## 733938


## Separate column
ele_distance <- db_colliers_elephants %>%
  separate_wider(cols = `Time Stamp`, delim = "\\s", new_cols = c("date", "time")) %>%
  separate_wider(cols = "date", delim = "/", new_cols = c("day", "month", "year")) %>%
  dplyr::select(c(Sex, id, month, year, season, geometry)) %>%
  ## Calculate the distance
  group_by(Sex, id, month, year, season) %>%
  summarise(
    geometry = st_combine(geometry),  # Combine points into a single geometry
    .groups = "drop"                  # Drop grouping metadata
  ) %>%
  mutate(geometry = st_cast(geometry, to = "LINESTRING"),
         path_length = round(as.numeric(st_length(geometry))/1000, 3),
  ) %>%
  st_drop_geometry() %>%
  mutate(mon_year = paste(month, year, sep = "-")) %>%
  arrange(year, month)
  ## Add color column

## Apply Kruskall Wallis test for difference of elephant displacement
test_list <- list()
for (sex in c("Male", "Femelle")) {
  #for (season in c("Male", "Femelle")) {
    sub_data <- ele_distance %>%
      dplyr::filter(Sex == sex)

    # Apply Kruskall walliss test
    distance_dry <- round(mean(sub_data %>% subset(season == "Dry") %>% pull(path_length)), 2)
    distance_wet <- round(mean(sub_data %>% subset(season == "Wet") %>% pull(path_length)), 2)

    model <- tibble(
      distance_dry = distance_dry,
      distance_wet = distance_wet
    ) %>%
      bind_cols(rstatix::wilcox_test(data = sub_data, formula = path_length ~ season) #%>%dplyr::select(-c(`.y.`, df, method))
                  ) %>%
      dplyr::mutate(sex = sex, statistic = round(statistic, 3))

    test_list[[sex]] <- model
  #}

}

test_tbl <- bind_rows(test_list)


## Plot displacement trend for two elephant individual for male and female.
# Convert Mon-Year to a factor to maintain order
ele_distance$mon_year <- factor(ele_distance$mon_year, levels = unique(ele_distance$mon_year))
ele_distance$season <- factor(ele_distance$season, levels = unique(ele_distance$season))

# Create the ggplot visualization with proper grouping and color handling
data_for_trend <- ele_distance %>%
  dplyr::filter(id %in% c("6FEF", "M-15", "9F98", "D91E")) %>%
  dplyr::mutate(Sex = case_when(Sex == "Femelle" ~ "Female", TRUE ~ Sex),
                group = paste(Sex, id), mon_year = lubridate::my(mon_year))

ggplot(data_for_trend,
       aes(x = mon_year, y = path_length, group = 1)) +
  geom_line() +
  geom_point(aes(color = season)) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b-%Y"
  ) +
  scale_color_manual(values = c(clr_set2[1], clr_set2[4]))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  facet_wrap(~ group, scales = "free") +
  labs(#title = "Distance Traveled by Elephant",
       x = "Date",
       y = "Distance (km)",
       color = "Season:")+
  #theme()+
  theme(legend.position = "top",
        legend.title.position = "left",
        legend.title = element_text(family = "msb", size = 13, hjust = .5),
        legend.text = element_text(size = 13, family = "mr"),
        legend.key.height = unit(1.5, units = "lines"),
        legend.key.width = unit(.5, units = "lines"),

        strip.background = element_rect(fill = clr_set2[1], colour = clr_set2[1]),
        strip.text = element_text(color = "white", family = "msb",
                                  size = 14),
        strip.text.x = element_text(margin = margin(t = .4, b = .4, unit = "lines")),
        strip.text.y = element_text(margin = margin(l = .4, r = .4, unit = "lines"),
                                    angle = 90),

        panel.background = element_rect(color = clr_set1[1], , fill = "white"),

        axis.text = element_text(family = "mr", size = 11),
        axis.title.x = element_text(margin = margin(t = .5, b = .5, unit = "lines")),
        axis.title.y = element_text(margin = margin(r = .5, l = .1, unit = "lines"), angle = 90),
        axis.title = element_text(family = "msb", size = 13),
  )
filename <- "plots/Figure_5.jpeg"#"plots/elephant_displacement_distance.jpeg"
ggsave(filename = filename, width = 30, height = 18, units = "cm")

#### Analysis of factor influencing elephant displacement
env_factor <- lapply(list.files("datasets/proximity/", pattern = ".tif$", full.names = TRUE),
                     function(x){terra::rast(x)})

## For each env_factor, extract corresponding value on each elephant displacement line
## then, summarize this value by min, max, and mean
extractions <- list()
for (rst in env_factor) {
  rst_name <- gsub("_proximity.tif", "", basename(terra::sources(rst)))
  inner_extr <- list()
  for (i in 1:nrow(ele_distance)) {
    message(sprintf("On raster %s and line (%s/%s)", toupper(rst_name), i, nrow(ele_distance)))

    displacement_line <- ele_distance %>%
      st_as_sf() %>%
      slice(i)

    if (displacement_line$path_length == 0) { # case LINESTRING has one coordinate
      displacement_line <- ele_distance %>%
        st_as_sf() %>%
        slice(i) %>%
        st_cast(to = "POINT")
    }

    extracted_values <- terra::extract(x = rst, y = displacement_line) %>%
      pull(2)

    out_tbl <- tibble(
      "min_{rst_name}" := min(extracted_values, na.rm = TRUE),
      "max_{rst_name}" := max(extracted_values, na.rm = TRUE),
      "mean_{rst_name}" := mean(extracted_values, na.rm = TRUE),
    )

    inner_extr[[i]] <- out_tbl
  }

  inner_extr <- bind_rows(inner_extr)

  extractions[[rst_name]] <- inner_extr
}
## Bind all output to construct final extracted value table
full_extractions <- bind_cols(extractions)

## Bind full_extractions with original to data to add supplementary information
model_data <- ele_distance %>%
  dplyr::select(Sex, id, season, path_length) %>%
  bind_cols(full_extractions)

## Write to disk
write.csv(model_data, "datasets/elephants_env_factor_value_along_displacement.csv",
          row.names = FALSE)


## Model dispalcement distence again environmental factor
model_data <- read.csv("datasets/elephants_env_factor_value_along_displacement.csv")

model_formula <- as.formula(paste0("path_length ~ ",
                                   paste0(colnames(model_data %>% select(starts_with("mean_"))),
                                          collapse = " + "))
                            )

displ_model_male <- summary(
  lm(formula = model_formula,
     data = model_data %>%
       dplyr::filter(Sex == "Male") %>%
       dplyr::select(- c(Sex, id, season))
     )
)$coefficients %>%
  as.data.frame() %>%
  slice(-1) %>%
  dplyr::mutate(Predictor = tools::toTitleCase(gsub("_", " ", rownames(.))),
                Sex = "Male") %>%
  relocate(Predictor, .before = 1)


displ_model_female <- summary(
  lm(formula = model_formula,
     data = model_data %>%
       dplyr::filter(Sex == "Femelle") %>%
       dplyr::select(- c(Sex, id, season))
     )
)$coefficients %>%
  as.data.frame() %>%
  slice(-1) %>%
  dplyr::mutate(Predictor = tools::toTitleCase(gsub("_", " ", rownames(.))),
                Sex = "Female") %>%
  relocate(Predictor, .before = 1)

displ_model <- bind_rows(displ_model_male, displ_model_female) %>%
  rowwise() %>%
  mutate(psign = pval_sig(`Pr(>|t|)`),
         `Pr(>|t|)` = round(`Pr(>|t|)`, 3),
         Predictor = gsub("Mean ", "", Predictor))

## Viz
write.csv(displ_model, "tables/elephants_lm_table.csv",
          row.names = FALSE)

