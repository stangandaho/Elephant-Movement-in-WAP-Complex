# load setup
source("scripts/setup.R")
source("scripts/utils.R")

#import wap
wap <- sf::st_read("datasets/wa_pa/wa_protected_area.shp", quiet = TRUE) %>%
  dplyr::filter(NAME %in% c("Pendjari", "Boucle de la Pendjari",
                            "W (Benin)", "Djona", "Parc National du W du Niger")) %>%
  slice(-2) %>%
  dplyr::mutate(zone = c("Pendjari National Park", "Djona Hunting Zone",
                         "Penjari Hunting Zone", "W Park Benin"),
                park = c("Pendjari", "W", "Pendjari", "W")) %>%
  dplyr::select(zone, park)


#sf::write_sf(wap, "datasets/wa_pa/wap.shp")

# import sheets
dpath <- "datasets/Données_Totale_des_Coliiers_du_WAP_20182024 .xlsx"
all_sheets <- excel_sheets(dpath)

db_colliers_elephants <- read_excel(dpath, sheet = all_sheets[1]) %>%
  dplyr::rename(id = `ER ID`,
                season = `Période 1 (Saison sèche ou saison des pluies)`) %>%
  mutate(season = case_when(season == "Sèche" ~ "Dry", TRUE ~ "Wet")) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = "EPSG:32631") %>%
  dplyr::bind_cols(st_coordinates(.)) %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(Longitude = X, Latitude = Y) %>%
  dplyr::mutate(obs = paste0(Longitude, Latitude)) %>%
  dplyr::distinct(obs, .keep_all = TRUE) %>%
  dplyr::select(-obs)

db_colliers_elephants %>% 
  count(id, Sex)

paste(unique(db_colliers_elephants %>%dplyr::filter(Sex == "Femelle") %>% pull(id))
  , collapse = ", ")
# Select Male "Male"
total_male <- length(unique(db_colliers_elephants %>% dplyr::filter(Sex == "Male") %>% pull(id)))
male_oi <- c("6FEF", "C100", "M-11", "M-15", "M-19", "M-5", "M-6")
ele_p_male <- db_colliers_elephants %>%
  dplyr::filter(Sex == "Male",
                Latitude > 10.5,
                id %in% male_oi)

# 7 of 14 male selected
# Select Season ("Sèche" or "Pluvieux")
ele_p_male_dry <- ele_p_male %>%
  dplyr::filter(season == "Dry") %>%
  dplyr::select(id, Longitude, Latitude, `Aire Protégée`)
ele_p_male_wet <- ele_p_male %>%
  dplyr::filter(season == "Wet") %>%
  dplyr::select(id, Longitude, Latitude, `Aire Protégée`)

## Home range estimation
# Prepare list data
male_data <- list(
   elephant_male_dry_pendjari = ele_p_male_dry %>% dplyr::filter(`Aire Protégée` == "Pendjari"),
  elephant_male_dry_w = ele_p_male_dry %>% dplyr::filter(`Aire Protégée` == "PNW"),
  elephant_male_wet_pendjari = ele_p_male_wet %>% dplyr::filter(`Aire Protégée` == "Pendjari"),
  elephant_male_wet_w = ele_p_male_wet %>% dplyr::filter(`Aire Protégée` == "PNW")
)

home_range(data = male_data[[1]] %>% dplyr::filter(id == "6FEF"),
           species_name = md, percent = c(50, 70, 90, 95, 100))

for (md in names(male_data)) {
  message(paste0("On ", md))
  home_range(data = male_data[[md]], species_name = md, percent = c(50, 70, 90, 95, 100))
}

#
# Select Female
total_female <- length(unique(db_colliers_elephants %>% dplyr::filter(Sex == "Femelle") %>% pull(id)))
female_oi <- c("9F98", "AFD9", "D91E", "F-1", "F-10", "F-12", "F-13",
                         "F-14", "F-16", "F-18", "F-2", "F-20", "F-3", "F-6", "F-8")
ele_p_female <- db_colliers_elephants %>%
  dplyr::filter(Sex == "Femelle",
                Latitude > 10.5,
                id %in% female_oi
                )
# 15 of 26 female selected

# Select Season ("Sèche" or "Pluvieux")
ele_p_female_dry <- ele_p_female %>%
  dplyr::filter(season == "Dry") %>%
  dplyr::select(id, Longitude, Latitude, `Aire Protégée`)
ele_p_female_wet <- ele_p_female %>%
  dplyr::filter(season == "Wet") %>%
  dplyr::select(id, Longitude, Latitude, `Aire Protégée`)

## Estimation of Kernel Home-Range ##
# Utilization Distribution (UD) method
female_data <- list(
  elephant_female_dry_pendjari = ele_p_female_dry %>% dplyr::filter(`Aire Protégée` == "Pendjari"),
  elephant_female_dry_w = ele_p_female_dry %>% dplyr::filter(`Aire Protégée` == "PNW"),
  elephant_female_wet_pendjari = ele_p_female_wet %>% dplyr::filter(`Aire Protégée` == "Pendjari"),
  elephant_female_wet_w = ele_p_female_wet %>% dplyr::filter(`Aire Protégée` == "PNW")
)

for (md in names(female_data)) {
  message(paste0("On ", md))
  home_range(data = female_data[[md]], species_name = md, percent = c(50, 70, 90, 95, 100))
}


## Data summary
df <- db_colliers_elephants %>%
  mutate(TimeStamp = dmy_hm(`Time Stamp`)) # adjust if format differs

id_used <- data.frame(id = c(female_oi, male_oi)) %>% 
  mutate(Used = "Yes")
# --- Create summary table by collar/elephant ID ---
summary_table <- df %>%
  group_by(id) %>%
  summarise(
    Species = first(Species),
    Sex = first(Sex),
    `Collar Number` = first(id),
    `Date of First Fix Used` = min(TimeStamp, na.rm = TRUE),
    `Date of Last Fix Used` = max(TimeStamp, na.rm = TRUE),
    `Number of GPS Fixes Used` = n(),
    `Fix Interval (Hours)` = 3,
    # `Fix Interval (Hours)` = round(
    #   median(diff(sort(TimeStamp)),  units = "hours")
    # ),
    `Protected Area` = first(`Aire Protégée`),
    `GPS Datum Used` = "WGS 84",
    `Name of Elephant` = "",           # manual entry
    `Social Status` = "",              # manual entry
    `Age of Elephant` = ""             # manual entry
  ) %>%
  mutate(`Fix Interval (Hours)` = as.numeric(`Fix Interval (Hours)`)) %>% 
  left_join(y = id_used, by = "id") %>% 
  mutate(Used = case_when(is.na(Used) ~ "No", TRUE ~ Used),
  Sex = case_when(Sex == "Femelle" ~ "Female", TRUE ~ Sex)) %>% 
  arrange(Sex, Used, .by_group = TRUE) %>% 
  relocate(Used, .after = `Collar Number`) %>% 
  relocate(`Collar Number`, .before = 1) %>%
  select(-c(id, Species))

write.csv(summary_table %>% filter(Used == "Yes"), 
"tables/data_summary_yes.csv", row.names = FALSE)
write.csv(summary_table, 
"tables/collaring_data_summary.csv", row.names = FALSE)
# --- View summary table ---

