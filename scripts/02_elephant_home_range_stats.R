source("scripts/setup.R")
source("scripts/utils.R")

## Apply statistical test to evaluate if home range size change significantly
## within season per sex.
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
  select(-c(park, species))

elephant_hr_sf %>%
  dplyr::filter(percent != 100) %>%
  summarise(area = mean(area), n = n(), .by = c("sex", "season", "percent"))

# Loop through 50%, 70%, 90% and 95% isopleths and appply kruskal test
# To check difference of area (km square) between season

## This apply for female and male
test_list <- list()

for (sex in c("male", "female")) {
  for (prt in c(50, 70, 90, 95)) {
    sub_data <- elephant_hr_sf %>%
      dplyr::filter(sex == !!sex & percent == prt)

    # Apply Kruskall walliss test
    area_dry <- round(mean(sub_data %>% subset(season == "dry") %>% pull(area)), 2)
    area_wet <- round(mean(sub_data %>% subset(season == "wet") %>% pull(area)), 2)

    model <- tibble(
      area_dry = area_dry,
      area_wet = area_wet
    ) %>%
      bind_cols(rstatix::wilcox_test(data = sub_data, formula = area ~ season, paired = TRUE)) %>%
      dplyr::mutate(isopleths = prt, sex = sex, statistic = round(statistic, 3))

    test_list[[paste0(sex, prt)]] <- model
  }

}

test_tbl <- bind_rows(test_list)

######### Plot

elephant_summary <- elephant_hr_sf %>%
  filter(percent != 100) %>%
  mutate(percent = as.character(percent)) %>%
  group_by(percent, sex, season) %>%
  summarize(
    mean_area = mean(area),
    ste = sd(area)/sqrt(length(area)),
    ymin = mean_area - ste,
    ymax = mean_area + ste,
    .groups = "drop"
  )
elephant_summary[, 1:4]

ggplot(data = elephant_summary, aes(x = percent, y = mean_area)) +
  geom_col(color = "gray", fill = "gray30") +
  #geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_label(mapping = aes(x = percent, y = mean_area - mean_area*0.05,
                          label = paste0(round(mean_area, 1), "\n±\n", round(ste, 1))),
            family = "msb", size = 8, color = "gray30", lineheight = 0.7)+
  facet_grid(sex ~ season) +
  labs(x = "Isopleths (%)", y = "Mean home range (km²)")+
  theme_minimal()+
  hr_viz_theme(panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               axis.text.y = element_blank())


ggsave(filename = "plots/Figure_3.jpeg",
       width = 16, height = 14)


wt <- wilcox.test(c(1, 2, 1.3, 2.6, 1.4, 5), conf.int = TRUE, conf.level = 0.95)
wt

species <- redlist::rl_scientific_name("Estrilda", "poliopareia")
