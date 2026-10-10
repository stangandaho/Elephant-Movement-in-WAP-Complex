# Load library
source("scripts/setup.R")
source("scripts/utils.R")

# Home range file
hr_files <- list.files(path = "home_range/", pattern = ".shp$", full.names = T)

# dry season
dry_files <- hr_files[grepl("dry", hr_files)]
dry_hr <- lapply(dry_files, function(x){
  sx <- ifelse(grepl("female", x), "female", "male")
  hr <- sf::st_read(x, quiet = TRUE) %>% st_as_sf() %>%
    dplyr::filter(percent == 95) %>%
    dplyr::mutate(sex = sx, id = paste0(toupper(sex), " ", id)) %>%
    dplyr::select(id, sex)
}) %>% dplyr::bind_rows()

# Matrice to store intersection
elephant_overlap <- matrix(ncol = length(dry_hr$id), nrow = length(dry_hr$id))
colnames(elephant_overlap) <- rownames(elephant_overlap) <- rev(sort(dry_hr$id))
for (i in dry_hr$id) {
  for (j in dry_hr$id) {
    if (i != j) {
      x <- dry_hr %>% dplyr::filter(id == i)
      y <- dry_hr %>% dplyr::filter(id == j)
      x_y <- st_intersection(x = x, y = y) %>%
        st_as_sf() %>% st_area()
      elephant_overlap[i, j] <- ifelse(length(x_y) == 0,
                                       0, round(as.numeric(x_y)/1e6, 1))
    }
  }
}

ct::ct_plot_overlap_coef(data = elephant_overlap, side = "lower",
                             show = "shape",
                             color_scale = c("#F2BC8D", "#BF9169", "#8C6849", "#403425"),
                             text_size = 5, excludes = 0, shape_size = 1.5)+
  labs(color = "Home range overlap (km²) - Wet season")+
  theme_minimal(base_family = "msb")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
    axis.text = element_text(size = 14, color = "gray10"),
    axis.title = element_blank(),
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(size = 15, hjust = .5),
    legend.text = element_text(size = 13),
    legend.key.height = unit(.6, units = "lines"),
    legend.key.width = unit(4, units = "lines")
  )

filename <- "plots/Figure_4_2.jpeg"#"plots/elephant_displacement_distance.jpeg"
ggsave(filename = filename, width = 25, height = 25, units = "cm")

# Stats of overlap
overlap_stat <- elephant_overlap %>%
  as.data.frame() %>%
  tibble::rownames_to_column('left') %>%
  tidyr::pivot_longer(cols = !left, names_to = "right", values_to = "overlap")

inter_male_dry <- overlap_stat %>%
  dplyr::filter(grepl("^MALE", left) & grepl("^MALE", right)) %>%
  dplyr::filter(left != right) %>%
  ct::ct_describe_df(overlap, fn = list('sd_error')) %>%
  rename(Between = Variable) %>%
  mutate(Between = "Males")

inter_female_dry <- overlap_stat %>%
  dplyr::filter(grepl("^FEMALE", left) & grepl("^FEMALE", right)) %>%
  dplyr::filter(left != right) %>%
  ct::ct_describe_df(overlap, fn = list('sd_error')) %>%
  rename(Between = Variable) %>%
  mutate(Between = "Females")

intra_dry <- overlap_stat %>%
  dplyr::filter(left != right & !is.na(overlap)) %>%
  ct::ct_describe_df(overlap, fn = list('sd_error')) %>%
  rename(Between = Variable) %>%
  mutate(Between = "Males and Females")


overlap_tbl <- dplyr::bind_rows(inter_male, inter_female, intra) %>%
  mutate(Season = "Wet") %>%
  bind_rows(
    bind_rows(inter_male_dry, inter_female_dry, intra_dry) %>%
      mutate(Season = "Dry")
  )

write.csv(x = overlap_tbl, file = "tables/overlap.csv", row.names = FALSE)


#########

# Stats of overlap
overlap_stat <- elephant_overlap %>%
  as.data.frame() %>%
  tibble::rownames_to_column('left') %>%
  tidyr::pivot_longer(cols = !left, names_to = "right", values_to = "overlap")

inter_male_dry <- overlap_stat %>%
  dplyr::filter(grepl("^MALE", left) & grepl("^MALE", right)) %>%
  dplyr::filter(left != right)

sum(inter_male_dry$overlap == 0)/length(inter_male_dry$overlap)

inter_female_dry <- overlap_stat %>%
  dplyr::filter(grepl("^FEMALE", left) & grepl("^FEMALE", right)) %>%
  dplyr::filter(left != right)

sum(inter_female_dry$overlap == 0)/length(inter_female_dry$overlap)

intra_dry <- overlap_stat %>%
  dplyr::filter(left != right & !is.na(overlap))

sum(intra_dry$overlap == 0)/length(intra_dry$overlap)


