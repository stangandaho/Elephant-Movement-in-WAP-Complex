library(sf)

wap <- sf::read_sf("E:/Manuscripts/elephant_apn/datasets/WAP/wap2.shp")
wap_ben <- sf::st_read("C:/Users/ganda/Downloads/dd/WAPER/wap_benin.shp")

sum(sf::st_area(wap))/1e6
sum(sf::st_area(wap_ben))/1e6

(sum(sf::st_area(wap_ben))/1e6)*100/(sum(sf::st_area(wap))/1e6)

st_centroid(wap %>% st_union())

st_bbox(wap %>% st_transform(crs = 4326))

1714831/1e4

library(dplyr)
library(tidyr)
library(ggplot2)

rainfall <- read.csv("E:\\Manuscripts\\elephant_apn\\datasets\\temperature_and_rainfall.csv") %>% 
  mutate(across(.cols = January:December, .fns = as.numeric)) %>% 
  pivot_longer(cols = January:December, values_to = "value", names_to = "months") %>% 
  mutate(month_year = paste(months, Year), month_year = lubridate::my(month_year)) %>% 
  select(-c(Year, months))

source("scripts/setup.R")
ggplot(data = rainfall)+
  geom_line(mapping = aes(x = month_year, y = value, group = Metric))+
  facet_grid(facet = Metric ~ Park, scales = "free_y")+
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b-%Y",
    limits = as.Date(c("2023-01-01", "2025-10-31"))
  ) +
  theme_minimal(base_family = "mr")+
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.background = element_rect(color = "gray90", fill = "gray90"),
    strip.text = element_text(family = "mb", size = 12)
  )

ggsave(filename = "plots/Figure_1.jpeg", width = 22, height = 16,
unit = "cm")


library(dplyr)
library(lubridate)

# --- Load your dataset ---

# --- Convert timestamps to proper datetime format ---
