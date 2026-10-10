## Terra setup
terra::terraOptions(tempdir = "E:\\APN Radio Tracking Analysis\\temp")

# Install packages
pkg_to_install <- c("pak",
                    "readxl",
                    "dplyr",
                    "ggplot2",
                    "adehabitatHR",
                    "sp",
                    "geodata",
                    "terra",
                    "rstatix",
                    "tidyr",
                    "ct",
                    "showtext")

for (pkg in pkg_to_install) {
  if(!pkg %in% rownames(installed.packages())){
    if (pkg == "ct") {
      pak::pkg_install("stangandaho/ct")
    }else{
      pak::pkg_install(pkg)
    }
  }
}

suppressMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(adehabitatHR)
  library(sf)
  library(terra)
  library(geodata)
  library(rstatix)
  library(tidyr)
  library(ct)
  library(showtext)
})
Sys.setenv("PROJ_LIB" = system.file("proj", package = "sf")[1])

rm(pkg, pkg_to_install)

## Colors
clr_set2 <- c("#193C40", "#2E5902", "#214001", "#D96941", "#A62B1F")
clr_set1 <- c("#F4E2DE", "#D94F04", "#F29325", "#007172", "#025259", "#2E5902", "#214001")

## Font
font_add("mr", "fonts/montserrat/Montserrat-Regular.ttf")
font_add("mmi", "fonts/montserrat/Montserrat-MediumItalic.ttf")
font_add("msb", "fonts/montserrat/Montserrat-SemiBold.ttf")
font_add("msbi", "fonts/montserrat/Montserrat-SemiBoldItalic.ttf")
font_add("mb", "fonts/montserrat/Montserrat-Bold.ttf")
font_add("meb", "fonts/montserrat/Montserrat-ExtraBold.ttf")
showtext_opts(dpi = 300)
showtext_auto()

## Global variable
## Select species id
male_id <- c("6FEF", "C100", "M-11", "M-15", "M-19", "M-5", "M-6")
female_id <- c("9F98", "AFD9", "D91E", "F-1", "F-10", "F-12", "F-13",
               "F-14", "F-16", "F-18", "F-2", "F-20", "F-3", "F-6", "F-8")
elephant_id <- c(male_id, female_id)
rm(male_id, female_id)
# import sheets
dpath <- "datasets/Données_Totale_des_Coliiers_du_WAP_20182024 .xlsx"
all_sheets <- readxl::excel_sheets(dpath)
