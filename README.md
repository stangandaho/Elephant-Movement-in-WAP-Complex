# Seasonal and environmental determinants of African elephant (*Loxodonta africana*) movement in the W-Arly-Pendjari complex

This repository contains the R code and outputs supporting the article of [Akpona et al. (2025)](https://doi.org/10.1016/j.jnc.2026.127225).


## Overview

The analysis uses GPS collar data collected between 2018 and 2024 on 22 African elephants (7 males and 15 females) in Pendjari National Park and W National Park, Benin. Fixes were projected to UTM zone 31N (EPSG:32631), duplicated positions were removed, and only locations north of 10.5° latitude were retained. Each individual was assigned to the dry or wet season using the season field recorded in the collar database.

Home ranges were estimated per individual, sex, season and park with kernel utilization distributions (`adehabitatHR::kernelUD`, LSCV bandwidth, 1000 cell grid) and extracted at the 50, 70, 90, 95 and 100% isopleths. Seasonal differences in home range size were tested within each sex with paired Wilcoxon tests. Spatial overlap between individuals was computed from the 95% isopleths. Monthly travel distances were derived by joining successive fixes into tracks and compared between seasons with Wilcoxon tests. Environmental covariates include WorldClim 2.1 bioclimatic variables and distance rasters to permanent rivers, water points, main roads and armed conflict events recorded around the parks.

## Repository structure

- `scripts/` folder holds the full workflow. `setup.R` installs and loads the required packages, defines colours, fonts and the list of elephants retained for analysis, and `utils.R` contains helper functions, including the `home_range()` wrapper used for kernel estimation. The numbered scripts are meant to be run in order: `01_elephant_home_range.R` prepares the collar data and writes the home range shapefiles, `02_elephant_home_range_stats.R` tests seasonal differences in home range size (Figure 3), `03_elephant_home_range_viz.R` maps individual home ranges, `04_home_range_intersection.R` computes pairwise overlap (Figure 4 and `tables/overlap.csv`), `05_elephant_travel_length.R` analyses monthly travel distances (Figure 5), and `06_study_area.R` summarises the study area and climate (Figure 1). `worldclim.R` downloads and crops the WorldClim layers, and `prepare_proximity_data.R` builds the distance rasters for water, roads and conflict events.

- `home_range/` folder stores the home range polygons produced by script 01, one shapefile per sex, season and park. 

- `tables/` folder contains the summary tables reported in the article

- `plots/` contains the figures and the Supporting Information.

- `fonts/` folder provides the Montserrat font used in all figures.

## Data availability

The `datasets/` and `clim/` folders are excluded from this repository. They contain the raw GPS collar data, which belong to the park management and cannot be redistributed, together with large spatial layers (protected area boundaries, hydrography, roads, conflict events and climate rasters). Requests for the collar data should be addressed to the authors. Public layers can be regenerated with `worldclim.R` and `prepare_proximity_data.R` once the source files are placed in `datasets/`.

## Reproducing the analysis

Open `Elephant Movment.Rproj` in RStudio so that all relative paths resolve from the project root, then run the scripts in numerical order. Each script sources `setup.R`, which installs missing packages through `pak`, including `ct` from GitHub (`stangandaho/ct`). Before running, edit the `terra` temporary directory set at the top of `setup.R` and the absolute paths in `06_study_area.R` to match your machine.

## Citation

Akpona, H. A., Gandaho, S. M., Gbemavo, C. D. S. J., Nobime, G., Kassa, B., & Djagoun, C. A. M. S. (2026). Seasonal and environmental determinants of African elephant (*Loxodonta africana*) movement in the W-Arly-Pendjari complex. *Journal for Nature Conservation*, 91, 127225. https://doi.org/10.1016/j.jnc.2026.127225

