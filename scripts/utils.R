# separate wider
separate_wider <- function(data, cols, delim, new_cols){

  cols <- paste0(dplyr::ensym(cols))
  splited <- strsplit(data[[cols]], split = delim)

  noe <- sapply(splited, FUN = length)
  max_noe <- max(noe, na.rm = TRUE)
  length_new_cols <- length(new_cols)

  if(!all(length_new_cols == noe)){
    warning(sprintf("Warning at row %s", paste0(which(length_new_cols != noe), collapse = " ")),
            call. = FALSE)
  }

  if(max_noe < length_new_cols){
    remaning <- max(max_noe, length_new_cols) - min(max_noe, length_new_cols)
    new_cols <- new_cols[1:max_noe]
  }else if(max_noe > length_new_cols) {
    remaning <- max(max_noe, length_new_cols) - min(max_noe, length_new_cols)
    new_cols <- c(new_cols, paste("_", 1:remaning, sep = ""))
  }else{
    new_cols <- new_cols
  }

  for (i in 1:length(splited)) {
    if(length(splited[[i]]) != length(new_cols)){
      remaining <- c(length(splited[[i]]), length(new_cols))
      remaining <- max(remaining) - min(remaining)

      splited[[i]] <- c(splited[[i]], rep(NA, remaining))
    }

  }

  new_tibble <- lapply(splited, function(x){
    mtr <- as.data.frame(matrix(x, ncol = length(x), nrow = 1))
    colnames(mtr) <- new_cols
    return(mtr)
  })


  new_tibble <- dplyr::bind_rows(new_tibble)

  new_tibble <- data %>%
    dplyr::bind_cols(new_tibble) %>%
    dplyr::relocate( dplyr::all_of(new_cols), .after = cols)

  return(as_tibble(new_tibble))

}

## Home range
home_range <- function(data,
                      lon = "Longitude",
                      lat = "Latitude",
                      utm_crs = "EPSG:32631",
                      percent = 95,
                      grid = 1000,
                      save_dir = "home_range",
                      species_name = "elephant_male") {
 ### Calculate a
  data <- data %>%
    dplyr::rename(lon = lon, lat = lat)

 ## Estimation of Kernel Home-Range ##
 # Utilization Distribution (UD) method
  #xy <- sp::SpatialPoints(coords = data[, c("lon", "lat")])
 xy <- sp::SpatialPointsDataFrame(coords = data[, c("lon", "lat")], data = data)
 xy <- xy[, 1]

 home_range <- adehabitatHR::kernelUD(xy = xy,
                                     h = "LSCV",
                                     grid = grid)


 # Estimating the home range from the UD
 ## Home ranges in vector mode
 hr_list <- list()
 for (prc in percent) {

   home_range_est <- adehabitatHR::getverticeshr(x = home_range,
                                                 percent = prc, # percentage level for home-range estimation,
                                                 unin = "m",
                                                 unout = "km2")

   hr_sf <- sf::st_as_sf(home_range_est)
   st_crs(hr_sf) <- utm_crs

   hr_list[[prc]] <- hr_sf %>% dplyr::mutate(percent = prc)
 }

 if(!dir.exists(save_dir)){dir.create(save_dir, recursive = TRUE)}

 save_path <- file.path(save_dir, paste0(species_name, ".shp"))
 if(file.exists(save_path)){sf::st_delete(save_path)}

 hr_sf <- dplyr::bind_rows(hr_list)
 sf::write_sf(hr_sf, save_path)

 return(hr_sf)
}



## Reclassify raster value into discret value precising a class size (number of class)
 classify_rast <- function(rst, class_n = 4){
   rst_val <- terra::values(rst)
   rst_val_q <- quantile(rst_val[!is.na(rst_val)],
                         probs(n = class_n))

   rst[rst <= rst_val_q[[1]]] <- 1

   for (i in 1:length(rst_val_q)) {
     if (i+1 > length(rst_val_q)) {
       break
     }
     rst[rst > rst_val_q[[i]] & rst <= rst_val_q[[i+1]]] <- i+1
   }

   return(rst)
 }


 ### A function to return numeric vector of probabilities with values in [0,1]
 probs <- function(num = 1, n = 4){
   qqq <- 0; ql <- c()
   for (i in 1:n) {
     qqq <- qqq + num/n
     ql[[i]] <- qqq
   }
   return(unlist(ql))
 }

 # Theme for home range visualization
 hr_font_size_scale <- 2

 hr_viz_theme <- function(hr_font_size_scale = 1.5, aspect.ratio = 1, ...) {
   theme_minimal() %+replace%
     theme(aspect.ratio = aspect.ratio,
           legend.title.position = "left",
           legend.title = element_text(family = "msb", size = 15*hr_font_size_scale, angle = 90, hjust = .5),
           legend.text = element_text(size = 12*hr_font_size_scale, family = "mr"),
           legend.key.height = unit(1.5*hr_font_size_scale, units = "lines"),
           legend.key.width = unit(.5*hr_font_size_scale, units = "lines"),

           strip.background = element_rect(fill = clr_set2[1], colour = clr_set2[1]),
           strip.text = element_text(color = "white", family = "msb",
                                     size = 16*hr_font_size_scale),
           strip.text.x = element_text(margin = margin(t = .4, b = .4, unit = "lines")),
           strip.text.y = element_text(margin = margin(l = .4, r = .4, unit = "lines"),
                                       angle = 90),

           panel.background = element_rect(color = clr_set1[1], , fill = "white"),

           axis.text = element_text(family = "mr", size = 14*hr_font_size_scale),
           axis.title.x = element_text(margin = margin(t = 1, b = 1, unit = "lines")),
           axis.title.y = element_text(margin = margin(r = 1, l = .5, unit = "lines"), angle = 90),
           axis.title = element_text(family = "msb", size = 16*hr_font_size_scale),
           ...
     )
 }

 pval_sig <- function(x){
   if (x >= 0 & x < 0.001) {
     sgn <- "***"
   }else if(x >= 0.001 & x < 0.01){
     sgn <- "**"
   }else if (x >= 0.01 & x < 0.05) {
     sgn <- "*"
   }else{
     sgn <- "ns"
   }
   return(sgn)
 }

 confid_int <- function(x){
   x <- x[!is.na(x)]
   # R program to find the confidence interval

   # Calculate the mean of the sample data
   mean_value <- mean(x)

   # Compute the size
   n <- length(x)

   # Find the standard deviation
   standard_deviation <- sd(x)

   # Find the standard error
   standard_error <- standard_deviation / sqrt(n)
   alpha <- 0.05
   degrees_of_freedom = n - 1
   print(degrees_of_freedom)
   t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
   margin_error <- t_score * standard_error

   # Calculating lower bound and upper bound
   lower_bound <- mean_value - margin_error
   upper_bound <- mean_value + margin_error
#print(c(lower_bound, upper_bound))
   # Print the confidence interval
   return(c(lower_bound, upper_bound))
 }


 ct_to_occupancy2 <- function(data, columns, site, format, tz = "", by_day = 5) {
   get_colname <- function(data, cols) {
     return(colnames(data %>% select({{ cols }})))
   }
   # Extract column names using tidy evaluation
   columns_name <- get_colname(data,  {{ columns }})
   site_name <- get_colname(data, {{ site }})

   selected_col <- c(columns_name, site_name)

   data <- data %>%
     dplyr::select(dplyr::all_of(selected_col)) %>%
     tidyr::pivot_longer(data = ., cols = !site, names_to = "dates",
                         values_to = "value") %>%
     dplyr::rename(`:=` ("site", site)) %>%
     ct::ct_to_occupancy(data = ., date_column = dates, format = format,
                             size_column = value, site_column = site, by_day = by_day) %>%
     dplyr::rename(`:=` ("{site}", site))
   return(data)
 }


 stack_data <- function (df_list)
 {
   if (!is.list(df_list)) {
     stop(sprintf("Input is %s but should be a plain list of dataframe items to be stacked",
                  class(df_list)[1L]))
   }
   col_size <- list()
   for (cl in seq_along(df_list)) {
     col_size[[cl]] <- colnames(df_list[[cl]])
   }
   uniq_colname <- unique(unlist(col_size))
   rebuilded <- list()
   for (df in seq_along(df_list)) {
     interdf <- list()
     for (rw in 1:nrow(df_list[[df]])) {
       it_df <- list()
       for (cln in uniq_colname) {
         it_df[cln] <- ifelse(is.null(df_list[[df]][[cln]]), NA, df_list[[df]][rw, cln])
       }
       interdf[[rw]] <- data.frame(it_df, check.names = FALSE)

     }

     rebuilded[[df]] <- dplyr::bind_rows(interdf)
   }

   treturn <- dplyr::bind_rows(rebuilded)
   return(dplyr::tibble(treturn))
 }


 #
 sd_error <- function(x){
   return(sd(x)/sqrt(length(x[!is.na(x)])))
 }
