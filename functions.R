update_prediction <- function(hfs, T.GC) {
  
  points <- hfs %>% 
    dplyr::select(x = Longitude, y = Latitude)
  
  xy.matrix <- points %>%
    as.data.frame() %>%
    as.matrix()
  
  ## Update accumulated cost
  temp.raster <- accCost(T.GC, xy.matrix)
  
  grid_newdf <- grid_df
  grid_xy <- grid_newdf[,c("lng", "lat")] %>%
    as.matrix()
  grid_newdf$time_allhf <- raster::extract(temp.raster, grid_xy)
  
  grid_newdf <- grid_newdf %>% 
    mutate(prev = predict(gam_mod1, grid_newdf, type = "response")) %>%
    mutate(incd = sapply(prev, prev_u5_to_incd_all, age_struct = c(0.142, 0.266, 0.592)) * 1000)
  
  return(grid_newdf)
}

icon_color <- function(type) {
  case_when(
    type == "CHPS" ~ "red", 
    type == "Health Center" ~ "blue", 
    type == "User Defined" ~ "cadetblue",
    type == "Optimization" ~ "purple"
  )
}

set_map_params <- function (metric, metric_type) {
  l <- list()
  
  if (metric == "prev") {
    if (metric_type == "magn") {
      l$val <- 0:5 * 0.2
      l$titl <- "Prevalence:"
      l$prefix <- "Prevalence"
    } else {
      l$val <- 6:0 * -0.02
      l$titl <- "Change in prevalence:"
      l$prefix <- "Change"
    }
    
  } else if (metric == "incd") {
    if (metric_type == "magn") {
      l$val <- 0:8 * 50 + 200
      l$titl <- "Incidence (per 1000 PYO):"
      l$prefix <- "Incidence"
    } else {
      l$val <- 4:-1 * -10
      l$titl <- "Change in incidence:"
      l$prefix <- "Change"
    }
  } else {
    if (metric_type == "magn") {
      l$val <- 0:6 * 20
      l$titl <- "Travel time (min):"
      l$prefix <- "Travel time"
    } else {
      l$val <- 4:0 * -10
      l$titl <- "Change in travel time:"
      l$prefix <- "Change"
    }
  }
  
  return(l)
}

#### Preset colour palette
prev_pal <- colorNumeric(palette = "RdYlBu", na.color = "#00000000", domain = c(0, 1),
                         reverse = T)
incd_pal <- colorNumeric(palette = "RdYlBu", na.color = "#00000000", domain = c(200, 600),
                         reverse = T)
time_allhf_pal <- colorNumeric(palette = "RdYlBu", na.color = "#00000000", domain = c(0, 120),
                               reverse = T)

prev_diff_pal <- colorNumeric(palette = "Blues", na.color = "#00000000", domain = c(-0.13, 0),
                              reverse = T)
incd_diff_pal <- colorNumeric(palette = "Blues", na.color = "#00000000", domain = c(-40, 10),
                              reverse = T)
time_allhf_diff_pal <- colorNumeric(palette = "Blues", na.color = "#00000000", domain = c(-45, 0),
                                    reverse = T)