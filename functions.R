update_prediction <- function(hfs) {
  
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
  
  grid_newdf$pred1 <- predict(gam_mod1, grid_newdf, type = "response") %>% as.vector
  grid_newdf$pred1 <- predict(gam_mod2, grid_newdf, type = "response") %>% as.vector
  grid_newdf <- grid_newdf %>% 
    mutate(prev1 = predict(gam_mod1, grid_newdf, type = "response") %>% 
             as.vector,
           prev2 = predict(gam_mod2, grid_newdf, type = "response") %>% 
             as.vector,
           prev = (prev1 + prev2)/2) %>%
    mutate(incd1 = sapply(prev1, prev_u5_to_incd_all, age_struct = c(0.142, 0.266, 0.592)) * pop_all,
           incd2 = sapply(prev2, prev_u5_to_incd_all, age_struct = c(0.142, 0.266, 0.592)) * pop_all,
           incd = (incd1 + incd2)/2)
  
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

reset_hf_scenario
