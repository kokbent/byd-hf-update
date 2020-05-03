update_prediction <- function(hfs) {
  
  points <- hfs %>% 
    dplyr::select(x = Longitude, y = Latitude)
  
  # Convert the points into a matrix
  xy.matrix <- points %>%
    as.data.frame() %>%
    as.matrix()
  
  # Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  temp.raster <- accCost(T.GC, xy.matrix)
  
  grid_newdf <- grid_df
  grid_xy <- grid_newdf[,c("x", "y")] %>%
    as.matrix()
  grid_newdf$time_hf <- raster::extract(temp.raster, grid_xy)
  
  grid_newdf <- grid_newdf %>% 
    mutate(prev = predict(gam_mod, grid_newdf, type = "response") %>% as.vector,
           incd = (prev * pop_den) %>% as.vector)
  
  return(grid_newdf)
}

icon_color <- function(type) {
  case_when(
    type == "CHPS" ~ "red", 
    type == "Health Center" ~ "blue", 
    type == "User Defined" ~ "cadetblue"
  )
}

get_domain <- function(x){
  tmp <- max(abs(x))
  out <- ifelse(is.na(tmp), 0.0001, tmp)
  out <- ifelse(out == 0, 0.0001, out)
  return(out)
}

p2i_0_to_5 <- function(x) {
  # Prev to Incidence per person-year
  return(2.38*x + 3.92*x^2 - 9.30*x^3 + 5.57*x^4 - 0.53*x^5)
}