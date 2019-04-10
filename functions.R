

load('datafiles/gam_mod.rda')

area.shape <- readOGR("shp/Study_Area_NEW.shp")
# district_grid <- readOGR("shp/grid_data.shp")
# grid_dat <- read_csv("datafiles/testing_data.csv")

# Initialize cost-surface
left   <- -2.0
right  <- 0.0
bottom <- 50.0
top    <- 52.0
transition.matrix.exists.flag <- 0 # if the geo-corrected graph has already been made, this can save time.  Uses the same T.GC.filename as specified using the T.GC.filename variable.

# Input Files
# friction.surface.filename <- 'shp/friction_surface/friction_surface_2015_v1.0.tif'
# point.filename <- 'data/hf-locations.csv' # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header.

# Output Files
T.filename <- 'study.area.T.rds'
T.GC.filename <- 'study.area.T.GC.rds'
# friction <- raster("output/shp/friction-crop.tif")
# fs1 <- crop(friction, extent(area.shape))
fs1 <- raster("shp/friction-crop.tif")

# Write functions ----
update_prediction <- function(hfs) {
  
  points <- hfs %>% 
    dplyr::select(x = Longitude, y = Latitude)
  temp <- dim(points)
  n.points <- temp[1]
  
  # Calculate time raster
  if (transition.matrix.exists.flag == 1) {
    # Read in the transition matrix object if it has been pre-computed
    T.GC <- readRDS(T.GC.filename)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    t <- transition(fs1, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
    # saveRDS(T, T.filename)
    T.GC <- geoCorrection(t)
    # saveRDS(T.GC, T.GC.filename)
  }
  
  # Convert the points into a matrix
  xy.data.frame <- data.frame()
  xy.data.frame[1:n.points,1] <- points[,1]
  xy.data.frame[1:n.points,2] <- points[,2]
  xy.matrix <- as.matrix(xy.data.frame)
  
  # Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  temp.raster <- accCost(T.GC, xy.matrix)
  
  # # Update travel time 
  # grid_loc <- dplyr::select(grid, lng, lat) # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header
  # grid$time_hf <- extract(temp.raster, grid_loc)
  
  # Make updated prediction 
  # pred <- predict(gam_mod, grid_dat, type = "response")
  
 
    grid_loc <- grid %>% 
      as.data.frame() %>% 
      dplyr::select(lng, lat) # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header
    # 
    # time_hf = extract(temp.raster, grid_loc)
    # prev = predict(gam_mod, grid, type = "response")
    # cases = prev*grid$pop_den
    # if_else(grid$ck_full == 1, prev, NA_real_)
    
    
    grid <- grid %>% 
      mutate(time_hf = extract(temp.raster, grid_loc), 
             prev = predict(gam_mod, grid, type = "response"), 
             cases = prev*grid$pop_den,
             prv_pdt = if_else(ck_full == 1, prev, NA_real_), 
             cas_pdt = if_else(ck_full == 1, cases, NA_real_))
    
    # grid <- grid %>% 
    #   mutate(prv_pdt = if_else(ck_full == "TRUE", prev, NA_real_), 
    #          cas_pdt = if_else(ck_full == "TRUE", cases, NA_real_))
    
    return(grid)
    
    
  
}