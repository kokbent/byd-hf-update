library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(geosphere)
library(sp)
library(rgdal)
library(raster)
library(htmltools)
library(mgcv)
library(splines)
library(DT)
library(gdistance)
library(mapview)

load('datafiles/gam_mod.rda')

area.shape <- readOGR("shp/Study_Area_NEW.shp")

left   <- -2.0
right  <- 0.0
bottom <- 50.0
top    <- 52.0
transition.matrix.exists.flag <- 0 # if the geo-corrected graph has already been made, this can save time.  Uses the same T.GC.filename as specified using the T.GC.filename variable.
T.filename <- 'study.area.T.rds'
T.GC.filename <- 'study.area.T.GC.rds'
fs1 <- raster("shp/friction-crop.tif")

hf_org <- read_csv("datafiles/hf-locations.csv") 
outline <- sf::st_read("shp/Study_Area_NEW.shp")
grid <- sf::st_read("shp/grid_data_update.shp") %>% 
  mutate(prv_pdt_org  = prv_pdt, 
         cas_pdt_org = cas_pdt, 
         prv_diff = prv_pdt - prv_pdt_org, 
         cas_diff = cas_pdt - cas_pdt_org)

# Functions
update_prediction <- function(hfs) {
  
  points <- hfs %>% 
    dplyr::select(x = Longitude, y = Latitude)
  temp <- dim(points)
  n.points <- temp[1]
  
  # Calculate time raster
  if (transition.matrix.exists.flag == 1) {
    T.GC <- readRDS(T.GC.filename)
  } else {
    t <- transition(fs1, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
    T.GC <- geoCorrection(t)
  }
  
  # Convert the points into a matrix
  xy.data.frame <- data.frame()
  xy.data.frame[1:n.points,1] <- points[,1]
  xy.data.frame[1:n.points,2] <- points[,2]
  xy.matrix <- as.matrix(xy.data.frame)
  
  # Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  temp.raster <- accCost(T.GC, xy.matrix)
  
  grid_loc <- grid %>% 
    as.data.frame() %>% 
    dplyr::select(lng, lat) # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header
  
  grid <- grid %>% 
    mutate(time_hf  = extract(temp.raster, grid_loc), 
           prev     = predict(gam_mod, grid, type = "response"), 
           cases    = prev*grid$pop_den,
           prv_pdt  = if_else(ck_full == 1, prev, NA_real_), 
           cas_pdt  = if_else(ck_full == 1, cases, NA_real_), 
           prv_diff = prv_pdt - prv_pdt_org, 
           cas_diff = cas_pdt - cas_pdt_org)
  
  return(grid)
}
icon_color <- function(type) {
  case_when(
    type == "CHPS" ~ "red", 
    type == "Health Center" ~ "blue", 
    type == "User Defined" ~ "cadetblue")
}
get_domain <- function(x){
  tmp <- max(abs(x))
  out <- ifelse(is.na(tmp), 0.0001, tmp)
  out <- ifelse(out == 0, 0.0001, out)
  return(out)
}

sum_prev <- sum(grid$prv_pdt_org, na.rm = T)
sum_case <- sum(grid$cas_pdt_org, na.rm = T)

header <- dashboardHeader(
  title = "BYD Health Facilities"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("prev_map", height = 500)
           ),
           box(width = NULL,
               dataTableOutput("hf_table")
           )
    ),
    column(width = 3,

           box(width = NULL, status = "warning",
               title = "Add new health facility",
               p(class = "text-muted",
                 "Click on the map to load a new Lat/Long for a propose location, 
                 and press the button below to refit the model."
               ),
               uiOutput("new_hf_text"),
               # actionButton("add_hf", "Add Facility"), 
               actionButton("refit", "Re-fit Model")
           ),
           box(width = NULL, status = "warning",
               title = "Reset",
               p(class = "text-muted",
                 "Reload app to remove added health facilities."
               ),
               actionButton("reset", "Reset Now")
           )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

server <- function(input, output) {
  vals <- reactiveValues()
  vals$hf_data <- hf_org
  vals$grid <- grid
  
  output$hf_table=renderDataTable({vals$hf_data})
  
  output$new_hf_text <- renderUI({
    
    lat <- unlist(input$prev_map_shape_click$lat)
    lon <- unlist(input$prev_map_shape_click$lng)

    new_loc <- sprintf("<strong>New Location:</strong> <br> Lat: %f <br> Lng: %f",
                       lat, lon) %>%
      lapply(htmltools::HTML)

    new_loc
  })

 output$prev_map <- renderLeaflet({
  
  prev_pal <- colorNumeric(palette = "inferno", na.color = "#E1E1DD", domain = c(0,1))
  case_pal <- colorNumeric(palette = "inferno", na.color = "#E1E1DD", domain = c(0, 200))
  
  # Get max values for setting diverging color palette
  # m_prev <- get_domain(vals$grid$prv_diff) + 0.001
  # m_case <- get_domain(vals$grid$cas_diff) + 1
  # m_prev <- max(abs(vals$grid$prv_diff))
  # m_case <- max(abs(vals$grid$cas_diff))
  
  # prev_diff_pal <- colorNumeric(palette = "RdBu", na.color = "#E1E1DD", domain = c(-m_prev, m_prev))
  # case_diff_pal <- colorNumeric(palette = "RdBu", na.color = "#E1E1DD", domain = c(-m_case, m_case))
  prev_diff_pal <- colorNumeric(palette = "RdBu", na.color = "#E1E1DD", domain = c(-0.02, 0.02), reverse = T)
  case_diff_pal <- colorNumeric(palette = "RdBu", na.color = "#E1E1DD", domain = c(-5, 5), reverse = T)
  
  icons <- awesomeIcons(icon = 'medkit', library = 'fa', iconColor = '#FFFFFF',
                        markerColor = icon_color(vals$hf_data$Type))

  prev_map <- leaflet(vals$grid) %>%
    addPolygons(data = outline,
                color = "black",
                weight = 2,
                opacity = 1,
                fillOpacity = 0) %>%
    addPolygons(color = "NULL",
                weight = 0, 
                fillColor = ~prev_pal(prv_pdt), 
                fillOpacity = 1,
                group = "Prevalence", 
                label = sprintf("Prevalence: %.3f", vals$grid$prv_pdt) %>% 
                  lapply(htmltools::HTML)) %>%
    addPolygons(color = "NULL",
                weight = 0, 
                fillColor = ~case_pal(cas_pdt), 
                fillOpacity = 1,
                group = "Cases", 
                label = sprintf("Exp. # of Cases: %.2f", vals$grid$cas_pdt) %>% 
                  lapply(htmltools::HTML)) %>%
    addPolygons(color = "NULL",
                weight = 0, 
                fillColor = ~prev_diff_pal(prv_diff), 
                fillOpacity = 1,
                group = "Diff. Prev", 
                label = sprintf("Change in Prevalence: %.3f", vals$grid$prv_diff) %>% 
                  lapply(htmltools::HTML)) %>%
    addPolygons(color = "NULL",
                weight = 0, 
                fillColor = ~case_diff_pal(cas_diff), 
                fillOpacity = 1,
                group = "Diff. Cases", 
                label = sprintf("Change in Exp. # of Cases: %.2f", vals$grid$cas_diff) %>% 
                  lapply(htmltools::HTML)) %>%
    addAwesomeMarkers(data = vals$hf_data,
                      lng = vals$hf_data$Longitude,
                      lat = vals$hf_data$Latitude,
                      popup = vals$hf_data$Name, 
                      icon = icons) %>%
    addLegend(pal = prev_pal, values = ~prv_pdt, na.label = NULL, 
              title = "Prevalence:", 
              group = "Prevalence legend") %>%
    addLegend(pal = case_pal, values = ~cas_pdt,
              title = "Est. Cases:", 
              group = "Cases legend") %>%
    addLegend(pal = prev_diff_pal, values = ~prv_diff,
              title = "Change in Prevalence",
              group = "Diff. Prev legend") %>%
    addLegend(pal = case_diff_pal, values = ~cas_diff,
              title = "Change in Exp. Cases",
              group = "Diff. Cases legend") %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
    addMiniMap(tiles = providers$OpenStreetMap.BlackAndWhite, 
               position = "bottomleft",
               toggleDisplay = TRUE, width = 120, height = 120) %>% 
    mapview::addMouseCoordinates() %>%
    addLayersControl(
      baseGroups = c("Prevalence", "Cases", "Diff. Prev", "Diff. Cases"), 
      overlayGroups = c("Prevalence legend", "Cases legend", "Diff. Prev legend", "Diff. Cases legend"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = TRUE)
    ) 
  prev_map
  })
 
 observeEvent(input$refit, {
   lat <- unlist(input$prev_map_shape_click$lat)
   lon <- unlist(input$prev_map_shape_click$lng)
   
   vals$hf_data <- vals$hf_data %>% 
     add_case(
       Name = "New Facility",
       Latitude = round(lat, 3), 
       Longitude = round(lon, 3),
       Type = "User Defined")
   
   vals$grid <- update_prediction(vals$hf_data)
 })
 
 observeEvent(input$reset, {
   vals$hf_data <- hf_org
   vals$grid <- grid
 })  
}
 
shinyApp(ui, server)