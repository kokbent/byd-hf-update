rm(list = ls())

library(shiny)
library(shinydashboard)
library(leaflet)
library(geosphere)
library(raster)
library(rgdal)
library(htmltools)
library(mgcv)
library(splines)
library(DT)
library(gdistance)
library(mapview)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)

#### Import
load('datafiles/gam_mod_v2.rda')
T.GC <- readRDS("datafiles/T_GC.rds")
hf_org <- read_csv("datafiles/hf-locations.csv") 
outline <- shapefile("shp/Study_Area_NEW.shp")
grid_df <- read_csv("datafiles/grid_data.csv")

#### Initialize
p2i_0_to_5 <- function(x) {
  # Prev to Incidence per person-year
  return(2.38*x + 3.92*x^2 - 9.30*x^3 + 5.57*x^4 - 0.53*x^5)
}

grid_df <- grid_df %>% 
  mutate(prev = predict(gam_mod, grid_df, type = "response") %>% as.vector) %>%
  mutate(incd = (p2i_0_to_5(prev) * pop_den) %>% as.vector)
         
prev_org <- mean(grid_df$prev)
incd_org <- sum(grid_df$incd)

#### Functions
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
    mutate(prev = predict(gam_mod, grid_newdf, type = "response") %>% as.vector) %>%
    mutate(incd = (p2i_0_to_5(prev) * pop_den) %>% as.vector)
  
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

#### UI
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
               title = "Choose metric to display",
               radioButtons("metric", "For children under 5 years old",
                            c("Prevalence (0 to 1)" = "prev",
                              "Incidence (Number of cases per year)" = "incd"))
           ),
           
           infoBoxOutput("prev_overall", width = NULL),
           
           infoBoxOutput("incd_overall", width = NULL),
           
           box(width = NULL, status = "warning",
               title = "Add new health facility",
               p(class = "text-muted",
                 "Click on the map and press the \"Add Facility\" button below 
                 to add new facilities. You can do it repeatedly to add more facilities."
               ),
               p(class = "text-muted",
                 "After adding all new facilities, press \"Re-fit Model\" to update the map.
                 You can keep adding facilities after updating the map."
               ),
               p(class = "text-muted",
                 "Press \"Reset Now\" button
                 to remove all proposed facilities and revert to initial map."
               ),
               uiOutput("new_hf_text"),
               actionButton("add_coord", "Add Facility"),
               actionButton("refit", "Re-fit Model"),
               actionButton("reset", "Reset Now")
           ),
           
           box(width = NULL, status = "warning",
               title = "Optimal locations for new health facilities (Under development)",
               p(class = "text-muted",
                 "Choose the metric to optimize, and then choose the number of
                 new health facilities to be added. Press \"Optimize\" and the app will
                 display the optimal locations that would reduce the chosen metric as much
                 as possible."
               ),
               radioButtons("opt-metric", "For children under 5 years old",
                            c("Prevalence" = "prev", "Incidence" = "incd"), inline = T),
               sliderInput("opt-nhf", "Number of health facilities to be added",
                           min = 1, max = 7, value = 1, step = 1)
               )
           # box(width = NULL, status = "warning",
           #     title = "Reset",
           #     p(class = "text-muted",
           #       "Reload app to remove added health facilities."
           #     ),
           #     actionButton("reset", "Reset Now")
           # )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)


#### Server logics
server <- function(input, output) {
  tmp <- reactiveValues()
  tmp$hf_data <- hf_org
  tmp$grid <- grid_df
  
  vals <- reactiveValues()
  vals$hf_data <- hf_org
  vals$grid <- grid_df
  
  output$hf_table=renderDataTable({vals$hf_data})
  
  output$new_hf_text <- renderUI({
    
    lat <- unlist(input$prev_map_click$lat)
    lon <- unlist(input$prev_map_click$lng)

    new_loc <- sprintf("<strong>New Location:</strong> <br> Lat: %f <br> Lng: %f",
                       lat, lon) %>%
      lapply(htmltools::HTML)

    new_loc
  })

 output$prev_map <- renderLeaflet({
  # Graphics
  prev_pal <- colorNumeric(palette = "inferno", na.color = "#00000000", domain = c(0, 1))
  incd_pal <- colorNumeric(palette = "inferno", na.color = "#00000000", domain = c(-0.1, 800))
  icons <- awesomeIcons(icon = 'medkit', library = 'fa', iconColor = '#FFFFFF',
                        markerColor = icon_color(vals$hf_data$Type))
  
  # Base map
  prev_map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = outline,
                color = "black",
                weight = 2,
                opacity = 1,
                fillOpacity = 0) %>%
    addAwesomeMarkers(data = vals$hf_data,
                      lng = vals$hf_data$Longitude,
                      lat = vals$hf_data$Latitude,
                      popup = vals$hf_data$Name,
                      icon = icons) %>%
    mapview::addMouseCoordinates()
  
  ras <- vals$grid[,c("x", "y", input$metric)] %>% rasterFromXYZ(crs = CRS("+init=epsg:4326"))
  pal <- paste0(input$metric, "_pal") %>% get
  if (input$metric == "prev") {
    val <- 0:4 * 0.25
    titl <- "Prevalence:"
    prefix <- "Prevalence"
  } else {
    val <- 0:4 * 200
    titl <- "Incidence (per yr):"
    prefix <- "Incidence"
  }
  
  prev_map <- prev_map %>%
    addRasterImage(x = ras,
                   colors = pal,
                   opacity = 0.5,
                   layerId = " ") %>%
    addLegend(pal = pal, values = val,
              title = titl) %>%
    addImageQuery(x = ras, 
                  type = "mousemove", 
                  layerId = " ",
                  digits = 3,
                  prefix = prefix,
                  position = "bottomright")
  
  # Prevalence or Incidence?
  # if (input$metric == "prev") {
  #   
  #     
  # } 
  # 
  # if (input$metric == "incd") {
  #   ras <- vals$grid[,c("x", "y", "incd")] %>% rasterFromXYZ(crs = CRS("+init=epsg:4326"))
  #   
  #   prev_map <- prev_map %>%
  #     addRasterImage(x = ras,
  #                    colors = incd_pal,
  #                    opacity = 0.5,
  #                    layerId = "Incidence") %>%
  #     addImageQuery(x = ras, 
  #                   type = "mousemove", 
  #                   layerId = "Incidence",
  #                   digits = 0,
  #                   prefix = "",
  #                   position = "bottomright") %>%
  #     addLegend(pal = incd_pal, values = 0:4 * 200,
  #               title = "Incidence (per yr):",
  #               group = "Incidence legend")
  #   
  # }
  
    # addLayersControl(
    #   baseGroups = c("Prevalence", "Cases", "Diff. Prev", "Diff. Cases"),
    #   overlayGroups = c("Prevalence legend", "Cases legend", "Diff. Prev legend", "Diff. Cases legend"),
    #   position = "bottomleft",
    #   options = layersControlOptions(collapsed = F)
    # )
  prev_map
  })
 
 output$prev_overall <- renderInfoBox({
   infoBox(
     "Mean prevalence per pixel",
     value = paste0(round(mean(vals$grid$prev), 3) * 100, "%"),
     subtitle = ifelse(nrow(vals$hf_data) == nrow(hf_org), "",
                       paste0(round(mean(vals$grid$prev) - prev_org, 3) * 100, 
                              "% from initial map")),
     icon = icon("heart"),
     color = "purple"
   )
 })
 
 output$incd_overall <- renderInfoBox({
   infoBox(
     "Total incidence per year",
     value = round(sum(vals$grid$incd), 1),
     subtitle = ifelse(nrow(vals$hf_data) == nrow(hf_org), "",
                       paste0(round(sum(vals$grid$incd) - incd_org, 1), 
                              " from initial map")),
     icon = icon("ambulance"),
     color = "red"
   )
 })
 
 observeEvent(input$refit, {
   lat <- unlist(input$prev_map_shape_click$lat)
   lon <- unlist(input$prev_map_shape_click$lng)
   
   vals$hf_data <- tmp$hf_data
   vals$grid <- update_prediction(vals$hf_data)
 })
 
 observeEvent(input$add_coord, {
   lat <- unlist(input$prev_map_click$lat)
   lon <- unlist(input$prev_map_click$lng)
   
   tmp$hf_data <- tmp$hf_data %>% 
     add_case(
       Name = "Added Facility",
       Latitude = round(lat, 5), 
       Longitude = round(lon, 5),
       Type = "User Defined")
   
   icons <- awesomeIcons(icon = 'medkit', library = 'fa', iconColor = '#FFFFFF',
                         markerColor = icon_color(tmp$hf_data$Type))
   
   proxy <- leafletProxy("prev_map")
   proxy %>% clearMarkers() %>%
     addAwesomeMarkers(data = tmp$hf_data,
                       lng = tmp$hf_data$Longitude,
                       lat = tmp$hf_data$Latitude,
                       popup = tmp$hf_data$Name, 
                       icon = icons)
 })
 
 
 observeEvent(input$prev_map_click, {
   click <- input$prev_map_click
   
   proxy <- leafletProxy("prev_map")
   proxy %>% 
     clearGroup("new_point") %>%
     addCircles(click$lng, click$lat, radius=10, color="red", group = "new_point")
   
 })
 
 observeEvent(input$reset, {
   vals$hf_data <- hf_org
   vals$grid <- grid_df
   tmp$hf_data <- hf_org
   tmp$grid <- grid_df
 })  
}


#### Execute
shinyApp(ui, server)