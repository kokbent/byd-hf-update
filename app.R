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

source("functions.R")

hf_org <- read_csv("datafiles/hf-locations.csv")
outline <- sf::st_read("shp/Study_Area_NEW.shp")
grid <- sf::st_read("shp/grid_data_update.shp")
sum_prev <- sum(grid$prv_pdt, na.rm = T)
sum_case <- sum(grid$case_pdt, na.rm = T)

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
                 "Click on the map to load a new Lat/Long for a propose location, and press the button below to refit the model"
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
  
  output$hf_table=renderDataTable({vals$hf_data} )
  
  
  output$new_hf_text <- renderUI({
    
    # click_dat <- unlist(input$prev_map_shape_click)
    E
    lat <- unlist(input$prev_map_shape_click$lat)
    lon <- unlist(input$prev_map_shape_click$lng)

    new_loc <- sprintf("<strong>New Location:</strong> <br> Lat: %f <br> Lng: %f",
                       lat, lon) %>%
      lapply(htmltools::HTML)

    new_loc
  })

 output$prev_map <- renderLeaflet({
  
  prev_pal <- colorNumeric(palette = "inferno",na.color = "NA", domain = c(0,1))
  case_pal <- colorNumeric(palette = "inferno", na.color = "NA",  domain = c(0, 200))
  
  prev_map <- leaflet(vals$grid) %>%
    addPolygons(data = outline,
                color = "black",
                weight = 1,
                opacity = 1, 
                fillOpacity = 0) %>%
    addPolygons(color = "NULL",
                weight = 0, 
                fillColor = ~prev_pal(prv_pdt), 
                fillOpacity = 0.7, 
                group = "Prevalence") %>%
    addPolygons(color = "NULL",
                weight = 0, 
                fillColor = ~case_pal(cas_pdt), 
                fillOpacity = 0.7, 
                group = "Cases") %>%
    addAwesomeMarkers(data = vals$hf_data,
                      lng = vals$hf_data$Longitude,
                      lat = vals$hf_data$Latitude,
                      popup = vals$hf_data$Name) %>%
    addLegend(pal = prev_pal, values = ~prv_pdt,
              title = "Prevalence:", 
              group = "Prevalence") %>%
    addLegend(pal = case_pal, values = ~cas_pdt,
              title = "Est. Cases:", 
              group = "Cases") %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
    addLayersControl(
      baseGroups = c("Prevalence", "Cases"), position = "bottomleft",
      # overlayGroups = c("Prevalence", "Cases"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  prev_map
  })
 
 observeEvent(input$refit, {
   
   # click_dat <- unlist(input$prev_map_shape_click)
   
   lat <- unlist(input$prev_map_shape_click$lat)
   lon <- unlist(input$prev_map_shape_click$lng)
   
   vals$hf_data <- vals$hf_data %>% 
     add_case(
       # Name = paste("New Facility", row_number(.) - 8),
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