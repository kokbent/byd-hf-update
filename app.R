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
library(leafem)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
source("p2i_functions.R")
source("functions.R", local = T)

#### Import
load('datafiles/gam_mod_v2-1.rda')
T.GC <- readRDS("datafiles/T_GC.rds")
hf_org <- read_csv("datafiles/hf-locations.csv")
outline <- shapefile("shp/Study_Area_NEW.shp")
grid_df <- read_csv("datafiles/grid_data.csv") %>%
  rename(lng = x, lat = y) %>%
  mutate(year = 2012)
opt_xy <- read_csv("datafiles/opt_xy.csv")
opt_xy_nochps <- read_csv("datafiles/opt_xy_nochps.csv")

#### Initialize
grid_df <- grid_df %>% 
  mutate(prev = predict(gam_mod1, grid_df, type = "response")) %>%
  mutate(incd = sapply(prev, prev_u5_to_incd_all, age_struct = c(0.142, 0.266, 0.592)) * 1000)

prev_org <- sum(grid_df$prev * grid_df$pop_u5) * 100 / sum(grid_df$pop_u5)
incd_org <- sum(grid_df$incd * grid_df$pop_all) / sum(grid_df$pop_all)
lid <- 1

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
           
           fluidRow(
             infoBoxOutput("prev_overall", width = 4),
             infoBoxOutput("incd_overall", width = 4),
             infoBoxOutput("ptme_overall", width = 4)
           ),
           # box(width = NULL, 
           #     
           # ),
           
           box(width = NULL,
               dataTableOutput("hf_table")
           )
    ),
    column(width = 3,
           
           box(width = NULL, status = "warning",
               title = "Scenario and metric",
               radioButtons("scenario", "Choose a scenario :",
                            c("With existing CHPS in place" = "wchps",
                              "Without existing CHPS" = "wochps")),
               radioButtons("metric", "Choose a metric to display :",
                            c("Prevalence (0 to 1) for children under 5 years old" = "prev",
                              "Incidence per 1000 person year for all ages" = "incd",
                              "Travel time (min)" = "time_allhf")),
               radioButtons("metric_type", "Magnitude or Difference :",
                            c("Magnitude of the metric" = "magn",
                              "Difference from the baseline" = "diff"))
           ),
           
           
           
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
               # uiOutput("new_hf_text"),
               actionButton("add_coord", "Add Facility"),
               actionButton("refit", "Update Predictions"),
               actionButton("reset", "Reset Now")
           ),
           
           box(width = NULL, status = "warning",
               title = "Optimal locations for new health facilities",
               p(class = "text-muted",
                 "Choose the metric to optimize, and then choose the number of
                 new health facilities to be added. Press \"Optimize\" and the app will
                 display the optimal locations that would reduce the chosen metric as much
                 as possible."
               ),
               radioButtons("opt_metric", "",
                            c("District-wide prevalence of children under 5" = "w_prev", 
                              "Incidence of all ages" = "incd",
                              "Average travel time per person" = "ptme"), 
                            inline = F),
               sliderInput("opt_nhf", "Number of health facilities to be added",
                           min = 1, max = 5, value = 1, step = 1),
               actionButton("optim", "Optimize")
              )

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
  
  # hf_org <- reactive(
  #   if (input$scenario == "wchps") {
  #     read_csv("datafiles/hf-locations.csv")
  #   } else {
  #     read_csv("datafiles/hf-locations.csv") %>%
  #       filter(Type != "CHPS")
  #   }
  # )
  
  ## Two sets of reactive values container
  # tmp holds all the newly added hfs that are not fitted yet
  # vals holds only the hfs that are already fitted
  # When we pass tmp to vals, refitting begins
  tmp <- reactiveValues()
  tmp$hf_data <- hf_org
  
  vals <- reactiveValues()
  vals$hf_data <- hf_org
  vals$grid <- grid_df
  vals$grid_org <- grid_df
  vals$prev_org <- prev_org
  vals$incd_org <- incd_org
  
  
  ## Output: Data Table
  output$hf_table=renderDataTable({vals$hf_data})
  
  
  ## Output: Coordinates of click (Need revision)
  output$new_hf_text <- renderUI({
    
    lat <- unlist(input$prev_map_click$lat)
    lon <- unlist(input$prev_map_click$lng)

    new_loc <- sprintf("<strong>New Location:</strong> <br> Lat: %f <br> Lng: %f",
                       lat, lon) %>%
      lapply(htmltools::HTML)

    new_loc
  })
  
  ## Output: Leaflet
  output$prev_map <- renderLeaflet({
    # Graphics

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
                        icon = icons)
      # mapview::addMouseCoordinates()
    
    # Choosing layer to display: Prevalence, Incidence or Travel time,
    # and Magnitude or differences
    if (input$metric_type == "magn") {
      ras <- vals$grid[,c("lng", "lat", input$metric)] %>% 
        rasterFromXYZ(crs = CRS("+init=epsg:4326"))
      pal <- paste0(input$metric, "_pal") %>% get
    } else {
      metric_diff <- vals$grid[, input$metric] - vals$grid_org[, input$metric]
      metric_diff[abs(metric_diff) < 0.001] <- 0
      ras <- cbind(vals$grid[,c("lng", "lat")], metric_diff) %>% 
        rasterFromXYZ(crs = CRS("+init=epsg:4326"))
      pal <- paste0(input$metric, "_diff_pal") %>% get
    }
    
    params <- set_map_params(input$metric, input$metric_type)
    print(params)
    
    lid <<- lid+1
    layerId <- paste0("l", lid)

    prev_map <- prev_map %>%
      addRasterImage(x = ras,
                     colors = pal,
                     opacity = 0.5,
                     layerId = layerId,
                     group = layerId) %>%
      addLegend(pal = pal, 
                values = params$val,
                bins = params$val,
                title = params$titl) %>%
      addImageQuery(x = ras,
                    type = "mousemove",
                    digits = 3,
                    prefix = params$prefix,
                    position = "bottomright",
                    layerId = layerId)
    
    prev_map
  })
  
  ## Output: Prevalence info box
  output$prev_overall <- renderInfoBox({
    prev_upd <- sum(vals$grid$prev * vals$grid$pop_u5) * 100 / sum(vals$grid$pop_u5)
    infoBox(
      "District-wide prevalence",
      value = round(prev_upd, 1) %>% paste0("%"),
      subtitle = ifelse(nrow(vals$hf_data) == ifelse(input$scenario == "wchps", 8, 5), "",
                        paste0(round(prev_upd - vals$prev_org, 1), "% from initial map")),
      icon = icon("heart"),
      color = "purple"
    )
  })
  
  ## Output: Incidence info box
  output$incd_overall <- renderInfoBox({
    incd_upd <- sum(vals$grid$incd * vals$grid$pop_all) / sum(vals$grid$pop_all)
    infoBox(
      "Incidence per 1000 person years observed",
      value = round(incd_upd, 1),
      subtitle = ifelse(nrow(vals$hf_data) == ifelse(input$scenario == "wchps", 8, 5), "",
                        paste0(round(incd_upd - vals$incd_org, 1), 
                               " from initial map")),
      icon = icon("ambulance"),
      color = "red"
    )
  })
  
  ## Output: Travel time info box
  output$ptme_overall <- renderInfoBox({
    ptme_upd <- sum(vals$grid$time_allhf * vals$grid$pop_all) / sum(vals$grid$pop_all)
    ptme_org <- sum(vals$grid_org$time_allhf * vals$grid_org$pop_all) / sum(vals$grid_org$pop_all)
    infoBox(
      "Travel time per person",
      value = paste0(round(ptme_upd, 1), " min"),
      subtitle = ifelse(nrow(vals$hf_data) == ifelse(input$scenario == "wchps", 8, 5), "",
                        paste0(round(ptme_upd - ptme_org, 1), " from initial map")),
      icon = icon("hourglass-half"),
      color = "yellow"
    )
  })
  
  ## Button logics: Refit
  observeEvent(input$refit, {
    vals$hf_data <- tmp$hf_data
    vals$grid <- update_prediction(vals$hf_data, T.GC)
  })
  
  ## Button logics: Add Facility
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
  
  ##
  observeEvent(input$scenario, {
    
    if (input$scenario == "wchps") {
      vals$hf_data <- hf_org
      vals$grid <- grid_df
      vals$grid_org <- grid_df
      vals$prev_org <- prev_org
      vals$incd_org <- incd_org
      tmp$hf_data <- hf_org
    } else {
      vals$hf_data <- hf_org %>% 
        filter(Type != "CHPS")
      vals$grid <- update_prediction(vals$hf_data, T.GC)
      vals$grid_org <- vals$grid
      tmp$hf_data <- hf_org %>%
        filter(Type != "CHPS")
      vals$prev_org <- sum(vals$grid$prev * vals$grid$pop_u5) * 100 / sum(vals$grid$pop_u5)
      vals$incd_org <- sum(vals$grid$incd)
    }
  })
  
  ## Button logics: Reset
  observeEvent(input$reset, {
    
    if (input$scenario == "wchps") {
      vals$hf_data <- hf_org
      vals$grid <- grid_df
      tmp$hf_data <- hf_org
    } else {
      vals$hf_data <- hf_org %>% 
        filter(Type != "CHPS")
      vals$grid <- update_prediction(vals$hf_data, T.GC)
      tmp$hf_data <- hf_org %>%
        filter(Type != "CHPS")
    }
  })
  
  ## Leaflet map click logics
  observeEvent(input$prev_map_click, {
    click <- input$prev_map_click
    
    proxy <- leafletProxy("prev_map")
    proxy %>% 
      clearGroup("new_point") %>%
      addCircles(click$lng, click$lat, radius=10, color="red", group = "new_point")
    
  })
  
  ## Optimization
  observeEvent(input$optim, {
    if (input$scenario == "wchps") {
      opt <- opt_xy %>%
        filter(nhf == input$opt_nhf, metric == input$opt_metric) %>%
        mutate(Name = "Added Facility",
               Latitude = round(lat, 5),
               Longitude = round(lng, 5),
               Type = "Optimization") %>%
        select(-nhf, -metric, -lat, -lng)
      hf_new <- bind_rows(hf_org, opt)
    } else {
      opt <- opt_xy_nochps %>%
        filter(nhf == input$opt_nhf, metric == input$opt_metric) %>%
        mutate(Name = "Added Facility",
               Latitude = round(lat, 5),
               Longitude = round(lng, 5),
               Type = "Optimization") %>%
        select(-nhf, -metric, -lat, -lng)
      hf_new <- bind_rows(hf_org %>% filter(Type != "CHPS"), 
                          opt)
    }
    
    tmp$hf_data <- hf_new
    vals$hf_data <- hf_new
    vals$grid <- update_prediction(vals$hf_data, T.GC)
  })
}


#### Execute
shinyApp(ui, server)