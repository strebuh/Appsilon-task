library(shiny)
library(shiny.semantic)
library(shinydashboard)
library(leaflet)
library(data.table)


vessel_types <- readRDS("data/vessel_types.RDS")
data_by_type <- readRDS("data/data_by_type.RDS")
data_by_country <- readRDS("data/data_by_country.RDS")
data_by_ship <- readRDS("data/data_by_ship.RDS")


info_box <- function(info, icon, color){
  div(class = paste0("ui icon", color," message"),
      icon(icon),
      div(class = "content",
          div(class = "header", p(info))
      )
  )
}

get_stat <- function(data, column, stat, ...) {
  if(column %in% names(data)){
    stat <- do.call(stat, list(data[[column]]))
    return(stat)
  } else {
    return(NA)
  }
}


# Module UI function
vessels_ui <- function(id, note) {
  ns <- NS(id)
  tagList(
    sidebar_layout(
      sidebar_panel(
        selectInput(ns("v_type_imt"), 
                    "Vessel types",
                    choices = vessel_types,
                    selected = vessel_types[1]),
        br(),
        uiOutput(ns("v_imt_UI")),
        br(),
        info_box(textOutput(
          ns("n_type_ships")),
          icon = "ship",
          color = "green"),
        br(),
        info_box(textOutput(
          ns("n_avg_speed")),
          icon = "anchor",
          color = "green"),
        br(),
        # info_box(textOutput(
        #   ns("n_type_ships")),
        #   icon = "ship",
        #   color = "green"),
        # br(),
        
        width = 4,
        fluid = T
      ),
      
    main_panel(
      h3(htmlOutput(ns("v_note_opt"))),
      leafletOutput(ns("v_move_map")),
      width = 8,
      fluid = T
    )
  )
    )
}

# total distance of the ship
# average distance of the ship
# avg speed compared to avg in the group
# size of the boat comparing to the average in a group

vessels_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Retrieve from the data ships of selected type
      output$v_imt_UI <- renderUI({
        ns <- NS(id)
        req(input$v_type_imt)
        choices <- data_by_ship[ship_type == input$v_type_imt,]$SHIPNAME
        selectInput(ns("v_imt"),
                    "Vessel name",
                    choices = choices,
                    selected = choices[1])
      })
      
      
      output$n_type_ships <- renderText({
        value = get_stat(data_by_ship[ship_type == input$v_type_imt,], "SHIP_ID", uniqueN)
        paste("Number of ships:", value)
        # get_stat(data_by_ship[ship_type == input$v_type_imt,], "id", uniqueN)
      })
      
      output$n_avg_speed <- renderText({
        req(input$v_imt)
        
        name = data_by_ship[SHIPNAME==input$v_imt, ]$SHIPNAME
        country = data_by_ship[SHIPNAME==input$v_imt, ]$FLAG
        max_speed = data_by_ship[SHIPNAME==input$v_imt, ]$MAX_SPEED 
        avg_speed = data_by_ship[SHIPNAME==input$v_imt, ]$AVG_SPEED
        tot_dist = data_by_ship[SHIPNAME==input$v_imt, ]$TOT_DIST 
        stand_time = data_by_ship[SHIPNAME==input$v_imt, ]$STAND_TIME
        
        paste("Ship name:", name, "\n",
              "Ship country:", country, "\n", 
              "Max speed:", max_speed, "\n",
              "Average speed:", avg_speed, "\n",
              "Total distance:", tot_dist, "\n",
              "Standing time:", stand_time)
      })
      
      # Generate and render the note with the distance
      get_note <- reactive({
        req(input$v_type_imt, input$v_imt)
        
        distance =  round(data_by_ship[ship_type == input$v_type_imt & SHIPNAME == input$v_imt,]$DIST_M, 2)
        start = data_by_ship[ship_type == input$v_type_imt & SHIPNAME == input$v_imt,]$DATETIME
        stop = data_by_ship[ship_type == input$v_type_imt & SHIPNAME == input$v_imt,]$DATETIME2
        
        paste(input$v_imt, "-", distance, "meters between", start ,"and" , stop)
      })
      output$v_note_opt <- renderText(get_note())
      
      # Generate leaflet map
      get_map <- reactive({
        req(input$v_type_imt, input$v_imt)
        leaflet(data = data_by_ship[ship_type == input$v_type_imt & SHIPNAME == input$v_imt]) %>% 
          addTiles() %>% 
          addMarkers(~LON, ~LAT, label = ~paste("Start stamp at ", as.character(DATETIME))) %>% 
          addMarkers(~LON2, ~LAT2,  label = ~paste("Stop stamp at", as.character(DATETIME2), "after", as.character(difftime(DATETIME2, DATETIME)), "sec."))
      })
      output$v_move_map <- renderLeaflet(get_map())
      
    }
  )
}