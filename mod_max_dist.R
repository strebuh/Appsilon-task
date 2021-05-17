library(shiny)
library(shiny.semantic)
library(leaflet)


# browser()
# getwd()
vessel_types <- readRDS("data/vessel_types.RDS")
data <- readRDS("data/prep_data.RDS")

# Module UI function
vessels_ui <- function(id, note) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    selectInput(ns("v_type_imt"), 
                  "Vessel types",
                  choices = vessel_types,
                  selected = vessel_types[1]),
    shiny::uiOutput(ns("v_imt_UI")),
    if(note == T){
      h3(htmlOutput(ns("v_note_opt")))
    },
    leafletOutput(ns("v_move_map"))
    )
}

vessels_server <- function(id) {
  # Calling the moduleServer function
  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {
      # This part is the same as the code you would put 
      # inside a standard server
      
      output$v_imt_UI <- renderUI({
        ns <- NS(id)
        req(input$v_type_imt)
        # browser()
        choices <- data[ship_type == input$v_type_imt,]$SHIPNAME
        selectInput(ns("v_imt"),
                           "Vessel name",
                           choices = choices,
                           selected = choices[1])
      })
      
      get_note <- reactive({
        req(input$v_type_imt, input$v_imt)
        paste(input$v_imt, "-", round(data[ship_type == input$v_type_imt & SHIPNAME == input$v_imt,]$DIST_M, 2), "meters.")
      })
      output$v_note_opt <- renderText(get_note())
      
      get_map <- reactive({
        req(input$v_type_imt, input$v_imt)
        leaflet(data = data[ship_type == input$v_type_imt & SHIPNAME == input$v_imt]) %>% 
          addTiles() %>% 
          addMarkers(~LON, ~LAT, label = ~paste("Start stamp at ", as.character(DATETIME))) %>% 
          addMarkers(~LON2, ~LAT2,  label = ~paste("Stop stamp at", as.character(DATETIME2), "after", as.character(difftime(DATETIME2, DATETIME)), "sec."))
      })
      
      output$v_move_map <- renderLeaflet(get_map())

      
      
    }
  )
}