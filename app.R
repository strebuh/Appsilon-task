source("mod_max_dist.R")
# source("modules2.R")


# Main application
library(shiny)
app_ui <- function() {
  semanticPage(
    # Call the UI  function, this is the only place 
    # your ids will need to be unique
    
    # modules
    vessels_ui(id = "v_type_id", note=TRUE)#,
    
    # # modules2
    # vessels_ui(id = "v_type_id", v_type=TRUE, note=FALSE),
    # vessels_ui(id = "v_name_id", v_type=FALSE, note=TRUE)
    
    # fluidRow(
    #   vessels_ui(id = "v_type_id", type="v_type", note=FALSE)
    # ),
    # fluidRow(
    #   vessels_ui(id = "v_name_id", type="v_name", note=TRUE)
    # )
  )
}

app_server <- function(input, output, session) {
  # We are now calling the module server functions 
  # on a given id that matches the one from the UI
  
  # modlues
  vessels_server(id = "v_type_id")
  
  # # modules2
  # vessels_server(id = "v_type_id", v_type=TRUE)
  # vessels_server(id = "v_name_id", v_type=FALSE)
}

shinyApp(app_ui, app_server)