source("mod_max_dist.R")
# source("modules2.R")


# Main application
library(shiny)
app_ui <- function() {
  semanticPage(
    title = "Ship max distance",
    vessels_ui(id = "v_type_id", note=TRUE)#,
  )
}

app_server <- function(input, output, session) {
  vessels_server(id = "v_type_id")
}

shinyApp(app_ui, app_server)