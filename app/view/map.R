box::use(
  shiny[div, NS, tags, HTML, moduleServer],
  leaflet[leafletOutput, renderLeaflet]
)

ui <- function(id) {
  ns <- NS(id)
  
  div(id = "main",
      
      leafletOutput(outputId = ns("listing_map"), height = "100vh")
      )
  
  
}


server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    
    output$listing_map <- renderLeaflet({
      
      
      
    })
    
    
  })
    
}
