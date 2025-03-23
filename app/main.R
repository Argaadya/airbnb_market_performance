box::use(
  shiny[NS, div, tags, HTML, moduleServer, renderUI, reactive, debounce, reactiveValues],
  shiny.fluent[Stack, fluentPage],
  waiter[useWaiter]
)

box::use(app/view/side_bar,
         app/view/map,
         app/view/menu_filter
         )

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  fluentPage(
    
    useWaiter(),
      
    div(id = "container",
        
        side_bar$ui(ns("side_bar")),
        
        div(id = "drag-handle", 
            tags$i(class = "fas fa-circle-left")
            ),
        
        map$ui(ns("listing_map"))
        )
    
  )
}

#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    side_bar$server("side_bar")
    
    map$server("listing_map") 
    
  })
  
}
