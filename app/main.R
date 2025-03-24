box::use(
  shiny[NS, div, tags, HTML, moduleServer, renderUI, reactive, debounce, reactiveValues],
  shiny.fluent[Stack, fluentPage],
  waiter[useWaiter, waiterPreloader, spin_3]
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
    waiterPreloader(html = spin_3() ),
      
    div(id = "container",
        
        side_bar$ui(ns("side_bar")),
        
        div(id = "drag-handle", 
            tags$i(class = "fas fa-circle-left")
            ),
        
        map$ui(ns("map_listing"))
        )
    
  )
}

#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    selected_data <- side_bar$server("side_bar")
    
    map$server("map_listing", selected_data) 
    
  })
  
}
