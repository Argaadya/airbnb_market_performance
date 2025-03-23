box::use(
  shiny[div, NS, tags, HTML, moduleServer, uiOutput, renderUI, br],
  shiny.fluent[Stack],
  highcharter[highchart, highchartOutput, renderHighchart]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    
    # First Row -------
    Stack(horizontal = T,
          tokens = list(childrenGap = 10),
          
          div(style = list(width = "250px",
                           height = "200px"),
              class = "card_box",
              
              div(style = "padding: 10px;font-size:16px", 
                  tags$span(HTML("<b>Active Listing<b>"))
                  ),
              
              uiOutput(ns("card_listing"))
              ),
          
          div(style = list(width = "250px"),
              class = "card_box",
              
              div(style = "padding: 10px;font-size:16px;", 
                  tags$span(HTML("<b>Occupancy Rate<b>"))
                  ),
              
              uiOutput(ns("card_occupancy")),
              
              highchartOutput(ns("trend_occupancy"), width = "250px", height = "100px")
              )
          
          ),
    
    # Second Row -----------
    Stack(horizontal = T,
          tokens = list(childrenGap = 10),
          style = "margin-top:10px;",
          
          div(style = list(width = "250px",
                           height = "200px"),
              class = "card_box",
              
              div(style = "padding: 10px;font-size:16px;", 
                  tags$span(HTML("<b>Revenue<b>"))
                  ),
              
              uiOutput(ns("card_earning")),
              
              highchartOutput(ns("trend_revenue"), width = "250px", height = "100px")
              ),
          
          div(style = list(width = "250px"),
              class = "card_box",
              
              div(style = "padding: 10px;font-size:16px;", 
                  tags$span(HTML("<b>Average Daily Rate<b>"))
                  ),
              
              uiOutput(ns("card_adr")),
              highchartOutput(ns("trend_adr"), width = "250px", height = "100px")
              )
          
          ),
    
    # Third Row -------
    
    Stack(horizontal = T,
          tokens = list(childrenGap = 10),
          style = "margin-top:10px;",
          
          div(style = list(width = "250px",
                           height = "300px"),
              class = "card_box",
              
              div(style = "padding: 10px;font-size:16px;", 
                  tags$span(HTML("<b>Amenities by Listings<b>"))
                  ),
              
              highchartOutput(ns("bar_amenities"), height = "250px", width = "99%")
              ),
          
          div(style = list(width = "250px",
                           height = "300px"),
              class = "card_box",
              
              div(style = "padding: 10px;font-size:16px;", 
                  tags$span(HTML("<b>Neighbourhood by Listings<b>"))
                  ),
              
              highchartOutput(ns("bar_neighbourhood"), height = "250px", width = "99%")
              )
          
          )
    
  )
  
}




#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    return(NULL)
    
  })
  
}

