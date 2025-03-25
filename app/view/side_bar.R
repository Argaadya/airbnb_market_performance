box::use(
  shiny[div, NS, tags, HTML, moduleServer, observeEvent, br],
  shiny.fluent[Stack, DefaultButton.shinyInput, Pivot, PivotItem],
)

box::use(app/view/menu_filter,
         app/view/overview,
         app/view/page_listing,
         app/view/page_host
         )

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(id = "sidebar",
      style = "font-family: 'Segoe UI', sans-serif !important;;",
      
      # Header ---------
      div(
        style = "background-color: white !important;position: sticky;top: 0;z-index: 1000;",
        
        # Header -----
        Stack(horizontal = T, 
              horizontalAlign = "space-between", 
              verticalAlign = "center",
              tokens = list(childrenGap = 10, padding = 10),
              Stack(tags$img(src = "static/airbnb-blo.svg", height = "30px")),
              Stack(tags$span(HTML("<span style='font-size:20px; font-family:\"Segoe UI\", sans-serif;'><b>Singapore Market Performance</b></span>"))),
              Stack(horizontalAlign = "end",
                    HTML("<a href = 'https://github.com/argaadya' target ='_blank' style='text-decoration:none;'>  <img src = 'https://avatars.githubusercontent.com/u/51928527?v=4' class='round-image' style='width: 30px; height: 30px; border-radius: 50%; object-fit: cover;'> </a>"))),
        ),
      
      # Filter Menu -------
      Stack(horizontal = T, 
            tokens = list(childrenGap = 20),
            DefaultButton.shinyInput(inputId = ns("date_button"), text = tags$span(HTML("<i class='fa fa-calendar-days'></i> Period ⌄"))),
            DefaultButton.shinyInput(inputId = ns("listing_button"), text = tags$span(HTML("<i class='fa fa-hotel'></i> Listing ⌄"))),
            DefaultButton.shinyInput(inputId = ns("amenities_button"), text = tags$span(HTML("<i class='fa fa-circle-plus'></i> Amenities ⌄")))
            ),
      
      menu_filter$ui(ns("menu_filter")),
      
      # Main Content ----------
      
      Pivot(
        style = list(marginTop = 20), 
        PivotItem(headerText = tags$span(HTML("<p style='font-size:14px;'>📊 Overview<p>")),
                  overview$ui(ns("overview"))
                  ),
        PivotItem(headerText = tags$span(HTML("<p style='font-size:14px;'>🏠 Listing<p>")),
                  page_listing$ui(ns("page_listing"))
                  ),
        PivotItem(headerText = tags$span(HTML("<p style='font-size:14px;'>👨👩 Host<p>")),
                  page_host$ui(ns("page_host"))
                  )
      ),
      
      br(),
      div(tags$span(HTML("<span style='font-family:\"Segoe UI\", sans-serif;'>Data provided by <a href='https://insideairbnb.com/' target='_blank'> Inside Airbnb</a></span>"))),
      br(),
      br()
      
      )
  
}

#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
  
  selected_data <- menu_filter$server("menu_filter")
    
  page_listing$server("page_listing", selected_data)
  page_host$server("page_host", selected_data)
  overview$server("overview", selected_data)
  
  # Trigger element visibility -----
  
  observeEvent( input$date_button,  {
    session$sendCustomMessage("togglePeriodBox", list())
  })
  
  observeEvent( input$listing_button,  {
    session$sendCustomMessage("toggleListingBox", list())
  })
  
  observeEvent( input$amenities_button,  {
    session$sendCustomMessage("toggleAmenitiesBox", list())
  })
  
  
  return(selected_data)
  
  })
  
}