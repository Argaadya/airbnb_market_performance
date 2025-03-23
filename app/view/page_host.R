box::use(
  shiny[div, NS, tags, HTML, moduleServer],
  shiny.fluent[Stack, Dropdown.shinyInput],
  reactable[reactableOutput, renderReactable, reactable]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    div(style = "padding: 10px;font-size:16px;text-align: center;", 
        tags$span(HTML("<b>Top Host<b>"))
        ),
    
    Dropdown.shinyInput(ns("filter_host"),
                        label = "Sort by",
                        value = "earning",
                        multiSelect = F,
                        options = list(list(key = "n_listing", text = "Number of Listing"),
                                       list(key = "n_entire_home", text = "Number of Entire Home/Apt"),
                                       list(key = "n_private_room", text = "Number of Private Room"),
                                       list(key = "n_shared_room", text = "Number of Shared Room"),
                                       list(key = "n_hotel_room", text = "Number of Hotel Room"),
                                       list(key = "occupancy_rate", text = "Occupancy"),
                                       list(key = "earning", text = "Revenue"),
                                       list(key = "average_daily_rate", text = "ADR"),
                                       list(key = "number_review", text = "Review"),
                                       list(key = "review_scores_rating", text = "Rating")
                                       ),
                        dropdownWidth = "auto",
                        styles = list(root = list(width = 200))
                        ),
    
    reactableOutput(ns("host_table"))
    )
  
}

#' @export
server <- function(id, shared_data) {
  
  moduleServer(id, function(input, output, session) {
    
    
    
  })
  
}

