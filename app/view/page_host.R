box::use(
  shiny[div, NS, tags, HTML, moduleServer, observe, req],
  shiny.fluent[Stack, Dropdown.shinyInput],
  reactable[reactableOutput, renderReactable, reactable, colDef, reactableTheme],
  utils[head],
  dplyr[coalesce, arrange, transmute],
  waiter[Waiter, spin_3, useWaiter]
)


box::use(
  app/view/menu_filter,
  app/logic/formatter
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
    ns <- session$ns
    
    w_2 <- Waiter$new(id = ns("host_table"), html = spin_3(), color = "white")
    
    output$host_table <- renderReactable({
      
      w_2$show()
      
      req(shared_data$host_perform())
      
      df_selected <- shared_data$host_perform()
      
      filter_host_selected <- coalesce(input$filter_host, "earning")
      
      command_text <- paste0("df_selected <- df_selected |> arrange(desc(", filter_host_selected,")) |> head(100)")
      
      eval(parse(text = command_text))
      
      df_selected <- df_selected |>
        transmute(host = paste0("<div style='display: flex; align-items: center; gap: 10px;'>",
                                "<a href='", host_url, "' target='_blank' style='text-decoration: none;'>",
                                "<img src='", host_picture_url, 
                                "' class='round-image' style='width: 30px; height: 30px; border-radius: 50%; object-fit: cover;'>",
                                "</a>",
                                
                                ifelse(host_is_superhost == 1,
                                        "<img src = 'static/superhost.png' style='height:15px;'>",
                                        ""),
                                
                                "<a href='", host_url, "' target='_blank' style='text-decoration: none;color:black;'>",
                                host_name, 
                                "</a>",
                                "</div>"
                                ),
                  n_listing,
                  n_entire_home,
                  n_private_room,
                  n_shared_room,
                  n_hotel_room,
                  occupancy_rate,
                  earning,
                  average_daily_rate,
                  number_review = coalesce(number_review, 0),
                  rating = coalesce(review_scores_rating, 0),
                  positive_sentiment = coalesce(percent_positive, 0)
                  ) 
      
      reactable(data = df_selected,
                defaultPageSize = 10,
                theme = reactableTheme(style = list(fontFamily = "Segoe UI, sans-serif")),
                highlight = T,
                striped = T,
                pagination = T,
                showPageSizeOptions = T,
                pageSizeOptions = c(5, 10, 25, 50), 
                sortable = T, 
                resizable = T,
                
                columns = list(
                  host = colDef(name = "Host",
                                vAlign = "center",
                                html = T,
                                sticky = "left",
                                minWidth = 200),
                  
                  n_listing = colDef(name = "Listing", align = "center", vAlign = "center"),
                  n_entire_home = colDef(name = "Entire Home/Apt", 
                                         align = "center", 
                                         vAlign = "center",
                                         cell = function(value) {formatter$zero_to_strip(value)}),
                  n_private_room = colDef(name = "Private Room", 
                                          align = "center", 
                                          vAlign = "center",
                                          cell = function(value) {formatter$zero_to_strip(value)}),
                  n_shared_room = colDef(name = "Shared Room", 
                                         align = "center", 
                                         vAlign = "center",
                                         cell = function(value) {formatter$zero_to_strip(value)}),
                  n_hotel_room = colDef(name = "Hotel Room", 
                                        align = "center", 
                                        vAlign = "center",
                                        cell = function(value) {formatter$zero_to_strip(value)}),
                  
                  occupancy_rate = colDef(name = "Occupancy",
                                          align = "center",
                                          vAlign = "center",
                                          html = T,
                                          cell = function(value) {formatter$html_bar_chart(value)}),
                  
                  earning = colDef(name = "Revenue",
                                   align = "center",
                                   vAlign = "center",
                                   cell = function(value){formatter$number_convert_no_curr(value)}),
                  
                  average_daily_rate = colDef(name = "ADR",
                                              align = "center",
                                              vAlign = "center",
                                              cell = function(value) {formatter$number_convert_no_curr( coalesce(value, 0) )}),
                  
                  number_review = colDef(name = "Reviews",
                                         align = "center",
                                         vAlign = "center",
                                         html = T,
                                         cell = function(value) { ifelse(value == 0, "-", scales::comma(value)) }),
                  
                  rating = colDef(name = "Average Rating",
                                  align = "center",
                                  vAlign = "center",
                                  html = T,
                                  cell = function(value) {
                                    
                                    ifelse(value == 0, '-',
                                            paste0(scales::comma(value, accuracy = 0.1), " <i class='fa fa-star' style='color:gold;'></i>")) 
                                    
                                  }),
                  
                  positive_sentiment = colDef(name = "Sentiment",
                                              align = "center",
                                              vAlign = "center",
                                              html = T,
                                              cell = function(value) {
                                                
                                                ifelse(value == 0, '-', 
                                                        paste0(scales::percent(value, accuracy = 1), " <i class='fa fa-smile' style='color:#26A65B;'></i>"))
                                                
                                              })
                  )
                )
      
      
      
    })
    
  })
  
}

