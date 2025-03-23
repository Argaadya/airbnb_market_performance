box::use(
  shiny[div, NS, tags, HTML, moduleServer, observe, req],
  shiny.fluent[Stack, Dropdown.shinyInput],
  reactable[reactableOutput, renderReactable, reactable, colDef, reactableTheme],
  utils[head],
  dplyr[coalesce, arrange, transmute],
  waiter[Waiter, spin_3, useWaiter, waiter_show_on_load]
)


box::use(
  app/view/menu_filter,
  app/logic/formatter
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    
    useWaiter(),
    
    div(style = "padding: 10px;font-size:16px;text-align: center;", 
        tags$span(HTML("<b>Top Listing<b>"))),
    
    Dropdown.shinyInput(ns("filter_listing"),
                        label = "Sort by",
                        multiSelect = F,
                        value = "earning",
                        options = list(list(key = "occupancy_rate", text = "Occupancy"),
                                       list(key = "earning", text = "Revenue"),
                                       list(key = "average_daily_rate", text = "ADR"),
                                       list(key = "number_review", text = "Review"),
                                       list(key = "review_scores_rating", text = "Rating")
                                       ),
                        dropdownWidth = "auto",
                        styles = list(root = list(width = 200))
                        ),
    
    reactableOutput(ns("listing_table"))
    
      )
  
}

#' @export
server <- function(id, shared_data) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    w_1 <- Waiter$new(id = ns("listing_table"), html = spin_3(), color = "white")
    
    # waiter_show_on_load(html = spin_3(), color = "white")
    
    output$listing_table <- renderReactable({
      
      w_1$show()
      
      req(shared_data$listing_perform())
      
      df_selected <- shared_data$listing_perform()
      
      filter_listing_selected <- coalesce(input$filter_listing, "earning")
      
      command_text <- paste0("df_selected <- df_selected |> arrange(desc(", filter_listing_selected,")) |> head(100)")
      
      eval(parse(text = command_text))
      
      df_selected <- df_selected |>
        transmute(name = paste0("<div style='display: flex; align-items: center; gap: 10px;'>",
                                "<a href='", listing_url, "'>",
                                "<img src = '", picture_url, "'  class='round-image' style='width: 30px; height: 30px; border-radius: 50%; object-fit: cover;'> </a>",
                                "<a href='", listing_url, "' target='_blank'  style='text-decoration:none;color:black;'>", name, "</a>",
                                "</div>"
                                ),
                  room_type,
                  neighbourhood,
                  host = paste0("<div style='display: flex; align-items: center; gap: 10px;'>",
                                "<a href='", host_url, "' target='_blank' style='text-decoration: none;'>",
                                "<img src='", host_picture_url, 
                                "' class='round-image' style='width: 30px; height: 30px; border-radius: 50%; object-fit: cover;'>",
                                "</a>",
                                "<a href='", host_url, "' target='_blank' style='text-decoration: none;color:black;'>",
                                host_name, 
                                "</a>",
                                "</div>"
                                ),
                  occupancy_rate,
                  earning,
                  average_daily_rate,
                  accommodates,
                  beds = coalesce(beds, 0),
                  bathrooms = coalesce(bathrooms, 0),
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
                  name = colDef(name = "Listing", 
                                vAlign = "center",
                                headerStyle = list(align = "center", textAlign = "center"),
                                html = T,
                                minWidth = 300,
                                sticky = "left"),
                  room_type = colDef(name = "Room Type",
                                     align = "center",
                                     vAlign = "center"),
                  neighbourhood = colDef(name = "Neighbourhood",
                                         align = "center",
                                         vAlign = "center"),
                  host = colDef(name = "Host",
                                vAlign = "center",
                                html = T),
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
                                              cell = function(value) { formatter$number_convert_no_curr( coalesce(value, 0) )}),
                  
                  accommodates = colDef(name = HTML(paste0("<i class='fa fa-person '></i> Guests")),
                                        align = "center",
                                        vAlign = "center",
                                        html = T),
                  
                  beds = colDef(name = HTML(paste0("<i class='fa fa-bed '></i> Beds")),
                                align = "center",
                                vAlign = "center",
                                html = T,
                                cell = function(value) {formatter$zero_to_strip(value)}),
                  
                  bathrooms = colDef(name = HTML(paste0("<i class='fa fa-bath '></i> Bathrooms")),
                                     align = "center",
                                     vAlign = "center",
                                     html = T,
                                     cell = function(value) {formatter$zero_to_strip(value)}),                  
                  
                  number_review = colDef(name = "Reviews",
                                         align = "center",
                                         vAlign = "center",
                                         html = T,
                                         cell = function(value) {formatter$zero_to_strip(value)}),
                  
                  rating = colDef(name = "Rating",
                                  align = "center",
                                  vAlign = "center",
                                  html = T,
                                  cell = function(value) {
                                    
                                    ifelse(value == 0, '-',
                                            paste0(scales::comma(value, accuracy = 0.1), " <i class='fa fa-star' style='color:gold;'></i>")) 
                                    }
                                  ),
                  
                  positive_sentiment = colDef(name = "Sentiment",
                                              align = "center",
                                              vAlign = "center",
                                              html = T,
                                              cell = function(value) {
                                                
                                                ifelse(value == 0, '-', 
                                                        paste0(scales::percent(value, accuracy = 1), " <i class='fa fa-smile' style='color:#26A65B;'></i>"))
                                                
                                                }
                                              )
                  
                  )
                
                )
      
    })
    
    
  })
  
}

