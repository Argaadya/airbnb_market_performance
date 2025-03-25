box::use(
  shiny[div, NS, tags, HTML, moduleServer, observe, req, textOutput, renderText],
  leaflet[leafletOutput, renderLeaflet, leaflet, markerClusterOptions, addTiles, addMarkers, addProviderTiles, icons],
  scales[percent, comma],
  dplyr[coalesce, case_when],
  utils[str],
  waiter[Waiter, spin_3, useWaiter]
)


box::use(
  app/view/menu_filter
  )

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(id = "main",
      
      textOutput(ns("text_map")),
      
      leafletOutput(outputId = ns("map_leaflet"), height = "100vh")
      )
  
}

#' @export
server <- function(id, shared_data){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    w_map <- Waiter$new(id = "main", html = spin_3(), color = "transparent")
    
    output$map_leaflet <- renderLeaflet({
      
      
      req(shared_data$listing_perform())
      
      w_map$show()
      
      df_listing <- shared_data$listing_perform()
      
      
      popup <- paste0("<div style='font-family:'Segoe UI', sans-serif;'>",
                      "<b> <p  style='font-size: 14px;margin-bottom: 1px;'>", df_listing$name, " </p> </b>",
                      df_listing$neighbourhood, 
                      
                      "<div style='text-align: center;'>",
                      "<img src='", df_listing$picture_url, 
                      "' class='round-image' style='width: 100%; height:125px;  border-radius: 10%; object-fit: cover;overflow:hidden'>",
                      "</div>",
                      
                      "<div style='display: flex; align-items: center; gap: 10px;'>",
                      "<a href='", df_listing$host_url, "' target='_blank' style='text-decoration: none;'>",
                      "<img src='", df_listing$host_picture_url, 
                      "' class='round-image' style='width: 30px; height: 30px; border-radius: 50%; object-fit: cover;'>",
                      "</a>",
                      "<a href='", df_listing$host_url, "' target='_blank' style='text-decoration: none;color:black;'>",
                      " <span style='font-size:12px;font-weight: bold;'> Hosted by ", df_listing$host_name, "</span>",
                      "</a>",
                      "</div>",
                      
                      "<div>",
                      comma(coalesce(df_listing$number_review, 0)), " Reviews     ",
                      
                      ifelse(!is.na(df_listing$review_scores_rating) | !is.na(df_listing$percent_positive),
                              "(", ""),
                      
                      ifelse(is.na(df_listing$review_scores_rating), '',
                              paste0(comma(df_listing$review_scores_rating, accuracy = 0.1), " <i class='fa fa-star' style='color:gold;'></i>")),
                      
                      ifelse(is.na(df_listing$percent_positive), '', 
                              paste0("     ", percent(df_listing$percent_positive, accuracy = 1), " <i class='fa fa-smile' style='color:#26A65B;'></i>)")),
                      
                      "</div>",
                      "<div>
                      <table>
                      <td>", df_listing$room_type, "</td>",
                      "<td style='color:#4B77BE'>",
                      ifelse(is.na(df_listing$accommodates), "",
                              paste0(" <i class='fa fa-person '></i> ", df_listing$accommodates)),
                      ifelse(is.na(df_listing$beds), "",
                              paste0(" <i class='fa fa-bed'></i> ", df_listing$beds)),  
                      ifelse(is.na(df_listing$bathrooms), "",
                              paste0(" <i class='fa fa-bath '></i> ", df_listing$bathrooms)),
                      "</td>",
                      "</table></div>",
                      
                      "<br>",
                      "<div>",
                      
                      
                      
                      "Occupancy Rate: <b>", percent(df_listing$occupancy_rate, accuracy = 0.1) , "</b><br>",
                      "Revenue: <b>", comma(df_listing$earning, accuracy = 1) , "</b><br>",
                      "Average Daily Rate: <b>", comma(coalesce(df_listing$average_daily_rate,0), accuracy = 1) , "</b><br>",
                      
                      "</div>",
                      
                      "<br>",
                      
                      
                      "<div style='text-align: center;'>",
                      "<a href='", df_listing$listing_url, "' target='_blank' ",
                      "style='display:inline-block; padding:10px 15px; background-color:#ff5733; ",
                      "color:white; text-decoration:none; border-radius:5px;'>",
                      "Visit</a>",
                      "</div>",
                      "</div>"
                      )
      
      room_icon <- icons(iconUrl = case_when(df_listing$room_type == "Entire home/apt" ~  "static/house.png",
                                             df_listing$room_type == "Private room" ~ "static/private.png",
                                             df_listing$room_type == "Shared room" ~ "static/shared_room.png",
                                             T ~ "static/hotel.png"),
                         iconWidth = 30, iconHeight = 30)

      p_map <- leaflet(data =  df_listing) |> 
        addTiles() |>
        addMarkers(lng = ~longitude,
                   lat = ~latitude,
                   popup = popup,
                   icon = room_icon,
                   clusterOptions = markerClusterOptions()) |>
        addProviderTiles(provider = "CartoDB.Voyager")
      
      w_map$hide()
      
      return(p_map)
      
    })
    
    
  })
    
}
