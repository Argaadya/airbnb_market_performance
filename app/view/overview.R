box::use(
  shiny[div, NS, tags, HTML, moduleServer, uiOutput, renderUI, br, req],
  shiny.fluent[Stack, Text],
  highcharter[highchart, highchartOutput, renderHighchart, hc_chart, hc_plotOptions, hc_xAxis, hc_yAxis, hc_add_series, hc_tooltip, hc_legend, hcaes],
  waiter[Waiter, spin_3, useWaiter],
  dplyr[summarise, mutate, select, filter, pull, n_distinct, group_by, arrange, desc],
  tidyr[pivot_longer],
  stringr[str_to_title],
  htmlwidgets[JS],
  scales[comma],
  stringi[stri_unescape_unicode],
  utils[head]
)

box::use(app/logic/formatter)


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
server <- function(id, shared_data) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Active Listing --------------
    w_listing <- Waiter$new(id = ns("card_listing"), html = spin_3(), color = "white")
    
    output$card_listing <- renderUI({
      
      w_listing$show()
      
      active_listing_text <- shared_data$listing_perform()$id |>
        n_distinct() |>
        comma()
      
      Stack(horizontal = T,
            tokens = list(childrenGap = 5),
            
            div(style = "padding:5px;text-align:center",
                Text(variant = "xxLarge", active_listing_text)),
            
            div(style = "padding:5px;font-size:16px;margin-top:-20px;",
                highchartOutput(ns("bar_listing_type"), height = "180px", width = "180px")))
      
    })
    
    output$bar_listing_type <- renderHighchart({
      
      active_listing_room <- shared_data$listing_perform() |>
        mutate(room_type = str_to_title(room_type)) |> 
        group_by(room_type) |>
        summarise(n_id = n_distinct(id)) |>
        arrange(desc(n_id)) 
      
      highchart() |>
        hc_chart(type = "bar") |>
        hc_xAxis(type = "category", gridLineWidth = 0) |>
        hc_yAxis(gridLineWidth = 0, labels = list(enabled = FALSE)) |>
        hc_add_series(
          data = active_listing_room,
          type = "bar",
          hcaes(name = room_type, y = n_id),
          colorByPoint = T
        ) |>
        hc_tooltip(
          formatter = JS("function() {
      return this.point.name + '<br>' + '<b>' + Highcharts.numberFormat(this.y, 0, '.', ',') + '</b>';
                     }")
        ) |>
        hc_legend(enabled = FALSE)
      
    })  
    
    # Occupancy ------
    w_occupancy <- Waiter$new(id = ns("card_occupancy"), html = spin_3(), color = "white")
    
    output$card_occupancy <- renderUI({
      
      w_occupancy$show()
      
      overall_occupancy <- shared_data$listing_perform() |>
        summarise(available = sum(available),
                  total_day = sum(n_day),
                  occupied = total_day - available
                  ) |>
        select(-total_day)
      
      overall_occupancy <- pivot_longer(overall_occupancy, cols = names(overall_occupancy)) |>
        mutate(value = round(value/sum(value) * 100, 1),
               name = str_to_title(name)
               )
      
      occupancy_text <- overall_occupancy |>
        filter(name == "Occupied") |>
        pull(value) |>
        scales::comma(accuracy = 0.1, suffix = "%")
      
      div(style = "padding:5px;text-align:center;",
          Text(variant = "xxLarge", occupancy_text)
          )
      
    })  
    
    output$trend_occupancy <- renderHighchart({
      
      df_trend <- shared_data$trend_perform() |>
        mutate(date_js = as.numeric(as.POSIXct(date_period, tz = "UTC")) * 1000)
      
      highchart() |> 
        hc_chart(type = "line") |> 
        hc_plotOptions(line = list(lineWidth = 2)) |>
        hc_xAxis(type = "datetime", gridLineWidth = 0
        ) |> 
        hc_yAxis(gridLineWidth = 0, labels = list(format = "{value}%")) |> 
        hc_add_series(
          data = df_trend,
          type = "line",
          color = "#f6595f",
          marker = list(radius = 1), 
          hcaes(x = date_js, y = occupancy_rate * 100)
        ) |> 
        hc_tooltip(
          formatter = JS("function() {
      return Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>' + Highcharts.numberFormat(this.y, 1, '.', ',') + '%</b>';
                     }")
        ) |> 
        hc_legend(enabled = FALSE)
      
    })
    
    # Revenue ----------
    w_earning <- Waiter$new(id = ns("card_earning"), html = spin_3(), color = "white")
    
    output$card_earning <- renderUI({
      
      w_earning$show()
      
      overall_earning <- shared_data$listing_perform() |>
        summarise(earning = sum(earning) |> 
                    formatter$number_convert()
                  ) 
      
      div(style = "padding:5px;text-align:center;",
          Text(variant = "xxLarge", overall_earning$earning))
      
    })
    
    output$trend_revenue <- renderHighchart({
      
      df_trend <- shared_data$trend_perform() |> 
        mutate(date_js = as.numeric(as.POSIXct(date_period, tz = "UTC")) * 1000)
      
      highchart() |> 
        hc_chart(type = "line") |> 
        hc_plotOptions(line = list(lineWidth = 2)) |>
        hc_xAxis(type = "datetime", gridLineWidth = 0) |> 
        hc_yAxis(gridLineWidth = 0, 
                 formatter = JS("function() { return Highcharts.numberFormat(this.value, 0, '.', ',') + '%'; }")) |> 
        hc_add_series(
          data = df_trend,
          type = "line",
          color = "#f6595f",
          marker = list(radius = 1), 
          hcaes(x = date_js, y = earning)) |> 
        hc_tooltip(
          formatter = JS("function() {
      return Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>' + Highcharts.numberFormat(this.y, 1, '.', ',') + '</b>';
                     }")) |> 
        hc_legend(enabled = FALSE)
      
    })  
    
    # ADR -----------
    w_adr <- Waiter$new(id = ns("card_adr"), html = spin_3(), color = "white")
    
    output$card_adr <- renderUI({
      
      w_adr$show()
      
      overall_earning <- shared_data$listing_perform() |>
        summarise(earning = sum(earning),
                  available = sum(available),
                  total_day = sum(n_day),
                  occupied = total_day - available,
                  adr = earning/ ( total_day - available)
                  ) |> 
        mutate(adr = formatter$number_convert(adr))
      
      div(style = "padding:5px;text-align:center;",
          Text(variant = "xxLarge", overall_earning$adr)
          )
      
    })
    
    
    output$trend_adr <- renderHighchart({
      
      df_trend <- shared_data$trend_perform() |>
        mutate(date_js = as.numeric(as.POSIXct(date_period, tz = "UTC")) * 1000)
      
      highchart() |> 
        hc_chart(type = "line") |> 
        hc_plotOptions(line = list(lineWidth = 2)) |>
        hc_xAxis(type = "datetime", gridLineWidth = 0) |> 
        hc_yAxis(gridLineWidth = 0, 
                 formatter = JS("function() { return Highcharts.numberFormat(this.value, 0, '.', ',') + '%'; }")) |> 
        hc_add_series(
          data = df_trend,
          type = "line",
          color = "#f6595f",
          marker = list(radius = 1), 
          hcaes(x = date_js, y = average_daily_rate)) |> 
        hc_tooltip(
          formatter = JS("function() {
      return Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>' + Highcharts.numberFormat(this.y, 1, '.', ',') + '</b>';
                     }")) |> 
        hc_legend(enabled = FALSE)
      
    })
    
    # Amenities ------------
    output$bar_amenities <- renderHighchart({
      
      total_listing <- nrow(shared_data$listing_perform())
      
      top_amenities <- shared_data$amenities() |>
        head(15) |>
        mutate(percent_listing = n_listing/total_listing,
               amenities = stri_unescape_unicode(amenities)
               )
      
      highchart() |>
        hc_chart(type = "bar") |>
        hc_xAxis(type = "category", gridLineWidth = 0) |>
        hc_yAxis(gridLineWidth = 0, labels = list(enabled = FALSE)) |>
        hc_add_series(
          data = top_amenities,
          type = "bar",
          hcaes(name = amenities, y = n_listing, percent_listing = percent_listing),
          color = "#f6595f"
        ) |>
        hc_tooltip(
          formatter = JS("function() {
      return '<b>' + this.point.name + '</b><br>' +
              '<b>' + Highcharts.numberFormat(this.y, 0, '.', ',') + '</b> listings' + '<br>' +
              Highcharts.numberFormat(this.point.percent_listing * 100, 1, '.', ',') + '%'
              ;
                     }")
        ) |>
        hc_legend(enabled = FALSE)
      
    })  
    
    # Neighbourhood ------------
    output$bar_neighbourhood <- renderHighchart({
      
      total_listing <- nrow(shared_data$listing_perform())
      
      top_neighbour <- shared_data$listing_perform() |> 
        group_by(neighbourhood) |> 
        summarise(n_listing = n_distinct(id),
                  percent_listing = n_listing/total_listing,
                  available = sum(available),
                  n_day = sum(n_day),
                  occupancy_rate = (1 - (available/n_day)) * 100
                  ) |> 
        arrange(desc(n_listing)) |> 
        head(15)
      
      highchart() |>
        hc_chart(type = "bar") |>
        hc_xAxis(type = "category", gridLineWidth = 0) |> 
        hc_yAxis(gridLineWidth = 0, labels = list(enabled = FALSE)) |> 
        hc_add_series(
          data = top_neighbour,
          type = "bar",
          hcaes(name = neighbourhood, y = n_listing, percent_listing = percent_listing, occupant = occupancy_rate),
          color = "#f6595f") |>
        hc_tooltip(
          formatter = JS("function() {
      return '<b>' + this.point.name + '</b><br>' +
              '<b>' + Highcharts.numberFormat(this.y, 0, '.', ',') + 
              '</b> (' + Highcharts.numberFormat(this.point.percent_listing * 100, 1, '.', ',') + '%'  + ') listings' + '<br>' +
              
              Highcharts.numberFormat(this.point.occupant, 1, '.', ',') + '% occupancy rate' 
              
              ;
                     }")
        ) |> 
        hc_legend(enabled = FALSE)
      
    })    
    
    
    
  
    
  })
  
}

