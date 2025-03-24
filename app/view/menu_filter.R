box::use(
  shiny[div, NS, moduleServer, observeEvent, tags, HTML, reactive, debounce, req, observe],
  shiny.fluent[Stack, DatePicker.shinyInput, Toggle.shinyInput, Text, Dropdown.shinyInput],
  shinyWidgets[noUiSliderInput, pickerInput, pickerOptions, wNumbFormat],
  htmlwidgets[JS],
  utils[head, str],
  dplyr[coalesce]
)

box::use(app/logic/input_data)
box::use(app/logic/fetch_data)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    
    # Filter Period -------
    div(id = "listing_date",  
        style = "display:none;",
        
        div(Text(variant = "large", "Select Reporting Period"), style = "margin-top: 10px" ),
        
        Stack(horizontal = T, 
              verticalAlign = "end",
              tokens = list(childrenGap = 20),
              
              DatePicker.shinyInput(inputId = ns("start_date"), 
                                    label = "Start Date", 
                                    value = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                                    showGoToToday = F,
                                    formatDate = JS("App.uiDateConvert"),
                                    styles = list(root = list(width = 200))),
              DatePicker.shinyInput(inputId = ns("end_date"), 
                                    label = "End Date", 
                                    value = as.POSIXct("2025-06-30 00:00:00", tz = "UTC"),
                                    showGoToToday = F,
                                    formatDate = JS("App.uiDateConvert"),
                                    styles = list(root = list(width = 200)))
              ),
        
        tags$hr(style = "margin-top: 10px; border: 1px solid #ccc; width: 100%;")
        ),
    
    # Filter Listing ------
    div(id = "listing_box", style = "display:none;",
        
        div(Text(variant = "large", "Filter Listing"), style = "margin-top: 10px" ),
        
        Stack(horizontal = T,
              tokens = list(childrenGap = 10),
              div(class = "card_box",
                  Dropdown.shinyInput(ns("drop_property_type"), 
                                      label = tags$span(HTML("<b>Room Type</b>")), 
                                      multiSelect = T,
                                      options = input_data$in_room(),
                                      styles = list(root = list(width = 150))
                                      )
                  ),
              div(class = "card_box",
                  Dropdown.shinyInput(ns("drop_neighbour"), 
                                      label = tags$span(HTML("<b>Neighbourhood</b>")), 
                                      multiSelect = T,
                                      options = input_data$in_neighbour(),
                                      dropdownWidth = "auto",
                                      styles = list(root = list(width = 150)))
                  ),
              div(class = "card_box",
                  Stack(horizontal = T,
                        tokens = list(childrenGap = 10),
                        Toggle.shinyInput(inputId = ns("toggle_superhost"), 
                                          label = tags$span(HTML("<b>Superhost</b>")), 
                                          defaultChecked = F),
                        Toggle.shinyInput(inputId = ns("toggle_instant"), 
                                          label = tags$span(HTML("<b>Instant</b>")),
                                          defaultChecked = F)
                        )
                  )
              ),
        
        div(class = "card_box", style = "margin-top:10px;",
            div(style = "display: flex;",
                div(style = "flex: 1",
                    noUiSliderInput(ns("accomm"), 
                                    label = tags$span(HTML("<b style='font-size:15px;'><i class='fa fa-person'></i> Accommodates</b>")),
                                    min = 1,
                                    max = input_data$in_number()$max_acommodate, 
                                    value = c(1, input_data$in_number()$max_acommodate),
                                    step = 1, 
                                    format = wNumbFormat(decimals = 0),
                                    color = "#f6595f", 
                                    height = "10px", width = "150px")),
                div(style = "flex: 1",
                    noUiSliderInput(ns("beds"), 
                                    label = tags$span(HTML("<b style='font-size:15px;'><i class='fa fa-bed'></i> Beds</b>")),
                                    min = 0,
                                    max = input_data$in_number()$max_bed, 
                                    value = c(0, input_data$in_number()$max_bed),
                                    step = 1, 
                                    format = wNumbFormat(decimals = 0),
                                    color = "#f6595f", 
                                    height = "10px", width = "150px")),
                div(style = "flex: 1",
                    noUiSliderInput(ns("bathroom"), 
                                    label = tags$span(HTML("<b style='font-size:15px;'><i class='fa fa-bath'></i> Bathroom</b>")),
                                    min = 0,
                                    max = input_data$in_number()$max_bathroom, 
                                    value = c(0, input_data$in_number()$max_bathroom),
                                    step = 1, 
                                    format = wNumbFormat(decimals = 0),
                                    color = "#f6595f", 
                                    height = "10px", width = "150px"))
                )    
            ),
        
        tags$hr(style = "margin-top: 10px; border: 1px solid #ccc; width: 100%;")
        ),
    
    # Filter Amenities ----------
    div(id = "listing_amenities", style = "display:none;",
        
        div(class = "card_box", style = "margin-top:10px;width:400px;",
            
            pickerInput(
              inputId = ns("picker_amenities"), 
              label = tags$span(HTML("<b style='font-size:15px;'><i class='fa fa-circle-plus'></i> Amenities Provided</b>")), 
              choices = input_data$in_amenities(), 
              width = "100%",
              options = pickerOptions(
                actionsBox = TRUE, 
                size = 10,
                liveSearch = T
              ), 
              multiple = TRUE,
            )
            
        ),
        
        tags$hr(style = "margin-top: 10px; border: 1px solid #ccc; width: 100%;")
        
    )
    
  )
  
  
}


#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
  # Reactive --------
  
  # Fix Timezone issue
  param_start_date <- reactive({
    
    input$start_date |>
      as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M:%S") |>
      format(tz = Sys.timezone(), usetz = T) |>
      strftime(format = "%Y-%m-%d")
    
  })
  
  param_end_date <- reactive({
    
    input$end_date  |>
      as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M:%S") |>
      format(tz = Sys.timezone(), usetz = T) |>
      strftime(format = "%Y-%m-%d")
    
  })
  
  # Listing Performance -----
  listing_perform <- debounce(
    
    reactive({
      req(input$start_date)
      
      suppressWarnings({
        start_date <- min(param_start_date(), param_end_date())
        end_date <- max(param_start_date(), param_end_date())  
      })
      
      p_out <- fetch_data$listing_performance(start_date = start_date, 
                                              end_date = end_date, 
                                              neighbourhood = input$drop_neighbour, 
                                              room = input$drop_property_type,
                                              accom_num = input$accomm,
                                              bed_num = input$beds,
                                              bath_num = input$bathroom,
                                              amenities_provided = input$picker_amenities,
                                              is_superhost = coalesce(input$toggle_superhost, F),
                                              is_instant = coalesce(input$toggle_instant, F))
      
      
      return(p_out)
    }
    ), millis = 500)
  
  # Host Performance -----
  host_perform <- debounce(
    
    reactive({
      req(input$start_date)
      
      suppressWarnings({
        start_date <- min(param_start_date(), param_end_date())
        end_date <- max(param_start_date(), param_end_date())  
      })
      
      p_out <- fetch_data$host_performance(start_date = start_date, 
                                           end_date = end_date, 
                                           neighbourhood = input$drop_neighbour, 
                                           room = input$drop_property_type,
                                           accom_num = input$accomm,
                                           bed_num = input$beds,
                                           bath_num = input$bathroom,
                                           amenities_provided = input$picker_amenities,
                                           is_superhost = coalesce(input$toggle_superhost, F),
                                           is_instant = coalesce(input$toggle_instant, F))
      
      return(p_out)
    }
    ), millis = 500)
  
  
  # Trend Performance -----
  trend_perform <- debounce(
    
    reactive({
      req(input$start_date)
      
      suppressWarnings({
        start_date <- min(param_start_date(), param_end_date())
        end_date <- max(param_start_date(), param_end_date())  
      })
      
      p_out <- fetch_data$trend_performance(start_date = start_date, 
                                           end_date = end_date, 
                                           neighbourhood = input$drop_neighbour, 
                                           room = input$drop_property_type,
                                           accom_num = input$accomm,
                                           bed_num = input$beds,
                                           bath_num = input$bathroom,
                                           amenities_provided = input$picker_amenities,
                                           is_superhost = coalesce(input$toggle_superhost, F),
                                           is_instant = coalesce(input$toggle_instant, F))
      
      return(p_out)
    }
    ), millis = 500)
  
  
  # Amenities Performance -----
  amenities_perform <- debounce(
    
    reactive({
      req(input$start_date)
      
      suppressWarnings({
        start_date <- min(param_start_date(), param_end_date())
        end_date <- max(param_start_date(), param_end_date())  
      })
      
      p_out <- fetch_data$get_amenities(start_date = start_date, 
                                        end_date = end_date, 
                                        neighbourhood = input$drop_neighbour, 
                                        room = input$drop_property_type,
                                        accom_num = input$accomm,
                                        bed_num = input$beds,
                                        bath_num = input$bathroom,
                                        amenities_provided = input$picker_amenities,
                                        is_superhost = coalesce(input$toggle_superhost, F),
                                        is_instant = coalesce(input$toggle_instant, F))
      
      return(p_out)
    }
    ), millis = 500)
  
  
  return(list(listing_perform = listing_perform, host_perform = host_perform, trend_perform = trend_perform, amenities = amenities_perform))
})
  
}