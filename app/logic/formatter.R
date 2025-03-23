
box::use(
  scales[comma, percent],
  dplyr[case_when],
  shiny[div],
  shiny
)

#' @export
zero_to_strip <- function(x) { ifelse(x == 0, "-", comma(x)) }

#' @export
number_convert <- function(x) {
  
  case_when(
    x >= 1e9 ~ comma(x, accuracy = 0.1, scale = 1/1e9, suffix = "B", prefix = "SGD "),
    x >= 1e6 ~ comma(x, accuracy = 0.1, scale = 1/1e6, suffix = "M", prefix = "SGD "),
    x >= 1e3 ~ comma(x, accuracy = 0.1, scale = 1/1e3, suffix = "K", prefix = "SGD "),
    T ~ comma(x, prefix = "SGD ", accuracy = 0.1))
  
}

#' @export
number_convert_no_curr <- function(x) {
  
  case_when(
    x >= 1e9 ~ comma(x, accuracy = 0.1, scale = 1/1e9, suffix = "B"),
    x >= 1e6 ~ comma(x, accuracy = 0.1, scale = 1/1e6, suffix = "M"),
    x >= 1e3 ~ comma(x, accuracy = 0.1, scale = 1/1e3, suffix = "K"),
    T ~ comma(x, accuracy = 0.1))
  
}

#' @export
html_bar_chart <- function(label, height = "16px", fill = "#f6595f", background = "#E1E1E1") {
  
  bar_width <- paste0(label * 100, "%")
  
  bar <- div(style = list(background = fill, width = bar_width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  
  label_text <- ifelse(label == 1, 
                       percent(label, accuracy = 1),
                       percent(label, accuracy = 0.1))
  
  div(style = list(display = "flex", alignItems = "center", fontSize = "12px"), 
      div(chart, style = "flex: 1"),
      div(paste0(" ", label_text), style = "flex: 1"))
  
}



