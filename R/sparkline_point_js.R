#' Sparkline Point Figure Javascript Render Using Plotly
#'
#' @param tbl A data frame
#' @param x A vector of variable names in `tbl` for value.
#' @param x_lower A vector of variable names in `tbl` for lower error difference.
#' @param x_upper A vector of variable names in `tbl` for upper error difference. Default is the same as `x_lower`.
#' @param xlim Numeric vectors of length 2, giving the x coordinates ranges.
#' @param y Numeric vector of y-axis value.
#' @param vline Numeric value for the location to draw the vertical reference line.
#' @param text Character vector of text information dispalted for each point.
#' @param height Numeric value of figure height.
#' @param width Numeric value of figure width.
#' @param color Character vector of point color name
#' @param color_errorbar Character vector of errorbar color name
#' @param color_vline Character vector of vertical reference line color name
#' @examples
#' \dontrun{
#' library(reactable)
#' library(htmltools)
#'
#' js <- sparkline_point_js(iris, "Sepal.Length")
#' js <- sparkline_point_js(iris, "Sepal.Length", text = "x")
#' js <- sparkline_point_js(iris, "Sepal.Length", text = "'count:' + x")
#' js <- sparkline_point_js(iris, "Sepal.Length", x_lower = "Sepal.Width", 
#'                          text = "'range:' + x + '(' + x_lower + ',' + x_upper + ')'" )
#' js <- sparkline_point_js(iris, "Sepal.Length", x_lower = "Sepal.Width")
#' js <- sparkline_point_js(iris, "Sepal.Length", vline = 6)
#' cat(js)
#' 
#' col <- colDef(html = TRUE, cell = JS(js), width = 150,
#'               style="font-size: 0px; padding: 0px; margin: 0px;"))
#' p <- reactable(data = iris[,1:2],
#'                theme = reactableTheme(cellPadding = "0px 8px"),
#'                borderless = TRUE,
#'                highlight = TRUE,
#'                resizable = TRUE,
#'                columns = list(Sepal.Length = col)
#'                
#' plotly_js <- "https://unpkg.com/react-plotly.js@1.0.2/dist/create-plotly-component.js"
#' browsable(tagList(
#'   reactR::html_dependency_react(),
#'   htmltools::tags$script(src="https://cdn.plot.ly/plotly-latest.min.js"),
#'   htmltools::tags$script(src=plotly_js),
#'   p
#' ))
#' }
#' 
#' @export
sparkline_point_js <- function(tbl,
                               x,
                               x_lower = NULL,
                               x_upper = x_lower,
                               xlim = NULL,
                               y = seq_along(x),
                               vline = NULL,
                               text = NULL,
                               height = 30,
                               width = 150,
                               color = "gold",
                               color_errorbar = color,
                               color_vline = "#00000050"){

  # Input Checking
  stopifnot(x %in% names(tbl))

  if(! is.null(x_lower)){
    stopifnot(x_lower %in% names(tbl))
    stopifnot(x_upper %in% names(tbl))
  }

  stopifnot(length(xlim) %in% c(0, 2))

  stopifnot(length(vline) <= 1)
  stopifnot(length(height) == 1)
  stopifnot(length(width) == 1)

  stopifnot(length(color) %in% c(1, length(x)) )
  stopifnot(length(color_errorbar) %in% c(1, length(x)) )
  stopifnot(length(color_vline) %in% 1 )

  # Convert x and error bar
  # x_v <- tbl[[x]]
  x_v <- 0
  js_x <- paste(paste0("cell.row['", x, "']"), collapse = ",")

  if(is.null(x_lower)){
    js_x_lower <- 0
    js_x_upper <- 0
    x_l <- x_v
    x_u <- x_v
    color_errorbar <- "#FFFFFF00"
  }else{
    x_l <- x_v - tbl[[x_lower]]
    x_u <- x_v + tbl[[x_upper]]
    js_x_lower <- paste(paste0("cell.row['", x_lower, "']"), collapse = ",")
    js_x_upper <- paste(paste0("cell.row['", x_upper, "']"), collapse = ",")
  }

  # Convert y
  js_y <- paste(y, collapse = ",")

  # Convert axis range
  if(is.null(xlim)){
    xlim <- range(c(x_v, x_l, x_u)) + c(-0.5, 0.5)
  }
  js_x_range <- paste(xlim, collapse = ",")
  js_y_range <- paste(c(0, length(x) + 1), collapse = ",")

  # Convert text
  if(is.null(text)){
    js_text <- '""'
  }else{
    js_text <- paste(text, collapse = ",")
  }

  # Convert v_line
  if(is.null(vline)) vline <- "[]"
  js_vline <- as.character(vline)

  # Convert shape
  js_height <- as.character(height)
  js_width <- as.character(width)

  # Convert color
  foo <- function(x){
    rgba <- grDevices::col2rgb(x, alpha = TRUE)
    rgba[4, ] <- rgba[4, ] / 255
    paste( paste0("'rgba(", apply( rgba, 2, paste, collapse = "," ), ")'"), collapse = ",")
  }
  js_color <- foo(color)
  js_color_errorbar <- foo(color_errorbar)
  js_color_vline <- foo(color_vline)

  # Brew

  plotly_file <- tempfile(fileext = ".js")
  brew::brew(system.file("javascripts/sparkline.js", package = "forestly"), output = plotly_file)

  js <- paste(readLines(plotly_file), collapse = "\n")

  js

}

