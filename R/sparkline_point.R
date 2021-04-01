#' sparkline_point
#'
#' This funtion is
#'
#' @param
#'
#' @examples
#'
#' x <- 1:3
#' y <- 1:3
#' color <- c("red", "blue", "gold")
#' sparkline_point(x = x, y = y, color = color, height = 30)
#'
#' @export
sparkline_point <- function(
                            x,
                            y,
                            color = NULL,
                            height = 30,
                            hover_text = NULL,
                            xlim = NULL,
                            vline = NULL,
                            margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                            ...){

  p <- plot_ly(
               x = x,
               y = y,
               color = color,
               colors = color,
               text = hover_text,
               hoverinfo = 'text',
               type = 'scatter',
               mode = 'markers',
               height = height)

  p %>% sparkline_layout(xlim = xlim,
                         vline = vline,
                         margin = margin,
                         ...)

}
