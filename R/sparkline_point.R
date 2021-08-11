#' sparkline_point
#'
#' This function is drawing the scatters on the sparkline plot
#'
#'
#' @param x a data frame corresponding location of scatters on x-axis
#' @param y a data frame corresponding location of scatters on y-axis
#' @param color A character string define the color of different scatters.
#' @param height Numeric value of figure height.
#' @param width Numeric value of figure width.
#' @param hover_text A character string describe the information of each scatters
#' @param xlim A data frame define the range of x-axis on the plot
#' @param vline A data frame indicates where to draw the vertical lines on the plot
#' @param margin A data define the size of the plot
#' @param ... Additional arguments transfer to `sparkline_layout`
#' @examples
#' library(plotly)
#' x <- 1:5
#' y <- 1:5
#' color <- c("red", "blue", "gold", "grey", "black")
#' sparkline_point(
#'   x = x, y = y, color = color, height = 500, vline = 1.5,
#'   hover_text = c("a", "b", "c", "d", "e")
#' )
#' @export
sparkline_point <- function(x,
                            y,
                            color = NULL,
                            height = 30,
                            width = 150,
                            hover_text = NULL,
                            xlim = NULL,
                            vline = NULL,
                            margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                            ...) {
  p <- plot_ly(
    x = x,
    y = y,
    color = color,
    colors = color,
    text = hover_text,
    hoverinfo = "text",
    type = "scatter",
    mode = "markers",
    height = height,
    width = width
  )

  p %>% sparkline_layout(
    xlim = xlim,
    vline = vline,
    margin = margin,
    ...
  )
}
