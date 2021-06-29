#' add_vline
#'
#' This function is a internal function drawing the vertical lines on the plot.
#'
#'
#' @param p A parameter indicate which plot to draw the vertical line
#' @param x A number indicate where to add vertical line on the x_axis
#' @examples
#' library(plotly)
#' p <- plot_ly(cars, x = ~speed, y = ~dist)
#' add_vline = function(p, x, ...) {
#'   l_shape = list(
#'     type = "line",
#'     y0 = 0, y1 = 1, yref = "paper",
#'     x0 = x, x1 = x,
#'     line = list(...)
#'  )
#'  p %>% layout(shapes=list(l_shape))
#' }
#' add_vline(p, x = 7.5)
#' @export
add_vline = function(p, x, ...) {
  l_shape = list(
    type = "line",
    y0 = 0, y1 = 1, yref = "paper",
    x0 = x, x1 = x,
    line = list(...)
  )
  p %>% layout(shapes=list(l_shape))
}



#' sparkline_legend
#'
#' This function is a internal function adding the title to the plot.
#'
#'
#' @param p A parameter indicate which plot to add a tittle
#' @param tittle A string adding as the tittle of the plot
#' @examples
#' library(plotly)
#' p <- plot_ly(cars, x = ~speed, y = ~dist)
#' sparkline_legend <- function(p, title = "Treatment", pos = -1){
#'   plotly::layout(p,
#'                  showlegend = TRUE,
#'                  legend = list(
#'                    title = list(text = title),
#'                    orientation = "h",   # show entries horizontally
#'                    xanchor = "center",  # use center of legend as anchor
#'                    x = 0.5,
#'                    y = pos)
#'   )
#' }
#' sparkline_legend(p, title = "cars plot")
#' @export
sparkline_legend <- function(p, title = "Treatment", pos = -1){
  plotly::layout(p,
           showlegend = TRUE,
           legend = list(
           title = list(text = title),
           orientation = "h",   # show entries horizontally
           xanchor = "center",  # use center of legend as anchor
           x = 0.5,
           y = pos)
  )
}



#' sparkline_layout
#'
#' This is the function which adding color to vertical line.
#'
#' @param p A parameter indicate which plot to add color for scatters.
#' @param xlim A data frame define the range of x-axis on the plot
#' @param vline A data frame indicates where to draw the vertical lines on the plot
#' @param color A character string define the color of vertical line.
#' @param margin A data define the size of the plot
#' @examples
#' p <- plot_ly(cars, x = ~speed, y = ~dist)
#' sparkline_layout <- function(p,
#'                              xlim  = NULL,
#'                              vline = NULL,
#'                              color = "#00000050",
#'                              margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
#'                              fixedrange = TRUE
#' ){
#'
#'   stopifnot("plotly" %in% class(p))
#'
#'   # Add vertical reference line
#'   l_shape = list(
#'     type = "line",
#'     y0 = 0, y1 = 1, yref = "paper", # i.e. y as a proportion of visible region
#'     x0 = vline,
#'     x1 = vline,
#'     line = list(color = color)
#'   )
#'
#'   plotly::layout(p,
#'                  xaxis = list(title = "", range = xlim, zeroline = FALSE, fixedrange = fixedrange),
#'                  yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange = fixedrange),
#'                  margin = margin,
#'                  shapes = list(l_shape),
#'                  plot_bgcolor  = "rgba(0, 0, 0, 0)",
#'                  paper_bgcolor = "rgba(0, 0, 0, 0)",
#'                  hoverlabel=list(bgcolor="lightgray"),
#'                  showlegend = FALSE) %>%
#'     plotly::config(displayModeBar = FALSE)
#' }
#' sparkline_layout(p, color='red')
#'
#' @export
sparkline_layout <- function(p,
                             xlim  = NULL,
                             vline = NULL,
                             color = "#00000050",
                             margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
                             fixedrange = TRUE
){

  stopifnot("plotly" %in% class(p))

  # Add vertical reference line
  l_shape = list(
    type = "line",
    y0 = 0, y1 = 1, yref = "paper", # i.e. y as a proportion of visible region
    x0 = vline,
    x1 = vline,
    line = list(color = color)
  )

  plotly::layout(p,
                 xaxis = list(title = "", range = xlim, zeroline = FALSE, fixedrange = fixedrange),
                 yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange = fixedrange),
                 margin = margin,
                 shapes = list(l_shape),
                 plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 hoverlabel=list(bgcolor="lightgray"),
                 showlegend = FALSE) %>%
    plotly::config(displayModeBar = FALSE)

}
