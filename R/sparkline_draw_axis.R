#' sparkline_draw_axis
#'
#' @examples
#' library(plotly)
#' color <- c("red", "blue")
#' label <- c("MK9999", "Placebo")
#' xlim  <- c(-2, 2)
#' xlab  <- "Example"
#'
#' sparkline_draw_axis(color, label, xlim,
#'                     showlegend = FALSE, height = 30)
#'
#' sparkline_draw_axis(color, label, xlim,
#'                     showlegend = TRUE, height = 60)
#'
#' sparkline_draw_axis(color, label, xlim, xlab = "Example",
#'                     showlegend = FALSE, height = 60)
#'
#' sparkline_draw_axis(color, label, xlim, xlab = "Example",
#'                     showlegend = TRUE, height = 100)
#'
#' sparkline_draw_axis(color, label, xlim, xlab = "Example",
#'                     mode = "lines", showlegend = TRUE, height = 100)
#'
#' sparkline_draw_axis(color, label, xlim, xlab = "Example",
#'                     mode = "markers+lines", showlegend = TRUE, height = 100)
#'
#' @export
sparkline_draw_axis <- function(color,
                                label,
                                xlim,
                                xlab = NULL,
                                mode = "markers",
                                showlegend = TRUE,
                                height = ifelse(showlegend, 60, 30),
                                margin_bottom = ifelse(showlegend, 0, height) ){

  p <- plot_ly(x = 0,
               y = 10,
               color = label,
               colors = color,
               type = 'scatter',
               mode = mode,
               height = height)

  # Bottom Margin
  b <- margin_bottom

  p <- p %>%
    sparkline_layout(margin = list(l = 0, r = 0, t = 0, b = b, pad = 0), xlim = xlim) %>%
    layout(xaxis = list(title = list(text = xlab, standoff = 0),
                        showline=TRUE, ticks = "outside"),
           yaxis = list(range = c(0, 1))) %>%
    style(hoverinfo = 'none')

  p

  if(showlegend){
    p <- sparkline_legend(p, pos = -2)
  }

  p

}
