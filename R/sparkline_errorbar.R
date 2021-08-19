#' sparkline_errorbar
#'
#' This function is used to draw error bars based on the parameters you defined.
#'
#' @param est A vector determined the point of your bar
#' @param lower A vector determined the leftmost point
#' @param upper A vector determined the rightmost point
#' @param color A vector of colors you want your bar to be
#' @param height A variable determines the distance between your bars
#' @param xlim Numeric vectors of length 2, giving the x coordinates ranges.
#' @param vline Numeric value for the location to draw the vertical
#'              reference line.
#' @param margin Numeric vector of figure margin
#' @param ... Additional arguments transfer to `sparkline_layout`
#'
#' @return Sparkline error bar for interactive graphics
#'
#' @examples
#' library(plotly)
#' library(ggplot2)
#' @examples
#' library(plotly)
#' est <- c(0,1,2)
#' lower <- c(-1, -2, -1)
#' upper <- c(3, 5, 5)
#' color <- c("red", "blue", "gold")
#' sparkline_errorbar(est, lower, upper, color)
#'
#' @export
sparkline_errorbar <- function(est,
                               lower,
                               upper,
                               color = NULL,
                               height = 30,
                               xlim = NULL,
                               vline = NULL,
                               margin = list(l = 0, r = 0, b = 0, t = 0,
                                             pad = 0),
                               ...) {

  db <- data.frame(est = est,
                   lower = lower,
                   upper = upper,
                   mid = (lower + upper) / 2)

  db$sd <- db$upper - db$mid

  db$est1   <- formatC(db$est, digits = 2, format = "f")
  db$lower1 <- formatC(db$lower, digits = 2, format = "f")
  db$upper1 <- formatC(db$upper, digits = 2, format = "f")

  hover_text <- with(db, paste0(db$est1, " (", db$lower1, ", ", db$upper1, ")"))
  color <- factor(color, levels = color)


  p <- plotly::plot_ly(data = db,
               x = ~ est,
               y = ~ 1:nrow(db),
               color = color,
               colors = as.character(color),
               text = hover_text,
               hoverinfo = "text",
               type = "scatter",
               mode = "markers",
               height = height)

  p <- p %>% plotly::add_trace(x = ~ mid,
                       alpha = 0,
                       text = hover_text,
                       hoverinfo = "text",
                       error_x =  ~list(array = sd, color = color))

  p %>% sparkline_layout(xlim = xlim,
                         vline = vline,
                         margin = margin,
                         ...)

}