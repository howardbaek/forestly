add_vline = function(p, x, ...) {
  l_shape = list(
    type = "line", 
    y0 = 0, y1 = 1, yref = "paper",
    x0 = x, x1 = x, 
    line = list(...)
  )
  p %>% layout(shapes=list(l_shape))
}

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

