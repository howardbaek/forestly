library(plotly)
library(jsonlite)
library(plotly)

plotly_snap <- function(p){
  p_js <- plotly_json(p, jsonedit = FALSE)
  p_lst <- fromJSON(p_js)
  gsub(names(p_lst$visdat), "0000", as.character(p_lst))
}



test_that("add_vline",{
  
  p1 <- plot_ly(cars, x = ~speed, y = ~dist, type = "scatter", mode = 'markers')
  temp1<-add_vline(p1, x = 7.5)
  
  
  
  expect_snapshot_output(plotly_snap(temp1))
  
})


test_that("sparkline_legend",{
  
  p2 <- plot_ly(cars, x = ~speed, y = ~dist, type = "scatter", mode = 'markers')
  temp2<-sparkline_legend(p2, title = "cars plot")
  
  
  
  expect_snapshot_output(plotly_snap(temp2))
  
})


test_that("sparkline_layout",{
  
  p3 <- plot_ly(cars, x = ~speed, y = ~dist, type = "scatter", mode = 'markers')
  temp3<-sparkline_layout(p3, color='red')
  
  
  
  expect_snapshot_output(plotly_snap(temp3))
  
})