library(usethis)
library(devtools)
library(dplyr)
library(tidyverse)
library(testthat)
library(plotly)

test_that("sparkline_point's 2 variables:'x' and 'y'must have input data.",{
  expect_error(sparkline_point( y = 1:5, color = color, height = 500, vline = 1.5,
                                hover_text = c('a','b','c','d','e'), "argument\"x\" is missing, with no default"))
  expect_error(sparkline_point( x=1:5, color = color, height = 500, vline = 1.5,
                                hover_text = c('a','b','c','d','e'), "argument\"y\" is missing, with no default"))
})

plotly_snap <- function(p){
  p_js <- plotly::plotly_json(p, jsonedit = FALSE)
  p_lst <- jsonlite::fromJSON(p_js) 
  gsub(names(p_lst$visdat), "0000", as.character(p_lst))
} 

test_that("sparkline_point",{
  x = 1:5
  y = 1:5
  color = c("red", "blue", "gold", "grey", "black")
  hover_text=c('a','b','c','d','e')
  xlim = 1.5
  vline = 1.5
  temp1<-sparkline_point(x, y, color,hover_text,xlim,vline)
  
  expect_snapshot_output(plotly_snap(temp1))
  
})

