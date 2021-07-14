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


test_that("scatterplots are generated without error", {
  p<-plot_ly(x = 1:5,y = 1:5,color = c("red", "blue", "gold", "grey", "black"),text=c('a','b','c','d','e'),
                                    hoverinfo = 'text',type = 'scatter',mode = 'markers',height = 300,width = 150)
  p<-p %>% sparkline_layout(xlim = 1.5,vline = 1.5,margin =list(l = 0, r = 0, b = 0, t = 0, pad = 0))
  expectation_scatterplots<-p

 # compare the actual result with expectation scatterplots:
  expect_equal(sparkline_point(x=1:5,y=1:5,color = c("red", "blue", "gold", "grey", "black"),height = 300,
                               width = 150,hover_text =c('a','b','c','d','e'),xlim = 1.5, vline =1.5,
                               margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)),expectation_scatterplots)
  })



testthat::expect_snapshot(sparkline_point(x=1:5,y=1:5,color = c("red", "blue", "gold", "grey", "black"),height = 300,
                                          width = 150,hover_text =c('a','b','c','d','e'),xlim = 1.5, vline =1.5,
                                          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)),expectation_scatterplots)
