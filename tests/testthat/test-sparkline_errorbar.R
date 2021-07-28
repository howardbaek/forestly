library(plotly)
library(forestly)

test_that("sparkline_errorbar",{
  est <- c(0,1,2)
  lower <- c(-1, -2, -1)
  upper <- c(3, 5, 5)
  color <- c("red", "blue", "gold")
  temp1<-sparkline_errorbar(est,lower,upper,color)

  expect_snapshot_output(temp1$x$attrs[[1]])
})
