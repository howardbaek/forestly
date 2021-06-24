library(testthat)

#' est <- c(0,1,2)
#' lower <- c(-1, -2, -1)
#' upper <- c(3, 5, 5)
#' color <- c("red", "blue", "gold")

test_that("sparkline_errorbar's 4 variables:'est', 'lower', 'upper' and 'color', must have input data.",{
  expect_error(sparkline_errorbar(est = c(0,1,2),
                                lower = c(-1, -2, -1),
                                upper  = c(3, 5, 5)))
  expect_error(sparkline_errorbar(est = c(0,1,2),
                                lower = c(-1, -2, -1),
                                color  = c("red", "blue", "gold")))
  expect_error(sparkline_errorbar(est = c(0,1,2),
                                upper = c(3, 5, 5),
                                color  = c("red", "blue", "gold")))
  expect_error(sparkline_errorbar(lower=c(-1, -2, -1),
                                upper = c(3, 5, 5),
                                color  = c("red", "blue", "gold")))
})

test_that("sparkline_errorbar can generate error bars", {

  expect_s3_class(tidy_observation(est = c(0,1,2),
                                   lower = c(-1, -2, -1),
                                   upper  = c(3, 5, 5),
                                   color  = c("red", "blue", "gold")))
})
