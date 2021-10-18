## Test case 1
test_that("throw error when the length of n0, n1, x0, x1 doesn't match", {
  expect_error(rate_compare_sum(n0 = c(10, 20, 30), n1 = c(20, 10, 40), 
                                x0 = c(1, 2, 3),    x1 = c(2, 4),  
                                strata = c(1, 2, 3)))
  expect_error(rate_compare_sum(n0 = c(10, 20, 30), n1 = c(20, 40), 
                                x0 = c(1, 2, 3),    x1 = c(2, 4, 6),  
                                strata = c(1, 2, 3)))
  expect_error(rate_compare_sum(n0 = c(10, 20, 30), n1 = c(20, 10, 40), 
                                x0 = c(1, 2, 3),    x1 = c(2, 4, 6),  
                                strata = c("a", "b")))
})

## Test case 2
test_that("match with prop_test_mn()", {
  
  my_n0 = 100  # number of subjects in arm 0
  my_n1 = 105  # number of subjects in arm 1
  my_x0 = 40   # number of response in arm 0
  my_x1 = 60   # number of response in arm 1
  
  xx <- rate_compare_sum(n0 = my_n0, n1 = my_n1, x0 = my_x0, x1 = my_x1)
  yy <- prop_test_mn(n0 = my_n0, n1 = my_n1, x0 = my_x0, x1 = my_x1)
  
  xx_output <- c(xx$est, xx$lower, xx$upper) * 100
  yy_output <- c(yy$est, yy$lower, yy$upper)
  
  expect_equal(xx_output, yy_output, tolerance = 1e-4)
})

## Test case 3
test_that("match with rate_compare()", {
  ana <- data.frame(
    treatment = c(rep(0, 100), rep(1, 100)),
    response  = c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60)),
    stratum   = c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
  )
  
  x0 <- sum(ana$response[1:100])
  x1 <- sum(ana$response[101:200])
  n0 <- 100
  n1 <- 100
  
  xx <- rate_compare(response~treatment, data = ana)
  yy <- rate_compare_sum(n0 = n0, n1 = n1, x0 = x0, x1 = x1)
  
  xx_output <- c(xx$est, xx$lower, xx$upper, xx$p)
  yy_output <- c(yy$est, yy$lower, yy$upper, yy$p)
  expect_equal(xx_output, yy_output, tolerance = 1e-4)
  expect_equal(colnames(xx), colnames(yy))
})