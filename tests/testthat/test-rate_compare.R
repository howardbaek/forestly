test_that("rate_compare() matches prop_test_mn for unstratified analysis", {
  ana <- data.frame(
    treatment = c(rep(0,100),rep(1,100)),
    response  = c(rep(0,80),rep(1,20),rep(0,40),rep(1,60)),
    stratum   = c(rep(1:4,12),1,3,3,1,rep(1:4,12),rep(1:4,25))
  )
  
  x0 <- sum(ana$response[1:100])
  x1 <- sum(ana$response[101:200])
  n0 <- 100
  n1 <- 100
  
  compare1 <- rate_compare(response~treatment, data = ana)
  compare2 <- prop_test_mn(x0, n0, x1, n1)
  
  o1 <- c(compare1$est, compare1$lower, compare1$upper, compare1$p)*100

  o2 <- c(compare2$est, compare2$lower, compare2$upper, compare2$p)
  expect_equal(o1, o2, tolerance = 1e-3)
})
