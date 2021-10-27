test_that("define_ae_select_list() gives errors if the number of ae_label does't match that of ae_criterion", {
  expect_error(define_ae_select_list(
    ae_criterion = c('AESER != "N"', 'AEREL != "None"'),
    ae_label = c("with serious adverse events")
  ))
})

test_that("missing argument would generate errors", {
  expect_error(define_ae_select_list(ae_criterion = c('AESER != "N"', 'AEREL != "None"')))
  expect_error(define_ae_select_list(ae_label = c(
    "with serious adverse events",
    "with drug-related adverse events"
  )))
})

test_that("compare with the output from list()", {
  expect_equal(
    define_ae_select_list(
      ae_criterion = c('AESER != "N"', 'AEREL != "None"'),
      ae_label = c(
        "with serious adverse events",
        "with drug-related adverse events"
      )
    ),
    list(
      interested_ae_criterion = c('AESER != "N"', 'AEREL != "None"'),
      interested_ae_label = c(
        "with serious adverse events",
        "with drug-related adverse events"
      )
    )
  )
})
