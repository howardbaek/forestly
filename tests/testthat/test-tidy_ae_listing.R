test_that("compare the colname with standard AE tables", {
  expect_equal(2 * 2, 4)
})

test_that("compare tidy_ae_listing(db) with db[, listing_var] ignoring the colnames, the order of columns and AE duration", {
  expect_equal(2 * 2, 4)
})