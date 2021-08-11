library(dplyr)

test_that("tidy_population's variable 'population_from' must have a input data.", {
  expect_error(tidy_population(), "argument \"population_from\" is missing")
})

test_that("tidy_population can generate the table", {
  expect_s3_class(tidy_population(population_from = adsl), "tbl_df")
})


test_that("tidy_population can tidy the data", {

  # function to creat expectation table.
  expectation_table <- function(data, pop_where, trt_var, trt_order, str_var) {
    pop <- eval(parse(text = paste0("subset(data,", pop_where, ")")))
    pop[["treatment"]] <- pop[[trt_var]]
    pop <- subset(pop, treatment %in% trt_order)

    if (!is.null(names(trt_order))) {
      label_name <- names(trt_order)
    } else {
      label_name <- trt_order
    }

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = trt_order, labels = label_name)

    if (length(str_var) == 0) {
      pop[["stratum"]] <- "All"
    } else {
      pop[["stratum"]] <- pop[[str_var]]
    }

    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }


  # compare the actual result with expectation table:
  # tidy_population(population_from = adsl).
  expect_equal(tidy_population(population_from = adsl), expectation_table(adsl, TRUE, "TRT01A", unique(adsl[["TRT01A"]]), NULL))

  # compare the actual result with expectation table:
  # tidy_population(population_from = adsl,
  #                 population_where="ITTFL=='Y'").
  expect_equal(tidy_population(population_from = adsl, population_where = "ITTFL=='Y'"), expectation_table(adsl, "ITTFL=='Y'", "TRT01A", unique(adsl[["TRT01A"]]), NULL))

  # compare the actual result with expectation table:
  # tidy_population(population_from = adsl,
  #                 treatment_var = "TRT01P").
  expect_equal(tidy_population(population_from = adsl, treatment_var = "TRT01P"), expectation_table(adsl, TRUE, "TRT01P", unique(adsl[["TRT01P"]]), NULL))

  # compare the actual result with expectation table:
  # tidy_population(population_from = adsl,
  #                 treatment_order  = c("ABC" = "Xanomeline Low Dose")).
  expect_equal(tidy_population(population_from = adsl, treatment_order = c("ABC" = "Xanomeline Low Dose")), expectation_table(adsl, TRUE, "TRT01A", c("ABC" = "Xanomeline Low Dose"), NULL))

  # compare the actual result with expectation table:
  # tidy_population(population_from = adsl,
  #                 stratum_var = "RACE").
  expect_equal(tidy_population(population_from = adsl, stratum_var = "RACE"), expectation_table(adsl, TRUE, "TRT01A", unique(adsl[["TRT01A"]]), "RACE"))

  # compare the actual result with expectation table:
  # tidy_population(population_from  = adsl %>% rename(TRTA = TRT01A),
  #  population_where = "ITTFL=='Y'",
  #  treatment_var    = "TRTA",
  #  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
  #  stratum_var      = NULL,
  #  baseline_var     = NULL).
  expect_equal(tidy_population(
    population_from = adsl %>% rename(TRTA = TRT01A),
    population_where = "ITTFL=='Y'",
    treatment_var = "TRTA",
    treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
    stratum_var = NULL,
    baseline_var = NULL
  ), expectation_table(adsl %>% rename(TRTA = TRT01A), "ITTFL=='Y'", "TRTA", c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"), NULL))
})
