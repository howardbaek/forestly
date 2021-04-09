library(dplyr)

test_that("tidy_population must have a input data.",{
  expect_error(tidy_population(),"argument \"population_from\" is missing")
})

test_that("tidy_population can generate the table", {
  expect_s3_class(tidy_population(population_from = adsl), "tbl_df")
})


test_that("tidy_population can tidy the data", {

  # function to creat expectation table: tidy_population(population_from = adsl).
  e1 <- function(data){
    pop <- subset(data, TRUE)
    pop[["treatment"]] <- pop[["TRT01A"]]
    pop <- subset(pop, treatment %in% unique(data[["TRT01A"]]))

    label_name = unique(data[["TRT01A"]])

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = unique(data[["TRT01A"]]), labels = label_name)
    pop[["stratum"]] <- "All"
    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }

  # function to create expectation table: tidy_population(population_from = adsl, population_where="ITTFL=='Y'").
  e2 <- function(data){
    pop <- subset(data, ITTFL=='Y')
    pop[["treatment"]] <- pop[["TRT01A"]]
    pop <- subset(pop, treatment %in% unique(data[["TRT01A"]]))

    label_name = unique(data[["TRT01A"]])

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = unique(data[["TRT01A"]]), labels = label_name)
    pop[["stratum"]] <- "All"
    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }

  # function to create expectation table: tidy_population(population_from = adsl, treatment_var = "TRT01P").
  e3 <- function(data){
    pop <- subset(data, TRUE)
    pop[["treatment"]] <- pop[["TRT01P"]]
    pop <- subset(pop, treatment %in% unique(data[["TRT01P"]]))

    label_name = unique(data[["TRT01P"]])

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = unique(data[["TRT01P"]]), labels = label_name)
    pop[["stratum"]] <- "All"
    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }

  # function to create expectation table: tidy_population(population_from = adsl, treatment_order  = c("ABC" = "Xanomeline Low Dose")).
  e4 <- function(data){
    pop <- subset(data, TRUE)
    pop[["treatment"]] <- pop[["TRT01A"]]
    pop <- subset(pop, treatment %in% c("ABC" = "Xanomeline Low Dose"))

    label_name = names(c("ABC" = "Xanomeline Low Dose"))

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = c("ABC" = "Xanomeline Low Dose"), labels = label_name)
    pop[["stratum"]] <- "All"
    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }

  # function to create expectation table: tidy_population(population_from = adsl, stratum_var = "RACE").
  e5 <- function(data){
    pop <- subset(data, TRUE)
    pop[["treatment"]] <- pop[["TRT01A"]]
    pop <- subset(pop, treatment %in% unique(data[["TRT01A"]]))

    label_name = unique(data[["TRT01A"]])

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = unique(data[["TRT01A"]]), labels = label_name)
    pop[["stratum"]] <- pop[["RACE"]]
    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }


  # function to create expectation table: tidy_population(population_from  = adsl %>% rename(TRTA = TRT01A),
  #  population_where = "ITTFL=='Y'",
  #  treatment_var    = "TRTA",
  #  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
  #  stratum_var      = NULL,
  #  baseline_var     = NULL).
  e6 <- function(data){
    data <- data %>% rename(TRTA = TRT01A)
    pop <- subset(data, ITTFL=='Y')
    pop[["treatment"]] <- pop[["TRTA"]]
    pop <- subset(pop, treatment %in% c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

    label_name = names(c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

    pop[["treatment"]] <- factor(pop[["treatment"]], levels = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"), labels = label_name)
    pop[["stratum"]] <- "All"

    pop <- pop[, unique(c("USUBJID", "treatment", "stratum"))]

    return(pop)
  }

  expect_equal(tidy_population(population_from = adsl), e1(adsl))
  expect_equal(tidy_population(population_from = adsl, population_where="ITTFL=='Y'"), e2(adsl))
  expect_equal(tidy_population(population_from = adsl, treatment_var = "TRT01P"), e3(adsl))
  expect_equal(tidy_population(population_from = adsl, treatment_order  = c("ABC" = "Xanomeline Low Dose")), e4(adsl))
  expect_equal(tidy_population(population_from = adsl, stratum_var = "RACE"), e5(adsl))
  expect_equal(tidy_population(population_from  = adsl %>% rename(TRTA = TRT01A),
                               population_where = "ITTFL=='Y'",
                               treatment_var    = "TRTA",
                               treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                               stratum_var      = NULL,
                               baseline_var     = NULL), e6(adsl))
})
