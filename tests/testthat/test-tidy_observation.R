test_that("tidy_observation must have input data.",{
  expect_error(tidy_observation(observation_where = NULL,
                                treatment_var = "TRTA",
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), "argument \"observation_from\" is missing")
  expect_error(tidy_observation(observation_from = adae,
                                treatment_var = "TRTA",
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), "argument \"observation_where\" is missing")
  expect_error(tidy_observation(observation_from = adae,
                                observation_where = NULL,
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")))
  expect_error(tidy_observation(observation_from = adae,
                                observation_where = NULL,
                                treatment_var    = "TRTA"))
})

test_that("tidy_observation can generate the table", {

  expect_s3_class(tidy_observation(observation_from = adae,
                                   observation_where = NULL,
                                   treatment_var    = "TRTA",
                                   treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), "tbl_df")
})

test_that("tidy_observation can tidy the data", {

  # function to create the expectation table:
  # tidy_observation(observation_from = adae,
  #                  observation_where = "SEX == 'M'",
  #                  treatment_var    = "TRTA",
  #                 treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))
  e1 <- function(data){
    db <- subset(data, SEX == 'M')
    db[["treatment"]] <- db[["TRTA"]]
    db <- subset(db, treatment %in% c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

    label_name = names(c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

    db[["treatment"]] <- factor(db[["treatment"]], levels = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"), labels = label_name)

    return(db)
  }


  # function to create the expectation table:
  # tidy_observation(observation_from = adae,
  #                  observation_where = NULL,
  #                  treatment_var    = "TRTA",
  #                  treatment_order  = c("ABC" = "Xanomeline Low Dose"))
  e2 <- function(data){
    db <- subset(data, TRUE)
    db[["treatment"]] <- db[["TRTA"]]
    db <- subset(db, treatment %in% c("ABC" = "Xanomeline Low Dose"))

    label_name = names(c("ABC" = "Xanomeline Low Dose"))

    db[["treatment"]] <- factor(db[["treatment"]], levels = c("ABC" = "Xanomeline Low Dose"), labels = label_name)

    return(db)
  }

  # function to create the expectation table:
  # tidy_observation(observation_from = adae,
  #                  observation_where = NULL,
  #                  treatment_var    = "TRTA",
  #                 treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))
  e3 <- function(data){
    db <- subset(data, TRUE)
    db[["treatment"]] <- db[["TRTA"]]
    db <- subset(db, treatment %in% c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

    label_name = names(c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))

    db[["treatment"]] <- factor(db[["treatment"]], levels = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"), labels = label_name)

    return(db)
  }

  expect_equal(tidy_observation(observation_from = adae,
                                observation_where = "SEX == 'M'",
                                treatment_var    = "TRTA",
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), e1(adae))
  expect_equal(tidy_observation(observation_from = adae,
                                observation_where = NULL,
                                treatment_var    = "TRTA",
                                treatment_order  = c("ABC" = "Xanomeline Low Dose")), e2(adae))
  expect_equal(tidy_observation(observation_from = adae,
                                observation_where = NULL,
                                treatment_var    = "TRTA",
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), e3(adae))
})
