test_that("tidy_observation's 4 variables:'observation_from', 'observation_where', 'treatment_var' and 'treatment_order', must have input data.",{
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
                                   stratum_var = NULL,
                                   treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), "tbl_df")
                               
})

test_that("tidy_observation can tidy the data", {

  # function to create the expectation table:
  expectation_table <- function(data, ob_where, trt_var, trt_order, stratum_var = NULL){
    db <- eval(parse(text = paste0("subset(data,", ob_where, ")")))
    db[["treatment"]] <- db[[trt_var]]
    db <- subset(db, treatment %in% trt_order)

    label_name = names(trt_order)

    db[["treatment"]] <- factor(db[["treatment"]], levels = trt_order, labels = label_name)
    
    # Define stratum
    if(length(stratum_var) == 0){
      db[["stratum"]] <- "All"
    }else{
      db[["stratum"]] <- db[[stratum_var]]
    }
    
    return(db)
  }

  # compare the actual result with expectation table:
  # tidy_observation(observation_from = adae,
  #                  observation_where = "SEX == 'M'",
  #                  treatment_var    = "TRTA",
  #                  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")).
  expect_equal(tidy_observation(observation_from = adae,
                                observation_where = "SEX == 'M'",
                                treatment_var    = "TRTA",
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), expectation_table(adae, "SEX == 'M'", "TRTA", c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")))

  # compare the actual result with expectation table:
  # tidy_observation(observation_from = adae,
  #                  observation_where = NULL,
  #                  treatment_var    = "TRTA",
  #                  treatment_order  = c("ABC" = "Xanomeline Low Dose")).
  expect_equal(tidy_observation(observation_from = adae,
                                observation_where = NULL,
                                treatment_var    = "TRTA",
                                treatment_order  = c("ABC" = "Xanomeline Low Dose")), expectation_table(adae, TRUE, "TRTA", c("ABC" = "Xanomeline Low Dose")))

  # compare the actual result with expectation table:
  # tidy_observation(observation_from = adae,
  #                  observation_where = NULL,
  #                  treatment_var    = "TRTA",
  #                  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")).
  expect_equal(tidy_observation(observation_from = adae,
                                observation_where = NULL,
                                treatment_var    = "TRTA",
                                treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")), expectation_table(adae, TRUE, "TRTA", c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo")))
})
