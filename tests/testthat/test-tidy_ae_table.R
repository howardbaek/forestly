library(usethis)
library(devtools)
library(dplyr)
library(tidyr)

test_that("tidy_observation's 2 variables:'population_from', 'observation_from', must have input data.",{
  expect_error(tidy_ae_table(observation_from=adae,
                             population_where = "ITTFL=='Y'",
                             observation_where = NULL,
                             treatment_var = "TRTA",
                             treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                             ae_var = "AEDECOD",
                             stratum_var = NULL,
                             listing_var = names(observation_from)), "argument \"population_from\" is missing")
  expect_error(tidy_ae_table(population_from = adsl %>% rename(TRTA = TRT01A),
                             population_where = "ITTFL=='Y'",
                             observation_where = NULL,
                             treatment_var = "TRTA",
                             treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                             ae_var = "AEDECOD",
                             stratum_var = NULL,
                             listing_var = names(observation_from)), "argument \"observation_from\" is missing")
})

test_that("tidy_ae_table can tidy the data", {
  
  # function to creat expectation table.
  expectation_table <- function(data1, data2, pop_where, obs_where, trt_var, trt_order, ae_var, str_var,listing_var = names(observation_from)){
    
    # Population Level Tidy Data
    pop <- tidy_population(population_from  = data1,
                           population_where = pop_where,
                           treatment_var    = trt_var,
                           treatment_order  = trt_order,
                           stratum_var      = str_var,
                           baseline_var     = NULL)
    
    # Select the Desired Observation
    db <- tidy_observation(observation_from = data2,
                           observation_where = obs_where,
                           treatment_var    = trt_var,
                           treatment_order  = trt_order)
    
    db[["ae"]] <- db[[ae_var]]
    db <- subset(db, USUBJID %in% pop$USUBJID)
    
    # Yilong: will remove dplyr, tiyer dependency
    db_N <- count(pop, treatment, stratum, name = "N")
    
    res <- db %>% group_by(treatment, ae) %>%
      summarise(n = n()) %>%
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(- treatment) %>%
      pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
      mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x)))
    
    listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
    list(table = res, listing = db[, listing_var])
  }
  
  # compare the actual result with expectation table:
  expect_equal(tidy_ae_table(population_from = adsl %>% rename(TRTA = TRT01A),
                             observation_from = adae,
                             population_where = "ITTFL=='Y'",
                             observation_where = NULL,
                             treatment_var = "TRTA",
                             treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                             ae_var = "AEDECOD",
                             stratum_var = "RACE",
                             listing_var = c("SITEID", "USUBJID", "AGE", "RACE", "SEX","AETERM", "AESER", "AEREL", "AEACN", "AEOUT")),
               expectation_table(data1=adsl %>% rename(TRTA = TRT01A),
                                 data2=adae,
                                 pop_where= "ITTFL=='Y'",
                                 obs_where=NULL,
                                 trt_var ="TRTA",
                                 trt_order= c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                                 ae_var = "AEDECOD",
                                 str_var="RACE",
                                 listing_var = c("SITEID", "USUBJID", "AGE", "RACE", "SEX","AETERM", "AESER", "AEREL", "AEACN", "AEOUT")))
})





