library(devtools)
library(dplyr)
library(tidyr)
load_all()

test_that("tidy_ae_table's 2 variables:'population_from', 'observation_from', must have input data.",{
  expect_error(tidy_ae_table(observation_from = adae,
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
  
  # function to create expectation table.
  expectation_table <- function(data1, data2, 
                                pop_where, obs_where, 
                                trt_var, trt_order, 
                                ae_var, ae_instd,
                                str_var = NULL,
                                listing_var = names(observation_from)){
    
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
    
    # select the overlap pop(adsl) and db(adae)
    db[["ae"]] <- tools::toTitleCase(tolower(db[[ae_var]])) 
    db <- subset(db, USUBJID %in% pop$USUBJID)
    
    # Select the variables to be listed in the detailed listing
    db_listing <- tidy_listing(db, listing_var)
    
    # count the sample size of each arm
    db_N <- dplyr::count(pop, treatment, stratum, name = "N")
    
    # rbind the data with interested AE labels
    interested_ae_criterion <- ae_instd$ae_criterion
    interested_ae_label <- ae_instd$ae_label
    
    ## Start with all AE
    res <- db %>% group_by(treatment, ae) %>%
      summarise(n = n_distinct(USUBJID)) %>%  # summarise(n = n()) %>%
      mutate(ae_label = "All") %>%            # give a label to the AE without filter   
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(- treatment) %>%
      pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
      mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x)))
    
    ## For each interested AE, iteration once and rbind them together
    for (ae_idx in seq_along(interested_ae_criterion)) {
      
      ## Decide the filer according to the interested AE
      temp_ae_criterion <- interested_ae_criterion[ae_idx]
      temp_ae_label <- interested_ae_label[ae_idx]
      
      ## Filter the interested AE
      res_new <- eval(parse(text = paste0("subset(db,", temp_ae_criterion, ")")))
      res_new <- res_new %>% group_by(treatment, ae) %>%
        summarise(n = n_distinct(USUBJID)) %>%  # summarise(n = n()) %>%
        mutate(ae_label = temp_ae_label) %>%    # give a label to the AE without filter
        left_join(db_N) %>%
        mutate(pct = n / N * 100) %>%
        ungroup() %>%
        mutate(trtn = as.numeric(treatment)) %>%
        select(- treatment) %>%
        pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
        mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x))) 
      
      ## Union the filtered AE with the old data
      res <- dplyr::union_all(res, res_new)
      
      ## It is possible that only treatment or control arm has the interested AE.
      ## So for the arm without the interested AE, we fill the AE case as 0 and also fill in the arm sample size
      res$N_1[is.na(res$N_1)] = db_N$N[as.numeric(db_N$treatment) == 1] 
      res$N_2[is.na(res$N_2)] = db_N$N[as.numeric(db_N$treatment) == 2] 
      res$n_1[is.na(res$n_1)] = 0
      res$n_2[is.na(res$n_2)] = 0
      res$pct_1[is.na(res$pct_1)] = 0.0000
      res$pct_2[is.na(res$pct_2)] = 0.0000
    }
    
    res <- res %>% mutate(across(pct_1 : pct_2, ~ round(.x, digits = 4)))
    
    # Title Case the cell values
    res$ae <- tools::toTitleCase(tolower(res$ae))
    
    # sort the output returns
    list(table = res, 
         listing = db_listing, 
         sample_size = db_N,
         treatment_order = trt_order)
  }
    
  
  # compare the actual result with expectation table:
  expect_equal(tidy_ae_table(population_from = adsl %>% rename(TRTA = TRT01A),
                             observation_from = adae,
                             population_where = "ITTFL=='Y'",
                             observation_where = NULL,
                             treatment_var = "TRTA",
                             treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                             ae_var = "AEDECOD",
                             ae_interested = ae_interested(ae_criterion = 'AESER == "Y"', ae_label = "with serious adverse events"),
                             listing_var = c("USUBJID", "SEX", "RACE", "AGE")),
               expectation_table(data1 = adsl %>% rename(TRTA = TRT01A),
                                 data2 = adae,
                                 pop_where = "ITTFL=='Y'",
                                 obs_where = NULL,
                                 trt_var = "TRTA",
                                 trt_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                                 ae_var = "AEDECOD",
                                 ae_instd = list(ae_criterion = 'AESER == "Y"', ae_label = "with serious adverse events"),
                                 listing_var = c("USUBJID", "SEX", "RACE", "AGE")))
})





