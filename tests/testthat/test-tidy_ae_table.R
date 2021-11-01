library(devtools)
library(dplyr)
library(tidyr)

# test case 1
test_that("tidy_ae_table's 2 variables:'population_from', 'observation_from', must have input data.",{
  expect_error(tidy_ae_table(observation_from = adae,
                             population_where = "ITTFL=='Y'",
                             observation_where = NULL,
                             treatment_var = "TRTA",
                             treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                             ae_var = "AEDECOD",
                             stratum_var = NULL,
                             listing_interested = define_ae_listing() ), "argument \"population_from\" is missing")
  expect_error(tidy_ae_table(population_from = adsl %>% rename(TRTA = TRT01A),
                             population_where = "ITTFL=='Y'",
                             observation_where = NULL,
                             treatment_var = "TRTA",
                             treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                             ae_var = "AEDECOD",
                             stratum_var = NULL,
                             listing_interested = define_ae_listing()), "argument \"observation_from\" is missing")
})

# test case 2
test_that("tidy_ae_table can tidy the data for non-stratum case", {
  
  # function to create expectation table.
  expectation_table <- function(data1, data2,
                                pop_where, obs_where,
                                trt_var, trt_order,
                                ae_var, ae_instd,
                                str_var = NULL,
                                listing_interested = define_ae_listing()){
    
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
                           treatment_order  = trt_order,
                           stratum_var      = str_var)
    
    # select the overlap pop(adsl) and db(adae)
    db[["ae"]] <- tools::toTitleCase(tolower(db[[ae_var]])) 
    db <- subset(db, USUBJID %in% pop$USUBJID)
    
    # Select the variables to be listed in the detailed listing
    db_listing <- tidy_ae_listing(db, 
                                  listing_var = listing_interested$listing_var,
                                  listing_label = listing_interested$listing_label)
    
    # count the sample size of each arm
    db_N <- dplyr::count(pop, treatment, stratum, name = "N")
    
    # rbind the data with interested AE labels
    res <- db %>% select(treatment, ae, stratum, USUBJID) %>%
      complete(treatment, ae, stratum) %>%   # fill it with 0
      group_by(treatment, ae, stratum) %>%
      summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>%  # summarise(n = n()) %>%
      mutate(ae_label = "All") %>%            # give a label to the AE without filter   
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(- treatment) %>%
      pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0)
    
    if(is.null(ae_instd)){
      res <- res %>% mutate(across(pct_1 : pct_2, ~ round(.x, digits = 4)))
    }
    
    interested_ae_criterion <- ae_instd$ae_criterion #
    interested_ae_label <- ae_instd$ae_label         #
    
    ## For each interested AE, iteration once and rbind them together
    for (ae_idx in seq_along(interested_ae_criterion)) {
      
      ## Decide the filer according to the interested AE
      temp_ae_criterion <- interested_ae_criterion[ae_idx]
      temp_ae_label <- interested_ae_label[ae_idx]
      
      ## Filter the interested AE
      res_new <- eval(parse(text = paste0("subset(db,", temp_ae_criterion, ")")))
      
      res_new <- res_new %>% select(treatment, ae, stratum, USUBJID) %>%
        complete(treatment, ae, stratum) %>%    # fill it with 0
        group_by(treatment, ae, stratum) %>%    # group_by(treatment, ae) %>%
        summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>%  # summarise(n = n()) %>%
        mutate(ae_label = temp_ae_label) %>%    # give a label to the AE without filter
        left_join(db_N) %>%
        mutate(pct = n / N * 100) %>%
        ungroup() %>%
        mutate(trtn = as.numeric(treatment)) %>%
        select(- treatment) %>%
        pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) 
      
      ## Union the filtered AE with the old data
      res <- dplyr::union_all(res, res_new)
    }
    
    res <- res %>% mutate(across(pct_1 : pct_2, ~ round(.x, digits = 4)))
    
    # sort the output returns
    #listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
    list(table = res, 
         listing = db_listing, #listing = db[, listing_var],
         sample_size = db_N,
         treatment_order = trt_order)
  }

  # compare the actual result with expectation table:
  a <- tidy_ae_table(population_from = adsl %>% rename(TRTA = TRT01A),
                     observation_from = adae,
                     population_where = "ITTFL=='Y'",
                     observation_where = NULL,
                     treatment_var = "TRTA",
                     stratum_var = NULL,
                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                     ae_var = "AEDECOD",
                     ae_interested = define_ae_select_list(ae_criterion = 'AESER == "Y"', ae_label = "with serious adverse events"),
                     listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "RACE", "AGE"),
                                                            listing_label = c("ID", "Gender", "Race", "Age")) )
  b <- expectation_table(data1 = adsl %>% rename(TRTA = TRT01A),
                         data2 = adae,
                         pop_where = "ITTFL=='Y'",
                         obs_where = NULL,
                         trt_var = "TRTA",
                         str_var = NULL,
                         trt_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                         ae_var = "AEDECOD",
                         ae_instd = list(ae_criterion = 'AESER == "Y"', ae_label = "with serious adverse events"),
                         listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "RACE", "AGE"),
                                                                listing_label = c("ID", "Gender", "Race", "Age")))
  expect_equal(a$sample_size, b$sample_size)
  expect_equal(a$treatment_order, b$treatment_order)
  expect_equal(a$table, b$table)
  expect_equal(a$listing, b$listing)
})



# test case 3
test_that("tidy_ae_table can tidy the data for stratum case", {
  
  ## Impute stratum 
  adsl$STRATUMN <- sample(seq(1,3), size = length(adsl$USUBJID), prob = c(0.3, 0.3, 0.4), replace = TRUE)
  adae <- adae %>% left_join(data.frame(USUBJID = adsl$USUBJID, STRATUMN = adsl$STRATUMN))
  
  # function to create expectation table.
  expectation_table <- function(data1, data2,
                                pop_where, obs_where,
                                trt_var, trt_order,
                                ae_var, ae_instd,
                                str_var = NULL,
                                listing_instd = NULL){
    
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
                           treatment_order  = trt_order,
                           stratum_var      = str_var)
    
    # select the overlap pop(adsl) and db(adae)
    db[["ae"]] <- tools::toTitleCase(tolower(db[[ae_var]])) 
    db <- subset(db, USUBJID %in% pop$USUBJID)
    
    # Select the variables to be listed in the detailed listing
    db_listing <- tidy_ae_listing(db, 
                                  listing_var = listing_instd$listing_var,
                                  listing_label = listing_instd$listing_label)
    
    # count the sample size of each arm
    db_N <- dplyr::count(pop, treatment, stratum, name = "N")
    
    # rbind the data with interested AE labels
    res <- db %>% select(treatment, ae, stratum, USUBJID) %>%
      complete(treatment, ae, stratum) %>%   # fill it with 0
      group_by(treatment, ae, stratum) %>%
      summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>%  # summarise(n = n()) %>%
      mutate(ae_label = "All") %>%            # give a label to the AE without filter   
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(- treatment) %>%
      pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0)
    
    if(is.null(ae_instd)){
      res <- res %>% mutate(across(pct_1 : pct_2, ~ round(.x, digits = 4)))
    }
    
    interested_ae_criterion <- ae_instd$ae_criterion #
    interested_ae_label <- ae_instd$ae_label         #
    
    ## For each interested AE, iteration once and rbind them together
    for (ae_idx in seq_along(interested_ae_criterion)) {
      
      ## Decide the filer according to the interested AE
      temp_ae_criterion <- interested_ae_criterion[ae_idx]
      temp_ae_label <- interested_ae_label[ae_idx]
      
      ## Filter the interested AE
      res_new <- eval(parse(text = paste0("subset(db,", temp_ae_criterion, ")")))
      
      res_new <- res_new %>% select(treatment, ae, stratum, USUBJID) %>%
        complete(treatment, ae, stratum) %>%    # fill it with 0
        group_by(treatment, ae, stratum) %>%             # group_by(treatment, ae) %>%
        summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>%  # summarise(n = n()) %>%
        mutate(ae_label = temp_ae_label) %>%    # give a label to the AE without filter
        left_join(db_N) %>%
        mutate(pct = n / N * 100) %>%
        ungroup() %>%
        mutate(trtn = as.numeric(treatment)) %>%
        select(- treatment) %>%
        pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) 
      
      ## Union the filtered AE with the old data
      res <- dplyr::union_all(res, res_new)
    }
    
    res <- res %>% mutate(across(pct_1 : pct_2, ~ round(.x, digits = 4)))
    
    # sort the output returns
    #listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
    list(table = res, 
         listing = db_listing, #listing = db[, listing_var],
         sample_size = db_N,
         treatment_order = trt_order)
  }
  
  # compare the actual result with expectation table:
  a <- tidy_ae_table(population_from = adsl %>% rename(TRTA = TRT01A),
                     observation_from = adae,
                     population_where = "ITTFL=='Y'",
                     observation_where = NULL,
                     treatment_var = "TRTA",
                     stratum_var = "STRATUMN",
                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                     ae_var = "AEDECOD",
                     ae_interested = define_ae_select_list(ae_criterion = 'AESER == "Y"', ae_label = "with serious adverse events"),
                     listing_interested = define_ae_listing(listing_var = c("USUBJID", "SEX", "RACE", "AGE"),
                                                     listing_label = c("ID", "Gender", "Race", "Age")))
  b <- expectation_table(data1 = adsl %>% rename(TRTA = TRT01A),
                         data2 = adae,
                         pop_where = "ITTFL=='Y'",
                         obs_where = NULL,
                         trt_var = "TRTA",
                         str_var = "STRATUMN",
                         trt_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
                         ae_var = "AEDECOD",
                         ae_instd = list(ae_criterion = 'AESER == "Y"', ae_label = "with serious adverse events"),
                         listing_instd = define_ae_listing(listing_var = c("USUBJID", "SEX", "RACE", "AGE"),
                                                         listing_label = c("ID", "Gender", "Race", "Age")))
  expect_equal(a$sample_size, b$sample_size)
  expect_equal(a$treatment_order, b$treatment_order)
  expect_equal(a$table, b$table)
  expect_equal(a$listing, b$listing)
})

