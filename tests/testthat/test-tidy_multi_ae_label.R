library(dplyr)
library(tidyr)
treatment_order <- c("MK9999" = "Xanomeline", "Placebo" = "Placebo")

db <- tibble(
  USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
  stratum = c("a", "a", "b", "c", "c", "c", "b", "b", "a"),
  treatment = factor(c(
    "Xanomeline", "Xanomeline", "Placebo", "Placebo", "Placebo",
    "Placebo", "Xanomeline", "Xanomeline", "Xanomeline"
  ),
  levels = treatment_order, labels = treatment_order
  ),
  ae = c(
    "headache", "pain", "headache", "fever", "running nose",
    "pain", "headache", "fever", "headache"
  ),
  AESER = c("N", "N", "N", "Y", "N", "N", "N", "Y", "N"),
  AEREL = c("N", "Y", "N", "N", "N", "Y", "N", "N", "Y")
)

db_N <- tibble(
  treatment = factor(c("Xanomeline", "Placebo"),
    levels = treatment_order,
    labels = treatment_order
  ),
  N = c(3, 2)
)

test_that("compare tidy_multi_ae_label(db = ..., db_N = ..., ae_interested = NULL) 
          with direct output from line 37-47", {
  res <- db %>%
    select(treatment, ae, stratum, USUBJID) %>%
    complete(treatment, ae, stratum) %>% # fill it with 0
    group_by(treatment, ae, stratum) %>%
    summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>% # summarise(n = n()) %>%
    mutate(ae_label = "All") %>% # give a label to the AE without filter
    left_join(db_N) %>%
    mutate(pct = n / N * 100) %>%
    ungroup() %>%
    mutate(trtn = as.numeric(treatment)) %>%
    select(-treatment) %>%
    pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
    mutate(across(pct_1:pct_2, ~ round(.x, digits = 4)))
  expect_equal(res, tidy_multi_ae_label(db, db_N, ae_interested = NULL))
})


test_that("compare tidy_multi_ae_label(db = ..., db_N = ..., ae_interested = ...) with rbind() logic", {
  res <- db %>%
    select(treatment, ae, stratum, USUBJID) %>%
    complete(treatment, ae, stratum) %>% # fill it with 0
    group_by(treatment, ae, stratum) %>%
    summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>% # summarise(n = n()) %>%
    mutate(ae_label = "All") %>% # give a label to the AE without filter
    left_join(db_N) %>%
    mutate(pct = n / N * 100) %>%
    ungroup() %>%
    mutate(trtn = as.numeric(treatment)) %>%
    select(-treatment) %>%
    pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0)

  ae_interested <- define_ae_select_list(
    ae_criterion = c('AESER == "Y"', 'AEREL != "N"'),
    ae_label = c(
      "with serious adverse events",
      "with drug-related adverse events"
    )
  )

  interested_ae_criterion <- ae_interested$interested_ae_criterion
  interested_ae_label <- ae_interested$interested_ae_label
  for (ae_idx in seq_along(interested_ae_criterion)) {

    ## Decide the filer according to the interested AE
    temp_ae_criterion <- interested_ae_criterion[ae_idx]
    temp_ae_label <- interested_ae_label[ae_idx]
    res_new <- eval(parse(text = paste0("subset(db,", temp_ae_criterion, ")")))

    res_new <- res_new %>%
      select(treatment, ae, stratum, USUBJID) %>%
      complete(treatment, ae, stratum) %>% # fill it with 0
      group_by(treatment, ae) %>% # group_by(treatment, ae) %>%
      summarise(n = n_distinct(USUBJID, na.rm = TRUE)) %>% # summarise(n = n()) %>%
      mutate(ae_label = temp_ae_label) %>% # give a label to the AE without filter
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(-treatment) %>%
      pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0)

    res <- dplyr::union_all(res, res_new) %>%
      mutate(across(pct_1:pct_2, ~ round(.x, digits = 4)))
  }
  expect_equal(
    res,
    tidy_multi_ae_label(db, db_N,
      ae_interested = define_ae_select_list(
        ae_criterion = c('AESER == "Y"', 'AEREL != "N"'),
        ae_label = c(
          "with serious adverse events",
          "with drug-related adverse events"
        )
      )
    )
  )
})
