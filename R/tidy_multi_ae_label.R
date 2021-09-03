#' This is a function to rbind data together according to the interested ae labels
#'
#' @param db a data frame which is usually an intersection of adae and adsl, 
#'           with the ae variable name (usually encoded as "AEDECOD") transformed into "ae", 
#'           and treatment variable name (usually encoded as "TRTA") transformed into "treatment",
#'           see the following example for the format of db
#' @param db_N a data frame contains the sample size of each arm
#' @param ae_interested A character string including the interested AE structure, ex., AESER, AEREL...
#'
#' @return a data frame which rbind data together according to the interested ae labels
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' treatment_order = c("MK9999" = "Xanomeline", "Placebo" = "Placebo")
#' 
#' db = tibble(USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
#'                     treatment = factor(c("Xanomeline", "Xanomeline", "Placebo", "Placebo", "Placebo", 
#'                                           "Placebo", "Xanomeline", "Xanomeline", "Xanomeline"), 
#'                                        levels = treatment_order, labels = treatment_order),
#'                     ae = c("headache", "pain", "headache", "fever", "running nose", 
#'                            "pain", "headache", "fever", "headache"),
#'                     AESER = c("N", "N", "N", "Y", "N", "N", "N", "Y", "N"),
#'                     AEREL = c("N", "Y", "N", "N", "N", "Y", "N", "N", "Y"))
#'                     
#' db_N = tibble(treatment = factor(c("Xanomeline", "Placebo"), 
#'                                          levels = treatment_order, 
#'                                          labels = treatment_order), 
#'                       N = c(3, 2))
#' 
#' tidy_multi_ae_label(db, db_N, ae_interested = ae_interested(ae_criterion = c('AESER == "Y"', 'AEREL != "N"'),
#'                                                             ae_label = c("with serious adverse events","with drug-related adverse events")))

tidy_multi_ae_label <- function(db, db_N, ae_interested){
  
  interested_ae_criterion <- ae_interested$interested_ae_criterion
  interested_ae_label <- ae_interested$interested_ae_label
  
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
  return(res)
}
