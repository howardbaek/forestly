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
#' treatment_order = c("MK9999" = "Xanomeline", "Placebo" = "Placebo")
#' 
#' db = tibble::tibble(USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
#'                     treatment = factor(c("Xanomeline", "Xanomeline", "Placebo", "Placebo", "Placebo", 
#'                                           "Placebo", "Xanomeline", "Xanomeline", "Xanomeline"), 
#'                                        levels = treatment_order, labels = treatment_order),
#'                     ae = c("headache", "pain", "headache", "fever", "running nose", 
#'                            "pain", "headache", "fever", "headache"),
#'                     AESER = c("N", "N", "N", "Y", "N", "N", "N", "Y", "N"),
#'                     AEREL = c("N", "Y", "N", "N", "N", "Y", "N", "N", "Y"))
#'                     
#' db_N = tibble::tibble(treatment = factor(c("Xanomeline", "Placebo"), 
#'                                          levels = treatment_order, 
#'                                          labels = treatment_order), 
#'                       N = c(3, 2))
#' 
#' tidy_multi_ae_label(db, db_N, ae_interested = c("AESER", "AEREL"))


tidy_multi_ae_label <- function(db, db_N, ae_interested){
  
  ## Start with all AE
  res <- db %>% group_by(treatment, ae) %>%
    # summarise(n = n()) %>%
    summarise(n = n_distinct(USUBJID)) %>%
    # give a label to the AE without filter
    mutate(ae_label = "NONE") %>%              
    left_join(db_N) %>%
    mutate(pct = n / N * 100) %>%
    ungroup() %>%
    mutate(trtn = as.numeric(treatment)) %>%
    select(- treatment) %>%
    pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
    mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x)))
  
  ## For each interested AE, iteration once and rbind them together
  for (ae_idx in seq_along(ae_interested)) {
    
    ## Decide the filer according to the interested AE
    if(ae_interested[ae_idx] == "AESER"){
      temp_ae_filter = "N"
    }else if(ae_interested[ae_idx] == "AEREL"){
      temp_ae_filter = "NONE"
    }else if(ae_interested[ae_idx] == "Grade3To5Flag"){
      temp_ae_filter = "Y"
    }else if(ae_interested[ae_idx] == "AEACN"){
      temp_ae_filter = "DRUG WITHDRAWN"
    }else{
      stop("This is not a commonly interested AE structure, please enter AE structure from the candidates c(AESER, AEREL, AEACN, Grade3To5Flag)")
    }
    
    ## Filter the interested AE
    res_new <- db %>% subset(eval(parse(text = paste0(ae_interested[ae_idx]))) != temp_ae_filter) %>%
      group_by(treatment, ae) %>%
      # summarise(n = n()) %>%
      summarise(n = n_distinct(USUBJID)) %>%
      # give a label to the AE without filter
      mutate(ae_label = ae_interested[ae_idx]) %>%
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(- treatment) %>%
      pivot_wider(names_from = trtn, values_from = c(n, N, pct), values_fill = 0) %>%
      mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x))) 
    
    ## Union the filtered AE with the old data
    res <- union_all(res, res_new)
    
    ## It is possible that only treatment or control arm has the interested AE.
    ## So for the arm without the interested AE, we fill the AE case as 0 and also fill in the arm sample size
    res$N_1[is.na(res$N_1)] = db_N$N[as.numeric(db_N$treatment) == 1] 
    res$N_2[is.na(res$N_2)] = db_N$N[as.numeric(db_N$treatment) == 2] 
    res$n_1[is.na(res$n_1)] = 0
    res$n_2[is.na(res$n_2)] = 0
    res$pct_1[is.na(res$pct_1)] = 0
    res$pct_2[is.na(res$pct_2)] = 0
  }
  
  return(res)
}
