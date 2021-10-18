#' ae_summary_table
#' 
#' This function is used to generate AE summary table
#'
#' @param population_from A data frame to obtain population level variables.Typically an 'adsl' dataset.
#' @param observation_from A data frame to obtain observation level variables.
#' @param population_where A character string to define the criteria to select analysis population.
#' @param observation_where A character string to define the criteria to select analysis observation.
#' @param treatment_var A character string to define the variable of new column called "treatment"
#' @param treatment_order A character vector to define the treatment display order and label.
#' @param ae_var A character string to define the variable of new column called ae
#' @param ae_interested An object returned by function define_ae_select_list()
#' @param stratum_var A character string to define the variable of baseline stratum in 'population_from'.Only one 'stratum_var' is allowed.
#' @param display_ci A logic value indicating whether displaying the risk difference, corresponding confidence interval and its p-value. Default is FALSE.
#' @param display_total A logic value indicating whether displaying the total column. Default is FALSE.
#' @param title_text The part of the text string to appear in the title row.
#' @param subtitle_text A vector of text strings to appear in the subtitle row(s).
#' @param end_notes A vector of text strings to appear in the footnote.
#' @param output_name A text string to output the table, can be specified with the path together. If no path specified, the default path will be used to save the table. 
#'
#' @return Return a table for summary of AE
#' @export
#' @importFrom dplyr mutate count summarise select group_by left_join n_distinct ungroup across union_all starts_with
#' @examples
#' library(dplyr)
#' library(r2rtf)
#' ae_summary_table(population_from = adsl %>% rename(TRTA = TRT01A),
#'                 observation_from = adae,
#'                 population_where = "ITTFL=='Y'",
#'                 observation_where = NULL,
#'                 treatment_var = "TRTA",
#'                 treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                 ae_var = "AEDECOD",
#'                 ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL != "NONE"',
#'                                                                        'AEACN == "DRUG WITHDRAWN"'),
#'                                                       ae_label = c("with serious adverse events",
#'                                                                    "with drug-related adverse events",
#'                                                                    "discontinued due to an adverse event")),
#'                 stratum_var = NULL,
#'                 display_ci = TRUE,
#'                 display_total = FALSE,
#'                 title_text = "Analysis of Adverse Event Summary", 
#'                 subtitle_text = NULL,
#'                 end_notes ="Every subject is counted a single time for each applicable row and column.",
#'                 output_name = file.path(tempdir(), 'ae0summary.rtf'))

ae_summary_table <- function(population_from,
                             observation_from,
                             population_where,
                             observation_where,
                             treatment_var,
                             treatment_order,
                             ae_var,
                             ae_interested = NULL,
                             stratum_var = NULL,
                             display_ci = FALSE,
                             display_total = FALSE,
                             title_text, 
                             subtitle_text = NULL,
                             end_notes,
                             output_name = "./tlf_ae_summary.rtf"){
  
  if (display_ci==TRUE&display_total==TRUE) stop('Cannot display difference estimates and total columns together.')
  
  pop <- tidy_population(population_from  = population_from,
                         population_where = population_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order,
                         baseline_var     = NULL)
  
  # Select the Desired Observation
  db <- tidy_observation(observation_from = observation_from,
                         observation_where = observation_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order)
  
  # select the overlap pop(adsl) and db(adae)
  db[["ae"]] <- tools::toTitleCase(tolower(db[[ae_var]])) 
  db <- subset(db, USUBJID %in% pop$USUBJID)
  
  # count the sample size of each arm
  db_N <- dplyr::count(pop, treatment, name = "N")
  total_N <- sum(db_N$N)
  
  res <- db_N %>% 
    mutate(trtn = as.numeric(treatment)) %>%
    select(-treatment) %>%
    mutate(ae_label = "Participants in population") %>%
    data.frame() %>%
    reshape(v.name = "N",
            idvar = "ae_label",
            timevar = 'trtn',
            direction = 'wide',
            sep = '_')
  names(res) <- gsub("N", "n", names(res))
  
  if(display_total){
    res$tot_n <- res$n_1 + res$n_2
  }
  
  if(is.null(ae_interested)){
    return(res)
  }
  
  interested_ae_criterion <- ae_interested$interested_ae_criterion
  interested_ae_label <- ae_interested$interested_ae_label
  
  ## For each interested AE, iteration once and rbind them together
  for (ae_idx in seq_along(interested_ae_criterion)) {
    
    ## Decide the filer according to the interested AE
    temp_ae_criterion <- interested_ae_criterion[ae_idx]
    temp_ae_label <- interested_ae_label[ae_idx]
    
    ## Filter the interested AE
    db_ae_interested <- eval(parse(text = paste0("subset(db,", temp_ae_criterion, ")")))
    res_new <- db_ae_interested %>% group_by(treatment, .drop=FALSE) %>%
      summarise(n = n_distinct(USUBJID)) %>%  
      mutate(ae_label = temp_ae_label) %>%    
      left_join(db_N) %>%
      mutate(pct = n / N * 100) %>%
      ungroup() %>%
      mutate(trtn = as.numeric(treatment)) %>%
      select(- treatment, -N) %>%
      data.frame() %>%
      reshape(v.name = c("n", "pct"),
              idvar = "ae_label",
              timevar = 'trtn',
              direction = 'wide',
              sep = '_')
    
    res_new <- res_new  %>%
      mutate(across(starts_with("pct"), ~ round(.x, digits = 4)))
    if(display_ci){
      n0 <- db_N$N[db_N$treatment[2]]
      n1 <- db_N$N[db_N$treatment[1]]
      x0 <- res_new$n_1
      x1 <- res_new$n_2
      
      stat <- rate_compare_sum(
        n0,n1,x0,x1,
        delta = 0,
        weight = "ss",
        strata = stratum_var,
        test = "one.sided",
        alpha = 0.05
      )
      
      res_new$est <- paste0(round(stat[[1]] *100,1), "(",round(stat[[4]], 1), ", ", round(stat[[5]], 1), ")")
      res_new$pvalue <- round(stat[[3]], 4)
      res_new$pvalue[is.nan(res_new$pvalue)] = NA
    }  
    
    if(display_total){
      res_new$tot_n <- res_new$n_1 + res_new$n_2
      res_new$tot_pct <- round((res_new$n_1 + res_new$n_2)/total_N *100, 1)
    }
    res_new$n_1[is.na(res_new$n_1)] = 0
    res_new$pct_1[is.na(res_new$pct_1)] = 0.0
    res_new$n_2[is.na(res_new$n_2)] = 0
    res_new$pct_2[is.na(res_new$pct_2)] = 0.0
    res <- dplyr::union_all(res, res_new)
  }
  

  tbl_ae_summary <- res %>%
    mutate(across(pct_1 : pct_2, ~ round(.x, digits = 1))) %>%
    select( ae_label, n_1, pct_1, n_2, pct_2, everything())
  # output rtf file
  if(display_total==FALSE&display_ci==FALSE){
    tbl_ae_summary %>%
      r2rtf::rtf_title(title_text, 
                       subtitle_text) %>%
      
      r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," "),
                           col_rel_width = c(3, rep(2, length(unique(pop$treatment))))
      ) %>%
      r2rtf::rtf_colheader(" | n | (%) | n | (%) ",
                           border_top = c("",rep("single",3*length(unique(pop$treatment)))),
                           border_bottom = "single",
                           border_left = c("single", rep(c("single", ""), length(unique(pop$treatment)))),
                           col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))))
      ) %>%
      r2rtf::rtf_body(
        col_rel_width = c(3,  rep(1, 2*length(unique(pop$treatment)))),
        border_left = c("single",rep(c("single",""),length(unique(pop$treatment)))),
        text_justification = c("l", rep("c", 2*length(unique(pop$treatment))))) %>% 
      
      r2rtf::rtf_footnote(end_notes) %>%
      r2rtf::rtf_encode() %>%
      r2rtf::write_rtf(output_name)
  }
  
  if(display_ci){
    tbl_ae_summary %>%
      r2rtf::rtf_title(title_text, 
                       subtitle_text) %>%
      
      r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Difference in % vs ",levels(pop$treatment)[2]," "),
                           col_rel_width = c(3, rep(2, length(unique(pop$treatment))),3)
      ) %>%
      r2rtf::rtf_colheader(" | n | (%) | n | (%) | Estimate (95% CI) | p-value",
                           border_top = c("",rep("single",3*length(unique(pop$treatment)))),
                           border_bottom = "single",
                           border_left = c("single", rep(c("single", ""), length(unique(pop$treatment))),"single","single"),
                           col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))),2,1)
      ) %>%
      
      r2rtf::rtf_body(
        col_rel_width = c(3,  rep(1, 2*length(unique(pop$treatment))),2,1),
        border_left = c("single",rep(c("single",""),length(unique(pop$treatment))),"single","single"),
        text_justification = c("l", rep("c", 3*length(unique(pop$treatment))))) %>% 
      
      r2rtf::rtf_footnote(end_notes) %>%
      r2rtf::rtf_encode() %>%
      r2rtf::write_rtf(output_name)
  }
  if(display_total){
    tbl_ae_summary %>%
      r2rtf::rtf_title(title_text, 
                       subtitle_text) %>%
      
      r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Total "),
                           col_rel_width = c(3, rep(2, length(unique(pop$treatment))),2)
      ) %>%
      r2rtf::rtf_colheader(" | n | (%) | n | (%) | n | (%) ",
                           border_top = c("",rep("single",3*length(unique(pop$treatment)))),
                           border_bottom = "single",
                           border_left = c("single", rep(c("single", ""), length(unique(pop$treatment))),"single","single"),
                           col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))),1,1)
      ) %>%
      
      r2rtf::rtf_body(
        col_rel_width = c(3,  rep(1, 2*length(unique(pop$treatment))),1,1),
        border_left = c("single",rep(c("single",""),length(unique(pop$treatment))),"single","single"),
        text_justification = c("l", rep("c", 3*length(unique(pop$treatment))))) %>% 
      
      r2rtf::rtf_footnote(end_notes) %>%
      r2rtf::rtf_encode() %>%
      r2rtf::write_rtf(output_name)
  }
  
}

