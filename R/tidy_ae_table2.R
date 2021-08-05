#' tidy_ae_table2
#'
#' Define analysis data
#' This function is used to obtain AE information ready for visualization
#'
#'
#' @param population_from A data frame to obtain population level variables.Typically an 'adsl' dataset.
#' @param observation_from A data frame to obtain observation level variables.
#' @param population_where A character string to define the criteria to select analysis population.
#' @param observation_where A character string to define the criteria to select analysis observation.
#' @param treatment_var A character string to define the variable of new column called "treatment"
#' @param treatment_order A character vector to define the treatment display order and label.
#' @param ae_var A character string to define the variable of new column called ae"
#' @param stratum_var A character string to define the variable of baseline stratum in 'population_from'.Only one 'stratum_var' is allowed.
#' @param listing_var  A character string to define the criteria to select the column of the table
#'
#' @return Return a standard adverse event data frame
#' @export
#' @importFrom rlang .data
#' @examples
#' library(dplyr)
#' library(tidyr)
#' db <- tidy_ae_table2(population_from  = adsl %>% rename(TRTA = TRT01A),
#'                     observation_from = adae,
#'                     treatment_var = "TRTA",
#'                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                     listing_var = c("SITEID", "USUBJID", "AGE", "RACE", "SEX",
#'                                     "AETERM", "AESER", "AEREL", "AEACN", "AEOUT") )
tidy_ae_table2 <- function(population_from,
                          observation_from,
                          population_where = "ITTFL=='Y'",
                          observation_where = NULL,
                          treatment_var = "TRTA",
                          treatment_order = unique(population_from[[treatment_var]]),
                          ae_var = "AEDECOD",
                          stratum_var = NULL,
                          listing_var = names(observation_from)){
  
  # Population Level Tidy Data
  pop <- tidy_population(population_from  = population_from,
                         population_where = population_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order,
                         stratum_var      = stratum_var,
                         baseline_var     = NULL)
  
  # Select the Desired Observation
  db <- tidy_observation(observation_from = observation_from,
                         observation_where = observation_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order)
  
  db[["ae"]] <- db[[ae_var]]
  db <- db %>% filter(.data$USUBJID %in% pop$USUBJID)
  
  # Yilong: will remove dplyr, tiyer dependency
  #db_N <- count(pop, .data$treatment, .data$stratum, name = "N")
  db_N2 <- aggregate(formula = USUBJID ~ treatment + stratum,
                     data = pop,
                     FUN=length)
  names(db_N2)[names(db_N2)=="USUBJID"] <- 'N'
  
  #res <- db %>% group_by(.data$treatment, .data$ae) %>%
  #  summarise(n = n()) %>%
  res2 <- aggregate(formula = STUDYID ~ treatment + ae, 
                    data = db, 
                    FUN = length)
  res2 <- res2[order(res2$treatment),]
  names(res2)[names(res2)=="STUDYID"] <- 'n'
  
  #  left_join(db_N) %>%
  res22 <- merge(x=res2,y=db_N2,by='treatment',all.x = TRUE)
  
  #  mutate(pct = n / .data$N * 100) %>%
  res22['pct'] <- res22$n / res22$N * 100
  
  #  ungroup() %>%
  #  mutate(trtn = as.numeric(.data$treatment)) %>%
  res22['trtn'] <- as.numeric(res22$treatment)
  
  #  select(- .data$treatment) %>%
  res22 <- res22[, !names(res22) %in% c("treatment")] 
  
  #  pivot_wider(names_from = .data$trtn, values_from = c(n, .data$N, .data$pct), values_fill = 0) %>%
  res222 <- reshape(data = res22, direction = "wide",
                     timevar = "trtn",
                     idvar = "ae",
                     v.names = c("n", "N", "pct"))
  res222[is.na(res222)]<-0
  res222 <- res222[,c('ae', 'stratum', 'n.1', 'n.2', 'N.1', 'N.2', 'pct.1', 'pct.2')]
  
  #  mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x)))
  res222$N.1[res222$N.1==0] <- max(res222$N.1)
  res222$N.2[res222$N.2==0] <- max(res222$N.2)
  
  attr(res222$ae,'label') <- c("Dictionary-Derived Term")
  
  names(res222)[names(res222)=='n.1'] <- 'n_1'
  names(res222)[names(res222)=='n.2'] <- 'n_2'
  names(res222)[names(res222)=='N.1'] <- 'N_1'
  names(res222)[names(res222)=='N.2'] <- 'N_2'
  names(res222)[names(res222)=='pct.1'] <- 'pct_1'
  names(res222)[names(res222)=='pct.2'] <- 'pct_2'
  
  rownames(res222) <- NULL
  
  listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
  list(table = res222, listing = db[, listing_var])
}
