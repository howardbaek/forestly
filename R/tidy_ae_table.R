#' tidy_ae_table
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
#' @importFrom stats aggregate reshape
#' @examples
#' db <- tidy_ae_table(population_from  = adsl %>% rename(TRTA = TRT01A),
#'                     observation_from = adae,
#'                     treatment_var = "TRTA",
#'                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                     listing_var = c("SITEID", "USUBJID", "AGE", "RACE", "SEX",
#'                                     "AETERM", "AESER", "AEREL", "AEACN", "AEOUT") )
tidy_ae_table <- function(population_from,
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
  db <- subset(db, db$USUBJID %in% pop$USUBJID)
  
  # Yilong: will remove dplyr, tiyer dependency
  #db_N <- count(pop, .data$treatment, .data$stratum, name = "N")
  db_N <- aggregate(formula = USUBJID ~ treatment + stratum,
                    data = pop,
                    FUN=length)
  names(db_N)[names(db_N)=="USUBJID"] <- 'N'
  
  #res <- db %>% group_by(.data$treatment, .data$ae) %>%
  #  summarise(n = n_distinct(USUBJID)) %>%
  res <- aggregate(formula = USUBJID ~ treatment + ae, 
                   data = db, 
                   FUN = function(x) length(unique(x)))
  res <- res[order(res$treatment),]
  names(res)[names(res)=="USUBJID"] <- 'n'
  
  #  left_join(db_N) %>%
  res <- merge(x=res,y=db_N,by='treatment',all.x = TRUE)
  
  #  mutate(pct = n / .data$N * 100) %>%
  res['pct'] <- res$n / res$N * 100
  
  #  ungroup() %>%
  #  mutate(trtn = as.numeric(.data$treatment)) %>%
  res['trtn'] <- as.numeric(res$treatment)
  
  #  select(- .data$treatment) %>%
  res <- res[, !names(res) %in% c("treatment")] 
  
  #  pivot_wider(names_from = .data$trtn, values_from = c(n, .data$N, .data$pct), values_fill = 0) %>%
  res <- reshape(data = res, direction = "wide",
                 timevar = "trtn",
                 idvar = c("ae", "stratum"),
                 v.names = c("n", "N", "pct"))
  res[is.na(res)]<-0
  res <- res[,c('ae', 'stratum', 'n.1', 'n.2', 'N.1', 'N.2', 'pct.1', 'pct.2')]
  
  #  mutate(across(starts_with("N", ignore.case = FALSE), ~ max(.x)))
  res$N.1 <- max(res$N.1)
  res$N.2 <- max(res$N.2)
  
  attr(res$ae,'label') <- attr(db$ae,'label')
  attr(res$stratum,'label') <- attr(pop$stratum,'label')
  
  names(res)[names(res)=='n.1'] <- 'n_1'
  names(res)[names(res)=='n.2'] <- 'n_2'
  names(res)[names(res)=='N.1'] <- 'N_1'
  names(res)[names(res)=='N.2'] <- 'N_2'
  names(res)[names(res)=='pct.1'] <- 'pct_1'
  names(res)[names(res)=='pct.2'] <- 'pct_2'
  
  rownames(res) <- NULL
  class(res) <- NULL

  listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
  list(table = res, listing = db[, listing_var])
}
