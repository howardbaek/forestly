#' tidy_ae_table
#' 
#' This function is used to obtain AE information ready for visualization
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
#' @param listing_var  A character string to define the criteria to select the column of the table
#'
#' @return Return a standard adverse event data frame
#' @export
#' @examples
#' library(dplyr)
#' db <- tidy_ae_table(population_from  = adsl %>% rename(TRTA = TRT01A),
#'                     observation_from = adae,
#'                     population_where = NULL,
#'                     observation_where = NULL,
#'                     treatment_var = "TRTA",
#'                     treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                     ae_var = "AEDECOD",
#'                     ae_interested = define_ae_select_list(
#'                              ae_criterion = c('AESER == "Y"', 'AEREL != "NONE"'),
#'                              ae_label = c("with serious adverse events",
#'                                           "with drug-related adverse events")),
#'                     listing_var = c("USUBJID", "SEX", "RACE", "AGE"))

tidy_ae_table <- function(population_from,
                          observation_from,
                          population_where = "ITTFL=='Y'",
                          observation_where = NULL,
                          treatment_var = "TRTA",
                          treatment_order = unique(population_from[[treatment_var]]),
                          ae_var = ae_var,
                          ae_interested = NULL,
                          stratum_var = NULL,
                          listing_interested = define_ae_listing()){

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
                         treatment_order  = treatment_order,
                         stratum_var      = stratum_var)
  
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
  res <- tidy_multi_ae_label(db, db_N, ae_interested)
  
  # Title Case the cell values
  res$ae <- tools::toTitleCase(tolower(res$ae))
  
  # sort the output returns
  #listing_var <- unique(c("USUBJID", "ae", "treatment", listing_var))
  list(table = res, 
       listing = db_listing, #listing = db[, listing_var],
       sample_size = db_N,
       treatment_order = treatment_order)
}
