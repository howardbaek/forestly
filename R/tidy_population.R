#' tidy_population
#'
#' Get population level tidy data
#'
#' @param population_from The input population level data table. For example, "subject level information" from the table "adsl".
#' @param population_where Tells the function which rows of the table should be select, according to the value of certain column. For example, select subset where "ITTFL=='Y'.
#' @param treatment_var The function will create a new column called "treatment" based on the column of this parameter. For example, the "treatment" column will be equal to "TRT01A".
#' @param treatment_order Tells the function which rows of the table should be select, only if the values in "treatment" is in this parameter. It also provide the label names of the treatments after turning them into factors. For example, include rows which treatment = "Placebo" and treatment = "Xanomeline High Dose", and change the factor labels into "Placebo" and "MK9999".
#' @param stratum_var The function will create a new column called "stratum" based on this parameter.
#' @param baseline_var Tells the function which column of the table to be selected as the final output.
#'
#' @return an output table of population information.
#'
#' @export
#'
#' @examples
#' pop <- tidy_population(population_from  = adsl %>% rename(TRTA = TRT01A),
#'  population_where = "ITTFL=='Y'",
#'  treatment_var    = "TRTA",
#'  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'  stratum_var      = NULL,
#'  baseline_var     = NULL)
#'
#'
tidy_population <- function(population_from,
                            population_where = NULL,
                            treatment_var = "TRT01A",
                            treatment_order = unique(population_from[[treatment_var]]),
                            stratum_var = NULL,
                            baseline_var = NULL){

  ##################################
  # End of Input Checking
  ##################################

  if(is.null(population_where)){ population_where <- TRUE}

  # Select the Desired Population
  pop <- eval(parse(text = paste0("subset(population_from,", population_where, ")")))

  # Define treatment group
  pop[["treatment"]] <- pop[[treatment_var]]
  pop <- subset(pop, treatment %in% treatment_order)

  # Define treatment label
  if (!is.null(names(treatment_order))) {
    label_name = names(treatment_order)
  } else {label_name = treatment_order}

  pop[["treatment"]] <- factor(pop[["treatment"]], levels = treatment_order, labels = label_name)

  # Define stratum
  if(length(stratum_var) == 0){
    pop[["stratum"]] <- "All"
  }else{
    pop[["stratum"]] <- pop[[stratum_var]]
  }

  pop <- pop[, unique(c("USUBJID", "treatment", "stratum", baseline_var))]

  pop
}

