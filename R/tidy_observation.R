#' tidy_observation
#'
#' Select the Desired Observation
#'
#' @param observation_from The input observation data table. For example, "adverse events information" from the table "adae".
#' @param observation_where Tells the function which rows of the table should be select, according to the value of certain column. For example, subset(observation_from,observation_where).
#' @param treatment_var The function will create a new column called "treatment" based on the column of this parameter. For example, the "treatment" column will be equal to "TRTA".
#' @param treatment_order Tells the function which rows of the table should be select, only if the values in "treatment" is in this parameter. It also provide the label names of the treatments after turning them into factors. For example, include rows which treatment = "Placebo" and treatment = "Xanomeline High Dose", and change the factor labels into "Placebo" and "MK9999".
#'
#' @return a table of observation information
#' @export
#'
#' @examples
#' db <- tidy_observation(observation_from = adae,
#'  observation_where = NULL,
#'  treatment_var    = "TRTA",
#'  treatment_order  = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"))
#'
#'
tidy_observation <- function(observation_from,
                             observation_where,
                             treatment_var    = treatment_var,
                             treatment_order  = treatment_order
){

  ##################################
  # End of Input Checking
  ##################################

  if(is.null(observation_where)){ observation_where <- TRUE}

  # Select the Desired Population
  db <- eval(parse(text = paste0("subset(observation_from,", observation_where, ")")))

  # Define treatment group
  db[["treatment"]] <- db[[treatment_var]]
  db <- subset(db, treatment %in% treatment_order)

  # Define treatment label
  if (!is.null(names(treatment_order))) {
    label_name = names(treatment_order)
  } else {label_name = treatment_order}

  db[["treatment"]] <- factor(db[["treatment"]], levels = treatment_order, labels = label_name)

  db
}

