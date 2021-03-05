#' tidy_observation
#'
#' Select the Desired Observation
#'
#' @param observation_from a variable to define the observation dataset
#' @param observation_where
#' @param treatment_var
#' @param treatment_order
#'
#' @return a table of observation information
#' @export
#'
#' @examples
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

