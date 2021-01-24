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
