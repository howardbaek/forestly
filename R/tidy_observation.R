#' tidy_observation
#'
#' This function is used to prepare the observation datasets for the use in function tidy_ae_table().
#' The function filter the 'adae' dataset, and define a 'treatment' in the dataset.
#'
#'
#' @param observation_from The input observation data table. For example, "adverse events information" from the table "adae".
#' @param observation_where A character string to define the criteria to select analysis observation.
#' @param treatment_var The function will create a new column called "treatment" based on the column of this parameter. For example, the "treatment" column will be equal to "TRTA".
#' @param treatment_order Tells the function which rows of the table should be select, only if the values in "treatment" is in this parameter. It also provide the label names of the treatments after turning them into factors. For example, include rows which treatment = "Placebo" and treatment = "Xanomeline High Dose", and change the factor labels into "Placebo" and "MK9999".
#'
#' @return an output data frame of observation information.
#'
#' STUDYID: Study identifier.
#' SITEID: Study site identifier.
#' USUBJID: Unique subject identifier.
#' TRTA: Actual treatment.
#' TRTAN: Actual treatment (N).
#' AGE: Age.
#' AGEGR1: Pooled age group 1.
#' AGEGR1N: Pooled age group 1 (N).
#' RACE: Race.
#' RACEN: Race (N).
#' SEX: Sex.
#' SAFFL: Safety population flag.
#' TRTSDT: Date of first exposure to treatment.
#' TRTEDT: Date of last exposure to treatment.
#' ASTDT: Analysis start date.
#' ASTDTF: Analysis start date imputation flag.
#' ASTDY: Analysis start relative day.
#' AENDT: Analysis end date.
#' AENDY: Analysis end relative day.
#' ADURN: AE duration (N).
#' ADURU: AE duration units.
#' AETERM: Reported term for the adverse event.
#' AELLT: Lowest level term.
#' AELLTCD: Lowest level term code.
#' AEDECOD: Dictionary-derived term.
#' AEPTCD: Preferred term code.
#' AEHLT: High level term.
#' AEHLTCD: High level term code.
#' AEHLGT: High level group term.
#' AEHLGTCD: High level group term code.
#' AEBODSYS: Body system or organ class.
#' AESOC: Primary system organ class.
#' AESOCCD: Primary system organ class code.
#' AESEV: Severity/intensity.
#' AESER: Serious event.
#' AESCAN: Involves cancer.
#' AESCONG: Congenital anomaly or birth defect.
#' AESDISAB: Persist or signif disability/incapacity.
#' AESDTH: Results in death.
#' AESHOSP: Requires or prolongs hospitalization.
#' AESLIFE: Is life threatening.
#' AESOD: Occurred with overdose.
#' AEREL: Causality.
#' AEACN: Action taken with study treatment.
#' AEOUT: Outcome of adverse event.
#' AESEQ: Sequence Number.
#' TRTEMFL: Treatment emergent analysis flag.
#' AOCCFL: 1st occurrence of any AE flag.
#' AOCCSFL: 1st occurrence of SOC flag.
#' AOCCPFL: 1st occurrence of preferred term flag.
#' AOCC02FL: 1st occurrence 02 flag for serious.
#' AOCC03FL: 1st occurrence 03 flag for serious SOC.
#' AOCC04FL: 1st occurrence 04 flag for serious PT.
#' CQ01NAM: Customized query 01 name.
#' AOCC01FL: 1st occurrence 01 flag for CQ01.
#' treatment: Actual treatment.
#'
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

