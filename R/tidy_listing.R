#' Output a data frame with the listed details in the reactable
#'
#' @param db the data frame, whose subset will servers as the listed details in the reactable
#' @param listing_var a character vector specifying the variable names to be displayed in the listed details
#'
#' @return a data frame with selected/renamed variables
#' @export
#'
#' @examples
#' library(tibble)
#' treatment_order = c("MK9999" = "Xanomeline", "Placebo" = "Placebo")
#' db = tibble(USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
#'             SITEID = c("a1", "a1", "b2", "c3", "c3", "c3", "d4", "d4", "e5"),
#'             SEX = c("F", "F", "M", "F", "F", "F", "M", "M", "F"),
#'             AGE = c(65, 65, 30, 22, 22, 22, 70, 70, 53),
#'             RACE = c("Asian", "Asian", "Latino or Hispanic", "WHITE", "WHITE", "WHITE",
#'                      "BLACK OR AFRICAN AMERICAN", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"),
#'             treatment = factor(c("Xanomeline", "Xanomeline", "Placebo", "Placebo", "Placebo",
#'                                  "Placebo", "Xanomeline", "Xanomeline", "Xanomeline"),
#'                                levels = treatment_order, labels = treatment_order),
#'             ae = c("headache", "pain", "headache", "fever", "running nose",
#'                    "pain", "headache", "fever", "headache"),
#'             ADURN = c(11, 3, 4, 5, 7, 20, 90, 34, 58),
#'             ADURU = rep("minutes", 9),
#'             AESEV = c(rep("MILD", 3), rep("MODERATE", 3), rep("SEVERE", 3)),
#'             AESER = c("N", "N", "N", "Y", "N", "N", "N", "Y", "N"),
#'             AEREL = c("N", "Y", "N", "N", "N", "Y", "N", "N", "Y"),
#'             AEACN = c(rep("DOSE NOT CHANGED", 3),  rep("DRUG INTERRUPTED", 3), rep("DRUG WITHDRAWN", 3)),
#'             AEOUT = c(rep("NOT RECOVERED/NOT RESOLVED", 3), rep("RECOVERED/RESOLVED", 3), rep("FATAL", 3))
#' )
#' 
#' detail1 <- tidy_listing(db, listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE"))
#' detail2 <- tidy_listing(db, listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE", "ADURN", "ADURU"))
#' detail3 <- tidy_listing(db, listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE", "ADURN", "ADURU", "AESEV"))


tidy_listing <- function(db, listing_var = NULL){
   
  # Set the default value of listing_var
  if(is.null(listing_var)){
    listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE",  # personal information
                    "TRTA", # Onset Epoch, i.e., treatment or control
                    "ADURN", "ADURU", # duration time and unit
                    "AESEV", # Intensity: c("MILD", "MODERATE", "SEVERE")
                    "AESER", # Serious event: c("N", "Y")
                    "AEREL", # Related: c("PROBABLE", "REMOTE", "POSSIBLE", "NONE")
                    "AEACN", # Action taken: c("DOSE NOT CHANGED", "DRUG INTERRUPTED", "DRUG WITHDRAWN")
                    "AEOUT" # Outcome: c("NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED", "FATAL" )
                    )
  }

  listing_var <- unique(c("ae", "treatment", listing_var))
  
  # Make sure the listing_var are included in the db
  for (i in seq_along(listing_var)) {
    stopifnot(listing_var[i] %in% colnames(db))
  }
  
  # Make sure "ADURN" and "ADURU" appear in pairs
  if(("ADURN" %in% listing_var) || ("ADURU" %in% listing_var)){
    stopifnot(("ADURN" %in% listing_var) && ("ADURU" %in% listing_var))
  }
  
  db_listing <- db[, listing_var]
  
  selected_var <- NULL
  
  # Rename the SITEID
  if("SITEID" %in% listing_var){
    db_listing[["Site Number"]] <- db_listing[["SITEID"]] 
    db_listing <- db_listing %>% dplyr::select(-SITEID)
    selected_var <- c(selected_var, "Site Number")
  }
  
  # Rename the USUBJID
  if("USUBJID" %in% listing_var){
    db_listing[["Participant ID"]] <- db_listing[["USUBJID"]] 
    db_listing <- db_listing %>% dplyr::select(-USUBJID)
    selected_var <- c(selected_var, "Participant ID")
  }
  
  # Rename the SEX
  if("SEX" %in% listing_var){
    db_listing <- db_listing %>% dplyr::rename(Gender = SEX)
    selected_var <- c(selected_var, "Gender")
  }
  
  # Rename the RACE
  if("RACE" %in% listing_var){
    db_listing$RACE <- tools::toTitleCase(tolower(db_listing$RACE))
    db_listing <- db_listing %>% dplyr::rename(Race = RACE)
    selected_var <- c(selected_var, "Race")
  }
  
  # Rename the AGE
  if("AGE" %in% listing_var){
    db_listing <- db_listing %>% dplyr::rename(Age = AGE)
    selected_var <- c(selected_var, "Age")
  }
  
  # Rename AETERM
  if("AETERM" %in% listing_var){
    db_listing$AETERM <- tools::toTitleCase(tolower(db_listing$AETERM))
    db_listing <- db_listing %>% dplyr::rename(AE = AETERM)
    selected_var <- c(selected_var, "AE")
  }
  
  # Rename the TRTA
  if("TRTA" %in% listing_var){
    db_listing[["Onset Epoch"]] <- db_listing[["TRTA"]] 
    db_listing <- db_listing %>% dplyr::select(-TRTA)
    selected_var <- c(selected_var, "Onset Epoch")
  }
  
  # Rename the ADURN + ADURU
  if("ADURN" %in% listing_var){
    db_listing <- db_listing %>% dplyr::mutate(Duration = tools::toTitleCase(tolower(do.call(paste, c(db[c("ADURN", "ADURU")], sep = " "))))) %>%
      dplyr::select(- c("ADURN", "ADURU"))
    selected_var <- c(selected_var, "Duration")
  }
  
  # Rename the AESEV
  if("AESEV" %in% listing_var){
    db_listing$AESEV <- tools::toTitleCase(tolower(db_listing$AESEV))
    db_listing <- db_listing %>% dplyr::rename(Intensity = AESEV)
    selected_var <- c(selected_var, "Intensity")
  }
  
  # Rename the AESER
  if("AESER" %in% listing_var){
    db_listing <- db_listing %>% dplyr::rename(Serious = AESER)
    selected_var <- c(selected_var, "Serious")
  }
  
  # Rename the AEREL
  if("AEREL" %in% listing_var){
    db_listing$AEREL <- tools::toTitleCase(tolower(db_listing$AEREL))
    db_listing <- db_listing %>% dplyr::rename(Related = AEREL)
    selected_var <- c(selected_var, "Related")
  }
  
  # Rename the AEACN
  if("AEACN" %in% listing_var){
    db_listing$AEACN <- tools::toTitleCase(tolower(db_listing$AEACN))
    db_listing[["Action Taken"]] <- db_listing[["AEACN"]] 
    db_listing <- db_listing %>% dplyr::select(-AEACN)
    selected_var <- c(selected_var, "Action Taken")
  }
  
  # Rename the AEOUT
  if("AEOUT" %in% listing_var){
    db_listing$AEOUT <- tools::toTitleCase(tolower(db_listing$AEOUT))
    db_listing <- db_listing %>% dplyr::rename(Outcome = AEOUT)
    selected_var <- c(selected_var, "Outcome")
  }
  
  db_listing %>% select(selected_var)
  
}