#' Output a data frame with the listed details in the reactable
#'
#' @param db the data frame, whose subset will servers as the listed details in the reactable
#' @param listing_var a character vector specifying the variable names to be displayed in the listed details
#' @param listing_label a character vector specifying the variable labels to be displayed in the listed details
#' @return a data frame with selected/renamed variables
#' @export
#'
#' @examples
#' library(tibble)
#' db = tibble(USUBJID = c("01", "01", "02", "03", "03", "03", "04", "04", "05"),
#'             SITEID = c("site1", "site1", "site2", "site3", "site3", "site3", "site4", "site4", "site5"),
#'             SEX = c("F", "F", "M", "F", "F", "F", "M", "M", "F"),
#'             AGE = c(65, 65, 30, 22, 22, 22, 70, 70, 53),
#'             RACE = c("Asian", "Asian", "Latino or Hispanic",
#'                      "WHITE", "WHITE", "WHITE",
#'                      "BLACK OR AFRICAN AMERICAN",
#'                      "BLACK OR AFRICAN AMERICAN",
#'                      "AMERICAN INDIAN OR ALASKA NATIVE"),
#'             ADURN = c(11, 3, 4, 5, 7, 20, 90, 34, 58),
#'             ADURU = rep("minutes", 9))
#' 
#' detail1 <- tidy_ae_listing(db, listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE"),
#'                            listing_label = c("ID", "Site Number", "Sex", "Race", "Age"))
#' detail2 <- tidy_ae_listing(db, listing_var = c("USUBJID", "SEX", "ADURN", "ADURU"),
#'                            listing_label = c("ID", "Gender", "Duration", "Duration"))
#' detail3 <- tidy_ae_listing(db, listing_var = c("USUBJID", "ADURN", "ADURU", "AGE"),
#'                            listing_label = c("ID", "Duration", "Duration", "Age"))


tidy_ae_listing <- function(db, listing_var = NULL, listing_label = NULL){

  # Make sure the listing_var are included in the db
  for (i in seq_along(listing_var)) {
    if(! listing_var[i] %in% colnames(db)){
      stop(paste("The ", i, "-th listing_var is not a validate colname of the input dataset!"))
    }
  }
  
  # Initialize db_listing
  n_var <- length(listing_var)
  n_label <- length(listing_label)
  db_listing <- db[, listing_var]
  
  # Refine db_listing, namely refine the labels of variables
  i = 1
  while (i <= n_var) {
    # Check if there is duplicated listing_labels
    # For example, "ADURN" and "ADURU" share the same label, i.e., "Duration"
    duplicate_idx = 0
    for (j in n_var:(i+1)){
      if(listing_label[j] == listing_label[i]){
        duplicate_idx = j
        break
      }
    }
    # Discuss two scenario
    # 1) if there is duplicated labels
    # 2) if there is not duplicated labels
    if(duplicate_idx == 0){
      db_listing[[listing_label[i]]] <- stringr::str_to_title(db_listing[[listing_var[i]]])
      db_listing <- db_listing %>% dplyr::select(-listing_var[i])
      i = i + 1
    }else{
      db_listing[[listing_label[i]]] <- stringr::str_to_title(do.call(paste, c(db[listing_var[i:duplicate_idx]], sep = " ")))
      db_listing <- db_listing %>% dplyr::select(- listing_var[i:duplicate_idx])
      i = duplicate_idx + 1
    }
  }
  
  return(db_listing) 
}