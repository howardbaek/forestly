#' To formate the listing variables and labels
#' For example, if "AESER" and "AEREF" are selected as the listing variable
#' Then, "Intensity" and "Related" can serve as listing label
#' 
#' @param listing_var a character vector specifying the variable names to be displayed in the listed details
#'                    For example, 
#'                    1) listing_var = c("USUBJID", "SEX")
#'                    2) listing_var = c("USUBJID", "SEX", "ADURN", "ADURU")
#' @param listing_label a string vector to specify the according AE labels to be displayed in the selected list.
#'                    Please note that, if variables contribute to the same listing, their label should be the same.
#'                    For example, when listing_var = c("ADURN", "ADURU"), 
#'                    listing_label should be listing_label = c("Duration", "Duration"), 
#' @return a list including 
#'         (1) the listing_var
#'         (3) the listing_label
#' @export
#'
#' @examples 
#' define_ae_listing(listing_var = c("USUBJID", "SITEID", "SEX", "RACE", "AGE"),
#'                   listing_label = c("ID", "Site Number", "Sex", "Race", "Age"))
#' define_ae_listing(listing_var = c("USUBJID", "SEX", "ADURN", "ADURU"),
#'                   listing_label = c("ID", "Gender", "Duration", "Duration"))
#' define_ae_listing(listing_var = c("USUBJID", "ADURN", "ADURU", "AGE"),
#'                   listing_label = c("ID", "Duration", "Duration", "Age"))                           
#' 
define_ae_listing <- function(listing_var = NULL,
                              listing_label = NULL){
  # Check if the length of listing_var matches that of listing_label
  if(length(listing_var) != length(listing_label)){
    stop("The length of the listing_var must match that of listing_label!")
  }
  
  # Set the default value of listing_var
  if(is.null(listing_var) && is.null(listing_label)){
    listing_var = c("USUBJID", "SEX", "RACE", "AGE")
    listing_label = c("Participant ID", "Gender", "Race", "Age")
  }
  
  list(listing_var = listing_var,
       listing_label = listing_label)
}