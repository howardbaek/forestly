#' To formate the interested ae and its filter
#' For example, if the interested ae are c("AESER", "AEREF"), 
#' then their accoding filters can be defined as c("N", "None") 
#' to filter OUT the NONE-SERIOUS ae and NONE-DRUG-RELATED AE
#' @param ae_criterion a character vector to specify the AE criterion to filter the interested ones.
#'                     For example, 
#'                     1) ae_criterion = c('AESER != "N"', 'AEREL != "None"') 
#'                     2) ae_criterion = c('AESER != "N"', 'AEREL != "None"', 'Grade3To5Flag != "Y"', 'AEACN != "DRUG WITHDRAWN"')
#' @param ae_label a string vector to specify the according AE labels to be displayed in the selected list.
#'                 The default value is c("with serious adverse events", "with drug-related adverse events",
#'                                        "with toxicity grade 3-5 adverse events","discontinued due to an adverse event")

#' @return a list including 
#'         (1) the ae filters
#'         (3) the ae labels
#' @export
#'
#' @examples 
#' ae_interested(ae_criterion = 'AESER != "N"', ae_label = "with serious adverse events")
#' ae_interested(ae_criterion = c('AESER != "N"', 'AEREL != "None"'),
#'               ae_label = c("with serious adverse events", "with drug-related adverse events"))
#' 
ae_interested <- function(ae_criterion = NULL,
                          ae_label = NULL){
  
  if(is.null(ae_criterion)){
    stop("Please list the ae_criterion!")
  }
  
  if(is.null(ae_label)){
    stop("Please list the ae_label!")
  }
  
  if(length(ae_criterion) != length(ae_label)){
    stop("the length of ae_criterion doesn't match that of ab_label!")
  }
  
  list(interested_ae_criterion = ae_criterion,
       interested_ae_label = ae_label)
}