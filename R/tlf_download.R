#' Title
#'
#' @param path The path where the tlf generated tlf_ae_summary or tlf_ae_specific is stored
#' @param output_name The folder name of the downloaded zip file
#' @param button_label The name to be displayed on the download button
#'
#' @return a downloaded zip folder
#' @export
#'
#' @examples
#' library(dplyr)
#' temp_dir <- tempdir()
#' tlf_ae_summary(population_from = adsl %>% rename(TRTA = TRT01A),
#'                observation_from = adae,
#'                population_where = "ITTFL=='Y'",
#'                observation_where = NULL,
#'                treatment_var = "TRTA",
#'                treatment_order = c("MK9999" = "Xanomeline High Dose", "Placebo" = "Placebo"),
#'                ae_var = "AEDECOD",
#'                ae_interested = define_ae_select_list(ae_criterion = c('AESER == "Y"', 'AEREL != "NONE"'),
#'                                                      ae_label = c("with serious adverse events",
#'                                                                   "with drug-related adverse events")),
#'                stratum_var = "STRATUMN", 
#'                display_ci = TRUE,
#'                display_total = FALSE,
#'                title_text = "Analysis of Adverse Event Summary",
#'                subtitle_text = NULL,
#'                end_notes = "Every subject is counted a single time for each applicable row and column.",
#'                output_name = file.path(temp_dir, 'ae0summary.rtf'))
#' tlf_download(path = temp_dir,
#'              output_name = "AE summary table", 
#'              button_label = "Download AE summary table")
tlf_download <- function(path, output_name = "AE TLF", button_label = "Download AE TLF"){
  
  if(is.null(path)) stop("Please give the path to download files!")
  
  download_file(
    path = list.files(path = path, full.names = TRUE),
    output_name = output_name,
    button_label = button_label,
    button_type = "danger",
    has_icon = TRUE,
    icon = "fa fa-save",
    self_contained = FALSE
  )
}


