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


