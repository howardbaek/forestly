plotly_snap <- function(p){
  p_js <- plotly::plotly_json(p, jsonedit = FALSE)
  p_lst <- jsonlite::fromJSON(p_js, simplifyVector = FALSE)
  
  gsub(names(p_lst$visdat), "0000", as.character(p_lst))
  
}