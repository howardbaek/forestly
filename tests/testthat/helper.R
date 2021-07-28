plotly_snap <- function(p){
  p_js <- plotly::plotly_json(p, jsonedit = FALSE)
  p_lst <- jsonlite::fromJSON(p_js, simplifyVector = FALSE)
  
  p_lst <- gsub(names(p_lst$visdat), "0000", as.character(p_lst))
  p_lst[3:8]
}